{
  config,
  lib,
  ...
}: let
  watched = [
    "caddy.service"
    "nginx.service"
    "phpfpm-nextcloud.service"
    "postgresql.service"
    "redis-nextcloud.service"
    "coolwsd.service"
    "forgejo.service"
    "immich-server.service"
    "immich-machine-learning.service"
    "redis-immich.service"
    "navidrome.service"
    "adguardhome.service"
    "syncthing.service"
    "podman-homeassistant.service"
    "podman-shieldy-bot.service"
    "podman-shieldy-mongo.service"
    "tailscaled.service"
    "fail2ban.service"
    "sshd.service"
    "status-bot.service"
    "prometheus.service"
    "alertmanager.service"
    "prometheus-node-exporter.service"
  ];

  watchedRegex = lib.concatStringsSep "|" (watched ++ ["cloudflared-tunnel-.*"]);

  rules = [
    (builtins.toJSON {
      groups = [
        {
          name = "leon";
          rules = [
            {
              alert = "InstanceDown";
              expr = ''up{job=~"prometheus|node"} == 0'';
              for = "2m";
              labels.severity = "critical";
              annotations.summary = "{{ $labels.job }} on {{ $labels.instance }} is down";
            }
            {
              alert = "SystemdUnitFailed";
              expr = ''node_systemd_unit_state{state="failed"} == 1'';
              for = "5m";
              labels.severity = "critical";
              annotations.summary = "systemd unit {{ $labels.name }} failed";
            }
            {
              alert = "CriticalUnitInactive";
              expr = ''node_systemd_unit_state{name=~"${watchedRegex}",state="active"} == 0'';
              for = "5m";
              labels.severity = "critical";
              annotations.summary = "{{ $labels.name }} is not active";
            }
            {
              alert = "FilesystemLow";
              expr = ''(node_filesystem_avail_bytes{fstype!~"tmpfs|overlay|squashfs|ramfs",mountpoint!="/nix/store"} / node_filesystem_size_bytes{fstype!~"tmpfs|overlay|squashfs|ramfs",mountpoint!="/nix/store"}) < 0.10'';
              for = "10m";
              labels.severity = "warning";
              annotations.summary = "{{ $labels.mountpoint }} is {{ $value | humanizePercentage }} full";
            }
            {
              alert = "MemoryLow";
              expr = ''node_memory_MemAvailable_bytes / node_memory_MemTotal_bytes < 0.10'';
              for = "10m";
              labels.severity = "warning";
              annotations.summary = "available memory is {{ $value | humanizePercentage }}";
            }
            {
              alert = "BackupStale";
              expr = ''time() - node_systemd_timer_last_trigger_seconds{name="restic-backups-nextcloud.timer"} > 30 * 3600'';
              for = "10m";
              labels.severity = "critical";
              annotations.summary = "nextcloud backup has not run in over 30h";
            }
          ];
        }
      ];
    })
  ];
in {
  services.prometheus = {
    enable = true;
    listenAddress = "127.0.0.1";
    port = 9090;
    retentionTime = "30d";
    globalConfig = {
      scrape_interval = "30s";
      evaluation_interval = "30s";
    };
    scrapeConfigs = [
      {
        job_name = "prometheus";
        static_configs = [{targets = ["127.0.0.1:9090"];}];
      }
      {
        job_name = "node";
        static_configs = [{targets = ["127.0.0.1:9100"];}];
      }
    ];
    alertmanagers = [
      {
        static_configs = [{targets = ["127.0.0.1:9093"];}];
      }
    ];
    exporters.node = {
      enable = true;
      listenAddress = "127.0.0.1";
      enabledCollectors = ["systemd"];
    };
    inherit rules;
  };

  services.prometheus.alertmanager = {
    enable = true;
    listenAddress = "127.0.0.1";
    port = 9093;
    # The token/chat files are only present at runtime.
    checkConfig = false;
    configuration = {
      global.resolve_timeout = "5m";
      route = {
        receiver = "telegram";
        group_by = ["alertname" "instance"];
        group_wait = "30s";
        group_interval = "5m";
        repeat_interval = "4h";
      };
      receivers = [
        {
          name = "telegram";
          telegram_configs = [
            {
              bot_token_file = "/run/credentials/alertmanager.service/telegram_token";
              chat_id_file = "/run/credentials/alertmanager.service/telegram_chat";
              send_resolved = true;
            }
          ];
        }
      ];
    };
  };

  systemd.services.alertmanager = {
    wants = ["sops-install-secrets.service"];
    after = ["sops-install-secrets.service"];
    serviceConfig.LoadCredential = [
      "telegram_token:${config.sops.secrets.tg-homelab-bot-token.path}"
      "telegram_chat:${config.sops.secrets.security-alert-chat-id.path}"
    ];
  };
}
