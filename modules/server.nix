{
  pkgs,
  lib,
  ...
}: {
  imports = [
    ./common.nix
    ./sops.nix
  ];

  options.lopl.proxies = lib.mkOption {
    type = lib.types.attrsOf lib.types.port;
    default = {};
  };

  config = {
    # SSH
    services.openssh = {
      enable = true;
      settings = {
        PasswordAuthentication = false;
        KbdInteractiveAuthentication = false;
        PermitRootLogin = "no";
        X11Forwarding = false;
        MaxAuthTries = 3;
        LoginGraceTime = 30;
      };
    };

    security.sudo.execWheelOnly = true;

    boot.kernel.sysctl = {
      "kernel.kptr_restrict" = 2;
      "kernel.dmesg_restrict" = 1;
      "kernel.unprivileged_bpf_disabled" = 1;
      "net.core.bpf_jit_harden" = 2;
      "kernel.yama.ptrace_scope" = 1;
      "net.ipv4.tcp_syncookies" = 1;
      "net.ipv4.conf.all.accept_redirects" = 0;
      "net.ipv4.conf.default.accept_redirects" = 0;
      "net.ipv6.conf.all.accept_redirects" = 0;
      "net.ipv6.conf.default.accept_redirects" = 0;
      "net.ipv4.conf.all.send_redirects" = 0;
      "net.ipv4.conf.default.send_redirects" = 0;
      "net.ipv4.conf.all.accept_source_route" = 0;
      "net.ipv6.conf.all.accept_source_route" = 0;
    };

    virtualisation.podman = {
      enable = true;
      dockerCompat = true;
    };

    networking.nftables.enable = true;
    systemd.network.wait-online.enable = false;
    boot.initrd.systemd.network.wait-online.enable = false;

    services.btrfs.autoScrub = {
      enable = true;
      interval = "weekly";
      fileSystems = ["/" "/var/lib"];
    };

    zramSwap = {
      enable = true;
      memoryPercent = 50;
    };

    environment.systemPackages = with pkgs; [
      vim
      git
      wget
      curl
      htop
      btop
      tmux
      ncdu
      sops
      age
      ssh-to-age
      cloudflared
      fastfetch
      podman
      dig
      nmap
      tcpdump
      forgejo
      nextcloud-client
      lsof
      iotop
      smartmontools
      btrfs-progs
      lnav
      caddy
      mcrcon
    ];
  };
}
