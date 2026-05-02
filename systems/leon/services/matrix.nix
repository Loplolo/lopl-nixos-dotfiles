{
  config,
  pkgs,
  ...
}: {
  services.matrix-conduit = {
    enable = true;
    settings.global = {
      server_name = "matrix.lopl.dev";
      port = 6167;
      address = "127.0.0.1";
      database_backend = "rocksdb";
      allow_registration = false;
      allow_federation = true;
      allow_public_room_directory = false;
      max_request_size = 20971520;

      turn_uris = [
        "turn:matrix.lopl.dev:3478?transport=udp"
        "turn:matrix.lopl.dev:3478?transport=tcp"
        "turns:matrix.lopl.dev:5349?transport=tcp"
      ];
      turn_ttl = 86400;
    };
    extraEnvironment = {
      CONDUIT_global__turn_secret = "$(cat ${config.sops.secrets.turn-secret.path})";
    };
  };

  services.coturn = {
    enable = true;
    use-auth-secret = true;
    static-auth-secret-file = config.sops.secrets.turn-secret.path;
    realm = "matrix.lopl.dev";
    listening-port = 3478;
    tls-listening-port = 5349;
    min-port = 49152;
    max-port = 65535;
    no-tcp-relay = false;
    cert = "${config.security.acme.certs."matrix.lopl.dev".directory}/full.pem";
    pkey = "${config.security.acme.certs."matrix.lopl.dev".directory}/key.pem";
    extraConfig = ''
      fingerprint
      no-multicast-peers
      denied-peer-ip=0.0.0.0-0.255.255.255
      denied-peer-ip=10.0.0.0-10.255.255.255
      denied-peer-ip=100.64.0.0-100.127.255.255
      denied-peer-ip=127.0.0.0-127.255.255.255
      denied-peer-ip=169.254.0.0-169.254.255.255
      denied-peer-ip=172.16.0.0-172.31.255.255
      denied-peer-ip=192.168.0.0-192.168.255.255
    '';
  };

  security.acme = {
    acceptTerms = true;
    defaults.email = "admin@lopl.dev";
    certs."matrix.lopl.dev" = {
      dnsProvider = "cloudflare";
      credentialFiles = {
        "CF_DNS_API_TOKEN_FILE" = config.sops.secrets.caddy-cf-token.path;
      };
      group = "turnserver";
    };
  };

  systemd.services.coturn.after = ["acme-matrix.lopl.dev.service"];

  networking.firewall = {
    allowedTCPPorts = [3478 5349];
    allowedUDPPorts = [3478 5349];
    allowedUDPPortRanges = [
      {
        from = 49152;
        to = 65535;
      }
    ];
  };
}
