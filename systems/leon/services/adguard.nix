{
  config,
  pkgs,
  lib,
  ...
}: {
  services.adguardhome = {
    enable = true;
    mutableSettings = false;
    port = 3001;
    settings = {
      dns = {
        bind_hosts = ["0.0.0.0"];
        port = 53;

        bootstrap_dns = [
          "9.9.9.9"
          "1.1.1.1"
        ];

        upstream_dns = [
          "[/lopl.dev/]127.0.0.1:5353"
          "https://dns.cloudflare.com/dns-query"
          "https://dns.quad9.net/dns-query"
        ];
      };
    };
  };
}
