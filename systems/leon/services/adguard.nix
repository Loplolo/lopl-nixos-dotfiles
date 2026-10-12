{
  config,
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
          "https://dns.cloudflare.com/dns-query"
          "https://dns.quad9.net/dns-query"
        ];
      };
      filtering.rewrites =
        map (name: {
          domain = "${name}.lopl.dev";
          answer = "100.87.157.78";
          enabled = true;
        })
        (lib.attrNames config.lopl.proxies ++ ["mc"]);
      filters = [
        {
          enabled = true;
          url = "https://adguardteam.github.io/HostlistsRegistry/assets/filter_1.txt";
          name = "AdGuard DNS filter";
          id = 1;
        }
        {
          enabled = true;
          url = "https://adaway.org/hosts.txt";
          name = "AdAway Default Blocklist";
          id = 2;
        }
        {
          enabled = true;
          url = "https://adguardteam.github.io/HostlistsRegistry/assets/filter_11.txt";
          name = "OISD Blocklist Basic";
          id = 3;
        }
      ];
    };
  };

  lopl.proxies.adguard = config.services.adguardhome.port;

  networking.firewall = {
    allowedTCPPorts = [53 3001];
    allowedUDPPorts = [53];
  };
}
