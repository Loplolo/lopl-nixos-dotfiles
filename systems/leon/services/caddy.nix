{
  config,
  pkgs,
  lib,
  ...
}: let
  caddyWithCloudflare = pkgs.caddy.withPlugins {
    plugins = [
      "github.com/caddy-dns/cloudflare@v0.2.4"
      "github.com/mholt/caddy-l4@v0.1.2"
    ];
    hash = "sha256-EWpo+POYSfxsAE4W4T9ngK+n1bmJPzLS8A1+JGvoJvE=";
  };
in {
  services.caddy = {
    enable = true;
    package = caddyWithCloudflare;

    globalConfig = ''
      acme_dns cloudflare {env.CF_API_TOKEN}

      {
          log {
              output stdout
              level DEBUG
          }
      }

      layer4 {
        # Wingo's server
        :25565 {
          route {
            proxy 100.92.106.16:25565
          }
        }
    '';

    virtualHosts = lib.mapAttrs' (name: port:
      lib.nameValuePair "${name}.lopl.dev" {
        extraConfig = ''
          header Strict-Transport-Security "max-age=31536000; includeSubDomains"
          reverse_proxy 127.0.0.1:${toString port}
          tls {
            dns cloudflare {env.CF_API_TOKEN}
          }
        '';
      })
    config.lopl.proxies;
  };

  systemd.services.caddy.serviceConfig.EnvironmentFile =
    config.sops.secrets.caddy-cf-token.path;

  sops.secrets.caddy-cf-token = {};

  networking.firewall = {
    allowedTCPPorts = [80 443 25565];
    allowedUDPPorts = [25565];
  };
}
