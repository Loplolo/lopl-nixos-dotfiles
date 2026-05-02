{
  config,
  pkgs,
  lib,
  ...
}: let
  tsProxies = {
    "home.lopl.dev" = "127.0.0.1:8280";
    "adguard.lopl.dev" = "127.0.0.1:3001";
    "pdf.lopl.dev" = "127.0.0.1:2000";
    "immich.lopl.dev" = "127.0.0.1:2283";
    "music.lopl.dev" = "127.0.0.1:4533";
    "search.lopl.dev" = "127.0.0.1:8082";
    "syncthing.lopl.dev" = "127.0.0.1:8384";
    "bin.lopl.dev" = "127.0.0.1:8125";
    "ha.lopl.dev" = "127.0.0.1:8123";
    # cloud.lopl.dev     = "127.0.0.1:8081;"
    # matrix.lopl.dev    = "127.0.0.1:6167;"
  };

  caddyWithCloudflare = pkgs.caddy.withPlugins {
    plugins = ["github.com/caddy-dns/cloudflare@v0.2.4"];
    hash = "sha256-J0HWjCPoOoARAxDpG2bS9c0x5Wv4Q23qWZbTjd8nW84=";
  };
in {
  services.caddy = {
    enable = true;
    package = caddyWithCloudflare;

    globalConfig = ''
      acme_dns cloudflare {env.CF_API_TOKEN}
      default_bind 100.87.157.78
    '';

    virtualHosts =
      (lib.mapAttrs (host: upstream: {
          extraConfig = ''
            reverse_proxy ${upstream}
            tls {
              dns cloudflare {env.CF_API_TOKEN}
            }
          '';
        })
        tsProxies)
      // {
        "cloud.lopl.dev" = {
          extraConfig = ''
            # CalDAV and CardDAV service discovery
            redir /.well-known/carddav /remote.php/dav 301
            redir /.well-known/caldav  /remote.php/dav 301

            reverse_proxy 127.0.0.1:8081
            tls {
              dns cloudflare {env.CF_API_TOKEN}
            }
          '';
        };

        "matrix.lopl.dev" = {
          extraConfig = ''
            handle /.well-known/matrix/server {
              respond "{\"m.server\": \"matrix.lopl.dev:443\"}" 200 {
                close
              }
            }
            handle /.well-known/matrix/client {
              respond "{\"m.homeserver\": {\"base_url\": \"https://matrix.lopl.dev\"}}" 200 {
                close
              }
            }
            handle {
              reverse_proxy 127.0.0.1:6167
            }
          '';
        };
      };
  };

  systemd.services.caddy.serviceConfig.EnvironmentFile =
    config.sops.secrets.caddy-cf-token.path;
}
