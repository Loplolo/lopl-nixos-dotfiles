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
    "movies.lopl.dev" = "127.0.0.1:8096";
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
    '';

    virtualHosts =
      lib.mapAttrs (host: upstream: {
        extraConfig = ''
          reverse_proxy ${upstream}
          tls {
            dns cloudflare {env.CF_API_TOKEN}
          }
        '';
      })
      tsProxies;
  };

  systemd.services.caddy.serviceConfig.EnvironmentFile =
    config.sops.secrets.caddy-cf-token.path;
}
