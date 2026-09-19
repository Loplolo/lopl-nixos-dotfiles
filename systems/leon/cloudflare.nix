{
  config,
  pkgs,
  lib,
  ...
}: let
  tunnelId = "4863ed27-ae19-40f1-b839-7e7f958b56e4";
in {
  services.cloudflared = {
    enable = true;
    tunnels = {
      ${tunnelId} = {
        credentialsFile = config.sops.secrets.cloudflared-creds.path;

        ingress = {
          "forgejo.lopl.dev" = "http://127.0.0.1:3000";
          "blog.lopl.dev" = "http://127.0.0.1:8081";
        };

        default = "http_status:404";
      };
    };
  };
}
