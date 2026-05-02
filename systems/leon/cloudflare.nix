{
  config,
  pkgs,
  lib,
  ...
}: {
  services.cloudflared = {
    enable = true;
    tunnels = {
      "4863ed27-ae19-40f1-b839-7e7f958b56e4" = {
        credentialsFile = "${config.sops.secrets.cloudflared-creds.path}";
        ingress = {
          "forgejo.lopl.dev" = "http://127.0.0.1:3000";
          "matrix.lopl.dev" = "http://127.0.0.1:6167";
          "blog.lopl.dev" = "http://127.0.0.1:8081";
        };
        default = "http_status:404";
      };
    };
  };
}
