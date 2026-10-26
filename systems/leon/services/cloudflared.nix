{config, ...}: let
  tunnelId = "4863ed27-ae19-40f1-b839-7e7f958b56e4";

  # Through the local Caddy, so the same certificate and headers apply as on the tailnet.
  viaCaddy = host: {
    service = "https://127.0.0.1:443";
    originRequest.originServerName = host;
  };
in {
  services.cloudflared = {
    enable = true;
    tunnels = {
      ${tunnelId} = {
        credentialsFile = config.sops.secrets.cloudflared-creds.path;

        ingress = {
          "forgejo.lopl.dev" = "http://127.0.0.1:3000";
          "cloud.lopl.dev" = viaCaddy "cloud.lopl.dev";
          "office.lopl.dev" = viaCaddy "office.lopl.dev";
        };

        default = "http_status:404";
      };
    };
  };

  sops.secrets.cloudflared-creds = {};
}
