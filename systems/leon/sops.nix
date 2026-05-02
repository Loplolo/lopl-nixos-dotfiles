{
  config,
  pkgs,
  ...
}: {
  sops = {
    validateSopsFiles = false;
    age.keyFile = "/var/lib/sops-nix/key.txt";

    secrets = {
      tailscale-authkey = {
        sopsFile = ../../secrets/secrets.yaml;
        key = "tailscale-authkey";
      };
      nextcloud-admin-pass = {
        sopsFile = ../../secrets/secrets.yaml;
        key = "nextcloud-admin-password";
      };
      pihole-webpassword = {
        sopsFile = ../../secrets/secrets.yaml;
        key = "pihole-web-password";
      };
      telegram-captcha-bot-token = {
        sopsFile = ../../secrets/secrets.yaml;
        key = "captcha-bot-token";
        owner = "root";
      };
      cloudflared-creds = {
        sopsFile = ../../secrets/secrets.yaml;
        key = "cloudflared-creds";
      };
      searxng-secret = {
        sopsFile = ../../secrets/secrets.yaml;
      };
      caddy-cf-token = {
        sopsFile = ../../secrets/secrets.yaml;
        key = "caddy-cf-token";
      };
      turn-secret = {
        sopsFile = ../../secrets/secrets.yaml;
        key = "turn-secret";
        owner = "turnserver";
      };
      steam-api-key = {
        sopsFile = ../../secrets/secrets.yaml;
      };
      tailscale-api-key = {
        sopsFile = ../../secrets/secrets.yaml;
        key = "tailscale-api-key";
      };
      tg-captcha-bot-token = {
        sopsFile = ../../secrets/secrets.yaml;
        key = "tg-captcha-bot-token";
      };
      tg-captcha-bot-owner = {
        sopsFile = ../../secrets/secrets.yaml;
        key = "tg-captcha-bot-owner";
      };
    };
  };
}
