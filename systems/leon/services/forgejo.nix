{
  config,
  pkgs,
  ...
}: {
  services.forgejo = {
    enable = true;
    stateDir = "/var/lib/forgejo";

    settings = {
      server = {
        HTTP_ADDR = "127.0.0.1";
        HTTP_PORT = 3000;
        DOMAIN = "forgejo.lopl.dev";
        ROOT_URL = "https://forgejo.lopl.dev";
        SSH_DOMAIN = "forgejo.lopl.dev";
        SSH_PORT = 22;
      };
      service = {
        DISABLE_REGISTRATION = true;
      };
      log.LEVEL = "Warn";
    };
  };
}
