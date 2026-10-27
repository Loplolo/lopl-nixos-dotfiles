{config, ...}: {
  services.microbin = {
    enable = true;
    passwordFile = config.sops.secrets.microbin-env.path;
    settings = {
      MICROBIN_PORT = 8125;
      MICROBIN_BIND = "127.0.0.1";
      MICROBIN_PUBLIC_PATH = "https://bin.lopl.dev";

      MICROBIN_PRIVATE = true;
      MICROBIN_READONLY = false;
      MICROBIN_DISABLE_TELEMETRY = true;
      MICROBIN_NO_LISTING = true;

      MICROBIN_ADMIN_USERNAME = "lopl";
    };
  };

  sops.secrets.microbin-env = {};

  lopl.proxies.bin = config.services.microbin.settings.MICROBIN_PORT;
}
