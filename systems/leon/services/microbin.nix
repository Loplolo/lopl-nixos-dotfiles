{
  config,
  pkgs,
  ...
}: {
  services.microbin = {
    enable = true;
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
}
