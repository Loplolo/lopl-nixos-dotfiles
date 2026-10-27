{config, ...}: {
  services.immich = {
    enable = true;
    host = "127.0.0.1";
    port = 2283;
  };

  lopl.proxies.immich = config.services.immich.port;
}
