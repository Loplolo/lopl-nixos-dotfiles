{config, ...}: {
  services.stirling-pdf = {
    enable = true;
    environment = {
      SERVER_PORT = 2000;
    };
  };

  lopl.proxies.pdf = config.services.stirling-pdf.environment.SERVER_PORT;
}
