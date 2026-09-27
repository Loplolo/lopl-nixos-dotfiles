{
  virtualisation.oci-containers.containers."homeassistant" = {
    image = "ghcr.io/home-assistant/home-assistant:stable";
    volumes = [
      "/var/lib/homeassistant:/config"
    ];
    environment = {
      TZ = "Europe/Rome";
    };
    extraOptions = [
      "--network=host"
    ];
  };

  lopl.proxies.ha = 8123;

  networking.firewall.allowedTCPPorts = [8123];
}
