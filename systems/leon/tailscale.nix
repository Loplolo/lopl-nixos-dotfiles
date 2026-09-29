{
  imports = [../../modules/tailscale.nix];

  services.tailscale = {
    useRoutingFeatures = "both";
    extraUpFlags = ["--accept-dns=true" "--ssh"];
  };

  systemd.services.tailscaled.serviceConfig.Environment = [
    "TS_DEBUG_FIREWALL_MODE=nftables"
  ];
}
