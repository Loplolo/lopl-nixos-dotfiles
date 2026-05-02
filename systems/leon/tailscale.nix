{
  config,
  pkgs,
  ...
}: {
  services.tailscale = {
    enable = true;
    authKeyFile = config.sops.secrets.tailscale-authkey.path;
    useRoutingFeatures = "both";
    extraUpFlags = ["--accept-dns=true" "--ssh"];
  };
}
