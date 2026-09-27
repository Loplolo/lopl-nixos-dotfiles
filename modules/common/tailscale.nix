{config, ...}: {
  imports = [./sops.nix];

  sops.secrets.tailscale-authkey = {};

  services.tailscale = {
    enable = true;
    authKeyFile = config.sops.secrets.tailscale-authkey.path;
  };

  networking.firewall.trustedInterfaces = ["tailscale0"];
}
