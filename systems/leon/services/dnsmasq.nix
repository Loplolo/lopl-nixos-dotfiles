{
  config,
  pkgs,
  ...
}: {
  services.dnsmasq = {
    enable = true;
    settings = {
      address = [
        "/lopl.dev/100.87.157.78"
        "/forgejo.lopl.dev/188.114.96.7"
        "/forgejo.lopl.dev/188.114.97.7"
        "/matrix.lopl.dev/188.114.96.7"
        "/matrix.lopl.dev/188.114.97.7"
        "/blog.lopl.dev/188.114.96.7"
        "/blog.lopl.dev/188.114.97.7"
        "/mc.lopl.dev/100.87.157.78"
      ];
      port = 5353;
      interface = ["lo" "tailscale0"];
      bind-dynamic = true;
      domain-needed = true;
      bogus-priv = true;
      no-resolv = true;
      srv-host = ["_minecraft._tcp.mc.lopl.dev,mc.lopl.dev,25565,0,5"];
    };
  };
}
