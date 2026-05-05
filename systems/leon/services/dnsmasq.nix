{
  config,
  pkgs,
  ...
}: {
  services.dnsmasq = {
    enable = true;

    settings = {
      address = [
        "/home.lopl.dev/100.87.157.78"
        "/adguard.lopl.dev/100.87.157.78"
        "/pdf.lopl.dev/100.87.157.78"
        "/immich.lopl.dev/100.87.157.78"
        "/music.lopl.dev/100.87.157.78"
        "/search.lopl.dev/100.87.157.78"
        "/syncthing.lopl.dev/100.87.157.78"
        "/bin.lopl.dev/100.87.157.78"
        "/ha.lopl.dev/100.87.157.78"
        "/movies.lopl.dev/100.87.157.78"

        "/forgejo.lopl.dev/188.114.96.7"
        "/forgejo.lopl.dev/188.114.97.7"

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

      server = [
        "1.1.1.1"
        "8.8.8.8"
      ];

      srv-host = [
        "_minecraft._tcp.mc.lopl.dev,mc.lopl.dev,25565,0,5"
      ];
    };
  };
}
