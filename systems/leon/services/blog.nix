let
  port = 8083;
in {
  services.nginx = {
    enable = true;
    virtualHosts."blog.lopl.dev" = {
      root = "/var/lib/blog";
      listen = [
        {
          addr = "127.0.0.1";
          inherit port;
        }
      ];
    };
  };

  systemd.tmpfiles.rules = [
    "d /var/lib/blog 0755 lopl users -"
  ];

  services.cloudflared.tunnels."4863ed27-ae19-40f1-b839-7e7f958b56e4".ingress."blog.lopl.dev" = "http://127.0.0.1:${toString port}";

  services.dnsmasq.settings.address = [
    "/blog.lopl.dev/188.114.96.7"
    "/blog.lopl.dev/188.114.97.7"
  ];
}
