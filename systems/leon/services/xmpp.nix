{
  config,
  pkgs,
  ...
}: let
  domain = "xmpp.lopl.dev";
  mucDomain = "conference.${domain}";
  uploadDomain = "upload.${domain}";
in {
  services.prosody = {
    enable = true;
    admins = ["admin@${domain}"];

    ssl = {
      cert = "/var/lib/acme/${domain}/fullchain.pem";
      key = "/var/lib/acme/${domain}/key.pem";
    };

    httpFileShare = {
      domain = uploadDomain;
      uploadFileSizeLimit = 100 * 1024 * 1024;
    };

    muc = [
      {
        domain = mucDomain;
        name = "Chat Rooms";
        restrictRoomCreation = false;
      }
    ];

    virtualHosts.${domain} = {
      enabled = true;
      domain = domain;
      ssl = {
        cert = "/var/lib/acme/${domain}/fullchain.pem";
        key = "/var/lib/acme/${domain}/key.pem";
      };
    };

    modules = {
      roster = true;
      sslauth = true;
      tls = true;
      dialback = true;
      disco = true;
      carbons = true;
      pep = true;
      mam = true;
      ping = true;
      admin_adhoc = true;
      http_files = true;
    };

    allowRegistration = false;
  };

  users.groups.certs.members = ["prosody" "caddy"];

  security.acme = {
    acceptTerms = true;
    default.email = "lopl@lopl.dev";
    certs.${domain} = {
      dnsProvider = "cloudflare";
      environmentFile = config.sops.secrets.caddy-cf-token.path;
    };
  };
}
