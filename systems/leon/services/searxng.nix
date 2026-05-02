{
  config,
  pkgs,
  lib,
  ...
}: {
  services.searx = {
    enable = true;

    environmentFile = config.sops.secrets.searxng-secret.path;

    settings = {
      server = {
        port = 8082;
        bind_address = "127.0.0.1";
        secret_key = "@SEARXNG_SECRET_KEY@";
      };

      ui = {
        static_use_hash = true;
        theme = "simple";
      };

      search = {
        safe_search = 0;
        autocomplete = "duckduckgo";
      };

      engines = [
        {
          name = "google";
          engine = "google";
          shortcut = "g";
        }
        {
          name = "duckduckgo";
          engine = "duckduckgo";
          shortcut = "ddg";
        }
        {
          name = "wikipedia";
          engine = "wikipedia";
          shortcut = "w";
        }
        {
          name = "bing";
          engine = "bing";
        }
      ];
    };
  };
}
