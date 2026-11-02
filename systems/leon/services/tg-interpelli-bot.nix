{
  config,
  pkgs,
  lib,
  ...
}: let
  interpelli = pkgs.fetchFromGitHub {
    owner = "Loplolo";
    rev = "main";
    repo = "interpelli-torino";
    hash = "sha256-n2EIBS7RwyCdjAEatxClGhgi4QoXslKCmaXNcwZiokQ=";
  };
in {
  virtualisation.podman.enable = true;
  virtualisation.containers.registries.settings.unqualified-search-registries = ["docker.io"];

  virtualisation.oci-containers = {
    backend = "podman";

    containers."interpelli-bot" = {
      image = "localhost/interpelli-bot:latest";
      autoStart = true;
      extraOptions = ["--network=host"];
      cmd = [
        "sh"
        "-c"
        "python -c 'import main; main.load_cache() or main.scrape(main.url, main.cache)' || echo 'initial scrape failed'; exec python main.py"
      ];
      volumes = [
        "/var/lib/interpelli-bot:/data"
      ];

      environment = {
        CACHE = "/data/interpelli.json";
        USERS_FILE = "/data/users.json";
        SENT = "/data/sent.json";
        TZ = "Europe/Rome";
      };

      environmentFiles = [config.sops.templates."interpelli-env".path];
    };
  };

  systemd.tmpfiles.rules = [
    "d /var/lib/interpelli-bot 0750 root root -"
  ];

  systemd.services = {
    podman-build-interpelli-bot = {
      wants = ["network-online.target"];
      after = ["network-online.target"];
      path = [pkgs.podman pkgs.coreutils];
      script = ''
        set -euo pipefail
        builddir=$(mktemp -d)
        trap 'rm -rf "$builddir"' EXIT
        cp -r ${interpelli}/. "$builddir"
        chmod -R u+w "$builddir"

        cat >"$builddir/Dockerfile" <<'EOF'
        FROM docker.io/library/python:3.13-slim
        ENV PYTHONUNBUFFERED=1
        ENV TZ=Europe/Rome
        WORKDIR /app
        RUN apt-get update \
            && apt-get install -y --no-install-recommends tzdata \
            && rm -rf /var/lib/apt/lists/*
        RUN pip install --no-cache-dir requests beautifulsoup4 lxml 'python-telegram-bot[job-queue]'
        COPY main.py .
        CMD ["python", "main.py"]
        EOF

        podman build -t localhost/interpelli-bot:latest "$builddir"
      '';
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        TimeoutStartSec = "15m";
      };
    };

    podman-interpelli-bot = {
      requires = ["podman-build-interpelli-bot.service"];
      after = ["podman-build-interpelli-bot.service"];
    };
  };

  sops.secrets = {
    "interpelli-bot-token" = {};
  };

  sops.templates."interpelli-env" = {
    mode = "0400";
    restartUnits = ["podman-interpelli-bot.service"];
    content = ''
      BOT_TOKEN=${config.sops.placeholder."interpelli-bot-token"}
    '';
  };
}
