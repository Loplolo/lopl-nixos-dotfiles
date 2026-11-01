{
  config,
  pkgs,
  lib,
  ...
}: let
  interpelli = pkgs.fetchFromGitHub {
    owner = "Loplolo";
    repo = "interpelli-torino";
    rev = "master";
    hash = lib.fakeHash;
  };
in {
  virtualisation.podman.enable = true;
  virtualisation.containers.registries.settings.registries.search.registries = ["docker.io"];

  virtualisation.oci-containers = {
    backend = "podman";

    containers."interpelli-bot" = {
      image = "localhost/interpelli-bot:latest";
      autoStart = true;
      extraOptions = ["--network=host"];
      cmd = ["python" "main.py"];
      volumes = [
        "/var/lib/interpelli-bot:/data"
      ];

      environment = {
        CACHE = "/data/interpello.json";
        USERS_FILE = "/data/users.json";
      };

      environmentFiles = [config.sops.templates."interpelli-env".path];
    };
  };

  systemd.services = {
    podman-build-interpelli-bot = {
      wants = ["network-online.target"];
      after = ["network-online.target"];
      path = [pkgs.podman];
      script = ''
        set -euo pipefail
        builddir=$(mktemp -d)
        trap 'rm -rf "$builddir"' EXIT
        cp -r ${interpelli}/. "$builddir"
        chmod -R u+w "$builddir"
        
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
