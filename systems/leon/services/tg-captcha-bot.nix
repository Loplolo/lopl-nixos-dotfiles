{
  config,
  pkgs,
  ...
}: let
  shieldy = pkgs.fetchFromGitHub {
    owner = "1inch";
    repo = "shieldy";
    rev = "master";
    hash = "sha256-12BqAYl9zji6D7/DCOAUiEBpgR0aEgNhTfA/ePTIPRQ=";
  };
in {
  virtualisation.podman.enable = true;
  virtualisation.containers.registries.settings.registries.search.registries = ["docker.io"];
  virtualisation.oci-containers = {
    backend = "podman";

    containers."shieldy-mongo" = {
      image = "docker.io/library/mongo:6";
      autoStart = true;
      extraOptions = ["--network=host"];
      cmd = ["--bind_ip" "127.0.0.1"];
      volumes = ["shieldy_mongo_data:/data/db"];
    };

    containers."shieldy-bot" = {
      image = "localhost/shieldy:latest";
      autoStart = true;
      extraOptions = ["--network=host"];
      dependsOn = ["shieldy-mongo"];
      cmd = ["yarn" "distribute"];
      environmentFiles = [config.sops.templates."shieldy-env".path];
    };
  };

  systemd.services = {
    podman-build-shieldy = {
      wants = ["network-online.target"];
      after = ["network-online.target"];
      path = [pkgs.podman];
      script = ''
        set -euo pipefail
        builddir=$(mktemp -d)
        trap 'rm -rf "$builddir"' EXIT
        cp -r ${shieldy}/. "$builddir"
        chmod -R u+w "$builddir"
        podman build -t localhost/shieldy:latest "$builddir"
      '';
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        TimeoutStartSec = "15m";
      };
    };

    podman-shieldy-bot = {
      requires = ["podman-build-shieldy.service"];
      after = ["podman-build-shieldy.service"];
    };
  };

  sops.secrets = {
    tg-shieldy-bot-token = {};
    tg-bot-owner = {};
  };

  sops.templates."shieldy-env" = {
    mode = "0400";
    restartUnits = ["podman-shieldy-bot.service"];
    content = ''
      TOKEN=${config.sops.placeholder."tg-shieldy-bot-token"}
      MONGO=mongodb://127.0.0.1:27017/shieldy
      ADMIN=${config.sops.placeholder."tg-bot-owner"}
    '';
  };
}
