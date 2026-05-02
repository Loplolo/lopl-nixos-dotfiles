{
  config,
  pkgs,
  lib,
  ...
}: {
  users.users.minecraft = {
    isSystemUser = true;

    group = "minecraft";

    home = "/srv/minecraft/sunlit-valley";

    createHome = true;
  };

  users.groups.minecraft = {};

  systemd.services.minecraft-sunlit-valley = {
    description = "Society: Sunlit Valley Minecraft Server";

    wantedBy = ["multi-user.target"];

    after = ["network.target"];

    serviceConfig = {
      User = "minecraft";

      Group = "minecraft";

      WorkingDirectory = "/srv/minecraft/sunlit-valley";

      ExecStart = ''${pkgs.jdk17}/bin/java @user_jvm_args.txt @libraries/net/minecraftforge/forge/1.20.1-47.4.0/unix_args.txt "$@"'';

      Restart = "on-failure";

      RestartSec = "30s";
    };
  };
}
