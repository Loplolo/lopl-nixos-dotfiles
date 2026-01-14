{ config, pkgs, lib, ... }:

let 

quake = pkgs.writeText "quake-logo" ''
_.   ._
.!'     '!.
.!'       '!.
:!.  '!'  .!:
:!:..!..:!:
'!!!!!!!'
!
:!:
'';

in { 
  services.greetd = {
    enable = true;
    settings = {
      default_session = {
        command = lib.concatStringsSep " " [
          "${pkgs.tuigreet}/bin/tuigreet"
          "--time"
          "--asterisks"
          "--user-menu"
          "--window-padding 3"
          "--container-padding 3"
          "--prompt-padding 2"
          "--remember"
          "--remember-session"
          "--greeting \"$(cat ${quake})\""
          "--theme 'border=#8f5332;text=#8f5332;prompt=#e69c65;time=#8f5332;action=#e69c65;button=#0a0a0a;container=#0a0a0a;input=#e69c65;selection=#3b2216'"
          "--cmd ${pkgs.sway}/bin/sway"
        ];
        user = "greeter";
      };
    };
  };
}
