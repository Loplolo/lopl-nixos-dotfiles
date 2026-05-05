{
  config,
  pkgs,
  ...
}: {
  services.syncthing = {
    user = "lopl";
    dataDir = "/home/lopl";

    enable = true;

    overrideDevices = true;
    overrideFolders = true;

    settings = {
      options = {
        listenAddresses = [
          "tcp://0.0.0.0:22000"
        ];
      };

      devices = {
        "desktop" = {
          id = "EMOAI33-KQWVQQF-73B2W2C-LNTCW2U-PBTI5RI-BUWRMMM-UV5Y63G-4URSJQU";
          addresses = ["dynamic"];
        };
        "laptop" = {
          id = "ZUMFGQV-ZAYZVYM-OWW44HT-JALAVH7-DPZZDBS-NCKJIV6-HHDBQXU-ODRSFQF";
          addresses = ["dynamic"];
        };
        "server" = {
          id = "BFM6PQL-3WLN6UQ-LYNHFLG-WWDDRZA-VTWSBDH-IJZRNXN-4DON3CX-35VFGQA";
          addresses = ["dynamic"];
        };
      };

      folders = {
        "Journal" = {
          path = "~/Documents/Journal";
          devices = ["desktop" "laptop" "server"];
        };
        "Notes" = {
          path = "~/Documents/Notes";
          devices = ["desktop" "laptop" "server"];
        };
        "Obsidian" = {
          path = "~/Documents/obsidian-vault";
          devices = ["desktop" "laptop" "server"];
        };
        "Pictures" = {
          path = "~/Pictures";
          devices = ["desktop" "laptop" "server"];
        };
        "Anki" = {
          path = "~/Documents/Anki";
          devices = ["desktop" "laptop" "server"];
        };
      };

      gui = {
        enabled = true;
        address = "127.0.0.1:8384";
        insecureSkipHostcheck = true;
      };
    };
  };
}
