{config, ...}: {
  services.syncthing = {
    enable = true;

    overrideDevices = true;
    overrideFolders = true;

    settings = {
      devices = {
        "desktop" = {id = "EMOAI33-KQWVQQF-73B2W2C-LNTCW2U-PBTI5RI-BUWRMMM-UV5Y63G-4URSJQU";};
        "laptop" = {id = "ZUMFGQV-ZAYZVYM-OWW44HT-JALAVH7-DPZZDBS-NCKJIV6-HHDBQXU-ODRSFQF";};
        "server" = {id = "BFM6PQL-3WLN6UQ-LYNHFLG-WWDDRZA-VTWSBDH-IJZRNXN-4DON3CX-35VFGQA";};
      };

      folders = {
        "Journal" = {
          path = "/home/lopl/Documents/Journal";
          devices = ["desktop" "laptop" "server"];
        };
        "Notes" = {
          path = "/home/lopl/Documents/Notes";
          devices = ["desktop" "laptop" "server"];
        };
        "Obsidian" = {
          path = "/home/lopl/Documents/obsidian-vault";
          devices = ["desktop" "laptop" "server"];
        };
        "Pictures" = {
          path = "/home/lopl/Pictures";
          devices = ["desktop" "laptop" "server"];
        };
        "Anki" = {
          path = "/home/lopl/Documents/Anki";
          devices = ["desktop" "laptop" "server"];
        };
      };

      gui = {
        enabled = true;
        address = "127.0.0.1:8384";
      };
    };
  };
}
