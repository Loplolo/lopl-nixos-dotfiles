{config, ...}: {
  home.file."Documents/Notes/.keep".text = "";
  home.file."Documents/Journal/.keep".text = "";
  services.syncthing = {
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
          id = "IVCWUDO-TSVCJ4C-DNUKRHB-INRQXTJ-OKXO43X-4UB2OOO-7L54WDU-WQ5JPAG";
          addresses = ["dynamic"];
        };
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
