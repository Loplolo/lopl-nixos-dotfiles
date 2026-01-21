{pkgs, ...}: {
  programs.alacritty.enable = true;

  programs.obs-studio = {
    enable = true;
    plugins = with pkgs.obs-studio-plugins; [
      wlrobs
      obs-backgroundremoval
      obs-pipewire-audio-capture
    ];
  };

 home.packages = with pkgs; [
    i7z
    appimage-run
    aider-chat
    blockbench
    blueman
    blender
    calibre
    cdparanoia
    chromium
    cmus
    # Metodi formali
    coqPackages.coq
    coqPackages.coq-hammer 
    # ----------------
    darkplaces
    (discord.override {
      withOpenASAR = true;
      withVencord = true;
    })
    ericw-tools
    fastfetch
    ffmpeg-full
    flameshot
    gcc
    geteduroam-cli
    gimp
    gnumake
    gnutar
    grim
    gvfs
    gzdoom
    htop
    i3status
    imagemagick
    imv
    inkscape
    ironwail
    jetbrains.idea-community
    jetbrains.pycharm-community
    jetbrains.webstorm
    krita
    libreoffice
    mpv
    networkmanagerapplet
    nyxt
    obsidian
    openarena
    openttd
    openvpn
    pavucontrol
    playerctl
    prismlauncher
    qbittorrent
    qemu
    qmmp
    quakespasm
    quickemu
    r2mod_cli
    steam-run
    teams-for-linux
    telegram-desktop
    thunderbird
    tofi
    tree
    ttyper
    typst
    unzip
    virt-manager
    vkquake
    vscode
    wdisplays
    wike
    wine
    winetricks
    wl-clipboard
    xarchiver
    (xfce.thunar.override {
      thunarPlugins = with xfce; [
        thunar-archive-plugin
        thunar-volman
        thunar-media-tags-plugin
      ];
    })
    xfce.tumbler
    xonotic-glx
    xournalpp
    xz
    zip
  ];

}
