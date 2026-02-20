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
    blockbench
    blueman
    blender
    calibre
    chromium
    claude-code
    vscode
    code-cursor
    cmus
    direnv
    # Metodi formali
    (coq.withPackages (coqPkgs: [
      coqPkgs.stdlib  
      coqPkgs.mathcomp
      
    ]))
    # ----------------
    darkplaces
    dbeaver-bin
    (discord.override {
      withOpenASAR = true;
      withVencord = true;
    })
    ericw-tools
    fastfetch
    ffmpeg-full
    flameshot
    element-desktop
    gcc
    geteduroam-cli
    gimp
    gnumake
    gnutar
    grim
    gvfs
    guile
    guile-hoot
    guile-reader
    guile-lib
    haunt
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
    python3
    networkmanagerapplet
    obsidian
    openarena
    openttd
    openvpn
    pandoc
    pavucontrol
    pkg-config
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
