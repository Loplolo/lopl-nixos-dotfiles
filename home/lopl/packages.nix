{
  pkgs,
  pkgs-stable,
  ...
}: {
  programs.info.enable = true;

  programs.obs-studio = {
    enable = true;
    plugins = with pkgs.obs-studio-plugins; [
      obs-backgroundremoval
      obs-pipewire-audio-capture
      obs-gstreamer
    ];
  };

  home.packages = with pkgs; [
    # System
    fastfetch
    freenect
    htop
    i7z
    jack2
    openal
    pavucontrol
    xwiimote

    # Desktop
    arandr
    blueman
    flameshot
    maim
    networkmanagerapplet
    playerctl
    wmctrl
    xclip
    xdotool
    xsel

    # Files
    (thunar.override {
      thunarPlugins = [
        thunar-archive-plugin
        thunar-volman
        thunar-media-tags-plugin
      ];
    })
    gnutar
    p7zip
    unrar
    unzip
    xarchiver
    xz
    zip

    # Tools
    ffmpeg
    imagemagick
    jq
    pandoc
    pv
    tmux
    tree
    ttyper
    yt-dlp

    # Security
    age
    openvpn
    sops
    ssh-to-age

    # Virtualization
    appimage-run
    distrobox
    pkgs-stable.steam-run
    qemu
    quickemu
    virt-manager

    # Nix
    update-nix-fetchgit

    # Development
    android-studio
    android-tools
    dbeaver-bin
    drawio
    gurobi
    octave
    pkg-config
    plantuml
    unityhub
    vscode

    # Info Manuals
    nix-pills-info
    sicp-info

    # Writing
    libreoffice
    zotero
    typst

    # Reading
    calibre
    wike

    # Notes
    anki
    obsidian
    super-productivity
    xournalpp

    # Browser
    chromium

    # File Sharing
    localsend
    nicotine-plus
    qbittorrent

    # Communication
    dino
    discord
    element-desktop
    teams-for-linux
    telegram-desktop
    thunderbird

    # Graphics
    feh
    gimp
    imv
    inkscape
    krita
    libresprite

    # 3D
    blender
    blockbench
    pkgs-stable.freecad
    orca-slicer

    # Maps
    qgis

    # Audio and Video
    cmus
    kdePackages.kdenlive
    mpv
    qmmp
    reaper
    strawberry

    # Wine
    alcom
    faugus-launcher
    pkgs-stable.bottles
    pkgs-stable.wineWow64Packages.staging
    pkgs-stable.winetricks
    umu-launcher

    # Emulation
    ppsspp
    (retroarch.withCores (
      cores:
        with cores; [
          flycast
          beetle-gba
          desmume
          dolphin
          citra
          dosbox
          mesen
          snes9x
          pcsx2
        ]
    ))

    # Games
    gamemode
    gzdoom
    nethack
    openarena
    openttd
    vintagestoryPackages.latest
    vintagestoryPackages.rustique
    xonotic-glx
    (olympus.override {celesteWrapper = "steam-run";})
    prismlauncher
    r2mod_cli

    # Quake Engines
    darkplaces
    fteqw-latest
    ironwail
    qss-m
    quakespasm

    # Quake Modding
    ericw-tools
    fteqcc
    paktool
    slade
    trenchbroom-appimage

    # Fonts
    fira-code
    font-awesome_6
    nerd-fonts.symbols-only
    noto-fonts
  ];
}
