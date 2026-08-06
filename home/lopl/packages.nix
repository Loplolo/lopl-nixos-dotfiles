{
  pkgs,
  pkgs-stable,
  ...
}: let
  trenchbroom-appimage = pkgs.callPackage ./packages/trenchbroom.nix {};
  paktool = pkgs.callPackage ./packages/paktool.nix {};
  qss-m = pkgs.callPackage ./packages/qss-m.nix {};
  slade = pkgs.callPackage ./packages/slade.nix {};
  xwiimote-mouse-driver = pkgs.callPackage ./packages/xwiimote-mouse-driver.nix {};
in {
  programs.alacritty.enable = true;

  programs.obs-studio = {
    enable = true;
    plugins = with pkgs.obs-studio-plugins; [
      obs-backgroundremoval
      obs-pipewire-audio-capture
      obs-gstreamer
    ];
  };

  home.packages = with pkgs; [
    i7z
    age
    alcom
    appimage-run
    android-tools
    anki
    blockbench
    blueman
    blender
    calibre
    chromium
    vesktop
    distrobox
    drawio
    ericw-tools
    vscode
    cmus
    darkplaces
    direnv
    dbeaver-bin
    fastfetch
    faugus-launcher
    fd
    ffmpeg
    flameshot
    freecad
    freenect
    fteqcc
    fteqw
    element-desktop
    gcc
    gimp
    gnumake
    gnutar
    grim
    gvfs
    guile
    guile-hoot
    guile-reader
    guile-lib
    guile-hall
    guile-commonmark
    guix
    gurobi
    haunt
    gzdoom
    htop
    i3status
    imagemagick
    imv
    inkscape
    ironwail
    jack2
    krita
    kdePackages.kdenlive
    libreoffice-still
    libresprite
    mpv
    nethack
    nicotine-plus
    p7zip
    python3
    networkmanagerapplet
    obsidian
    octave
    (olympus.override {celesteWrapper = "steam-run";})
    openal
    openarena
    openttd
    openvpn
    orca-slicer
    pandoc
    paktool
    pavucontrol
    pkg-config
    pkgs-stable.bottles
    pkgs-stable.wineWow64Packages.staging
    pkgs-stable.winetricks
    umu-launcher
    pkgs-stable.steam-run
    playerctl
    ppsspp
    prismlauncher
    qbittorrent
    qemu
    qmmp
    qss-m
    quakespasm
    quickemu
    r2mod_cli
    reaper
    (retroarch.withCores (cores:
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
      ]))
    slade
    slurp
    ssh-to-age
    strawberry
    sops
    super-productivity
    teams-for-linux
    telegram-desktop
    thunderbird
    tmux
    tree
    trenchbroom-appimage
    ttyper
    typst
    localsend
    unityhub
    unrar
    unzip
    update-nix-fetchgit
    virt-manager
    vscode
    wdisplays
    wike
    wl-clipboard
    xarchiver
    (thunar.override {
      thunarPlugins = [
        thunar-archive-plugin
        thunar-volman
        thunar-media-tags-plugin
      ];
    })
    xonotic-glx
    xournalpp
    xsel
    xwiimote
    xz
    zip
    zotero
  ];
}
