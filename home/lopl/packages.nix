{pkgs, ...}: {
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
    bottles
    calibre
    chromium
    distrobox
    drawio
    vscode
    cmus
    direnv
    darkplaces
    dbeaver-bin
    ericw-tools
    fastfetch
    ffmpeg
    flameshot
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
    haunt
    gzdoom
    htop
    i3status
    imagemagick
    imv
    inkscape
    ironwail
    jack2
    jetbrains.idea-oss
    jetbrains.pycharm-oss
    jetbrains.webstorm
    kdePackages.kdenlive
    libreoffice-still
    mpv
    nicotine-plus
    python3
    networkmanagerapplet
    obsidian
    octave
    openal
    openarena
    openttd
    openvpn
    packwiz
    pandoc
    pavucontrol
    pkg-config
    playerctl
    ppsspp
    prismlauncher
    qbittorrent
    qemu
    qmmp
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
      ]))
    (pkgs.rstudioWrapper.override {packages = with pkgs.rPackages; [ggplot2 dplyr xts];})
    slurp
    steam-run
    ssh-to-age
    sops
    teams-for-linux
    telegram-desktop
    thunderbird
    tmux
    tree
    typst
    umu-launcher
    unityhub
    unrar
    unzip
    virt-manager
    vscode
    wdisplays
    wike
    wineWow64Packages.staging
    winetricks
    wl-clipboard
    xarchiver
    (thunar.override {
      thunarPlugins = [
        thunar-archive-plugin
        thunar-volman
        thunar-media-tags-plugin
      ];
    })
    tumbler
    xonotic-glx
    xournalpp
    xz
    zip
    zotero
  ];

  programs.vesktop = {
    enable = true;
    vencord.settings = {
      autoUpdate = true;
      autoUpdateNotification = true;
      notifyAboutUpdates = true;
    };
  };
}
