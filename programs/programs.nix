{pkgs, ...}: {
  environment.systemPackages = with pkgs; [
    ghostty

    usbutils
    uhubctl
    coreutils-full

    clang
    lldb

    localsend

    sakura
    alacritty

    kiwix

    efibootmgr

    kdePackages.filelight
    komikku

    torrential
    unzip
    zip
    appimage-run
    media-downloader

    notesnook
    obsidian
    logseq

    heroic

    ppsspp
    mgba
    pcsx2

    podman-compose
    distrobox
    boxbuddy

    wget
    git
    helix
    love
    lua
    lua-language-server

    nixd

    brave
  ];
}
