{ config, pkgs, ... }:

{

  environment.systemPackages = with pkgs; [

    # Zsh plugins
    zsh-autoenv

    # Man pages (Duh!)
    man-db

    # Browsers
    firefox-wayland
    google-chrome

    git

    # Editor setup
    emacs
    # Language server for the Nix layer
    nixd # https://github.com/nix-community/nixd
    # Prettify nix code
    nixfmt-rfc-style
    # Prettify html and js
    nodePackages.prettier

    # docker
    docker

    # Utils
    unzip
    jq
    tree
    file
    bind # dig
    w3m # Text web-browser

    # Debug
    wireshark

    # GTK conf
    lxappearance

    # Local directory setups
    direnv

    #
    # DevOps
    #

    # k8s
    kubectl
    kubectx
    kubernetes-helm

    # FluxCD
    fluxcd

    # Utils
    fzf # Fuzzy finder on the cli

    # Pass password manager
    pass

    # Backups
    restic

    # VPN
    tailscale # <3

    #
    ## Tools and Utils
    #

    # Python

    # Formatter
    black

    # CLI tools 4 openssl
    openssl

    # IRC reader
    irssi

    # Log viewer
    lnav

    # QR code creator
    qrencode

    gnucash # Accounting software

    # Minio
    minio-client

    #
    # Languages
    #
    python3Full

    # Virtualenv manager
    pipenv

    # For formatting SQL files
    python312Packages.sqlfmt
    sqlfluff

    # Golang
    go
    gopkgs
    gotools
    gopls

    # Yaml
    yaml-language-server
    yamlfmt
    # yamlfix # Another formatter
    yamllint
    yq # jq for yaml

    #
    ## Tools
    #
    gnumake
    binutils
    gcc
    clang
    clang-tools

    # Encryption
    sops

    # Search
    ripgrep
    silver-searcher # Also known as ag
    postgresql # Really only need psql
    perl540Packages.TAPParserSourceHandlerpgTAP # postregresql unit testing using pg_prove

    # Network
    whois
    wireshark
    traceroute
    tcpdump

    #
    # # NixOS Build
    #

    # NixOS build debugging tool
    cntr

    #
    ## Sway
    #
    swaylock

    # Screenshot
    sway-contrib.grimshot

    # Alacritty themes
    alacritty-theme

    # Video
    wf-recorder # Screen recording utility
    ffmpeg # Multimedia swiss army knife
    vlc # Video player

  ];

  # nixpkgs.config.allowUnfreePredicate =
  #   pkg:
  #   builtins.elem (lib.getName pkg) [
  #     "google-chrome"
  #   ];

  services.tailscale.enable = true;

}
