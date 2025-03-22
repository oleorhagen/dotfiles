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

    # For formatting SQL files
    python312Packages.sqlfmt
    sqlfluff

    # Golang
    go

    # Yaml
    yaml-language-server
    yamlfmt
    # yamlfix # Another formatter
    yamllint
    yq # jq for yaml

    #
    ## Tools
    #
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

    # Network
    whois
    wireshark
    traceroute

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

  ];

  # nixpkgs.config.allowUnfreePredicate =
  #   pkg:
  #   builtins.elem (lib.getName pkg) [
  #     "google-chrome"
  #   ];

  services.tailscale.enable = true;

}
