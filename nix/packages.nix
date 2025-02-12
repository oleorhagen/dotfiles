{ config, pkgs, ... }:

{

  environment.systemPackages = with pkgs; [
    # Man pages (Duh!)
    man-db

    firefox-wayland
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

    # Golang
    go

    # Yaml
    yaml-language-server
    yamlfmt
    # yamlfix # Another formatter
    yamllint

    #
    ## Tools
    #
    binutils
    gcc
    clang

    # Encryption
    sops

    # Search
    ripgrep
    postgresql # Really only need psql

    # Network
    whois
    wireshark

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

  services.tailscale.enable = true;

}
