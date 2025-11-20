{
  config,
  pkgs,
  lib,
  ...
}:

{

  environment.systemPackages = with pkgs; [
    grim # Screenshot
    slurp # Screenshot
    swaylock
    swayidle
    waybar # Top line program visualizer
    wl-clipboard
    mako # Notification system
    dmenu # program launcher
    wofi # For selection with dmenu
    alacritty # Terminal emulator
    zsh # We use zsh by default
    oh-my-zsh
    powerline-fonts

    starship # Terminal prompt generator (type fancy)
  ];

  environment.sessionVariables = {
    MOZ_ENABLE_WAYLAND = "1";
    XDG_CURRENT_DESKTOP = "sway";
  };

  services.gnome.gnome-keyring.enable = true;

  programs.sway = {
    enable = true;
    wrapperFeatures.gtk = true;
  };

  # We need zsh, as our alacritty config has it as the default
  # programs.zsh.enable = true;
  programs.zsh = {
    ohMyZsh = {
      enable = true;
      # plugins = [
      #   "git"
      #   "thefuck"
      # ];
      theme = "sammy"; # Or robbyrussel
    };
  };

  # Set it as the default for all users
  users.defaultUserShell = pkgs.zsh;
  # Add Zsh to /etc/shells
  environment.shells = with pkgs; [ zsh ];

  #
  # # Font setup
  #
  fonts.enableDefaultPackages = true; # when set to true, causes some "basic" fonts to be installed for reasonable Unicode coverage.
  # Some fonts of my liking
  fonts.packages =
    with pkgs;
    [
      noto-fonts
      noto-fonts-cjk-sans
      noto-fonts-color-emoji
      liberation_ttf
      fira-code
      fira-code-symbols
      mplus-outline-fonts.githubRelease
      dina-font
      proggyfonts
      source-code-pro
      ubuntu-classic
      liberation_ttf
      font-awesome
    ]
    ++ builtins.filter lib.attrsets.isDerivation (builtins.attrValues pkgs.nerd-fonts);

}
