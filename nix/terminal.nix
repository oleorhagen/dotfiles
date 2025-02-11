{ config, pkgs, ... }:

{

  environment.systemPackages = with pkgs; [
    cowsay # Make the cow say funny
    lolcat # Pretty rainbow colors of the output
  ];

}
