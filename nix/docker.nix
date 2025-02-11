{ config, pkgs, ... }:

{

  virtualisation.docker.enable = true;

  users.users.oleorhagen.extraGroups = [ "docker" ];
}
