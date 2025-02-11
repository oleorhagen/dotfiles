{ config, pkgs, ... }:

{

  networking.interfaces.eth.useDHCP = false;
}
