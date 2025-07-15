{ config, pkgs, ... }:

{

  networking.interfaces.eth.useDHCP = false;
  networking.extraHosts = "127.0.0.1 skjeberg-invitational.com";

}
