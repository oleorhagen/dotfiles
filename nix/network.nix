{ config, pkgs, ... }:

{

  # networking.interfaces.eth.useDHCP = false;
  # networking.extraHosts = "127.0.0.1 skjeberg-invitational.com";

  # Enable mDNS for .local address resolution
  services.avahi = {
    enable = true;
    nssmdns4 = true; # Enable mDNS for IPv4
    nssmdns6 = true; # Enable mDNS for IPv6
    publish = {
      enable = true;
      addresses = true;
      domain = true;
      hinfo = true;
      userServices = true;
      workstation = true;
    };
  };

}
