

{ config, pkgs, fetchFromGitHub, ... }:

{

  # Require Emacs to be installed
  environment.systemPackages = with pkgs; [
	emacs
  ];

  stdenv.mkDerivation rec {
  name = "spacemacs-mine";
  version = "1.0.0";
  src = fetchFromGitHub {
      # path to the upstream repository
      owner = "syl20bnr";
      repo = "spacemacs";
      rev = "26b8fe0c317915b622825877eb5e5bdae88fb2b2";
      sha256 = "00cfm6caaz85rwlrbs8rm2878wgnph6342i9688w4dji3dgyz3rz";
  };

  dontBuild = true;
}
