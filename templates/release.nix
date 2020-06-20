let
  bootstrap = import <nixpkgs> { };

  nixpkgs = builtins.fromJSON (builtins.readFile ./nixpkgs.json);

  src = bootstrap.fetchFromGitHub {
    owner = "NixOS";
    repo  = "nixpkgs";
    inherit (nixpkgs) rev sha256;
  };

  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          templates =
            haskellPackagesNew.callPackage ./templates.nix { };

          lucid-lego =
            haskellPackagesNew.callPackage ./../../lucid-lego/lucid-lego.nix { };
        };
      };
    };
  };

  pkgs = import src { inherit config; };
in
  { templates = pkgs.haskellPackages.templates;
  }

