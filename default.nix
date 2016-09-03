{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghcjs" }:
	    nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./reflex-todomvc.nix
	   { }
