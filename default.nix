let nix-haskell = import ../../default.nix {};
    project = import ./project.nix;
in nix-haskell project
