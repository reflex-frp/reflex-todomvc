{ config, nix-haskell-patches, ... }:

let nix-thunk = config.importing.nix-thunk;
    deps = with nix-thunk; mapSubdirectories thunkSource ./deps;

in {
  imports = [
    "${nix-haskell-patches}/js/splitmix"
  ];

  name = "reflex-todomvc";
  src = ./.;
  compiler-nix-name = "ghc912";

  extraSrcFiles = {
    library.extraSrcFiles = [
      "static/style.css"
      "reflex-todomvc.app"
    ];
    exes.reflex-todomvc.extraSrcFiles = [
      "static/style.css"
      "reflex-todomvc.app"
      "reflex-todomvc.app/Info.plist"
    ];
  };

  overrides = [
    ({ config, pkgs, lib, ... }: {
      packages = {
        reflex-todomvc.components.exes.reflex-todomvc = {
          frameworks = if (!pkgs.stdenv.targetPlatform.isiOS && pkgs.stdenv.targetPlatform.isDarwin) then [ pkgs.darwin.apple_sdk.frameworks.CoreFoundation ] else [ ];
          postInstall = lib.optionalString (pkgs.stdenv.hostPlatform.isDarwin) ''
            mkdir -p $out/reflex-todomvc.app
            cp -r reflex-todomvc.app $out
            cp $out/bin/reflex-todomvc $out/reflex-todomvc.app
          '';
        };

      };
    })
  ];

  shell = {
    crossPlatforms = ps: with ps; [ ghcjs ];
    packages = ps: with ps; [ reflex-todomvc ];
    withHaddock = false;
    withHoogle = false;
  };

  #haskell-nix.project = {
  #  # haskell.nix specific overrides
  #};

  #reflex-platform.project = {
  #  compiler-nix-name = "ghc8107Splices";
  #  ghcjs-compiler-nix-name = "ghcjs8107JSString";

  #  android = {
  #    executable = ps: exes: (exes (ps.reflex-todomvc)).reflex-todomvc;
  #    executableName = "reflex-todomvc";
  #    applicationId = "org.reflexfrp.todomvc";
  #    displayName = "Reflex TodoMVC";
  #  };
  #  ios = {
  #    executable = ps: exes: (exes (ps.reflex-todomvc)).reflex-todomvc;
  #    executableName = "reflex-todomvc";
  #    bundleIdentifier = "org.reflexfrp.todomvc";
  #    bundleName = "Reflex TodoMVC";
  #  };
  #  web = {
  #    executable = ps: exes: (exes (ps.reflex-todomvc)).reflex-todomvc;
  #  };

  #  thunkInputs = [
  #    {
  #      thunk = ./deps/jsaddle;
  #      subdirs = [
  #        "jsaddle"
  #      ];
  #    }
  #  ] ++ config.reflex-platform.nixpkgs.obsidianCompilers.thunkSets.aeson-1541;

  #  overrides = [
  #    ({ config, pkgs, lib, ... }: {
  #      packages.jsaddle-wkwebview.src = deps.jsaddle + "/jsaddle-wkwebview";
  #      packages.jsaddle-wkwebview.components.library = {
  #        frameworks =
  #          if (pkgs.stdenv.targetPlatform.isiOS) then lib.mkForce [ pkgs.darwin.iosSdkPkgs.sdk pkgs.darwin.apple_sdk.frameworks.CoreFoundation ]
  #          else [ pkgs.darwin.apple_sdk.frameworks.CoreFoundation ];
  #      };
  #    })

  #    ({ config, lib, ... }: {
  #      packages.bitvec.patches = lib.mkForce [ ];
  #    })
  #  ];

  #  android_sdk_accept_license = true;
  #  allowUnfree = true;
  #  doPatch = true;
  #  patches = [
  #    {
  #      url = "https://github.com/obsidiansystems/nixpkgs/commit/d39ee6b7c45deb224d95f717bd1e6e2144e09dd9.diff";
  #      sha256 = "sha256-stn4C43O5M0Qk80gj7YK/87qCDflnm/AwYcOXv5fErI=";
  #    }
  #    {
  #      url = "https://github.com/obsidiansystems/nixpkgs/commit/4516c1a5bb5d11209324bd00239448528bd5fb6d.diff";
  #      sha256 = "sha256-6GyCvZbuquVS++xR68e+jb4IiFPlIbbJb/kmc9uTers=";
  #    }
  #  ];
  #};
}
