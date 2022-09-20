{ mkDerivation, lib, reflex, reflex-dom, file-embed, cabal-macosx, jsaddle-warp, jsaddle-webkit2gtk, jsaddle-wkwebview, ghc, stdenv, darwin
, buildPackages
}:

mkDerivation {
  pname = "reflex-todomvc";
  version = "0.1";
  src = builtins.filterSource (path: type: !(builtins.elem (baseNameOf path) [ ".git" "dist" ])) ./.;
  isExecutable = true;
  isLibrary = true;
  buildTools = [
    cabal-macosx
  ];
  buildDepends = [
    reflex
    reflex-dom
    file-embed
  ] ++ (if ghc.isGhcjs or false then [
  ] else if stdenv.hostPlatform.isiOS then [
    jsaddle-wkwebview
    buildPackages.darwin.apple_sdk.libs.xpc
    (buildPackages.osx_sdk or null)
  ] else if stdenv.hostPlatform.isMacOS then [
    jsaddle-wkwebview
    jsaddle-warp
  ] else [
    jsaddle-webkit2gtk
    jsaddle-warp
  ]);
  postInstall = lib.optionalString (ghc.isGhcjs or false) ''
    rm "$out/bin/reflex-todomvc" || true # This is not designed to be run from node, so don't let it be
  '';
  license = null;
}
