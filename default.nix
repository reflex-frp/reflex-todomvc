{ mkDerivation, reflex, reflex-dom, reflex-dom-core, file-embed, cabal-macosx, jsaddle-wasm, jsaddle-warp, jsaddle-webkit2gtk, jsaddle-wkwebview, ghc, stdenv, darwin
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
    reflex-dom-core
  ] ++ stdenv.lib.optional (!stdenv.hostPlatform.isWasm) reflex-dom
  ++ (if ghc.isGhcjs or false then [
  ] else if stdenv.hostPlatform.isiOS then [
    jsaddle-wkwebview
    buildPackages.darwin.apple_sdk.libs.xpc
    (buildPackages.osx_sdk or null)
  ] else if stdenv.hostPlatform.isMacOS then [
    jsaddle-wkwebview
    jsaddle-warp
  ] else if stdenv.hostPlatform.isWasm then [
    jsaddle-wasm
  ] else [
    jsaddle-webkit2gtk
    jsaddle-warp
  ]);
  postInstall = stdenv.lib.optionalString (ghc.isGhcjs or false) ''
    rm "$out/bin/reflex-todomvc" || true # This is not designed to be run from node, so don't let it be
  '';
  license = null;
}
