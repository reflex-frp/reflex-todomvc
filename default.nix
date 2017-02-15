{ mkDerivation, reflex, reflex-dom, file-embed, cabal-macosx, jsaddle-warp, jsaddle-webkit2gtk, jsaddle-wkwebview, ghc, stdenv, darwin, osx_sdk
}:

mkDerivation {
  pname = "reflex-todomvc";
  version = "0.1";
  src = builtins.filterSource (path: type: !(builtins.elem (baseNameOf path) [ ".git" "dist" ])) ./.;
  isExecutable = true;
  isLibrary = true;
  buildDepends = [
    reflex
    reflex-dom
    file-embed
    cabal-macosx
  ] ++ (if ghc.isGhcjs or false then [
  ] else if stdenv.isDarwin then [
    jsaddle-wkwebview
#    jsaddle-webkit2gtk
    jsaddle-warp
    darwin.cctools
    darwin.libobjc
    darwin.apple_sdk.libs.xpc
    osx_sdk
  ] else [
    jsaddle-webkit2gtk
    jsaddle-warp
  ]);
  license = null;
}
