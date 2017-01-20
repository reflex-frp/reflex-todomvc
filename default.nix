{ mkDerivation, reflex, reflex-dom, file-embed, cabal-macosx, jsaddle-warp, jsaddle-webkit2gtk, jsaddle-wkwebview, ghc, stdenv
}:

mkDerivation {
  pname = "reflex-todomvc";
  version = "0.1";
  src = builtins.filterSource (path: type: baseNameOf path != ".git") ./.;
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
  ] else [
    jsaddle-webkit2gtk
    jsaddle-warp
  ]);
  license = null;
}
