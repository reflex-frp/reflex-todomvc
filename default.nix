{ mkDerivation, reflex, reflex-dom, file-embed, cabal-macosx
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
  ];
  license = null;
}
