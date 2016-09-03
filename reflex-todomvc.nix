{ mkDerivation, base, containers, file-embed, ghcjs-dom, mtl
, reflex, reflex-dom, stdenv, text
}:
mkDerivation {
  pname = "reflex-todomvc";
  version = "0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers file-embed ghcjs-dom mtl reflex reflex-dom text
  ];
  executableHaskellDepends = [ base ];
  description = "Functional Reactive TodoMVC";
  license = stdenv.lib.licenses.bsd3;
}
