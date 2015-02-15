{ mkDerivation, reflex, reflex-dom, file-embed
}:

mkDerivation {
  pname = "reflex-todomvc";
  version = "0.1";
  src = ./.;
  isExecutable = true;
  buildDepends = [
    reflex
    reflex-dom
    file-embed
  ];
  license = null;
}
