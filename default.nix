{ cabal, reflex, reflexDom, MissingH, fileEmbed
}:

cabal.mkDerivation (self: {
  pname = "reflex-todomvc";
  version = "0.1";
  src = ./.;
  buildDepends = [
    reflex
    reflexDom
    MissingH
    fileEmbed
  ];
  meta = {
    description = "Functional Reactive TodoMVC application";
    license = self.stdenv.lib.licenses.unfree;
    platforms = self.ghc.meta.platforms;
  };
})
