{
  description = "TodoMVC in Reflex";

  outputs = { self, nixpkgs }: {
    defaultPackage.x86_64-linux =
      with import nixpkgs { system = "x86_64-linux"; };
      haskellPackages.callPackage ./. {};
  };
}
