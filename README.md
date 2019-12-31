# TodoMVC in Reflex - [Try It!](http://todomvc.reflex-frp.org/)

All of the code lives in `src/Reflex/TodoMVC.hs`.
`style.css` is embedded into the application.

## Build Instructions

```bash
nix-build -E '(import dep/reflex-platform {}).ghcjs.callPackage ./. {}'
cd result/bin/reflex-todomvc.jsexe
```

Then open `index.html` in your browser!
