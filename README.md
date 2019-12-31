# TodoMVC in Reflex

All of the code lives in `src/Reflex/TodoMVC.hs`.
`style.css` is embedded into the application.

## Build Instructions

You probably want to use https://nixcache.reflex-frp.org, the official build
artifact cache, so you don't spend hours building dependencies from source when
you run the commands below.  See https://github.com/reflex-frp/reflex-platform
for details.

```bash
nix-build -E '(import dep/reflex-platform {}).ghcjs.callPackage ./. {}'
cd result/bin/reflex-todomvc.jsexe
```

Then open `index.html` in your browser!
