Hacking on reflex-todomvc
=========================
Prerequisites
-------------

A working ghcjs environment with reflex and reflex-todomvc in it. See the [Reflex
Hacking
instructions](https://github.com/reflex-frp/reflex-platform/blob/develop/HACKING.md)
on how to get one.

Reflex-todomvc can be built with ghc and ghcjs. There are two ways to do
this: building with straighg ghcjs and ghc, or building with cabal.

The tradeoffs are:

- Building with cabal involves a little cost. Every time you switch from ghc to ghcjs
and vice versa you have to rembember to run 'cabal --configure
--<compiler>'.

- Building with plain ghc and ghjcs may feel a bit strange when you're
used to building with cabal.  On the upside, you can keep two shells with
their respective nix configuration open and generate ghc and ghcjs
versions as you wish. Probably keeping cabal configured for ghc, so you
can use e.g. `cabal repl` to play with your code, and using plain ghcjs
to see the application in a browser.

Building with just ghcjs and ghc
--------------------------------

Assuming you have cloned [Try
Reflex](https://github.com/reflex-frp/reflex-platform) and carried out the
instructions in
[Hacking](https://github.com/reflex-frp/reflex-platform/blob/develop/HACKING.md),
and are in the `scripts` directory of reflex-platform. Start a ghcjs environment:

```
:~/dev/spikes/reflex-platform$ ./scripts/work-on ghcjs reflex-todomvc
If you have any trouble with this script, please submit an issue at
https://github.com/reflex-frp/reflex-platform/issues

[nix-shell:~/dev/spikes/reflex-platform]$ cd reflex-todomvc/
```

Build with ghcjs. We have to specify the `src` directory where the TodoMVC
library lives, while we build the Main module in `src-bin`. We also add
a few options so `src-bin` only contains our source, not compiler output.

```
[nix-shell:~/dev/spikes/reflex-platform/reflex-todomvc]$ ghcjs -isrc --make
-outputdir dist -o dist/main.jsexe src-bin/main.hs
[2 of 2] Compiling Main             ( src-bin/main.hs, dist/Main.js_o )
Linking dist/main.jsexe (Main,Reflex.TodoMVC)
```

Open `/dist/main.jsexe/index.html` with a webbrowser to see your
program.

For ghc the steps are virtually identical, except the output is
`dist/main` instead of `dist/main.jsexe`.

```
:~/dev/spikes/reflex-platform$ ./scripts/work-on ghc reflex-todomvc
If you have any trouble with this script, please submit an issue at
https://github.com/reflex-frp/reflex-platform/issues

[nix-shell:~/dev/spikes/reflex-platform]$ cd reflex-todomvc/
```

Build with ghc. Like with ghcjs We have to specify the `src` directory.

```
[nix-shell:~/dev/spikes/reflex-platform/reflex-todomvc]$ ghc -isrc --make
-outputdir dist -o dist/main src-bin/main.hs
[2 of 2] Compiling Main             ( src-bin/main.hs, dist/Main.o )
Linking dist/main ...
```

To see the application, just run `./dist/main`.

Building with cabal
-------------------
The first step is the same as for plain ghc and ghcjs - start a nix
shell. Then run `cabal configure --ghcjs` or `cabal configure --ghc`.
Executables can then be found under dist. Each time you switch
compilers, run `cabal configure`.

Assuming you have cloned [Try
Reflex](https://github.com/reflex-frp/reflex-platform) and carried out the
instructions in
[Hacking](https://github.com/reflex-frp/reflex-platform/blob/develop/HACKING.md),
and are in the root of reflex-platform. Start a ghcjs environment:

```
./scripts/work-on ghcjs reflex-todomvc
```

You should see this prompt

```
[nix-shell:~/dev/spikes/reflex-platform]$
```

Go into the reflex-todomvc directory, configure and build with ghcjs.

```
[nix-shell:~/dev/spikes/reflex-platform]$ cd reflex-todomvc/

[nix-shell:~/dev/spikes/reflex-platform/reflex-todomvc]$ cabal configure
--ghcjs
Warning: The package list for 'hackage.haskell.org' is 42.0 days old.
Run 'cabal update' to get the latest list of available packages.
Resolving dependencies...
Configuring reflex-todomvc-0.1...

[nix-shell:~/dev/spikes/reflex-platform/reflex-todomvc]$ cabal build
Building reflex-todomvc-0.1...
Preprocessing library reflex-todomvc-0.1...
In-place registering reflex-todomvc-0.1...
Preprocessing executable 'reflex-todomvc' for reflex-todomvc-0.1...
[1 of 1] Compiling Main             ( src-bin/main.hs,
dist/build/reflex-todomvc/reflex-todomvc-tmp/Main.js_dyn_o )
Linking dist/build/reflex-todomvc/reflex-todomvc.jsexe (Main)
```

You can open `index.html` in
`dist/build/reflex-todomvc/reflex-todomvc.jsexe` with a browser to see
the application in action.

Building with ghc is very similar. `cabal configure --ghc && cabal
build`. The executable will be in
`./dist/build/reflex-todomvc/reflex-todomvc`.
