
# Package reflex-dom-htmlea-vs

Vector-sized trials working with the 
[reflex-dom-htmlea](https://github.com/gspia/reflex-dom-htmlea) -library.

We move some code from reflex-dom-htmlea here.


## Usage

First, get the repo with `git clone` and `cd` into the directory, and after that make sure that the reflex-platform is in place:

```
git submodule update --init --recursive
```

To build with GHC, use the nix-shell command to enter the sandbox shell and use cabal (which is supplied by the sandbox):

```
nix-shell -A shells.ghc
cabal new-build all
```

To build with GHCJS:

```
nix-shell -A shells.ghcjs
cabal --project-file=cabal-ghcjs.project --builddir=dist-ghcjs new-build all
```

You can also build examples separately by replacing all with exe:name, e.g.

```
cabal new-build exe:tableEx
cabal --project-file=cabal-ghcjs.project --builddir=dist-ghcjs new-build exe:exampleTbl
```

For further information, see the following
- [project-development documentation](https://github.com/reflex-frp/reflex-platform/blob/develop/docs/project-development.md)
- [blanket project derivation (default.nix)](https://github.com/reflex-frp/reflex-platform/blob/develop/project/default.nix)
- [reflex-project-skeleton](https://github.com/ElvishJerricco/reflex-project-skeleton)

Note that if you have already obtained repo but want to update the 
reflex-platform, you can try, e.g.,

```
git submodule foreach "(git checkout develop; git pull --recurse-submodules)&"
```

(Note that the above command gets the develop-branch of the platform.)


See also the [.ghci](./.ghci). The warp-compilation can be used with ghci.


## TODOs

The list can be seen
at [todo.md](./todo.md).


## Caveats

As this lib is in it's infancy, maybe it is better to be open to naming 
convention and re-structuring suggestions etc. At the moment, some of the 
chosen names for attribute combinators are not consistent with regards to 
each other.

Bugs are more than likely to be found, as this is early work
and there are no test cases (except example-programs).

If you want to compile the examples to android, the singletons-package is a
bit problematic at the moment. 


