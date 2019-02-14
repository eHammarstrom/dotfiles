# Config setup

## clangd

`-DCMAKE_EXPORT_COMPILE_COMMANDS=ON` [see](https://github.com/Sarcasm/irony-mode#compilation-database).

## nixos notes

### display package dependency tree

`nix-store -q --tree $(nix-build -A xorg.xinit  '<nixpkgs>' )`

### why does x depend on y?

`nix why-depends y x`
