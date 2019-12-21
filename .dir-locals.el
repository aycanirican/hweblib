((haskell-mode
  (dante-repl-command-line "nix-shell" "--run" "cabal repl lib:hweblib --builddir=dist/dante")
  (dante-target . "lib:hweblib")))

