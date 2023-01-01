self: super:
  {
   /* Find all project packages and make a set for adding them to the build set.
    *
    * This function is inteded to be used with overrideSourcePackages, cheopsSourcePackages
    * or similar
    *
    * Type: lookupCabalProjectPackages :: Path -> Map Name Path
    *
    * Example:
    *
    *   cabal.project
    *     packages/a
    *     b
    *
    * lookupCabalProjectPackages ./.
    *
    *   => { a = cleanSource (./packages/a) ; b = cleanSource (./b); }
    *
    */
   lookupCabalProjectPackages = path:
      super.lib.pipe (builtins.readFile (path + "/cabal.project"))
        [ # Load all packages from the cabal.project file
          (builtins.match ".*^packages:(.*)\n[^ ].*")
          # We need only the first matching group
          builtins.head
          # We split the text by lines
          (builtins.split "\n")
          # There are some lists in the result, so we filter them out
          (builtins.filter builtins.isString)
          # Now we remove all spaces
          (builtins.map (builtins.replaceStrings [" "] [""]))
          # and empty names
          (builtins.filter (x: x != ""))
          # for each package we prepare an entry in the attribute list
          (builtins.map (name: { name = builtins.baseNameOf name; value = super.lib.cleanSource (path + "/${name}");}))
          # and return a desired attribute list
          builtins.listToAttrs
        ];

   /* Make a checkout source packages from the package contents.
    *
    * Type: Map Path Path -> Map Path HaskellPackageDerivation
    *
    * We do several things:
    *   - strip file
    *   - disable profiling (at least for now)
    *   - disable haddock
    *   - use additional options for GHC so it can use more memory while building pkg
    */
    cheopsSourceOverrides = overrides: cself: csuper:
      super.lib.mapAttrs
        (name: src:
          super.haskell.lib.doStrip(
          super.haskell.lib.disableLibraryProfiling (
          super.haskell.lib.dontHaddock (
          super.haskell.lib.appendConfigureFlags
            (cself.callCabal2nix name src {})
            [ "--ghc-option=+RTS" "--ghc-option=-A128M" "--ghc-option=-RTS" ]))))
        overrides;
  }

