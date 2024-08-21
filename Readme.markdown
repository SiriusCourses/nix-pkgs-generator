# The shake-based build for nix

This is a repository for the nix files that are common a number of projects.
The purpose of the repo is to keep track of the dependencies, security updates
and package versions of the entire repo.

The repository contains `nix` that can be included into any repository and
scripts that are used to generate nix file and update it accordingly.

## To run the build

Before you continue please make sure that you have all updated your nix environment. See details [here](https://nixos.org/manual/nix/stable/installation/upgrading.html).

Run to prepare the shell:

```console
user@somewhere$ nix-shell
```

Collect packages locally when shell will be prepared:

```console
[nix-shell:nixpkgs]$ runhaskell ./hackage.hs
```

To run test build `cheops-*` packages:

```console
[nix-shell:nixpkgs]$ nix-build ./build.nix
```

To cleanup your local environment:

```console
[nix-shell:nixpkgs]$ runhaskell ./hackage.hs clean
```

Inputs are read from several YAML files. During import file names are sorted
alphanumerically and value from later files take precedence

* `packages*.yaml` -- list of the packages controlled by the system
* `repo*.yaml` -- list of repositories that `packages.yaml` can link to.
* `config*.yaml` -- setting common for all packages.
  - `revision` hackage revision.
  - `ghc_version` GHC version to be used by `cabal2nix`. Generated files could
    be different for different GHC versions.
  - `profile` -- boolean flag on whether compile libraries with or without
    profiling support. Default is false.
  - `haddock` -- boolean flag on whether to build haddocks. Default is false.
  - `tests` -- boolean flag on whether to run tests when building
    package. Default is false.

For example:

In the `repo.yaml` we may have a definition of the repositories to which package
definitions may refer:

```yaml
"cheopslab/cheops-logger":
  git:    "https://github.com/cheopslab/cheops-logger"
  rev:    "3f23c216607f804a5fbd8c64df4e1056701e1f84"
```

`packages.yaml` contain dictionary of with definitions of packages for overlay.
Packages from hackage could be specified either in full or abbreviated form:

```yaml
cborg: "0.2.8.0"
vector:
  hackage: "0.13.0.0"
```

Package which are fetched from git repository either specify git repository
directly:

```yaml
"cheopslab/nixpkgs":
  git:    "http://github.com/Shimuuar/math-functions.git"
  rev:    "f746f282a16a0cf407a0a2a90c9797decd878ec3"

```

or could refer to repository defined in `repo.yaml` file:

```yaml
wai-middleware-prometheus:
  repo:    "cheopslab/cheops-logger"
  subpath: "cheops-logger"
```

in both case `subpath` could be specified which tell `cabal2nix` that source is
located in subpath of repository.

For all packages one could specify `parameters` which allows to pass parameters
to `callPackage`.

```yaml
splitmix:
  hackage:    "0.1.0.4"
  parameters: ["testu01=null"]
```

This is to allow to override dependencies of package. For example splitmix has
dummy dependency `testu01`. This allows to provide dependencies.
```
  splitmix = ... (prev.callPackage ./pkgs/haskell/splitmix.nix { testu01=null; });
```

* `patches` - contains patches that are applied to the nix files (will be modified in the future)

Notes:

Shake database is not stored in git so any checkut will rebuild nix
completely. This process will take some time. It's not clear whether it's safe
to store DB in version control system.

## How to test build

Add interesting packages in `build.nix`:

```
in pkgs.cheopslab.haskellPackages.ghcWithPackages (p: with p; [
   # PUT packages list here
])
```

and add `nix build` command to your CI script.


## How to update packages.yaml

At some point you will want to update to newer set of packages of to newer
GHC. Process of doing so is depressingly manual. Easiest approach is to use
cabal solution:

1. Create `cabal.project` in new directory list all packages that are fetched
   from git there.

2. Run `cabal build all --only-configure` to generate the build plan. 
   Make sure to use same GHC version since build plan *depends*
   on it.

3. Run `./sh/extract-cabal.sh path/to/project_dir`. It will produce list of
   packages that are fetched from hackage. Use it to update `packages.yaml`.


## Dispalying diffs between version on hackage and local

Phony target `list-new` shows list of packages which are newer on hackage than
version used in our snapshot. It separately list packages which differ only in
pathc revision.

It order to display diff between latest version of package and package in
stapshot phony target `diff@package-name` could be used.

Still **TBD**...
