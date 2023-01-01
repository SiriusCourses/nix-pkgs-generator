# The shake-based build for nix

This is a repository for the nix files that are common a number of projects.
The purpose of the repo is to keep track of the dependencies, security updates
and package versions of the entire repo.

The repository contains `nix` that can be included into any repository and scripts that are used to generate nix file and update it accordingly.

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
[nix-shell:nixpkgs]$ rm ./nix/default.nix ./nix/pkgs/haskell/*
```

Inputs:

* `packages.yaml` -- list of the packages controlled by the system
* `repo.yaml` -- list of repositories that `packages.yaml` can link to.

For example:

In the `repo.yaml` we may have a definition of the repo:

```yaml
"cheopslab/nixpkgs":
  git:    "https://youruser:yourtoken/cheopslab/nixpkgs"
  rev:    "994d82c619b9a113638b571aa2d880ab7ca5395a"
  sha256: "0rshd33xxar09y83isdi8xyxik42sx54hn6z6kp52yd2iqgq044v"
```

Then we can use it in the `packages.yaml` files

```yaml
nixpkgs:
  repo:    "cheopslab/prometheus-haskell"
  subpath: "wai-middleware-prometheus"
```

Then we can change the versions only in the `packages.yaml` file.

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

Still **TBD**...

## Dispalying diffs between version on hackage and local

Phony target `list-new` shows list of packages which are newer on hackage than
version used in our snapshot. It separately list packages which differ only in
pathc revision.

It order to display diff between latest version of package and package in
stapshot phony target `diff@package-name` could be used.

Still **TBD**...
