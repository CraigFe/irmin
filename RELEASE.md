# Release process

This file documents the necessary steps for releasing Irmin to its various users
(via GitHub, `opam-repository` and Tezos).

At a high level, releasing Irmin consists of publishing the following artefacts:

- a Git [commit tag][git-tags];
- a set of documentation on GitHub pages (e.g. [`mirage.github.io/irmin`][pages-docs]);
- a release archive (`.tbz` file containing the project source) on GitHub;
- a set of `.opam` files in `opam-repository` that point to this release archive.

Most of this can be handled automatically by [`dune-release`][dune-release], as
described in the instructions below.

[git-tags]: https://git-scm.com/book/en/v2/Git-Basics-Tagging
[pages-docs]: https://mirage.github.io/irmin
[dune-release]: https://github.com/ocamllabs/dune-release

## 1. Releasing to opam-repository and GitHub

- Check that no `.opam` files contain `pin-depends` fields. If so, release those
  packages first.

- Make pull-request to development repository containing pre-release changes
  (drop `pin-depends`, add release number to `CHANGES.md`, etc.) and an empty
  commit to host the release tag.

```sh
git fetch upstream
git checkout -B release-X.Y.Z upstream/main
git commit -m "Prepare X.Y.Z release" -- CHANGES.md
git commit --allow-empty -m "Release X.Y.Z"
hub pull-request
```

- Wait for CI to pass on the release PR, then perform the following steps to
  release to `opam-repository`:

```sh
dune-release tag                        #  Create appropriate Git tag by reading CHANGES.md
dune-release distrib --skip-tests       #  Build release archive
dune-release publish distrib --verbose  #  Push release archive to GitHub
dune-release publish doc --verbose      #  Push documentation to GitHub pages
dune-release opam pkg                   #  Generate `opam` files for `opam-repository`
dune-release opam submit                #  Make PR to `opam-repository`
```

- Once the release PR is merged on `opam-repository`, merge the release PR on
  the development repository. If any changes to `.opam` files were necessary in
  the `opam-repository` PR, add those to the release PR before merging.

### 1.1. Re-cutting a failed Opam release

It may be necessary to re-cut an attempted release, for instance if the
`opam-repository` CI raised issues that couldn't be fixed via if the
`opam-repository`.

First delete the release distribution via the GitHub UI, then cleanup the Git
tags and re-perform the required release steps:

```sh
git tag -d X.Y.Z                     #  Erase git tag locally
git push -d upstream X.Y.Z           #  Erase git tag on GitHib
dune-release distrib --skip-tests
dune-release publish doc --verbose   # ... if necessary
dune-release opam pkg
dune-release opam submit ^C          #  Exit at prompt to avoid creating pull request
cd <opam-repository>
git push -u origin --force           #  Add new `.opam` files to PR
```

## 2. Releasing to Tezos' opam-repository

The Tezos project uses [its own `opam-repository`][tezos-opam] to source its
dependencies, so upgrading its dependencies requires making a separate release
to this _after_ having released to the main `opam-repository`.

[tezos-opam]: https://gitlab.com/tezos/opam-repository
