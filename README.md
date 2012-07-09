# A Starter Haskell Project

This is my attempt at a very basic template for new Haskell projects,
including a `cabal` file and the beginnings of a testing harness.
Additionally, as an exercise of using Haskell for scripting with
minimal dependencies, there is Init.hs, which prompts for a few fields
and fills in placeholders across the project template.

## Setting up Haskell

I found that the most suitable setup for me was installing GHC along
with cabal-dev, and nothing else at the system level (so, no Haskell
Platform).

    `brew install ghc cabal-install`
    `cabal install cabal-dev`

Haskell has great library management tools (via Cabal and
`cabal-install`), but suffers a similar problem that ruby's gems do,
in that by default libraries are installed into a global namespace,
and different versions have a habit of stomping on each other.
(Whereas ruby's problems stem from the fact that the require mechanism
is not version aware, Haskell has difficulty with transitive
dependency version mismatches.) Thankfully, there are a few tools that
help solve this issue, mostly by allowing you to easily restrict
installing dependencies to a per-project environment. I found
`cabal-dev` to be the most straightforward to use, though there is
also `hsenv` to check out.

## Development Workflow

The second larger unknown for me was simply, what's the fastest, most
common workflow used for Haskell development with Cabal? `cabal test`
is somewhat useless as a command, as it does not invoke its
dependencies. Arguably better, but somewhat more confusing, running
`cabal-dev install --enable-tests` will run the configured test-suite.
