# A Starter Haskell Project

This is my attempt at a very basic template for new Haskell projects,
including a `cabal` file and the beginnings of a testing harness.
Additionally, as an exercise of using Haskell for scripting with
minimal dependencies, there is Init.hs, which prompts for a few fields
and fills in placeholders across the project template.

## Up and Running

I found that the most suitable setup for me was installing GHC along
with cabal-dev, and nothing else at the system level (so, no Haskell
Platform).

    $ brew install ghc cabal-install
    $ cabal install cabal-dev

Haskell has pretty good library management tools (via Cabal and
`cabal-install`), but suffers a similar problem that ruby's gems do,
in that by default libraries are installed into a global namespace,
and different versions have a habit of stomping on each other.

Thankfully, 'cabal-dev' mostly solves this issue, by installing
dependencies in a per-project environment. Although, I found
`cabal-dev` to be the most straightforward to use, there are a few
other similar tools worth checking out, such as `hsenv` or `capri`.

## Development Workflow

This was somewhat unintuitive when starting out. With ruby projects,
there's `rake test` or `rake spec`, with Scala there's `sbt test`. The
current state of Haskell seems to be different with every project.

For now, the best one-liner seems to be

    $ cabal-dev install --enable-tests

Additionally, cabal-dev's ghci wrapper is nice. Start up ghci with

    $ cabal-dev ghci

Then at the prompt

    > runTests

Or simply:

    > :main
