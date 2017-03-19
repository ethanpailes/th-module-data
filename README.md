
[![Build Status](https://travis-ci.org/ethanpailes/th-module-data.svg?branch=master)](https://travis-ci.org/ethanpailes/th-module-data)

About
======

`th-module-data` is a library to allow template haskell authors
to store state in a data log which can span multiple modules. In
order to accomplish this, it outputs data files which mirror the
haskell modules in which the relevant template haskell code
is called. `th-module-data` came about as the result of [this
thread](https://mail.haskell.org/pipermail/libraries/2017-March/027780.html)
on the libraries mailing list.

Using it
==========

The `Env` module provides the highest level interface, exporting a
few TH functions which generate `envGet_<namespace>` and `envPut_<namespace>`
functions. These
function add an environment to the Q monad which can be used to store
types or kinds or whatever else you might wish for.

The `DataLog` module provides a slightly lower level interface,
allowing you to directly append to a log (represented with
a `Vector`).

In order to properly import all the modules you depend on it
is required to call `dataLogInit_<namespace>` before any of
the other functions are invoked. Fortunatly, this funciton is
idempotent, so you can call it as many times as you want without
being worried that it might trample on any state. All the work is
done the first time, and after that it just immediatly aborts.

The code itself has haddock comments, which are a good place to look
for more information.

Unresolved Issues
======================

If there is a way to add `rm -rf .stack-work/th-module-data` to
the `stack clean` command, the README should recomend doing that.
I should add simmilar advice for `cabal`.

Efficent append? Mutable vectors? Should I just give up and use a list?

What is the right name for all these modules?
`Language.Haskell.TH.DataLog`, `Language.Haskell.TH.Env`, and
`Language.Haskell.TH.File`? The last one at least seems a little presumptuous.
