# bobkonf2017

Tutorial at BOB Konferenz 2017 [![Build Status](https://travis-ci.org/larsrh/bobkonf2017.svg?branch=master)](https://travis-ci.org/larsrh/bobkonf2017)

## Overview

The main working file for the workshop is `Main.hs` in the root of the repository.
The aim is ``blackbox testing'':
Tests should be written for an unknown implementation of a specification.
The specification is given in `Main.hs`; the implementation resides in the `src` folder.
It is recommended to _not_ look at the source code.

## Installation instructions

The easiest way to get started is to [install Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/).
Make yourself familiar with their instructions for your platform (there are specific instructions for Linux flavours, Windows, and macOS).

## Build instructions

Run the following commands in your shell and you should be ready to go in no time:

```
$ stack setup
$ stack build
$ stack test
```

## Development

Stack is able to rebuild on save.
Use this to accelerate the edit-save-recompile-test cycle:

```
$ stack test --file-watch
```
