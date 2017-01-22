# bobkonf2017

Tutorial at BOB Konferenz 2017

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
