# crypto-dca-calculator

[![crypto-dca-calculator Build Status](https://github.com/prikhi/crypto-dca-calculator/actions/workflows/main.yml/badge.svg)](https://github.com/prikhi/crypto-dca-calculator/actions/workflows/main.yml)


Generate a series of amounts & prices for creating laddered cryptocurrency buy
orders.

Requires [`stack`][get-stack]:

```sh
stack run
stack run -- --help
```

[get-stack]: https://docs.haskellstack.org/en/stable/README/


## Install

You can install the CLI exe by running `stack install`. This lets you call the
executable directly instead of through stack:

```sh
stack install
export PATH="${HOME}/.local/bin/:${PATH}"
crypto-dca-calculator
```


## Build

You can build the project with stack:

```sh
stack build
```

For development, you can enable fast builds with file-watching,
documentation-building, & test-running:

```sh
stack test --haddock --fast --file-watch --pedantic
```

To build & open the documentation, run:

```sh
stack haddock --open crypto-dca-calculator
```


## LICENSE

BSD-3
