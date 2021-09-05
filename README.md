# crypto-dca-calculator

[![crypto-dca-calculator Build Status](https://github.com/prikhi/crypto-dca-calculator/actions/workflows/main.yml/badge.svg)](https://github.com/prikhi/crypto-dca-calculator/actions/workflows/main.yml)


Generate a series of amounts & prices for creating laddered cryptocurrency buy
orders.

Requires [`stack`][get-stack]:

```sh
# Spend $100 in 4 steps of -0.25% at a current price of $50,000
$ stack run 0.25 4 50000 100
+--------++------------+----------+
|        ||     Amount |    Price |
+========++============+==========+
| -0.25% || 0.00050125 | 49875.00 |
| -0.50% || 0.00050251 | 49750.00 |
| -0.75% || 0.00050378 | 49625.00 |
| -1.00% || 0.00050505 | 49500.00 |
+========++============+==========+
|  Total || 0.00201259 |          |
|    Avg ||            | 49687.11 |
+--------++------------+----------+
$ stack run -- --help
```

TODO:

* Add `amount-precision` & `price-precision` CLI params for controlling
  precision of printed values.
* Add Coinbase/Binance integrations for auto-placing orders.


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
