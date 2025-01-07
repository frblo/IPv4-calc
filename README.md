# IPv4-calc

Small program for calculating sizes of IPv4 block allocations. It will generate the first and last address of a given IPv4 block, as well as the number of addresses.

## Execution

The program is built with `cabal`, and can be executed directly or properly compiled first.

Running `cabal run . "82.93.225.78/18"` will generate

```
First address: 82.93.192.0/18
Last address: 82.93.255.255/18
Number of addresses: 16384
```

