# `rlists`
Because I miss all the nice stuff from
[Haskell's `Data.List`](http://hackage.haskell.org/package/base-4.11.1.0/docs/Data-List.html).

## Usage
Just take the module `rlists.erl` and put it in your project.
There is no application, just a library module.
The rebar3 skeleton is just there for easier compilation and testing).

## Compile
  ```bash
  rebar3 compile
  ```

## Test
   ```bash
   rebar3 eunit
   ```

## Notes
There are no infinite lists in Erlang.
Thus, functions that operate with infinite lists in Haskell will require
an additional input argument stating how many elements are required.

For example,
```erlang
  rlists:iterate(fun(X) -> X * 2 end, 1, 99).
```
is equivalent to
```haskell
  take 99 $ iterate (*2) 1
```
i.e., the resulting list will have the function applied
0, 1, .., 98 times on the initial value (1).