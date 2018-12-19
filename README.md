# rings.oneloop
Oneloop massless scalar and tensor integral reduction by means of Tarasov's recurrence relations (adapted from O. V. Tarasov's Maple codes).

## Standalone

```
oneloop v1.0

Usage: oneloop i2|i3|i4|i5 [--di <shift>] --n1 <n1> --n2 <n2>  ...
Options:
  -h, --help      Show help message
  -v, --version   Show version of this program

Subcommand: i2 - Computes massless 2-point integral I2[s12] in (d + di) dimensions
      --n1  <arg>               Exponent of the first propagator
      --n2  <arg>               Exponent of the second propagator
      --di  <arg>               Dimension shift (must be even)

  -c, --characteristic  <arg>   Characteristic of the used ring
      --database  <arg>         Alternative path to database file
  -f, --factorize               Factorize coefficient at each integral summand
  -i, --indices  <arg>          Tensor indices
      --no-database             Disable use of database (each expression will be
                                calculated from scratch)
  -o, --output-format  <arg>    Format of output. Possible values: FORM, MMA,
                                Maple. Default is MMA.
      --s12  <arg>              Optional value for kinematic invariant s12
  -t, --table-print             Print each summand of the result on a new line
  -h, --help                    Show help message
Subcommand: i3 - Computes massless 3-point integral I3[s23, s13, s12] in (d + di) dimensions
      --n1  <arg>               Exponent of the first propagator
      --n2  <arg>               Exponent of the second propagator
      --n3  <arg>               Exponent of the third propagator
      --di  <arg>               Dimension shift (must be even)

  -c, --characteristic  <arg>   Characteristic of the used ring
      --database  <arg>         Alternative path to database file
  -f, --factorize               Factorize coefficient at each integral summand
  -i, --indices  <arg>          Tensor indices
      --no-database             Disable use of database (each expression will be
                                calculated from scratch)
  -o, --output-format  <arg>    Format of output. Possible values: FORM, MMA,
                                Maple. Default is MMA.
      --s12  <arg>              Optional value for kinematic invariant s12
      --s13  <arg>              Optional value for kinematic invariant s13
      --s23  <arg>              Optional value for kinematic invariant s23
  -t, --table-print             Print each summand of the result on a new line
  -h, --help                    Show help message
Subcommand: i4 - Computes massless 4-point integral I4[s12, s23, s34, s14, s24, s13] in (d + di) dimensions
      --n1  <arg>               Exponent of the first propagator
      --n2  <arg>               Exponent of the second propagator
      --n3  <arg>               Exponent of the third propagator
      --n4  <arg>               Exponent of the fourth propagator
      --di  <arg>               Dimension shift (must be even)

  -c, --characteristic  <arg>   Characteristic of the used ring
      --database  <arg>         Alternative path to database file
  -f, --factorize               Factorize coefficient at each integral summand
  -i, --indices  <arg>          Tensor indices
      --no-database             Disable use of database (each expression will be
                                calculated from scratch)
  -o, --output-format  <arg>    Format of output. Possible values: FORM, MMA,
                                Maple. Default is MMA.
      --s12  <arg>              Optional value for kinematic invariant s12
      --s13  <arg>              Optional value for kinematic invariant s13
      --s14  <arg>              Optional value for kinematic invariant s14
      --s23  <arg>              Optional value for kinematic invariant s23
      --s24  <arg>              Optional value for kinematic invariant s24
      --s34  <arg>              Optional value for kinematic invariant s24
  -t, --table-print             Print each summand of the result on a new line
  -h, --help                    Show help message
Subcommand: i5 - Computes massless 5-point integral I5[s12, s23, s34, s45, s15, s13, s14, s24, s25, s35] in (d + di) dimensions
      --n1  <arg>               Exponent of the first propagator
      --n2  <arg>               Exponent of the second propagator
      --n3  <arg>               Exponent of the third propagator
      --n4  <arg>               Exponent of the fourth propagator
      --n5  <arg>               Exponent of the fifth propagator
      --di  <arg>               Dimension shift (must be even)

  -c, --characteristic  <arg>   Characteristic of the used ring
      --database  <arg>         Alternative path to database file
  -f, --factorize               Factorize coefficient at each integral summand
  -i, --indices  <arg>          Tensor indices
      --no-database             Disable use of database (each expression will be
                                calculated from scratch)
  -o, --output-format  <arg>    Format of output. Possible values: FORM, MMA,
                                Maple. Default is MMA.
      --s12  <arg>              Optional value for kinematic invariant s12
      --s13  <arg>              Optional value for kinematic invariant s13
      --s14  <arg>              Optional value for kinematic invariant s14
      --s15  <arg>              Optional value for kinematic invariant s15
      --s23  <arg>              Optional value for kinematic invariant s23
      --s24  <arg>              Optional value for kinematic invariant s24
      --s25  <arg>              Optional value for kinematic invariant s25
      --s34  <arg>              Optional value for kinematic invariant s24
      --s35  <arg>              Optional value for kinematic invariant s25
      --s45  <arg>              Optional value for kinematic invariant s25
  -t, --table-print             Print each summand of the result on a new line
  -h, --help                    Show help message
```

## Mathematica interface

Install MMA wrapper using

```
Import["https://raw.githubusercontent.com/PoslavskySV/rings.oneloop/develop/mma/Install.m"]; DoInstall[];
```

Then run e.g. 

```
<< oneloop`
I3[1, 1, 1, 2, Factorize -> True, Characteristic -> 0, Indices -> "mn"]
```
