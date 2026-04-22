# Groupwise Operations

`groupdiff_tau()` computes \\min(x/y, y/x)\\, i.e. the smallest
symmetric ratio between \\x\\ and \\y\\ that is smaller than 1. If \\x\\
is a vector, the symmetric ratio between all elements in \\x\\ is
computed.

`groupdiff_absdiff()` computes \\max(abs(x-y, y-x))\\, i.e. the smallest
absolute difference between \\x\\ and \\y\\. If \\x\\ is a vector, the
symmetric absolute difference between all elements in \\x\\ is computed.

## Usage

``` r
groupdiff_tau(x)

groupdiff_absdiff(x)

groupdiff_diff(x)
```

## Arguments

- x:

  ([`numeric()`](https://rdrr.io/r/base/numeric.html))  
  Measured performance in group 1, 2, ...

## Value

A single `numeric`.

## Protected Attributes

The protected attribute is specified as a `col_role` in the
corresponding
[`mlr3::Task()`](https://mlr3.mlr-org.com/reference/Task.html):  
`<Task>$col_roles$pta = "name_of_attribute"`  
This also allows specifying more than one protected attribute, in which
case fairness will be considered on the level of intersecting groups
defined by all columns selected as a predicted attribute.

## Examples

``` r
groupdiff_tau(1:3)
#> [1] 0.3333333
groupdiff_diff(1:3)
#> [1] -2
groupdiff_absdiff(1:3)
#> [1] 2
```
