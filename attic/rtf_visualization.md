```
Feature Name: `do_something`
Start Date: 2019-11-04
Target Date:
```

## Summary
[summary]: #summary

Implement the common fairness visualizations along with a clear documentation on how to interpret them in mlr3fairness packages.

## Motivation
[motivation]: #motivation

We need fairness visualizations so the package will be more user friendly. The visualizations are much more clear than fairness metrics.

## Guide-level explanation
[guide-level-explanation]: #guide-level-explanation

Users could use the following code to display the fairness metrics. 

Example:
```r
foo = 1
bar = 2
do_something(foo, bar)
```


## Reference-level explanation
[reference-level-explanation]: #reference-level-explanation

Internally, the function would look the following:

Example:
```r
do_something = function(a, b) {
  ...
}
```


## Rationale, drawbacks and alternatives
[rationale-and-alternatives]: #rationale-and-alternatives

This design seems fairly obvious choice in the design space.
The main alternative to this proposal is not to implement it,
and let users to calculate joined subslices from indexes or pointers.

## Prior art
[prior-art]: #prior-art

There exists a function that implements the API as here...

## Introduced Dependencies
This solution would introduce dependencies on the following (additional) packages:

Those packages either depend on or import the following other (additional) packages:

Using this package would allow us to ... instead of re-implementing and maintining
N loc ourselves.


## Unresolved questions
[unresolved-questions]: #unresolved-questions
