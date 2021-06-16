```
Feature Name: `Fairness Metrics`
Start Date: 2021-06-15
Target Date: 2021-06-22
```

## Summary
[summary]: #summary

Add fairness related metrics to fairness package.

## Motivation
[motivation]: #motivation
Those metrics could be used to evaluate the bias and accuracy of the dataset. Then further being used in debiasing algorithms and visualizations. Both of them would be the main functions of this package.

## Guide-level explanation
[guide-level-explanation]: #guide-level-explanation

I propose when applying a metric method, what occurs should be ...

Example:
```r
#The true label of the observations
truth = as.factor(c(1,1,1,0,0,1,1,0,0,0))

#The predicted labels of the observations
response = as.factor(c(1,1,0,0,1,1,0,0,0,1))

fn(truth, response, positive = "1")
```

## Reference-level explanation
[reference-level-explanation]: #reference-level-explanation

Internally, the function would look the following:

Example:
```r
fn = function(truth, response, positive, ...) {
  assert_binary(truth, response = response, positive = positive)
  fn_cm(cm(truth, response, positive))
}

fn_cm = function(m, na_value = NaN) {
  m[2L, 1L]
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
https://github.com/mlr-org/mlr3measures/blob/main/R/binary_fn.R

## Introduced Dependencies
This solution would introduce dependencies on the following (additional) packages:
mlr3measures
Those packages either depend on or import the following other (additional) packages:
Depends:
    R (>= 3.1.0)
Imports:
    checkmate,
    PRROC
Using this package would allow us to use some basic fairness metrics instead of re-implementing and maintining
N loc ourselves.


## Unresolved questions
[unresolved-questions]: #unresolved-questions
