```
Feature Name: `bias_mitigation`
Start Date: 2020-08-01
Target Date: 2020-08-25
```

## Summary
[summary]: #summary

Add debiasing methods to mlr3fairness. Which could allow users to mitigate the biases or fairness problems in their models.

## Motivation
[motivation]: #motivation

Users could now use fairness metrics to measure the fairness problems in their dataset, or use fairness visualizations to compare or detect the fairness problems in their models. If they want to mitigate such fairness problems. They need fairness debiasing algorithms/methods.

## Guide-level explanation
[guide-level-explanation]: #guide-level-explanation

There are no bias mitigation methods that have been implemented before, so there are no changes but new implementations.
All the debiasing methods will be applied as PipeOps. If users want to apply debiasing methods, what looks like will be this:


Example:
```r
reweighing = po("reweighing")
learner_po = po("learner", learner = lrn("classif.rpart"))
data = tsk("adult_train")

graph = reweighing %>>% learner_po
glrn = GraphLearner$new(graph)
glrn$train(data)
predictions = glrn$predict(data)
```


## Reference-level explanation
[reference-level-explanation]: #reference-level-explanation

Internally, the function would look the following:

Working principle:
The PipeOpsReweighing will take one task and calculate the reweighing weights based on the protected attribute and target column. After that, the .train_task and .predict_task will attach a reweighing.WEIGHT column at the task$data().

Example:
```r
PipeOpReweighing = R6Class("PipeOpReweighing",
  inherit = mlr3pipelines::PipeOpTaskPreproc,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class][`PipeOp`] R6 class.
    #'
    #' @param id The PipeOps identifier in the PipeOps library.
    initialize = function(id = "reweighing") {
      super$initialize(id, feature_types = c("numeric", "integer"))
    }
  ),
  private = list(

    .train_task = function(task) {
      if ("twoclass" %nin% task$properties) {
        stop("Only binary classification Tasks are supported.")
      }

      weightcolname = "reweighing.WEIGHTS"
      if (weightcolname %in% unlist(task$col_roles)) {
        stopf("Weight column '%s' is already in the Task", weightcolname)
      }

      data = task$data()
      positive = task$positive
      pta = task$col_roles$pta
      privileged = unlist(task$data()[1,task$col_roles$pta, with = F])
      target = task$target_names

      weights = private$get_weights(task, data, positive, pta, privileged)
      tem_data = data[,c(pta, target), with=F]
      index_pri_pos = data[list(privileged, positive), on = c(pta, target), which = T]
      index_pri_neg = setdiff(data[list(privileged), on = pta, which = T], index_pri_pos)
      index_unpri_pos = setdiff(data[list(positive), on = target, which = T], index_pri_pos)
      index_unpri_neg = setdiff(c(1:dim(data)[1]), Reduce(union, list(index_pri_pos, index_pri_neg, index_unpri_pos)))

      wcol = setnames(data.table(rep(0, dim(data)[1])), weightcolname)
      wcol[index_pri_pos] = weights[1]
      wcol[index_pri_neg] = weights[2]
      wcol[index_unpri_pos] = weights[3]
      wcol[index_unpri_neg] = weights[4]

      task$cbind(wcol)
      task$col_roles$feature = union(task$col_roles$feature, weightcolname)
      task$col_roles$weight = weightcolname
      task
    },

    .predict_task = function(task){
      return( private$.train_task(task) )
    },

    # Get the weights used for reweighing algorithm (Helper Function)
    #
    # @param task The task
    # @param data The complete data.table in task
    # @param positive Default positive label in target
    # @param pta The name of the protected attribute, (task$col_roles$pta)
    # @param privileged The privileged group in protected attribute
    #
    # @return c(weight_positive_privileged, weight_negative_privileged, weight_positive_unprivileged, weight_negative_unprivileged)
    get_weights = function(task, data, positive, pta, privileged) {
      conditional_counts = binary_classif_pta_count(task, data, positive, pta, privileged)
      N_pos_privileged = conditional_counts[1]
      N_pos_unprivileged = conditional_counts[2]
      N_neg_privileged = conditional_counts[3]
      N_neg_unprivileged = conditional_counts[4]

      N_all = dim(data)[1]
      N_positive = N_pos_privileged + N_pos_unprivileged
      N_negative = N_neg_privileged + N_neg_unprivileged
      N_privileged = N_pos_privileged + N_neg_privileged
      N_unprivileged = N_pos_unprivileged + N_neg_unprivileged

      W_positive_privileged = (N_positive * N_privileged)/(N_all * N_pos_privileged)
      W_negative_privileged = (N_negative * N_privileged)/(N_all * N_neg_privileged)
      W_positive_unprivileged = (N_positive * N_unprivileged)/(N_all * N_neg_unprivileged)
      W_negative_unprivileged = (N_negative * N_unprivileged)/(N_all * N_neg_unprivileged)

      return( c(W_positive_privileged, W_negative_privileged, W_positive_unprivileged, W_negative_unprivileged) )
    }
  )
)

```


## Rationale, drawbacks and alternatives
[rationale-and-alternatives]: #rationale-and-alternatives

This design seems fairly obvious choice in the design space.
The main alternative to this proposal is not to implement it,
and let users to implement their bias mitigation algorithms themself.

## Prior art
[prior-art]: #prior-art

There exists a function that implements the API as here...

## Introduced Dependencies
This solution would introduce dependencies on the following (additional) packages:

```
mlr3pipelines
```

Those packages either depend on or import the following other (additional) packages:

Using this package would allow us to ... instead of re-implementing and maintining
N loc ourselves.


## Unresolved questions
[unresolved-questions]: #unresolved-questions
* Should we assume there could be other weight columns in the input data tasks. If this is the case, we then need to compute reweighing.WEIGHT = existing.WEIGHT * original_reweighing.WEIGHT
