learner = lrn("classif.rpart", cp = .01)
adult_train = tsk("adult_train")
adult_test = tsk("adult_test")

#adult_train$col_roles --> $pta [1] "sex"
adult_train$col_roles
learner$train(adult_train)

me = MeasureFairness$new("groupwise_abs_diff", base_measure = msr("classif.fpr"))
me = MeasureFairness$new("groupwise_diff", base_measure = msr("classif.fpr"))
me = MeasureFairness$new("groupwise_quotient", base_measure = msr("classif.fpr"))
me = MeasureFairness$new("groupwise_diff")

#The id for me >>> [1] "fairness.classif.fpr"
me$id

#predictions **cannot access pta field**
predictions = learner$predict(adult_test)
predictions$score(me, task = adult_test)

#Add the task check for the following input
predictions$score(me)


