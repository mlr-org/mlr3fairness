% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mlr_learners_fairness.R
\docType{data}
\name{mlr_learners_fairness}
\alias{mlr_learners_fairness}
\title{Fair Learners in mlr3}
\format{
An object of class \code{data.table} (inherits from \code{data.frame}) with 5 rows and 3 columns.
}
\usage{
mlr_learners_fairness
}
\value{
A data.table containing an overview of available fair learners.
}
\description{
Fair Learners in mlr3
}
\section{Predefined measures}{

\CRANpkg{mlr3fairness} comes with a set of predefined 'fair learners' listed below:

\tabular{lll}{
\strong{key} \tab \strong{package} \tab \strong{reference} \cr
regr.fairfrrm     \tab fairml \tab Scutari et al., 2021 \cr
classif.fairfgrrm \tab fairml \tab Scutari et al., 2021 \cr
regr.fairzlm      \tab fairml \tab Zafar et al., 2019   \cr
classif.fairzlrm  \tab fairml \tab Zafar et al., 2019   \cr
regr.fairnclm     \tab fairml \tab Komiyama et al., 2018
}
}

\section{Protected Attributes}{

The protected attribute is specified as a \code{col_role} in the corresponding \code{\link[mlr3:Task]{mlr3::Task()}}:\cr
\verb{<Task>$col_roles$pta = "name_of_attribute"} \cr
This also allows specifying more than one protected attribute,
in which case fairness will be considered on the level of intersecting groups defined by all columns
selected as a predicted attribute.
}

\examples{
\dontshow{if (rlang::is_installed("fairml")) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
# example code

library("mlr3")
# Available learners:
mlr_learners_fairness
\dontshow{\}) # examplesIf}
}
\keyword{datasets}
