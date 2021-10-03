% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MeasureFairness.R
\name{MeasureFairnessComposite}
\alias{MeasureFairnessComposite}
\title{Composite Fairness Measure}
\description{
Compute a composite measure from multiple fairness metrics.
Aggregates using \code{aggfun}, defaults to \code{mean()}.
}
\examples{
  # Create MeasureFairness to measure the Predictive Parity.
  library(mlr3)
  library(mlr3fairness)
  # Equalized Odds Metric
  MeasureFairnessComposite$new(measures = list("fairness.classif.fpr", "fairness.classif.tpr"))
  # Other metrics e.g. based on negative rates
  MeasureFairnessComposite$new(measures = list("fairness.classif.fnr", "fairness.classif.tnr"))
}
\section{Super class}{
\code{\link[mlr3:Measure]{mlr3::Measure}} -> \code{MeasureFairnessComposite}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{MeasureFairnessComposite$new()}}
\item \href{#method-clone}{\code{MeasureFairnessComposite$clone()}}
}
}
\if{html}{
\out{<details open ><summary>Inherited methods</summary>}
\itemize{
\item \out{<span class="pkg-link" data-pkg="mlr3" data-topic="Measure" data-id="aggregate">}\href{../../mlr3/html/Measure.html#method-aggregate}{\code{mlr3::Measure$aggregate()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="mlr3" data-topic="Measure" data-id="format">}\href{../../mlr3/html/Measure.html#method-format}{\code{mlr3::Measure$format()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="mlr3" data-topic="Measure" data-id="help">}\href{../../mlr3/html/Measure.html#method-help}{\code{mlr3::Measure$help()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="mlr3" data-topic="Measure" data-id="print">}\href{../../mlr3/html/Measure.html#method-print}{\code{mlr3::Measure$print()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="mlr3" data-topic="Measure" data-id="score">}\href{../../mlr3/html/Measure.html#method-score}{\code{mlr3::Measure$score()}}\out{</span>}
}
\out{</details>}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\if{latex}{\out{\hypertarget{method-new}{}}}
\subsection{Method \code{new()}}{
Creates a new instance of this \link[R6:R6Class]{R6} class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MeasureFairnessComposite$new(
  id = NULL,
  measures,
  aggfun,
  operation = function(x, y) {     abs(x - y) },
  minimize = TRUE,
  range = c(-Inf, Inf)
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{id}}{\code{\link{character}}\cr
Id of the measure. Defaults to the concatenation of id's in \code{measure}.}

\item{\code{measures}}{list of \link[=`MeasureFairness`| `character`]{\code{MeasureFairness}| \code{character}}\cr
List of fairness measures to aggregate (or valid keys to be used with \code{msr()}).}

\item{\code{aggfun}}{(\verb{function()})\cr
Aggregation function used to aggregate results from respective measures. Defaults to \code{sum}.}

\item{\code{operation}}{(\code{function})\cr
The operation used to compute the difference. A function with args 'x' and 'y'(optional) that returns
a single value. See \code{MeasureFairness} for more information. Only used if \code{measures} is of type \code{character}.}

\item{\code{minimize}}{(\code{logical})\cr
Should the measure be minimized? Defaults to \code{TRUE}.}

\item{\code{range}}{(\code{numeric})\cr
Range of the resulting measure. Defaults to \code{c(-Inf, Inf)}.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\if{latex}{\out{\hypertarget{method-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MeasureFairnessComposite$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}