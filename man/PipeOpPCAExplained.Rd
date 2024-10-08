% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PipeOpPCAExplained.R
\name{PipeOpPCAExplained}
\alias{PipeOpPCAExplained}
\title{PCA explained}
\description{
PCA explained

PCA explained
}
\section{Super classes}{
\code{\link[mlr3pipelines:PipeOp]{mlr3pipelines::PipeOp}} -> \code{\link[mlr3pipelines:PipeOpTaskPreproc]{mlr3pipelines::PipeOpTaskPreproc}} -> \code{PipeOpPCAExplained}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-PipeOpPCAExplained-new}{\code{PipeOpPCAExplained$new()}}
\item \href{#method-PipeOpPCAExplained-clone}{\code{PipeOpPCAExplained$clone()}}
}
}
\if{html}{\out{
<details open><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="mlr3pipelines" data-topic="PipeOp" data-id="help"><a href='../../mlr3pipelines/html/PipeOp.html#method-PipeOp-help'><code>mlr3pipelines::PipeOp$help()</code></a></span></li>
<li><span class="pkg-link" data-pkg="mlr3pipelines" data-topic="PipeOp" data-id="predict"><a href='../../mlr3pipelines/html/PipeOp.html#method-PipeOp-predict'><code>mlr3pipelines::PipeOp$predict()</code></a></span></li>
<li><span class="pkg-link" data-pkg="mlr3pipelines" data-topic="PipeOp" data-id="print"><a href='../../mlr3pipelines/html/PipeOp.html#method-PipeOp-print'><code>mlr3pipelines::PipeOp$print()</code></a></span></li>
<li><span class="pkg-link" data-pkg="mlr3pipelines" data-topic="PipeOp" data-id="train"><a href='../../mlr3pipelines/html/PipeOp.html#method-PipeOp-train'><code>mlr3pipelines::PipeOp$train()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PipeOpPCAExplained-new"></a>}}
\if{latex}{\out{\hypertarget{method-PipeOpPCAExplained-new}{}}}
\subsection{Method \code{new()}}{
Constructor.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PipeOpPCAExplained$new(id = "pca_explained", param_vals = list())}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{id}}{(character(1))
Identifier of the resulting object.}

\item{\code{param_vals}}{(named list)
Named list of hyperparameter settings, overwriting the hyperparameter settings that would}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
[`PipeOpFilterRows`].
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PipeOpPCAExplained-clone"></a>}}
\if{latex}{\out{\hypertarget{method-PipeOpPCAExplained-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PipeOpPCAExplained$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
