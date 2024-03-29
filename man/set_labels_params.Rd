% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cms_dendro.R
\name{set_labels_params}
\alias{set_labels_params}
\title{set_label_param}
\usage{
set_labels_params(nbLabels, direction = c("tb", "bt", "lr", "rl"), fan = FALSE)
}
\arguments{
\item{nbLabels}{number of labels}

\item{direction}{direction of level tob-bottom ('tb'), bottom-top ('bt'),
left-right ('lr'), right-left ('rl')}

\item{fan}{fan the labels}
}
\description{
set parameters of labels for plotting customized dendrogram.
following https://atrebas.github.io/post/2019-06-08-lightweight-dendrograms
}
