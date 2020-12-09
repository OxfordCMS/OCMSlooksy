% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cms_dendro.R
\name{plot_ggdendro}
\alias{plot_ggdendro}
\title{plot_ggdendro}
\usage{
plot_ggdendro(
  hcdata,
  metadata,
  direction = c("lr", "rl", "tb", "bt"),
  fan = FALSE,
  branch.size = 1,
  label.size = 3,
  label.category = NULL,
  nudge.label = 0.01,
  expand.y = 0.5,
  category = NULL,
  id = "sampleID"
)
}
\description{
use ggplot to make customized dendrogram.
following \href{https://atrebas.github.io/post/2019-06-08-lightweight-dendrograms/}
}