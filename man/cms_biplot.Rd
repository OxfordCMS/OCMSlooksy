% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cms_biplot.R
\name{cms_biplot}
\alias{cms_biplot}
\title{cms_biplot}
\usage{
cms_biplot(
  plot.data,
  loadings.data = NULL,
  colour = NULL,
  size = NULL,
  linetype = NULL,
  alpha = NULL,
  fill = NULL,
  shape = NULL,
  label = FALSE,
  label.label = "rownames",
  label.colour = colour,
  label.alpha = NULL,
  label.size = NULL,
  label.angle = NULL,
  label.family = NULL,
  label.fontface = NULL,
  label.lineheight = NULL,
  label.hjust = NULL,
  label.vjust = NULL,
  label.repel = FALSE,
  loadings = FALSE,
  loadings.colour = "#FF0000",
  loadings.shape = NULL,
  loadings.size = NULL,
  loadings.alpha = NULL,
  loadings.arrow = FALSE,
  loadings.label = FALSE,
  loadings.label.label = "rownames",
  loadings.label.colour = "#FF0000",
  loadings.label.alpha = NULL,
  loadings.label.size = NULL,
  loadings.label.angle = NULL,
  loadings.label.family = NULL,
  loadings.label.fontface = NULL,
  loadings.label.lineheight = NULL,
  loadings.label.hjust = NULL,
  loadings.label.vjust = NULL,
  loadings.label.repel = FALSE,
  label.show.legend = NA,
  frame = FALSE,
  frame.type = NULL,
  frame.colour = colour,
  frame.line = "solid",
  frame.level = 0.95,
  frame.alpha = 0.2,
  xlim = c(NA, NA),
  ylim = c(NA, NA),
  log = "",
  main = NULL,
  xlab = NULL,
  ylab = NULL,
  asp = NULL,
  ...
)
}
\arguments{
\item{plot.data}{score data, samples in rows}

\item{colour}{colour of score points. default NULL}

\item{size}{size of score points. default NULL}

\item{linetype}{line type of arrows to points. default NULL}

\item{alpha}{alpha of score points. default NULL}

\item{fill}{fill of score points. default NULL}

\item{shape}{shape of score points.default NULL}

\item{label}{show score labels. default FALSE}

\item{label.label}{name of column to be used to label scores. default 'rownames'}

\item{label.colour}{colour of score labels}

\item{label.alpha}{alpha of score labels default NULL}

\item{label.size}{size of score labels. default NULL}

\item{label.angle}{angle fo score labels. default NULL}

\item{label.family}{font of score labels. default NULL}

\item{label.fontface}{fontface of score labels. default NULL}

\item{label.lineheight}{lineheight of score labels. default FALSE}

\item{label.hjust}{hjust of score labels. default NULL}

\item{label.vjust}{vjsut of score labels, default NULL}

\item{label.repel}{use ggrepel for score labels. default FALSE}

\item{loadings}{show loadings. default FALSE}

\item{loadings.colour}{colour of loadings. default red (#FF0000)}

\item{loadings.shape}{shape of loadings. default NULL}

\item{loadings.size}{size of loadings. default NULL}

\item{loadings.alpha}{alpha of loadings. default NULL}

\item{loadings.arrow}{show line to loadings. default FALSE}

\item{loadings.label}{show labels for loadings. default FALSE}

\item{loadings.label.label}{column used to label loadings. default 'rownames'}

\item{loadings.label.colour}{colour of loading label. default red (#FF0000)}

\item{loadings.label.alpha}{alpha of loading label default NULL}

\item{loadings.label.size}{size of loading label. default NULL}

\item{loadings.label.angle}{angle of loadings label. default NULL}

\item{loadings.label.family}{font of loading labels. default NULL}

\item{loadings.label.fontface}{fontface of loading labels. default NULL}

\item{loadings.label.hjust}{hjust of loading labels. default NULL}

\item{loadings.label.vjust}{vjust of loading labels. defualt NULL}

\item{loadings.label.repel}{use ggrepel on loading labels. default FALSE}

\item{label.show.legend}{show legend labels. default NA}

\item{frame}{frame clusters based on confidence interval estimation default FALSE}

\item{frame.type}{type of CI estimation. must one of: \code{convex, t, norm or euclid} default NULL}

\item{frame.colour}{frame colour. default \code{colour}}

\item{frame.line}{frame linetype. default \code{'solid'}}

\item{frame.level}{frame lineweight. default \code{0.95}}

\item{frame.alpha}{frame alpha. default \code{0.2}}

\item{xlim}{x-axis limits default \code{c(NA, NA)}}

\item{ylim}{y-axis limits default \code{c(NA, NA)}}

\item{log}{default ''}

\item{main}{plot title. default NULL}

\item{xlab}{x-axis title. default NULL}

\item{ylab}{y-axis title. default NULL}

\item{asp}{default NULL}

\item{loadings_data}{loading data, features in rows}
}
\description{
Modification of \code{ggfortify::ggbiplot} to allow for variables in aes_string (not just colour specification)
}
