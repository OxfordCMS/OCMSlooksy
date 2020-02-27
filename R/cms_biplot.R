#' cms_biplot
#' 
#' Modification of \code{ggfortify::ggbiplot} to allow for selection of PCs
#' 
#' @param plot.data score data, samples in rows
#' @param loadings_data loading data, features in rows
#' @param xPC numeric; PC number for x axis. default 1
#' @param yPC number; PC number for y axis. default 2
#' @param colour colour of score points. default NULL
#' @param size size of score points. default NULL
#' @param linetype line type of arrows to points. default NULL
#' @param alpha alpha of score points. default NULL
#' @param fill fill of score points. default NULL
#' @param shape shape of score points.default NULL
#' @param label show score labels. default FALSE
#' @param label.label name of column to be used to label scores. default 'rownames'
#' @param label.colour colour of score labels
#' @param label.alpha alpha of score labels default NULL
#' @param label.size size of score labels. default NULL
#' @param label.angle angle fo score labels. default NULL
#' @param label.family font of score labels. default NULL
#' @param label.fontface fontface of score labels. default NULL
#' @param label.lineheight lineheight of score labels. default FALSE
#' @param label.hjust hjust of score labels. default NULL
#' @param label.vjust vjsut of score labels, default NULL
#' @param label.repel use ggrepel for score labels. default FALSE
#' @param loadings show loadings. default FALSE
#' @param loadings.arrow show line to loadings. default FALSE
#' @param loadings.colour colour of loadings. default red (#FF0000)
#' @param loadings.size size of loadings. default NULL
#' @param loadings.alpha alpha of loadings. default NULL
#' @param loadings.shape shape of loadings. default NULL
#' @param loadings.label show labels for loadings. default FALSE
#' @param loadings.label.label column used to label loadings. default 'rownames'
#' @param loadings.label.colour colour of loading label. default red (#FF0000)
#' @param loadings.label.alpha alpha of loading label default NULL
#' @param loadings.label.size size of loading label. default NULL
#' @param loadings.label.angle angle of loadings label. default NULL
#' @param loadings.label.family font of loading labels. default NULL
#' @param loadings.label.fontface fontface of loading labels. default NULL
#' @param loadings.label.hjust hjust of loading labels. default NULL
#' @param loadings.label.vjust vjust of loading labels. defualt NULL
#' @param loadings.label.repel use ggrepel on loading labels. default FALSE
#' @param label.show.legend show legend labels. default NA
#' @param frame frame clusters based on confidence interval estimation default FALSE
#' @param frame.type type of CI estimation. must one of: \code{convex, t, norm or euclid} default NULL
#' @param fram.colour frame colour. default \code{colour}
#' @param frame.level frame lineweight. default \code{0.95}
#' @param frame.alpha frame alpha. default \code{0.2}
#' @param xlim x-axis limits default \code{c(NA, NA)}
#' @param ylim y-axis limits default \code{c(NA, NA)}
#' @param log default ''
#' @param main plot title. default NULL
#' @param xlab x-axis title. default NULL
#' @param ylab y-axis title. default NULL
#' @param asp default NULL


cms_biplot <- function (plot.data, loadings.data = NULL, 
                        colour = NULL, size = NULL, 
                        linetype = NULL, alpha = NULL, fill = NULL, 
                        shape = NULL, label = FALSE, label.label = "rownames", 
                        label.colour = colour, label.alpha = NULL, 
                        label.size = NULL, label.angle = NULL, 
                        label.family = NULL, label.fontface = NULL, 
                        label.lineheight = NULL, label.hjust = NULL, 
                        label.vjust = NULL, label.repel = FALSE, 
                        loadings = FALSE, loadings.colour = "#FF0000",
                        loadings.shape = NULL, loadings.size = NULL,
                        loadings.alpha = NULL,
                        loadings.arrow = FALSE,
                        loadings.label = FALSE, loadings.label.label = "rownames", 
                        loadings.label.colour = "#FF0000", 
                        loadings.label.alpha = NULL, loadings.label.size = NULL, 
                        loadings.label.angle = NULL, loadings.label.family = NULL, 
                        loadings.label.fontface = NULL, 
                        loadings.label.lineheight = NULL, 
                        loadings.label.hjust = NULL, loadings.label.vjust = NULL, 
                        loadings.label.repel = FALSE, label.show.legend = NA, 
                        frame = FALSE, frame.type = NULL, frame.colour = colour, 
                        frame.level = 0.95, frame.alpha = 0.2, xlim = c(NA, NA), 
                        ylim = c(NA, NA), log = "", main = NULL, 
                        xlab = NULL, ylab = NULL, asp = NULL, ...) 
{
  # score data
  plot.columns <- colnames(plot.data)
  
  # initiate score plot mapping
  mapping <- ggplot2::aes_string(x = plot.columns[1L], y = plot.columns[2L])
  if (is.logical(shape) && !shape && missing(label)) {
    label <- TRUE
  }
  
  # initiate score plot
  p <- ggplot2::ggplot(data = plot.data, mapping = mapping)
  
  # score points
  if (!is.logical(shape) || shape) {
    p <- p + ggfortify:::geom_factory(ggplot2::geom_point, plot.data, 
                          colour = colour, size = size, linetype = linetype, 
                          alpha = alpha, fill = fill, shape = shape)
  }
  
  # score labels
  p <- ggfortify:::plot_label(p = p, data = plot.data, label = label, 
                              label.label = label.label, 
                  label.colour = label.colour, label.alpha = label.alpha, 
                  label.size = label.size, label.angle = label.angle, 
                  label.family = label.family, label.fontface = label.fontface, 
                  label.lineheight = label.lineheight, 
                  label.hjust = label.hjust, label.vjust = label.vjust, 
                  label.repel = label.repel, label.show.legend = label.show.legend)
  
  # loading data
  if (loadings.label && !loadings) {
    loadings <- TRUE
  }
  if (loadings && !is.null(loadings.data)) {
    scaler <- min(max(abs(plot.data[, 1L])) / 
                    max(abs(loadings.data[,1L])), 
                  max(abs(plot.data[, 2L])) / 
                    max(abs(loadings.data[,2L])))
    
    loadings.columns <- colnames(loadings.data)
    
    # loading plot mapping
    loadings.mapping <- ggplot2::aes_string(x = 0, y = 0, 
                                            xend = loadings.columns[1L], 
                                            yend = loadings.columns[2L])
    
    loadings.data[, 1L:2L] <- loadings.data[, 1L:2L] * scaler * 
      0.8
    
    # loading points
    if (!is.logical(loadings.shape) || loadings.shape) {
      p <- p + ggfortify:::geom_factory(ggplot2::geom_point, loadings.data, 
                                        colour = loadings.colour, 
                                        size = loadings.size, 
                                        alpha = loadings.alpha, 
                                        shape = loadings.shape)
    }
    # loading arrows
    if(loadings.arrow) {
      p <- p + ggplot2::geom_segment(data = loadings.data, mapping = loadings.mapping, 
                            arrow = grid::arrow(length = grid::unit(8, "points")), 
                            colour = 'grey50', alpha = loadings.alpha)
    }
    
    
    # loading labels
    p <- ggfortify:::plot_label(p = p, data = loadings.data, label = loadings.label, 
                    label.label = loadings.label.label, 
                    label.colour = loadings.label.colour, 
                    label.alpha = loadings.label.alpha, 
                    label.size = loadings.label.size, 
                    label.angle = loadings.label.angle, 
                    label.family = loadings.label.family, 
                    label.fontface = loadings.label.fontface, 
                    label.lineheight = loadings.label.lineheight, 
                    label.hjust = loadings.label.hjust, 
                    label.vjust = loadings.label.vjust, 
                    label.repel = loadings.label.repel, 
                    label.show.legend = label.show.legend)
  }
  
  # framing clusters
  if (missing(frame) && !is.null(frame.type)) {
    frame <- TRUE
  }
  . <- NULL
  if (frame) {
    if (is.null(frame.type) || frame.type == "convex") {
      if (is.null(frame.colour) || !(frame.colour %in% 
                                     colnames(plot.data))) {
        hulls <- plot.data[grDevices::chull(plot.data[, 
                                                      1L:2L]), ]
      }
      else {
        hulls <- plot.data %>% dplyr::group_by_(frame.colour) %>% 
          dplyr::do(.[grDevices::chull(.[, 1L:2L]), ])
      }
      mapping <- aes_string(colour = frame.colour, fill = frame.colour)
      p <- p + ggplot2::geom_polygon(data = hulls, mapping = mapping, 
                                     alpha = frame.alpha)
    }
    else if (frame.type %in% c("t", "norm", "euclid")) {
      mapping <- aes_string(colour = frame.colour, fill = frame.colour)
      p <- p + ggplot2::stat_ellipse(mapping = mapping, 
                                     level = frame.level, type = frame.type, geom = "polygon", 
                                     alpha = frame.alpha)
    }
    else {
      stop("frame.type must be convex, t, norm or euclid")
    }
  }
  
  p <- ggfortify:::post_autoplot(p = p, xlim = xlim, ylim = ylim, log = log, 
                     main = main, xlab = xlab, ylab = ylab, asp = asp)
  return(p)
}