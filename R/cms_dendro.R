#' dendro_data_k
#' 
#' Extract dendrogram data for plot customized dendrogram. 
#' following \href{https://atrebas.github.io/post/2019-06-08-lightweight-dendrograms/}

dendro_data_k <- function(hc, k) {
  
  hcdata    <-  ggdendro::dendro_data(hc, type = "rectangle")
  seg       <-  hcdata$segments
  labclust  <-  cutree(hc, k)[hc$order]
  segclust  <-  rep(0L, nrow(seg))
  heights   <-  sort(hc$height, decreasing = TRUE)
  height    <-  mean(c(heights[k], heights[k - 1L]), na.rm = TRUE)
  
  for (i in 1:k) {
    xi      <-  hcdata$labels$x[labclust == i]
    idx1    <-  seg$x    >= min(xi) & seg$x    <= max(xi)
    idx2    <-  seg$xend >= min(xi) & seg$xend <= max(xi)
    idx3    <-  seg$yend < height
    idx     <-  idx1 & idx2 & idx3
    segclust[idx] <- i
  }
  
  idx                    <-  which(segclust == 0L)
  segclust[idx]          <-  segclust[idx + 1L]
  hcdata$segments$clust  <-  segclust
  hcdata$segments$line   <-  as.integer(segclust < 1L)
  hcdata$labels$clust    <-  labclust
  
  return(hcdata)
}

#' set_label_param
#' 
#' set parameters of labels for plotting customized dendrogram. 
#' following \href{https://atrebas.github.io/post/2019-06-08-lightweight-dendrograms/}

set_labels_params <- function(nbLabels,
                              direction = c("tb", "bt", "lr", "rl"),
                              fan       = FALSE) {
  if (fan) {
    angle       <-  360 / nbLabels * 1:nbLabels + 90
    idx         <-  angle >= 90 & angle <= 270
    angle[idx]  <-  angle[idx] + 180
    hjust       <-  rep(0, nbLabels)
    hjust[idx]  <-  1
  } else {
    angle       <-  rep(0, nbLabels)
    hjust       <-  0
    if (direction %in% c("tb", "bt")) { angle <- angle + 90 }
    if (direction %in% c("tb", "rl")) { hjust <- 1 }
  }
  return(list(angle = angle, hjust = hjust, vjust = 0))
}

#' plot_ggdendro
#' 
#' use ggplot to make customized dendrogram.
#' following \href{https://atrebas.github.io/post/2019-06-08-lightweight-dendrograms/}

plot_ggdendro <- function(hcdata,
                          metadata,
                          direction   = c("lr", "rl", "tb", "bt"),
                          fan         = FALSE,
                          branch.size = 1,
                          label.size  = 3,
                          label.category = NULL,
                          nudge.label = 0.01,
                          expand.y    = 0.1,
                          category = NULL,
                          id = 'sampleID') {
  
  direction <- match.arg(direction) # if fan = FALSE
  ymax      <- round(max(segment(hcdata)$y))
  ymin <- -1
  ybreaks   <- seq(ymin, ymax, 5)
  
  # set dendrogram labels
  if(!is.null(label.category) & label.category != id) {
    label_data <- hcdata$labels %>%
      dplyr::inner_join(metadata[,c(id, label.category)] %>% 
                          dplyr::rename(new_label = !!label.category), 
                        c('label' = id))
    hcdata$labels <- label_data
  }
  else {
    hcdata$labels$new_label <- hcdata$labels$label
  }
  
  ## branches
  p <- ggplot() +
    geom_segment(data         =  segment(hcdata),
                 aes(x        =  x,
                     y        =  y,
                     xend     =  xend,
                     yend     =  yend,
                     linetype =  factor(line),
                     colour   =  factor(clust)),
                 lineend      =  "round",
                 size         =  branch.size)
  
  # labels
  labelParams <- set_labels_params(nrow(hcdata$labels), direction, fan)
  hcdata$labels$angle <- labelParams$angle
  
  if(is.null(category)) {
    hcdata$labels$y <- -0.1
  }
  else {
    hcdata$labels$y <- -0.6
  }
  p <- p +
    geom_text(data        =  ggdendro::label(hcdata),
              aes(x       =  x,
                  y       =  y,
                  label   =  new_label,
                  # colour  =  factor(clust),
                  angle   =  angle),
              vjust       =  labelParams$vjust,
              hjust       =  labelParams$hjust,
              nudge_y     =  ymax * nudge.label,
              size        =  label.size,
              show.legend =  FALSE)
  
  # plot limits
  ylim <- -round(ymax * expand.y, 1)
  p    <- p + expand_limits(y = ylim)
  
  # colours
  nclust <- unique(ggdendro::segment(hcdata)$clust)
  nclust[is.na(nclust)] <- 0
  nclust <- sort(as.numeric(nclust))
  if(length(nclust) == 1) {
    p <- p + scale_colour_manual(values = 'black', guide = FALSE) +
      scale_linetype_discrete(guide = FALSE)
  }
  else{
    p <- p + 
      scale_linetype_discrete(guide = FALSE) +
      scale_colour_manual(name = 'cluster', labels = nclust,
                         values = cms_palette(max(segment(hcdata)$clust) + 1))
  }
  
  # categorical data
  if(!is.null(category)) {
    cat_data <- metadata %>% 
      dplyr::rename(rowID = !!id) %>%
      tidyr::gather('met_cat', 'value', -rowID) %>%
      dplyr::filter(met_cat == category)  %>%
      dplyr::mutate(shift_y = -0.3) %>%
      dplyr::inner_join(ggdendro::label(hcdata), c('rowID' = 'label'))
    
    p <- p +
      geom_tile(data = cat_data, 
                aes_string(x = 'x', y = 'shift_y', fill = 'value'), 
                height = max(segment(hcdata)$y) * 0.05) +
      scale_fill_discrete(name = category)
  }
  
  else {
    p <- p + scale_fill_discrete(guide = FALSE)
  }

  
  ## orientation
  if (fan) {
    p <- p +
      coord_polar(direction = -1) +
      scale_x_continuous(breaks = NULL,
                         limits = c(0, nrow(label(hcdata)))) +
      scale_y_reverse(breaks = ybreaks)
  } else {
    p <- p + scale_x_continuous(breaks = NULL)
    if (direction %in% c("rl", "lr")) {
      p <- p + coord_flip()
    }
    if (direction %in% c("bt", "lr")) {
      p <- p + scale_y_reverse(breaks = ybreaks)
    } else {
      p <- p + scale_y_continuous(breaks = ybreaks)
      nudge.label <- -(nudge.label)
    }
  }
  p <- p +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank(),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 14),
          legend.key = element_blank(),
          axis.title.x = element_text(colour = NA),
          axis.title.y = element_blank(),
          axis.text = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          plot.margin = unit(c(0.1,0.1,0.1,0.1), 'cm'))
  return(p)
}