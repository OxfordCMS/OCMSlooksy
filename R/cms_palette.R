#' cms_palette
#' generates qualitative colour palette from RColorBrewer
#'
#' @param n  numeric; default 1. number of colours; cannot exceed 335
#' @param preview logic; default FALSE; when set, gives preview of colour
#' @param full logic; default FALSE; when set, returns all 355 colours
#' @import RColorBrewer
#' @importFrom graphics pie
#' @return vector of HEX colours, if full set to TRUE, gives all 335 colours, else gives n numbers

cms_palette <- function(n = 1, preview = FALSE, full = FALSE) {

  # pulling colours from RColorBrewer------------------------------------------------
  full_palette <- brewer.pal.info
  full_palette$palID <- rownames(full_palette)

  ordered_pal <- rbind(dplyr::filter(full_palette, category == 'qual'),
                       dplyr::filter(full_palette, category == 'div'),
                       dplyr::filter(full_palette, category == 'seq'))

  col_vector = unlist(mapply(RColorBrewer::brewer.pal, ordered_pal$maxcolors,
                             ordered_pal$palID))

  col_samp <- col_vector[1:n]

  if(full==TRUE) out <- col_vector
  else out <- col_samp

  if(preview==TRUE) pie(rep(1,n), col=out)

  return(out)
}