#' cms_rarefy
#' 
#' rarefaction curve based on vegan. returns dataframe of rarefaction curve adapted from https://github.com/mahendra-mariadassou/phyloseq-extended/blob/master/R/graphical_methods.R and vegan
#' 
#' @param x matrix with samples in row, feature in column
#' 

cms_rarefy <- function(x) {
  
  rarefun <- function(i) {
    step <- 1
    se <- TRUE
    
    cat(paste("rarefying sample", rownames(x)[i]), sep = "\n")
    n <- seq(1, tot[i], by = step)
    if (n[length(n)] != tot[i]) {
      n <- c(n, tot[i])
    }
    y <- vegan::rarefy(x[i, ,drop = FALSE], n, se = se)
    if (nrow(y) != 1) {
      rownames(y) <- c(".S", ".se")
      return(data.frame(t(y), Size = n, Sample = rownames(x)[i]))
    } else {
      return(data.frame(.S = y[1, ], Size = n, Sample = rownames(x)[i]))
    }
  }
  
  x <- as.matrix(x)
  if (!identical(all.equal(x, round(x)), TRUE)) 
    stop("function accepts only integers (counts)")
 
  tot <- rowSums(x)
  S <- rowSums(x > 0)
  nr <- nrow(x)
  step = 1
  
  out <- lapply(seq_len(nr), rarefun)
  
  df <- do.call(rbind, out)
  return(df)
 
 
}