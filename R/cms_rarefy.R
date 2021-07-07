#' cms_rarefy
#' 
#' from MetaSequencing by microbialman
#' 
#' @param x matrix with samples in columns, feature in rows
#' import foreach
#' import vegan

cms_rarefy <- function(df) {
  
  `%dopar%` <- foreach::`%dopar%`
  # getting sample read depth
  sampmax = colSums(df)
  raredepths = round(c(seq(from=1, to=max(sampmax),
                           by=(max(sampmax)-1)/20)))


  # initiate rarefaction values matrix
  vals = matrix(nrow=length(raredepths), ncol=ncol(df))
  
  # calculate rarefaction
  forres = foreach::foreach(i = 1:length(raredepths)) %dopar% {
    depth = raredepths[i]
    res = suppressWarnings(vegan::rarefy(round(t(df)), depth))
    res[sampmax < depth] = NA
    return(res)
  }

  # populate matrix with rarefaction values
  for(i in 1:length(forres)){
    vals[i,] = unlist(forres[i])
  }
  
  colnames(vals)=colnames(df)
  rownames(vals)=raredepths
  
  raremelt = reshape2::melt(vals)
  colnames(raremelt) = c("Depth","sampleID","Richness")
  raremelt = raremelt[!is.na(raremelt$Richness),]

  labels = data.frame(Sample = colnames(df),
                      Depth = sampmax,
                      Rich = apply(vals, 2, function(x){max(x,na.rm = T)}))
  
  # 
  # rarefun <- function(i) {
  #   step <- 1
  #   se <- TRUE
  #   
  #   cat(paste("rarefying sample", rownames(x)[i]), sep = "\n")
  #   n <- seq(1, tot[i], by = step)
  #   if (n[length(n)] != tot[i]) {
  #     n <- c(n, tot[i])
  #   }
  #   
  #   y <- vegan::rarefy(x[i, ,drop = FALSE], n, se = se)
  #   
  #   if (nrow(y) != 1) {
  #     rownames(y) <- c("species_richness", "std_error")
  #     return(data.frame(t(y), sample_size = n, sampleID = rownames(x)[i]))
  #   } 
  #   else {
  #     return(data.frame(species_richness = y[1, ], sample_size = n, 
  #                       sampleID = rownames(x)[i]))
  #   }
  # }
  # 
  # x <- as.matrix(x)
  # if (!identical(all.equal(x, round(x)), TRUE)) 
  #   stop("function accepts only integers (counts)")
  # 
  # tot <- rowSums(x)
  # S <- rowSums(x > 0)
  # nr <- nrow(x)
  # step = 1
  # 
  # out <- lapply(seq_len(nr), rarefun)
  # 
  # df <- do.call(rbind, out)
  return(raremelt)
}