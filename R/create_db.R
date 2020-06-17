#' create_db
#' generates qualitative colour palette from RColorBrewer
#'
#' @param counts dataframe of counts with samples in columns, features in rows. 
#'   First column should contain feature identities under the heading of 
#'   \code{featureID}. Feature IDs must be unique.
#' @param taxonomy dataframe of taxonomy classifications corresponding to the 
#'   counts table. Must contain the columns \code{featureID}, \code{sequence}, 
#'   \code{Kingdom}, \code{Phylum}, \code{Class}, \code{Order}, \code{Family},
#'   \code{Genus}, \code{Species}, \code{Taxon}. \code{Taxon} contains 
#'   taxonomic lassifications at all taxonomic levels, seperated by a semicolon. 
#'   Unclassified taxa should be represented with \code{NA}.
#' @param overwrite logic. default FALSE. Allow database tables to overwrite
#'   existing tables of the same name.
#' @param outdir output directory.
#' @param fromfile default FALSE. read count table and taxonomy tables from 
#'   file (tab delimited or comma delimited)
#' @import RSQLite
#' @export
#' @return sqlite database with merged_abundance_id (counts) and
#'   merged_taxonomy (taxonomy) in a sqlite database file named 'csvdb'

create_db <- function(counts, taxonomy, overwrite = FALSE, outdir, 
                      fromfile = FALSE) {
  
  # from file
  if(fromfile) {
    tryCatch({
      ext <- tools::file_ext(counts)
      counts <- switch(ext,
                       csv = vroom::vroom(counts, delim = ","),
                       tsv = vroom::vroom(counts, delim = "\t")
      )
    }, error = function() {
      stop("Could not read in counts file. Check that the correct path is specified. File must be a tab or comma delimited file.")
    }
    )
    
    tryCatch({
      ext <- tools::file_ext(taxonomy)
      taxonomy <- switch(ext,
                         csv = vroom::vroom(taxonomy, delim = ","),
                         tsv = vroom::vroom(taxonomy, delim = "\t")
      )
    }, error = function() {
      stop("Could not read in taxonomy file. Check that the correct path is specified. File must be a tab or comma delimited file.")
    }
    )
  }
  # check counts
  if(!is.data.frame(counts)) {
    stop("counts must be a dataframe.")
  }
  if(colnames(counts)[1] != 'featureID') {
    stop("First column of counts must be 'featureID'.")
  }
  if(any(counts[,2:ncol(counts)] %% 1 != 0)) {
    stop("Non-integer found in counts table (besides featureID)")
  }
  
  # check taxonomy 
  if(!is.data.frame(taxonomy)) {
    stop("taxonomy must be dataframe or matrix")
  }
  tax_col <- c('featureID','sequence','Kingdom','Phylum','Class','Order','Family',
               'Genus','Species', 'Taxon')
  tryCatch(taxonomy[, tax_col],
           error = function(e) {
             stop(sprintf("taxonomy must have the columns: %s", 
                  paste0(tax_col, collapse = ', ')))
             }
           )
  if(colnames(taxonomy)[1] != 'featureID') {
    stop("First column of taxonmoy must be 'featureID'.")
  }
  
  # check featureID in count and taxonomy match
  tryCatch(feat_tally <- as.data.frame(table(counts$featureID)),
           error = function(e) {
             stop("counts missing 'featureID'")
           })
  
  if(any(feat_tally$Freq > 1)) {
    stop('counts featureID must be unique')
  }
  
  tryCatch(feat_tally <- as.data.frame(table(taxonomy$featureID)),
           error = function(e) {
             stop("taxonomy missing 'featureID'")
           })
  
  if(any(feat_tally$Freq > 1)) {
    stop('taxonomy featureID must be unique')
  }
  
  if(!identical(sort(counts$featureID), sort(taxonomy$featureID))) {
    stop('featureID in taxonomy and counts do not match')
  }
  
  tryCatch({
      con <- dbConnect(SQLite(), file.path(outdir, 'csvdb'))
      dbWriteTable(con, 'merged_abundance_id', counts, overwrite = overwrite)
      dbWriteTable(con, 'merged_taxonomy', taxonomy, overwrite = overwrite)
      dbDisconnect(con)
    }, error = function(e) {
      stop("Cannot create connection to output directory")
    }  
  )
}