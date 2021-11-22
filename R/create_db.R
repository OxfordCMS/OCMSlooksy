#' create_db
#'
#' generates an sqlite database from 16S rRNA gene counts produced by 16S rRNA gene sequence processing pipelines. Depending on the pipeline used, count and taxonomy tables may have to be formatted according to \code{create_db} function specifications. This function will configure the sqlite database such that is is compatible as an input into the \code{OCMSlooksy} RShiny app.
#'
#' @param counts file or dataframe of counts with samples in columns, features in rows.
#'   First column should contain feature identities under the heading of
#'   \code{featureID}. Feature IDs must be unique.
#' @param taxonomy file or dataframe of taxonomy classifications corresponding to the
#'   counts table. Must contain the columns \code{featureID}, \code{sequence},
#'   \code{Kingdom}, \code{Phylum}, \code{Class}, \code{Order}, \code{Family},
#'   \code{Genus}, \code{Species}, \code{Taxon}. \code{Taxon} contains
#'   taxonomic classifications at all taxonomic levels, seperated by a semicolon.
#'   Unclassified taxa should be represented with \code{NA}.
#' @param overwrite logic. default FALSE. Allow database tables to overwrite
#'   existing tables of the same name.
#' @param outdir output directory.
#' @param db_name string. Name of database file. Default 'OCMSlooksy.db'
#' @param fromfile default TRUE. read count table and taxonomy tables from
#'   file (tab delimited or comma delimited)
#'
#' @details
#' This function requires two tables: a count table which contains feature
#' (ASVs or OTUs) for each sample, and a taxonomy table which contains the
#' taxonomy classification associated with features found in the count table.
#' 16S rRNA gene sequence processing pipelines vary in the format of their
#' outputs, but most provide a tab-delimitted or comma-delimited spreadsheet
#' containing this information. You will need to re-format (or create a new file)
#' such that the count and taxonomy tables conform to the following requirements:
#'
#' count table:
#' * samples in columns and features (ASVs or OTUs) in rows
#' * first column should contain feature identities under the heading \code{featureID} (column name is case sensitive)
#' * feature IDs must be unique
#'
#' taxonomy table:
#' * contains taxonomy classifications that correspond to features in count table
#' * first column contains feature IDs under the heading \code{featureID}
#' * Must contain the columns \code{featureID}, \code{sequence},
#'   \code{Kingdom}, \code{Phylum}, \code{Class}, \code{Order}, \code{Family},
#'   \code{Genus}, \code{Species}, \code{Taxon}. Column names are case-sensitive.
#' * if \code{sequence} for a feature is not known, you can leave the column empty
#' * if \code{species} for a feature is not known or us not classified, set species to \code{NA}. The same rule applies other taxonomy levels.
#' * \code{Taxon} contains classifications at all taxonomic levels, separated by a semicolon. Taxonomy names can be pre-pended with the initial of the taxonomy level but it is not necessary.
#' (e.g. k__Bacteria;p__Firmicutes;c__Clostridia;o__Clostridiales;f__Ruminococcaceae;g__Faecalibacterium;s__NA")
#'
#' The count and taxonomy tables can be supplied as .tsv or .csv files by setting \code{fromfile = TRUE} (default). Alternatively, if count and taxonomy tables are already in R, set \code{fromfile = FALSE} and supply the dataframes to the \code{counts} and \code{taxonomy} arguments, respectively.
#'
#'
#' @import RSQLite
#' @export
#' @return sqlite database with merged_abundance_id (counts) and
#'   merged_taxonomy (taxonomy) in a sqlite database file named as specified by \code{db_name}

create_db <- function(counts, taxonomy, overwrite = FALSE, outdir, db_name='OCMSlooksy.db',
                      fromfile = TRUE) {

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
      con <- dbConnect(SQLite(), file.path(outdir, db_name))
      dbWriteTable(con, 'merged_abundance_id', counts, overwrite = overwrite)
      dbWriteTable(con, 'merged_taxonomy', taxonomy, overwrite = overwrite)
      dbDisconnect(con)
    }, error = function(e) {
      stop("Cannot create connection to output directory")
    }
  )
}