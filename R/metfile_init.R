#' metfile_init
#'
#' Initiate a metadata file that is compatible with OCMSlooksy from OCMS 16S database file or from a tab-delimited file of a count table.
#'
#' @param db_file rsqlite database file. Set to \code{FALSE} to use a count table in a tab-delimited file
#'
#' @param out_dir output directory. default \code{NULL} means output not written to file
#'
#' @param ref_table name of table in database from which sampleID are generated.
#'     default \code{NULL}. when \code{NULL} assumes table is merged_abundance_id
#'     which is the count table produced from the ocms_16s dada2 pipeline.
#'     When initiating the metadata file from a count table, set \code{db_file}
#'     to \code{FALSE} and count table is supplied in \code{ref_table} as tab-delimited file
#'
#' @param id_orient indicates orientation of sample IDs. default 'col' indicates
#'     samples are in columns. 'row' indicates samples are in rows and
#'     sample IDs are in the first column
#' @param dummy default NULL. when string supplied, adds a dummy column of NAs with the string as the column name.
#'
#' @details
#' When initiating the metadata file from a count table, set \code{db_file}
#'     to \code{FALSE} and count table is supplied in \code{ref_table}
#'     as tab-delimited file. If samples are in columns, set \code{id_orient} to 'col'.
#'     If samples are in rows, the sample IDs should be in the first column.
#' @import RSQLite
#' @export
#' @returns function returns a dataframe . if \code{out_dir} is set, writes the initiated metadata to file as a tab-delimited file named "metadata.tsv"


metfile_init <- function(db_file, out_dir=NULL, ref_table=NULL, id_orient = 'col',
                         dummy = NULL) {

  # check inputs----------------------------------------------------------------
  if(!db_file){
    if(!file.exists(ref_table)) {
      stop("db_file set to FALSE but file provided in ref_table is not found")
    }
  } else if(!file.exists(db_file)) {
    stop("db_file not found")
  }

  if(!is.null(out_dir)) {
    if(!file.exists(out_dir)) {
      stop("out_dir not found")
    }
  }

  if(!id_orient %in% c('col','row')) {
    stop("id_orient should be either 'col' or 'row'")
  }

  # read in data----------------------------------------------------------------
  if(!db_file) {

    ref_df <- read.table(ref_table, header=TRUE, sep="\t")
    if(id_orient == 'row') {
      rownames(ref_df) <- ref_df[,1]
      ref_df <- ref_df[,2:ncol(ref_df)]
    }

  } else {
    # connect to database
    con <- dbConnect(SQLite(), db_file)

    # table in database
    table_ls <- dbListTables(con)

    if(is.null(ref_table)) {
      ref_table <- "merged_abundance_id"
    } else {

      if(db_file != FALSE & !ref_table %in% table_ls) {
        stop("ref_table supplied not found in database file")
      }
    }

    query <- sprintf("SELECT * FROM %s;", ref_table)

    # read in count data
    ref_df <- dbGetQuery(con, query)

    # disconenct dabase
    dbDisconnect(con)

  }

  # get sample IDs--------------------------------------------------------------
  if(id_orient == 'col') {
    ids <- colnames(ref_df)
  } else {
    ids <- rownames(ref_df)
  }

  # initiate metadata table
  met <- data.frame(sampleID = ids)

  if(!is.null(dummy)) {
    met[[dummy]] <- NA
  }

  if(!is.null(out_dir)) {
    write.table(met, file.path(out_dir, "metadata.tsv"),
                sep = "\t", row.names=FALSE, quote=FALSE)
  }

  return(met)
}