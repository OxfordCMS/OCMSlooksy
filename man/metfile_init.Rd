% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metfile_init.R
\name{metfile_init}
\alias{metfile_init}
\title{metfile_init}
\usage{
metfile_init(
  db_file,
  out_dir = NULL,
  ref_table = NULL,
  id_orient = "col",
  dummy = NULL
)
}
\arguments{
\item{db_file}{rsqlite database file. Set to \code{NULL} to use a count table in a tab-delimited file}

\item{out_dir}{output directory. default \code{NULL} means output not written to file}

\item{ref_table}{name of table in database from which sampleID are generated.
default \code{NULL}. when \code{NULL} assumes table is merged_abundance_id
which is the count table produced from the ocms_16s dada2 pipeline.
When initiating the metadata file from a count table, set \code{db_file}
to \code{FALSE} and count table is supplied in \code{ref_table} as tab-delimited file}

\item{id_orient}{indicates orientation of sample IDs. default 'col' indicates
samples are in columns. 'row' indicates samples are in rows and
sample IDs are in the first column}

\item{dummy}{default NULL. when string supplied, adds a dummy column of NAs with the string as the column name.}
}
\value{
function returns a dataframe . if \code{out_dir} is set, writes the initiated metadata to file as a tab-delimited file named "metadata.tsv"
}
\description{
Initiate a metadata file that is compatible with OCMSlooksy from OCMS 16S database file or from a tab-delimited file of a count table.
}
\details{
When initiating the metadata file from a count table, set \code{db_file}
to \code{FALSE} and count table is supplied in \code{ref_table}
as tab-delimited file. If samples are in columns, set \code{id_orient} to 'col'.
If samples are in rows, the sample IDs should be in the first column.
}
