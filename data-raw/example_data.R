## code to prepare `example_data` dataset goes here

usethis::use_data("example_data")

save_dir <- "/gfs/devel/syen/OCMSExplorer/data-raw/"
save_dir <- "~/OCMSExplorer/data-raw/"
# create database
con <- dbConnect(RSQLite::SQLite(), file.path(save_dir, "example_database.db"))

# read in example data from csv files
example_tables <- list.files(save_dir, ".csv")
example_tables <- example_tables[1:2]
for(i in 1:length(example_tables)) {
  
  entry <- read.csv(file.path(save_dir, example_tables[i]),
                    header = TRUE)
  dbWriteTable(con, str_extract(example_tables[i], '.*(?=.csv)'), entry, overwrite = TRUE)
}

# read in data on filtering and removing chimeras and save as one table
work_path <- "/gfs/work/syen/testdada2/analysis/dada2/"
infiles = list.files(path=file.path(work_path, "filtered.dir"), 
                     pattern = "*summary.tsv")

# put into one table
filtered <- c()
for(i in infiles) {
  curr_file <- file.path(work_path, "filtered.dir", i)
  entry = read.csv(curr_file, header=T, stringsAsFactors=F, sep="\t")
  
  filtered <- rbind(filtered, entry)
}

infiles = list.files(path=file.path(work_path, "abundance.dir"), 
                     pattern = "*summary.tsv")

# put into one table
nochim <- c()
for(i in infiles) {
  curr_file <- file.path(work_path, "abundance.dir", i)
  entry = read.csv(curr_file, header=T, stringsAsFactors=F, sep="\t")
  
  nochim <- rbind(nochim, entry)
}

# Add table to database
dbWriteTable(con, 'merged_filter_summary', filtered)
dbWriteTable(con, 'merged_qc_summary', nochim)


# writing database table into list to save as RData file
# extract data tables
table_ls <- RSQLite::dbListTables(con)

example_data <- list()
for(i in 1:length(table_ls)) {
  query <- sprintf("SELECT * FROM %s", table_ls[i])
  entry <- RSQLite::dbGetQuery(con, query)

  example_data[[table_ls[i]]] <- entry
}
# close connection
dbDisconnect(con)


save(example_data, 'example_data', file = './data/example_data.RData')
