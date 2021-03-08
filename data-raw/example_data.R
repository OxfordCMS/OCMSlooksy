## code to prepare `example_data` dataset goes here

usethis::use_data("example_data")

# save_dir <- "/gfs/devel/syen/OCMSlooksy/data-raw"
save_dir <- "./data-raw"
# create database
con <- dbConnect(RSQLite::SQLite(), file.path(save_dir, "DSS_DB"))

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

# add metadata to list
example_data$metadata <- read.csv(file.path(save_dir, "dss_metadata.tsv"), sep="\t")

save(example_data, 'example_data', file = './data/example_data.RData')
