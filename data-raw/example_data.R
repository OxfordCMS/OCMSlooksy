## code to prepare `example_data` dataset goes here

usethis::use_data("example_data")

# read in local file
## open connection
con <- RSQLite::dbConnect(RSQLite::SQLite(), 'H:/example_data.db')

# extract data tables
table_ls <- RSQLite::dbListTables(con)

example_data <- list()
for(i in 1:length(table_ls)) {
  query <- sprintf("SELECT * FROM %s", table_ls[i])
  entry <- RSQLite::dbGetQuery(con, query)
  
  example_data[[table_ls[i]]] <- entry
}

save(example_data, 'example_data', file = './data/example_data.RData')


