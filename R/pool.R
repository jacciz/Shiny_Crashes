pool <- pool::dbPool(RSQLite::SQLite(), dbname = "inst/app/www/crash_db.db")

# NON Pool
# pool <- dbConnect(RSQLite::SQLite(), dbname = "data/vaccine_inventory.db")
# crash <-DBI::dbReadTable(pool, "17crash")
# dbDisconnect(pool)
# dbListFields(pool, "crsh")
# poolClose(pool)