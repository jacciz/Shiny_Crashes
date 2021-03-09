# https://cran.r-project.org/web/packages/pool/pool.pdf

pool <- pool::dbPool(RSQLite::SQLite(), dbname = "inst/app/www/crash_db.db")

# Use this to "checkout" the connection. Don't use pool.
conn <-pool::poolCheckout(pool)

# pool <- dbConnect(RSQLite::SQLite(), dbname = "data/vaccine_inventory.db")
# crash <-DBI::dbReadTable(conn, "17crash")
# dbListFields(conn, "crsh")
