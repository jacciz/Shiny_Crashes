# https://cran.r-project.org/web/packages/pool/pool.pdf

pool <- pool::dbPool(RSQLite::SQLite(), dbname = "inst/app/www/crash_db.db")

# Use this to "checkout" the connection. Don't use pool.
# conn <-pool::poolCheckout(pool)

# person <-DBI::dbReadTable(conn, "2017person")
# crash <-DBI::dbReadTable(conn, "2017crash")
# dbListFields(conn, "crsh")
