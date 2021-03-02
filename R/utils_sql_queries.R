# pool <- pool::dbPool(RSQLite::SQLite(), dbname = "inst/app/www/crash_db.db")


  # min_year,
  # max_year,
  # county,
  # crsh_svr
# min = 2017
# max = 2020
# county = 40 # "Milwaukee"
# crash_svr = "Property Damage"
# get_all_years_to_select = "17crash"
#' SQL Load data
#'
#' @param db_type # crash, vehicle, person, or crash_flags
#' 
#' @param county the input
#' @param crash_svr theinput
#' @param max_year max year selected
#' @param min_year min year selected
#' 
load_crash_data <- function(db_type, county){
                     # county,
                     # crash_svr,
                     # max_year,
                     # min_year) {
  min = 2017# min(input$year_input)
  max = 2017 #max(input$year_input)
  # Get all years from min to max and adds db type - i.e. char list of "2017crash
  # get_all_years_to_select <- paste0(seq(min,max, by = 1), db_type, sep = "")
  # 
  # read_db_tables <- function(db_name){
  #   DBI::dbReadTable(pool, db_name) %>% filter(CNTYCODE %in% county()) #, CRSHSVR %in% input$crshsvr_selected)
  # }
  # 
  # # Iterates each year/db type and returns a combined df
  # do.call(dplyr::bind_rows, lapply(get_all_years_to_select, read_db_tables))
  # lapply(get_all_years_to_select, read_db_tables)
}


# load_crash_data(
#   crash,
#   county_input,
#   crshsvr_selected,
#   max_date_selected,
#   min_date_selected
#   # muni_input,
# )


# load_crash_data <-
#   function(data, db_type = "17crash") {
#     # Data is matched via ID_Key, order_no, shipping_group, transport_route
#     # if (length(data != 0)) {
#       # Construct the update query
#       sql <- paste0(
#         " SELECT * FROM ?table WHERE ",
#         "CNTYCODE",
#         " = ?county AND ",
#         "CRSHSVR",
#         " = ?crsh_svr;"
#       )
#       query <- pool::sqlInterpolate(pool, sql, .dots = c(
#         list(table = db_type),
#         list(
#           county = county,
#           crsh_svr = crash_svr
#         )
#       ))
#       # Submit query and disconnect
#       pool::dbGetQuery(pool, query)
#       # pool::poolReturn()
#     # }
#   }
# microbenchmark::microbenchmark(
# load_crash_data(), # 72
# loadData() # 391
# )
# 
# yearsrange = lubridate::interval(lubridate::mdy(paste0(
#   "01-01-", min
# )), lubridate::mdy(paste0(
#   "12-31-", max
# )))
