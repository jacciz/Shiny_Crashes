# returns crash data, depends if a flag was selected
filter_crashes_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive(if (length(get_crshflag_list()) == 0) {
      # if no flags selected
      return (filtered_crashes_no_flags())
    } else {
      # if at least 1 flag was selected
      # returns the join with filtered_crashes
      return(semi_join(
        filtered_crashes_no_flags(),
        filtered_crsh_flags(),
        by = c("CRSHNMBR" = "CRSHNMBR")
      ))
    })
  })
}