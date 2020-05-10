#' Run the Shiny Application
#' 
#' @param users User table
#' @param df_responses Responses tibble
#' @param df_columns_description Responses column description tibble
#' 
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(users, df_responses, df_columns_description) {
  with_golem_options(
    app = shinyApp(
      ui = app_ui(),
      server = app_server
    ), 
    golem_opts = list(
      users = users,
      df_responses = df_responses,
      df_columns_description = df_columns_description
    )
  )
}
