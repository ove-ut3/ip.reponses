# Module UI
  
#' @title   mod_stats_values_ui and mod_stats_values_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_stats_values
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_stats_values_ui <- function(id){
  ns <- NS(id)
  tagList(
    valueBoxOutput(ns("diplomes"), width = 4),
    valueBoxOutput(ns("repondants"), width = 4),
    valueBoxOutput(ns("taux_reponse"), width = 4)
  )
}
    
# Module Server
    
#' @rdname mod_stats_values
#' @export
#' @keywords internal
    
mod_stats_values_server <- function(input, output, session, rv){
  ns <- session$ns
  
  df_responses_stats <- reactive({
    
    req(rv$df_responses_user)
    
    data <- rv$df_responses_user()
    
    if (!is.null(rv$df_responses_filter_formation)) {
      
      data <- data %>% 
        dplyr::semi_join(
          rv$df_responses_filter_formation(),
          by = "identifiant"
        )
      
    }
    
    if (!is.null(rv$df_responses_filter_annee)) {
      
      data <- data %>% 
        dplyr::semi_join(
          rv$df_responses_filter_annee(),
          by = "identifiant"
        )
      
    }
    
    data
    
  })
  
  output$diplomes <- renderValueBox(
    valueBox(
      nrow(df_responses_stats()),
      "Diplômés",
      icon = icon("user-graduate")
    )
  )
  
  date_jour <- Sys.Date() %>%
    format("%d %B %Y") %>% stringr::str_remove("^0") %>%
    stringr::str_replace("^1 ", "1er ")
  
  output$repondants <- renderValueBox({
    valueBox(
      df_responses_stats() %>%
        dplyr::filter(completed == "Oui") %>%
        nrow(),
      glue::glue("Répondants"),
      icon = icon("edit")
    )
  })
  
  output$taux_reponse <- renderValueBox({
    repondants <- df_responses_stats() %>%
      dplyr::filter(completed == "Oui")
    valueBox(
      scales::percent(
        nrow(repondants) / nrow(df_responses_stats()),
        decimal.mark = ",",
        suffix = "\U202F%",
        accuracy = .1
      ),
      glue::glue("Taux de réponse"),
      icon = icon("chart-bar")
    )
  })
  
}
