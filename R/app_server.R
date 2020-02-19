#' @import shiny
app_server <- function(input, output, session) {
  # List the first level callModules here
  
  res_auth <- shinymanager::secure_server(
    check_credentials = shinymanager::check_credentials(
      golem::get_golem_options("users") %>% 
        dplyr::rename_all(tolower) %>% 
        dplyr::rename_all(stringr::str_replace_all, "[[:punct:]\\s]+", "_") %>% 
        dplyr::rename_all(stringr::str_remove_all, "[^\\w]") %>% 
        dplyr::rename_all(stringi::stri_trans_general, "latin-ascii") %>% 
        tidyr::nest(code_diplome = code_diplome) %>% 
        dplyr::mutate_at("code_diplome", purrr::map, 1) %>% 
        dplyr::mutate_at("code_diplome", dplyr::na_if, "")
    )
  )

  rv <- reactiveValues()
  
  rv$df_columns_description <- golem::get_golem_options("df_columns_description") %>% 
    dplyr::arrange(ordre_champ) %>% 
    dplyr::semi_join(
      dplyr::tibble(champ = names(golem::get_golem_options("df_responses"))),
      by = "champ"
    )
  
  rv$df_responses_user <- reactive({
    
    df_responses <- golem::get_golem_options("df_responses")
    
    rv$user <- res_auth$user
    
    if (any(!is.na(res_auth$code_diplome[[1]]))) {
      
      df_responses <- df_responses %>% 
        dplyr::filter(code_etape %in% res_auth$code_diplome[[1]])
      
    }
    
    df_responses
    
  })
  
  callModule(mod_filters_server, "filters_ui", rv, res_auth)
  
  callModule(mod_stats_values_server, "stats_values_ui", rv)
  
  callModule(mod_responses_table_server, "responses_table_ui", rv, global, res_auth)
  
}
