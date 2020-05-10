# Module UI
  
#' @title   mod_responses_table_ui and mod_responses_table_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_responses_table
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_responses_table_ui <- function(id){
  ns <- NS(id)
  tagList(
    DT::DTOutput(ns("responses_table"))
  )
}
    
# Module Server
    
#' @rdname mod_responses_table
#' @export
#' @keywords internal

mod_responses_table_server <- function(input, output, session, rv, res_auth){
  ns <- session$ns
  
  rv$df_responses_hot <- reactive({
    
    req(rv$df_responses_filter_annee)
    
    data <- rv$df_responses_filter_annee()
    
    if (!is.null(rv$df_responses_filter_formation)) {
      
      data <- data %>% 
        dplyr::semi_join(
          rv$df_responses_filter_formation(),
          by = c("annee", "code_etudiant")
        )
      
    }
    
    data
    
  })
  
  output$responses_table <- DT::renderDT({

    data <- rv$df_responses_hot() %>% 
      tidyr::nest(data = -.data$type_diplome) %>% 
      dplyr::mutate(
        data = purrr::map2(
          data,
          .data$type_diplome,
          ~ dplyr::select(
            .x, 
            rv$df_columns_description %>% 
              tidyr::separate_rows(.data$filtre, sep = ";") %>% 
              dplyr::filter(.data$filtre %in% .y | is.na(.data$filtre)) %>% 
              dplyr::pull(.data$champ)
          )
        )
      ) %>% 
      tidyr::unnest(data) %>% 
      dplyr::select(-.data$type_diplome)
    
    targets <- data %>% 
      dplyr::select_if(is.character) %>% 
      dplyr::select_if( ~ any(nchar(.) > 30, na.rm = TRUE)) %>% 
      names()

    data %>%
      DT::datatable(
        rownames = FALSE,
        escape = FALSE,
        options = list(
          dom = "rtp",
          scrollX = TRUE,
          scrollY = '67vh',
          columnDefs = list(list(
            targets = which(names(data) %in% targets) - 1,
            render = DT::JS(
              "function(data, type, row, meta) {",
              "return type === 'display' && data != null && data.length > 30 ?",
              "'<span title=\"' + data + '\">' + data.substr(0, 30) + '...</span>' : data;",
              "}")
          ))
        )
      )

  })
  
}
