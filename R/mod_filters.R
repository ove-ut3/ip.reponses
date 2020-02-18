# Module UI
  
#' @title   mod_filters_ui and mod_filters_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_filters
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_filters_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("ui_filters"), inline = TRUE),
    div(
      style = "display: inline-block; vertical-align: top; margin-top: 1.4%;",
      downloadButton(ns("excel"), "Excel")
    )
  )
}
    
# Module Server
    
#' @rdname mod_filters
#' @export
#' @keywords internal
    
mod_filters_server <- function(input, output, session, rv, res_auth){
  ns <- session$ns
  
  output$filter_formation <- renderUI({
    
    req(rv$df_responses_user)
    
    rv$df_responses_filter_formation <- callModule(
      module = shinyWidgets::selectizeGroupServer,
      id = "filter_formation",
      data = rv$df_responses_user(),
      vars = "lib_etape"
    )
    
    shinyWidgets::selectizeGroupUI(
      ns("filter_formation"),
      params = list(
        lib_etape = 
          list(inputId = "lib_etape", title = "Formation :")
      )
    )
    
  })
  
  output$filter_annee <- renderUI({
    
    req(rv$df_responses_user)
    
    rv$df_responses_filter_annee <- callModule(
      module = shinyWidgets::selectizeGroupServer,
      id = "filter_annee",
      data = rv$df_responses_user(),
      vars = "annee_u"
    )
    
    shinyWidgets::selectizeGroupUI(
      ns("filter_annee"),
      params = list(
        annee_u = list(inputId = "annee_u", title = "Année :")
      )
    )
    
  })
  
  output$ui_filters <- renderUI({
    
    if (all(is.na(res_auth$code_diplome[[1]])) | length(res_auth$code_diplome[[1]]) >= 2) {
      
      tagList(
        div(
          style = "display: inline-block; width: 48%; vertical-align: top;",
          uiOutput(ns("filter_formation"))
        ),
        div(
          style = "display: inline-block; width: 47%; vertical-align: top;",
          uiOutput(ns("filter_annee"))
        )
      )
      
    } else {
      
      tagList(
        div(
          style = "display: inline-block; width: 95%; vertical-align: top;",
          uiOutput(ns("filter_annee"))
        )
      )
      
    }
    
  })
  
  output$excel <- downloadHandler(
    
    filename = function() {
      "export.xlsx"
    },
    content = function(con) {
      
      data <- rv$df_responses_hot() %>% 
        tidyr::nest(data = -type_diplome) %>% 
        dplyr::mutate(
          data = purrr::map2(
            data,
            type_diplome,
            ~ dplyr::select(
              .x, 
              patchr::filter_data_patch(rv$df_columns_description, filtre = .y) %>% 
                dplyr::pull(champ)
            )
          )
        ) %>% 
        tidyr::unnest(data) %>% 
        dplyr::select(-type_diplome)
      
      dictionnaire <- rv$df_columns_description %>% 
        patchr::filter_data_patch(filtre = unique(rv$df_responses_hot()$type_diplome)) %>% 
        dplyr::select(champ, signification, commentaire)
      
      data <- list(
        "Données" = data,
        "Dictionnaire" = dictionnaire
      )
      
      writexl::write_xlsx(data, con)
    }
    
  )
  
  rv$filters <- input
  
}
