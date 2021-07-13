#' state_lv UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shiny.fluent
#' @importFrom  shiny NS tagList
mod_state_lv_ui <- function(id){
  comment_ui <- ""
  
  ns <- NS(id)
  tagList(
    tags$style(".card { padding: 28px; margin-bottom: 28px; }"),
    Stack(
      tokens = list(childrenGap = 10), horizontal = TRUE,
      makeCard(
        "Overview",
        tagList(
          tags$small("Click a DRG to see modelling details"),
          DT::DTOutput(ns("dt"))
        ),
        size = 4, style = "background-color:white; height: 80vh"
      ),
      makeCard(
        "Model results for selected DRG group",
        Pivot(
          PivotItem(
            headerText = "Separation rate",
            plotly::plotlyOutput(ns("seps_rate_p"), height = "70vh")
          ),
          PivotItem(
            headerText = "ALOS",
            plotly::plotlyOutput(ns("alos_p"), height = "70vh")
          )
        ),
        size = 8, style = "background-color:white; height: 80vh"
      )
    )
  )
}

#' state_lv Server Functions
#'
#' @import fable plotly ggplot2
#' @noRd
mod_state_lv_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    drg_grp_rv <- reactiveValues()
    drg_grp_rv$selected <- NULL
    
    # Overview DT ----
    output$dt <- DT::renderDT({
      drg_spk_df %>%
        dplyr::select(
          drg_grp,
          `DRG, grouped` = drg_grpx,
          `Separation (day-only)` = seps_sd,
          `Separation (overnight)` = seps_ov,
          `ALOS (overnight)` = alos_ov
        ) %>%
        DT::datatable(
          rownames = FALSE,
          escape = FALSE,
          selection = "single",
          # filter = list(position = "top", clear = FALSE),
          options = list(
            dom = "tf",
            scrollY = "60vh",
            # autoWidth = TRUE,
            search = list(regex = TRUE, caseInsensitive = TRUE),
            drawCallback =  htmlwidgets::JS('function(){debugger;HTMLWidgets.staticRender();}'),
            columnDefs = list(
              list(visible = FALSE, targets = c(0))
              # ,
              # list(width = "50px", targets = 3),
              # list(width = "200px", targets = 1)
            ),
            pageLength = nrow(.)
          )
        ) %>%
        sparkline::spk_add_deps()
    })
    observeEvent(input$dt_cell_clicked, {
      .i <- input$dt_row_last_clicked
      drg_grp_rv$selected <- drg_spk_df$drg_grp[.i]
    })
    
    # seps_rate_plotly ----
    output$seps_rate_p <- plotly::renderPlotly({
      validate(
        need(length(drg_grp_rv$selected) == 1, "Click the table")
      )
      .e <- drg_grp_rv$selected
      # .e <- "01"
      # projected_rates[substr(names(projected_rates), 1, 3) == .e] %>% .[[3]] -> .ls
      
      .df <- projected_rates[substr(names(projected_rates), 1, 2) == .e] %>%
        purrr::map(function(.ls) {
          tibble::as_tibble(.ls$data) %>%
            dplyr::mutate(
              rate2 = rate * 100000,
              .model2 = dplyr::case_when(
                .model == "Observed" ~ .model,
                .model == "ENSEMBLE" ~ .model,
                .model == unique(.ls$perf$best_model) ~ .model
              ),
              .tooltip1 = paste0(
                year, "<br>",
                "<b>", clean_number(rate2), " per 100k</b>", "<br>",
                "Separations: ", clean_number(seps), "<br>",
                "Population:", clean_number(pop), "<br>"
              ),
              .tooltip2 = dplyr::case_when(
                .model == "Observed" ~ "Observed",
                .model == "ENSEMBLE" ~ paste0(
                  "ENSEMBLE (MAPE: ",
                  clean_number(dplyr::filter(.ls$perf, .model == "ENSEMBLE")$mape, 1), ")"
                ),
                .model == unique(.ls$perf$best_model) ~ .ls$perf %>%
                  dplyr::arrange(mape) %>%
                  dplyr::mutate(
                    summary = ifelse(
                      .model == best_model,
                      paste0("<b>", .model, " (MAPE ", clean_number(mape, 1), ")</b>"),
                      paste0(       .model, " (",      clean_number(mape, 1), ")")
                    )
                  ) %>%
                  .$summary %>%
                  paste(collapse = "<br>")
              ),
              .tooltip = paste0(.tooltip1, "<br>", .tooltip2)
              # ,
              # small_cell2 = any(.ls$data$small_cell2, na.rm = TRUE)
            ) %>%
            dplyr::filter(!is.na(.model2))
        }) %>%
        dplyr::bind_rows() %>%
        dplyr::select(-drg_grp, -sameday, -sex, -agegrp) %>% 
        tidyr::separate(group, into = c("drg_grp", "dayonly", "sex", "agegrp"), sep = "_", remove = FALSE) %>% 
        dplyr::mutate(
          dayonly_sex = paste(
            ifelse(dayonly == "1", "1 Day-only", "2 Overnight"),
            "/",
            ifelse(sex == "1", "1 Male", "2 Female")
          )
          # ,
          # small_cell2 = ifelse(small_cell2, "Small cell", "")
        )
      .g <- ggplot2::ggplot(
        .df,
        ggplot2::aes(year, rate2, text = .tooltip)) +
        ggplot2::geom_line(
          ggplot2::aes(col = .model, group = .model)
        ) +
        # ggplot2::geom_text(
        #   data = .df %>%
        #     dplyr::select(dayonly_sex, age_aim, small_cell2) %>%
        #     unique() %>%
        #     dplyr::mutate(
        #       year = min(.df$year) + 10,
        #       rate2 = max(.df$rate2) * 0.95,
        #       .tooltip = "Small cell"
        #     ),
        #   ggplot2::aes(label = small_cell2)
        # ) +
        ggplot2::facet_grid(dayonly_sex ~ agegrp) +
        ggplot2::labs(
          title = paste0("Separations per 100k population: <i><b>",
                         unique(dplyr::filter(drg_grp_lkup, drg_grp == .e)$drg_grpx), "</i></b>"),
          x = "",
          y = "Separation rate per 100k"
        ) +
        ggplot2::coord_cartesian(ylim = c(0, max(.df$rate2) * 1.05))
      
      plotly::ggplotly(.g, tooltip = "text") %>%
        plotly::hide_legend()
    })
    # alos_plotly ----
    output$alos_p <- plotly::renderPlotly({
      validate(
        need(length(drg_grp_rv$selected) == 1, "Click the table")
      )
      .e <- drg_grp_rv$selected
      # .e <- "21"
      # projected_rates[substr(names(projected_rates), 1, 2) == .e] %>% .[["111_2_2_1644"]] -> .ls
      
      .df <- projected_rates[
        substr(names(projected_rates), 1, 2) == .e & 
          substr(names(projected_rates), 4, 4) == "2"
      ] %>%
        purrr::map(function(.ls) {
          alos_df <- tibble::as_tibble(.ls$data) %>%
            dplyr::filter(sameday == "2") %>%
            dplyr::mutate(
              alos = los / seps,
              .tooltip1 = paste0(
                year, "<br>",
                "<b>", clean_number(alos, 1), "</b>", "<br>"
              )
            )
          if ("perf_alos" %in% names(.ls)) {
            alos_df <- alos_df %>%
              dplyr::mutate(
                .tooltip2 = dplyr::case_when(
                  .model_alos == "Observed" ~ "Observed",
                  .model_alos == "ENSEMBLE" ~ paste0(
                    "ENSEMBLE (MAPE: ",
                    clean_number(dplyr::filter(.ls$perf_alos, .model == "ENSEMBLE")$mape, 1), ")"
                  ),
                  .model_alos == unique(.ls$perf_alos$best_model) ~ .ls$perf_alos %>%
                    dplyr::arrange(mape) %>%
                    dplyr::mutate(
                      summary = ifelse(
                        .model == best_model,
                        paste0("<b>", .model, " (MAPE ", clean_number(mape, 1), ")</b>"),
                        paste0(       .model, " (",      clean_number(mape, 1), ")")
                      )
                    ) %>%
                    .$summary %>%
                    paste(collapse = "<br>")
                )
              )
          } else {
            alos_df <- alos_df %>%
              dplyr::mutate(
                .tooltip2 = .model_alos
              )
          }
          alos_df %>%
            dplyr::mutate(.tooltip = paste0(.tooltip1, "<br>", .tooltip2))
        }) %>%
        dplyr::bind_rows() %>%
        dplyr::mutate(sex = ifelse(sex == "1", "1 Male", "2 Female"))
      .g <- .df %>%
        # dplyr::mutate(.model_alos = factor(.model_alos, levels = model_key$model)) %>%
        ggplot2::ggplot(ggplot2::aes(year, alos, text = .tooltip)) +
        ggplot2::geom_line(
          ggplot2::aes(col = .model_alos, group = .model_alos)
        ) +
        ggplot2::facet_grid(sex ~ agegrp) +
        ggplot2::labs(
          title = paste0("ALOS (overnight only): <i><b>",
                         unique(dplyr::filter(drg_grp_lkup, drg_grp == .e)$drg_grpx), "</i></b>"),
          x = "",
          y = "ALOS"
        ) +
        ggplot2::coord_cartesian(ylim = c(0, max(.df$alos) * 1.05)) +
        # ggplot2::scale_colour_manual(values = model_key$colour) +
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = "#f5f5f5"))
      
      plotly::ggplotly(.g, tooltip = "text") %>%
        plotly::hide_legend()
    })
  })
}

