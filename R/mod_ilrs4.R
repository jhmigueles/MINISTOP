#' ilrs4 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_ilrs4_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    
    shiny::sidebarPanel(
      div(
        shiny::sliderInput(inputId = ns("ilr1"),
                           label = "Reallocation of min/day to vigorous PA at 4 years old",
                           min = -10, max = 30,
                           value = 0, step = 1, post = " min/day")
      ),
      # h5("Note: The reallocation of time to vigorous PA is compensated with the other behaviours"),
      hr(),
      div(style = "font-size:80%",
          # h4("Your selection (4 years old)"),
          hr(),
          hr(),
          col_6(DT::dataTableOutput(ns("Table4"))),
          col_6(plotly::plotlyOutput(ns("pieChart4"))),
          hr()
      ),
      div(
        uiOutput(ns("img")),
        hr(),
        div("citation to paper when published")
      )
    ),
    
    shiny::mainPanel(
      fluidRow(
        div(
          col_6(
            h3("Change in behaviours at 9 years (Mediator)"),
            hr(),
            plotly::plotlyOutput(ns("barChart9"))
          ),
          col_6(
            h3("Change in outcomes at 9 years"),
            hr(),
            col_6(plotly::plotlyOutput(ns("barFMI9"))),
            col_6(plotly::plotlyOutput(ns("barCRF9")))
          )
        )
      )
    )
  )
}

#' ilrs4 Server Functions
#'
#' @noRd 
mod_ilrs4_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # DAG picture
    output$img = renderUI({tags$img(src = "img/DAG.PNG", width = "100%")}) 
    
    # PIE CHART 4 years old ------------------------------------------------
    # data for plot if ILR 1 modified
    
    m4 = as.numeric(out[1, c("VPA4", "MPA4", "LPA4", "SB4", "Sleep4")])
    
    row = reactive(which(out$ilr == 1 & round(out$VPA4) == input$ilr1))
    
    data_plot = reactive(data.frame(x = c("VPA", "MPA", "LPA", "SB", "Sleep"),
                                    values_4 = as.numeric(out[row(), c("VPA4", "MPA4", "LPA4", "SB4", "Sleep4")]) + m4,
                                    values_9 = as.numeric(out[row(), c("VPA9", "MPA9", "LPA9", "SB9", "Sleep9")])))
    FMI9 = reactive(data.frame(x = "",
                               y = as.numeric(out[row(), c("FMI9")])))
    CRF9 = reactive(data.frame(x = "",
                               y = as.numeric(out[row(), c("laps20m9")])))
    Motor9 = reactive(data.frame(x = "",
                                 y = as.numeric(out[row(), c("Speed9")])))
    Str9 = reactive(data.frame(x = "",
                               y = as.numeric(out[row(), c("Str9")])))
    
    # Table 4 YEARS ---------------------------
    output$Table4 = DT::renderDataTable(DT::datatable(data.frame(Behaviour = c("VPA", "MPA", "LPA",
                                                                               "SB", "Sleep"), 
                                                                 Time = c(paste(round(data_plot()$values_4[1]),"min/day"),
                                                                          paste(round(data_plot()$values_4[2]),"min/day"),
                                                                          paste(round(data_plot()$values_4[3]), "min/day"),
                                                                          paste(round(data_plot()$values_4[4]),"min/day"),
                                                                          paste(round(data_plot()$values_4[5]), "min/day"))),
                                                      options = list(dom = 't', 
                                                                     columnDefs = list(list(className = 'dt-center', targets = 0:1))),
                                                      rownames = FALSE))
    
    output$pieChart4 = plotly::renderPlotly({
      p = plotly::plot_ly(data_plot(), labels = ~factor(x), values = ~values_4, 
                          type = 'pie',
                          textinfo = "label",
                          insidetextfont = list(color = '#FFFFFF'),
                          hovertemplate = "<b>%{label}:</b>  %{text} <br>%{percent}</br><extra></extra>",
                          text = ~paste0(round(values_4), " min/day"),
                          marker = list(colors = c("#ffa600", "#ff6361", "#bc5090", "#58508d", "#003f5c"),
                                        line = list(color = '#FFFFFF', width = 2)),
                          showlegend = FALSE)
      p = plotly::layout(p, title = '\n', autosize = F, width = 200, height = 200, 
                         margin = list(l = 0, r = 10, b = 0, t = 0, pad = 4),
                         paper_bgcolor = 'transparent')
      p
    })
    
    
    # barcharts 9 YEARS ---------------------------
    output$barChart9 = plotly::renderPlotly({
      p = plotly::plot_ly(data_plot(), x = ~factor(x), y = ~values_9, 
                          type = 'bar',
                          insidetextfont = list(color = '#FFFFFF'),
                          hoverinfo = "text",
                          text = ~paste0(round(values_9), " min/day"),
                          marker = list(colors = c("#ffa600", "#ff6361", "#bc5090", "#58508d", "#003f5c"),
                                        line = list(color = '#FFFFFF', width = 2)),
                          showlegend = FALSE)
      p = plotly::layout(p, title = 'Movement behaviors',
                         yaxis = list(title = "min/day", range = c(-8, 8)),
                         xaxis = list(title = ""))
      p
    })
    
    # PREDICTIONS BODY COMPOSITION AND PHYSICAL FITNESS ---------------------------
    
    output$barFMI9 = plotly::renderPlotly({
      p = plotly::plot_ly(FMI9(), x = ~factor(x), y = ~y, 
                          type = 'bar',
                          insidetextfont = list(color = '#FFFFFF'),
                          hoverinfo = "text",
                          text = ~paste0(round(FMI9()$y, 3), " kg/m2"),
                          marker = list(colors = c("#ffa600", "#ff6361", "#bc5090", "#58508d", "#003f5c")[1]),
                          showlegend = FALSE)
      p = plotly::layout(p, title = "Fat mass index", 
                         yaxis = list(title = "kg/m2", range = c(-0.2, 0.2), 
                                      showticklabels = FALSE),
                         xaxis = list(title = ""))
      p
    })
    
    output$barCRF9 = plotly::renderPlotly({
      p = plotly::plot_ly(CRF9(), x = ~factor(x), y = ~y, 
                          type = 'bar',
                          # textposition = 'outside',
                          insidetextfont = list(color = '#FFFFFF'),
                          # hoverinfo = "text",
                          # hovertemplate = "<b>%{label}:</b>  %{text} <br>%{percent}</br><extra></extra>",
                          text = ~paste0(round(CRF9()$y, 3), " laps"),
                          marker = list(colors = c("#ffa600", "#ff6361", "#bc5090", "#58508d", "#003f5c")[2]),
                          showlegend = FALSE)
      p = plotly::layout(p, title = "Aerobic fitness", 
                         yaxis = list(title = "laps", range = c(-0.3, 0.3), 
                                      showticklabels = FALSE),
                         xaxis = list(title = ""))
      p
    })
  })
}

## To be copied in the UI
# mod_ilrs4_ui("ilrs4_ui_1")

## To be copied in the server
# mod_ilrs4_server("ilrs4_ui_1")
