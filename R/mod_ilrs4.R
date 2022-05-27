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
        radioButtons(inputId = ns("select"), 
                     choices = c("VPA", "MPA", "LPA", "SB"), 
                     label = "Select the behavior to change at 4 years old", 
                     selected = "VPA", inline = T),
      ),
      hr(),
      div(
        shiny::sliderInput(inputId = ns("ilr1"),
                           label = "ILR 1: VPA vs MPA·LPA·SB·Sleep",
                           min = -10, max = 30,
                           value = 0, step = 1, post = " min/day")
      ),
      hr(),
      div(
        shiny::sliderInput(inputId = ns("ilr2"),
                           label = "ILR 2: MPA vs LPA·SB·Sleep",
                           min = -30, max = 30,
                           value = 0, step = 2, post = " min/day")
      ),
      hr(),
      div(
        shiny::sliderInput(inputId = ns("ilr3"),
                           label = "ILR 3: LPA vs SB·Sleep",
                           min = -90, max = 90,
                           value = 0, step = 5, post = " min/day")
      ),
      hr(),
      div(
        shiny::sliderInput(inputId = ns("ilr4"),
                           label = "ILR 4: SB vs Sleep",
                           min = -300, max = 300,
                           value = 0, step = 15, post = " min/day")
      ),
      hr(),
      div("citation to paper when published")
    ),
    shiny::mainPanel(
      fluidRow(
        div(
          h3("Your selection (4 years old)"),
          hr(),
          # plotly::plotlyOutput(ns("pieChart4"))
          DT::dataTableOutput(ns("Table4"))
        ),
      ),
      fluidRow(
        div(
          hr(),
          h3("Outcomes derived from model coefficients (9 years old)"),
          hr(),
          col_6(
            # plotly::plotlyOutput(ns("pieChart9"))
            plotly::plotlyOutput(ns("barChart9"))
          ),
          col_3(
            plotly::plotlyOutput(ns("barFMI9"))
          ),
          col_3(
            plotly::plotlyOutput(ns("barCRF9"))
          )
          # ,
          # col_2(
          #   plotly::plotlyOutput(ns("barMotor9"))
          # ),
          # col_2(
          #   plotly::plotlyOutput(ns("barStr9"))
          # )
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
    
    # GET PREDICTIONS
    
    observeEvent(input$select, {
      # Enable/disable sliders
      shinyjs::toggleState(id = "ilr1", condition = input$select == "VPA")
      shinyjs::toggleState(id = "ilr2", condition = input$select == "MPA")
      shinyjs::toggleState(id = "ilr3", condition = input$select == "LPA")
      shinyjs::toggleState(id = "ilr4", condition = input$select == "SB")
      
      # if a different ilr is selected, slider = 1
      if(input$select != "VPA") updateSliderInput(session = session, inputId = "ilr1", value = 0)
      if(input$select != "MPA") updateSliderInput(session = session, inputId = "ilr2", value = 0)
      if(input$select != "LPA") updateSliderInput(session = session, inputId = "ilr3", value = 0)
      if(input$select != "SB") updateSliderInput(session = session, inputId = "ilr4", value = 0)
    })
    
    # PIE CHART 4 years old ------------------------------------------------
    # data for plot if ILR 1 modified
    observeEvent(input$select, {
      
      m4 = as.numeric(out[1, c("VPA4", "MPA4", "LPA4", "SB4", "Sleep4")])
      
      if (input$select == "VPA") row = reactive(which(out$ilr == 1 & round(out$VPA4) == input$ilr1))
      if (input$select == "MPA") row = reactive(which(out$ilr == 2 & round(out$MPA4) == input$ilr2))
      if (input$select == "LPA") row = reactive(which(out$ilr == 3 & round(out$LPA4) == input$ilr3))
      if (input$select == "SB")  row = reactive(which(out$ilr == 4 & round(out$SB4)  == input$ilr4))
      
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
      output$Table4 = DT::renderDataTable(DT::datatable(data.frame('Vigorous Physical Activity' = paste(round(data_plot()$values_4[1],1),"min/day"),
                                                                   'Moderate Physical Activity' = paste(round(data_plot()$values_4[2],1),"min/day"),
                                                                   'Light Physical Activity' = paste(round(data_plot()$values_4[3],1), "min/day"),
                                                                   'Sedentary Behaviour' = paste(round(data_plot()$values_4[4],1),"min/day"),
                                                                   'Sleep Time' = paste(round(data_plot()$values_4[5],1), "min/day"),
                                                                   check.names = F),
                                                        options = list(dom = 't', columnDefs = list(list(className = 'dt-center', targets = 0:4))), 
                                                        rownames = FALSE))
      
      # PIECHART 9 YEARS ---------------------------
      output$pieChart9 = plotly::renderPlotly({
        p = plotly::plot_ly(data_plot(), labels = ~factor(x), values = ~values_9, 
                            type = 'pie',
                            textinfo = "label",
                            insidetextfont = list(color = '#FFFFFF'),
                            hovertemplate = "<b>%{label}:</b>  %{text} <br>%{percent}</br><extra></extra>",
                            text = ~paste0(round(values_9), " min/day"),
                            marker = list(colors = c("#ffa600", "#ff6361", "#bc5090", "#58508d", "#003f5c"),
                                          line = list(color = '#FFFFFF', width = 2)),
                            showlegend = FALSE)
        p = plotly::layout(p, title = '\n')
        p
      })
      
      
      
      
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
                           yaxis = list(title = "min/day", range = c(-8, 8)))
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
                           yaxis = list(title = "kg/m2", range = c(-0.1, 0.2), 
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
                           yaxis = list(title = "laps", range = c(-0.3, 0.2), 
                                           showticklabels = FALSE),
                           xaxis = list(title = ""))
        p
      })
      
      output$barMotor9 = plotly::renderPlotly({
        p = plotly::plot_ly(Motor9(), x = ~factor(x), y = ~y, 
                            type = 'bar',
                            insidetextfont = list(color = '#FFFFFF'),
                            text = ~paste0(round(Motor9()$y, 3), " seconds"),
                            marker = list(colors = c("#ffa600", "#ff6361", "#bc5090", "#58508d", "#003f5c")[3]),
                            showlegend = FALSE)
        p = plotly::layout(p, yaxis = list(title = "Motor fitness", #range = c(-0.05, 0.15), 
                                           showticklabels = FALSE),
                           xaxis = list(title = ""))
        p
      })
      
      output$barStr9 = plotly::renderPlotly({
        p = plotly::plot_ly(Str9(), x = ~factor(x), y = ~y, 
                            type = 'bar',
                            insidetextfont = list(color = '#FFFFFF'),
                            text = ~paste0(round(Str9()$y, 3), " SDs"),
                            marker = list(colors = c("#ffa600", "#ff6361", "#bc5090", "#58508d", "#003f5c")[4]),
                            showlegend = FALSE)
        p = plotly::layout(p, yaxis = list(title = "Muscular fitness", #range = c(-0.05, 0.15), 
                                           showticklabels = FALSE),
                           xaxis = list(title = ""))
        p
      })
      
    })
  })
}

## To be copied in the UI
# mod_ilrs4_ui("ilrs4_ui_1")

## To be copied in the server
# mod_ilrs4_server("ilrs4_ui_1")
