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
                           min = 0.124520, max = 40.124520,
                           value = 10.124520, step = 2.105263,
                           round = TRUE, post = "min/day")
      ),
      hr(),
      div(
        shiny::sliderInput(inputId = ns("ilr2"),
                           label = "ILR 2: MPA vs LPA·SB·Sleep",
                           min = 24.14555, max = 84.14555,
                           value = 54.14555, step = 3.157895,
                           round = TRUE, post = "min/day")
      ),
      hr(),
      div(
        shiny::sliderInput(inputId = ns("ilr3"),
                           label = "ILR 3: LPA vs SB·Sleep",
                           min = 257.9626, max = 437.9626,
                           value = 347.9626, step = 9.473684,
                           round = TRUE, post = "min/day")
      ),
      hr(),
      div(
        shiny::sliderInput(inputId = ns("ilr4"),
                           label = "ILR 4: SB vs Sleep",
                           min = 213.2692, max = 513.2692,
                           value = 513.2692, step = 31.57895,
                           round = TRUE, post = "min/day")
      )
    ),
    shiny::mainPanel(
      div(
        col_6(
          plotly::plotlyOutput(ns("pieChart4"))
        ),
        col_6(
          plotly::plotlyOutput(ns("pieChart9"))
        )
      ),
      div(
        textOutput(ns("textFMI9"))
      ),
      div(
        textOutput(ns("textCRF9"))
      ),
      div(
        textOutput(ns("textMotor9"))
      ),
      div(
        textOutput(ns("textStr9"))
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
      if(input$select != "VPA") updateSliderInput(session = session, inputId = "ilr1", value = 10.124520)
      if(input$select != "MPA") updateSliderInput(session = session, inputId = "ilr2", value = 54.14555)
      if(input$select != "LPA") updateSliderInput(session = session, inputId = "ilr3", value = 347.9626)
      if(input$select != "SB") updateSliderInput(session = session, inputId = "ilr4", value = 513.2692)
    })
    
    # PIE CHART 4 years old ------------------------------------------------
    # data for plot if ILR 1 modified
    observeEvent(input$select, {
      
      out = MINISTOP::get_MS_predictions()
      
      
      if (input$select == "VPA") {
        row = reactive(which.min(abs(out$VPA4 - input$ilr1)))
        data_plot = reactive(data.frame(x = c("VPA", "MPA", "LPA", "SB", "Sleep"),
                                        values_4 = as.numeric(out[row(), c("VPA4", "MPA4", "LPA4", "SB4", "Sleep4")]),
                                        values_9 = as.numeric(out[row(), c("VPA9", "MPA9", "LPA9", "SB9", "Sleep9")])))
        FMI9 = reactive(as.numeric(out[row(), c("FMI9")]))
        CRF9 = reactive(as.numeric(out[row(), c("CRF9")]))
        Motor9 = reactive(as.numeric(out[row(), c("Motor9")]))
        Str9 = reactive(as.numeric(out[row(), c("Str9")]))
      }
      
      if (input$select == "MPA") {
        row = reactive(which.min(abs(out$MPA4 - input$ilr2)))
        data_plot = reactive(data.frame(x = c("VPA", "MPA", "LPA", "SB", "Sleep"),
                                        values_4 = as.numeric(out[row(), c("VPA4", "MPA4", "LPA4", "SB4", "Sleep4")]),
                                        values_9 = as.numeric(out[row(), c("VPA9", "MPA9", "LPA9", "SB9", "Sleep9")])))
        FMI9 = reactive(as.numeric(out[row(), c("FMI9")]))
        CRF9 = reactive(as.numeric(out[row(), c("CRF9")]))
        Motor9 = reactive(as.numeric(out[row(), c("Motor9")]))
        Str9 = reactive(as.numeric(out[row(), c("Str9")]))
      }
      
      if (input$select == "LPA") {
        row = reactive(which.min(abs(out$LPA4 - input$ilr3)))
        data_plot = reactive(data.frame(x = c("VPA", "MPA", "LPA", "SB", "Sleep"),
                                        values_4 = as.numeric(out[row(), c("VPA4", "MPA4", "LPA4", "SB4", "Sleep4")]),
                                        values_9 = as.numeric(out[row(), c("VPA9", "MPA9", "LPA9", "SB9", "Sleep9")])))
        FMI9 = reactive(as.numeric(out[row(), c("FMI9")]))
        CRF9 = reactive(as.numeric(out[row(), c("CRF9")]))
        Motor9 = reactive(as.numeric(out[row(), c("Motor9")]))
        Str9 = reactive(as.numeric(out[row(), c("Str9")]))
      }
      
      if (input$select == "SB") {
        row = reactive(which.min(abs(out$SB4 - input$ilr4)))
        data_plot = reactive(data.frame(x = c("VPA", "MPA", "LPA", "SB", "Sleep"),
                                        values_4 = as.numeric(out[row(), c("VPA4", "MPA4", "LPA4", "SB4", "Sleep4")]),
                                        values_9 = as.numeric(out[row(), c("VPA9", "MPA9", "LPA9", "SB9", "Sleep9")])))
        FMI9 = reactive(as.numeric(out[row(), c("FMI9")]))
        CRF9 = reactive(as.numeric(out[row(), c("CRF9")]))
        Motor9 = reactive(as.numeric(out[row(), c("Motor9")]))
        Str9 = reactive(as.numeric(out[row(), c("Str9")]))
        
      }
      
      output$pieChart4 = plotly::renderPlotly({
        p = plotly::plot_ly(data_plot(), labels = ~factor(x), values = ~values_4, 
                            type = 'pie',
                            # textposition = 'outside',
                            textinfo = "text",
                            insidetextfont = list(color = '#FFFFFF'),
                            # hoverinfo = "text",
                            hovertemplate = "<b>%{label}:</b>  %{text} <br>%{percent}</br><extra></extra>",
                            text = ~paste0(round(values_4), " min/day"),
                            marker = list(colors = c("#ffa600", "#ff6361", "#bc5090", "#58508d", "#003f5c"),
                                          line = list(color = '#FFFFFF', width = 2)),
                            showlegend = FALSE)
        p = plotly::layout(p, title = '<b>4 years old (Selected)</b>')
        p
      })
      
      # PIECHART 9 YEARS ---------------------------
      output$pieChart9 = plotly::renderPlotly({
        p = plotly::plot_ly(data_plot(), labels = ~factor(x), values = ~values_9, 
                            type = 'pie',
                            # textposition = 'outside',
                            textinfo = "text",
                            insidetextfont = list(color = '#FFFFFF'),
                            # hoverinfo = "text",
                            hovertemplate = "<b>%{label}:</b>  %{text} <br>%{percent}</br><extra></extra>",
                            text = ~paste0(round(values_9), " min/day"),
                            marker = list(colors = c("#ffa600", "#ff6361", "#bc5090", "#58508d", "#003f5c"),
                                          line = list(color = '#FFFFFF', width = 2)),
                            showlegend = FALSE)
        p = plotly::layout(p, title = '<b>9 years old (Predicted)</b>')
        p
      })
      
      # PREDICTIONS BODY COMPOSITION AND PHYSICAL FITNESS ---------------------------
      output$textFMI9 = renderText(c("Predicted change in FMI at 9 years: ", as.character(round(FMI9(), 3)), " kg/m2"))
      output$textCRF9 = renderText(c("Predicted change in CRF at 9 years: ", as.character(round(CRF9(), 3)), " laps"))
      output$textMotor9 = renderText(c("Predicted change in Motor fitness at 9 years: ", as.character(round(Motor9(), 3)), " s"))
      output$textStr9 = renderText(c("Predicted change in Muscular fitness at 9 years: ", as.character(round(Str9(), 3)), " kg"))
      
      
    })
  })
}

## To be copied in the UI
# mod_ilrs4_ui("ilrs4_ui_1")

## To be copied in the server
# mod_ilrs4_server("ilrs4_ui_1")
