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
                     choices = c("ILR1", "ILR2", "ILR3", "ILR4"), 
                     label = "Select the ILR to change", 
                     selected = "ILR1", inline = T),
      ),
      hr(),
      div(
        shiny::sliderInput(inputId = ns("ilr1"),
                           label = "ILR 1: VPA vs MPA·LPA·SB·Sleep",
                           min = 0.01229853, max = 3.96310440,
                           value = 1, step = 0.1039686)
      ),
      hr(),
      div(
        shiny::sliderInput(inputId = ns("ilr2"),
                           label = "ILR 2: MPA vs LPA·SB·Sleep",
                           min = 0.4459378, max = 1.5540622,
                           value = 1, step = 0.05832233)
      ),
      hr(),
      div(
        shiny::sliderInput(inputId = ns("ilr3"),
                           label = "ILR 3: LPA vs SB·Sleep",
                           min = 0.7413515, max = 1.2586485,
                           value = 1, step = 0.02722615 )
      ),
      hr(),
      div(
        shiny::sliderInput(inputId = ns("ilr4"),
                           label = "ILR 4: SB vs Sleep",
                           min = 0.4155114, max = 1.5844886,
                           value = 1, step = 0.06152512)
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
        col_6(
          textOutput(ns("textILRm4"))
        ),
        col_6(
          textOutput(ns("textILRm9"))
        )
      ),
      div(
        col_6(
          textOutput(ns("textILR4"))
        ),
        col_6(
          textOutput(ns("textILR9"))
        )
      ),
      div(
        col_6(
          textOutput(ns("a4"))
        ),
        col_6(
          textOutput(ns("a9"))
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
    observeEvent(input$select, {
      # Enable/disable sliders
      shinyjs::toggleState(id = "ilr1", condition = input$select == "ILR1")
      shinyjs::toggleState(id = "ilr2", condition = input$select == "ILR2")
      shinyjs::toggleState(id = "ilr3", condition = input$select == "ILR3")
      shinyjs::toggleState(id = "ilr4", condition = input$select == "ILR4")
      
      # if a different ilr is selected, slider = 1
      if(input$select != "ILR1") updateSliderInput(session = session, inputId = "ilr1", value = 1)
      if(input$select != "ILR2") updateSliderInput(session = session, inputId = "ilr2", value = 1)
      if(input$select != "ILR3") updateSliderInput(session = session, inputId = "ilr3", value = 1)
      if(input$select != "ILR4") updateSliderInput(session = session, inputId = "ilr4", value = 1)
    })
    
    # PIE CHART 4 years old ------------------------------------------------
    m4 = c(10.12452, 54.14555, 347.96264, 513.26918, 514.49811)
    m9 = c(12.71212, 55.71468, 293.33485, 536.14679, 542.09155)
    compo_names = c("VPA", "MPA", "LPA", "SB", "Sleep")
    sbp <- matrix(c(  1, -1, -1, -1, -1,
                      0,  1, -1, -1, -1,
                      0,  0,  1, -1, -1,
                      0,  0,  0,  1, -1),
                  ncol = 5, byrow = TRUE)
    psi <- compositions::gsi.buildilrBase(t(sbp))
    
    ilr.m4 = compositions::ilr(m4, V = psi)
    ilr.m9 = compositions::ilr(m9, V = psi)
    
    # b values for ilr 2
    b_ilr2 = data.frame(a = c(seq((m4[2] - 30)/m4[2], (m4[2]+30)/m4[2], length.out = 20), 1),
                        b = c(1.02305, 1.02057, 1.01812, 1.01567, 1.01324, 
                              1.01081, 1.0084, 1.00599, 1.00359, 1.0012,
                              0.998806, 0.996421,0.994041, 0.991664, 0.989292,
                              0.986923, 0.984556, 0.982193, 0.979833, 0.977475, 1))
    
    # b values for ilr 3
    b_ilr3 = data.frame(a = c(seq((m4[3]-90)/m4[3], (m4[3]+90)/m4[3], length.out = 20), 1),
                        b = c(1.09015, 1.08057, 1.07101, 1.06147, 1.05195, 
                              1.04246, 1.03299, 1.02354, 1.01411, 1.0047,
                              0.995305, 0.985929, 0.97657, 0.967228, 0.957901,
                              0.94859, 0.939294, 0.930013, 0.920746, 0.911493, 1))
    
    # b values for ilr 4
    b_ilr4 = data.frame(a = c(seq((m4[4]-300)/m4[4], (m4[4]+300)/m4[4], length.out = 20), 1),
                        b = c(1.70747129300068, 1.61765, 1.53258396271130, 1.45163158459809, 1.37429607331649, 
                              1.30019763836909, 1.22903672376019, 1.16057215920667, 1.09460734053028, 1.03098011732903,
                              0.969555539924622, 0.910221021541094, 0.852881845533020, 0.797458632034325, 0.743885001892015,
                              0.692105785227467, 0.642076049762841, 0.593759607742649, 0.547128623508218, 0.502163009780822, 1))
    
    
    # data for plot if ILR 1 modified
    observeEvent(input$select, {
      
      if (input$select == "ILR1") {
        a = reactive(input$ilr1)
        b = reactive((1440 - (m4[1]*a())) / (m4[2] + m4[3] + m4[4] + m4[5]))
        
        data_plot = reactive(data.frame(x = c("VPA", "MPA", "LPA", "SB", "Sleep"),
                                        values = c(m4[1]*a(),
                                                   m4[2]*b(),
                                                   m4[3]*b(),
                                                   m4[4]*b(),
                                                   m4[5]*b())))
        # data for 9 years old
        ilr.4 = reactive(compositions::ilr(data_plot()$values, V = psi))
        ilr.9 = reactive(c(ilr.m9[1] + (0.205*(ilr.4()[1] - ilr.m4[1])), ilr.m9[2:4]))
        
        a9 = reactive(ilr.m9[1] / ilr.9()[1])
        b9 = reactive((1440 - (m9[1]*a9())) / (m9[2] + m9[3] + m9[4] + m9[5]))
        
        data_plot9 = reactive(data.frame(x = c("VPA", "MPA", "LPA", "SB", "Sleep"),
                                         values = c(m9[1]*a9(),
                                                    m9[2]*b9(),
                                                    m9[3]*b9(),
                                                    m9[4]*b9(),
                                                    m9[5]*b9())))
        
      }
      
      if (input$select == "ILR2") {
        #1440 = m[1]*(a*b*b*b)^(1/4) + m[2]*a + m[3]*b + m[4]*b +m[5]*b
        #1440 = 10.12452*(a*b*b*b)^(1/4) + 54.14555*a + 347.96264*b + 513.26918*b +514.49811*b
        #I plugged in the real numbers for m and a, and used an online calculator. https://www.symbolab.com/solver/equation-calculator
        a = reactive(input$ilr2)
        what_b = reactive(which(abs(b_ilr2$a - a()) == min(abs(b_ilr2$a - a()))))
        b = reactive(b_ilr2[what_b(), "b"])
        
        data_plot = reactive(data.frame(x = c("VPA", "MPA", "LPA", "SB", "Sleep"),
                                        values = c(m4[1]*(a()*b()^3)^(1/4),
                                                   m4[2]*a(),
                                                   m4[3]*b(),
                                                   m4[4]*b(),
                                                   m4[5]*b())))
        
        # data for 9 years old
        ilr.4 = reactive(compositions::ilr(data_plot()$values, V = psi))
        ilr.9 = reactive(c(ilr.m9[1], ilr.m9[2] + (0.181*(ilr.4()[2] - ilr.m4[2])), ilr.m9[3:4]))
        
        a9 = reactive(ilr.m9[2] / ilr.9()[2])
        b9 = reactive((1440 - (m9[1]*a9())) / (m9[2] + m9[3] + m9[4] + m9[5]))
        
        data_plot9 = reactive(data.frame(x = c("VPA", "MPA", "LPA", "SB", "Sleep"),
                                         values = c(m9[1]*a9(),
                                                    m9[2]*b9(),
                                                    m9[3]*b9(),
                                                    m9[4]*b9(),
                                                    m9[5]*b9())))
      }
      
      if (input$select == "ILR3") {
        #1440 = m[1]*(((a*b*b)^(1/3))^(1/4) * (a*b*b)^(1/4)) + m[2]*((a*b*b)^(1/3))  + m[3]*a + m[4]*b +m[5]*b
        #1440 = 10.12452*(((a*b*b)^(1/3))^(1/4) * (a*b*b)^(1/4)) + 54.14555*((a*b*b)^(1/3)) + 347.96264*a + 513.26918*b +514.49811*b
        #I plugged in the real numbers for m and a, and used an online calculator. https://www.symbolab.com/solver/equation-calculator
        a = reactive(input$ilr3)
        what_b = reactive(which(abs(b_ilr3$a - a()) == min(abs(b_ilr3$a - a()))))
        b = reactive(b_ilr3[what_b(), "b"])
        
        data_plot = reactive(data.frame(x = c("VPA", "MPA", "LPA", "SB", "Sleep"),
                                        values = c(m4[1]*((a()*b()*b())^(1/3))^(1/4) * (a()*b()*b())^(1/4),
                                                   m4[2]*(a()*b()*b())^(1/3),
                                                   m4[3]*a(),
                                                   m4[4]*b(),
                                                   m4[5]*b())))
      }
      
      if (input$select == "ILR4") {
        #1440 = m[1]*(a*b)^(1/4)*(sqrt(a*b)^(1/4))*((sqrt(a*b)*(a*b))^(1/3))^(1/4) + m[2]*(sqrt(a*b)*(a*b))^(1/3) + m[3]*sqrt(a*b) + m[4]*a +m[5]*b
        #1440 = 10.12452*(a*b)^(1/4)*(sqrt(a*b)^(1/4))*((sqrt(a*b)*(a*b))^(1/3))^(1/4) + 54.14555*(sqrt(a*b)*(a*b))^(1/3) + 347.96264*sqrt(a*b) + 513.26918*a +514.49811*b
        #I plugged in the real numbers for m and a, and used an online calculator. https://www.symbolab.com/solver/equation-calculator
        a = reactive(input$ilr4)
        what_b = reactive(which(abs(b_ilr4$a - a()) == min(abs(b_ilr4$a - a()))))
        b = reactive(b_ilr4[what_b(), "b"])
        
        
        data_plot = reactive(data.frame(x = c("VPA", "MPA", "LPA", "SB", "Sleep"),
                                        values = c(m4[1]*(a()*b())^(1/4)*(sqrt(a()*b())^(1/4))*((sqrt(a()*b())*(a()*b()))^(1/3))^(1/4),
                                                   m4[2]*(sqrt(a()*b())*(a()*b()))^(1/3),
                                                   m4[3]*sqrt(a()*b()),
                                                   m4[4]*a(),
                                                   m4[5]*b())))
      }
      
      output$pieChart4 = plotly::renderPlotly({
        p = plotly::plot_ly(data_plot(), labels = ~factor(x), values = ~values, 
                            type = 'pie',
                            # textposition = 'outside',
                            textinfo = "text",
                            insidetextfont = list(color = '#FFFFFF'),
                            # hoverinfo = "text",
                            hovertemplate = "<b>%{label}:</b>  %{text} <br>%{percent}</br><extra></extra>",
                            text = ~paste0(round(values), " min/day"),
                            marker = list(colors = c("#ffa600", "#ff6361", "#bc5090", "#58508d", "#003f5c"),
                                          line = list(color = '#FFFFFF', width = 2)),
                            showlegend = FALSE)
        p = plotly::layout(p, title = '<b>4 years old (Selected)</b>')
        p
      })
      
      # PIECHART 9 YEARS ---------------------------
      output$pieChart9 = plotly::renderPlotly({
        p = plotly::plot_ly(data_plot9(), labels = ~factor(x), values = ~values, 
                            type = 'pie',
                            # textposition = 'outside',
                            textinfo = "text",
                            insidetextfont = list(color = '#FFFFFF'),
                            # hoverinfo = "text",
                            hovertemplate = "<b>%{label}:</b>  %{text} <br>%{percent}</br><extra></extra>",
                            text = ~paste0(round(values), " min/day"),
                            marker = list(colors = c("#ffa600", "#ff6361", "#bc5090", "#58508d", "#003f5c"),
                                          line = list(color = '#FFFFFF', width = 2)),
                            showlegend = FALSE)
        p = plotly::layout(p, title = '<b>9 years old (Predicted)</b>')
        p
      })
      
      output$textILRm4 = renderText(c("Mean:", as.character(round(ilr.m4, 3))))
      output$textILRm9 = renderText(c("Mean:", as.character(round(ilr.m9, 3))))
      
      output$textILR4 = renderText(c("New:", as.character(round(ilr.4(), 3))))
      output$textILR9 = renderText(c("New:", as.character(round(ilr.9(), 3))))
      
      output$a4 = renderText(as.character(c("a = ", round(a(), 3))))
      output$a9 = renderText(as.character(c("a = ", round(a9(), 3))))
      
    })
    
    
    
    
    
    # return for use in other modules -----------------------------------------
    return(list(
      ilr1 = reactive(input$ilr1),
      ilr2 = reactive(input$ilr2), 
      ilr3 = reactive(input$ilr3), 
      ilr4 = reactive(input$ilr4)
    ))
    
  })
}

## To be copied in the UI
# mod_ilrs4_ui("ilrs4_ui_1")

## To be copied in the server
# mod_ilrs4_server("ilrs4_ui_1")
