# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  #tab 1
  # output$nationalPlot <- renderPlot({
  #   national_plot(input, phe_cou_date, phe_cou_data, col_cou, log=F, type=input$radio_type)
  # })
  
  output$regionalPlot <- renderPlotly({
plot_regional(input, phe_date, cases) 

  })
  
  output$lrfPlot <- renderPlotly({
    plot_lrf(input, phe_date, cases)
  })
  
  output$utlaPlot <- renderPlotly({
    plot_utla(input, phe_date, cases)
  })
  
  output$ltlaPlot <- renderPlotly({
    plot_ltla(input, phe_date, cases)
  })
  
  # tab 2
  # output$nationalLogPlot <- renderPlot({
  #   national_plot(input, phe_cou_date, phe_cou_data, col_cou, log=T, type=input$radio_type)
  # })
  
  output$regionalLogPlot <- renderPlotly({
    plot_regional(input, phe_date, cases, log=T)
  })
  
  output$lrfLogPlot <- renderPlotly({
    plot_lrf(input, phe_date, cases, log=T)
  })
  
  output$utlaLogPlot <- renderPlotly({
    plot_utla(input, phe_date, cases, log=T)
  })
  
  output$ltlaLogPlot <- renderPlotly({
    plot_ltla(input, phe_date, cases, log=T)
  })
  
  # tab 3
  
  output$ltladeathPlot <- renderPlotly({
    plot_ltla(input, ons_date, deaths, src="ons")
  })
  
  output$utladeathPlot <- renderPlotly({
    plot_utla(input, ons_date, deaths, src="ons")
  }) 
  
  output$lrfdeathPlot <- renderPlotly({
    plot_lrf(input, ons_date, deaths, src="ons")
  }) 
  
  output$regdeathPlot <- renderPlotly({
    plot_regional(input, ons_date, deaths, src="ons")
  }) 
  # tab 4
  
  output$ltladeathallPlot <- renderPlotly({
    plot_ltla(input, ons_date, deaths, src="ons", ons_type=2)
  })
  
  output$utladeathallPlot <- renderPlotly({
    plot_utla(input, ons_date, deaths, src="ons", ons_type=2)
  }) 
  
  output$lrfdeathallPlot <- renderPlotly({
    plot_lrf(input, ons_date, deaths, src="ons", ons_type=2)
  }) 
  
  output$regdeathallPlot <- renderPlotly({
    plot_regional(input, ons_date, deaths, src="ons", ons_type=2)
  }) 
  
  # tab 5
  
  output$ltladeathnonPlot <- renderPlotly({
    plot_ltla(input, ons_date, deaths, src="ons", ons_type=3)
  })
  
  output$utladeathnonPlot <- renderPlotly({
    plot_utla(input, ons_date, deaths, src="ons", ons_type=3)
  }) 
  
  output$lrfdeathnonPlot <- renderPlotly({
    plot_lrf(input, ons_date, deaths, src="ons", ons_type=3)
  }) 
  
  output$regdeathnonPlot <- renderPlotly({
    plot_regional(input, ons_date, deaths, src="ons", ons_type=3)
  }) 

  
  
}