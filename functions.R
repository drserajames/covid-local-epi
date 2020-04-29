# transform data for the different plots
data_transform <- function(data, type,  pop, name=rownames(data)){
  new_data <- data
  if (type<4){
    new_data <- t(apply(data,1,cumsum))
  }
  if (type==2 | type==5){
    new_data <- new_data/pop$age_all[name]
  }
  if (type==3 | type==6){
    new_data <- new_data/pop$age60[name]
  }
  return(new_data)
}

# plot functions
plot_data <- function(date, data, name=rownames(data), xax=as.Date(input$date), yax="default", cols=multi_col, ylb="", mn="", log=F,lty=1){
  fig <- plot_ly()
  for (i in 1:length(name)){
    fig <- add_trace(fig, x = date, y = data[name[i],], name=name[i], type = 'scatter', mode = 'lines')
  }
  if(log==T){
    fig <- layout(fig, yaxis = list(type = "log"))
  }
  
  fig <- add_trace(fig, x = date[1], y = 0, name="                                                     ", 
                   type = 'scatter', mode = 'lines', line = list(color ="white"))
  fig <- fig %>% layout(showlegend =T)
  
  
  return(fig)
}

plot_title <- function(fig, input, j, src="phe", ons_type=1){
  fig <- fig %>%   layout(  title = "Regions",
                            xaxis = list(title = "Date"),
                            yaxis = list(title = "Cases"))
  fig <- fig %>%   layout(xaxis = list(title = "Date"))
  # abline(v=Sys.Date(), lwd=2)
  if (src=="phe"){
    if (input$radio_type==1){
      fig <- fig %>%   layout(yaxis = list(title = "Cumulative cases"))
    }
    if (input$radio_type==2){
      fig <- fig %>%   layout(yaxis = list(title ="Cumulative cases per 100,000 persons"))
      }
    if (input$radio_type==3){
      fig <- fig %>%   layout(yaxis = list(title ="Cumulative cases per 100,000 persons aged over 50"))
    }
    if (input$radio_type==4){
      fig <- fig %>%   layout(yaxis = list(title ="New cases"))
    }
    if (input$radio_type==5){
      fig <- fig %>%   layout(yaxis = list(title ="New cases per 100,000 persons"))
    }
    if (input$radio_type==6){
      fig <- fig %>%   layout(yaxis = list(title ="New cases per 100,000 persons aged over 50"))
    }
 
  }
  
  if (src=="ons"){
    if (input$radio_type==1){
      fig <- fig %>%   layout(yaxis = list(title ="Cumulative deaths"))
    }
    if (input$radio_type==2){
      fig <- fig %>%   layout(yaxis = list(title ="Cumulative deaths per 100,000 persons"))
    }
    if (input$radio_type==3){
      fig <- fig %>%   layout(yaxis = list(title ="Cumulative deaths per 100,000 persons aged over 50"))
    }
    if (input$radio_type==4){
      fig <- fig %>%   layout(yaxis = list(title ="New deaths"))
    }
    if (input$radio_type==5){
      fig <- fig %>%   layout(yaxis = list(title ="New deaths per 100,000 persons"))
    }
    if (input$radio_type==6){
      fig <- fig %>%   layout(yaxis = list(title ="New deaths per 100,000 persons aged over 50"))
    }
    
  }
  
  if(j==0){
    fig <- fig %>%   layout(title = "Local authority districts")
  }
  
  if(j==1){
    fig <- fig %>%   layout(title = "Upper tier local authorities")
  }
  if(j==2){
    fig <- fig %>%   layout(title = "Local resilience forums")
  }
  if(j==3){
    fig <- fig %>%   layout(title = "Regions")
  }
  return(fig)
}

plot_rect <- function(fig, input, data, log=F, src="phe"){
  if(src=="phe"){
  if (log==F){
    ymin <- 0
  }else{
    ymin <- 1
    if (input$radio_type==2){
      ymin <- 0.01
    }
    if (input$radio_type==3){
      ymin <- 0.01
    }
    if (input$radio_type==5){
      ymin <- 0.01
    }
    if (input$radio_type==6){
      ymin <- 0.05
    }
  }
    fig <- layout(fig, shapes =
                    list(type = "rect",
                         fillcolor = "grey", line = list(color = "grey"), opacity = 0.3,
                         x0 = Sys.Date()-5, x1 = Sys.Date(), xref = "x",
                         y0 = ymin, y1 = max(data, na.rm=T)*1.05, yref = "y"))
  }
  if (src=="ons"){
    fig <- layout(fig, shapes =list(type = "line",
                                    line = list(color = "black"),
                                    x0=Sys.Date(), x1 = Sys.Date(),
                                    y0 = 0, y1 = max(data, na.rm=T)*1.05,
                                    xref = "x", yref = "y"))
  }
return(fig)
}

plot_regional <- function(input, date, data, log=F, src="phe", ons_type=1){
  if (src=="phe"){
    fig2 <- plot_data(date, data[[3]][[as.numeric(input$radio_type)]], log=log)
    fig2 <- plot_title(fig2, input, 3)
    fig2 <- plot_rect(fig2, input, data[[3]][[as.numeric(input$radio_type)]], log=log, src=src)
  }
  if (src=="ons"){
    fig2 <- plot_data(date, data[[4]][[ons_type]][[as.numeric(input$radio_type)]], log=log)
    fig2 <- plot_title(fig2, input, 3, src="ons", ons_type)
    fig2 <- plot_rect(fig2, input, data[[4]][[ons_type]][[as.numeric(input$radio_type)]], log=log, src=src)
  }
  
  return(fig2)
}

plot_lrf <- function(input, date, data, log=F, src="phe", ons_type=1){
  if (src=="phe"){
    if(input$radio_regional==1){
      fig2 <- plot_data(date, data[[2]][[as.numeric(input$radio_type)]], log=log)
      fig2 <- plot_rect(fig2, input, data[[2]][[as.numeric(input$radio_type)]], log=log, src=src)
    }else{
      fig2 <- plot_data(date, data[[2]][[as.numeric(input$radio_type)]], 
                        name=names(lrf[[as.numeric(input$radio_regional)]][2:length(lrf[[as.numeric(input$radio_regional)]])]), log=log)
      fig2 <- plot_rect(fig2, input, data[[2]][[as.numeric(input$radio_type)]][names(lrf[[as.numeric(input$radio_regional)]][2:length(lrf[[as.numeric(input$radio_regional)]])]),], log=log, src=src)
    }
    fig2 <- plot_title(fig2, input, 2)
  }
  if (src=="ons"){
    if(input$radio_regional==1){
      fig2 <- plot_data(date, data[[3]][[ons_type]][[as.numeric(input$radio_type)]], log=log)
      fig2 <- plot_rect(fig2, input, data[[3]][[ons_type]][[as.numeric(input$radio_type)]], log=log, src=src)
    }else{
      fig2 <- plot_data(date, data[[3]][[ons_type]][[as.numeric(input$radio_type)]], 
                        name=names(lrf[[as.numeric(input$radio_regional)]][2:length(lrf[[as.numeric(input$radio_regional)]])]), log=log)
      fig2 <- plot_rect(fig2, input, data[[3]][[ons_type]][[as.numeric(input$radio_type)]][names(lrf[[as.numeric(input$radio_regional)]][2:length(lrf[[as.numeric(input$radio_regional)]])]),], log=log, src=src)
      
          }
    fig2 <- plot_title(fig2, input, 2, src="ons", ons_type)
  }
  return(fig2)
}

plot_utla <- function(input, date, data, log=F, src="phe", ons_type=1){
  input_lrf <- input[[paste0("radio_lrf", input$radio_regional)]]
  if (src=="phe"){
    if(input$radio_regional==1){
      fig2 <- plot_data(date, data[[1]][[as.numeric(input$radio_type)]], log=log)
      fig2 <- plot_rect(fig2, input, data[[1]][[as.numeric(input$radio_type)]], log=log, src=src)
      
    }else{
      if (input_lrf==1){
        fig2 <- plot_data(date, data[[1]][[as.numeric(input$radio_type)]], 
                          name=unique(names(unlist(utla[[as.numeric(input$radio_regional)]])))[2:length(unique(names(unlist(utla[[as.numeric(input$radio_regional)]]))))], log=log)
        fig2 <- plot_rect(fig2, input, data[[1]][[as.numeric(input$radio_type)]][unique(names(unlist(utla[[as.numeric(input$radio_regional)]])))[2:length(unique(names(unlist(utla[[as.numeric(input$radio_regional)]]))))],], log=log, src=src)
        
              }else{
        fig2 <- plot_data(date, data[[1]][[as.numeric(input$radio_type)]], 
                          name=names(utla[[as.numeric(input$radio_regional)]][[as.numeric(input_lrf)]])[2:length(names(utla[[as.numeric(input$radio_regional)]][[as.numeric(input_lrf)]]))], log=log)
        fig2 <- plot_rect(fig2, input, data[[1]][[as.numeric(input$radio_type)]][names(utla[[as.numeric(input$radio_regional)]][[as.numeric(input_lrf)]])[2:length(names(utla[[as.numeric(input$radio_regional)]][[as.numeric(input_lrf)]]))],], log=log, src=src)
        
              }
    }
    fig2 <- plot_title(fig2, input, 1)
  }
  if (src=="ons"){
    if(input$radio_regional==1){
      fig2 <- plot_data(date, data[[2]][[ons_type]][[as.numeric(input$radio_type)]], log=log)
      fig2 <- plot_rect(fig2, input, data[[2]][[ons_type]][[as.numeric(input$radio_type)]], log=log, src=src)
    }else{
      if (input_lrf==1){
        fig2 <- plot_data(date, data[[2]][[ons_type]][[as.numeric(input$radio_type)]], 
                          name=unique(names(unlist(utla2[[as.numeric(input$radio_regional)]])))[2:length(unique(names(unlist(utla2[[as.numeric(input$radio_regional)]]))))], log=log)
        fig2 <- plot_rect(fig2, input, data[[2]][[ons_type]][[as.numeric(input$radio_type)]][unique(names(unlist(utla2[[as.numeric(input$radio_regional)]])))[2:length(unique(names(unlist(utla2[[as.numeric(input$radio_regional)]]))))],], log=log, src=src)
              }else{
        fig2 <- plot_data(date, data[[2]][[ons_type]][[as.numeric(input$radio_type)]], 
                          name=names(utla2[[as.numeric(input$radio_regional)]][[as.numeric(input_lrf)]])[2:length(names(utla2[[as.numeric(input$radio_regional)]][[as.numeric(input_lrf)]]))], log=log)
        fig2 <- plot_rect(fig2, input, data[[2]][[ons_type]][[as.numeric(input$radio_type)]][names(utla2[[as.numeric(input$radio_regional)]][[as.numeric(input_lrf)]])[2:length(names(utla2[[as.numeric(input$radio_regional)]][[as.numeric(input_lrf)]]))],], log=log, src=src)
              }
    }
    fig2 <- plot_title(fig2, input, 1, src="ons", ons_type)
  }
  return(fig2)
}
# plot phe/ons (different deaths)
# plot region/lrf/la (with changes in lower level plot)

plot_ltla <- function(input, date, data, log=F, src="ons", ons_type=1){
  input_lrf <- input[[paste0("radio_lrf", input$radio_regional)]]
  if(input$radio_regional==1){
    fig2 <- plot_data(date, data[[1]][[ons_type]][[as.numeric(input$radio_type)]], log=log)
    fig2 <- plot_rect(fig2, input, data[[1]][[ons_type]][[as.numeric(input$radio_type)]], log=log, src=src)
  }else{
    if (input_lrf==1){
      name_all <- unique(names(unlist(ltla[[as.numeric(input$radio_regional)]])))
      fig2 <- plot_data(date, data[[1]][[ons_type]][[as.numeric(input$radio_type)]], 
                        name=name_all[2:length(name_all)], log=log)
      fig2 <- plot_rect(fig2, input, data[[1]][[ons_type]][[as.numeric(input$radio_type)]][name_all[2:length(name_all)],], log=log, src=src)
    }else{
      name_all <- unique(names(unlist(ltla[[as.numeric(input$radio_regional)]][[as.numeric(input_lrf)]])))
      fig2 <- plot_data(date, data[[1]][[ons_type]][[as.numeric(input$radio_type)]], 
                        name=name_all[2:length(name_all)], log=log)
      fig2 <- plot_rect(fig2, input, data[[1]][[ons_type]][[as.numeric(input$radio_type)]][name_all[2:length(name_all)],], log=log, src=src)
    }
  }
  fig2 <- plot_title(fig2, input, 0, src="ons", ons_type)
  return(fig2)
}

# line_lon <- function(fig, date, data){
#   fig <- add_trace(fig, x = date, y = data["London",], name="London", type = 'scatter', mode = 'lines')
#   return(fig)
# }
# 
# line_eng <- function(input, date, data, lty=2){
#   plot_data(date, data, name="England", cols="black", plot=F, legend=F, lty=lty)
# }