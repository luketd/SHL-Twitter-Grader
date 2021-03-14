library (shiny)
library (rsconnect)
library(tidyverse)  
library(rvest)  
library(rtweet)
library(dplyr) 
library(purrr)


if (interactive()) {
  

ui <- fluidPage(
    tableOutput("static"),
  )
server <- function(input, output, session) {
  output$static <- renderTable(head(SHL))
}
      
}
shinyApp(ui = ui, server = server)
