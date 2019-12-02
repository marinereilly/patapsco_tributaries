library(shiny)
library(ggplot2)
library(dplyr)

#Add load data stuff here 
pat_full<-readRDS("patapsco_wide.rds")

ui <- fluidPage(
  titlePanel("Patapsco Tributaries"),
  sidebarPanel(h3("Things to Change"),
               #maybe slider instead?
               dateRangeInput("date_input",
                              "Date Range",
                              format = "M-yyyy",
                              start = "2018-04-01",
                              end   = "2019-12-31"
                              ),
               selectInput("wqparameter",
                           "Parameter:",
                           multiple = FALSE),
               radioButtons("depth", "Show:", c(
                 "Surface Only"       = "SW",
                 "Bottom Only"        = "BW",
                 "Surface and Bottom" = "both")),
               checkboxInput("table", "Show Values")
               )
  mainPanel(h3("the results will go here"),
            plotOutput("coolplot"),
            br(),
            br(),
            tableOutput("results")
            )
  )
server <- function(input, output) {
  #Define Palatte here patpal<-c()
  filter<-pat_full %>% 
    filter(
      date >= input$date_input[1],
      date <= input$date_input[2],
      parameter == input$wqparameter)
  ggplot(data = filter, aes(x=creek, y=value, fill=date_label))+
    geom_col(position = "dodge")+scale_fill_manual(values=patpal)+
    facet_grid(station~.)+
    ggtitle(input$wqparameter)+ylab(input$wqparameter)
      
}
shinyApp(ui = ui, server = server) #this must be the last line of code