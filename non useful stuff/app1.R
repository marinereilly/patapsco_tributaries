library(shiny)
library(ggplot2)
library(dplyr)


pat_full<-readRDS("pat_long.rds")
param_choices<-levels(as.factor(pat_full$parameter))

# describes how you look at the data/the User Interface
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
                           multiple = FALSE,
                           choices= param_choices)#,
              # radioButtons("depth", "Show:", c(
               #  "Surface Only"       = "SW",
                # "Bottom Only"        = "BW",
                 #"Surface and Bottom" = "both")),
               #checkboxInput("table", "Show Values")
               ),
  mainPanel(h3("the results will go here"),
            plotOutput("coolplot")#,
            #br(),
            #br(),
            #tableOutput("results")
            )
  )
#Describes how to show the data (aka the input/output)

server <- function(input, output) {
    output$coolplot <- renderPlot({
      pat_full %>% 
    filter(date>=input$date_input[1]) %>% 
    filter(date<=input$date_input[2]) %>%
    filter(parameter==input$wqparameter) %>% 
    ggplot(aes(x=creek, y=measurement, fill=my))+
    geom_col(position="dodge", color="black")+
    scale_fill_viridis_d()+
    ggtitle(paste0(input$wqparameter))+
    facet_grid(depth~station)+
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
          panel.background = element_rect(fill = "white"),
          #panel.border = element_rect(linetype = "solid", fill=NA),
          strip.background = element_rect(colour = "black", fill = "white"),
          legend.title = element_blank())
  
      
    })
}
shinyApp(ui = ui, server = server) #this must be the last line of code