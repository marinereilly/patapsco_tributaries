library(shiny)
library(ggplot2)
library(dplyr)


pat_full<-readRDS("pat_long.rds")
param_choices<-levels(as.factor(pat_full$parameter))
creeks<-levels(as.factor(pat_full$creek))
stations<-levels(as.factor(pat_full$station))
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
               br(),
               selectInput("wqparameter",
                           "Parameter:",
                           multiple = FALSE,
                           choices= param_choices),
               h4("For Data Table Only:"),
               selectInput("crk",
                           "What Creek:", 
                           multiple = FALSE,
                           choices=creeks),
               selectInput("stn",
                           "What Station:", 
                           multiple = FALSE,
                           choices=stations)#,
              # radioButtons("depth", "Show:", c(
               #  "Surface Only"       = "SW",
                # "Bottom Only"        = "BW",
                 #"Surface and Bottom" = "both")),
               #checkboxInput("table", "Show Values")
               ),
  
  mainPanel(tabsetPanel(type = "tabs",
                        id = "tabselected",
                        
                        tabPanel("By Station",
                                 h3("Plot"),
                                 br(),
                                 plotOutput("stationplot"),
                                 br(),
                                 br(),
                                 h3("Data"),
                                 tableOutput("stationtable")
                                 ),
                        tabPanel("By Creek",
                                 h3("Plot"),
                                 br(),
                                 plotOutput("creekplot"),
                                 br(),
                                 br(),
                                 h3("Data"),
                                 tableOutput("creektable"))
                        )
            )
  )
#Describes how to show the data (aka the input/output)

server <- function(input, output) {
    
  output$stationplot <- renderPlot({
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
   
   output$stationtable <-renderTable({
      filter<-pat_full %>% 
        filter(date>=input$date_input[1]) %>% 
        filter(date<=input$date_input[2]) %>%
        filter(parameter==input$wqparameter) %>% 
        filter(station==input$stn) %>% 
        select(-date) %>% 
        arrange(my,station,creek)
      filter
    })
    
    output$creektable <-renderTable({
      filter<-pat_full %>% 
        filter(date>=input$date_input[1]) %>% 
        filter(date<=input$date_input[2]) %>%
        filter(parameter==input$wqparameter) %>% 
        filter(creek==input$crk) %>% 
        select(-date) %>% 
        arrange(my,creek,station)
      filter
    })
      
    output$creekplot <-renderPlot({
        pat_full %>% 
        filter(date>=input$date_input[1]) %>% 
        filter(date<=input$date_input[2]) %>%
        filter(parameter==input$wqparameter) %>% 
        ggplot(aes(x=station, y=measurement, fill=my))+
        geom_col(position="dodge", color="black")+
        scale_fill_viridis_d()+
        ggtitle(paste0(input$wqparameter))+
        facet_grid(depth~creek)+
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
              panel.background = element_rect(fill = "white"),
              #panel.border = element_rect(linetype = "solid", fill=NA),
              strip.background = element_rect(colour = "black", fill = "white"),
              legend.title = element_blank())
    })
}
shinyApp(ui = ui, server = server) #this must be the last line of code