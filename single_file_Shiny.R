library(dygraphs)
library(shiny)
library(dplyr)
library(reshape2)
library(ggplot2)
library(plotly)
library(maps)


#Global Variariable
#Predicted CO2 emissions(ELSV)
CO2_ELSV<-read.csv("~/DDP_Finalproject/data3.csv", stringsAsFactors = FALSE)
#creating time serie object, generating data via sampling.
df <- ts(sample(CO2_ELSV$CO2.emissions., 50, replace = TRUE), frequency= 12 , start=c(2013,1))
#Adjusted Net National Income (annual % growth)
incData<-read.csv("~/DDP_Finalproject/data1.csv", stringsAsFactors = FALSE)
#Central America Map
df_CA <- world.cities %>% filter( country.etc == c("Belize", "Guatemala", "El Salvador", "Honduras", "Nicaragua",
                                                   "Costa Rica", "Panama" ))


ui <- navbarPage(strong("Central America at Glance"),
                 tabPanel(
                   sidebarLayout(position = "right",
                                 sidebarPanel(img(src = "CA.png", height = 200, width = 200)),
                   mainPanel(
                     p( "The following interactive displays are meant to provide a brief information regarding the Central America region:"),
                     p("1-	Reactive application regarding CO2 emissions in the Republic of El Salvador. It predicts the amount of CO2-emission (metric tons per capita) in the time span of choice."),
                     p("2-	Interactive graph that generates the adjusted national income differences among the concerned countries."),
                     p("3-	Interactive Central American map that portrays population in the different areas."),
                     br()
                     )
                   )),
                 navbarMenu(h4("Select from the dropdown list:"), 
                 tabPanel("Predicted CO2 emissions(ELSV) - Interactive Prediction App",
                          sidebarLayout(
                            sidebarPanel(
                              numericInput("months", label = "Months to Predict",
                                           value = 12, min = 12, max = 144, step = 12),
                              selectInput("Interval", label = "Confidence Interval",
                                          choices = c( "0.90", "0.95", "0.99"),
                                          selected = "0.95"),
                              checkboxInput("showgrid", label = "Show Grid", value = TRUE)
                              ),
                            mainPanel(
                              dygraphOutput("dygraph")
                              )
                            )
                          ),
                 tabPanel("Adjusted Net National Income(annual % growth) - Interactive Chart",
                          mainPanel( 
                            plotlyOutput("plot")
                            )
                          ),
                 tabPanel("Map of Central America - Interactive Map",
                          mainPanel(
                            plotlyOutput("map")
                            )
                 )))


server <- function(input, output) {
  predicted <- reactive({
    model <- HoltWinters(df)
    predict(model, n.ahead = input$months,
            prediction.interval = TRUE,
            level = as.numeric(input$interval))
  })
  output$dygraph <- renderDygraph({
    dygraph(predicted(), main = "Predicted CO2/Years") %>%
      dySeries(label = "CO2 emission" ) %>%
      dyOptions(drawGrid = input$showgrid, 
                fillGraph = TRUE, fillAlpha = 0.5,
                stepPlot = TRUE)
  })
 output$plot <- renderPlotly({
   plot_ly(incData, x = ~variable, y = ~value, color = ~Country.Code, size = ~value) %>%
  add_markers(text = ~paste(Country.Code, "<br />", value), hoverinfo = "text")%>%
                        layout(showlegend = FALSE)
 })
 output$map <- renderPlotly({
   ggplot2::map_data("world", c("Belize", "Guatemala", "El Salvador", "Honduras", "Nicaragua",
                                "Costa Rica", "Panama"))%>%
     plot_ly(x = ~long, y = ~lat)%>%
     add_trace() %>%
     add_markers(text = ~paste(country.etc, "<br />", name, "<br />", pop), hoverinfo = "text",
                 data = df_CA)%>%
     layout(showlegend = FALSE)%>%
     add_histogram2d()
 })
  
}


shinyApp(ui = ui, server = server)





