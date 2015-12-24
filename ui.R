dados<-read.csv("Data_Extract_From_World_Development_Indicators_Data.csv", na.strings=c(".."))
names(dados)[1]="Name"
lista = as.character(dados$Name[1:496])


shinyUI(pageWithSidebar(
headerPanel("Forecast - GDP per capita (current US$)"),
sidebarPanel(
selectInput("id2", label = "Select a country/region",
#choices = c("Brazil", "United States","Germany"))
choices = lista),

selectInput("id3", label = "Select a second country/region. Optional (for comparison)",
choices = c("-",lista)),
h6("Note: some countries have insufficient data to make a prediction.")

),


mainPanel(
h4('Your selection:'),
textOutput("country1"),
tags$head(tags$style("#country1{color: blue;
                                 font-size: 20px;
                               }"
                         )
              ),

h4('Comparing with:'),
textOutput("country2"),
tags$head(tags$style("#country2{color: red;
                                 font-size: 20px;
                               }"
                         )
              ),
h4('Historical Data from 1960 to 2014 (blue or red) obtained from World DataBank - World Development Indicators'),
h4('http://databank.worldbank.org/data/reports.aspx?source=world-development-indicators'),
h4('Prediction of the period 2015-2020 (grey) and projection and growth (percentage change from previous year) related to 2015.'),
#textOutput("gdp2014c1"),
plotOutput('newPlot')

)
))