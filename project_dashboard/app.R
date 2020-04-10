#Contents:
#    1. Package Imports
#    2. Loading of data
#    3. Visualisation Functions
#    4. Application UI

#Package

library(shiny)
library(shinydashboard)
library(plotly)
library(ggplot2)
library(viridis)
library(GGally)


# Loading of Data:
residential_data_2019 <- read_csv("Data/Residential_Planning/respopagesextod2011to2019.csv")

ls()


#UI Section

ui <- dashboardPage(skin = "green",

    #---------------------------- dashboard header ----------------------------                                    
    header <- dashboardHeader(title = "Precision Policy And Planning",
                titleWidth = 300),

    
    #--------------------------- dashboard sidebar ----------------------------
    sidebar <-dashboardSidebar(
    sidebarMenu(
        menuItem(text = "Introduction",
                 tabName = "Introduction",
                 icon = icon("home")
                 ),
        menuItem(text = "Dashboard 1",
                 tabName = "Dashboard 1",
                 icon = icon("dice-one")
                 ),
        menuItem(text = "Dashboard 2",
                 tabName = "Dashboard 2",
                 icon = icon("dice-two")
                 )
        )
    ),
    
    #--------------------------- dashboard sidebar ----------------------------
    body <- dashboardBody()
    
)


shinyApp(ui, server)
