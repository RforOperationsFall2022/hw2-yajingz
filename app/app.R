# Load packages and data ----------------------------------------------

library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)

deaths_cause <- read.csv("Leading_Causes_of_Death_US.csv")

# Avoid plotly issues ----------------------------------------------
pdf(NULL)

# Application header & title ----------------------------------------------
header <- dashboardHeader(title = "Death Causes in the US Dashboard",
                          
                          # Drop down menu ------------------------------
                          dropdownMenu(type = "notifications",
                                       notificationItem(text = "Data updated")
                          ),
                          dropdownMenu(type = "tasks", badgeStatus = "success",
                                       taskItem(value = 110, color = "green",
                                                "HW2")
                          )
)

# Dashboard Sidebar ----------------------------------------------
sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    
    # Menu Items ----------------------------------------------
    menuItem("Plot", icon = icon("bar-chart"), tabName = "plot"),
    menuItem("Table", icon = icon("table"), tabName = "data table"),
    
    # Inputs: select variables to plot ----------------------------------------------
    # Death Causes Selection ----------------------------------------------
    selectInput("deathSelect",
                "Death Causes:",
                choices = sort(unique(deaths_cause$Cause.Name)),
                multiple = TRUE,
                selectize = TRUE,
                selected = c("Alzheimer's disease", "Cancer")),
    
    # States Selection ----------------------------------------------
    selectInput("stateSelect",
                "States:",
                choices = sort(unique(deaths_cause$State)),
                multiple = TRUE,
                selectize = TRUE,
                selected = c("Alabama", "Alaska")),
    
    # Year Selection ----------------------------------------------
    sliderInput("yearSelect",
                "Years:",
                min = min(deaths_cause$Year, na.rm = T),
                max = max(deaths_cause$Year, na.rm = T),
                value = c(min(deaths_cause$Year, na.rm = T), max(deaths_cause$Year, na.rm = T)),
                step = 1)
  )
)

# Dashboard body ----------------------------------------------
body <- dashboardBody(tabItems(
  
  # Plot page ----------------------------------------------
  tabItem("plot",
          
          # Input and Value Boxes ----------------------------------------------
          fluidRow(
            infoBoxOutput("info1"),
            infoBoxOutput("info2"),
            infoBoxOutput("info3"),
          ),
          
          # Plot ----------------------------------------------
          fluidRow(
            tabBox(title = "Plot",
                   width = 12,
                   tabPanel("Deaths by Cause", plotlyOutput("plot_cause")),
                   tabPanel("Deaths by State", plotlyOutput("plot_state")),
                   tabPanel("Deaths by Year", plotlyOutput("plot_year"))
          )
  ),
  
  # Data Table Page ----------------------------------------------
  tabItem("table",
          fluidPage(
            box(title = "Selected Deaths Data", DT::dataTableOutput("table"), width = 12))
  )
)))

ui <- dashboardPage(header, sidebar, body)

# Run the application ----------------------------------------------
shinyApp(ui = ui, server = server)