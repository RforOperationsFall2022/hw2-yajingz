# Load packages and data ----------------------------------------------

library(shiny)
library(shinydashboard)
library(data.table)
library(dplyr)
library(stringr)
library(plotly)

deaths_cause <- read.csv("Leading_Causes_of_Death_US.csv")

# Avoid plotly issues ----------------------------------------------
pdf(NULL)

# Application header & title ----------------------------------------------
header <- dashboardHeader(title = "Death Causes in the US",
                          
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
    menuItem("plot", icon = icon("bar-chart"), tabName = "plot"),
    menuItem("table", icon = icon("table"), tabName = "table"),
    
    # Inputs: select variables to plot ----------------------------------------------
    # Death Causes Selection ----------------------------------------------
    selectInput("deathSelect",
                "Death Causes:",
                choices = sort(unique(deaths_cause$Cause.Name)),
                multiple = TRUE,
                selectize = TRUE,
                selected = c("Alzheimer's disease")),
    
    # States Selection ----------------------------------------------
    selectInput("stateSelect",
                "States:",
                choices = sort(unique(deaths_cause$State)),
                multiple = TRUE,
                selectize = TRUE,
                selected = c("United States")),
    
    # Year Selection ----------------------------------------------
    selectInput("yearSelect",
                "Years:",
                choices = sort(unique(deaths_cause$Year)),
                multiple = FALSE,
                selectize = TRUE,
                selected = c("2017"))
  )
)

# Dashboard body ----------------------------------------------
body <- dashboardBody(tabItems(
  
  # Plot page ----------------------------------------------
  tabItem("plot",
          
          # Input and Value Boxes ----------------------------------------------
          fluidRow(
            infoBoxOutput("info1")
            # infoBoxOutput("info2"),
            # infoBoxOutput("info3")
          ),
          
          # Plot ----------------------------------------------
          fluidRow(
            tabBox(title = "Plot",
                   width = 12,
                   tabPanel("Deaths by Cause", plotlyOutput("plot_cause")),
                   tabPanel("Deaths by State", plotlyOutput("plot_state")),
                   tabPanel("Deaths by Year", plotlyOutput("plot_year"))
          )
  )),
  
  # Data Table Page ----------------------------------------------
  tabItem("table",
          fluidPage(
            box(title = "Selected Deaths Data", DT::dataTableOutput("table"), width = 12))
  )
))

ui <- dashboardPage(header, sidebar, body)

# Define server function required to create plots and boxes -----
server <- function(input, output) {
  
  # Reactive data function -------------------------------------------
  SelectedYearInput <- reactive({
    req(input$deathSelect)
    req(input$stateSelect)
    req(input$yearSelect)
    filter(deaths_cause,
           Cause.Name %in% input$deathSelect & 
           State %in% input$stateSelect & Year %in% input$yearSelect)
  })
  
  NotSelectedYearInput <- reactive({
    req(input$deathSelect)
    req(input$stateSelect)
    req(input$yearSelect)
    filter(deaths_cause,
           Cause.Name %in% input$deathSelect & State %in% input$stateSelect)
  })
    
  
  # Plot comparing dif death causes -----------------------------
  output$plot_cause <- renderPlotly({
    dat <- SelectedYearInput()
    ggplot(data = dat, aes(x = Cause.Name, y = Deaths, fill = Cause.Name)) + 
      geom_bar(stat = "identity") +
      xlab("Deaths Causes")+
      ylab("Deaths Number")
  })
  
  # Plot comparing dif states selected deaths -----------------------------------
  output$plot_state <- renderPlotly({
    dat <- SelectedYearInput()
    ggplot(data = dat, aes(x = State, y = Deaths, fill = State)) + 
      geom_bar(stat = "identity") +
      xlab("States")+
      ylab("Deaths Number")
  })
  
  # Plot comparing dif years selected deaths -----------------------------------
  output$plot_year <- renderPlotly({
    dat <- NotSelectedYearInput()
    ggplot(data=dat, aes(x=Year, y=Deaths, group = 1)) +
      geom_line()+
      geom_point(data = SelectedYearInput()) +
      xlab("Years")+
      ylab("Deaths Number")
  })
  
  # Data table ----------------------------------------------
  output$table <- DT::renderDataTable({
    subset(dcInput(), select = c(Year, Cause.Name, State, Deaths, Age.adjusted.Death.Rate))
  })
  
  # Mass mean info box ----------------------------------------------
  output$info1 <- renderInfoBox({
    dat <- dcInput()
    infoBox("Selected Death Number in Selected Year and States", nrow(distinct(dat,Deaths)))
  })
  # 
  # output$info2 <- renderInfoBox({
  #   cd <- crimedatainput()
  #   cd <- cd%>%filter(description=="HOMICIDE")
  #   infoBox("Total Death Number in Selected Year and States", nrow(distinct(cd,complaint)),color = "purple")
  # })
  # 
  # output$info3 <- renderInfoBox({
  #   cd <- crimedatainput()
  #   cd <- cd%>%filter(str_detect(description, "^ROBBERY CARJACKING"))
  #   infoBox("Total Number of Carjackings", nrow(distinct(cd,complaint)),color = "red")
  # })
}

# Run the application ----------------------------------------------
shinyApp(ui = ui, server = server)