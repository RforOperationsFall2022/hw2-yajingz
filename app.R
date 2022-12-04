# Load packages and data ----------------------------------------------

library(shiny)
library(shinydashboard)
library(data.table)
library(dplyr)
library(stringr)
library(plotly)

deaths_cause <- read.csv("Leading_Causes_of_Death_US.csv")
deaths_cause <- subset(deaths_cause, State != "United States")
deaths_cause$Deaths <- as.numeric(gsub(",","",deaths_cause$Deaths))
deaths_cause$Age.adjusted.Death.Rate <- as.numeric(gsub(",","",deaths_cause$Age.adjusted.Death.Rate))


# Avoid plotly issues ----------------------------------------------
pdf(NULL)

# Application header & title ----------------------------------------------
header <- dashboardHeader(title = "Death Causes",
                          
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
                selected = c("Alzheimer's disease", "Cancer")),
    
    # States Selection ----------------------------------------------
    selectInput("stateSelect",
                "States:",
                choices = sort(unique(deaths_cause$State)),
                multiple = TRUE,
                selectize = TRUE,
                selected = c("California", "Alabama")),
    
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
            valueBoxOutput("box1"),
            valueBoxOutput("box2"),
            valueBoxOutput("box3")
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
    
  NotSelectedCauseInput <- reactive({
    req(input$deathSelect)
    req(input$stateSelect)
    req(input$yearSelect)
    filter(deaths_cause,
            State %in% input$stateSelect & Year %in% input$yearSelect)
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
    ggplot(data=dat, aes(x = Year, y = Deaths, group = 1)) +
      geom_line()+
      geom_point(data = SelectedYearInput()) +
      xlab("Years")+
      ylab("Deaths Number")
  })
  
  # Data table ----------------------------------------------
  output$table <- DT::renderDataTable({
    subset(SelectedYearInput(), select = c(Year, Cause.Name, State, Deaths, Age.adjusted.Death.Rate))
  })
  
  # Mass mean info box ----------------------------------------------
  output$box1 <- renderInfoBox({
    infoBox(title = tags$p("What is AADR?", style = "font-size: 120%; font-weight: bold;"),
             value = tags$p("Age-adjusted Death Rate is a death rate that controls for differences in population age distributions.", 
                            style = "font-size: 75%; font-weight: 500;"),
            color = "red", icon = icon("list-alt"))
  })

  output$box2 <- renderValueBox({
    dat <- NotSelectedCauseInput()
    num <- round(mean(dat$Age.adjusted.Death.Rate[dat$Cause.Name=="All causes"], na.rm = TRUE),1)
    valueBox("Avg AADR For Total Causes In The Year And States", value = num, color = "purple")
  })

  output$box3 <- renderValueBox({
    dat <- SelectedYearInput()
    num <- round(mean(dat$Age.adjusted.Death.Rate, na.rm = TRUE),1)
    valueBox("Avg AADR For Selected Causes In The Year And States", value = num, color = "maroon")
  })
}

# Run the application ----------------------------------------------
shinyApp(ui = ui, server = server)