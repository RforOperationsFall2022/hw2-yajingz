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

# Set color-blind friendly Pal
safe_pal <- c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", 
              "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888")

# Avoid plotly issues ----------------------------------------------
pdf(NULL)

# Application header & title ----------------------------------------------
header <- dashboardHeader(title = "US Death Causes",
                          
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
    
    # Inputs: select variables to plot -----------------------------------------
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
    
    # Year Selection (not multiple) --------------------------------------------
    selectInput("yearSelect",
                "Years:",
                choices = sort(unique(deaths_cause$Year)),
                multiple = FALSE,
                selectize = TRUE,
                selected = c("2017")),
    
    # Download button -----------------------------------------------
    downloadButton(
      outputId = "download", 
      label = "Download Data Table",
      style="color: #fff; background-color: green; border-color: Black;"
    )
  )
)

# Dashboard body ----------------------------------------------
body <- dashboardBody(tabItems(
  
  # Plot page ----------------------------------------------
  tabItem("plot",
          
          # Input and Value Boxes ----------------------------------------------
          fluidRow(
            infoBoxOutput("box1"),
            valueBoxOutput("box2"),
            valueBoxOutput("box3"),
            valueBoxOutput("box4")
          ),
          
          # Plot ----------------------------------------------
          fluidRow(
            tabBox(title = "Plot",
                   width = 12,
                   tabPanel("Deaths by Cause", plotlyOutput("plot_cause")),
                   tabPanel("Deaths by State", plotlyOutput("plot_state")),
                   tabPanel("Year Trend for Selected Causes and States", plotlyOutput("plot_year"))
          )
  )),
  
  # Data Table Page ----------------------------------------------
  tabItem("table",
          fluidPage(
            box(title = "Selected Deaths Data Table", DT::dataTableOutput("table"), width = 12))
  )
))

ui <- dashboardPage(header, sidebar, body)

# Define server function required to create plots and boxes -----
server <- function(input, output) {
  
  # Reactive data function -------------------------------------------
  # Input with cause.Name, State, and Year all selected ----------
  SelectedYearInput <- reactive({
    req(input$deathSelect)
    req(input$stateSelect)
    req(input$yearSelect)
    filter(deaths_cause,
           Cause.Name %in% input$deathSelect & 
           State %in% input$stateSelect & Year %in% input$yearSelect)
  })

  # Input with cause.Name and State selected but not Year ----------    
  NotSelectedYearInput <- reactive({
    req(input$deathSelect)
    req(input$stateSelect)
    req(input$yearSelect)
    filter(deaths_cause,
           Cause.Name %in% input$deathSelect & State %in% input$stateSelect)
  })

  # Input with Year and State selected but not Cause.Name ----------          
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
    ggplot(data = dat, aes(x = Cause.Name, y = Deaths, fill = State)) + 
      geom_bar(stat = "identity") +
      scale_fill_manual(values = safe_pal) +
      xlab("Deaths Causes")+
      ylab("Deaths Number")
  })
  
  # Plot comparing dif states selected deaths ----------------------------------
  output$plot_state <- renderPlotly({
    dat <- SelectedYearInput()
    ggplot(data = dat, aes(x = State, y = Deaths, fill = Cause.Name)) + 
      geom_bar(stat = "identity") +
      scale_fill_manual(values = safe_pal) +
      xlab("States")+
      ylab("Deaths Number")
  })
  
  # Plot showing year trend for selected deaths --------------------------------
  output$plot_year <- renderPlotly({
    NotSelectedYearInput() %>%
      group_by(Year) %>%
      mutate(sum.Deaths = sum(Deaths)) %>%
    ggplot(aes(x = Year, y = sum.Deaths)) +
      geom_line() +
      geom_point(data = SelectedYearInput() %>% mutate(sum.Deaths = sum(Deaths)), color = "dark green") +
      xlab("Years")+
      ylab("Deaths Number") +
      scale_x_continuous(breaks=seq(1999,2017,by=1))
  })
  
  # Data table ----------------------------------------------
  output$table <- DT::renderDataTable({
    subset(SelectedYearInput(), select = c(Year, Cause.Name, State, Deaths, Age.adjusted.Death.Rate))
  })

  # Download data table --------------------------------------------------------
  output$download <- downloadHandler(
    filename = function() {
      paste("US Selected Death Causes Data (", input$yearSelect, ").csv", 
            sep = "")
    },
    content = function(file) {
      write.csv(SelectedYearInput(), file, row.names = FALSE)
    })
  
  # Info box and Value box for AADR ----------------------------------------------
  output$box1 <- renderInfoBox({
    infoBox(title = tags$p("What is AADR?", style = "font-size: 150%; font-weight: bold;"),
             value = tags$p("Age-adjusted Death Rate, or AADR, is a death rate that controls for effects of differences in population age distributions.", 
                            style = "font-size: 110%; font-weight: 500;"),
            color = "green", icon = icon("list-alt"))
  })

  output$box2 <- renderValueBox({
    dat <- NotSelectedCauseInput()
    num <- round(mean(dat$Age.adjusted.Death.Rate[dat$Cause.Name=="All causes"], na.rm = TRUE),1)
    valueBox("Avg AADR For Total Causes In The Selected Year And States", value = num, color = "purple")
  })

  output$box3 <- renderValueBox({
    dat <- SelectedYearInput()
    num <- round(mean(dat$Age.adjusted.Death.Rate, na.rm = TRUE),1)
    valueBox("Avg AADR For Selected Causes In The Selected Year And States", value = num, color = "maroon")
  })
  
  output$box4 <- renderValueBox({
    dat <- SelectedYearInput()
    num <- sum(dat$Deaths, na.rm = TRUE)
    valueBox("Death Number For Selected Causes In The Selected Year And States", value = num, color = "black")
  })
}

# Run the application ----------------------------------------------
shinyApp(ui = ui, server = server)