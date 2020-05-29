## ---------------------------
##
## Script name: ui.R
##
## Purpose of script:  Specifies user interface for coronaRisk app
##
## Author: Domingo Velazquez
##
## Date Created: 2020-03-20
## Date Updated: 2020-04-11
##
## ---------------------------
##
## load up the packages we will need 

library(shiny)
library(plotly)
library(shinyWidgets)
library(repmis)

## ---------------------------

## load up our functions into memory
## source files

source("functions.R") # makes functions available to the instance.
source("getDataLocal.R") #makes data available to the instance.

## ---------------------------

options(scipen = 8)

# Define UI

shinyUI(fluidPage( theme = "cbx.css",
  # Application title
  titlePanel(title=div(img(height = 80, width = 160,src="CBX.png"), style="text-align: center;")),
  navbarPage(paste("COVID - 19 Analysis","(",format(dates[length(dates)], "%d %B"), ")"),
##### n-day forecast #####         p(format(dates[length(dates)], "%d %b"))    
      tabPanel("10-days forecast",
             # Sidebar 
             sidebarLayout(
               sidebarPanel(
                 #titlePanel("Summary"),
                 #hr(),
                 selectizeInput(inputId = "global_or_country",
                                label = "Select global or country:",
                                choices = c("Global", "Mexico", "US", "China"), 
                                selected = "Global"),
                 selectizeInput(inputId = "countryFinder",
                                label = "Country/Region:",
                                choices = ddReg),
                 hr(),
                 h5("Confirmed Cases:"),
                 tableOutput(outputId = "confirmedStats"),
                 hr(),
                 h5("Death Cases:"),
                 tableOutput(outputId = "deathStats"),
                 hr(),
                 h5("Active Cases:"),
                 tableOutput(outputId = "activeStats"),
                 titlePanel("Forecast Metrics:"),
                 textOutput(outputId = "forecastMetrics"),
                 radioButtons("modelType", "", choiceNames = list('Time-varying growth','Constant growth'), choiceValues = list('True', 'False')),
                 sliderInput(inputId = "fitWinSlider", min = 1, max = 10, value = 7, ticks = FALSE, label = "Fit window:", post = " days"),
                 hr(),
                 sliderInput(inputId = "fitWinSliderout", min = 1, max = 15, value = 10, ticks = FALSE, label = "Forecast window:", post = " days"),
                 hr(),
                 titlePanel("Detection"),
                 h5("Cases succesfully detected:"),
                 textOutput(outputId = "detRate"),
                 h5("Possible number of active cases now given imperfect detection:"),
                 tableOutput(outputId = "tablePredTrue"),
                 sliderInput(inputId = "fatalityRatioSlider", min=1, max=15, value=10, step=0.1, label = "Case fatality ratio:", post="%")
                 #hr(),
                 #titlePanel("Notes"),
                 #h5("This estimation is a rough approximation, so it is necessary to take them with care. Nevertheless, the low detection indicates that there could be many more infected cases and therefore deaths than those currently reported."),
               ),
               
               # Show a plot of the generated distribution
               mainPanel(
                 fluidRow(
                   splitLayout(cellWidths = c("50%", "50%"),  plotlyOutput("confirmedPlot"), plotlyOutput("newCasesc"))
                 ),
                 fluidRow(
                   splitLayout(cellWidths = c("50%", "50%"),  plotlyOutput("deathPlot"), plotlyOutput("newCasesd"))
                 ),
                 fluidRow(
                   plotlyOutput("activePlot")
                 )
               )
             )
      ),
##### Growth Rate ##### 
      tabPanel("Growth rate",
               # Sidebar
               sidebarLayout(
                 sidebarPanel(
                   titlePanel("Summary"),
                   hr(),
                   selectInput(inputId = "countryGrowthRate",
                               label = "Country/Region:",
                               choices = ddReg, 
                               multiple = TRUE,
                               selectize = TRUE,
                               selected = c("China", "Mexico", "US")),
                   hr(),
                   h5("Active cases growth rate (last 10 days)."),
                   h5("Progress in control is indicated by a steady decrease in the growth rate over time and by staying in negative territory."),
                   hr(),
                   titlePanel("Notes"),
                   h5("Days with low or zero growth, followed by large spikes, indicate flaws in reporting information. Countries miss a reporting day (or days) and then add cases the next day.")
                 ),
                 mainPanel(
                   fluidRow(plotlyOutput("growthRatec"), plotlyOutput("growthRatea"),  plotlyOutput("growthRated")
                   )
                 )
               )
      ),
##### CFI ##### 
      tabPanel("Curve-flattenig Index",
               # Sidebar
               sidebarLayout(
                 sidebarPanel(
                   titlePanel("Summary"),
                   hr(),
                   selectInput(inputId = "countryCFI",
                               label = "Country/Region:",
                               choices = ddReg, 
                               multiple = TRUE,
                               selectize = TRUE,
                               selected = c("China", "Mexico", "US")),
                   h5("This is a measure of how well a country is flattening the pandemic curve at any time. Positive values indicate success in flattening.")
                 ),
                 mainPanel(
                   plotlyOutput("cfi")
                 )
               )
      )
      
  )
))
