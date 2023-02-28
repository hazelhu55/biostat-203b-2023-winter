library(shiny)
library(dplyr)
library(data.table)
library(lubridate)
library(R.utils)
library(tidyverse)
library(readr)
library(ggplot2)
library(bigrquery)
library(eply)
library(jsonlite)

satoken <- "~/mimic/biostat-203b-2023winter-3fdc2392ac39.json"
# BigQuery authentication using service account
bq_auth(
  path = satoken,
  email = "mimiciv-bigquery@biostat-203b-2023winter.iam.gserviceaccount.com",
  scopes = c("https://www.googleapis.com/auth/bigquery",
             "https://www.googleapis.com/auth/cloud-platform")
)

#remember to set windows
setwd("~/BIOSTAT/2022-2023/BIOSTAT203B/2023-hw/hw3/mimiciv_shiny")
icu_cohort<-read_rds("icu_cohort.rds")

x0 <- list(
  "Thirty Day Mortality"="thirty_day_mort",
  "Gender"="gender",
  "Insurance"="insurance",
  "Language"="language",
  "Marital Status"="marital_status",
  "Ethnicity"="ethnicity",
  "Bicarbonate"="bicarbonate",
  "Creatinine"="creatinine",
  'Potassium'='potassium',
  'Sodium'='sodium',
  'Chloride'='chloride',
  'Hematocrit'='hematocrit',
  'Number of White Bood Cell'="n_wb_cell",
  'Glucose'='glucose',
  'Heart Rate'='heart_rate',
  "Mean Non-invasive Blood Pressure"='ni_blood_pressure',
  "Non-invasive Blood Pressure"='sni_blood_pressure',
  "Body Temperature in Fahrenheit"='body_temp',
  "Respiratory Rate"='respiratory_rate')

#Filter some outliers for the numeric data to make plots readable.
df_lab <- icu_cohort %>%
  select(c("bicarbonate", "creatinine", 'sodium', 'chloride',
            'hematocrit', "potassium", "n_wb_cell", "thirty_day_mort"))   %>%
  pivot_longer(cols = bicarbonate : n_wb_cell )  %>%
  group_by(name) %>%
  mutate(percentr = percent_rank(value)) %>%
  filter(percentr < 0.975 & percentr > 0.025) %>%
  select(-percentr) %>%
  ungroup()

df_vital <- icu_cohort %>%
  select(c("glucose", "heart_rate", 'ni_blood_pressure', 'body_temp',
           'respiratory_rate', "thirty_day_mort")) %>%
  pivot_longer(cols = glucose : respiratory_rate) %>%
  group_by(name) %>%
  mutate(percentr = percent_rank(value)) %>%
  filter(percentr < 0.975 & percentr > 0.025) %>%
  select(-percentr) %>%
  ungroup()


# Define UI for application
ui <- fluidPage(
  
  # Application title,
  
  navbarPage(
    title = "ICU Cohort Data",
    tabPanel("Plot",
      sidebarLayout(
        sidebarPanel(
          #select a variable to plot or show
          selectInput(inputId="variable",
                      label=strong("Variables"),
                      choices=x0),
          helpText("ICU Cohort Data VS. Thirty Day Mortality Rate")
          ),
    
        # Show a plot of the generated distribution
        mainPanel( shinycssloaders::withSpinner(plotOutput("Plot")))
      )
    ),
    
    tabPanel("Summary Table",
      sidebarLayout(
        sidebarPanel(
          #select a variable to show the summary plot
          selectInput(inputId="measure",
                      label=strong("Choose a Table:"),
                      choices=c("Demographic Factors",
                                "Lab Measurements",
                                "Vital Measurements"))),
          mainPanel(shinycssloaders::withSpinner(verbatimTextOutput("Summary")))
      )
    )
  )
)



# Define server logic required to draw plots and tables
server <- function(input, output) {
  
  output$Plot <- renderPlot({
     
    x_target <- input$variable
      #Demographical data
      if(x_target %in% x0[2:6]){
        value <- unlist(unname(x_target))
        as_tibble(select(icu_cohort, 'thirty_day_mort', value)) %>%
          group_by(thirty_day_mort, !!sym(value)) %>%
          summarise(count = n()) %>%
          ungroup %>%
          ggplot(mapping = aes(x = thirty_day_mort,y = count)) +
          geom_col(aes(fill = !!sym(value))) +
          xlab("Thirty Day Mortality") + 
          ylab("Count")
     }
     
     #lab measurements
     else if(x_target %in% x0[7:13]){
       df_lab %>%
       filter(name == x_target) %>%
       ggplot() +
         geom_boxplot(mapping = aes(x = thirty_day_mort, 
                                    y = value))
     }
     #vital measurements
     else if(x_target %in% x0[14:19]){
       df_vital %>%
         filter(name == x_target) %>%
         ggplot() +
         geom_boxplot(mapping = aes(x = thirty_day_mort, 
                                    y = value))
     }
     else{
       select(icu_cohort, 'thirty_day_mort') %>%
         ggplot() +
         geom_bar(mapping = aes(x=thirty_day_mort))
     }
  })
   
   output$Summary <- renderPrint({
     
     #Print table for demographic Factors, run very slowly >-<
     icu_cohort1 <- as_tibble(icu_cohort)
     if(input$measure == "Demographic Factors"){
       categ<-c('thirty_day_mort', 'gender', 'language', 'insurance',
                'marital_status', 'ethnicity')
        for(i in 2:6){
          a<-table(icu_cohort1[[categ[1]]], icu_cohort1[[categ[i]]])
          print(a)
        }
     }
     
     #Print table for lab measurements, run very slowly >-<
     else if(input$measure == "Lab Measurements"){
       as_tibble(df_lab) %>%
         group_by(thirty_day_mort,name) %>%
         summarise(
           mean=mean(value, na.rm=TRUE),
           min=min(value, na.rm = TRUE),
           max=max(value, na.rm = TRUE),
           median=median(value, na.rm = TRUE),
           variance=var(value)
         ) %>%
         print(n = 14, width = Inf)
     }
     #Print table for vital measurements, run very slowly >-<
     else{
       as_tibble(df_vital) %>%
         group_by(thirty_day_mort, name) %>%
         summarise(
           mean=mean(value, na.rm=TRUE),
           min=min(value, na.rm=TRUE),
           max=max(value, na.rm=TRUE),
           median=median(value, na.rm=TRUE),
           variance=var(value)
         ) %>%
         print(n = 10, width = Inf)
     }
   })
}



# Run the application
shinyApp(ui = ui, server = server)
