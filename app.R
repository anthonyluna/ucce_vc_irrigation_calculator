###############################################################################
# Ventura County Irrigation Calculator Demo 
# Author: Anthony Luna
# Description: A simple irrigation calculator to help farmers effectively
# manage their irrigation. Currently, Ventura County is the main coverage area. 
# Weather data is pulled from the CIMIS API using the cimir package.
#
# Links
# CIMIS API: https://et.water.ca.gov/
# cimir: https://github.com/mkoohafkan/cimir
###############################################################################

library(shiny)
library(cimir)
library(tidyverse)


# Here is where you put your CIMIS API Key. Key kept off remote repo.
key <- dget("key.R")
set_key(key=key)


# Data for user selection

if(!exists("kc_data")){
    kc_data <- data.frame(
        crop=c(rep("Strawberry",3),rep("Celery",3)),
        stage=rep(c("Early Season","Mid Season", "Late Season"),2),
        kc = c(1,1.2,1.3,1,1.1,1.4)
    )
}

# Where the layout of the page exists
ui <- fluidPage(

    # Application title
    titlePanel("Ventura County Irrigation Calculator"),

     
    sidebarLayout(
        sidebarPanel(
            h3(textOutput("today")),
            textInput("zipcode", 
                      label = "Zip Code",
                      placeholder = "99999",
                      width = "100%"),
            dateInput("last_ir",
                      label = "Last Irrigation",
                      min = Sys.Date()-180,
                      max = Sys.Date()-1,
                      width = "100%",
                      autoclose=TRUE),
            selectInput("crop",
                           label = "Crop Type",
                           choices = unique(kc_data$crop),
                           width = "100%"),
            selectInput("stage",
                           label = "Crop Stage",
                           choices = unique(kc_data$stage),
                           width = "100%"),
            numericInput("rate",
                         label = "Irrigation Rate (in/h)",
                         value=0.5,
                         min=0,
                         max=5,
                         width = "100%")
            
        ),

        
        mainPanel(
            h3(textOutput("reccomend")),
            h3(textOutput("irr_time")),
            dataTableOutput("table")
        )
    )
)

# The server of information
server <- function(input, output) {
    

    
    output$today <- renderText({paste("Today is:",as.character(Sys.Date()))})
    
    # Data is pulled on user loading. This could be done once per day 
    # but haven't figured out how to set up a scheduler in shinyapp.io yet. 
    # Pulls all data from the last 180 days since this is as far back as 
    # would be relevant
    
    df_198 <- cimis_data(targets = "198",
                         start.date = Sys.Date()-180,
                         end.date = Sys.Date()-1,
                         items = "day-asce-eto,day-precip")
    df_152 <- cimis_data(targets = "152",
                         start.date = Sys.Date()-180,
                         end.date = Sys.Date()-1,
                         items = "day-asce-eto,day-precip")
    df_217 <- cimis_data(targets = "217",
                         start.date = Sys.Date()-180,
                         end.date = Sys.Date()-1,
                         items = "day-asce-eto,day-precip") 
    
    # Creates data frame from called CIMIS data
    df_all <- bind_rows(df_198,df_152,df_217)
    
    output_table <- reactive({
        
        # where the validations live
        req(input$last_ir)
        req(input$crop)
        req(input$stage)
        
        validate(
            need(input$zipcode,message = FALSE),
            need(nchar(input$zipcode)==5&!is.na(as.numeric(input$zipcode)),
                 message = "Zipcode must be 5 digits")
        )
        
        # station to grab from based on zipcode. Mapping created separating
        # regions "as the crow flies" distance, then looking at where 
        # the majority of ag land is for zipcodes straddling regions. Rough
        # estimate but required for now due to CIMIS limitations.
        
        station_198 <- c("93066", 
                         "93023", 
                         "93013", 
                         "93022", 
                         "93002", 
                         "93001", 
                         "93006", 
                         "93003", 
                         "93009", 
                         "93004", 
                         "93005", 
                         "93036", 
                         "93031", 
                         "93035", 
                         "93043", 
                         "93034", 
                         "93032", 
                         "93024", 
                         "93060", 
                         "93061",
                         "93030")
        
        station_152 <- c("93041", 
                         "93044", 
                         "93033", 
                         "93010", 
                         "93011", 
                         "93012", 
                         "91320", 
                         "91319", 
                         "91361", 
                         "90265", 
                         "93042")
        
        station_217 <- c("91384", 
                         "93040", 
                         "93015", 
                         "93016", 
                         "93021", 
                         "93020", 
                         "93065", 
                         "93099", 
                         "93062", 
                         "91360", 
                         "91362", 
                         "91358", 
                         "91359", 
                         "91377", 
                         "93063", 
                         "93094", 
                         "93064", 
                         "91304", 
                         "91307", 
                         "91302") 
        
        # logic to chose data relevant to user
        
        station_data <- ifelse(
            input$zipcode %in% station_152,"152",
                ifelse(input$zipcode %in% station_198,"198", 
                       ifelse(input$zipcode %in% station_217,"217","")
                       )
            ) 
        
        # more validation logic
        validate(
            need( station_data,
                  "Sorry, the zipcode you entered is not covered at this time.")
        )
        
        # calculations based on user inputs and CIMIS Data. This whole table
        # is displayed and could easily be made into an interactive graph.
        
        table <- df_all %>% 
            pivot_wider(id_cols=c("Date","Station"),
                        names_from = "Item",
                        values_from="Value") %>% 
            mutate(kc=kc_data$kc[kc_data$crop==input$crop&kc_data$stage==input$stage]) %>% 
            mutate(ETc=kc*DayAsceEto) %>%
            mutate(irr=ETc-DayPrecip) %>% 
            filter(Date>=input$last_ir) %>% 
            filter(Station==station_data)
        
        return(table)
    })
    
    output$table <- renderDataTable({
        return(output_table())    
        })
    
    # Irrigation reccomendation output in inches
    output$reccomend <- renderText({
        paste("Total Irrigation Reccomended: ",sum(output_table()$irr))
        })
    
    # Irrigation time based on irrigation flow rate
    output$irr_time <- renderText({
        paste("Total Irrigation Time (hr): ",
              round(sum(output_table()$irr)/input$rate,digits=2),
              " hours")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
