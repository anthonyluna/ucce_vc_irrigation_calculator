###############################################################################
# Irrigation Calculator Demo
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
library(shinyBS)
library(cimir)
library(tidyverse)
library(DT)


# Here is where you put your CIMIS API Key. Key kept off remote repo.
key <- dget("key.R")
set_key(key = key)

# Funny that the server time matters for this implementation.
Sys.setenv(TZ = "America/Los_Angeles")

# Data for user selection

if (!exists("kc_data")) {
  kc_data <- data.frame(
    crop = c(rep("Strawberry", 3), rep("Celery", 3)),
    stage = rep(c("Early Season", "Mid Season", "Late Season"), 2),
    kc = c(1, 1.2, 1.3, 1, 1.1, 1.4)
  )
}
# Data is pulled on user loading. This could be done once per day
# but haven't figured out how to set up a scheduler in shinyapp.io yet.
# Pulls all data from the last 180 days since this is as far back as
# would be relevant
tryCatch(
  {
    df_198 <- cimis_data(
      targets = "198",
      start.date = Sys.Date() - 180,
      end.date = Sys.Date() - 1,
      items = "day-asce-eto,day-precip"
    )
    df_152 <- cimis_data(
      targets = "152",
      start.date = Sys.Date() - 180,
      end.date = Sys.Date() - 1,
      items = "day-asce-eto,day-precip"
    )
    df_217 <- cimis_data(
      targets = "217",
      start.date = Sys.Date() - 180,
      end.date = Sys.Date() - 1,
      items = "day-asce-eto,day-precip"
    )

    # Creates data frame from called CIMIS data
    df_all <- bind_rows(df_198, df_152, df_217)
  },
  error = function(cond) {
    df_all <<- NULL
  }
)

# Where the layout of the page exists
ui <- fillPage(
  padding = 10,
  includeCSS("styling.css"),

  # Application title
  titlePanel("Irrigation Calculator"),


  fluidRow(
    column(
      3,

      textInput("tgittgit",
        label = list(
          tipify(
            icon("info-circle"), "This is used for determining the evapotranspiration (ETo) with Spatial CIMIS."
          ),
          "Zip Code"
        ),
        placeholder = "99999",
        width = "100%"
      ),

      dateInput("last_ir",
        label = list(
          tipify(
            icon("info-circle"), "Enter the date when the field was last irrigated."
          ),
          "Last Irrigation"
        ),
        min = Sys.Date() - 180,
        max = Sys.Date() - 1,
        width = "100%",
        autoclose = TRUE
      ),


      selectInput("crop",
        label = list(
          tipify(
            icon("info-circle"),
            "This is used for choosing the crop coefficient (Kc)"
          ),
          "Crop Type"
        ),
        choices = unique(kc_data$crop),
        width = "100%"
      ),

      selectInput("stage",
        label = list(
          tipify(
            icon("info-circle"),
            "This is used for adjusting the crop coefficient (Kc) according to the crop size"
          ),
          "Crop Stage"
        ),
        choices = unique(kc_data$stage),
        width = "100%"
      ),

      numericInput("rate",
        label = list(
          tipify(
            icon("info-circle"),
            "This is used to convert the inches of irrigation to hours of irrigation."
          ),
          "Irrigation Rate (inches per hour)"
        ),
        value = NULL,
        min = 0,
        max = 5,
        width = "100%"
      ),

      h5(
        tipify(
          icon("info-circle"),
          "This is the recommended amount of irrigation your crop needs in inches based on the crop information provided and CIMIS weather data."
        ),
        strong("Total Irrigation Recommended: ")
      ),

      h4(textOutput("reccomend")),

      h5(
        tipify(
          icon("info-circle"),
          "This is the recommended amount of irrigation your crop needs in hours based on the crop information provided, your irrigation system flow rate and CIMIS weather data."
        ), strong("Total Irrigation Time: ")
      ),

      h4(textOutput("irr_time")),

      actionButton("calculate", "Click Here to Calculate",width = "100%")
    ),


    column(
      4,
      dataTableOutput("table")
    )
  )
)

# The server of information
server <- function(input, output) {
  output$table <- renderDataTable({
    isolate(
      DT::datatable(data.frame(
        "Date" = rep("", 10),
        "ETo" = rep("", 10),
        "Precipitation" = rep("", 10),
        "ETc" = rep("", 10),
        "Reccomended Irrigation (in)" = rep("", 10)
      ),
      options = list(
        lengthMenu = list(c(5, 10), c("5", "10")),
        pageLength = 10,
        searching = FALSE,
        width = "100%",
        height = "500px"
      ),
      rownames = FALSE,
      colnames = c("Date", "ETo", "Precipitation", "ETc", "Reccomended Irrigation (in)")
      )
    )
  })

  output_table <- reactive({

    # where the validations live
    req(input$last_ir)
    req(input$crop)
    req(input$stage)
    validate(need(df_all,
      message = paste(
        "There was an issue connecting to,",
        "the CIMIS Weather Data. Please reload",
        "the page, and if the error persists,",
        "come back a little later."
      )
    ))

    validate(
      need(input$tgittgit, message = FALSE),
      need(nchar(input$tgittgit) == 5 & !is.na(as.numeric(input$tgittgit)),
        message = "Zipcode must be 5 digits"
      )
    )

    # station to grab from based on zipcode. Mapping created separating
    # regions "as the crow flies" distance, then looking at where
    # the majority of ag land is for zipcodes straddling regions. Rough
    # estimate but required for now due to CIMIS limitations.

    station_198 <- c(
      "93066",
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
      "93030"
    )

    station_152 <- c(
      "93041",
      "93044",
      "93033",
      "93010",
      "93011",
      "93012",
      "91320",
      "91319",
      "91361",
      "90265",
      "93042"
    )

    station_217 <- c(
      "91384",
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
      "91302"
    )

    # logic to chose data relevant to user

    station_data <- ifelse(
      input$tgittgit %in% station_152, "152",
      ifelse(input$tgittgit %in% station_198, "198",
        ifelse(input$tgittgit %in% station_217, "217", "")
      )
    )

    # more validation logic
    validate(
      need(
        station_data,
        "Sorry, the zipcode you entered is not covered at this time."
      )
    )

    # calculations based on user inputs and CIMIS Data. This whole table
    # is displayed and could easily be made into an interactive graph.

    table <- df_all %>%
      pivot_wider(
        id_cols = c("Date", "Station"),
        names_from = "Item",
        values_from = "Value"
      ) %>%
      mutate(kc = kc_data$kc[kc_data$crop == input$crop & kc_data$stage == input$stage]) %>%
      mutate(ETc = kc * DayAsceEto) %>%
      mutate(irr = ETc - DayPrecip) %>%
      filter(Date >= input$last_ir) %>%
      filter(Station == station_data) %>%
      select(-kc, -Station)

    return(table)
  })

  observeEvent(input$calculate, {
    output$table <- renderDataTable({
      validate(
        need(df_all,
          message = paste(
            "There was an issue connecting to ",
            "the CIMIS Weather Data. Please reload ",
            "the page, and if the error persists, ",
            "come back a little later."
          )
        )
      )
      DT::datatable(output_table(),
        options = list(
          lengthMenu = list(
            c(5, 10),
            c("5", "10")
          ),
          pageLength = 10,
          searching = FALSE
        ),
        rownames = FALSE,
        colnames = c(
          "Date",
          "ETo",
          "Precipitation",
          "ETc",
          "Reccomended Irrigation (in)"
        )
      )
    })


    # Irrigation recommendation output in inches
    output$reccomend <- renderText({
      validate(need(df_all, message = ""))
      paste(round(sum(output_table()$irr),digits = 1), "inches")
    })

    # Irrigation time based on irrigation flow rate
    output$irr_time <- renderText({
      validate(need(df_all, message = ""))
      validate(need(input$rate, "Enter your irrigation rate to see the recommended hours of irrigation"))
      req(input$rate > 0)
      paste(
        round(sum(output_table()$irr) / input$rate, digits = 1),
        " hours"
      )
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
