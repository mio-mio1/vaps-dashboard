library(leaflet)
library(plotly)

countries_in_data <- list("Australia" = "AUS",
  "Austria" = "AUT",
  "Belgium" = "BEL",
  "Canada" = "CAN",
  "Switzerland" = "CHE",
  "Czech Republic" = "CZE",
  "Germany" = "DEU",
  "Denmark" = "DNK",
  "Spain" = "ESP",
  "Estonia" = "EST",
  "Finland" = "FIN",
  "France" = "FRA",
  "United Kingdom" = "GBR",
  "Greece" = "GRC",
  "Ireland" = "IRL",
  "Iceland" = "ISL",
  "Israel" = "ISR",
  "Italy" = "ITA",
  "Japan" = "JPN",
  "Luxembourg" = "LUX",
  "Netherlands" = "NLD",
  "Norway" = "NOR",
  "New Zealand" = "NZL",
  "Poland" = "POL",
  "Portugal" = "PRT",
  "Slovenia" = "SVN",
  "Sweden" = "SWE",
  "United States" = "USA"
)

shinyUI(fluidPage(

  tags$head(
    tags$h1("VAPS-Dashboard"),
    tags$link(href="css/vaps_dashboard.css", rel="stylesheet", type="text/css")
  ),

  sidebarLayout(
    sidebarPanel(
      h4("Please select variable(s) of interest!"),

      conditionalPanel(condition = "input.tabs == 1",
        selectInput("var1", "Variable",
          choices = list("Veto Point President" = "vto_prs",
            "Veto Point Head of Government" = "vto_hog",
            "Veto Point Lower House" = "vto_lh",
            "Veto Point Upper House" = "vto_uh",
            "judicial Veto Point" = "vto_jud",
            "electoral Veto Point" = "vto_elct",
            "territorial Veto Point" = "vto_terr",
            "Sum of open Veto Points" = "vto_pts"
          )
        )
      ),
      conditionalPanel(condition = "input.tabs == 2",
        selectInput("var1", "Variable x-axis",
          choices = list("Cabinet seat share" = "cab_shr")
        ),
        selectInput("var2", "Variable y-axis",
          choices = list("Least squares index" = "lsq")
        ),
        checkboxInput("linear_box", "Add linear prediction", value= FALSE)
      ),
      conditionalPanel(condition = "input.tabs == 3",
        selectInput("var1", "Variable",
          choices = list("Seat share in Lower House" = "germany")
        )
      ),
      conditionalPanel(condition = "input.tabs == 4",
        selectInput("var_map", "Variable",
          choices = list("Average Cabinet Lower House Seat Share" = "Average Cabinet Lower House Seat Share")
        )
      ),
      conditionalPanel(condition = "input.tabs != 4",
        selectInput("country", "Country",
          choices = countries_in_data
        )
      ),
      
      sliderInput(
        "year_range",
        label = h3("Year range"),
        min = 1940,
        max = 2010,
        value = c(1940, 2010),
        sep = ""
      ) 
    ),

    mainPanel(

      tabsetPanel(id ="tabs",
        tabPanel("Lineplot",
          value = 1,
          tableOutput("information_veto"),
          h5(textOutput("polltitle_veto", inline=TRUE)),
          plotlyOutput("lineplot_veto", height="600px"),
          tableOutput("summary_veto"),
          downloadButton('downloadTable', 'Download table')
        ),
        tabPanel("Bivariate Association",
          value = 2,
          h5(textOutput("polltitle_bivar", inline=TRUE)),
          plotlyOutput("plot_bivar", height="600px"),
          downloadButton('downloadPlot2', 'Download graph'),
          tableOutput("summary_bivariate"),
          downloadButton('downloadTable2', 'Download table')
        ),
        tabPanel("Barplot",
          value = 3,
          plotlyOutput("plot_bar", height="600px")
        ),
        tabPanel("Map",
          value = 4,
          leafletOutput("plot_map", height="600px")
        )
      )
    )
  ),

  tags$footer(
    tags$a(target="_blank", href="https://www.sowi.hu-berlin.de/de/lehrbereiche/comppol", "Chair Comparative Politics at Humboldt-Universität zu Berlin"),
    tags$span(" · "),
    tags$a(target="_blank", href="https://welfarestatefutures.org", " Welfare State Futures - Norface")
  )

))