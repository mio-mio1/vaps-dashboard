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
        selectInput("variable_veto", "Variable",
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
        selectInput("variable_bivar1", "Variable x-axis",
          choices = list("LH Disproportionality" = "lhelc_lsq",
            "effective number of parties" = "lh_enpp",
            "cabinet seat share" = "cab_lh_sts_shr",
            "seat A volatitlity" = "lhelc_vola_sts",
            "seat B volatitlity" ="lhelc_volb_sts",
            "vote A volatitlity" = "lhelc_vola_vts",
            "vote B volatitlity" = "lhelc_volb_vts"
          ),
          selected = "lhelc_lsq"
        ),
        selectInput("variable_bivar2", "Variable y-axis",
          choices = list("LH Disproportionality" = "lhelc_lsq",
            "effective number of parties" = "lh_enpp",
            "cabinet seat share" = "cab_lh_sts_shr",
            "seat A volatitlity" = "lhelc_vola_sts",
            "seat B volatitlity" ="lhelc_volb_sts",
            "vote A volatitlity" = "lhelc_vola_vts",
            "vote B volatitlity" = "lhelc_volb_vts"
          ),
          selected = "lh_enpp"
        ),
        radioButtons("axis_scale", label = "Range x-Axis", 
          choices = list("Adjusted" = 2, "Original" = 1),
          selected = 2
        ),
        radioButtons("prediction_box", label = "Add linear prediction", 
          choices = list("None" = 0, "Linear" = 1, "Local" = 2),
          selected = 0
        )
      ),
      conditionalPanel(condition = "input.tabs == 3",
        selectInput("variable_barplot", "Variable",
          choices = list("Seat share in Lower House" = "germany")
        )
      ),
      conditionalPanel(condition = "input.tabs == 4",
        selectInput("variable_map", "Variable",
          choices = list("Average Cabinet Lower House Seat Share" = "Average Cabinet Lower House Seat Share")
        )
      ),
      conditionalPanel(condition = "input.tabs != 4",
        selectInput("country", "Country",
          choices = countries_in_data
        )
      ),

      conditionalPanel(condition = "input.tabs < 3",
        sliderInput(
          "year_range",
          label = h3("Year range"),
          min = 1940,
          max = 2016,
          value = c(1940, 2016),
          sep = ""
        )
      )
    ),

    mainPanel(

      tabsetPanel(id ="tabs",
        tabPanel("Lineplot",
          value = 1,
          tableOutput("information_veto"),
          h5(textOutput("polltitle_veto", inline=TRUE)),
          plotOutput("lineplot_veto", height="600px"),
          downloadButton('downloadVetoPlot', 'Download graph'),
          tableOutput("summary_veto"),
          downloadButton('downloadVetoTable', 'Download table')
        ),
        tabPanel("Bivariate Association",
          value = 2,
          h5(textOutput("polltitle_bivar", inline=TRUE)),
          plotOutput("plot_bivar", height="600px"),
          downloadButton('downloadBivariatePlot', 'Download graph'),
          tableOutput("summary_bivar"),
          downloadButton('downloadBivariateTable', 'Download table')
        ),
        tabPanel("Barplot",
          value = 3,
          plotOutput("plot_bar", height="600px")
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