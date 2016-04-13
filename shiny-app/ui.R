library(leaflet)

shinyUI(fluidPage(theme = "bootstrap.css",

  headerPanel("VAPS-Dashboard"),

  sidebarLayout(
    sidebarPanel(
      p(strong(em("Please select variable(s) of interest!"))),

      conditionalPanel(condition = "input.tabs == 1",
        selectInput("var1", "Variable",
          choices = list("Veto Point Election" = "vto_elct",
            "Veto Point Judiciary" = "vto_jud",
            "Veto Point Lower House" = "vto_lh")
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
        ),
        selectInput("var2", "Select country",
          choices = list("Germany" = "germany")
        )
      ),
      conditionalPanel(condition = "input.tabs == 4",
        selectInput("var_map", "Variable",
          choices = list("Average Cabinet Lower House Seat Share" = "Average Cabinet Lower House Seat Share")
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
          h5(textOutput("polltitle_veto", inline=TRUE)),
          plotOutput("lineplot_veto",height="500px"),
          downloadButton('downloadPlot', 'Download graph'),
          tableOutput("summary_veto"),
          downloadButton('downloadTable', 'Download table')
        ),
        tabPanel("Bivariate Association",
          value = 2,
          h5(textOutput("polltitle_bivar", inline=TRUE)),
          plotOutput("plot_bivar",height="500px"),
          downloadButton('downloadPlot2', 'Download graph'),
          tableOutput("summary_bivariate"),
          downloadButton('downloadTable2', 'Download table')
        ),
        tabPanel("Barplot",
          value = 3,
          plotOutput("plot_bar",height="500px")
        ),
        tabPanel("Map",
          value = 4,
          leafletOutput("plot_map",height="500px")
        )
      )
    )
  )
))