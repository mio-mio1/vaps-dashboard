shinyUI(fluidPage(theme = "bootstrap.css",

  headerPanel("VAPS-Dashboard"),

  sidebarLayout(
    sidebarPanel(

    p(strong(em("Please select variable(s) of interest!"))),

    conditionalPanel(
      condition = "input.tabs == 1",
      selectInput("var1", "Variable",
        choices = list("Veto Point Lower House" = "vto_lh"
        )
      )
    ),

    sliderInput(
      "year_range",
      label = h3("Year range"),
      min = 1940,
      max = 2010,
      value = c(1940, 2010),
      sep = ""
    ),

    br (),

    a("link data source", 
    href = "https://www.sowi.hu-berlin.de/de/lehrbereiche/comppol/forschung/aktuell/polisdato")
    
    ),

    mainPanel(

      tabsetPanel(
        id ="tabs",
        tabPanel(
          "Lineplot",
          value = 1,
          h5(textOutput("polltitle_veto", inline=TRUE)),
          plotOutput("lineplot_veto",height="500px"),
          downloadButton('downloadPlot', 'Download graph'),
          tableOutput("summary_veto"),
          downloadButton('downloadTable', 'Download table')
        )
      )
    )
  )
))