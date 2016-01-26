library(ggplot2)
library(shiny)
library(scales)

country <- read.csv2("/www-shiny/country.csv")
config_data_view_configuration_vto_lh <- read.csv2("/www-shiny/config_data.view_configuration_vto_lh.csv")

merged_data <- merge(config_data_view_configuration_vto_lh, country[c(1,2,3)], by="ctr_id")

merged_data$vto_lh <-  as.factor(merged_data$vto_lh)

shinyServer(function(input, output) {
  output$lineplot_veto <- renderPlot({
    print(get_plot_veto())
  })

  output$summary_veto <- renderTable({
    get_summary_veto()
  })

  get_plot_veto <- reactive ({
    min_date <- as.Date(as.character(input$year_range[1]),format="%Y")
    max_date <- as.Date(as.character(input$year_range[2]),format="%Y")

    merged_data <- merged_data[as.Date(merged_data$sdate) %in% c(max_date:min_date), ]
    
    choices = list(
      "Veto Point Lower House" = "vto_lh")

     veto_plot <- ggplot(merged_data, aes(x=as.Date(sdate),group=ctr_n))
     veto_plot <- veto_plot + geom_line(aes_string(y=input$var1), stat = "identity",colour="grey")
     #veto_plot <- veto_plot + geom_rug(aes_string(y=input$var1), stat = "identity",colour="blue")
     veto_plot <- veto_plot + geom_point(aes_string(y=input$var1), stat = "identity",colour="black", size=1)
     veto_plot <- veto_plot + facet_wrap( ~ ctr_n, ncol = 4) + theme_light() +
       ylab(names(choices[c(which(choices==input$var1)[1])])) + xlab("Date")
     veto_plot
   })

  get_summary_veto <- reactive({
    min_date <- as.Date(as.character(input$year_range[1]),format="%Y")
    max_date <- as.Date(as.character(input$year_range[2]),format="%Y")
    
    merged_data <- merged_data[as.Date(merged_data$sdate) %in% c(max_date:min_date), ]
    
    summary <- data.frame(table(merged_data[,input$var1],merged_data$ctr_n))
    names(summary)[1] <- "Open/Closed"
    names(summary)[2] <- "Country"
    summary
  })

  output$polltitle_veto = renderPrint({
    title <- "Presence of specific Veto Points. 1 = Open, 0 = Closed"
    cat(title)
  })

  output$downloadPlot <- downloadHandler(
    filename = function () {
      paste('plot', '.png', sep='')
    },
    content = function (file) {
      plot <- get_plot_veto()
      ggsave(file, plot, width=10, height=10)
    }
  )

  output$downloadTable <- downloadHandler(
    filename = function () {
      paste('table', '.csv', sep='')
    },
    content = function (file) {
      write.table(get_summary_veto(), file, row.names = TRUE)
    }
  )
})