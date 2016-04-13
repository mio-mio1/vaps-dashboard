library(ggplot2)
library(shiny)
library(scales)
library(R.cache)
library(leaflet)
library(rgdal)
library(jsonlite)

loadCache(load(file = "data/data_13april.Rdata"), key = list("", ""))

merged_data <- merge(vto_lh, country[c(1,2,3)], by="ctr_id")
merged_data <- merge(merged_data, vto_jud, by=c("ctr_id","sdate"))
merged_data <- merge(merged_data, vto_elct, by=c("ctr_id","sdate"))

merged_data$vto_jud <-  as.factor(merged_data$vto_jud)
merged_data$vto_lh <-  as.factor(merged_data$vto_lh)
merged_data$vto_elct <-  as.factor(merged_data$vto_elct)

shinyServer(function(input, output) {
  output$lineplot_veto <- renderPlot({
    print(get_plot_veto())
  })

  output$summary_veto <- renderTable({
    get_summary_veto()
  })

  output$plot_bivar <- renderPlot({
    get_plot_bivar()
  })

  output$plot_bar <- renderPlot({
    get_plot_bar()
  })

  output$plot_map <- renderLeaflet({
    get_plot_map()
  })

  get_plot_veto <- reactive ({
    min_date <- as.Date(as.character(input$year_range[1]),format="%Y")
    max_date <- as.Date(as.character(input$year_range[2]),format="%Y")

    merged_data <- merged_data[as.Date(merged_data$sdate) %in% c(max_date:min_date), ]
    
    choices <- list("Veto Point Election" = "vto_elct",
      "Veto Point Judiciary" = "vto_jud",
      "Veto Point Lower House" = "vto_lh")
     veto_plot <- ggplot(subset(merged_data, merged_data$ctr_ccode==input$country), aes(x=as.Date(sdate)))
     #veto_plot <- veto_plot + geom_line(aes_string(y=input$var1), stat = "identity",colour="grey")
     veto_plot <- veto_plot + geom_point(aes_string(y=input$var1), stat = "identity",colour="black", size=1)
     veto_plot <- veto_plot + facet_wrap( ~ ctr_n, ncol = 4) + theme_light() +
       ylab(names(choices[c(which(choices==input$var1)[1])])) + scale_y_discrete(limits=c("0","1"), drop = FALSE) + xlab("Date")
     veto_plot
   })

  get_summary_veto <- reactive({
    min_date <- as.Date(as.character(input$year_range[1]),format="%Y")
    max_date <- as.Date(as.character(input$year_range[2]),format="%Y")

    merged_data <- subset(merged_data,merged_data[,"ctr_ccode"]==input$country)

    merged_data <- merged_data[as.Date(merged_data$sdate) %in% c(max_date:min_date), ]

    table <- table(as.Date(merged_data$sdate),merged_data[,input$var1])

    summary <- data.frame(table[,1])
    summary[1] <- ifelse(summary[1]==1, "Closed", "Open")
    names(summary)[1] <- "Open/Closed"
    summary


  })

  get_plot_bivar <- reactive ({
    view_cab_lh_sts_shr$cab_lh_sts_shr <- as.numeric(as.character(view_cab_lh_sts_shr$cab_lh_sts_shr))

    merged_agg_data <-aggregate(view_cab_lh_sts_shr, by=list(view_cab_lh_sts_shr$lh_id), 
      FUN=mean, na.rm=TRUE)
    names(view_lhelc_lsq)[1] <- "lh_id"

    view_lhelc_lsq$lhelc_lsq_computed <- as.numeric(as.character(view_lhelc_lsq$lhelc_lsq_computed))
    
    merged_agg_data <- merge(merged_agg_data, view_lhelc_lsq, by="lh_id")
    merged_agg_data$cab_lh_sts_shr2 <- merged_agg_data$cab_lh_sts_shr*100
    model <- lm(lhelc_lsq_computed ~ cab_lh_sts_shr2, data=merged_agg_data)

    veto_plot <- ggplot(merged_agg_data, aes(x=cab_lh_sts_shr,y=lhelc_lsq_computed))
    veto_plot <- veto_plot + geom_point(stat = "identity",colour="black", size=1) + theme_light() +
      ylab("Least squares index") + xlab("Cabinet seat share in Lower House") + 
      scale_y_continuous(expand = c(0, 0)) + scale_x_continuous(labels = percent_format(),expand = c(0, 0),limits=c(0, 1.1))

    if (input$linear_box) {
      veto_plot <- veto_plot + geom_smooth(method=lm)
      veto_plot
    } else {
      veto_plot
    }
  })

  get_plot_bar <- reactive ({
    merged_data_bar <- merge(lower_house, view_pty_lh_sts_shr, by="lh_id")
    merged_data_bar <- merge(merged_data_bar, country[c(1,2,3)], by="ctr_id")
    merged_data_bar <- merge(merged_data_bar, party[,c(1,2)], by="pty_id")

    choices = list("Lower house seat share" = "vto_elct")

    merged_data_bar <- subset(na.omit(subset(merged_data_bar,merged_data_bar$ctr_n=="GERMANY" & merged_data_bar$pty_abr!= "Z")))

    merged_data_bar$pty_lhelc_sts_shr <- as.numeric(as.character(merged_data_bar$pty_lhelc_sts_shr))

    merged_data_bar <- merged_data_bar[order(merged_data_bar$ctr_id, merged_data_bar$pty_abr),]

    veto_plot <- ggplot(merged_data_bar, aes(x=as.Date(lh_sdate), y=pty_lhelc_sts_shr, fill=pty_abr))
    veto_plot <- veto_plot + geom_bar(stat = "identity", position = "stack") + scale_fill_brewer(palette=1, na.value="black")
    veto_plot <- veto_plot + geom_text(data=merged_data_bar,aes(x=as.Date(lh_sdate), y=pty_lhelc_sts_shr, fill=pty_abr, 
      label =  ifelse(merged_data_bar$pty_lhelc_sts_shr > 0.05,paste0(round(pty_lhelc_sts_shr,2)*100,"%"), "")), position = "stack", vjust=1, size=6) +
      theme_light() + scale_y_continuous(labels = percent_format(),expand = c(0, 0),limits=c(0, 1.05)) +
      xlab("Date") + ylab("Seat share by party") + guides(fill = guide_legend(reverse = TRUE, title="Party"))
    veto_plot
  })

  get_plot_map <- reactive ({
    geojson <- readLines("data/countriesOld.geojson", warn = FALSE) %>%
      paste(collapse = "\n") %>%
      fromJSON(simplifyVector = FALSE)

    # merge our data so that there are country ids in there
    merged_data <- merge(vto_lh, country[c(1,2,3)], by="ctr_id")

    names(merged_data)[names(merged_data)=="ctr_ccode"] <- "iso_a3"
    merged_data$cab_lh_sts_shr <- merged_data$cab_lh_sts_shr*100

    # in this example, aggregate cabinet lower house seat share by country
    aggdata <-aggregate(merged_data$cab_lh_sts_shr, by=list(merged_data$iso_a3), 
      FUN=mean, na.rm=TRUE)

    id_list <- list()
    id_list$data <- data.frame()
    for (i in 1:length(unique(merged_data$iso_a3))) {
      boolean <- lapply(geojson$features, function(x) {
        x$properties$iso_a3==aggdata[,1][i]
      })
      id_list$data[i,1] <- print(which(boolean==TRUE))
      id_list$data[i,2]  <-  aggdata[,2][i]
    }

    for (i in 1:nrow(id_list$data)) {
      geojson$features[[id_list$data[i,1]]]$properties$cab_lh_sts_shr <- id_list$data[i,2]
    }

    cab_lh_sts_shr <- unlist(sapply(geojson$features, function(feat) {
      print(feat$properties$cab_lh_sts_shr)
    }))

    ## Setting overall Styles
    geojson$style = list(
      weight = 1,
      color = "#555555",
      opacity = 1,
      fillOpacity = 0.8
    )

    # choose numeric instead of categorical palette
    pal <- colorNumeric(
      palette = "YlGnBu",
      domain = cab_lh_sts_shr
    )

    geojson$features <- lapply(geojson$features, function(feat) {
      feat$properties$style <- list(
        fillColor = pal(
          feat$properties$cab_lh_sts_shr
        )
      )
      feat
    })

    ## Mapping GDP/Capita
    map_cab_lh_sts_shr <- leaflet() %>%
      setView(lng = 8, lat = 55, zoom = 3) %>%
      setMaxBounds(-33, 25, 48, 72) %>%
      addGeoJSON(geojson) %>%
      addLegend("bottomleft", pal = pal, values = cab_lh_sts_shr,
        title = "Average Cabinet Lower House Seat Share", opacity = 1, 
        labFormat = labelFormat(suffix = "%"))
    map_cab_lh_sts_shr
    
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