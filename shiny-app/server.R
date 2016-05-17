library(ggplot2)
library(shiny)
library(scales)
library(R.cache)
library(leaflet)
library(rgdal)
library(jsonlite)
library(plotly)

loadCache(load(file = "data/data_13april.Rdata"), key = list("", ""))


shinyServer(function(input, output) {

  output$lineplot_veto <- renderPlot({
    #pdf(NULL)
    get_plot_veto()
  })

  output$summary_veto <- renderTable({
    get_summary_veto()
  })

  output$information_veto <-  renderTable({
    get_information_veto()
  }, include.rownames=FALSE)

  output$plot_bivar <- renderPlot({
    #pdf(NULL)
    get_plot_bivar()
  })

  output$plot_bar <- renderPlot({
    #pdf(NULL)
    get_plot_bar()
  })

  output$plot_map <- renderLeaflet({
    get_plot_map()
  })


  #reactives needed for first tab, veto point plots
  get_information_veto <- reactive({
    if (input$var1 != "vto_pts") {
      country <- country[c("ctr_id", "ctr_n", "ctr_ccode")]
      merged_data <- merge(veto_points, country, by="ctr_id")
      merged_data <- subset(merged_data,merged_data[,"ctr_ccode"]==input$country)

      choices <- list(
        "1" = "vto_prs",
        "2" = "vto_hog",
        "3" = "vto_lh",
        "4" = "vto_uh",
        "5" = "vto_jud",
        "6" = "vto_elct",
        "7" = "vto_terr"
      )

      names(choices) <- paste0(unique(merged_data[,"ctr_id"]),"00",names(choices))
      merged_data[,"vto_cmt"] <- gsub("^.$", "no comment", merged_data[,"vto_cmt"])

      information_table <- data.frame()
      information_table[1,1] <- merged_data[ which(merged_data$vto_id==names(which(choices==input$var1))),c(4)]
      information_table[1,2] <- merged_data[ which(merged_data$vto_id==names(which(choices==input$var1))),c(5)]
      information_table[1,3] <- merged_data[ which(merged_data$vto_id==names(which(choices==input$var1))),c(8)]
      information_table[1,4] <- merged_data[ which(merged_data$vto_id==names(which(choices==input$var1))),c(9)]
      colnames(information_table) <- c("Name", "English Name", "Veto Power", "Comment")
      information_table
    }
  })

  get_plot_veto <- reactive ({
    ## institutional veto points
    # Head of State / President
    # Head of Government 
    # Lower House
    # Upper House
    # judicial
    # electoral
    # territorial

    country <- country[c("ctr_id", "ctr_n", "ctr_ccode")]
    merged_data <- merge(eval(parse(text=input$var1))[c("ctr_id", "sdate", input$var1)], country, by="ctr_id")

    merged_data <- subset(merged_data,merged_data[,"ctr_ccode"]==input$country)

    if (length(merged_data[,1]) == 0) {
      # do nothing
    } else {

      # adjust level of factor dependent on missing values and input chosen
      if (any(is.na(merged_data[,input$var1]))==TRUE & input$var1 != "vto_pts") {
        merged_data[,input$var1] <- factor(merged_data[,input$var1], levels=c("0","1", "NA"))
        merged_data[,input$var1][is.na(merged_data[,input$var1])] <- "NA"
      } else if (any(is.na(merged_data[,input$var1]))==FALSE & input$var1 != "vto_pts") {
        merged_data[,input$var1] <- factor(merged_data[,input$var1], levels=c("0","1"))
      } else if (any(is.na(merged_data[,input$var1]))==TRUE & input$var1 == "vto_pts") {
        merged_data[,input$var1] <- factor(merged_data[,input$var1], levels=c(seq(0, max(as.numeric(as.character(na.omit(merged_data[,"vto_pts"]))))),"NA"))
        merged_data[,input$var1][is.na(merged_data[,input$var1])] <- "NA"
      } else if (any(is.na(merged_data[,input$var1]))==FALSE & input$var1 == "vto_pts") {
        merged_data[,input$var1] <- factor(merged_data[,input$var1], levels=c(seq(0, max(as.numeric(as.character(na.omit(merged_data[,"vto_pts"])))))))
      }

      min_date <- as.Date(as.character(input$year_range[1]),format="%Y")
      max_date <- as.Date(as.character(input$year_range[2]),format="%Y")

      merged_data$sdate <- as.Date(merged_data$sdate)

      merged_data <- merged_data[merged_data$sdate %in% c(max_date:min_date), ]

      choices <- list("Veto Point President" = "vto_prs",
        "Veto Point Head of Government" = "vto_hog",
        "Veto Point Lower House" = "vto_lh",
        "Veto Point Upper House" = "vto_uh",
        "judicial Veto Point" = "vto_jud",
        "electoral Veto Point" = "vto_elct",
        "territorial Veto Point" = "vto_terr",
        "Sum of open Veto Points" = "vto_pts"
      )

      veto_plot <- ggplot(merged_data, aes_string(x="sdate", y=input$var1))
      veto_plot <- veto_plot + geom_point(stat = "identity",colour="black", size=1)
      veto_plot <- veto_plot + theme_light() + xlab("Date") +
        ylab(names(choices[c(which(choices==input$var1)[1])]))
      if (input$var1 != "vto_pts") {
        veto_plot <- veto_plot + scale_y_discrete(drop=FALSE)
        veto_plot
        #(plotly_veto_plot <- ggplotly(veto_plot))
      } else {
        veto_plot <- veto_plot + scale_y_discrete(drop=FALSE)
        veto_plot
      }
    }
  })

  get_summary_veto <- reactive({
    country <- country[c("ctr_id", "ctr_n", "ctr_ccode")]
    merged_data <- merge(eval(parse(text=input$var1))[c("ctr_id", "sdate", input$var1)], country, by="ctr_id")

    merged_data[,input$var1] <- as.factor(merged_data[,input$var1])
    min_date <- as.Date(as.character(input$year_range[1]),format="%Y")
    max_date <- as.Date(as.character(input$year_range[2]),format="%Y")
    merged_data <- merged_data[as.Date(merged_data$sdate) %in% c(max_date:min_date), ]
    choices <- list("Veto Point President" = "vto_prs",
      "Veto Point Head of Government" = "vto_hog",
      "Veto Point Lower House" = "vto_lh",
      "Veto Point Upper House" = "vto_uh",
      "judicial Veto Point" = "vto_jud",
      "electoral Veto Point" = "vto_elct",
      "territorial Veto Point" = "vto_terr",
      "Sum of open Veto Points" = "vto_pts"
    )
    merged_data <- subset(merged_data,merged_data[,"ctr_ccode"]==input$country)

    if (length(merged_data[,1]) == 0) {
      # do nothing
    } else {
      if (input$var1 != "vto_pts") {
        table <- table(as.Date(merged_data$sdate),merged_data[,input$var1])

        summary <- data.frame(table[,1])
        summary[1] <- ifelse(summary[1]==1, "Closed", "Open")
        names(summary)[1] <- "Open/Closed"
        summary
      } else {
        table <- table(as.Date(merged_data$sdate),merged_data[,"vto_pts"])

        for (i in 1:length(colnames(table))) {
          table[,i] <- ifelse(table[,i]==1, as.numeric(colnames(table)[i]), 0)
        }

        table <- as.data.frame.matrix(table)
        table$'Sum Veto Points' <- rowSums(table)
        table$'Sum Veto Points' <- as.factor(table$'Sum Veto Points')
        table <- subset(table, select = c('Sum Veto Points'))
      }
    }
  })

  get_plot_bivar <- reactive ({
    view_cab_lh_sts_shr$cab_lh_sts_shr <- as.numeric(as.character(view_cab_lh_sts_shr$cab_lh_sts_shr))

    merged_agg_data <-aggregate(view_cab_lh_sts_shr, by=list(view_cab_lh_sts_shr$lh_id), 
      FUN=mean, na.rm=TRUE)
    names(view_lhelc_lsq)[1] <- "lh_id"

    view_lhelc_lsq$lhelc_lsq_computed <- as.numeric(as.character(view_lhelc_lsq$lhelc_lsq_computed))
    
    merged_agg_data <- merge(merged_agg_data, view_lhelc_lsq, by="lh_id")
    merged_agg_data$cab_lh_sts_shr2 <- merged_agg_data$cab_lh_sts_shr*100
    
    min_date <- as.Date(as.character(input$year_range[1]),format="%Y")
    max_date <- as.Date(as.character(input$year_range[2]),format="%Y")
    
    merged_agg_data <- merged_agg_data[as.Date(merged_agg_data$sdate) %in% c(max_date:min_date), ]
    
    model <- lm(lhelc_lsq_computed ~ cab_lh_sts_shr2, data=merged_agg_data)

    veto_plot <- ggplot(merged_agg_data, aes(x=cab_lh_sts_shr,y=lhelc_lsq_computed))
    veto_plot <- veto_plot + geom_point(stat = "identity",colour="black", size=1) + theme_light() +
      ylab("Least squares index") + xlab("Cabinet seat share in Lower House") + 
      scale_y_continuous(expand = c(0, 0)) + scale_x_continuous(labels = percent_format(),expand = c(0, 0),limits=c(0, 1.1))

    if (input$linear_box) {
      veto_plot <- veto_plot + geom_smooth(method=lm)
      veto_plot
      #(veto_plot <- ggplotly(veto_plot))
    } else {
      #(veto_plot <- ggplotly(veto_plot))
      veto_plot
    }
  })

  get_plot_bar <- reactive ({
    merged_data_bar <- merge(lower_house, view_pty_lh_sts_shr, by="lh_id")
    merged_data_bar <- merge(merged_data_bar, country[c(1,2,3)], by="ctr_id")
    merged_data_bar <- merge(merged_data_bar, party[,c(1,2)], by="pty_id")

    choices = list("Lower house seat share" = "vto_elct")

    merged_data_bar <- subset(na.omit(subset(merged_data_bar,merged_data_bar[,"ctr_ccode"]==input$country & merged_data_bar$pty_abr!= "Z")))
    
    min_date <- as.Date(as.character(input$year_range[1]),format="%Y")
    max_date <- as.Date(as.character(input$year_range[2]),format="%Y")
    
    merged_data_bar <- merged_data_bar[as.Date(merged_data_bar$sdate) %in% c(max_date:min_date), ]
    
    merged_data_bar$pty_lhelc_sts_shr <- as.numeric(as.character(merged_data_bar$pty_lhelc_sts_shr))

    merged_data_bar <- merged_data_bar[order(merged_data_bar$ctr_id, merged_data_bar$pty_abr),]

    bar_plot <- ggplot(merged_data_bar, aes(x=as.Date(lh_sdate), y=pty_lhelc_sts_shr, fill=pty_abr))
    bar_plot <- bar_plot + geom_bar(stat = "identity", position = "stack") + scale_fill_brewer(palette=1, na.value="black")
    bar_plot <- bar_plot + theme_light() + xlab("Date") + ylab("Seat share by party") + guides(fill = guide_legend(reverse = TRUE, title="Party"))
    (bar_plot <- ggplotly(bar_plot))
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
    if (input$var1 != "vto_pts") {
      cat("Presence of specific Veto Points. 1 = Open, 0 = Closed")
    } else {
      cat("Summary of specific Veto Points by configuration")
    }
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