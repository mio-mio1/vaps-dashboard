library(ggplot2)
library(shiny)
library(scales)
library(R.cache)
library(leaflet)
library(rgdal)
library(jsonlite)
library(plotly)
library(plyr)

loadCache(load(file = "data/data_17may.Rdata"), key = list("", ""))


shinyServer(function(input, output) {

  output$lineplot_veto <- renderPlot({
    #pdf(NULL)
    get_plot_veto()
  })

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

  output$summary_veto <- renderTable({
    get_summary_veto()
  })

  output$summary_bivar <- renderTable({
    get_summary_bivar()
  })

  output$summary_barplot <- renderTable({
    get_summary_bar()
  }, include.rownames=FALSE)

  output$information_veto <-  renderTable({
    get_information_veto()
  }, include.rownames=FALSE)

  #reactives needed for first tab, veto point plots
  get_information_veto <- reactive({
    if (input$variable_veto != "vto_pts") {
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
      information_table[1,1] <- merged_data[ which(merged_data$vto_id==names(which(choices==input$variable_veto))),c(4)]
      information_table[1,2] <- merged_data[ which(merged_data$vto_id==names(which(choices==input$variable_veto))),c(5)]
      information_table[1,3] <- merged_data[ which(merged_data$vto_id==names(which(choices==input$variable_veto))),c(8)]
      information_table[1,4] <- merged_data[ which(merged_data$vto_id==names(which(choices==input$variable_veto))),c(9)]
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

    choices <- list("Veto Point President" = "vto_prs",
      "Veto Point Head of Government" = "vto_hog",
      "Veto Point Lower House" = "vto_lh",
      "Veto Point Upper House" = "vto_uh",
      "judicial Veto Point" = "vto_jud",
      "electoral Veto Point" = "vto_elct",
      "territorial Veto Point" = "vto_terr",
      "Sum of open Veto Points" = "vto_pts"
    )

    country <- country[c("ctr_id", "ctr_n", "ctr_ccode")]
    merged_data <- merge(eval(parse(text=input$variable_veto))[c("ctr_id", "sdate", input$variable_veto)], country, by="ctr_id")

    merged_data <- subset(merged_data,merged_data[,"ctr_ccode"]==input$country)

    if (length(merged_data[,1]) == 0) {
      # do nothing
    } else {

      # adjust level of factor dependent on missing values and input chosen
      if (any(is.na(merged_data[,input$variable_veto]))==TRUE & input$variable_veto != "vto_pts") {
        merged_data[,input$variable_veto] <- factor(merged_data[,input$variable_veto], levels=c("0","1", "NA"))
        merged_data[,input$variable_veto][is.na(merged_data[,input$variable_veto])] <- "NA"
      } else if (any(is.na(merged_data[,input$variable_veto]))==FALSE & input$variable_veto != "vto_pts") {
        merged_data[,input$variable_veto] <- factor(merged_data[,input$variable_veto], levels=c("0","1"))
      } else if (any(is.na(merged_data[,input$variable_veto]))==TRUE & input$variable_veto == "vto_pts") {
        merged_data[,input$variable_veto] <- factor(merged_data[,input$variable_veto], levels=c(seq(0, max(as.numeric(as.character(na.omit(merged_data[,"vto_pts"]))))),"NA"))
        merged_data[,input$variable_veto][is.na(merged_data[,input$variable_veto])] <- "NA"
      } else if (any(is.na(merged_data[,input$variable_veto]))==FALSE & input$variable_veto == "vto_pts") {
        merged_data[,input$variable_veto] <- factor(merged_data[,input$variable_veto], levels=c(seq(0, max(as.numeric(as.character(na.omit(merged_data[,"vto_pts"])))))))
      }

      min_date <- as.Date(as.character(input$year_range[1]),format="%Y")
      max_date <- as.Date(as.character(input$year_range[2]),format="%Y")

      merged_data$sdate <- as.Date(merged_data$sdate)

      merged_data <- merged_data[merged_data$sdate %in% c(max_date:min_date), ]

      veto_plot <- ggplot(merged_data, aes_string(x="sdate", y=input$variable_veto))
      veto_plot <- veto_plot + geom_point(stat = "identity",colour="black", size=1)
      veto_plot <- veto_plot + theme_light() + xlab("Date") +
        ylab(names(choices[c(which(choices==input$variable_veto)[1])]))
      if (input$variable_veto != "vto_pts") {
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
    merged_data <- merge(eval(parse(text=input$variable_veto))[c("ctr_id", "sdate", input$variable_veto)], country, by="ctr_id")

    merged_data[,input$variable_veto] <- as.factor(merged_data[,input$variable_veto])
    min_date <- as.Date(as.character(input$year_range[1]),format="%Y")
    max_date <- as.Date(as.character(input$year_range[2]),format="%Y")
    merged_data <- merged_data[as.Date(merged_data$sdate) %in% c(max_date:min_date), ]

    merged_data <- subset(merged_data,merged_data[,"ctr_ccode"]==input$country)

    if (length(merged_data[,1]) == 0) {
      # do nothing
    } else {
      if (input$variable_veto != "vto_pts") {
        table <- table(as.Date(merged_data$sdate),merged_data[,input$variable_veto])

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
    choices <- list("LH Disproportionality" = "lhelc_lsq",
      "effective number of parties" = "lh_enpp",
      "cabinet seat share" = "cab_lh_sts_shr",
      "seat A volatitlity" = "lhelc_vola_sts",
      "seat B volatitlity" ="lhelc_volb_sts",
      "vote A volatitlity" = "lhelc_vola_vts",
      "vote B volatitlity" = "lhelc_volb_vts"
    )

    country <- country[c("ctr_id", "ctr_n", "ctr_ccode")]
    merged_data <- merge(view_cab_lh_sts_shr, country, by="ctr_id")
    merged_data <- merge(merged_data, lower_house, by="lh_id")
    merged_data <- merge(merged_data, lh_election, by="lhelc_id")

    merged_data[,input$variable_bivar1] <- as.numeric(as.character(merged_data[,input$variable_bivar1]))
    merged_data[,input$variable_bivar2] <- as.numeric(as.character(merged_data[,input$variable_bivar2]))

    max_x_value <- max(merged_data[,input$variable_bivar1], na.rm = TRUE)
    max_y_value <- max(merged_data[,input$variable_bivar2], na.rm = TRUE)

    merged_data <- merged_data[c("ctr_ccode", "sdate", input$variable_bivar1, input$variable_bivar2)]

    merged_data <- subset(merged_data,merged_data[,"ctr_ccode"]==input$country)

    merged_agg_data <-aggregate(merged_data, by=list(merged_data$sdate),
      FUN=mean, na.rm=TRUE)

    min_date <- as.Date(as.character(input$year_range[1]),format="%Y")
    max_date <- as.Date(as.character(input$year_range[2]),format="%Y")

    merged_agg_data <- merged_agg_data[as.Date(merged_agg_data$sdate) %in% c(max_date:min_date), ]

    #if ( (any(!is.na(merged_agg_data[,input$variable_bivar1]))==FALSE) |  (any(!is.na(merged_agg_data[,input$variable_bivar2]))==FALSE) ) {
      # do nothing
    #} else {

      bivar_plot <- ggplot(merged_agg_data, aes_string(x=input$variable_bivar1, y=input$variable_bivar2))
      bivar_plot <- bivar_plot + geom_point(stat = "identity",colour="black", size=1) + theme_light() +
        xlab(names(choices[c(which(choices==input$variable_bivar1)[1])])) +
        ylab(names(choices[c(which(choices==input$variable_bivar2)[1])]))
        #scale_y_continuous(expand = c(0, 0)) + scale_x_continuous(expand = c(0, 0))

      if (input$prediction_box==1) {
        if (input$prediction_box==1 & input$axis_scale==1) {
          bivar_plot <- bivar_plot + scale_x_continuous(limits=c(0, max_x_value)) +
            scale_y_continuous(limits=c(0, max_y_value)) +
            geom_smooth(method=lm, alpha=0.2)
        } else if (input$prediction_box==1 & input$axis_scale==2) {
          bivar_plot <- bivar_plot + 
          geom_smooth(method=lm, alpha=0.2)
        }
      } else if (input$prediction_box==2) {
        if (input$prediction_box==2 & input$axis_scale==1) {
          bivar_plot <- bivar_plot + scale_x_continuous(limits=c(0, max_x_value)) +
            scale_y_continuous(limits=c(0, max_y_value)) +
            geom_smooth(method=loess, alpha=0.2)
        } else if (input$prediction_box==2 & input$axis_scale==2) {
          bivar_plot <- bivar_plot + 
          geom_smooth(method=loess, alpha=0.2)
        }
      } else if (input$prediction_box==0) {
        if (input$prediction_box==0 & input$axis_scale==1) {
          bivar_plot <- bivar_plot + scale_x_continuous(limits=c(0, max_x_value)) +
            scale_y_continuous(limits=c(0, max_y_value))
        } else if (input$prediction_box==0 & input$axis_scale==2) {
          #do nothing
        }
      }
      bivar_plot
    #}
  })


  get_summary_bivar <- reactive({
    choices <- list("LH Disproportionality" = "lhelc_lsq",
      "effective number of parties" = "lh_enpp",
      "cabinet seat share" = "cab_lh_sts_shr",
      "seat A volatitlity" = "lhelc_vola_sts",
      "seat B volatitlity" ="lhelc_volb_sts",
      "vote A volatitlity" = "lhelc_vola_vts",
      "vote B volatitlity" = "lhelc_volb_vts"
    )

    country <- country[c("ctr_id", "ctr_n", "ctr_ccode")]
    merged_data <- merge(view_cab_lh_sts_shr, country, by="ctr_id")
    merged_data <- merge(merged_data, lower_house, by="lh_id")
    merged_data <- merge(merged_data, lh_election, by="lhelc_id")

    merged_data[,input$variable_bivar1] <- as.numeric(as.character(merged_data[,input$variable_bivar1]))
    merged_data[,input$variable_bivar2] <- as.numeric(as.character(merged_data[,input$variable_bivar2]))

    merged_data <- merged_data[c("ctr_ccode", "sdate", input$variable_bivar1, input$variable_bivar2)]

    merged_data <- subset(merged_data,merged_data[,"ctr_ccode"]==input$country)

    merged_agg_data <-aggregate(merged_data, by=list(merged_data$sdate),
      FUN=mean, na.rm=TRUE)

    #merged_agg_data <- merge(merged_data, view_lhelc_lsq, by="lh_id")
    #merged_agg_data$cab_lh_sts_shr2 <- merged_agg_data$cab_lh_sts_shr*100

    min_date <- as.Date(as.character(input$year_range[1]),format="%Y")
    max_date <- as.Date(as.character(input$year_range[2]),format="%Y")

    merged_agg_data <- merged_agg_data[as.Date(merged_agg_data$sdate) %in% c(max_date:min_date), ]

    #if (length(merged_data[,1]) == 0) {
      # do nothing
    #} else {
      merged_agg_data <- merged_agg_data[,c(1,4,5)]
      rownames(merged_agg_data) <-  merged_agg_data[,c(1)]
      merged_agg_data <- merged_agg_data[,c(2,3)]
      colnames(merged_agg_data) <- c(names(choices[c(which(choices==input$variable_bivar1)[1])]),
        names(choices[c(which(choices==input$variable_bivar2)[1])]))
      merged_agg_data
    #}
  })

  get_plot_bar <- reactive ({
    if (input$variable_barplot=="pty_lhelc_sts_shr") {
      merged_data_bar <- merge(lower_house, view_pty_lh_sts_shr, by="lh_id")
      merged_data_bar <- merge(merged_data_bar, country[c(1,2,3)], by="ctr_id")
      merged_data_bar <- merge(merged_data_bar, party[,c(1,2)], by="pty_id")
      names(merged_data_bar)[names(merged_data_bar)=="lh_sdate"] <- "sdate"
    } else if (input$variable_barplot=="pty_uh_sts_shr") {
      merged_data_bar <- merge(upper_house, view_pty_uh_sts_shr, by="uh_id")
      merged_data_bar <- merge(merged_data_bar, country[c(1,2,3)], by="ctr_id")
      merged_data_bar <- merge(merged_data_bar, party[,c(1,2)], by="pty_id")
      names(merged_data_bar)[names(merged_data_bar)=="uh_sdate"] <- "sdate"
    }

    choices <- list("Lower house seat share" = "pty_lhelc_sts_shr",
      "Upper house seat share" = "pty_uh_sts_shr")

    merged_data_bar <- subset(merged_data_bar,merged_data_bar[,"ctr_ccode"]==input$country)

    min_date <- as.Date(as.character(input$year_range[1]),format="%Y")
    max_date <- as.Date(as.character(input$year_range[2]),format="%Y")

    merged_data_bar$sdate <- as.Date(merged_data_bar$sdate)
    merged_data_bar <- merged_data_bar[merged_data_bar$sdate %in% c(max_date:min_date), ]

    merged_data_bar[,input$variable_barplot] <- as.numeric(as.character(merged_data_bar[,input$variable_barplot]))

    threshold <- ifelse(input$threshold_barplot==1, 0.025, 
      ifelse(input$threshold_barplot==2, 0.05, 0.10))
    merged_data_bar[,"pty_abr"] <- ifelse(merged_data_bar[,input$variable_barplot] < threshold, "other", merged_data_bar[,"pty_abr"])

    merged_data_bar <- merged_data_bar[order(merged_data_bar$ctr_id, merged_data_bar$sdate, merged_data_bar$pty_abr),]

    split_data <- split(merged_data_bar,merged_data_bar$sdate)

    for (i in 1:length(split_data)){
      split_data[[i]][,input$variable_barplot] <- ifelse(split_data[[i]][,input$variable_barplot] <= threshold, round(sum(split_data[[i]][,input$variable_barplot][which(split_data[[i]][,input$variable_barplot] <= threshold)])*100,2),
      paste0(round(split_data[[i]][,input$variable_barplot]*100,2)))
    }

    for (i in 1:length(split_data)){
      non_other <- which(split_data[[i]]$pty_abr!="other")
      other_first <- which(split_data[[i]]$pty_abr=="other")[1]
      split_data[[i]] <- split_data[[i]][c(non_other,other_first),]
    }

    merged_data_bar <- rbind.fill(split_data)
    merged_data_bar[,input$variable_barplot] <- as.numeric(merged_data_bar[,input$variable_barplot])

    bar_plot <- ggplot(merged_data_bar, aes_string(x="sdate", y=input$variable_barplot, fill="pty_abr", label = input$variable_barplot))
    bar_plot <- bar_plot + geom_bar(stat = "identity", position = "stack") + scale_fill_brewer(palette=1, na.value="black")
    bar_plot <- bar_plot + theme_light() + xlab("Date") + ylab(names(choices[c(which(choices==input$variable_barplot)[1])])) +
      guides(fill = guide_legend(reverse = TRUE, title="Party"))
    
    if (input$label_barplot==TRUE & input$flip_barplot==FALSE) {
      bar_plot <- bar_plot + geom_text(aes(label = paste0(round(merged_data_bar[,input$variable_barplot],0), "%")),
        position = "stack", vjust=1, size=5)
    }

    if (input$label_barplot==TRUE & input$flip_barplot==TRUE) {
      bar_plot <- bar_plot + geom_text(aes(label = paste0(round(merged_data_bar[,input$variable_barplot],0), "%")),
        position = "stack", hjust=1, size=5) + coord_flip()
    }

    if (input$label_barplot==FALSE & input$flip_barplot==TRUE) {
      bar_plot <- bar_plot + coord_flip()
    }

    bar_plot
  })

  get_summary_bar <- reactive ({
    if (input$variable_barplot=="pty_lhelc_sts_shr") {
      merged_data_bar <- merge(lower_house, view_pty_lh_sts_shr, by="lh_id")
      merged_data_bar <- merge(merged_data_bar, country[c(1,2,3)], by="ctr_id")
      merged_data_bar <- merge(merged_data_bar, party[,c(1,2)], by="pty_id")
      names(merged_data_bar)[names(merged_data_bar)=="lh_sdate"] <- "sdate"
    } else if (input$variable_barplot=="pty_uh_sts_shr") {
      merged_data_bar <- merge(upper_house, view_pty_uh_sts_shr, by="uh_id")
      merged_data_bar <- merge(merged_data_bar, country[c(1,2,3)], by="ctr_id")
      merged_data_bar <- merge(merged_data_bar, party[,c(1,2)], by="pty_id")
      names(merged_data_bar)[names(merged_data_bar)=="uh_sdate"] <- "sdate"
    }

    choices <- list("Lower house seat share" = "pty_lhelc_sts_shr",
      "Upper house seat share" = "pty_uh_sts_shr")

    merged_data_bar <- subset(merged_data_bar,merged_data_bar[,"ctr_ccode"]==input$country)

    min_date <- as.Date(as.character(input$year_range[1]),format="%Y")
    max_date <- as.Date(as.character(input$year_range[2]),format="%Y")

    merged_data_bar$sdate <- as.Date(merged_data_bar$sdate)
    merged_data_bar <- merged_data_bar[merged_data_bar$sdate %in% c(max_date:min_date), ]

    merged_data_bar[,input$variable_barplot] <- as.numeric(as.character(merged_data_bar[,input$variable_barplot]))

    sum_table <- merged_data_bar[c("sdate", "pty_abr", input$variable_barplot)]
    sum_table[,input$variable_barplot] <- round(sum_table[,input$variable_barplot]*100,2)
    sum_table <- sum_table[order(sum_table$sdate, sum_table$pty_abr),]
    colnames(sum_table) <- c("Date", "Party abbreviation", names(choices[c(which(choices==input$variable_barplot)[1])]))

    #due to xtable bug in R, date has to be transformed to character string
    sum_table$Date <- as.character(sum_table$Date)
    sum_table
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
    if (input$variable_veto != "vto_pts") {
      cat("Presence of specific Veto Points. 1 = Open, 0 = Closed")
    } else {
      cat("Summary of specific Veto Points by configuration")
    }
  })

  output$downloadVetoPlot <- downloadHandler(
    filename = function () {
      paste('plot', '.png', sep='')
    },
    content = function (file) {
      plot <- get_plot_veto()
      ggsave(file, plot, width=10, height=10)
    }
  )

  output$downloadVetoTable <- downloadHandler(
    filename = function () {
      paste('table', '.csv', sep='')
    },
    content = function (file) {
      write.table(get_summary_veto(), file, row.names = TRUE)
    }
  )

    output$downloadBivariatePlot <- downloadHandler(
    filename = function () {
      paste('plot', '.png', sep='')
    },
    content = function (file) {
      plot <- get_plot_bivar()
      ggsave(file, plot, width=10, height=10)
    }
  )

  output$downloadBivariateTable <- downloadHandler(
    filename = function () {
      paste('table', '.csv', sep='')
    },
    content = function (file) {
      write.table(get_summary_bivar(), file, row.names = TRUE)
    }
  )

    output$downloadBarPlot <- downloadHandler(
    filename = function () {
      paste('plot', '.png', sep='')
    },
    content = function (file) {
      plot <- get_plot_bar()
      ggsave(file, plot, width=10, height=10)
    }
  )

  output$downloadBarTable <- downloadHandler(
    filename = function () {
      paste('table', '.csv', sep='')
    },
    content = function (file) {
      write.table(get_summary_bar(), file, row.names = TRUE)
    }
  )

})