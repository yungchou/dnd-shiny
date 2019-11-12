function(input, output, session) {

  output$devLocMap<- renderLeaflet({

    leaflet() %>% addTiles() %>%
      setView(-99, 38, 4) %>%
      addMarkers(lng,lat,label=as.character(deviceID),labelOptions=labelOptions(
          noHide = T,textOnly = T,textsize = "16px",offset = c(0, 5)) )
  })

  output$ts <- renderDygraph({

    dygraph(ambient,
            main='Real-Time IoT Stream Data History',
            group=ts) %>%
      dySeries("iot_temp", label='Temperature') %>%
      dySeries("iot_humi", axis='y2', label='Humidity') %>%
      dyAxis("y",  label="Temperature (C)", independentTicks=TRUE) %>%
      dyAxis("y2", label="Humidity (%)") %>%
      dyOptions( fillGraph=TRUE, fillAlpha=0.6) %>%
      dyHighlight(
        highlightCircleSize=5,
        highlightSeriesBackgroundAlpha=0.2,
        hideOnMouseOut=FALSE) %>%
      # dyRoller(rollPeriod=5) %>%
      dyOptions(colors=RColorBrewer::brewer.pal(3, "Set2")) %>%
      dyRangeSelector(height=10) %>% dyUnzoom() %>%
      dyCrosshair(direction="vertical") %>%
      dyLegend(show = "follow")

  })

  output$rp1 <- renderDygraph({

    rp1 <- xts(rp1, order.by = iot_time[1:nrow(rp1)])

    dygraph(rp1,
          main='RP1',
          group=ts) %>%
    dySeries("temperature", label='Temperature') %>%
    dySeries("humidity", axis='y2', label='Humidity') %>%
    dyAxis("y",  label="Temperature (C)", independentTicks=TRUE) %>%
    dyAxis("y2", label="Humidity (%)") %>%
    dyOptions( fillGraph=TRUE, fillAlpha=0.6) %>%
    dyHighlight(
      highlightCircleSize=5,
      highlightSeriesBackgroundAlpha=0.2,
      hideOnMouseOut=FALSE) %>%
    # dyRoller(rollPeriod=5) %>%
    dyOptions(colors=RColorBrewer::brewer.pal(3, "Set2")) %>%
    dyRangeSelector(height=10) %>% dyUnzoom() %>%
    dyCrosshair(direction="vertical") %>%
    dyLegend(show = "follow")

  })

  output$rp2 <- renderDygraph({

    rp2 <- xts(rp2, order.by = iot_time[1:nrow(rp2)])

    dygraph(rp2,
            main='RP2',
            group=ts) %>%
      dySeries("temperature", label='Temperature') %>%
      dySeries("humidity", axis='y2', label='Humidity') %>%
      dyAxis("y",  label="Temperature (C)", independentTicks=TRUE) %>%
      dyAxis("y2", label="Humidity (%)") %>%
      dyOptions( fillGraph=TRUE, fillAlpha=0.6) %>%
      dyHighlight(
        highlightCircleSize=5,
        highlightSeriesBackgroundAlpha=0.2,
        hideOnMouseOut=FALSE) %>%
      # dyRoller(rollPeriod=5) %>%
      dyOptions(colors=RColorBrewer::brewer.pal(3, "Set2")) %>%
      dyRangeSelector(height=10) %>% dyUnzoom() %>%
      dyCrosshair(direction="vertical") %>%
      dyLegend(show = "follow")
  })

  output$rp3 <- renderDygraph({

    rp3 <- xts(rp3, order.by = iot_time[1:nrow(rp3)])

    dygraph(rp3,
            main='RP3',
            group=ts) %>%
      dySeries("temperature", label='Temperature') %>%
      dySeries("humidity", axis='y2', label='Humidity') %>%
      dyAxis("y",  label="Temperature (C)", independentTicks=TRUE) %>%
      dyAxis("y2", label="Humidity (%)") %>%
      dyOptions( fillGraph=TRUE, fillAlpha=0.6) %>%
      dyHighlight(
        highlightCircleSize=5,
        highlightSeriesBackgroundAlpha=0.2,
        hideOnMouseOut=FALSE) %>%
      # dyRoller(rollPeriod=5) %>%
      dyOptions(colors=RColorBrewer::brewer.pal(3, "Set2")) %>%
      dyRangeSelector(height=10) %>% dyUnzoom() %>%
      dyCrosshair(direction="vertical") %>%
      dyLegend(show = "follow")
  })

  output$table <- DT::renderDataTable(df1)

  output$summary <- renderPrint({
    summary(df1)
  })

  output$blog <- renderPrint(a(href="http://commons.wikimedia.org/wiki/User:Sfoskett",
                    "User:Sfoskett"))

  output$more <- renderPlot({

    rp1temp <- rp1$temperature
    rp2temp <- rp2$temperature
    rp3temp <- rp3$temperature
    rp1humi <- rp1$temperature
    rp2humi <- rp2$temperature
    rp3humi <- rp3$temperature

    tempMin <- min(length(rp1temp),length(rp2temp),length(rp3temp))

    dftemp <- melt(data.frame(
      rp1=rp1temp[1:tempMin], rp2=rp2temp[1:tempMin], rp3=rp3temp[1:tempMin]))

    options(repr.plot.width=4, repr.plot.height=2)

    tempDensity <- ggplot(dftemp,aes(x=value, fill=variable)) +
      geom_density(alpha=0.1) + theme_bw()

    dfhumi <- melt(data.frame(
      rp1=rp1humi[1:tempMin], rp2=rp2humi[1:tempMin], rp3=rp3humi[1:tempMin]))
    humiDensity <- ggplot(dfhumi,aes(x=value, fill=variable)) +
      geom_density(alpha=0.1) + theme_bw()

    box <- ggplot(dftemp, aes(y=value,x=variable)) + theme_bw() +
      geom_point(aes(col=variable)) +
      geom_jitter(aes(fill=variable)) +
      geom_boxplot(aes(fill=variable), alpha=0.5)

    grid.arrange(arrangeGrob(tempDensity,humiDensity,box, ncol=3))

  })

}

