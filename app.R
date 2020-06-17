#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(shinyWidgets)
library(tidyverse)
library(leaflet)
library(rgdal)
library(mgcv)
#library(rsconnect)

#setwd("")

df <- readOGR("ohiocounty",layer = "ohio.county") 

county <- read_csv("county_info.csv")

odh_data <- read.csv(url("https://coronavirus.ohio.gov/static/COVIDSummaryData.csv"))
odh_data <- odh_data[-nrow(odh_data),]
names(odh_data)[1] <- "County"
odh_data$Case.Count <- as.numeric(odh_data$Case.Count)

odh_df <- aggregate(Case.Count ~ County + Onset.Date, data = odh_data, sum)

odh_df <- odh_df %>%
  mutate(Onset.Date = as.Date(Onset.Date, format = "%m/%d/%Y")) %>%
  filter(Onset.Date >= as.Date("2020-03-01")) %>%
  rename(Date = Onset.Date) %>%
  complete(Date = seq.Date(min(Date), max(Date), by = "day"),County) %>%
  right_join(county, by = "County")
odh_df[is.na(odh_df)] <- 0

odh_df <- odh_df %>% 
  arrange(Date,County) %>%
  select(County,Date,Case.Count,Zone,Region,Population) %>%
  rename(Daily_new = Case.Count)

county_data <- odh_df %>%
  mutate(State = "OH",
         NAME = County,
         rate = Daily_new/Population)

county_data.zone1 <- county_data %>% 
 filter(Zone %in% 1)
county.region1 <- county_data.zone1 %>%
 filter(Region %in% 1)
county_data.region2 <- county_data.zone1 %>%
 filter(Region %in% 2)
county_data.region5 <- county_data.zone1 %>%
 filter(Region %in% 5)
county_data.zone2 <- county_data %>% 
 filter(Zone %in% 2)
county_data.region4 <- county_data.zone2 %>%
 filter(Region %in% 4)
county_data.region7 <- county_data.zone2 %>%
 filter(Region %in% 7)
county_data.region8 <- county_data.zone2 %>%
 filter(Region %in% 8)
county_data.zone3 <- county_data %>% 
 filter(Zone %in% 3)
county_data.region3 <- county_data.zone3 %>%
 filter(Region %in% 3)
county_data.region6 <- county_data.zone3 %>%
 filter(Region %in% 6)

data <- county_data

##===================================================##

############### insert Patrick's model ################

##===================================================##
construct.focus.dataset <- function(
  data,
  aggregation.variable = "State",
  aggregation.level = "OH",
  focus.variable = "State",
  focus.levels = c("OH"),
  leave.focus.out = TRUE) {
  
  # check for existence of variables in dataset
  if (!(aggregation.variable %in% colnames(data))) {
    stop("Aggregation variable `", aggregation.variable, "`` not in data.")
  }
  if (!(focus.variable %in% colnames(data))) {
    stop("Focus variable `", focus.variable, "` not in data.")
  }
  if (!("Daily_new" %in% colnames(data))) {
    stop("Outcome variable `Daily_new`` not in data.")
  }
  if (!("Population" %in% colnames(data))) {
    stop("Normalization variable `Population`` not in data.")
  }
  
  data.universe <- data %>%
    filter(get(aggregation.variable) == aggregation.level)
  
  if (nrow(data.universe) == 0) {
    stop("No rows matching `",
         aggregation.variable, " == '", aggregation.level,
         "'` in data.")
  }
  
  ### standardization ###
  
  if (leave.focus.out) {
    data.comparator <- data.universe %>%
      filter(!(get(focus.variable) %in% focus.levels))
  } else {
    data.comparator <- data.universe
  }
  
  data.agg.Daily_new <- data.comparator %>%
    group_by(Date) %>%
    summarize(agg_Daily_new = sum(Daily_new)) %>%
    ungroup()
  
  
  agg.Population <- data.comparator %>%
    distinct(County, .keep_all = TRUE) %>%
    pull(Population) %>% sum(na.rm = TRUE)
  
  data.focus <- data.universe %>%
    filter(get(focus.variable) %in% focus.levels) %>%
    group_by(Date) %>%
    summarize(focus_Population = sum(Population, na.rm = TRUE),
              focus_Daily_new = sum(Daily_new)) %>%
    left_join(data.agg.Daily_new, by = "Date") %>%
    mutate(agg_Population = agg.Population,
           Expected = focus_Population * (agg_Daily_new / agg_Population),
           Crude_rr = focus_Daily_new / Expected,
           Rel_Date = as.numeric(Date - min(Date)))
  
  if (nrow(data.focus) == 0) {
    stop("No rows matching `",
         focus.variable, " %in% c('",
         paste(focus.levels, collapse = "', '"),
         "')` in data.")
  }
  
  data.focus
}

test.rr.positive <- function(
  data,
  aggregation.variable = "State",
  aggregation.level = "OH",
  focus.variable = "County",
  focus.levels = c("Franklin"),
  leave.focus.out = TRUE
) {
  
  data.focus <- construct.focus.dataset(
    data = data,
    aggregation.variable = aggregation.variable,
    aggregation.level = aggregation.level,
    focus.variable = focus.variable,
    focus.levels = focus.levels,
    leave.focus.out = leave.focus.out
  )
  
  fit <- tryCatch({
    gam(focus_Daily_new ~ 1 + offset(log(Expected)) + s(Rel_Date),
        family = nb(),
        data = data.focus %>% mutate(
          Expected = if_else(Expected == 0, 1 / 100, Expected)))
  }, error = function(cond) {
    gam(Daily_new ~ 1 + offset(log(Expected)) + Rel.Date,
        family = nb(),
        data = data.focus %>% mutate(
          Expected = if_else(Expected == 0, 1 / 100, Expected)))
  })
  
  newdata <- data.focus
  newdata$Expected[newdata$Expected == 0] <- 1 / 100
  
  pred <- predict(fit, newdata = newdata, type = "link", se.fit = TRUE)
  
  pred$fit <- pred$fit - log(newdata$Expected)
  
  last <- length(pred$fit)
  ci <- exp(pred$fit[last] + qnorm(c(0.025, 0.975)) * pred$se.fit[last])
  prob.rr.gt.1 <- pnorm(pred$fit[last] / pred$se.fit[last])
  
  rel.risk <- c(
    "Estimate" = unname(log2(exp(pred$fit[last]))),
    "2.5%" = log2(ci[1]),
    "97.5%" = log2(ci[2]),
    "Post prob pos" = unname(prob.rr.gt.1)
  )
  
  
  Xp <- predict(fit, newdata = newdata, type = "lpmatrix")
  a <- c(rep(0, nrow(Xp) - 2), c(-1, 1))
  Xs <- t(a) %*% Xp
  fdiff <- drop(Xs %*% coef(fit))
  var.fdiff <- drop(Xs %*% fit$Vp %*% t(Xs))
  se.fdiff <- sqrt(var.fdiff)
  ci <- exp(fdiff + qnorm(c(0.025, 0.975)) * se.fdiff)
  prob.fdiff.gt.1 <- pnorm(fdiff / se.fdiff)
  
  rr.trend <- c(
    "Estimate" = unname(log2(exp(fdiff))),
    "2.5%" = log2(ci[1]),
    "97.5%" = log2(ci[2]),
    "Post prob pos" = unname(prob.fdiff.gt.1)
  )
  
  rbind(
    "Log2 rel risk" = rel.risk,
    "Deriv log2 rel risk" = rr.trend
  )
}


ggplot.local.trend <-
  function(data,
           aggregation.variable = "State",
           aggregation.level = "OH",
           focus.variable = "State",
           focus.levels = c("OH"),
           log.scale = TRUE,
           rel.risk = TRUE,
           leave.focus.out = TRUE,
           title = NULL,
           confint = TRUE) {
    
    data.focus <- construct.focus.dataset(
      data = data,
      aggregation.variable = aggregation.variable,
      aggregation.level = aggregation.level,
      focus.variable = focus.variable,
      focus.levels = focus.levels,
      leave.focus.out = leave.focus.out
    )
    
    ### model fit ###
    
    if (rel.risk) {
      fit <- tryCatch({
        gam(focus_Daily_new ~ 1 + offset(log(Expected)) + s(Rel_Date),
            family = nb(),
            data = data.focus %>% mutate(
              Expected = if_else(Expected == 0, 1 / 100, Expected)))
      }, error = function(cond) {
        gam(Daily_new ~ 1 + offset(log(Expected)) + Rel.Date,
            family = nb(),
            data = data.focus %>% mutate(
              Expected = if_else(Expected == 0, 1 / 100, Expected)))
      })
      
      spred <- predict(fit,
                       newdata = data.focus %>% mutate(
                         Expected = if_else(Expected == 0, 1 / 100, Expected)
                       ),
                       type = "response")
      
      p <- predict(fit,
                   newdata = data.focus %>% mutate(
                     Expected = if_else(Expected == 0, 1 / 100, Expected)
                   ),
                   type = "link",
                   se.fit = TRUE)
      upper <- fit$family$linkinv(p$fit + qnorm(0.975) * p$se.fit)
      lower <- fit$family$linkinv(p$fit + qnorm(0.025) * p$se.fit)
    } else { # raw counts
      fit <- tryCatch({
        gam(focus_Daily_new ~ 1 + s(Rel_Date),
            family = nb(),
            data = data.focus)
      }, error = function(cond) {
        gam(Daily_new ~ 1 + Rel.Date,
            family = nb(),
            data = data.focus)
      })
      
      spred <- predict(fit, type = "response")
      
      p <- predict(fit, type = "link", se.fit = TRUE)
      upper <- fit$family$linkinv(p$fit + qnorm(0.975) * p$se.fit)
      lower <- fit$family$linkinv(p$fit + qnorm(0.025) * p$se.fit)
    }
    
    ### plotting ###
    
    if (is.null(title)) {
      plot.title <- paste0(
        "",
        focus.variable,
        " = ",
        paste(focus.levels, collapse = ", "),
        ifelse(rel.risk,
               paste0(
                 "\nversus ",
                 ifelse(leave.focus.out, "everything else in ", ""),
                 aggregation.variable,
                 " = ",
                 aggregation.level
               ),
               ""
        )
      )
    } else {
      plot.title = title
    }
    
    
    # plotting options
    if (rel.risk) {
      
      plot.data <- data.focus %>% select(Date, focus_Daily_new, Expected, Crude_rr) %>%
        mutate(
          expected.filled = if_else(Expected == 0, 1 / 100, Expected)
        )
      
      plot.data$predicted <- spred
      plot.data$upper <- upper
      plot.data$lower <- lower
      
      transform <- ifelse(log.scale, log2, identity)
      
      p <- ggplot(plot.data, aes(x = Date, y = transform(Crude_rr))) +
        geom_point() +
        geom_line(aes(y = transform(predicted / expected.filled)), color = "dodgerblue4", size = 1.2) +
        xlab("Date") +
        ylab(ifelse(log.scale, "Log2 relative risk", "Relative risk")) +
        ggtitle(plot.title) +
        theme_minimal() + 
        geom_hline(yintercept = transform(1), linetype = "dashed")
      
      if (confint) {
        p <- p + geom_ribbon(aes(ymax = transform(upper / expected.filled),
                                 ymin = transform(lower / expected.filled)),
                             alpha = 0.3,
                             fill = "dodgerblue3")
      }
      
      p
      
    } else { # plotting raw counts
      
      plot.data <- data.focus %>% select(Date, focus_Daily_new, Expected, Crude_rr) %>%
        mutate(
          expected.filled = if_else(Expected == 0, 1 / 100, Expected)
        )
      
      plot.data$predicted <- spred
      plot.data$upper <- upper
      plot.data$lower <- lower
      
      p <- ggplot(plot.data, aes(x = Date, y = focus_Daily_new)) +
        geom_point() +
        geom_line(aes(y = predicted),color = "dodgerblue4", size = 1.2) +
        scale_y_continuous(trans = if_else(log.scale, expr(log10()), expr(identity()))) +
        xlab("Date") +
        ylab(ifelse(log.scale, "Daily new count (log scale)", "Daily new count")) +
        theme_minimal() + 
        ggtitle(plot.title)
      
      if (confint) {
        p <- p + geom_ribbon(aes(ymax = upper,
                                 ymin = lower),
                             alpha = 0.3,
                             fill = "dodgerblue3")
      }
      
      p
    }
  }

screen <- function(
  data,
  aggregation.variable = "State",
  aggregation.level = "OH",
  focus.variable = "County",
  leave.focus.out = TRUE
) {
  focus.levels <- data %>%
    filter(get(aggregation.variable) == aggregation.level) %>%
    pull(get(focus.variable)) %>%
    unique()
  
  summary <- array(NA, dim = c(length(focus.levels), 2, 4))
  dimnames(summary) <- list(focus.variable = focus.levels,
                            "Summary" = c("Log2 rel risk",
                                          "Deriv log2 rel risk"),
                            "Quantity" = c("Estimate",
                                           "2.5%", "97.5%",
                                           "Post prob pos"))
  
  
  
  
  for (focus.level in focus.levels) {
    test <- test.rr.positive(data,
                             aggregation.variable = aggregation.variable,
                             aggregation.level = aggregation.level,
                             focus.variable = focus.variable,
                             focus.levels = focus.level,
                             leave.focus.out = TRUE)
    
    summary[focus.level, , ] <- test
  }
  
  summary
}

test.counties <- screen(data,
                        aggregation.variable = "State",
                        aggregation.level = "OH",
                        focus.variable = "County",
                        leave.focus.out = TRUE)

# flag counties with increasing relative risk
target <- dimnames(test.counties)[[1]][test.counties[, "Deriv log2 rel risk", "Post prob pos"] > 0.95]
target <- union(target, dimnames(test.counties)[[1]][test.counties[, "Log2 rel risk", "Post prob pos"] > 0.95])

county.fills <- data.frame(subregion = tolower(dimnames(test.counties)[[1]]),
                           Flag.Positive = test.counties[, "Log2 rel risk", "Post prob pos"] >= 0.95,
                           Flag.Increasing = test.counties[, "Deriv log2 rel risk", "Post prob pos"] >= 0.95) %>%
  mutate(Flag = factor(Flag.Positive + 2 * Flag.Increasing),
         Flag = recode_factor(
           Flag,
           `0` = "No Alert",
           `1` = "High Relative Risk",
           `2` = "Increasing Relative Risk",
           `3` = "Both"
         ))

ohio.county <- df

mapdata <- as.data.frame(ohio.county) %>% 
  mutate(subregion = tolower(NAME)) %>%
  left_join(county.fills, by = c("subregion"))

ohio.county@data <- mapdata


##############################################################################################################

ui <- fluidPage(
  
  # Application title
  titlePanel("Ohio COVID-19 Surveillance and Screening"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      #pickerInput("county_select","County:",
      #           choices = c(unique(df$County), Zone_1, Zone_2, Zone_3),
      #          multiple = FALSE),
      #selectInput("View", "Counties", choices = unique(data$County), multiple = TRUE),
      #selectInput("AggregateVariable", "Aggregate Area Variable", 
                 # choices = c("region","Zone","State")),
      selectizeInput("AggregateVariable", "Aggregate Variable:", 
                     choices = c("State", "Zone","Region"),
                     selected = "State"),
      
      selectizeInput("AggregateLevel", "Aggregate Level:", 
                     choices = c("OH", "1", "2", "3", "4", "5", "6", "7", "8"),
                     selected = "OH"),
      selectizeInput("View", "County:", 
                     choices =unique(data$County), 
                     selected= "Franklin", multiple = TRUE),
      
      sliderInput("date_select","Date:",
                  min = as.Date("2020-03-01"),
                  max = max(county_data$Date),
                  value = max(county_data$Date),
                  timeFormat = "%m/%d",
                  animate = TRUE),
      hr(),
      checkboxInput("LogScale", "Log Scale", value = F),
      checkboxInput("confint", "Confidence band", value = T),
      checkboxInput("RelRisk", "Relative Risk (RR)", value = F),
      checkboxInput("LeaveFocusOut", "Focus area excluded from RR", value = T),
      hr(),
      span(em(paste0("Last updated: ", max(county_data$Date))))
      
    ),

    
    # Show a plot of the generated distribution
    mainPanel(leafletOutput(outputId = "OHmap"),
              hr(),
              plotOutput(outputId = "modelPlot"))
  ),
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  )
)


server <- function(input, output, session) {
  observe({
    av <- input$AggregateVariable
    if(av == "State"){
      updateSelectizeInput(session, "AggregateLevel",
                           choices = "OH",
                           selected = "OH"
      )
    }
    if(av == "Zone"){
      updateSelectizeInput(session, "AggregateLevel",
                           choices = c("1","2","3"),
                           selected = "1"
      )
    }
    if (av == "Region"){
      updateSelectizeInput(session, "AggregateLevel",
                           choices = c("1", "2", "3", "4", "5", "6", "7", "8"),
                           selected = "4"
      )
    }
    
  })
  
  observe({
    req(input$AggregateVariable)
    req(input$AggregateLevel)
    av <- input$AggregateVariable
    region <- input$AggregateLevel
    if (region == "OH"){
      updateSelectizeInput(session,"View",
                           choices =unique(county_data$County), 
                           selected="Franklin")
    }
    if (av == "Zone" & region == "1"){
      updateSelectizeInput(session,"View",
                           choices =unique(county_data.zone1$County), 
                           selected="Cuyahoga")
    }
    if (av == "Zone" & region == "2"){
      updateSelectizeInput(session,"View",
                           choices =unique(county_data.zone2$County), 
                           selected="Franklin")
    }
    if (av == "Zone" & region == "3"){
      updateSelectizeInput(session,"View",
                           choices =unique(county_data.zone3$County), 
                           selected="Hamilton")
    }
    if (av == "Region" & region == "1"){
      updateSelectizeInput(session,"View",
                           choices =unique(county_data.region1$County), 
                           selected="Lucas")
    }
    if (av == "Region" & region == "2"){
      updateSelectizeInput(session,"View",
                           choices =unique(county_data.region2$County), 
                           selected="Cuyahoga")
    }
    if (av == "Region" & region == "3"){
      updateSelectizeInput(session,"View",
                           choices =unique(county_data.region3$County), 
                           selected="Montgomery")
    }
    if (av == "Region" & region == "4"){
      updateSelectizeInput(session,"View",
                           choices =unique(county_data.region4$County), 
                           selected="Franklin")
    }
    if (av == "Region" & region == "5"){
      updateSelectizeInput(session,"View",
                           choices =unique(county_data.region5$County), 
                           selected="Summit")
    }
    if (av == "Region" & region == "6"){
      updateSelectizeInput(session,"View",
                           choices =unique(county_data.region6$County), 
                           selected="Hamilton")
    }
    if (av == "Region" & region == "7"){
      updateSelectizeInput(session,"View",
                           choices =unique(county_data.region7$County), 
                           selected="Hocking")
    }
    if (av == "Region" & region == "8"){
      updateSelectizeInput(session,"View",
                           choices =unique(county_data.region8$County), 
                           selected="Belmont")
    }
  })
  
  
  # create function with selected counties
  OH_map_db = reactive({
    req(input$View)
    req(input$date_select)
    db = county_data
    db <- db %>% 
      filter(Date %in% input$date_select)
    db_map <- subset(df, NAME %in% db$NAME)
    db_map_df <- db_map@data
    db_map_df <- left_join(db_map_df,db,by="NAME")
    db_map@data <- db_map_df
    db_map
  })

  
  output$OHmap <- renderLeaflet({
    selected_county = county_data %>%
      filter(County %in% input$View)
    bins <- c(0,1,10,20,30,40,50,3000)
    legend <- c("0","1-10","10-20","20-30","30-40","40-50",">50")
    # label format
    mylabelFormat <-function(
      prefix = "", suffix = "", between = " &ndash; ", digits = 3, big.mark = ",",
      transform = identity
    ) {
      
      formatNum <- function(x) {
        format(
          round(transform(x), digits), trim = TRUE, scientific = FALSE,
          big.mark = big.mark
        )
      }
      
      function(type, ...) {
        switch(
          type,
          numeric = (function(cuts) {
            paste0(prefix, formatNum(cuts), suffix)
          })(...), # nolint
          bin = (function(cuts) {
            n <- length(cuts)
            prefix
          })(...), # nolint
          quantile = (function(cuts, p) {
            n <- length(cuts)
            p <- paste0(round(p * 100), "%")
            cuts <- paste0(formatNum(cuts[-n]), between, formatNum(cuts[-1]))
            # mouse over the legend labels to see the values (quantiles)
            paste0(
              "<span title=\"", cuts, "\">", prefix, p[-n], between, p[-1], suffix,
              "</span>"
            )
          })(...), # nolint
          factor = (function(cuts) {
            paste0(prefix, as.character(transform(cuts)), suffix)
          })(...) # nolint
        )
      }
      
    }
    
    ## leaflet
    ## define color 
    ## for zip code surveillance
    pal <- colorBin(palette = "Blues", bins=bins)
    ## for screening
    pal2 <- colorFactor(palette = c('white','yellow','orange','red'),ohio.county$Flag)
    
    # zoom level
    if (length(unique(OH_map_db()$County)) == 1){zoom = 10} else{zoom = 7.5}

    leaflet() %>%
      #st_transform(crs = "+init=epsg:4326") %>%
      addProviderTiles("CartoDB.Positron", options= providerTileOptions(opacity = 0.99)) %>%
      addPolygons(data = OH_map_db(),
                  popup = ~ paste("Zone:", Zone, "<br>","Region:", Region, "<br>","County:", County, "<br>", "New Cases:", Daily_new),
                  stroke = TRUE,
                  smoothFactor = 0,
                  weight = 1,
                  color = "#444444",
                  fillColor = ~ pal(OH_map_db()$Daily_new),
                  #opacity = 0.5,
                  group = "Daily Cases",
                  fillOpacity = 0.8
      ) %>%
      addLegend("bottomright", 
                pal = pal, 
                values = c(0,max(selected_county$Daily_new)),
                title = "New Cases",
                opacity = 1,
                labFormat = mylabelFormat(legend),
                group = "Daily Cases") %>%
      addPolygons(data = ohio.county,
                  popup = ~ paste(str_extract(NAME, "^([^,]*)"),"<br>", "Status:", Flag),
                  stroke = TRUE,
                  smoothFactor = 0,
                  weight = 1,
                  color = "#444444",
                  fillColor = ~ pal2(Flag),
                  #opacity = 0.5,
                  group = "Potential Alerts",
                  fillOpacity = 0.6) %>% 
      # the opacity feature is not working and idk why
      addLegend("bottomright", 
                pal = pal2, 
                values = ohio.county$Flag,
                title = "Category",
                opacity = 1,
                group = "Potential Alerts") %>%
      addLayersControl(overlayGroups = c("Potential Alerts","Daily Cases"),
                       options = layersControlOptions((collapsed = FALSE))) %>%
      hideGroup("Potential Alerts") #%>%
      #setView(lng = gCentroid(OH_map_db())$x, lat = gCentroid(OH_map_db())$y, zoom = zoom)
    

    #ggplot(OH_map_db(), aes(fill = Count)) +
    #      geom_sf()+
    #      scale_fill_distiller(name="Daily New Cases",
    #                           limits=c(0,max(selected_county$Daily_new)),
    #                           palette='Reds',
    #                           direction=1)
  })
  
  output$modelPlot = renderPlot({
    req(input$AggregateVariable)
    req(input$AggregateLevel)
    ggplot.local.trend(data,
                       #aggregation.variable = "State",
                       #aggregation.level = "OH",
                       aggregation.variable = input$AggregateVariable,
                       aggregation.level = input$AggregateLevel,
                       focus.variable = "County",
                       focus.levels = input$View,
                       log.scale = as.logical(input$LogScale),
                       rel.risk = as.logical(input$RelRisk),
                       leave.focus.out = as.logical(input$LeaveFocusOut),
                       confint = as.logical(input$confint))
  })

  
  #output$CensusPlot <- renderPlot({
  #  ggplot(census_plot_db(), aes(x=Date,y=Count,group=1))+
  #    geom_line()+
  #    geom_point()
  #})
}

# Run the application 
shinyApp(ui = ui, server = server)


