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

source("./utility.R")
odh_data <- get_merged_ODRS_region(
  data_url = "https://coronavirus.ohio.gov/static/COVIDSummaryData.csv",
  backup_data_file = NULL,
  last_update_date = "auto"
)

odh_df <- agg_daily_counts_by_cols(
  data = odh_data,
  group_col = "County",
  date_col = "Onset Date",
  last_update_date = "auto",
  evenly_space = TRUE
)

df <- readOGR("ohiocounty",layer = "ohio.county") 

county <- read_csv("county_info.csv")

names(odh_df) <- c("Onset.Date","County","Case.Count")
odh_df <- odh_df %>%
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

data <- county_data

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
      sliderInput("date_select","Date:",
                  min = as.Date("2020-03-01"),
                  max = max(county_data$Date),
                  value = max(county_data$Date),
                  timeFormat = "%m/%d",
                  animate = TRUE),
      hr(),
      selectizeInput("AggregateVariable", "Aggregate Variable:", 
                     choices = c("State", "Zone","Region"),
                     selected = "State"),
      
      conditionalPanel(
        condition = "input.AggregateVariable == 'State'",
        selectizeInput(
          inputId = "AggregateLevel_state",
          label = "Aggregate Level",
          choices = c("Ohio" = "OH"),
          selected = c("Ohio" = "OH")
        )
      ),
      
      conditionalPanel(
        condition = "input.AggregateVariable == 'Zone'",
        selectizeInput(
          inputId = "AggregateLevel_zone",
          label = "Aggregate Level",
          choices = c("1","2","3"),
          selected = "1"
        )
      ),
      
      conditionalPanel(
        condition = "input.AggregateVariable == 'Region'",
        selectizeInput(
          inputId = "AggregateLevel_region",
          label = "Aggregate Level",
          choices = c("1","2","3","4","5","6","7","8"),
          selected = "1"
        )
      ),
      
      fluidRow(
        column(
          width = 10,
          offset = 0,
          # style = "padding: 0px",
          selectizeInput(
            inputId = "View",
            label = "Select County(ies)",
            choices = unique(data$County),
            selected = "Franklin",
            multiple = TRUE,
            options = list(
              hideSelected = FALSE
            )
          ),
        ),
        column(
          width = 2,
          offset = 0,
          style = "margin-top: 27px; padding: 0px",
          actionBttn(
            inputId = "run_RR",
            label = NULL,
            icon = icon("arrow-alt-circle-right"),
            style = "unite",
            color = "primary",
            block = FALSE,
            no_outline = TRUE,
            size = "xs"
          )
        )
      ),
      

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
  
  # ------- Diable the run button when there is no values specified in regions/counties
  observe({
    shinyjs::enable(id = "run_RR")
    if (is.null(input$View)) {
      shinyjs::disable(id = "run_RR")
    } else {
      shinyjs::enable(id = "run_RR")
    }
  })
  
  
  OH_map_db = reactive({
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
    bins <- c(0,1,10,20,30,40,50,3000)
    legend <- c("0","1-10","10-20","20-30","30-40","40-50",">50")
    
    ## leaflet
    ## define color 
    ## for zip code surveillance
    pal <- colorBin(palette = "Blues", bins=bins)
    ## for screening
    pal2 <- colorFactor(palette = c('white','yellow','orange','red'),ohio.county$Flag)
    
    # zoom level
    #if (length(unique(OH_map_db()$County)) == 1){zoom = 10} else{zoom = 7.5}
    
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
                values = c(0,3000),
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
                  fillOpacity = 0.9) %>% 
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
  })
  
  
  # ----- Region number or County names depend on "run" button
  aggregation_variable <- eventReactive(input$run_RR + 1, {
    input$AggregateVariable
  })
  
  aggregation_level_state <- eventReactive(input$run_RR + 1, {
    readr::parse_character(input$AggregateLevel_state)
  })
  
  aggregation_level_zone <- eventReactive(input$run_RR + 1, {
    as.numeric(input$AggregateLevel_zone)
  })
  
  aggregation_level_region <- eventReactive(input$run_RR + 1, {
    as.numeric(input$AggregateLevel_region)
  })
  
  focus_county_level <- eventReactive(input$run_RR + 1, {
    input$View
  })
  
  
  output$modelPlot = renderPlot({
    av <- aggregation_variable()
    if (av == "State") {
      region <- aggregation_level_state()
    } else if (av == "Zone") {
      region <- aggregation_level_zone()
    } else if (av == "Region") {
      region <- aggregation_level_region()
    }
    
    ggplot.local.trend(data,
                       #aggregation.variable = "State",
                       #aggregation.level = "OH",
                       aggregation.variable = av,
                       aggregation.level = region,
                       focus.variable = "County",
                       focus.levels = focus_county_level(),
                       log.scale = as.logical(input$LogScale),
                       rel.risk = as.logical(input$RelRisk),
                       leave.focus.out = as.logical(input$LeaveFocusOut),
                       confint = as.logical(input$confint))
  })
  
  
  reactive_zone <- eventReactive(input$AggregateLevel_zone, {
    zone_num <- as.numeric(input$AggregateLevel_zone)
    county_df <- county_data %>% 
      filter(Zone %in% zone_num)
    subset(df,NAME %in% county_df$NAME)
  })
  reactive_region <- eventReactive(input$AggregateLevel_region, {
    region_num <- as.numeric(input$AggregateLevel_region)
    county_df <- county_data %>% 
      filter(Region %in% region_num)
    subset(df,NAME %in% county_df$NAME)
  })
  
  
  observe({
    av <- input$AggregateVariable
    if (av == "State"){
      updateSelectizeInput(session,"View",
                           choices =unique(county_data$County), 
                           selected="Franklin") 
      leafletProxy("OHmap")%>% 
        clearGroup("highlighted_polygon")
    }
    else if (av == "Zone"){

      #zone_num <- as.numeric(input$AggregateLevel)
      #county_df <- county_data %>% 
      #  filter(Zone %in% zone_num)
      #ohio_RR_map_sf_highligh <- subset(df, NAME %in% county_df$NAME)
      
      leafletProxy("OHmap") %>% 
        clearGroup("highlighted_polygon") %>%
        addPolylines(data = reactive_zone(),
                     stroke=TRUE, 
                     weight = 2,
                     color="black",
                     group="highlighted_polygon")
      updateSelectizeInput(session,"View",
                           choices =unique(reactive_zone()$NAME), 
                           selected=reactive_zone()$NAME[1])   
      }
    
    else if (av == "Region"){
      #region_num <- as.numeric(input$AggregateLevel)
      #county_df <- county_data %>%
      #  filter(Region %in% region_num)
      #ohio_RR_map_sf_highligh <- subset(df, NAME %in% county_df$NAME)
      
      leafletProxy("OHmap") %>% 
        clearGroup("highlighted_polygon") %>%
        addPolylines(data = reactive_region(),
                     stroke=TRUE, 
                     weight = 2,
                     color="black",
                     group="highlighted_polygon")
      updateSelectizeInput(session,"View",
                           choices =unique(reactive_region()$NAME), 
                           selected=reactive_region()$NAME[1])
    }
  })
    
}
# Run the application 
shinyApp(ui = ui, server = server)


