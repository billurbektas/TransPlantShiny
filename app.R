# Shiny libraries
library(shiny)  
library(bs4Dash)
library(tidyverse)  
library(shinythemes)
library(shinyjs)
library(shinycssloaders)
library(shinymanager)
library(keyring)
# To build maps
library(leaflet)
library(leaflet.extras)
# To build interactive plots
library(ggplot2)
library(plotly)
library(colorspace)
library(ggpubr)

# TO DO ####
# That question mark on the navbar
# Add collaborators list

options(spinner.color="#dc3545", spinner.color.background="#f4f6f9ff", spinner.size=2)
tsize = 15 #textsize
## Load data ---- 
exptab = readRDS("data/exptab.rds")
dfs = readRDS("data/dfs.rds")
pools = readRDS("data/pools.rds")
slopes = readRDS("data/slopes.rds")
pred = readRDS("data/pred.rds")
PRC = readRDS("data/PRC.rds")
sp.scores = readRDS("data/sp.scores.rds")
sp.pred = readRDS("data/sp.pred.rds")
pvar = readRDS("data/pvar.rds")

## Load functions ----
source("functions.R")
## User interface ----
ui <- bs4DashPage(
  
  bs4DashNavbar(title = "TransPlant", 
                status = "danger", 
                skin = "light", 
                compact = TRUE,
                fixed = TRUE,
                controlbarIcon = NULL),
  bs4DashSidebar(
    status = "danger",
    collapsed = FALSE,
    minified = FALSE,
    expandOnHover = FALSE,
    bs4SidebarMenu(
      bs4Dash::menuItem("Overview", tabName = "overview", icon = icon("house")),
      bs4Dash::menuItem("Experiments", tabName = "experiments", icon = icon("mountain")),
      bs4Dash::menuItem("Data", tabName = "data", icon = icon("database")),
      bs4Dash::menuItem("Analyses", icon = icon("magnifying-glass-chart"), 
               menuSubItem("Taxonomic changes", tabName = "convergence"),
               menuSubItem("Winners & losers", tabName = "winnerloser"),
               menuSubItem("Functional changes", tabName = "traitAbundance")
               
      )
    )
  ),
  bs4DashBody(
    tabItems(
      tabItem(tabName = "overview",
              bs4Card(
                width = 12,
                title = "The TransPlant Network", 
                status = "danger", 
                collapsible = TRUE,
                collapsed = FALSE,
                p("TransPlant is a network of 22 elevation gradients, distributed across 20 different mountainous regions of the Northern Hemisphere, where whole plant communities have been transplanted to lower elevations to simulate climate change. The basic design of the transplant experiments involve the transplantation of vegetation turfs (or monoliths) of whole plant communities with their soil from a higher (cooler) to a lower (warmer) elevation site to simulate climate warming (“warmed” treatment). Moving plant communities to lower elevation in this way allows plants from the lower elevation site to quickly colonise transplanted turfs, also simulating the arrival of novel species into high elevation communities that we expect with climate warming. For any given set of warmed turfs, there are “origin controls” (high-elevation turfs locally transplanted at their high-elevation site of origin) and “destination controls” (low-elevation turfs locally transplanted at the low elevation site of destination). Thus, these controls make it possible to disentangle potential transplantation artefacts from the effects of experimental climate change. Along some TransPlant gradients multiple experiments are conducted by transplanting the turfs from high to mid, high to low and mid to low elevations. We refer to a set of warmed, origin and destination control turfs within each gradient as an experiment, yielding a total of 40 experiments because 12 gradients were composed of more than two sites. The experiments were established between 2007 and 2017. Each experiment had 3-10 replicates of transplant and control turfs, and in total there were 668 turfs varying in sizes between 0.02 m2 and 1 m2 with a mean size of 0.3 m2 across the 40 experiments.")
                ),
              bs4Card(
                width = 12,
                title = "Experimental Setup", 
                status = "danger", 
                collapsible = TRUE,
                collapsed = FALSE,
                img(src = "experiment_main.jpg", height = "500px", width = "auto", alt = "Experimental Setup"),
                p("Experimental design of the TransPlant network and the ecological processes simulated with whole-community transplant experiments. A) The basic design of the transplant experiments. Each TransPlant gradient comprises at least one pair of sites at high and lower elevation, which we refer to as an “experiment”. For simplicity, on the figure a single pair of origin and destination sites are shown as an experiment. B) Transplant experiments aim to simulate climate change and colonisation from low elevation communities (colonising species) by transplanting whole plant communities to putative future climates at low elevation. This approach also lifts the dispersal barrier for colonising species. We refer to the turfs transplanted from origin to destination sites as “warmed” communities (red circle), and to turfs locally replanted at origin and destination sites as origin (blue circle) and destination (orange circle) controls, respectively.")
                ),
              bs4Card(
                  width = 12,
                  title = "Protocols", 
                  status = "danger", 
                  collapsible = TRUE,
                  collapsed = TRUE,
                  p("")
                ),
              bs4Card(
                  width = 12,
                  title = "Collaborators", 
                  status = "danger", 
                  collapsible = TRUE,
                  collapsed = TRUE,
                  p("")
                )
                ),
      
      tabItem(tabName = "experiments",
              fluidRow(
                column(width = 3,
                       uiOutput("filter_box")),  # Dynamic filtering options
                column(width = 9,
                       leafletOutput("map", height = "800px"))
              )),
      tabItem(tabName = "data",
              tabsetPanel(type = "tabs",
                          tabPanel("By experiment",
                                   fluidRow(
                                     column(width = 3, 
                                            selectInput("selectedGradient", "Choose Gradient:", choices = sort(unique(dfs$Gradient))),
                                            uiOutput("experimentID_ui"),
                                            selectInput("selectedPool", "Choose Pool:", choices = unique(pools$pool)),
                                            uiOutput("SpeciesNameID_ui"),
                                            selectInput("selectedMethod", "Choose smoothing method:", choices = c("Linear regression", "Locally estimated scatterplot smoothing"))
                                            
                                     ),
                                     column(width = 7, 
                                            plotlyOutput("experimentPlot", height = "500px")
                                     )
                                   )
                          ),
                          tabPanel("By species",
                                   fluidRow(
                                     column(width = 3,
                                            selectInput("selectedSpeciesNameID2", "Choose species:", choices = sort(unique(dfs$SpeciesName))),
                                            uiOutput("gradientID2_ui"),
                                            uiOutput("experimentID2_ui"),
                                            selectInput("selectedMethod2", "Choose smoothing method:", choices = c("Linear regression", "Locally estimated scatterplot smoothing"))
                                            
                                     ),
                                     column(width = 9, 
                                            plotlyOutput("speciesPlot", height = "500px") # Placeholder for the plot output
                                     )
                                   )
                          ),
                          tabPanel("By pool",
                                   fluidRow(
                                     column(width = 3,
                                            selectInput("selectedPool3", "Choose Pool:", choices = unique(pools$pool)),
                                            uiOutput("gradientID3_ui"),
                                            uiOutput("experimentID3_ui"),
                                            selectInput("selectedMethod3", "Choose smoothing method:", choices = c("Linear regression", "Locally estimated scatterplot smoothing"))
                                     ),
                                     column(width = 9, 
                                            plotlyOutput("poolsPlot",height = "500px") # Placeholder for the plot output
                                     )
                                   )
                          )
              )
      ),
      tabItem(tabName = "convergence",
              tabsetPanel(type = "tabs",
                          tabPanel("Principal response curves",
                                   fluidRow(
                                     column(width = 3,
                                            selectInput("selectedGradientPRC", "Choose Gradient:", choices = sort(unique(dfs$Gradient))),
                                            uiOutput("experimentPRC_ID_ui"),
                                            selectInput("selectedAxis", "Choose PRC axis:", choices = c("RDA1","RDA2")),
                                            sliderInput("selectedWei", "Species weights:",
                                                        min = 0, max = 0.9,
                                                        value = c(0.2), step = 0.1),
                                            
                                            
                                            
                                     ),
                                     column(width = 9, 
                                            plotlyOutput("prcPlot", height = "500px"),
                                            plotlyOutput("prcSP", height = "500px")
                                     )
                                   )
                          ),
                          tabPanel("Rates of community changes",
                                     column(width = 9, 
                                           withSpinner(plotlyOutput("ratesPlot", height = "700px"), type = 8),
                                           tags$p("Each panel reflects both divergence of the warmed community from the origin control (dotted lines) and convergence towards the destination control (dashed lines). For divergence, the origin controls are set at zero as the reference community and the plot shows the divergence of warmed communities (dotted lines) and destination controls (solid line) from the origin controls. For convergence, the destination controls are set at zero and the plot shows the convergence of the warmed communities (dashed line) and destination controls (solid line) towards the destination controls. Distances between controls (solid line) represent the expected taxonomic distance if warmed communities fully converge (i.e., become identical to the destination community composition).", style = "text-align: justify; margin-top: 14px;")
                                           )
                                   
                          ),
                          tabPanel("Rates of species pool changes",
                                     column(width = 11, 
                                            withSpinner(plotlyOutput("spratesPlot", height = "700px"), type = 8),
                                            tags$p("Proportional differences in relative abundances of each species pool in warmed communities in comparison to origin controls (dotted lines) and destination controls (dashed lines) calculated from associated PRC axis. Positive values indicate higher relative abundances in warmed communities, negative values indicate lower relative abundances in warmed communities in comparison to controls.", style = "text-align: justify; margin-top: 14px;")
                                     )
                                   
                          ),
                          tabPanel("Experimental variation",
                                   fluidRow(
                                     column(width = 3,
                                            selectInput("selectedLevel", "Choose level:", choices = unique(pvar$pool)),
                                            selectInput("selectedType", "Choose response variable:", choices = unique(pvar$type)),
                                            selectInput("selectedVariable", "Choose explanatory variable", choices = unique(pvar$variables), multiple = TRUE)
                                     ),
                                     column(width = 11, 
                                            plotlyOutput("expvarPlot",height = "700px") # Placeholder for the plot output
                                     )
                                   )
                                   
                          )
              )),
      tabItem(tabName = "traitAbundance",
              h2("Coming soon!")),
      tabItem(tabName = "winnerloser",
              h2("Coming soon!"))
    )
  )
)

# SERVER ----
server <- function(input, output, session) {
  res_auth <- secure_server(
    check_credentials = check_credentials("cre/database.sqlite",
                                          passphrase = "loremipsum")
  )
  #### INTERACTIVE MAP OF THE OVERVIEW TAB-----
  # Dynamically generate filtering options
  output$filter_box <- renderUI({
    tagList(
      selectInput("selected_gradients", "Choose Gradients:",
                  choices = unique(exptab$Gradient),
                  selected = unique(exptab$Gradient),
                  multiple = TRUE),
      radioButtons("site_type", "Select Site Type:",
                   choices = c("All", "Destination", "Origin"),
                   selected = "All"),
      sliderInput("year_range", "Experiment duration:",
                  min = min(exptab$`Experimental duration`), max = max(exptab$`Experimental duration`),
                  value = c(min(exptab$`Experimental duration`), max(exptab$`Experimental duration`))),
      sliderInput("experimental_warming", "Experimental Warming (°C):",
                  step = 0.5,
                  min = 0, 
                  max = round(max(exptab$`Experimental warming (°C)`)),
                  value = c(0.5, 11.5))
    )
  })
  
  # Reactive expression for filtered data
  filteredData <- reactive({
    req(input$selected_gradients, input$site_type, input$year_range, input$experimental_warming)
    
    data <- exptab %>%
      # filter((Gradient %in% input$selected_gradients)|
      #          (`Experimental duration` >= input$year_range[1] & `Experimental duration` <= input$year_range[2])|
      #          (`Experimental warming (°C)` >= input$experimental_warming[1]& `Experimental warming (°C)` <= input$experimental_warming[2])
      # )
     filter(Gradient %in% input$selected_gradients) %>%
     filter(`Experimental duration` >= input$year_range[1], `Experimental duration` <= input$year_range[2]) %>%
     filter(`Experimental warming (°C)` >= input$experimental_warming[1], `Experimental warming (°C)` <= input$experimental_warming[2])
     
    data
  })
  
  
  output$map <- renderLeaflet({
    leaflet() %>%  addProviderTiles(providers$Stadia.StamenTerrain) # Initialize map with base tiles
  })
  
  observe({
    df <- req(filteredData())
    
    # Clear existing markers and shapes to reset the map
    leafletProxy("map") %>%
      clearMarkers() %>%
      clearShapes()
    
    # When "All" is selected, add both destination and origin markers, and draw arrows
    # Inside your observe event for drawing on the map
    if (input$site_type == "All") {
      for(i in 1:nrow(df)) {
        origin_lng <- df$`Origin Longitude`[i]
        origin_lat <- df$`Origin Latitude`[i]
        dest_lng <- df$`Destination Longitude`[i]
        dest_lat <- df$`Destination Latitude`[i]
        
        # Adding the main line from origin to destination
        leafletProxy("map") %>%
          addPolylines(
            lng = c(origin_lng, dest_lng),
            lat = c(origin_lat, dest_lat),
            color = "black", weight = 5, opacity = 0.7
          )
        
        # Calculating and adding the arrow sides
        arrowhead_points <- get_arrowhead(origin_lng, origin_lat, dest_lng, dest_lat, 
                                          length = 0.1, angle = 20)
        leafletProxy("map") %>%
          addPolylines(
            lng = c(dest_lng, arrowhead_points$arrowPoint1$lng),
            lat = c(dest_lat, arrowhead_points$arrowPoint1$lat),
            color = "black", weight = 5, opacity = 1
          ) %>%
          addPolylines(
            lng = c(dest_lng, arrowhead_points$arrowPoint2$lng),
            lat = c(dest_lat, arrowhead_points$arrowPoint2$lat),
            color = "black", weight = 5, opacity = 1
          )
      }
    }
    
    
    
    # Add destination markers
    if("Destination" %in% input$site_type || input$site_type == "All") {
      leafletProxy("map", data = df) %>%
        addCircleMarkers(
          lng = ~`Destination Longitude`,
          lat = ~`Destination Latitude`,
          color = "#fe7e00", fillColor = "#fe7e00", fillOpacity = 0.5,
          popup = ~paste("Destination Site<br>",
                         "Gradient:", Gradient, "<br>",
                         "Site ID:", destSiteID, "<br>",
                         "Longitude:", `Destination Longitude`, "<br>",
                         "Latitude:", `Destination Latitude`,"<br>",
                         "Elevation:", `Destination elevation`, "m<br>",
                         "Experimental Warming:", `Experimental warming (°C)`, "°C<br>",
                         "Turf size:",`Turf size (m)`, "m²<br>",
                         "Number of replicates:", `Number of replicates`, "<br>",
                         "Site temperature:", `Destination temperature(°C)`, "°C<br>",
                         "Site PET:", `Destination PET(mm)`, "mm"
                         ),
          group = "Destinations"
        )
    }
    
    # Add origin markers
    if("Origin" %in% input$site_type || input$site_type == "All") {
      leafletProxy("map", data = df) %>%
        addCircleMarkers(
          lng = ~`Origin Longitude`,
          lat = ~`Origin Latitude`,
          color = "#4285f4", fillColor = "#4285f4", fillOpacity = 0.5,
          popup = ~paste("Destination Site<br>",
                         "Gradient:", Gradient, "<br>",
                         "Site ID:", destSiteID, "<br>",
                         "Longitude:", `Destination Longitude`, "<br>",
                         "Latitude:", `Destination Latitude`,"<br>",
                         "Elevation:", `Destination elevation`, "m<br>",
                         "Turf size:",`Turf size (m)`, "m²<br>",
                         "Number of replicates:", `Number of replicates`, "<br>",
                         "Site temperature:", `Destination temperature(°C)`, "°C<br>",
                         "Site PET:", `Destination PET(mm)`, "mm"),
          group = "Origins"
        )
    }
  })
  #### INTERACTIVE PLOT OF THE DATA TAB ----
  ##### PLOT: By Experiment ----
  # Dynamic UI for originSiteID based on selected Gradient
  output$experimentID_ui <- renderUI({
    selected_gradient <- input$selectedGradient
    if (!is.null(selected_gradient)) {
      experiments <- dfs %>% filter(Gradient == selected_gradient) %>% 
        dplyr::select(experiment) %>% distinct() %>% arrange(experiment) %>% pull(experiment)
      selectInput("selectedExperimentID", "Choose Experiment:", choices = experiments)
    }
  })
  
  output$SpeciesNameID_ui <- renderUI({
    selected_gradient <- input$selectedGradient
    selectedExperimentID <-  input$selectedExperimentID
    selectedPool <-  input$selectedPool
    if (!is.null(selected_gradient) & !is.null(selectedExperimentID) & !is.null(selectedPool)) {
      species <- dfs %>% 
        filter(Gradient == selected_gradient) %>% 
        filter(experiment == selectedExperimentID) %>%
        filter(pool %in% selectedPool) %>%
        dplyr::select(SpeciesName) %>% 
        distinct() %>% 
        arrange(SpeciesName) %>%
        pull(SpeciesName)
      selectInput("selectedSpeciesNameID", "Choose species:", choices = species, multiple = TRUE, selected = unique(species)[1])
    }
  })
  
  # Interactive plot based on selected filters
  output$experimentPlot <- renderPlotly({
    req(input$selectedGradient, input$selectedExperimentID, input$selectedPool,input$selectedSpeciesNameID, input$selectedMethod)
    selectedMethod = ifelse(input$selectedMethod == "Linear regression","lm","loess")
    
    filteredData <- dfs %>%
      filter(Gradient == input$selectedGradient, 
             experiment == input$selectedExperimentID)%>%
      filter(pool %in% input$selectedPool)%>%
      filter(SpeciesName %in% input$selectedSpeciesNameID)%>%
      mutate(ODT = recode(ODT, destControls = "Destination control",
                          originControls = "Origin control",
                          warmed = "Warmed"))%>%
      mutate(ODT = factor(ODT, levels = c("Origin control","Destination control","Warmed")))%>%
      mutate(Year = Year-`Transplantation year`)
    
     ODTcol  = c("#4285f4", "#fe7e00", "#ff0000")
     names(ODTcol) = c("Origin control","Destination control","Warmed")
     
    p <- ggplot(filteredData, aes(x = Year, y = Rel_Cover, color = ODT)) +
      theme_bw()+
      geom_line(aes(group = interaction(SpeciesName, ODT, destPlotID)), alpha = 0.2) +
      stat_smooth(method = selectedMethod, aes(group = interaction(SpeciesName, ODT)), se = FALSE)+
      labs(title = "Relative Cover of Species Over Years", color = "Treatments", x = "Experimental years", y = "Relative Cover")+
      scale_color_manual(values = ODTcol)+
      theme(
        text=element_text(size=tsize),
        panel.background = element_rect(fill = "#f4f6f9ff", color = "#f4f6f9ff"),
        panel.grid.major = element_line(color = "#f4f6f9ff"), 
        panel.grid.minor = element_line(color = "#f4f6f9ff"),
        plot.background = element_rect(fill = "#f4f6f9ff"),
        legend.background = element_rect(fill = "#f4f6f9ff")
      )+
      scale_x_continuous(breaks = seq(0,9,1))
    
    ggplotly(p, tooltip = "all")
  })
  
  ##### PLOT: By Species ----
  
  # Dynamic UI for originSiteID based on selected Gradient
  
  output$gradientID2_ui <- renderUI({
    selectedsp <-  input$selectedSpeciesNameID2
    if (!is.null(selectedsp)) {
      gradients <- dfs %>% 
        filter(SpeciesName == selectedsp) %>% 
        dplyr::select(Gradient) %>% distinct() %>% pull(Gradient)
      selectInput("selectedGradient2", "Choose Gradient:", choices = c("All", gradients), multiple = TRUE, selected = "All")
    }
  })
  
  observeEvent(input$selectedGradient2, {
    selectedGradient <- input$selectedGradient2
    # If "All" is selected along with other gradients, remove "All" from the selection
    if("All" %in% selectedGradient && length(selectedGradient) > 1) {
      selectedGradient <- selectedGradient[selectedGradient != "All"]
      updateSelectInput(session, "selectedGradient2", selected = selectedGradient)
    }
  })

  output$experimentID2_ui <- renderUI({
    selectedGradient <- input$selectedGradient2
    
    if (length(selectedGradient) == 1 && selectedGradient == "All") {
      experiments <- c("All")
    } else {
      experiments <- dfs %>%
        filter(Gradient %in% selectedGradient) %>%
        dplyr::select(experiment) %>% 
        distinct() %>%
        pull(experiment)
      experiments <- c("All",experiments)
    }
    
    selectInput("selectedExperimentID2", "Choose Experiment:", choices = experiments, selected = "")
  })

  # Interactive plot based on selected filters
  output$speciesPlot <- renderPlotly({
    req(input$selectedGradient2, input$selectedExperimentID2, input$selectedSpeciesNameID2, input$selectedMethod2)
    selectedMethod = ifelse(input$selectedMethod2 == "Linear regression","lm","loess")
    
    if("All" %in% input$selectedGradient2 &  "All" %in% input$selectedExperimentID2){
      filteredData <- dfs %>%
        filter(SpeciesName %in% input$selectedSpeciesNameID2)
    }
    if(!("All" %in% input$selectedGradient2) &  "All" %in% input$selectedExperimentID2){
      filteredData <- dfs %>%
        filter(SpeciesName %in% input$selectedSpeciesNameID2)%>%
        filter(Gradient %in% input$selectedGradient2)
    }
    if(!("All" %in% input$selectedGradient2) & !( "All" %in% input$selectedExperimentID2)){
      filteredData <- dfs %>%
        filter(SpeciesName %in% input$selectedSpeciesNameID2)%>%
        filter(Gradient %in% input$selectedGradient2)%>%
        filter(experiment %in% input$selectedExperimentID2)
    }
    filteredData =
      filteredData %>%
      mutate(ODT = recode(ODT, destControls = "Destination control",
                          originControls = "Origin control",
                          warmed = "Warmed"))%>%
      mutate(ODT = factor(ODT, levels = c("Origin control","Warmed","Destination control")))%>%
      mutate(Year = Year-`Transplantation year`)
    
    set.seed(123)
    expcol  = sample(sequential_hcl(n = 40, palette = "Hawaii"), 40)
    names(expcol) = unique(paste0(dfs$Gradient,".", dfs$experiment))
    
    #if("All" %in% input$selectedGradient2 &  "All" %in% input$selectedExperimentID2){
      p <- ggplot(filteredData, aes(x = Year, y = Rel_Cover, color = interaction(Gradient, experiment))) +
        theme_bw()+
        facet_grid(.~ODT)+
        stat_smooth(method = selectedMethod, aes(group = interaction(Gradient, experiment, ODT)), se = FALSE, alpha = 0.5, linewidth = 0.5)+
        stat_smooth(method = selectedMethod, aes(group = interaction(ODT)), se = FALSE, color = "grey20", linetype = "dotted")+
        labs(title = "Relative Cover of Species Over Years", color = "Experiments", x = "Experimental years", y = "Relative Cover")+
        theme(
          text=element_text(size=tsize),
          panel.background = element_rect(fill = "#f4f6f9ff", color = "#f4f6f9ff"),
          panel.grid.major = element_line(color = "#f4f6f9ff"), 
          panel.grid.minor = element_line(color = "#f4f6f9ff"),
          plot.background = element_rect(fill = "#f4f6f9ff"),
          legend.background = element_rect(fill = "#f4f6f9ff")
        )+
        scale_x_continuous(breaks = seq(0,9,1))+
        scale_color_manual(values = expcol)
    # }else{
    # p <- ggplot(filteredData, aes(x = Year, y = Rel_Cover, color = interaction(Gradient, experiment))) +
    #   theme_bw()+
    #   facet_grid(.~ODT)+
    #   stat_smooth(aes(group = interaction(Gradient, experiment, ODT)), se = FALSE)+
    #   stat_smooth(aes(group = interaction(ODT)), se = FALSE, color = "grey20", linetype = "dotted")+
    #   
    #   labs(title = "Relative Cover of Species Over Years", x = "Year", y = "Relative Cover")+
    #   labs(color = "Treatments")+
    #   theme(
    #     panel.background = element_rect(fill = "#f4f6f9ff", color = "#f4f6f9ff"),
    #     panel.grid.major = element_line(color = "#f4f6f9ff"), 
    #     panel.grid.minor = element_line(color = "#f4f6f9ff"),
    #     plot.background = element_rect(fill = "#f4f6f9ff"),
    #     legend.background = element_rect(fill = "#f4f6f9ff")
    #   )+
    #   scale_x_continuous(breaks = seq(0,9,1))+
    #   scale_color_manual(values = expcol)
    # }
    ggplotly(p, tooltip = "all")
  })
  
  ##### PLOT: By Pools ----
  
  # Dynamic UI for originSiteID based on selected Gradient
  
  output$gradientID3_ui <- renderUI({
    selected_pool <-  input$selectedPool3
    if (!is.null(selected_pool)) {
      gradients <- dfs %>% 
        filter(pool == selected_pool) %>% 
        dplyr::select(Gradient) %>% distinct() %>% pull(Gradient)
      selectInput("selectedGradient3", "Choose Gradient:", choices = c("All", gradients), multiple = TRUE, selected = "All")
    }
  })
  
  observeEvent(input$selectedGradient3, {
    selectedGradient <- input$selectedGradient3
    # If "All" is selected along with other gradients, remove "All" from the selection
    if("All" %in% selectedGradient && length(selectedGradient) > 1) {
      selectedGradient <- selectedGradient[selectedGradient != "All"]
      updateSelectInput(session, "selectedGradient3", selected = selectedGradient)
    }
  })
  
  output$experimentID3_ui <- renderUI({
    selectedGradient <- input$selectedGradient3
    
    if (length(selectedGradient) == 1 && selectedGradient == "All") {
      experiments <- c("All")
    } else {
      experiments <- dfs %>%
        filter(Gradient %in% selectedGradient) %>%
        dplyr::select(experiment) %>% 
        distinct() %>%
        pull(experiment)
      experiments <- c("All",experiments)
    }
    
    selectInput("selectedExperimentID3", "Choose Experiment:", choices = experiments, selected = "")
  })
  
  output$poolsPlot <- renderPlotly({
    # Make sure the input IDs match with those in ui.R
    req(input$selectedPool3, input$selectedGradient3, input$selectedExperimentID3, input$selectedMethod3)
    selectedMethod = ifelse(input$selectedMethod3 == "Linear regression","lm","loess")
    
    # Correct data filtering according to selected inputs
    if("All" %in% input$selectedGradient3 &  "All" %in% input$selectedExperimentID3){
      filteredData <- dfs %>%
        filter(pool %in% input$selectedPool3)
    }
    if(!("All" %in% input$selectedGradient3) &  "All" %in% input$selectedExperimentID3){
      filteredData <- dfs %>%
        filter(pool %in% input$selectedPool3)%>%
        filter(Gradient %in% input$selectedGradient3)
    }
    if(!("All" %in% input$selectedGradient3) & !( "All" %in% input$selectedExperimentID3)){
      filteredData <- dfs %>%
        filter(pool %in% input$selectedPool3)%>%
        filter(Gradient %in% input$selectedGradient3)%>%
        filter(experiment %in% input$selectedExperimentID3)
    }
    filteredData =
      filteredData %>%
      mutate(ODT = recode(ODT, destControls = "Destination control",
                          originControls = "Origin control",
                          warmed = "Warmed"))%>%
      mutate(ODT = factor(ODT, levels = c("Origin control","Warmed","Destination control")))%>%
      mutate(Year = Year-`Transplantation year`)
    
    set.seed(123)
    expcol  = sample(sequential_hcl(n = 40, palette = "Hawaii"), 40)
    names(expcol) = unique(paste0(dfs$Gradient,".", dfs$experiment))
    
    #if("All" %in% input$selectedGradient2 &  "All" %in% input$selectedExperimentID2){
    p <- ggplot(filteredData, aes(x = Year, y = Rel_Cover, 
                                  color = interaction(Gradient, experiment))) +
      theme_bw()+
      facet_grid(.~ODT)+
      stat_smooth(method = selectedMethod,aes(group = interaction(Gradient, experiment)), se = FALSE, alpha = 0.5, linewidth = 0.5)+
      stat_smooth(method = selectedMethod, aes(group = interaction(ODT)), se = FALSE, color = "grey20", linetype = "dotted")+
      labs(title = "Relative Cover of Species Over Years", color = "Experiments", x = "Experimental years", y = "Relative Cover")+
      theme(
        text=element_text(size=tsize),
        panel.background = element_rect(fill = "#f4f6f9ff", color = "#f4f6f9ff"),
        panel.grid.major = element_line(color = "#f4f6f9ff"), 
        panel.grid.minor = element_line(color = "#f4f6f9ff"),
        plot.background = element_rect(fill = "#f4f6f9ff"),
        legend.background = element_rect(fill = "#f4f6f9ff")
      )+
      scale_x_continuous(breaks = seq(0,9,1))+
      scale_color_manual(values = expcol)
      
    ggplotly(p, tooltip = "all")
  })
  
  ##### CONVERGENCE PLOTS ------
  ####### PRC PLOT -----
  output$experimentPRC_ID_ui <- renderUI({
    selected_gradient <- input$selectedGradientPRC
    if (!is.null(selected_gradient)) {
      experiments <- dfs %>% filter(Gradient == selected_gradient) %>% dplyr::select(experiment) %>% distinct() %>% pull(experiment)
      selectInput("selectedExperimentPRC", "Choose Experiment:", choices = experiments)
    }
  })
  
  output$prcPlot <- renderPlotly({
    ODTcol  = c("#4285f4", "#fe7e00", "#ff0000")
    names(ODTcol) = c("Origin control","Destination control","Warmed")
    
    req(input$selectedGradientPRC, input$selectedExperimentPRC, input$selectedAxis)
    pPRC = 
    PRC %>%
      filter(Region == input$selectedGradientPRC)%>%
      filter(experiment == input$selectedExperimentPRC)
    p1 = 
    pPRC %>%
      dplyr::select(change, coef)%>%
      unnest(coef)%>%
      filter(axis == input$selectedAxis)%>%
      mutate(treatment = recode(treatment, warmed = "Warmed",
                                destControls = "Destination control",
                                originControls = "Origin control"))%>%
      ggplot(aes(Year, contrast, color = treatment, group = axis))+
      facet_grid(.~change)+
      geom_hline(yintercept = 0, color = "grey50")+
      geom_point()+
      geom_line()+
      scale_color_manual(values = ODTcol)+
      labs(x="Experimental years",y=paste0("Canonical coefficient (PRC axis ",input$selectedAxis,")"), color ="")+
      ylim(-1,1)+
      theme(
        text=element_text(size=tsize),
        panel.background = element_rect(fill = "#f4f6f9ff", color = "#f4f6f9ff"),
        panel.grid.major = element_line(color = "#f4f6f9ff"), 
        panel.grid.minor = element_line(color = "#f4f6f9ff"),
        plot.background = element_rect(fill = "#f4f6f9ff"),
        legend.background = element_rect(fill = "#f4f6f9ff")
      )
    
    ggplotly(p1)
  })
  output$prcSP <- renderPlotly({
    poolCol = c( "#ff7f00", "grey20","grey50","#d25fff","#197af6")
    names(poolCol) = c("colonizing","control","outsider","overlapping","strictly_high_elevation")
  req(input$selectedGradientPRC,input$selectedWei, input$selectedExperimentPRC, input$selectedAxis)
    pPRC = 
      PRC %>%
      filter(Region == input$selectedGradientPRC)%>%
      filter(experiment == input$selectedExperimentPRC)
    
    p2 = 
      pPRC %>%
      dplyr::select(change, sp.scores)%>%
      unnest(sp.scores)%>%
      filter(name == input$selectedAxis)%>%
      filter(value > input$selectedWei[1] | value < input$selectedWei[1]*(-1))%>%
      ggplot(aes(x = name, y = value, color = pool))+
      geom_hline(yintercept = 0, color = "grey30")+
      geom_vline(xintercept = input$selectedAxis, color = "grey30")+
      geom_point(aes(group = SpeciesName), show.legend = FALSE)+
      facet_grid(.~change)+
      scale_color_manual(values = poolCol)+
      labs(y = "Species weights", x = "", color = "")+
      theme(
        text=element_text(size=tsize),
        panel.background = element_rect(fill = "#f4f6f9ff", color = "#f4f6f9ff"),
        panel.grid.major = element_line(color = "#f4f6f9ff"), 
        panel.grid.minor = element_line(color = "#f4f6f9ff"),
        plot.background = element_rect(fill = "#f4f6f9ff"),
        legend.background = element_rect(fill = "#f4f6f9ff"),
        legend.position = "bottom"
      )
    ggplotly(p2)
  })
  ####### RATES PLOT ------
  output$ratesPlot <- renderPlotly({
    set.seed(123)
    expcol  = sample(sequential_hcl(n = 40, palette = "Hawaii"), 40)
    names(expcol) = unique(paste0(slopes$experiment))
    
    pslope =
      slopes %>%
      mutate(change = ifelse(change == "Distance to destination controls"& treatment == "originControls", "Distance between controls",change))%>%
      mutate(change = ifelse(change == "Distance to origin controls"& treatment == "destControls", "remove",change))%>%
      filter(change !="remove")%>%
      mutate(change = factor(change, levels = c("Distance between controls",
                                                "Distance to origin controls",
                                                "Distance to destination controls")))%>%
      mutate(treatment = recode(treatment, destControls = "Destination controls",
                                originControls = "Origin controls", 
                                warmed = "Warmed"))%>%
      mutate(axis = recode(axis, RDA1 = "Canonical coefficient (PRC axis 1)",
                           RDA2 = "Canonical coefficient (PRC axis 2)"))%>%
      
      ggplot()+
      geom_hline(yintercept = 0, color = "black")+
      theme_bw()+
      geom_point(alpha = 0.3, aes(Year_0, contrast, group = interaction(experiment, change)), color = "black")+
      stat_smooth(aes(Year_0, contrast, group = interaction(experiment, change), color = experiment, linetype = change), 
                  geom = "line", method = "lm", formula = y~log(x), se = FALSE, linewidth = 0.5, alpha = 0.3)+
      geom_line(data = pred, aes(Year_0, y= fit, group = interaction(experiment, change), linetype = change), color = "black", linewidth = 1)+
      geom_ribbon(data = pred, aes(Year_0, ymin = lwr, ymax = upr, group = change), color = "grey50", alpha = 0.1, show.legend = FALSE)+
      facet_grid(axis~., scales = "free_y")+
      #scale_color_manual(values = c(expcol))+
      #scale_fill_manual(values = c(expcol))+
      scale_x_continuous(breaks = seq(0,9,1))+
      scale_y_continuous(breaks = seq(0,0.7,0.1))+
      guides(linetype = "none")+
      #scale_linetype_manual(values = c("dashed","dashed","solid"))+
      labs(color = "Experiments", fill = "", linetype = "",
           x= "Experimental years", y = "Taxonomic distance")+
      theme(
        text=element_text(size=tsize),
        panel.background = element_rect(fill = "#f4f6f9ff", color = "#f4f6f9ff"),
        panel.grid.major = element_line(color = "#f4f6f9ff"), 
        panel.grid.minor = element_line(color = "#f4f6f9ff"),
        plot.background = element_rect(fill = "#f4f6f9ff"),
        legend.background = element_rect(fill = "#f4f6f9ff")
      )
      
    ggplotly(pslope, tooltip = "all")
  })
  ####### SP. RATES PLOT ------
  output$spratesPlot <- renderPlotly({
    set.seed(123)
    expcol  = sample(sequential_hcl(n = 40, palette = "Hawaii"), 40)
    names(expcol) = unique(paste0(slopes$experiment))
    
    sp.slope =
      sp.scores %>%
      unnest(data)%>%
      mutate(change = ifelse(change == "Distance to destination controls"& treatment == "originControls", "Distance between controls",change))%>%
      mutate(change = ifelse(change == "Distance to origin controls"& treatment == "destControls", "remove",change))%>%
      filter(change !="remove")%>%
      mutate(change = ifelse(change == "Distance to origin controls", "origin controls", "destination controls"))%>%
      mutate(change = factor(change, levels = c("origin controls",
                                                "destination controls")))%>%
      mutate(treatment = recode(treatment, destControls = "Destination controls",
                                originControls = "Origin controls", 
                                warmed = "Warmed"))%>%
      
      mutate(axis = recode(axis, RDA1 = "PRC axis 1",
                           RDA2 = "PRC axis 2"))%>%
      group_by(Region, originSiteID, destSiteID, experiment, Year_0, change, axis, treatment, pool)%>%
      summarize(sp.change = mean(sp.change))%>%
      ungroup()%>%
      mutate(pool = recode(pool, colonizing = "Colonizing",
                           overlapping = "Overlapping",
                           strictly_high_elevation = "Strictly high elevation"))%>%
      
      ggplot()+
      geom_hline(yintercept = 0, color = "black")+
      theme_bw()+
      geom_point(alpha = 0.3, aes(Year_0, sp.change, group = interaction(experiment, change, pool), color = experiment), show.legend = FALSE)+
      stat_smooth(aes(Year_0, sp.change, group = interaction(experiment, change, pool), color = experiment, linetype = change), 
                  geom = "line", method = "lm", formula = y~log(x), se = FALSE, linewidth = 0.5, alpha = 0.7)+
      geom_line(data = sp.pred, aes(Year_0, y= fit, group = interaction(experiment, change, pool), linetype = change), linewidth = 1, show.legend = FALSE)+
      geom_ribbon(data = sp.pred, aes(Year_0, ymin = lwr, ymax = upr, group = interaction(change, pool)), alpha = 0.1, show.legend = FALSE)+
      facet_grid(axis~pool, scales = "free_y")+
      scale_color_manual(values = expcol)+
      scale_linetype_manual(values = c("dotted","dashed"))+
      scale_x_continuous(breaks = seq(0,9,1))+
      guides(linetype = "none")+
      labs(color = "Experiments", x = "Experimental years", 
           y = "Proportional difference in warmed communities",
           title = "Changes in different species pools in warmed communities in comparison to controls over time",
           )+
      theme(
        text = element_text(size = tsize),
        panel.background = element_rect(fill = "#f4f6f9ff", color = "#f4f6f9ff"),
        panel.grid.major = element_line(color = "#f4f6f9ff"), 
        panel.grid.minor = element_line(color = "#f4f6f9ff"),
        plot.background = element_rect(fill = "#f4f6f9ff"),
        legend.background = element_rect(fill = "#f4f6f9ff")
      )

    ggplotly(sp.slope, tooltip = "all")
  })
  
  ####### Experimental Variation PLOT ------
  output$expvarPlot <- renderPlotly({
    req(input$selectedLevel, input$selectedType, input$selectedVariable)
    set.seed(123)
    expcol  = sample(sequential_hcl(n = 40, palette = "Hawaii"), 40)
    names(expcol) = unique(paste0(pvar$experiment))
    
    pvar1 =
      pvar %>% 
      filter(pool == input$selectedLevel)%>%
      filter(type == input$selectedType)%>%
      filter(variables %in% input$selectedVariable)%>%
      ggplot(aes(value, response))+
      theme_bw()+
      geom_hline(yintercept = 0, color = "grey50")+
      geom_point(size = 2, aes(fill = experiment, color = variables))+
      facet_grid(.~change)+
      geom_smooth(method = "lm", se = FALSE, aes(group = variables, color = variables))+
      scale_fill_manual(values = expcol)+
      labs(y = "Changes in warmed communities in comparison to controls", x = "Explanatory variable (standardized)", fill = "Experiments")+
      theme(
        text = element_text(size = tsize),
        panel.background = element_rect(fill = "#f4f6f9ff", color = "#f4f6f9ff"),
        panel.grid.major = element_line(color = "#f4f6f9ff"), 
        panel.grid.minor = element_line(color = "#f4f6f9ff"),
        plot.background = element_rect(fill = "#f4f6f9ff"),
        legend.background = element_rect(fill = "#f4f6f9ff")
      )
    
    ggplotly(pvar1, tooltip = "all")
  })
}

#secure_ui <- secure_app(ui)
#shinyApp(ui = secure_ui, server = server)

shinyApp(ui = ui, server = server)