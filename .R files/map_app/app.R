library(shiny)
library(leaflet)
library(dplyr)
library(rnaturalearth)
library(sf)
library(maps)
library(ggplot2)

source('functions.R')

shapefile_BA_path <- 'C:/Users/XJ/Documents/OYC_map/Control_Areas-shp/Control_Areas.shp'
shapefile_BA <- read_sf(shapefile_BA_path) %>% arrange(NAME)
#shapefile_ERCO <- shapefile_BA %>% 
#  filter(NAME == "ELECTRIC RELIABILITY COUNCIL OF TEXAS, INC.")
#shapefile_ERCO <- st_transform(shapefile_ERCO, crs = 4326)


# List of region names
region_names <- c("ELECTRIC RELIABILITY COUNCIL OF TEXAS, INC.", 
                  "PJM INTERCONNECTION, LLC", 
                  "MIDCONTINENT INDEPENDENT TRANSMISSION SYSTEM OPERATOR, INC..", 
                  "NEW YORK INDEPENDENT SYSTEM OPERATOR", 
                  "SOUTHWEST POWER POOL",
                  "CALIFORNIA INDEPENDENT SYSTEM OPERATOR")

#region_names_code <- c("ERCO", "PJM", "MISO", "NYIS", "SWPP", "CISO")


# Initialize an empty list
shapefile_list <- list()

# Loop through each region and create its shapefile
for(i in 1: length(region_names)) {
  region_name = region_names[i]
  #region_name_code = region_names_code[i]
  
  # Filter and transform the shapefile for the region
  region_shapefile <- shapefile_BA %>% 
    filter(NAME == region_name) %>%
    st_transform(crs = 4326)
  
  # Store the shapefile in the list
  shapefile_list[[region_name]] <- region_shapefile
}


ui <- navbarPage("Interactive Plant Map",
                 tabPanel("Data",
                          sidebarLayout(
                            sidebarPanel(
                              h4("Scenario 1"),
                              fileInput("file1", "Choose Files",
                                        accept = c("text/csv",
                                                   "text/comma-separated-values,text/plain",
                                                   ".csv"),
                                        multiple = TRUE
                              ),
                              checkboxInput("header", "Header", TRUE),
                              uiOutput("summaryText1"),
                              
                              
                              tags$hr(),  # Horizontal line for separation
                              tags$hr(),  # Horizontal line for separation
                              tags$hr(),  # Horizontal line for separation
                              tags$hr(),  # Horizontal line for separation
                              
                              h4("Scenario 2"),
                              fileInput("file2", "Choose Files",
                                        accept = c("text/csv",
                                                   "text/comma-separated-values,text/plain",
                                                   ".csv"),
                                        multiple = TRUE
                              ),
                              checkboxInput("header", "Header", TRUE),
                              uiOutput("summaryText2")
                            ),
                            mainPanel(
                              tabsetPanel(
                                tabPanel("Scenario 1 Data", dataTableOutput("table1")),
                                tabPanel("Scenario 2 Data", dataTableOutput("table2"))
                              )
                            )
                          )
                 ),
                 tabPanel("Map View 1",
                          leafletOutput("map1", width = "100%", height = "1000px")
                 ),
                 tabPanel("Map View 2",
                          leafletOutput("map2", width = "100%", height = "1000px")
                 ),
                 tabPanel("Analysis",
                          fluidRow(
                            column(4,
                                   checkboxInput("removeOutliers1", "Remove Outliers", FALSE),
                                   numericInput("numBins1", "Number of Bins", value = 30, min = 1),
                                   numericInput("iqrMultiplier1", "IQR Multiplier", value = 1.5, min = 0.5),
                                   checkboxGroupInput("selectedAuthorities1", "Choose Balancing Authorities:",
                                                      choices = c("ELECTRIC RELIABILITY COUNCIL OF TEXAS, INC.", 
                                                                  "PJM INTERCONNECTION, LLC",
                                                                  "MIDCONTINENT INDEPENDENT TRANSMISSION SYSTEM OPERATOR, INC..", 
                                                                  "NEW YORK INDEPENDENT SYSTEM OPERATOR", 
                                                                  "SOUTHWEST POWER POOL",
                                                                  "CALIFORNIA INDEPENDENT SYSTEM OPERATOR"),  # Replace with actual names
                                                      selected = c("ELECTRIC RELIABILITY COUNCIL OF TEXAS, INC.", 
                                                                   "PJM INTERCONNECTION, LLC",
                                                                   "MIDCONTINENT INDEPENDENT TRANSMISSION SYSTEM OPERATOR, INC..", 
                                                                   "NEW YORK INDEPENDENT SYSTEM OPERATOR", 
                                                                   "SOUTHWEST POWER POOL",
                                                                   "CALIFORNIA INDEPENDENT SYSTEM OPERATOR")  # By default, all are selected
                                   )
                            ),
                            column(8,
                                   plotOutput("histogram1")
                            )
                          ),
                          fluidRow(
                            column(4,
                                   checkboxInput("removeOutliers2", "Remove Outliers", FALSE),
                                   numericInput("numBins2", "Number of Bins", value = 30, min = 1),
                                   numericInput("iqrMultiplier2", "IQR Multiplier", value = 1.5, min = 0.5),
                                   checkboxGroupInput("selectedAuthorities2", "Choose Balancing Authorities:",
                                                      choices = c("ELECTRIC RELIABILITY COUNCIL OF TEXAS, INC.", 
                                                                  "PJM INTERCONNECTION, LLC",
                                                                  "MIDCONTINENT INDEPENDENT TRANSMISSION SYSTEM OPERATOR, INC..", 
                                                                  "NEW YORK INDEPENDENT SYSTEM OPERATOR", 
                                                                  "SOUTHWEST POWER POOL",
                                                                  "CALIFORNIA INDEPENDENT SYSTEM OPERATOR"),  # Replace with actual names
                                                      selected = c("ELECTRIC RELIABILITY COUNCIL OF TEXAS, INC.", 
                                                                   "PJM INTERCONNECTION, LLC",
                                                                   "MIDCONTINENT INDEPENDENT TRANSMISSION SYSTEM OPERATOR, INC..", 
                                                                   "NEW YORK INDEPENDENT SYSTEM OPERATOR", 
                                                                   "SOUTHWEST POWER POOL",
                                                                   "CALIFORNIA INDEPENDENT SYSTEM OPERATOR")  # By default, all are selected
                                   )
                            ),
                            column(8,
                                   plotOutput("histogram2")
                            )
                          ),
                          fluidRow(
                            column(4,
                                   checkboxInput("removeOutliers3", "Remove Outliers", FALSE),
                                   numericInput("numBins3", "Number of Bins", value = 30, min = 1),
                                   numericInput("iqrMultiplier3", "IQR Multiplier", value = 1.5, min = 0.5),
                                   checkboxGroupInput("selectedAuthorities3", "Choose Balancing Authorities:",
                                                      choices = c("ELECTRIC RELIABILITY COUNCIL OF TEXAS, INC.", 
                                                                  "PJM INTERCONNECTION, LLC",
                                                                  "MIDCONTINENT INDEPENDENT TRANSMISSION SYSTEM OPERATOR, INC..", 
                                                                  "NEW YORK INDEPENDENT SYSTEM OPERATOR", 
                                                                  "SOUTHWEST POWER POOL",
                                                                  "CALIFORNIA INDEPENDENT SYSTEM OPERATOR"),  # Replace with actual names
                                                      selected = c("ELECTRIC RELIABILITY COUNCIL OF TEXAS, INC.", 
                                                                   "PJM INTERCONNECTION, LLC",
                                                                   "MIDCONTINENT INDEPENDENT TRANSMISSION SYSTEM OPERATOR, INC..", 
                                                                   "NEW YORK INDEPENDENT SYSTEM OPERATOR", 
                                                                   "SOUTHWEST POWER POOL",
                                                                   "CALIFORNIA INDEPENDENT SYSTEM OPERATOR")  # By default, all are selected
                                   )
                            ),
                            column(8,
                                   plotOutput("histogram3")
                            )
                          ),
                          fluidRow(
                            column(4,
                                   checkboxInput("removeOutliers4", "Remove Outliers", FALSE),
                                   numericInput("numBins4", "Number of Bins", value = 30, min = 1),
                                   numericInput("iqrMultiplier4", "IQR Multiplier", value = 1.5, min = 0.5),
                                   checkboxGroupInput("selectedAuthorities4", "Choose Balancing Authorities:",
                                                      choices = c("ELECTRIC RELIABILITY COUNCIL OF TEXAS, INC.", 
                                                                  "PJM INTERCONNECTION, LLC",
                                                                  "MIDCONTINENT INDEPENDENT TRANSMISSION SYSTEM OPERATOR, INC..", 
                                                                  "NEW YORK INDEPENDENT SYSTEM OPERATOR", 
                                                                  "SOUTHWEST POWER POOL",
                                                                  "CALIFORNIA INDEPENDENT SYSTEM OPERATOR"),  # Replace with actual names
                                                      selected = c("ELECTRIC RELIABILITY COUNCIL OF TEXAS, INC.", 
                                                                   "PJM INTERCONNECTION, LLC",
                                                                   "MIDCONTINENT INDEPENDENT TRANSMISSION SYSTEM OPERATOR, INC..", 
                                                                   "NEW YORK INDEPENDENT SYSTEM OPERATOR", 
                                                                   "SOUTHWEST POWER POOL",
                                                                   "CALIFORNIA INDEPENDENT SYSTEM OPERATOR")  # By default, all are selected
                                   )
                            ),
                            column(8,
                                   plotOutput("histogram4")
                            )
                          )
                 )
                 
)

# Define server logic

server <- function(input, output, session) {
 
  uploadedData1 <- reactive({
    inFile <- input$file1
    if (is.null(inFile) || nrow(inFile) == 0) {
      return(list(data = NULL, params = NULL))
    }
    
    # Separate CSV and text files
    csvFiles <- inFile[grepl("\\.csv$", inFile$name),]
    textFiles <- inFile[grepl("\\.txt$", inFile$name),]
    
    # Read all uploaded files and row-bind them
    data_list <- lapply(csvFiles$datapath, function(filePath) {
      
      # Read each file. You can include additional parameters in read.csv if needed
      read.csv(filePath, header = input$header)
    })
    
    # Combine the data frames in the list. Make sure they all have the same structure
    combined_data <- do.call(rbind, data_list)
    combined_data$Balancing_Authority_Name<- toupper(combined_data$Balancing_Authority_Name)
    
    
    # Initialize parameters
    params <- list(Battery_Hour = NA, Battery_Rate = NA, Solar_Pre_Pene = NA, Wind_Pre_Pene = NA)
    
    # Process text files
    if (nrow(textFiles) > 0) {
      text_data <- read.csv(textFiles$datapath[1], header = TRUE)
      params <- list(Battery_Hour = text_data$Battery_Hour[1],
                     Battery_Rate = text_data$Battery_Rate[1],
                     Solar_Pre_Pene = text_data$Solar_Pre_Pene[1],
                     Wind_Pre_Pene = text_data$Wind_Pre_Pene[1])
    }
    
    
    return(list(combined_data = combined_data, params = params))
  })
  
  uploadedData2 <- reactive({
    inFile <- input$file2
    if (is.null(inFile) || nrow(inFile) == 0) {
      return(list(data = NULL, params = NULL))
    }
    
    # Separate CSV and text files
    csvFiles <- inFile[grepl("\\.csv$", inFile$name),]
    textFiles <- inFile[grepl("\\.txt$", inFile$name),]
    
    # Read all uploaded files and row-bind them
    data_list <- lapply(csvFiles$datapath, function(filePath) {
      
      # Read each file. You can include additional parameters in read.csv if needed
      read.csv(filePath, header = input$header)
    })
    
    # Combine the data frames in the list. Make sure they all have the same structure
    combined_data <- do.call(rbind, data_list)
    combined_data$Balancing_Authority_Name<- toupper(combined_data$Balancing_Authority_Name)
    
    # Initialize parameters
    params <- list(Battery_Hour = NA, Battery_Rate = NA, Solar_Pre_Pene = NA, Wind_Pre_Pene = NA)
    
    # Process text files
    if (nrow(textFiles) > 0) {
      text_data <- read.csv(textFiles$datapath[1], header = TRUE)
      params <- list(Battery_Hour = text_data$Battery_Hour[1],
                     Battery_Rate = text_data$Battery_Rate[1],
                     Solar_Pre_Pene = text_data$Solar_Pre_Pene[1],
                     Wind_Pre_Pene = text_data$Wind_Pre_Pene[1])
    }
    
    
    return(list(combined_data = combined_data, params = params))
  })
  
  # uploadedData <- reactive({
  #   inFile <- input$file1
  #   if (is.null(inFile))
  #     return(NULL)
  #   read.csv(inFile$datapath, header = input$header)
  # })
  # 
  # observe({
  #   
  #   data <- uploadedData()
  #   if (is.null(data)) return()
  
  # # Reactive expression to read the uploaded file
  # uploadedData <- reactive({
  #   inFile <- input$file1
  #   if (is.null(inFile))
  #     return(NULL)
  #   
  #   # Read all uploaded files and row-bind them
  #   data_list <- lapply(inFile$datapath, function(filePath) {
  #     read.csv(filePath, header = input$header)
  #   })
  #   do.call(rbind, data_list)
  # })
  # 
  # observe({
  # 
  #   data <- uploadedData()
  #   if (is.null(data)) return()
  # 
  #   # Process or display your combined data here
  #   output$table <- renderTable({
  #     data <- uploadedData()
  #     if (is.null(data)) return()
  #     data  # Return the data to render
  #   })
    # Update each slider's range based on the data
    # updateSliderInput(session, "A", min = min(data$A, na.rm = TRUE), max = max(data$A, na.rm = TRUE))
    # updateSliderInput(session, "B", min = min(data$B, na.rm = TRUE), max = max(data$B, na.rm = TRUE))
    # updateSliderInput(session, "C", min = min(data$C, na.rm = TRUE), max = max(data$C, na.rm = TRUE))
    # updateSliderInput(session, "D", min = min(data$D, na.rm = TRUE), max = max(data$D, na.rm = TRUE))
    # 
    # 
    # # Calculate the min, max, and quantiles for variable A
    # min_val <- round(min(data$A, na.rm = TRUE), 1)
    # max_val <- round(max(data$A, na.rm = TRUE), 1)
    # quartiles <- round(quantile(data$A, probs = seq(0, 1, 0.25), na.rm = TRUE), 1)
    # 
    # # Update the sliderInput with the calculated values
    # updateSliderInput(session, "A",
    #                   min = round(min_val, 1),
    #                   max = round(max_val, 1),
    #                   value = c(round(quartiles[2], 1), round(quartiles[3], 1)),  # 25th to 75th percentile by default
    #                   step = (max_val - min_val) / 4  # Adjust step size for four distinct levels
    # )
    # 
    # #--------------------
    # # Calculate the min, max, and quantiles for variable B
    # min_val <- round(min(data$B, na.rm = TRUE), 1)
    # max_val <- round(max(data$B, na.rm = TRUE), 1)
    # quartiles <- round(quantile(data$B, probs = seq(0, 1, 0.25), na.rm = TRUE), 1)
    # 
    # # Update the sliderInput with the calculated values
    # updateSliderInput(session, "B",
    #                   min = round(min_val, 1),
    #                   max = round(max_val, 1),
    #                   value = c(round(quartiles[2], 1), round(quartiles[3], 1)),  # 25th to 75th percentile by default
    #                   step = (max_val - min_val) / 4  # Adjust step size for four distinct levels
    # )
    # #------------------------
    # # Calculate the min, max, and quantiles for variable C
    # min_val <- round(min(data$C, na.rm = TRUE), 1)
    # max_val <- round(max(data$C, na.rm = TRUE), 1)
    # quartiles <- round(quantile(data$C, probs = seq(0, 1, 0.25), na.rm = TRUE), 1)
    # 
    # # Update the sliderInput with the calculated values
    # updateSliderInput(session, "C",
    #                   min = round(min_val, 1),
    #                   max = round(max_val, 1),
    #                   value = c(round(quartiles[2], 1), round(quartiles[3], 1)),  # 25th to 75th percentile by default
    #                   step = (max_val - min_val) / 4  # Adjust step size for four distinct levels
    # )
    # #-------------------------
    # # Calculate the min, max, and quantiles for variable D
    # min_val <- round(min(data$D, na.rm = TRUE), 1)
    # max_val <- round(max(data$D, na.rm = TRUE), 1)
    # quartiles <- round(quantile(data$D, probs = seq(0, 1, 0.25), na.rm = TRUE), 1)
    # 
    # # Update the sliderInput with the calculated values
    # updateSliderInput(session, "D",
    #                   min = round(min_val, 1),
    #                   max = round(max_val, 1),
    #                   value = c(round(quartiles[2], 1), round(quartiles[3], 1)),  # 25th to 75th percentile by default
    #                   step = (max_val - min_val) / 4  # Adjust step size for four distinct levels
    # )
  #})
  
  
  # Reactive expression for filtered data
  #filteredData <- reactive({
    # Ensure there is data to filter
    #if (is.null(uploadedData()))
    #  return(NULL)
    
    # Filter the data based on slider inputs
  #  data <- uploadedData()
    # data[data$A >= input$A[1] & data$A <= input$A[2] &
    #        data$B >= input$B[1] & data$B <= input$B[2] &
    #        data$C >= input$C[1] & data$C <= input$C[2] &
    #        data$D >= input$D[1] & data$D <= input$D[2], ]
  #})
  
  
  # Create the summary text
  output$summaryText1 <- renderText({
    params <- uploadedData1()$params
    if (is.null(params)) return("No data available.")
    
    paste0("<span style='font-family: Arial; font-size: 14px;'>Here, we look at the analysis with <b>Battery Hour</b> =", params$Battery_Hour,
           ", <b>Battery Rate</b> =", params$Battery_Rate,
           ", <b>Solar Pre-Penetration</b> =", round(as.numeric(params$Solar_Pre_Pene), 2),
           ", <b>Wind Pre-Penetration</b> =", round(as.numeric(params$Wind_Pre_Pene), 2), ".</span>")
    
  })
  
  output$summaryText2 <- renderText({
    params <- uploadedData2()$params
    if (is.null(params)) return("No data available.")
    
    paste0("<span style='font-family: Arial; font-size: 14px;'>Here, we look at the analysis with <b>Battery Hour</b> =", params$Battery_Hour,
           ", <b>Battery Rate</b> =", params$Battery_Rate,
           ", <b>Solar Pre-Penetration</b> =", round(as.numeric(params$Solar_Pre_Pene), 2),
           ", <b>Wind Pre-Penetration</b> =", round(as.numeric(params$Wind_Pre_Pene), 2), ".</span>")
    
  })
  
  # Render the table output for filtered data
  output$table1 <- renderDataTable({
    uploadedData1()$combined_data
  })
  
  output$table2 <- renderDataTable({
    uploadedData2()$combined_data
  })
  
  # Define colors for each region
  region_colors <- setNames(c("#add8e6", "#FF6347", "#32CD32", "#FFD700", "#6A5ACD", "#FF7F50"), 
                            c("ELECTRIC RELIABILITY COUNCIL OF TEXAS, INC.", 
                              "PJM INTERCONNECTION, LLC", 
                              "MIDCONTINENT INDEPENDENT TRANSMISSION SYSTEM OPERATOR, INC..", 
                              "NEW YORK INDEPENDENT SYSTEM OPERATOR", 
                              "SOUTHWEST POWER POOL",
                              "CALIFORNIA INDEPENDENT SYSTEM OPERATOR"))
  
  output$map1 <- renderLeaflet({
    
    # Ensure there is data to plot
    df <- uploadedData1()$combined_data
    if (is.null(df) || nrow(df) == 0)
      return(NULL)
    
    #color_palette <- colorFactor(palette = unname(region_colors), domain = names(region_colors))
    
    legendHTML_generation <- HTML(
      '<div style="background: rgba(255, 255, 255, 0.8); padding: 6px; position: fixed; bottom: 50px; right: 20px; z-index: 1000; border-radius: 4px; font-size: 14px;">
    <b>Plant Output</b><br>
    <div style="display: flex; align-items: center; margin-bottom: 4px;">
      <svg width="24" height="24" style="margin-right: 4px;"><circle cx="12" cy="12" r="4.02" fill="grey20"/></svg> 
      <span>1000000 MWh</span>
    </div>
    <div style="display: flex; align-items: center; margin-bottom: 4px;">
      <svg width="24" height="24" style="margin-right: 4px;"><circle cx="12" cy="12" r="4.92" fill="grey20"/></svg>
      <span>1500000 MWh</span>
    </div>
    <div style="display: flex; align-items: center; margin-bottom: 4px;">
      <svg width="24" height="24" style="margin-right: 4px;"><circle cx="12" cy="12" r="5.68" fill="grey20"/></svg>
      <span>2000000 MWh</span>
    </div>
    <div style="display: flex; align-items: center; margin-bottom: 4px;">
      <svg width="24" height="24" style="margin-right: 4px;"><circle cx="12" cy="12" r="7.51" fill="grey20"/></svg>
      <span>35000000 MWh</span>
    </div>
    <div style="display: flex; align-items: center;">
      <svg width="24" height="24" style="margin-right: 4px;"><circle cx="12" cy="12" r="8.98" fill="grey20"/></svg>
      <span>5000000 MWh</span>
    </div>
  </div>'
    )
    
    legendHTML_region <- HTML(
      paste0(
        '<div style="display: flex; flex-direction: column; justify-content: center; align-items: flex-end; height: 100%; position: fixed; top: 0; right: 10px; z-index: 1000; pointer-events: none;">',
        '<div style="background: rgba(255, 255, 255, 0.8); padding: 6px; position: fixed; bottom: 10px; left: 10px; z-index: 1000; border-radius: 4px; font-size: 14px; width: 600px;">',
        '<b>Regions</b><br>',
        paste(sapply(1:length(region_colors), function(i) {
          paste0(
            '<div style="display: flex; align-items: center; margin-bottom: 4px;">',
            '<div style="width: 24px; height: 24px; background:', region_colors[i], '; margin-right: 4px;"></div>',
            '<span>', names(region_colors)[i], '</span>',
            '</div>'
          )
        }), collapse = ""),
        '</div>',
        '</div>'
      )
    )
    
    # Initialize leaflet map
    leaflet_map <- leaflet(df) %>% addTiles()
    
    # Loop to add polygons for each region
    for(region in names(region_colors)) {
      region_shapefile <- shapefile_list[[region]]  # Assuming you have a list of shapefiles
      leaflet_map <- leaflet_map %>% 
        addPolygons(data = region_shapefile,
                    fillColor = unname(region_colors[region]),
                    color = unname(region_colors[region]),
                    weight = 1,
                    fillOpacity = 0.5) 
    }
    # } %>% 
    #   addLegend("bottomright", # Position of the legend
    #             pal = color_palette, # Palette of colors
    #             values = names(region_colors), # The names of the regions
    #             title = "Region", # Title of the legend
    #             opacity = 0.5
    #   )
    
    # Add circle markers with dynamic color
    leaflet_map %>% addCircleMarkers(
      ~Longitude, ~Latitude,
      radius = ~sqrt(Cumulative_Output) / 249,
      popup = ~paste("<b>Plant Code:</b>", Plant_Code, "<br>",
                     "<b>Plant Name:</b>", Plant_Name, "<br>",
                     "<b>Total Namaplate Capacity (MW):</b>", Total_Namaplate_Capacity, "<br>",
                     "<b>Average Operating Cost ($/MWh):</b>", round(Avg_Op_Cost, 2), "<br>",
                     "<b>Plant Output (MWh):</b>", round(Cumulative_Output), "<br>",
                     "<b>Number of Generators:</b>", Number_of_Generators, "<br>",
                     "<b>Capacity Factor (%) :</b>", round(Cap_Factor * 100, 2), "<br>"),
      color = "grey20",
      fillOpacity = 0.8
    ) %>%
      addTiles() %>% 
      addControl(legendHTML_generation, position = "bottomright") %>% 
      addControl(legendHTML_region, position = "bottomleft") %>% 
      
      fitBounds(~min(Longitude), ~min(Latitude), ~max(Longitude), ~max(Latitude)) 

  })
  
  output$map2 <- renderLeaflet({
    
    # Ensure there is data to plot
    df <- uploadedData2()$combined_data
    if (is.null(df) || nrow(df) == 0)
      return(NULL)
    
    #color_palette <- colorFactor(palette = unname(region_colors), domain = names(region_colors))
    
    legendHTML_generation <- HTML(
      '<div style="background: rgba(255, 255, 255, 0.8); padding: 6px; position: fixed; bottom: 50px; right: 20px; z-index: 1000; border-radius: 4px; font-size: 14px;">
    <b>Plant Output</b><br>
    <div style="display: flex; align-items: center; margin-bottom: 4px;">
      <svg width="24" height="24" style="margin-right: 4px;"><circle cx="12" cy="12" r="4.02" fill="grey20"/></svg> 
      <span>1000000 MWh</span>
    </div>
    <div style="display: flex; align-items: center; margin-bottom: 4px;">
      <svg width="24" height="24" style="margin-right: 4px;"><circle cx="12" cy="12" r="4.92" fill="grey20"/></svg>
      <span>1500000 MWh</span>
    </div>
    <div style="display: flex; align-items: center; margin-bottom: 4px;">
      <svg width="24" height="24" style="margin-right: 4px;"><circle cx="12" cy="12" r="5.68" fill="grey20"/></svg>
      <span>2000000 MWh</span>
    </div>
    <div style="display: flex; align-items: center; margin-bottom: 4px;">
      <svg width="24" height="24" style="margin-right: 4px;"><circle cx="12" cy="12" r="7.51" fill="grey20"/></svg>
      <span>35000000 MWh</span>
    </div>
    <div style="display: flex; align-items: center;">
      <svg width="24" height="24" style="margin-right: 4px;"><circle cx="12" cy="12" r="8.98" fill="grey20"/></svg>
      <span>5000000 MWh</span>
    </div>
  </div>'
    )
    
    legendHTML_region <- HTML(
      paste0(
        '<div style="display: flex; flex-direction: column; justify-content: center; align-items: flex-end; height: 100%; position: fixed; top: 0; right: 10px; z-index: 1000; pointer-events: none;">',
        '<div style="background: rgba(255, 255, 255, 0.8); padding: 6px; position: fixed; bottom: 10px; left: 10px; z-index: 1000; border-radius: 4px; font-size: 14px; width: 600px;">',
        '<b>Regions</b><br>',
        paste(sapply(1:length(region_colors), function(i) {
          paste0(
            '<div style="display: flex; align-items: center; margin-bottom: 4px;">',
            '<div style="width: 24px; height: 24px; background:', region_colors[i], '; margin-right: 4px;"></div>',
            '<span>', names(region_colors)[i], '</span>',
            '</div>'
          )
        }), collapse = ""),
        '</div>',
        '</div>'
      )
    )
    
    # Initialize leaflet map
    leaflet_map <- leaflet(df) %>% addTiles()
    
    # Loop to add polygons for each region
    for(region in names(region_colors)) {
      region_shapefile <- shapefile_list[[region]]  # Assuming you have a list of shapefiles
      leaflet_map <- leaflet_map %>% 
        addPolygons(data = region_shapefile,
                    fillColor = unname(region_colors[region]),
                    color = unname(region_colors[region]),
                    weight = 1,
                    fillOpacity = 0.5) 
    }
    # } %>% 
    #   addLegend("bottomright", # Position of the legend
    #             pal = color_palette, # Palette of colors
    #             values = names(region_colors), # The names of the regions
    #             title = "Region", # Title of the legend
    #             opacity = 0.5
    #   )
    
    # Add circle markers with dynamic color
    leaflet_map %>% addCircleMarkers(
      ~Longitude, ~Latitude,
      radius = ~sqrt(Cumulative_Output) / 249,
      popup = ~paste("<b>Plant Code:</b>", Plant_Code, "<br>",
                     "<b>Plant Name:</b>", Plant_Name, "<br>",
                     "<b>Total Namaplate Capacity (MW):</b>", Total_Namaplate_Capacity, "<br>",
                     "<b>Average Operating Cost ($/MWh):</b>", round(Avg_Op_Cost, 2), "<br>",
                     "<b>Plant Output (MWh):</b>", round(Cumulative_Output), "<br>",
                     "<b>Number of Generators:</b>", Number_of_Generators, "<br>",
                     "<b>Capacity Factor (%) :</b>", round(Cap_Factor * 100, 2), "<br>"),
      color = "grey20",
      fillOpacity = 0.8
    ) %>%
      addTiles() %>% 
      addControl(legendHTML_generation, position = "bottomright") %>% 
      addControl(legendHTML_region, position = "bottomleft") %>% 
      
      fitBounds(~min(Longitude), ~min(Latitude), ~max(Longitude), ~max(Latitude)) 
    
  })
  
  # Render histogram for Total_Namaplate_Capacity
  output$histogram1 <- renderPlot({
    # Ensure there is data to plot
    df1 <- uploadedData1()$combined_data
    df2 <- uploadedData2()$combined_data

    if (is.null(df1) && is.null(df2)) return(NULL)
    
    # Remove outliers if checkbox is checked
    if (input$removeOutliers1) {
      df1 <- removeOutliers(df1, "Total_Namaplate_Capacity", input$iqrMultiplier1)
      df2 <- removeOutliers(df2, "Total_Namaplate_Capacity", input$iqrMultiplier1)
    }
    
    # Prepare data for ggplot
    plot_data <- list()
    if (!is.null(df1)) {
      df1$Scenario <- 'Scenario 1'
      plot_data[[1]] <- df1
    }
    if (!is.null(df2)) {
      df2$Scenario <- 'Scenario 2'
      plot_data[[2]] <- df2
    }
    combined_df <- do.call(rbind, plot_data)
    
    # Filter based on selected authorities
    combined_df <-  combined_df %>% filter(Balancing_Authority_Name %in% input$selectedAuthorities1)
    
    ggplot(combined_df, aes(x = Total_Namaplate_Capacity, fill = Balancing_Authority_Name)) +
      geom_histogram(bins = input$numBins1, alpha = 0.5) +
      facet_grid(~ Scenario, scales = "free_x") +  # Use facet_grid to create side-by-side histograms
      scale_fill_manual(values = unname(region_colors)) +
      theme_minimal() +
      labs(title = "Total Namaplate Capacity (MW)", x = "Total Namaplate Capacity (MW)") +
      theme(
        plot.title = element_text(hjust = 0.5, size = 20),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)
      )
  })
  
  output$histogram2 <- renderPlot({
    # Ensure there is data to plot
    df1 <- uploadedData1()$combined_data
    df2 <- uploadedData2()$combined_data
    
    if (is.null(df1) && is.null(df2)) return(NULL)
    
    # Remove outliers if checkbox is checked
    if (input$removeOutliers2) {
      df1 <- removeOutliers(df1, "Avg_Op_Cost", input$iqrMultiplier1)
      df2 <- removeOutliers(df2, "Avg_Op_Cost", input$iqrMultiplier1)
    }
    
    # Prepare data for ggplot
    plot_data <- list()
    if (!is.null(df1)) {
      df1$Scenario <- 'Scenario 1'
      plot_data[[1]] <- df1
    }
    if (!is.null(df2)) {
      df2$Scenario <- 'Scenario 2'
      plot_data[[2]] <- df2
    }
    combined_df <- do.call(rbind, plot_data)
   
    # Filter based on selected authorities
    combined_df <-  combined_df %>% filter(Balancing_Authority_Name %in% input$selectedAuthorities2)
    
    ggplot(combined_df, aes(x = Avg_Op_Cost, fill = Balancing_Authority_Name)) +
      geom_histogram(bins = input$numBins2, alpha = 0.5) +
      facet_grid(~ Scenario, scales = "free_x") +  # Use facet_grid to create side-by-side histograms
      scale_fill_manual(values = unname(region_colors)) +
      theme_minimal() +
      labs(title = "Average Operating Cost ($/MWh)", x = "Average Operating Cost ($/MWh)") +
      theme(
        plot.title = element_text(hjust = 0.5, size = 20),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)
      )
    
  })
  
  output$histogram3 <- renderPlot({
    # Ensure there is data to plot
    df1 <- uploadedData1()$combined_data
    df2 <- uploadedData2()$combined_data
    
    if (is.null(df1) && is.null(df2)) return(NULL)
    
    # Remove outliers if checkbox is checked
    if (input$removeOutliers3) {
      df1 <- removeOutliers(df1, "Cumulative_Output", input$iqrMultiplier1)
      df2 <- removeOutliers(df2, "Cumulative_Output", input$iqrMultiplier1)
    }
    
    # Prepare data for ggplot
    plot_data <- list()
    if (!is.null(df1)) {
      df1$Scenario <- 'Scenario 1'
      plot_data[[1]] <- df1
    }
    if (!is.null(df2)) {
      df2$Scenario <- 'Scenario 2'
      plot_data[[2]] <- df2
    }
    combined_df <- do.call(rbind, plot_data)
    
    # Filter based on selected authorities
    combined_df <-  combined_df %>% filter(Balancing_Authority_Name %in% input$selectedAuthorities3)
    
    ggplot(combined_df, aes(x = Cumulative_Output, fill = Balancing_Authority_Name)) +
      geom_histogram(bins = input$numBins3, alpha = 0.5) +
      facet_grid(~ Scenario, scales = "free_x") +  # Use facet_grid to create side-by-side histograms
      scale_fill_manual(values = unname(region_colors)) +
      theme_minimal() +
      labs(title = "Plant Output (MWh)", x = "Plant Output (MWh)") +
      theme(
        plot.title = element_text(hjust = 0.5, size = 20),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)
      )
    
  })
  
  output$histogram4 <- renderPlot({
    # Ensure there is data to plot
    df1 <- uploadedData1()$combined_data
    df2 <- uploadedData2()$combined_data
    
    if (is.null(df1) && is.null(df2)) return(NULL)
    
    # Remove outliers if checkbox is checked
    
    if (input$removeOutliers4) {
      df1 <- removeOutliers(df1, "Cap_Factor", input$iqrMultiplier1)
      df2 <- removeOutliers(df2, "Cap_Factor", input$iqrMultiplier1)
    }
    
    # Prepare data for ggplot
    plot_data <- list()
    if (!is.null(df1)) {
      df1$Scenario <- 'Scenario 1'
      plot_data[[1]] <- df1
    }
    if (!is.null(df2)) {
      df2$Scenario <- 'Scenario 2'
      plot_data[[2]] <- df2
    }
    combined_df <- do.call(rbind, plot_data)
    
    # Filter based on selected authorities
    combined_df <-  combined_df %>% filter(Balancing_Authority_Name %in% input$selectedAuthorities4)
    
    ggplot(combined_df, aes(x = Cap_Factor*100, fill = Balancing_Authority_Name)) +
      geom_histogram(bins = input$numBins4, alpha = 0.5) +
      facet_grid(~ Scenario, scales = "free_x") +  # Use facet_grid to create side-by-side histograms
      scale_fill_manual(values = unname(region_colors)) +
      theme_minimal() +
      labs(title = "Capacity Factor (%)", x = "Capacity Factor (%)") +
      theme(
        plot.title = element_text(hjust = 0.5, size = 20),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)
      )
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
