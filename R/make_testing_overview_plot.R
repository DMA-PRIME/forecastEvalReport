#------------------------------------------------------------------------------#
#                                                                              #
#      Building location-level forecast vs. observed comparison plot           #
#                                                                              #
#------------------------------------------------------------------------------#
# About:                                                                       #
#                                                                              #
# This function generates an interactive Plotly visualization comparing        #
# observed (training) data with forecast outputs across multiple horizons for  #
# a single location. It serves as a diagnostic view of model performance       #
# during the testing period.                                                   #
#                                                                              #
# Observed data are plotted as a solid reference line, while forecast outputs  #
# are displayed as separate dashed lines for each forecast horizon. A dynamic  #
# color palette is applied to distinguish horizons and support visual          #
# comparison across prediction windows.                                        #
#                                                                              #
# The layout is configured for clarity and usability, including unified hover  #
# interactions, standardized axis formatting, and a horizontally oriented      #
# legend. An expandable fullscreen control is also added to enhance user       #
# interaction and accessibility.                                               #
#                                                                              #
# Centralizing location-level plotting ensures consistent visualization        #
# design and supports scalable comparison of model performance across          #
# geographies and forecasting horizons.                                        #
#------------------------------------------------------------------------------#
#                       Author: Amanda Bleichrodt                              #
#------------------------------------------------------------------------------#
#                        Last Updated: 2026-04-11                              #
#------------------------------------------------------------------------------#
make_testing_overview_plot <- function(data, loc, outcome,
                                           peakTrough.data){
  
  #########################################
  # Preparing the input data for the plot #
  #########################################
  
  # Filtering for the correct location 
  loc_data <- data %>% dplyr::filter(location %in% c(loc))
  
  # Filtering for the correct horizon 
  horizons <- sort(unique(loc_data$horizon))
  
  # Setting the colors for the horizon 
  horizon_colors <- setNames(colorRampPalette(c("#7F77DD", "#D85A30"))(length(horizons)),
                             as.character(horizons))
  
  # Creating the observed data
  observed_data <- loc_data %>%
    dplyr::distinct(target_end_date, .keep_all = TRUE)
  
#------------------------------------------------------------------------------#
# Joining phase indicator and season for peak window shading -------------------
#------------------------------------------------------------------------------#
# About: This section joins the peak trough data with the full data set. The   #
# goal of this is to allow for the drawing of the shaded boxes for the         #
# "At Peak" time to make it clear when the "Peak Phase" happened, and the      #
# forecasts considered as part of the table and the rest of the peak           #
# evaluations.                                                                 #
#------------------------------------------------------------------------------#
  
  ######################
  # Joining phase data #              
  ######################
  
  # Preparing the phase data 
  phase_data <- peakPhase.data %>%
    dplyr::ungroup() %>%
    dplyr::filter(location %in% c(loc)) %>%
    dplyr::select(target_end_date, borderStart, borderEnd, season) %>%
    dplyr::distinct(target_end_date, .keep_all = TRUE)
  
  # Joining with the full data set 
  loc_data <- loc_data %>%
    dplyr::left_join(phase_data, by = "target_end_date")
  
  ##########################
  # Getting unique seasons #
  ##########################
  seasons <- sort(unique(loc_data$season[!is.na(loc_data$season)]))
  
#------------------------------------------------------------------------------#
# Building the 'peak phase' section --------------------------------------------
#------------------------------------------------------------------------------#
# About: This section builds the peak phase section for the peak evaluation    #
# section of the reports. The goal of this section is to highlight what is the #
# actual 'global peak' considered in the calculations.                         #
#------------------------------------------------------------------------------#
  
  #####################################
  # Building one rectangle per season #
  #####################################
  lapply(seasons, function(s){

    # Preparing the seasonal 'dates' 
    s_dates <- loc_data %>% dplyr::filter(season == s)
    
    # Creating the rectangle 
    list(type      = "rect",
         layer     = "below",
         xref      = "x",
         yref      = "paper",
         x0        = format(unique(s_dates$borderStart), "%Y-%m-%d"),
         x1        = format(unique(s_dates$borderEnd), "%Y-%m-%d"),
         y0        = 0,
         y1        = 1,
         fillcolor = "rgba(200, 200, 200, 0.25)",
         line      = list(width = 0))
    })

  
  #------------------------------------------------------------------------------#
  # Building the observed data plotly --------------------------------------------#
  #------------------------------------------------------------------------------#
  # About: This section builds the observed data plotly trace. This is just the  #
  # line that shows the "truth" during the testing period and is to act as a     #
  # visual reference for users to see how the models did during the testing      #
  # period.                                                                      #
  #------------------------------------------------------------------------------#
  p <- plotly::plot_ly(height = 470, width = 800) %>%
    
    ######################
  # Creating the trace #
  ######################
  plotly::add_trace(
    
    data          = observed_data,
    x             = ~target_end_date,
    y             = ~Observed,
    type          = "scatter",
    mode          = "lines",
    name          = paste0("Training Data: "), 
    line          = list(color = "#333333", width = 2.5, dash = "solid"),
    hovertemplate = "<b>Training Data</b><br>Date: %{x}<br>Value: %{y}<extra></extra>"
  )
  
  #------------------------------------------------------------------------------#
  # Building the model output plotly trace ---------------------------------------#
  #------------------------------------------------------------------------------#
  # About: This section creates the model output traces for the testing period.  #
  # It creates the plotly traces for each horizon loaded by the user for         #
  # comparison against the truth data. It is robust to multiple horizon types    #
  # and uses the color scheme specified above.                                   #
  #------------------------------------------------------------------------------#
  
  ############################
  # Looping through horizons #
  ############################
  for (h in horizons){
    
    # Filtering the data for the correct horizon 
    h_data <- loc_data %>% dplyr::filter(horizon == h)
    
    #############################
    # Creating the plotly trace #
    #############################
    p <- p %>%
      plotly::add_trace(
        data          = h_data,
        x             = ~target_end_date,
        y             = ~value,
        type          = "scatter",
        mode          = "lines",
        name          = paste0("Horizon ", h),
        line          = list(color = horizon_colors[as.character(h)],
                             width = 1.5,
                             dash  = "dash"),
        hovertemplate = paste0("<b>Horizon ", h, "</b><br>Date: %{x}<br>Value: %{y}<extra></extra>"))
    
  }
  
  #------------------------------------------------------------------------------#
  # Setting the plotly layout ----------------------------------------------------#
  #------------------------------------------------------------------------------#
  # About: This section sets the plotly layout, including labels for the x-axis  #
  # and y-axis, the legend set up, and the hovers over the graph points. It      #
  # also sets the margins and includes the peak window shading shapes.           #
  #------------------------------------------------------------------------------#
  
  p <- p %>%
    plotly::layout(
      
      #############################
      # Setting the x-axis layout #
      #############################
      xaxis         = list(
        title      = "",
        tickformat = "%b %d, %Y",
        showgrid   = TRUE,
        gridcolor  = "#f0f0f0",
        zeroline   = FALSE,
        range      = list(
          min(loc_data$target_end_date) - 5,
          max(loc_data$target_end_date) + 5
        )
      ),
      
      #############################
      # Setting the y-axis layout #
      #############################
      yaxis         = list(
        title     = outcome,
        showgrid  = TRUE,
        gridcolor = "#f0f0f0",
        zeroline  = FALSE
      ),
      
      #########################
      # Formatting the legend #
      #########################
      legend        = list(
        orientation = "h",
        x           = 0.5,
        xanchor     = "center",
        y           = -0.12
      ),
      
      #########################
      # Setting the hovermode #
      #########################
      hovermode     = "x unified",
      
      ########################
      # Plot layout: General #
      ########################
      plot_bgcolor  = "#ffffff",
      paper_bgcolor = "#ffffff",
      font          = list(family = "sans-serif", color = "#555"),
      margin        = list(l = 40, r = 40, t = 20, b = 60),
      
      ##################################
      # Adding peak window shading     #
      ##################################
      shapes        = shade_shapes
    )
  
  #------------------------------------------------------------------------------#
  # Creating the 'Expand' button -------------------------------------------------#
  #------------------------------------------------------------------------------#
  # About: This section creates the 'Expand' button to allow the user to switch  #
  # between the small plot and the full screen plot. It uses the same icon as    #
  # the rest of the report and is solely there to increase the accessibility of  #
  # the plot.                                                                    #
  #------------------------------------------------------------------------------#
  
  ##########################################
  # Creating the plotly full screen button #
  ##########################################
  p <- build_fullscreen_button(p)
  
  ######################
  # Returning the plot #
  ######################
  return(p)
  
}