# --------------------------- #
#
# FAO Shiny App
#
# Description: 
# Author: Tom Jenkins
# Organisation: Natural England
# Email: tom.l.jenkins@outlook.com
# GitHub: https://github.com/Tom-Jenkins
#
# --------------------------- #

# Load libraries
library(shiny)
library(shinyWidgets)
library(awn)
library(shinydashboard)
library(shinydashboardPlus)
library(fresh)
library(htmltools)
library(tippy)
library(vroom)
library(dplyr)
library(ggplot2)
library(stringr)
library(purrr)
library(plotly)
library(DT)
library(randomcoloR)
options(dplyr.summarise.inform = FALSE)

# Import myfunctions
source("data/myfunctions.R")

# Import data
fao_capture = vroom("data/fao_capture_preprocessed.csv", show_col_types = FALSE)
fao_aquaculture = vroom("data/fao_aquaculture_quantity_preprocessed.csv", show_col_types = FALSE)

# Vectors
capture_common_names = unique(fao_capture$Common_name) %>% sort
capture_scientific_names = unique(fao_capture$Scientific_name) %>% sort
aqua_common_names = unique(fao_aquaculture$Common_name) %>% sort
aqua_scientific_names = unique(fao_aquaculture$Scientific_name) %>% sort

# Flags
flags_df = vroom("data/flags.csv", show_col_types = FALSE) %>% arrange(Country)
flags_df

# Custom ggplot theme
custom_theme = theme(
  panel.border = element_rect(fill = NA, colour = "black", size = 0.5, linetype = 1),
  panel.background = element_rect(fill = NA),
  panel.grid = element_line(colour = "grey90", size = 0.5),
  strip.text = element_text(size = 14)
)

# Date updated
date_updated = "07 January 2022"



# ----------------- #
#
# *** User Interface *** ####
#
# ----------------- #

# UI Header
ui_header = dashboardHeader(title = "FAO Dashboard",
                            controlbarIcon = NULL,
                            userOutput("about")
)

# UI Sidebar
ui_sidebar = dashboardSidebar(
  collapsed = FALSE,
  minified = FALSE,
  sidebarMenu(
    # Menu item
    shinydashboard::menuItem("Capture Production", tabName = "capture_menu", icon = icon("fish")),
    # Menu item
    shinydashboard::menuItem("Aquaculture Production", tabName = "aquaculture_menu", icon = icon("water"))
    # Menu item
    # shinydashboard::menuItem("Capture vs. Aquaculture", tabName = "versus_menu", icon = icon("water"))
  )
)

# UI Body ####
ui_body = dashboardBody(
  tabItems(
    # ___ Capture Production UI ___ ####
    tabItem(
      tabName = "capture_menu",
      fluidRow(
        column(
          width = 3,
          # Selector box
          selector_box(
            helpID = "capture_help",
            speciesSelectorID = "capture_species",
            speciesSelectorTypeID = "capture_speciesType",
            speciesSelectorNames = capture_common_names,
            countrySelectorID = "capture_country",
            clearCountryID = "capture_clearCountry",
            scaleID = "capture_scale",
            flags = flags_df
          ),
          # Total Capture Production box 
          box(
            width = 12,
            solidHeader = TRUE,
            status = "info",
            title = "Total Capture Production",
            collapsible = TRUE,
            # Value box renderUI
            plotOutput("capture_total_plt", height = "250px"),
            br(),
            # Selector
            uiOutput("capture_total_selector"),
            # Slider
            chooseSliderSkin("Shiny", color = "#00c0ef"),
            sliderInput(
              inputId = "capture_infostat_slider",
              label = "Filter by year:",
              min = 1950,
              max = 2019,
              value = 2019,
              sep = ""
            ),
            uiOutput("capture_production_valuebox"),
            footer = "Source: Food and Agriculture Organization of the United Nations"
          )
        ),
        column(
          width = 9,
          # Capture Production by Country box
          box(
            width = 12,
            collapsible = TRUE,
            # Style of "myValidate" warning message
            tags$head(
              tags$style(
                HTML(".shiny-output-error-myValidate { color: red; font-size: 15px; font-weight: bold; }")
              )
            ),
            title = "Capture Production by Country",
            solidHeader = TRUE,
            status = "info",
            plotlyOutput(outputId = "capture_plt_country", height = "600px"),
            switchInput(
              inputId = "capture_toggle_legend",
              label = "Legend",
              value = TRUE
            ),
            footer = "Source: Food and Agriculture Organization of the United Nations"
          ),
          # Download Raw Data box
          box(
            width = 12,
            collapsible = TRUE,
            title = "Download Raw Data",
            solidHeader = TRUE,
            status = "info",
            DTOutput("capturetableDT")
          )
        )
      )
    ),
    # ___ Aquaculture Production UI ___ ####
    tabItem(
      tabName = "aquaculture_menu",
      fluidRow(
        column(
          width = 3,
          # Selector box
          selector_box(
            helpID = "aqua_help",
            speciesSelectorID = "aqua_species",
            speciesSelectorTypeID = "aqua_speciesType",
            speciesSelectorNames = aqua_common_names,
            countrySelectorID = "aqua_country",
            clearCountryID = "aqua_clearCountry",
            scaleID = "aqua_scale",
            flags = flags_df
            ),
          # Total Capture Production box 
          box(
            width = 12,
            solidHeader = TRUE,
            status = "info",
            title = "Total Aquaculture Production",
            collapsible = TRUE,
            # Value box renderUI
            plotOutput("aqua_total_plt", height = "250px"),
            br(),
            # Selector
            uiOutput("aqua_total_selector"),
            # Slider
            chooseSliderSkin("Shiny", color = "#00c0ef"),
            sliderInput(
              inputId = "aqua_infostat_slider",
              label = "Filter by year:",
              min = 1950,
              max = 2019,
              value = 2019,
              sep = ""
            ),
            uiOutput("aqua_production_valuebox"),
            footer = "Source: Food and Agriculture Organization of the United Nations"
            )
          ),
        column(
          width = 9,
          # Aquaculture Production by Country box
          box(
            width = 12,
            collapsible = TRUE,
            tags$head(
              tags$style(
                HTML(".shiny-output-error-myValidate { color: red; font-size: 15px; font-weight: bold; }")
                )
              ),
            title = "Aquaculture Production by Country",
            solidHeader = TRUE,
            status = "info",
            plotlyOutput(outputId = "aqua_plt_country", height = "600px"),
            switchInput(
              inputId = "aqua_toggle_legend",
              label = "Legend",
              value = TRUE
              ),
            footer = "Source: Food and Agriculture Organization of the United Nations"
            ),
          # Download Raw Data box
          box(
            width = 12,
            collapsible = TRUE,
            title = "Download Raw Data",
            solidHeader = TRUE,
            status = "info",
            DTOutput("aquatableDT")
            )
          )
        )
      )
    )
  )


# Customise theme using fresh package
# https://dreamrs.github.io/fresh/articles/vars-shinydashboard.html
fresh_theme = create_theme(
  adminlte_color(
    # Primary status
    light_blue = "#3c9dbc",
    # Danger status
    red = "#dd4b39",
    # Success status
    green = "#00a65a",
    # Info status,
    aqua = "#00c0ef",
    # Warning status
    yellow = "#f39c12"
  )
)

# UI Dashboard Page ####
ui = dashboardPage(
  header = ui_header,
  sidebar = ui_sidebar,
  freshTheme = fresh_theme,
  body = ui_body
)



# ----------------- #
#
# *** Server *** ####
#
# ----------------- #

# Server
server = function(input, output, session){
  
  # ----------------- #
  #
  # __ Capture Production Server __ ####
  #
  # ----------------- #
  
  # Convert species selector type from spaces to underscore
  capture_name_type = eventReactive(input$capture_speciesType, {
    str_replace(input$capture_speciesType, " ", "_")
  })
  
  # capt_df1: Filter capture data.frame when species selector is updated
  capt_df1 = reactive({
    
    fao_capture %>%
      dplyr::filter(.data[[capture_name_type()]] %in% input$capture_species) %>% 
      group_by(.data[[capture_name_type()]], PERIOD, Country) %>%
      summarise(VALUE = round(sum(VALUE), digits = 1))
    }) %>% 
    # Use bindCache() to store the reactive results of user queries
    bindCache(capture_name_type(), input$capture_species)
  
  # capt_df2: Filter capt_df1 data.frame when country selector is updated
  capt_df2 = reactive({
    
    # If no countries are selected then capt_df2 == capt_df1
    if (length(input$capture_country) == 0){
      capt_df1()
      }
    # If countries are selected filter capt_df1 and store results in capt_df2
    else {
      capt_df1() %>% 
        dplyr::filter(Country %in% input$capture_country)
      }
    }) %>% 
    # Use bindCache() to store the reactive results of user queries
    bindCache(capt_df1(), input$capture_country)
  
  # Update axis scale argument when capture axis scale selector is updated
  capture_scale_arg = eventReactive(input$capture_scale, {
    
    # If "Free" is selected return "free_y" otherwise return "fixed"
    ifelse(test = tolower(input$capture_scale) == "Free",
           yes = "free_y",
           no = tolower(input$capture_scale))
    })
  
  # Render Capture Production by Country plotly object
  output$capture_plt_country = renderPlotly({
    
    # Only proceed with action when a species name is selected
    req(input$capture_species)
    
    # Message to display if invalid countries are selected
    validate(
      need(nrow(capt_df2()) > 0, "Please select a valid country to display"),
      # Style defined in UI
      errorClass = "myValidate"
    )
    
    # Create ggplot2 object and convert to a plotly object
    ggplotly(
      ggplot(dplyr::rename(capt_df2(), Year = PERIOD, Production = VALUE))+
        geom_line(aes(x = Year, y = Production, col = Country), show.legend = TRUE)+
        facet_wrap(~.data[[capture_name_type()]], scales = capture_scale_arg(), ncol = 2)+
        xlab("Year")+
        ylab("Production")+
        # scale_colour_manual(values = distinctColorPalette(k = n_distinct(capt_df2()$Country)))+
        custom_theme
      ) %>%
      # Plotly customisation
      layout(
        # Axis arguments
        # yaxis = list(hoverformat = '.1f'),
        # Legend toggle on / off
        showlegend = input$capture_toggle_legend,
        # Legend arguments
        legend = list(
          orientation = "v",
          itemsizing = "trace",
          itemwidth = 30,
          font = list(
            size = 11
          ),
          # Legend title in bold
          title = list(
            text = "<b> Country </b><br>"
          )
        )
      ) %>%
      # Function to slightly move x and y axis titles in ggplotly
      # https://github.com/plotly/plotly.R/issues/1224
      layout_ggplotly(x = 0, y = -0.011)
    }) %>% 
    # Use bindCache() to store the reactive results of user queries
    bindCache(capt_df2(), input$capture_species, capture_name_type(), capture_scale_arg(), input$capture_toggle_legend)
  
  # Create Total Capture Production data.frame
  total_capture = reactive({
    req(input$capture_species)
    # Group by species and year then sum production
    capt_df2() %>% 
      group_by(.data[[capture_name_type()]], PERIOD) %>% 
      summarise(VALUE = sum(VALUE))
    })
  
  # Render Total Capture Production line graph
  output$capture_total_plt = renderPlot({
    
    ggplot(total_capture())+
      geom_line(aes(x = PERIOD, y = VALUE,
                    col = .data[[capture_name_type()]],
                    linetype = .data[[capture_name_type()]]))+
      scale_colour_manual(values = c("black","grey50","black","grey50"))+
      scale_linetype_manual(values = c(1,1,2,2))+
      # scale_x_continuous(expand = c(0,0))+
      theme(
        axis.title = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        plot.margin = margin(r = 10)
      )+
      guides(color = guide_legend(nrow = 2, byrow = TRUE))
    })
  
  # Render selectInput UI for Total Capture Production
  output$capture_total_selector = renderUI({
    selectInput(
      inputId = "capture_TotalSelectorID",
      label = "Filter by species:",
      selected = input$capture_species[1],
      choices = c("Choose" = "", input$capture_species)
      )
    })
  
  # Render valueBox based on species selected and yearly Total Capture Production
  output$capture_production_valuebox = renderUI({
    valueBox(
      width = 12,
      value = total_capture() %>%
        # Filter by the species selected
        dplyr::filter(.data[[capture_name_type()]] == input$capture_TotalSelectorID) %>%
        # Filter by the year selected
        dplyr::filter(PERIOD == input$capture_infostat_slider) %>%
        # Extract column as a vector and sum the production
        pull(VALUE) %>% 
        sum %>% 
        round(digits = 0),
      subtitle = paste("Total capture production of", input$capture_TotalSelectorID, "in", input$capture_infostat_slider)
      )
    })
  
  # Render common or scientific names selector based on species_selector_type
  observeEvent(input$capture_speciesType,
               # Do not execute on app start up
               ignoreInit = T, priority = 1, {
                 
                 # Freeze reactive to allow all observers to update first
                 # This removes a brief error flicker in plot rendering
                 freezeReactiveValue(input, "capture_species")  
                 
                 # Update the species selector
                 if (input$capture_speciesType == "Common name"){
                   updateSelectizeInput(
                     session,
                     inputId = "capture_species",
                     # selected = "European lobster",
                     choices = c("Choose" = "", capture_common_names)
                   )
                 } else if (input$capture_speciesType == "Scientific name"){
                   updateSelectizeInput(
                     session,
                     inputId = "capture_species",
                     # selected = "Homarus gammarus",
                     choices = c("Choose" = "", capture_scientific_names)
                   )
                 }
               })
  
  # Clear all countries from selector
  observeEvent(input$capture_clearCountry, {
    updateMultiInput(
      session = session,
      inputId = "capture_country",
      selected = character(0)
      )
    })
  
  # Create data.frame for DT table
  captureDT_reactive = reactive({
    req(input$capture_species)
    
    capt_df2() %>%
      set_names(c("Species","Year","Country","Capture Production")) %>%
      mutate(Species = as.factor(Species),
             Year = as.integer(Year),
             Country = as.factor(Country),
             # numeric > round > integer conversion needed to remove decimal places in column filtering
             `Capture Production` = round(as.numeric(`Capture Production`), digits = 1)
             )
    })
  
  # Render DT table
  output$capturetableDT = renderDT(server = FALSE, {
    DT::datatable(
      captureDT_reactive(),
      rownames = FALSE,
      filter = "top",
      extensions = "Buttons",
      options = list(
        # left align Year column
        columnDefs = list(list(className = "dt-left", targets = 1)),
        # scrollX = TRUE,
        autowidth = TRUE,
        paging = TRUE,
        dom = "Bfrtip",
        buttons = list(
          # server = FALSE allows users to copy all records
          list(extend = "copy", title = NULL),
          # options: filename = X, text = X, 
          list(extend = "csv"),
          list(extend = "excel", title = NULL)
          )
        )
      ) %>% 
      formatRound(columns = "Capture Production", digits = 1)
    })
  
  # __ Capture Help button __
  observeEvent(input$capture_help, {
    # Show a modal when the help button is pressed
    awn::modal(
      div(
        # icon("question-circle"),
        h4("Step 1: Select up to four species"),
        p("Start typing the name of a species in the input box and options will appear
          to help you select your species of interest. You can search by common name
          or by scientific name (but not both at the same time). Once selected,
          the app will update the dashboard graphics and stats."),
        p("You may select up to four species which will display multiple plots in the
          plotting panel and will update both the total and table panels. You can also
          change the axis scale:"),
        p(strong("Fixed axis"),
          br(),
          "All plots have the same axis scale which is useful for comparing the
          levels of capture production across multiple species."
        ),
        p(strong("Free axis"),
          br(),
          "All plots have independent axis scales which is useful for comparing trends
          of capture production across multiple species."
        ),
        br(),
        h4("Step 2: Filter by country (optional)"),
        p("Use the filter by country widget to select and display only countries
          of interest. This filter will update all panels. You can deselect all
          countries by clicking the Deselect All button. If no countries are selected
          then all countries are displayed by default."
        ),
        br(),
        h4("Additional information"),
        p("The total capture production panel will sum the production for species
          and countries selected. Use the selector and slider to display the total amount of
          capture production for a particular species and year."
        ),
        p("The unit of measurement for the majority of species is tonnes (live weight).
          However, aquatic mammals, alligators and crocodiles are measured by the
          number of animals. Click", tags$a("here", href = "https://www.fao.org/fishery/en/collection/capture?lang=en"),
          "for more information."
        ),
        p("The download table panel will display the raw data after all selections.
          Users are able to apply additional filters to these tabular data
          before copying or exporting data as csv or excel files.")
        )
      )
    })
  
  
  # ----------------- #
  #
  # __ Aquaculture Production Server __ ####
  #
  # ----------------- #
  
  # Convert species selector type from spaces to underscore
  aqua_name_type = eventReactive(input$aqua_speciesType, {
    str_replace(input$aqua_speciesType, " ", "_")
  })
  
  # aqua_df1: Filter Aquaculture data.frame when species selector is updated
  aqua_df1 = reactive({
    
    fao_aquaculture %>%
      dplyr::filter(.data[[aqua_name_type()]] %in% input$aqua_species) %>% 
      group_by(.data[[aqua_name_type()]], PERIOD, Country) %>%
      summarise(VALUE = round(sum(VALUE), digits = 1))
    }) %>% 
    # Use bindCache() to store the reactive results of user queries
    bindCache(aqua_name_type(), input$aqua_species)
  
  # aqua_df2: Filter aqua_df1 data.frame when country selector is updated
  aqua_df2 = reactive({
    
    # If no countries are selected then aqua_df2 == aqua_df1
    if (length(input$aqua_country) == 0){
      aqua_df1()
    }
    # If countries are selected filter aqua_df1 and store results in aqua_df2
    else {
      aqua_df1() %>% 
        dplyr::filter(Country %in% input$aqua_country)
      }
    }) %>% 
    # Use bindCache() to store the reactive results of user queries
    bindCache(aqua_df1(), input$aqua_country)
  
  # Update axis scale argument when aqua axis scale selector is updated
  aqua_scale_arg = eventReactive(input$aqua_scale, {
    
    # If "Free" is selected return "free_y" otherwise return "fixed"
    ifelse(test = tolower(input$aqua_scale) == "Free",
           yes = "free_y",
           no = tolower(input$aqua_scale))
  })
  
  # Render Aquaculture Production by Country plotly object
  output$aqua_plt_country = renderPlotly({
    
    # Only proceed with action when a species name is selected
    req(input$aqua_species)
    
    # Message to display if invalid countries are selected
    validate(
      need(nrow(aqua_df2()) > 0, "Please select a valid country to display"),
      # Style defined in UI
      errorClass = "myValidate"
    )
    
    # Create ggplot2 object and convert to a plotly object
    ggplotly(
      ggplot(dplyr::rename(aqua_df2(), Year = PERIOD, Production = VALUE))+
        geom_line(aes(x = Year, y = Production, col = Country), show.legend = TRUE)+
        facet_wrap(~.data[[aqua_name_type()]], scales = aqua_scale_arg(), ncol = 2)+
        xlab("Year")+
        ylab("Production")+
        # scale_colour_manual(values = distinctColorPalette(k = n_distinct(aqua_df2()$Country)))+
        custom_theme
      ) %>%
      # Plotly customisation
      layout(
        # Axis arguments
        # yaxis = list(hoverformat = '.1f'),
        # Legend toggle on / off
        showlegend = input$aqua_toggle_legend,
        # Legend arguments
        legend = list(
          orientation = "v",
          itemsizing = "trace",
          itemwidth = 30,
          font = list(
            size = 11
          ),
          # Legend title in bold
          title = list(
            text = "<b> Country </b><br>"
          )
        )
      ) %>%
      # Function to slightly move x and y axis titles in ggplotly
      # https://github.com/plotly/plotly.R/issues/1224
      layout_ggplotly(x = 0, y = -0.011)
  }) %>% 
    # Use bindCache() to store the reactive results of user queries
    bindCache(aqua_df2(), input$aqua_species, aqua_name_type(), aqua_scale_arg(), input$aqua_toggle_legend)
  
  # Create Total Aquaculture Production data.frame
  total_aqua = reactive({
    req(input$aqua_species)
    # Group by species and year then sum production
    aqua_df2() %>% 
      group_by(.data[[aqua_name_type()]], PERIOD) %>% 
      summarise(VALUE = sum(VALUE))
    })
  
  # Render Total Aquaculture Production line graph
  output$aqua_total_plt = renderPlot({
    
    ggplot(total_aqua())+
      geom_line(aes(x = PERIOD, y = VALUE,
                    col = .data[[aqua_name_type()]],
                    linetype = .data[[aqua_name_type()]]))+
      scale_colour_manual(values = c("black","grey50","black","grey50"))+
      scale_linetype_manual(values = c(1,1,2,2))+
      # scale_x_continuous(expand = c(0,0))+
      theme(
        axis.title = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        plot.margin = margin(r = 10)
        )+
      guides(color = guide_legend(nrow = 2, byrow = TRUE))
    })
  
  # Render selectInput UI for Aquaculture Production
  output$aqua_total_selector = renderUI({
    selectInput(
      inputId = "aqua_TotalSelectorID",
      label = "Filter by species:",
      selected = input$aqua_species[1],
      choices = c("Choose" = "", input$aqua_species)
    )
  })
  
  # Render valueBox based on species selected and yearly Total aqua Production
  output$aqua_production_valuebox = renderUI({
    valueBox(
      width = 12,
      value = total_aqua() %>%
        # Filter by the species selected
        dplyr::filter(.data[[aqua_name_type()]] == input$aqua_TotalSelectorID) %>%
        # Filter by the year selected
        dplyr::filter(PERIOD == input$aqua_infostat_slider) %>%
        # Extract column as a vector and sum the production
        pull(VALUE) %>% 
        sum %>% 
        round(digits = 0),
      subtitle = paste("Total aquaculture production of", input$aqua_TotalSelectorID, "in", input$aqua_infostat_slider)
      )
    })
  
  # Render common or scientific names selector based on species_selector_type
  observeEvent(input$aqua_speciesType,
               # Do not execute on app start up
               ignoreInit = T, priority = 1, {
                 
                 # Freeze reactive to allow all observers to update first
                 # This removes a brief error flicker in plot rendering
                 freezeReactiveValue(input, "aqua_species")  
                 
                 # Update the species selector
                 if (input$aqua_speciesType == "Common name"){
                   updateSelectizeInput(
                     session,
                     inputId = "aqua_species",
                     # selected = "European lobster",
                     choices = c("Choose" = "", aqua_common_names)
                   )
                 } else if (input$aqua_speciesType == "Scientific name"){
                   updateSelectizeInput(
                     session,
                     inputId = "aqua_species",
                     # selected = "Homarus gammarus",
                     choices = c("Choose" = "", aqua_scientific_names)
                   )
                 }
               })
  
  # Clear all countries from selector
  observeEvent(input$aqua_clearCountry, {
    updateMultiInput(
      session = session,
      inputId = "aqua_country",
      selected = character(0)
      )
    })
  
  # Create data.frame for DT table
  aquaDT_reactive = reactive({
    req(input$aqua_species)
    
    aqua_df2() %>%
      set_names(c("Species","Year","Country","Aquaculture Production")) %>%
      mutate(Species = as.factor(Species),
             Year = as.integer(Year),
             Country = as.factor(Country),
             # numeric > round > integer conversion needed to remove decimal places in column filtering
             `Aquaculture Production` = round(as.numeric(`Aquaculture Production`), digits = 1)
             )
    })
  
  # Render DT table
  output$aquatableDT = renderDT(server = FALSE, {
    DT::datatable(
      aquaDT_reactive(),
      rownames = FALSE,
      filter = "top",
      extensions = "Buttons",
      options = list(
        # left align Year column
        columnDefs = list(list(className = "dt-left", targets = 1)),
        # scrollX = TRUE,
        autowidth = TRUE,
        paging = TRUE,
        dom = "Bfrtip",
        buttons = list(
          # server = FALSE allows users to copy all records
          list(extend = "copy", title = NULL),
          # options: filename = X, text = X, 
          list(extend = "csv"),
          list(extend = "excel", title = NULL)
          )
        )
      ) %>% 
      formatRound(columns = "Aquaculture Production", digits = 1)
    })
  
  # __ aqua Help button __
  observeEvent(input$aqua_help, {
    # Show a modal when the help button is pressed
    awn::modal(
      div(
        # icon("question-circle"),
        h4("Step 1: Select up to four species"),
        p("Start typing the name of a species in the input box and options will appear
          to help you select your species of interest. You can search by common name
          or by scientific name (but not both at the same time). Once selected,
          the app will update the dashboard graphics and stats."),
        p("You may select up to four species which will display multiple plots in the
          plotting panel and will update both the total and table panels. You can also
          change the axis scale:"),
        p(strong("Fixed axis"),
          br(),
          "All plots have the same axis scale which is useful for comparing the
          levels of aquaculture production across multiple species."
        ),
        p(strong("Free axis"),
          br(),
          "All plots have independent axis scales which is useful for comparing trends
          of aqua production across multiple species."
        ),
        br(),
        h4("Step 2: Filter by country (optional)"),
        p("Use the filter by country widget to select and display only countries
          of interest. This filter will update all panels. You can deselect all
          countries by clicking the Deselect All button. If no countries are selected
          then all countries are displayed by default."
        ),
        br(),
        h4("Additional information"),
        p("The total aquaculture production panel will sum the production for species
          and countries selected. Use the selector and slider to display the total amount of
          aquaculture production for a particular species and year."
        ),
        p("The unit of measurement for the majority of species is tonnes (live weight).
          However, aquatic mammals, alligators and crocodiles are measured by the
          number of animals. Click", tags$a("here", href = "https://www.fao.org/fishery/en/collection/aquaculture?lang=en"),
          "for more information."
        ),
        p("The download table panel will display the raw data after all selections.
          Users are able to apply additional filters to these tabular data
          before copying or exporting data as csv or excel files.")
      )
    )
  })

  
  # __ About This Dashboard __ ####
  output$about = renderUser({
    dashboardTopright(
      nameMenu = "About",
      image = "https://cdn-icons-png.flaticon.com/512/1041/1041728.png",
      fluidRow(
        shinydashboard::box(
          width = 12,
          div(
            strong("About this Dashboard"),
            p(
              "This dashboard enables users to visually interact with and
          download data from the Capture Production and Aquaculture Production data sets
          assembled by the Food and Agriculture Organization of the United Nations (FAO)."
            ),
            strong("Data sources"),
            p(
              "FAO: ",
              tags$a("Capture Production", href = "https://www.fao.org/fishery/en/collection/capture?lang=en"),
              br(),
              "FAO: ",
              tags$a("Aquaculture Production", href = "https://www.fao.org/fishery/en/collection/aquaculture?lang=en")
            ),
            strong("Disclaimer"),
            p(
              "This is not an official FAO dashboard."
            ),
            strong("Author"),
            p(
              tags$a("Tom Jenkins", href = "https://tomjenkins.netlify.app/"),
              "tom.l.jenkins@outlook.com"
            ),
            strong("Last updated"),
            p(
              date_updated
            )
          )
        )
      )
    )
  })
  
}
shinyApp(ui, server)

