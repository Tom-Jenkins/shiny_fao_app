# --------------------------- #
#
# FAO Shiny App
#
# Description: 
# Author: Tom Jenkins
# Organisation: Natural England
# Email: Tom.Jenkins@naturalengland.org.uk
# GitHub: https://github.com/Tom-Jenkins
#
# --------------------------- #

# Selector box UI ####
selector_box = function(helpID, speciesSelectorID, speciesSelectorTypeID, speciesSelectorNames,
                        countrySelectorID, clearCountryID, displayDataID, scaleID, flags){
  # Box parameters
  box(
    width = 12,
    title = "Selectors",
    solidHeader = TRUE,
    status = "info",
    collapsible = TRUE,
    useAwn(),
    # Maxheight and scrollable Help popup modal
    tags$head(
      tags$style(
        ".awn-popup-modal-awn{max-height: 500px; overflow-y: scroll; }"
      )
    ),
    # Help popup model action button
    actionBttn(
      inputId = helpID,
      label = "Help",
      icon = icon("question"),
      style = "gradient",
      color = "primary",
      size = "xs"
    ),
    br(),
    br(),
    # Species selector
    # https://shiny.rstudio.com/articles/selectize.html
    tippy::with_tippy(
      selectizeInput(
        inputId = speciesSelectorID,
        label = "1. Select up to four species",
        # selected = "European lobster",
        choices = c("Choose" = "", speciesSelectorNames),
        options = list(
          maxOptions = 5,
          maxItems = 4
        )
      ),
      tooltip = "Type a species name"
    ),
    # Species selector type button
    awesomeRadio(
      inputId = speciesSelectorTypeID,
      label = NULL,
      choices = c("Common name","Scientific name"),
      selected = "Common name",
      inline = TRUE
    ),
    br(),
    # Country multi-selector widget
    shinyWidgets::multiInput(
      inputId = countrySelectorID,
      label = "2. Filter by country (optional)",
      choices = NULL,
      choiceNames = lapply(
        seq_along(flags$Country),
        function(x) tagList(tags$img(src = flags$Flag_src[x],
                                     width = 20, height = 15),
                            flags$Country[x])
      ),
      choiceValues = flags$Country,
      options = list(
        limit = 20,
        enable_search = TRUE,
        non_selected_header = "Countries:",
        selected_header = "You have selected:"
      )
    ),
    # Clear all countries action button
    actionButton(clearCountryID, "Deselect All"),
    br(),
    br(),
    # Axis scale options selector
    awesomeRadio(
      inputId = scaleID,
      label = "Axes scale for multiple plots:",
      choices = c("Fixed","Free"),
      selected = "Fixed",
      inline = TRUE
      )
    )
}


# Function to slightly move x and y axis titles in ggplotly ####
# https://github.com/plotly/plotly.R/issues/1224
layout_ggplotly = function(gg, x = -0.02, y = -0.08){
  # The 1 and 2 goes into the list that contains the options for the x and y axis labels respectively
  gg[['x']][['layout']][['annotations']][[1]][['y']] = x
  gg[['x']][['layout']][['annotations']][[2]][['x']] = y
  gg
}

# Custom dashboardUser() function from shinydashboardPlus package ####
# https://github.com/RinteRface/shinydashboardPlus/blob/master/R/dashboardHeader.R
dashboardTopright = function(..., nameMenu = NULL, image = NULL,
                             headerTitle = NULL, footer = NULL){
  
  # Label in rop-right corner
  name = nameMenu
  
  # Header content
  headerContent = div(
    headerTitle,
    tags$img(src = image, width = "40px", height = "40px")
    )
  
  # Main info for top-right text
  bodyTopright = tagList(
    
    # Menu toggle button
    tags$a(
      href = "#", 
      class = "dropdown-toggle", 
      `data-toggle` = "dropdown",
      # Place image and name in dashboardHeader
      tags$img(src = image, class = "user-image", alt = ""),
      tags$span(class = "hidden-xs", name)
      ),
    # Menu dropdown header content
    tags$ul(
      class = "dropdown-menu dashboard-user",
      tags$li(
        class = "user-header",
        # Add coloured box at the top of menu
        tags$script(
          HTML('$(".user-header").css("height", "60px")')
        ),
        # Add images or text to header
        # tags$img(src = image, class = "img-circle", alt = ""),
        if(!is.null(headerContent)) headerContent
      ),
      # Menu dropdown body content
      if(length(list(...)) > 0) tags$li(class = "user-body", ...),
      # Menu dropdown footer content
      if(!is.null(footer)) tags$li(class = "user-footer", footer)
      )
    )
  bodyTopright
}

