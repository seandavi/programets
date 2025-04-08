#' Google analytics metadata browser
#'
#' This function retrieves Google Analytics metadata
#' and returns it as a dataframe.
#'
#' @family Google Analytics
#'
#' @examples
#' ga_meta_simple()
#'
#' @export
ga_meta_simple <- function() {
  # Authenticate with Google Analytics
  googleAnalyticsR::ga_auth()

  # Retrieve the metadata
  ga_metadata <- googleAnalyticsR::ga_meta() |>
    dplyr::filter(
      rlang::.data$status != "DEPRECATED",
    ) |>
    dplyr::select(
      rlang::.data$name,
      rlang::.data$type,
      rlang::.data$dataType,
      rlang::.data$group,
      rlang::.data$description
    )

  return(ga_metadata)
}

#' Launch Google Analytics metadata browser Shiny app
#'
#' This function creates a Shiny app that allows users to search and filter
#' Google Analytics metadata interactively.
#'
#' @param ... Additional parameters passed to shiny::runApp()
#'
#' @family Google Analytics
#'
#' @examples
#' \dontrun{
#' ga_metrics_and_dimensions_browser()
#' }
#'
#' @export
ga_metrics_and_dimensions_browser <- function(...) {
  # Check if required packages are installed
  if (!requireNamespace("shiny", quietly = TRUE) ||
    !requireNamespace("DT", quietly = TRUE)) {
    stop("Packages 'shiny' and 'DT' are required for this function. Please install them.")
  }

  # Get the metadata using the existing function
  ga_metadata <- ga_meta_simple()

  # Define unique values for filters
  types <- sort(unique(ga_metadata$type))
  dataTypes <- sort(unique(ga_metadata$dataType))
  groups <- sort(unique(ga_metadata$group))

  # Define UI
  ui <- shiny::fluidPage(
    shiny::titlePanel("Google Analytics Metadata Browser"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::textInput("search", "Search (name/description):", ""),
        shiny::selectInput("type", "Filter by Type:",
          choices = c("All", types), selected = "All"
        ),
        shiny::selectInput("dataType", "Filter by Data Type:",
          choices = c("All", dataTypes), selected = "All"
        ),
        shiny::selectInput("group", "Filter by Group:",
          choices = c("All", groups), selected = "All"
        ),
        shiny::hr(),
        shiny::helpText("This app allows you to browse and filter Google Analytics metadata.")
      ),
      shiny::mainPanel(
        DT::DTOutput("metadata_table")
      )
    )
  )

  # Define server logic
  server <- function(input, output, session) {
    # Filtered data based on inputs
    filtered_data <- shiny::reactive({
      data <- ga_metadata

      # Search filter (case insensitive)
      if (input$search != "") {
        search_pattern <- tolower(input$search)
        data <- data[grepl(search_pattern, tolower(data$name)) |
          grepl(search_pattern, tolower(data$description)), ]
      }

      # Type filter
      if (input$type != "All") {
        data <- data[data$type == input$type, ]
      }

      # Data Type filter
      if (input$dataType != "All") {
        data <- data[data$dataType == input$dataType, ]
      }

      # Group filter
      if (input$group != "All") {
        data <- data[data$group == input$group, ]
      }

      return(data)
    })

    # Render the table
    output$metadata_table <- DT::renderDT({
      DT::datatable(
        filtered_data(),
        options = list(
          pageLength = 15,
          searchHighlight = TRUE,
          searchDelay = 100
        ),
        filter = "top",
        rownames = FALSE
      )
    })
  }

  # Return the Shiny app
  shiny::shinyApp(ui, server, ...)
}

#' Browse google analytics query explorer
#'
#' This function opens the Google Analytics Query Explorer in a web browser.
#'
#' It allows users to explore and test Google Analytics queries interactively.
#' This is useful for developers and analysts who want to experiment with
#' different queries and see the results in real-time.
#'
#' @return Opens the Google Analytics Query Explorer in a web browser.
#'
#' @note This function is intended for users who have a Google Analytics account
#' and want to explore the API capabilities. It does not return any data or
#' perform any analysis.
#'
#' @family Google Analytics
#'
#' @examples
#' \dontrun{
#' ga_query_explorer()
#' }
#'
#' @export
ga_query_explorer <- function() {
  # Open the Google Analytics API Explorer in a web browser
  browseURL("https://ga-dev-tools.google/ga4/query-explorer/")
}
