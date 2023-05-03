
library(shiny)
library(dplyr)
library(DT)

# Load the dataset
source("prep_data.R")
source('globals.R')

# Define UI for the app
ui <- fluidPage(
  tags$head(tags$link(rel = "stylesheet", href = "https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css")),
  titlePanel("Measuring financial health: Examples from existing data collection instruments"),
  p("This tool allows users to explore questions that are used to measure various aspects of financial health in surveys around the world.\nYou can select questions to create a short-list that you can export as a pdf.", style = "font-size: 20px;"),
  sidebarLayout(
    sidebarPanel(
      selectInput("target_outcome", "Target Outcome:",
                  choices = unique(questionnaire_data$target_outcome), selected = "Managing expenses", multiple = TRUE),
      selectInput("source", "Source:",
                  choices = unique(questionnaire_data$source), selected = unique(questionnaire_data$source), multiple = TRUE),
      selectInput("sector", "Sector:",
                  choices = unique(questionnaire_data$sector), selected = unique(questionnaire_data$sector), multiple = TRUE),
      selectInput("objective_subjective", "Objective / Subjective:",
                  choices = unique(questionnaire_data$objective_subjective), selected = unique(questionnaire_data$objective_subjective), multiple = TRUE),
      textOutput("selected_items_count")
    ),

    mainPanel(
      tags$style(type="text/css",
                 ".bigfont { font-size: 16px; }",
                 ".smallfont { font-size: 12px; }"),
      tabsetPanel(
        tabPanel("Filtered Items",
                 br(),
                 textOutput("filtered_items_count"),
                 br(),
                 DTOutput("filtered_questionnaire_items")),
        tabPanel("Designed Survey",
                 br(),
                 DTOutput("designed_survey"))
      )
    )
  )
)

server <- function(input, output, session) {

  filtered_data <- reactive({
    questionnaire_data %>%
      filter(target_outcome %in% input$target_outcome,
             source %in% input$source,
             sector %in% input$sector,
             objective_subjective %in% input$objective_subjective)
  })

  output$filtered_items_count <- renderText({
    count <- nrow(filtered_data())
    paste("A total of", count, "questions match your search criteria", sep = " ")
  })

  observe({
    target_outcome_filtered_data <- questionnaire_data %>%
      filter(target_outcome %in% input$target_outcome)

    updateSelectInput(session, "source", choices = unique(target_outcome_filtered_data$source), selected = unique(target_outcome_filtered_data$source))
    updateSelectInput(session, "sector", choices = unique(target_outcome_filtered_data$sector), selected = unique(target_outcome_filtered_data$sector))
    updateSelectInput(session, "objective_subjective", choices = unique(target_outcome_filtered_data$objective_subjective), selected = unique(target_outcome_filtered_data$objective_subjective))
  })

  output$filtered_questionnaire_items <- renderDT({
    data <- filtered_data() %>%
      mutate(select_item = NA,
             questionnaire_item = paste0('<span class="bigfont">', questionnaire_item, '</span>'),
             source = paste0('<span class="smallfont">', source, '</span>')) %>%
      select(select_item, target_outcome, topic, questionnaire_item, source)

    cols <- intersect(names(data), collabels)
    labels <- setNames(names(collabels), collabels)[cols]

    datatable(data,
              rownames = FALSE,
              colnames = setNames(names(labels), labels),
              escape = FALSE,
              callback = JS('
                table.on("click.dt", "input[type=checkbox]", function() {
                  var rowIndex = $(this).closest("tr").index();
                  if (this.checked) {
                    table.row(rowIndex).select();
                  } else {
                    table.row(rowIndex).deselect();
                  }
                });
              '),
              options = list(iDisplayLength = 10,
                             columnDefs = list(list(targets = 0, data = NULL, title = "Select item",
                                                    defaultContent = "", orderable = FALSE,
                                                    className = "select-checkbox",
                                                    render = JS("function(data, type, row, meta) {
                                                      return '<input type=checkbox>';
                                                    }")),
                                               list(targets = "_all", searchable = FALSE)),
                             select = list(style = "none", selector = "td:first-child"))
  )
  }, server = FALSE)

observeEvent(input$filtered_questionnaire_items_rows_selected, {
  req(input$filtered_questionnaire_items_rows_selected)
  new_items <- filtered_data()[input$filtered_questionnaire_items_rows_selected, ]

  if (is.null(values$designed_survey)) {
    values$designed_survey <- new_items
  } else {
    values$designed_survey <- unique(rbind(values$designed_survey, new_items))
  }
})

values <- reactiveValues(designed_survey = NULL)

output$designed_survey <- renderDT({
  req(values$designed_survey)

  survey <- values$designed_survey %>%
    arrange(target_outcome, topic) %>%
    mutate(remove_row = '<i class="fa fa-trash"></i>',
           questionnaire_item = paste0('<span class="bigfont">', questionnaire_item, '</span>'),
           source = paste0('<span class="smallfont">', source, '</span>')) %>%
    select(remove_row, target_outcome, topic, questionnaire_item, source)

  cols <- intersect(names(survey), collabels)
  labels <- setNames(names(collabels), collabels)[cols]

  datatable(survey,
            rownames = FALSE,
            colnames = setNames(names(labels), labels),
            escape = FALSE,
            callback = JS('
                table.on("click", "td.remove-row", function() {
                  var rowIndex = $(this).closest("tr").index();
                  table.row(rowIndex).remove().draw(false);
                  Shiny.setInputValue("remove_row_index", rowIndex);
                });
              '),
            options = list(iDisplayLength = 10,
                           columnDefs = list(list(className = "remove-row", targets = 0, orderable = FALSE),
                                             list(targets = "_all", searchable = FALSE))))
}, server = FALSE)

observeEvent(input$remove_row_index, {
  req(input$remove_row_index)
  values$designed_survey <- values$designed_survey[-input$remove_row_index, ]
})

output$selected_items_count <- renderText({
  if (!is.null(values$designed_survey)) {
    n <- nrow(values$designed_survey)
  } else {
    n <- 0
    }
  paste("You have added", n, "items to the short-list")
})

}

# Run the app
shinyApp(ui = ui, server = server)

