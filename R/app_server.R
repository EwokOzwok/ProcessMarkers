#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinyMobile
#' @import dplyr
#' @import googlesheets4
#' @importFrom shinyalert shinyalert
#' @noRd
app_server <- function(input, output, session) {
  # Initial Setup
  sheet_id <- googledrive::drive_get("ProcessMarkers_Guy")$id
  data_collection_timepoint <- reactiveVal(1)  # Start at 1

  participant_data <- reactiveValues(
    df = data.frame(SessionTimePoint = integer(),
                    PARTID = character(),
                    M1 = logical(),
                    M2 = logical(),
                    M3 = logical(),
                    M4 = logical(),
                    M5 = logical(),
                    stringsAsFactors = FALSE)  # Start with an empty data frame
  )

  timer <- reactiveVal(5)
  active <- reactiveVal(FALSE)

  # When "Start" button is clicked
  observeEvent(input$begin, {
    req(input$partid)

    participant_data$df <- data.frame(SessionTimePoint = integer(),
                                      PARTID = character(),
                                      M1 = logical(),
                                      M2 = logical(),
                                      M3 = logical(),
                                      M4 = logical(),
                                      M5 = logical(),
                                      stringsAsFactors = FALSE)  # Re-initialize






    output$participant_start_info <- renderUI({
      tagList(
        f7Block(
          f7Shadow(
            intensity = 5,
            hover = TRUE,
            f7Card(
              f7Align(h2("Participant Instructions:"), side = c("center")),
              br(),
              f7Align(h3("When instructed, press the continue button below:"), side = c("center")),
              br(),
              f7Button("participant_start_button", "Continue"),
              footer = NULL,
              hairlines = FALSE, strong = TRUE, inset = FALSE, tablet = FALSE
            )
          )
        )
      )
    })
    updateF7Tabs(session = session, id = "tabs", selected = "ParticipantStart")
  })

  # Start the countdown when participant presses "Start Timer"
  observeEvent(input$participant_start_button, {
    timer(5)
    active(TRUE)
    updateF7Tabs(session = session, id = "tabs", selected = "TimerTab")
  })




  observe({
    invalidateLater(1000, session)
    isolate({
      if (active()) {
        time_remaining <- timer()
        output$countdown <- renderUI({
          tagList(
            f7Block(
              f7Shadow(
                intensity = 5,
                hover = TRUE,
                f7Card(
                  f7Align(h1(paste("Identify Markers in", time_remaining, "seconds", sep=" ")), side=c("center")),
                  footer = NULL,
                  hairlines = F, strong = T, inset = F, tablet = FALSE)))
          )
        })
        timer(timer() - 1)
        if (timer() == 0) {
          output$countdown <- renderUI({
            tagList(
              f7Block(
                f7Shadow(
                  intensity = 5,
                  hover = TRUE,
                  f7Card(
                    f7Align(h1("Click Identify Markers to provide data"), side=c("center")),
                    br(),
                    f7Button("provide_data_button", "Identify Markers"),
                    footer = NULL,
                    hairlines = F, strong = T, inset = F, tablet = FALSE)))
            )
          })
          active(FALSE)
        }
      }
    })
  })

  observeEvent(input$provide_data_button, {
    updateF7Tabs(session, id = "tabs", selected = "ProvideData")
  })





  # Render the data collection UI
  output$DataCollection <- renderUI({
    tagList(
      f7Block(
        f7Shadow(
          intensity = 5,
          hover = TRUE,
          f7Card(
            f7Align(h2("Select all the markers that you observed in the past minute"), side = c("center")),
            br(),
            f7Checkbox("marker1", "Marker1"),
            br(),
            f7Checkbox("marker2", "Marker2"),
            br(),
            f7Checkbox("marker3", "Marker3"),
            br(),
            f7Checkbox("marker4", "Marker4"),
            br(),
            f7Checkbox("marker5", "Marker5"),
            br(),
            h4("Press 'Done' when finished"),
            f7Button('submit_data', "Done"),
            footer = NULL,
            hairlines = F, strong = T, inset = F, tablet = FALSE)))
    )
  })

  # Handle the data submission and reset the process
  observeEvent(input$submit_data, {
    timepoint <- data_collection_timepoint()  # Get the current timepoint

    # Create a new row of data
    new_row <- data.frame(
      SessionTimePoint = timepoint,  # Use the current timepoint
      PARTID = input$partid,
      M1 = input$marker1,
      M2 = input$marker2,
      M3 = input$marker3,
      M4 = input$marker4,
      M5 = input$marker5,
      stringsAsFactors = FALSE
    )

    # Append the new row to the participant data
    participant_data$df <- rbind(participant_data$df, new_row)

    # Append the new row to the Google Sheet
    sheet_id <- googledrive::drive_get("ProcessMarkers_Guy")$id  # Get the sheet ID
    googlesheets4::sheet_append(ss = sheet_id, data = new_row)

    # Printing the dataframe as a test
    test_dataframe <- participant_data$df
    print(test_dataframe)

    # Reset checkbox values
    updateCheckboxInput(session, "marker1", value = FALSE)
    updateCheckboxInput(session, "marker2", value = FALSE)
    updateCheckboxInput(session, "marker3", value = FALSE)
    updateCheckboxInput(session, "marker4", value = FALSE)
    updateCheckboxInput(session, "marker5", value = FALSE)

    # Increment the timepoint after appending the data
    data_collection_timepoint(data_collection_timepoint() + 1)

    # Reset timer and switch back to TimerTab
    timer(5)
    active(TRUE)
    updateF7Tabs(session = session, id = "tabs", selected = "TimerTab")
  })

}

