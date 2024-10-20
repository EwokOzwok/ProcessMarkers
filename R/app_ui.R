#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinyMobile
#' @import htmltools
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic

    shinyMobile::f7Page(
      title = "Process Markers",
      options = list(theme=c("auto"), dark=TRUE, preloader = F,  pullToRefresh=F),
      allowPWA=TRUE,


      f7TabLayout(
        # panels are not mandatory. These are similar to sidebars
        navbar = f7Navbar(
          title= "Process Markers"),


        # f7Tabs is a special toolbar with included navigation
        f7Tabs(
          animated = F,
          id = "tabs",
          swipeable = F,
          style = c("strong"),

          f7Tab(
            tabName = "WelcomeTab",
            icon = f7Icon("house_fill"),
            active = T,
            hidden= T,
            f7Block(
              f7Shadow(
                intensity = 5,
                hover = TRUE,
                f7Card(
                  f7Align(h2("Guy's Study"),side=c("center")),
                  br(),
                  f7Align(h2("Enter the unique ID of the participant and press continue"),side=c("center")),
                  br(),
                  f7Text("partid", "Participant ID"),
                  br(),
                  f7Button("begin", "Start!"),
                  footer = NULL,
                  hairlines = F, strong = T, inset = F, tablet = FALSE)))
          ),


          f7Tab(
            tabName = "ParticipantStart",
            icon = NULL,
            active = F,
            hidden=T,
            br(),
            uiOutput("participant_start_info")

          ),

          f7Tab(
            tabName = "TimerTab",
            icon = NULL,
            active = F,
            hidden=T,
            br(),
            uiOutput("countdown"),

          ),

          f7Tab(
            tabName = "ProvideData",
            icon = NULL,
            active = F,
            hidden=T,
            br(),
            uiOutput("DataCollection"),

          )


        )
      )






    )
  )

}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  # add_resource_path(
  #   "www",
  #   app_sys("./www"),
  # )

  tags$head(

    # favicon -----------------------------------------------------------------
    favicon(),
    # bundle_resources(
    #   path = app_sys("./www"),
    #   app_title = "MHScreener"),
    # includeCSS("./www/newcss.css"),


    HTML('<link rel="stylesheet" type="text/css" href="https://ewokozwok.github.io/MHScreener/www/framework7.bundle.min.css">')



    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()

  )

}
