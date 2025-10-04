mod_scrna_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    h2("scRNA-seq"),
    p("Placeholder"),
    tags$hr(),
    sidebarLayout(
      sidebarPanel(
        p(em("placeholder"))
      ),
      mainPanel(
        plotOutput(ns("plot"), height = "400px"),
        tableOutput(ns("table"))
      )
    )
  )
}

mod_scrna_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$plot <- renderPlot({
      ggplot() + theme_minimal() + labs(title = "placeholder")
    })
    output$table <- renderTable(NULL)
  })
}
