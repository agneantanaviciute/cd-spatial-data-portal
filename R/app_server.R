app_server <- function(input, output, session) {

  started <- reactiveValues(
    visium = FALSE, x480 = FALSE, x5100 = FALSE, scrna = FALSE
  )
  
  observeEvent(input$main_nav, {
    switch(input$main_nav,
           "visium" = {
             if (!started$visium) {
               started$visium <- TRUE
               mod_visium_server("visium")
             }
           },
           "x480" = {
             if (!started$x480) {
               started$x480 <- TRUE
               mod_xenium480_server("x480")
             }
           },
           "x5100" = {
             if (!started$x5100) {
               started$x5100 <- TRUE
               mod_xenium5100_server("x5100")
             }
           },
           "scrna" = {
             if (!started$scrna) {
               started$scrna <- TRUE
               mod_scrna_server("scrna")
             }
           },
           { }
    )
  }, ignoreInit = TRUE)
}
