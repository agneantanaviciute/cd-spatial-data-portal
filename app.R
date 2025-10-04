library(shiny)
library(ggplot2)

options(supabase.workers = 1)

if (file.exists(".Renviron")) readRenviron(".Renviron")

supabase_require()

for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)

shinyApp(ui = app_ui(), server = app_server)
