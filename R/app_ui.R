app_ui <- function() {
  navbarPage(
    title    = "CD Spatial Data Portal",
    id       = "main_nav",
    selected = "home",                         
    theme    = shinythemes::shinytheme("paper"),
    
    tabPanel("Home",            value = "home",   mod_landing_ui("landing")),
    #tabPanel("scRNA-seq",       value = "scrna",  mod_scrna_ui("scrna")), # need to finish this one, currently placeholder
    tabPanel("Visium",          value = "visium", mod_visium_ui("visium")),
    tabPanel("Xenium 480-plex", value = "x480",   mod_xenium480_ui("x480")),
    tabPanel("Xenium 5100-plex",value = "x5100",  mod_xenium5100_ui("x5100"))
  )
}

