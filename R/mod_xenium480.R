mod_xenium480_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        3,
        selectizeInput(
          ns("image"), "Slide / Image",
          choices = NULL, multiple = FALSE,
          options = list(placeholder = "Select image…")
        ),
        radioButtons(
          ns("mode"), "Colour by",
          choices = c("Gene", "Cell type"),
          selected = "Cell type", inline = FALSE
        ),
        conditionalPanel(
          sprintf("input['%s'] == 'Gene'", ns("mode")),
          selectizeInput(
            ns("gene"), "Gene",
            choices = NULL, multiple = FALSE,
            options = list(placeholder = "Search gene…", create = FALSE, maxOptions = 600)
          )
        ),
        tags$hr(),
        tags$h6("Maximum 10k cells per sample will be plotted."),
        textOutput(ns("status"))
      ),
      column(
        9,
        tabsetPanel(
          id = ns("plots"),
          tabPanel("UMAP",    plotOutput(ns("umap_plot"), height = 500)),
          tabPanel("Spatial", plotOutput(ns("spatial_plot"), height = 500))
        )
      )
    )
  )
}

mod_xenium480_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    supabase_require()
    
    `%||%` <- function(a, b) if (!is.null(a)) a else b
    fmt_int <- function(x) formatC(as.integer(x %||% 0L), format = "d", big.mark = ",")
    set_status <- function(txt) output$status <- renderText(txt)
    
    observe({
      set_status("Fetching 480-plex images…")
      imgs <- try(sb480_distinct_images(), silent = TRUE)
      if (inherits(imgs, "try-error") || !length(imgs)) imgs <- character()
      updateSelectizeInput(session, "image", choices = imgs, server = TRUE)
      set_status(sprintf("Loaded %s images.", fmt_int(length(imgs))))
    })
    
    observe({
      set_status("Fetching 480-plex genes…")
      genes <- try(sb480_distinct_genes(), silent = TRUE)
      if (inherits(genes, "try-error") || !length(genes)) genes <- character()
      updateSelectizeInput(session, "gene",
                           choices  = genes,
                           selected = if (length(genes)) genes[[1]] else NULL,
                           server   = length(genes) > 2000,
                           options  = list(placeholder = "Search gene…", maxOptions = 2000)
      )
      set_status(sprintf("Loaded %s genes.", fmt_int(length(genes))))
    })
    
    r_cells <- reactiveVal(NULL)
    r_expr  <- reactiveVal(data.frame(cell = character(), value = double()))
    
    observeEvent(input$image, {
      req(input$image)
      withProgress(message = sprintf("Loading cells for '%s'…", input$image), value = 0, {
        set_status("Fetching 480-plex cells…")
        dat <- try(sb480_fetch_cells_full(input$image), silent = TRUE)
        if (inherits(dat, "try-error") || !is.data.frame(dat)) dat <- data.frame()
        r_cells(dat)
        incProgress(1)
        set_status(sprintf("Cells loaded: %s", fmt_int(nrow(dat))))
      })
    }, ignoreInit = FALSE)
    
    observeEvent(list(input$mode, input$image, input$gene), {
      req(input$image)
      if (identical(input$mode, "Gene")) {
        req(input$gene)
        r_expr(data.frame(cell = character(), value = double()))
        withProgress(message = sprintf("Loading %s expression…", input$gene), value = 0, {
          set_status(sprintf("Fetching expression for %s…", input$gene))
          ex <- try(sb480_fetch_expression(input$image, input$gene), silent = TRUE)
          if (inherits(ex, "try-error") || !is.data.frame(ex)) {
            ex <- data.frame(cell = character(), value = double())
          }
          r_expr(ex)
          incProgress(1)
          set_status(sprintf("Number of Cells Expressing: %s", fmt_int(nrow(ex))))
        })
      } else {
        r_expr(NULL)
      }
    }, ignoreInit = TRUE)
    
    output$umap_plot <- renderPlot({
      dat <- r_cells()
      validate(need(is.data.frame(dat) && nrow(dat) > 0, "No cells loaded."))
      if (identical(input$mode, "Gene")) {
        req(input$gene)
        ex <- r_expr(); validate(need(!is.null(ex), "Loading gene expression…"))
        if (!is.data.frame(ex) || !all(c("cell_id","value") %in% names(ex))) {
          ex <- data.frame(cell = character(), value = double())
        }
        print(head(ex))
        dat <- merge(dat, ex[, c("cell_id","value")], by = "cell_id", all.x = TRUE)
        if (!"value" %in% names(dat)) dat$value <- NA_real_
        ggplot2::ggplot(dat, ggplot2::aes(umap_1, umap_2, colour = value)) +
          ggplot2::geom_point(size = 0.6, alpha = 0.85) +
          ggplot2::scale_colour_viridis_c(option = "magma", na.value = "grey80", direction = -1) +
          ggplot2::coord_equal() + ggplot2::theme_minimal(base_size = 14) +
          ggplot2::labs(colour = input$gene, title = "UMAP (Xenium 480-plex)")
      } else {
        ggplot2::ggplot(dat, ggplot2::aes(umap_1, umap_2, colour = cellannotationbroad)) +
          ggplot2::geom_point(size = 0.6, alpha = 0.85) +
          ggplot2::coord_equal() + ggplot2::theme_minimal(base_size = 14) +
          ggplot2::labs(colour = "Cell type", title = "UMAP (Xenium 480-plex)")
      }
    })
    
    output$spatial_plot <- renderPlot({
      dat <- r_cells()
      validate(need(is.data.frame(dat) && nrow(dat) > 0, "No cells loaded."))
      if (identical(input$mode, "Gene")) {
        req(input$gene)
        ex <- r_expr(); validate(need(!is.null(ex), "Loading gene expression…"))
        if (!is.data.frame(ex) || !all(c("cell_id","value") %in% names(ex))) {
          ex <- data.frame(cell = character(), value = double())
        }
        dat <- merge(dat, ex[, c("cell_id","value")], by = "cell_id", all.x = TRUE)
        if (!"value" %in% names(dat)) dat$value <- NA_real_
        ggplot2::ggplot(dat, ggplot2::aes(x, y, colour = value)) +
          ggplot2::geom_point(size = 0.6, alpha = 0.85) +
          ggplot2::scale_colour_viridis_c(option = "magma", na.value = "grey80", direction = -1) +
          ggplot2::coord_equal() + ggplot2::scale_y_reverse() + ggplot2::theme_minimal(base_size = 14) +
          ggplot2::labs(colour = input$gene, title = "Spatial (Xenium 480-plex)")
      } else {
        ggplot2::ggplot(dat, ggplot2::aes(x, y, colour = cellannotationbroad)) +
          ggplot2::geom_point(size = 0.6, alpha = 0.85) +
          ggplot2::coord_equal() + ggplot2::scale_y_reverse() + ggplot2::theme_minimal(base_size = 14) +
          ggplot2::labs(colour = "Cell type", title = "Spatial (Xenium 480-plex)")
      }
    })
  })
}
