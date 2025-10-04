mod_landing_ui <- function(id) {
  ns <- NS(id)
  
  tagList(

    tags$head(
      tags$style(HTML("
        /* Fallback styles */
        .hero img{
          width: clamp(360px, 22vw, 800px) !important;
          max-width: 100% !important;
          height: auto !important;
          display: block;
          border-radius: 8px;
          box-shadow: 0 2px 10px rgba(0,0,0,0.08);
        }
        @media (max-width: 900px) {
          .hero img { width: clamp(300px, 60vw, 800px) !important; }
        }
        .title { margin-top: 0; margin-bottom: 10px; font-weight: 600; }
        .summary { font-size: 1.05em; color: #555; }
        .card { background: #fff; border: 1px solid #e6e6e6; border-radius: 10px; padding: 16px 18px; margin-bottom: 18px; }
        .card h4 { margin-top: 0; margin-bottom: 12px; font-weight: 600; }
        .link { margin-bottom: 10px; }
        .link .desc { color: #777; font-size: 0.95em; margin-top: 3px; }
      "))
    ),
    

    fluidRow(
      column(
        width = 4,
        div(class = "hero",

            tags$img(
              src = "landing_hero.png",
              alt = "Project overview",
              loading = "lazy",
              style = "width: clamp(180px, 22vw, 300px); max-width: 100%; height: auto; display: block; border-radius: 8px; box-shadow: 0 2px 10px rgba(0,0,0,0.08);"
            )
        )
      ),
      column(
        width = 8,
        h2(class = "title", "Spatial Transcriptomics Atlas of Fistulating Crohn's disease"),
        div(class = "summary",
            p("We assembled a multi-modal resource profiling fistulating Crohn’s disease and controls across 92 patients, including 68 anatomically diverse intestinal fistulae (enterocutaneous, colocutaneous, perianal) spanning epithelialised and non-epithelialised tracts. The dataset integrates pan-lineage scRNA-seq (129,204 high-quality cells), unbiased Visium spatial transcriptomics (93,075 tissue-covered spots), and subcellular-resolution Xenium profiling (7.2M segmented single cells with 480-plex panel and 1.8M with 5100-plex panel). This portal lets you explore spatial gene expression from all samples profiled in the manuscript. Schematic created with biorender.com")
        ),
        div(class = "card",
            h4("What’s in this data portal?"),
            tags$ul(
              tags$li("Interactive UMAP & spatial gene expression plots for Xenium 5100-plex, 480-plex and Visium."),
              tags$li("Download links to raw and processed datasets."),
              tags$li("Questions about the data or this app? Email agne.antanaviciute@imm.ox.ac.uk"),
              tags$li("For more information, see our publication at [PLACEHOLDER URL]")
            )
        )
      )
    ),
    

    fluidRow(
      column(
        width = 6,
        div(class = "card",
            h4("Additional Mendeley Datasets"),
            div(class = "link",
                tags$a(href = "https://data.mendeley.com/datasets/tn972brm9s/3", target = "_blank",
                       "Mendeley Data Part 1"),
                div(class = "desc", em("Data and seurat object RDS files from visium and scRNA-Seq datasets."))
            ),
            div(class = "link",
                tags$a(href = "https://data.mendeley.com/datasets/64fkdfcpzb/3", target = "_blank",
                       "Mendeley Data Part 2"),
                div(class = "desc", em("Xenium 480-plex custom gut panel datasets and seurat RDS file."))
            ),
            div(class = "link",
                tags$a(href = "https://data.mendeley.com/datasets/mxy6p6wfmy/2", target = "_blank",
                       "Mendeley Data Part 3"),
                div(class = "desc", em("Xenium 5100-plex seurat object RDS, additional analyses. Fibroblast scRNA-Seq meta analysis files."))
            )
        )
      ),
      column(
        width = 6,
        div(class = "card",
            h4("Raw Data GEO Accessions"),
            div(class = "link",
                tags$a(href = "https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE283945", target = "_blank",
                       "GEO: GSE283945"),
                div(class = "desc", em("Visium Spatial Transcriptomics"))
            ),
            div(class = "link",
                tags$a(href = "https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE284230", target = "_blank",
                       "GEO: GSE284230"),
                div(class = "desc", em("scRNA-Seq"))
            ),
            div(class = "link",
                tags$a(href = "https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE305631", target = "_blank",
                       "GEO: GSE305631"),
                div(class = "desc", em("Bulk RNA-Seq."))
            )
        )
      )
    )
  )
}

mod_landing_server <- function(id) {
  moduleServer(id, function(input, output, session) { })
}
