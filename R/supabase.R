library(httr2)

`%||%` <- function(a, b) if (!is.null(a)) a else b

supabase_config <- function() {
  list(
    url = Sys.getenv("SUPABASE_URL", ""),
    key = Sys.getenv("SUPABASE_ANON_KEY", "")
  )
}

supabase_require <- function() {
  cfg <- supabase_config()
  if (!nzchar(cfg$url) || !nzchar(cfg$key)) {
    stop("Supabase credentials missing (SUPABASE_URL / SUPABASE_ANON_KEY).")
  }
  invisible(cfg)
}

sb_verbose <- function(on = TRUE) options(supabase.verbose = isTRUE(on))

supabase_request <- function(path = "rest/v1/") {
  cfg <- supabase_require()
  req <- request(cfg$url) |>
    req_url_path_append(path) |>
    req_headers(apikey = cfg$key, Authorization = paste("Bearer", cfg$key)) |>
    req_error(is_error = ~ FALSE)  
  if (isTRUE(getOption("supabase.verbose"))) req <- req_verbose(req)
  req
}



sb_json_df <- function(x) {
  if (is.null(x)) return(data.frame())
  if (is.data.frame(x)) return(x)
  if (length(x) == 0L) return(data.frame())
  as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE)
}

sb_fetch_pages_qs <- function(path, query = list(), select = "*",
                              order = NULL, page_size = 1000L, max_pages = 10000L) {
  out <- list(); offset <- 0L; page <- 0L
  
  repeat {
    page <- page + 1L
    
    params <- c(list(select = select, limit = page_size, offset = offset), query)
    if (!is.null(order)) params$order <- order
    
    req <- supabase_request(paste0("rest/v1/", path))
    req <- do.call(req_url_query, c(list(req), params))
    
    resp <- req_perform(req)
    if (resp_is_error(resp)) {
      stop("HTTP ", resp_status(resp), " — ", resp_status_desc(resp), "\n", resp_body_string(resp))
    }
    
    df <- sb_json_df(resp_body_json(resp, simplifyVector = TRUE))
    n  <- nrow(df)
    
    if (n == 0L) break
    out[[length(out) + 1L]] <- df
    if (n < page_size) break           
    if (page >= max_pages) break       
    offset <- offset + page_size      
  }
  
  if (!length(out)) return(data.frame())
  do.call(rbind, out)
}


sb_fetch_pages_parallel <- function(path, query = list(), select = "*",
                                    order = NULL, page_size = 1000L,
                                    workers = getOption("supabase.workers", 6L),
                                    max_batches = 10000L) {
  stopifnot(page_size >= 1L, workers >= 1L)
  oplan <- future::plan()
  on.exit(future::plan(oplan), add = TRUE)
  future::plan(future::multisession, workers = as.integer(workers))
  
  fetch_one <- function(off) {
    req <- supabase_request(paste0("rest/v1/", path))
    params <- c(list(select = select, limit = page_size, offset = off), query)
    if (!is.null(order)) params$order <- order
    req <- do.call(req_url_query, c(list(req), params))
    r <- req_perform(req)
    if (resp_is_error(r)) stop("HTTP ", resp_status(r), " — ", resp_status_desc(r), "\n", resp_body_string(r))
    sb_json_df(resp_body_json(r, simplifyVector = TRUE))
  }
  
  out <- list(); start <- 0L; batch <- 0L
  repeat {
    batch <- batch + 1L
    offsets <- start + (0:(workers - 1L)) * page_size
    
    parts <- future.apply::future_lapply(offsets, fetch_one, future.seed = TRUE)
    sizes <- vapply(parts, nrow, integer(1))
    
    for (i in seq_along(parts)) if (sizes[i] > 0L) out[[length(out) + 1L]] <- parts[[i]]
    
    if (all(sizes == 0L) || any(sizes < page_size)) break
    if (batch >= max_batches) { warning("Reached max_batches; stopping."); break }
    start <- start + workers * page_size
  }
  
  if (!length(out)) return(data.frame())
  do.call(rbind, out)
}

pg_in <- pg_in %||% function(vals) {
  if (!length(vals)) return("in.()")
  q <- vapply(vals, function(v) {
    v <- as.character(v)
    if (grepl("[^A-Za-z0-9_\\-\\.]", v)) paste0('"', gsub('"', '\\"', v, fixed=TRUE), '"') else v
  }, character(1))
  paste0("in.(", paste(q, collapse=","), ")")
}

fmt_int <- function(x) formatC(as.integer(x), format = "d", big.mark = ",")


sb_can_parallel <- function() {
  w <- as.integer(getOption("supabase.workers", 1L))
  w > 1L &&
    requireNamespace("future", quietly = TRUE) &&
    requireNamespace("future.apply", quietly = TRUE)
}



#########XENIUMv5k##################
sb_distinct_images <- function(limit_total = 100000L) {

  df <- try(sb_fetch_pages_qs(
    path   = "seurat5k_images_new",
    select = "image",
    page_size = 1000L
  ), silent = TRUE)
  
  imgs <- character()
  
  if (!inherits(df, "try-error") && is.data.frame(df) && "image" %in% names(df)) {
    v <- df[["image"]]
    v <- as.character(v)
    v <- v[!is.na(v) & nzchar(v)]
    if (length(v)) imgs <- sort(unique(v), na.last = NA)
  }
  
  if (!length(imgs)) {
    req <- supabase_request("rest/v1/seurat5k_spatial") |>
      req_url_query(select = "image", distinct = "", order = "image", limit = 10000)
    resp <- req_perform(req)
    if (!resp_is_error(resp)) {
      j <- resp_body_json(resp, simplifyVector = TRUE)
      if (is.list(j) || is.data.frame(j)) {
        dd <- as.data.frame(j, stringsAsFactors = FALSE, check.names = FALSE)
        if ("image" %in% names(dd)) {
          v <- as.character(dd$image)
          v <- v[!is.na(v) & nzchar(v)]
          if (length(v)) imgs <- sort(unique(v), na.last = NA)
        }
      }
    }
  }
  
  if (!length(imgs)) return(character())
  imgs[seq_len(min(length(imgs), as.integer(limit_total)))]
}


sb_distinct_genes <- function(limit_total = 100000L) {

  df <- try(sb_fetch_pages_qs(
    path   = "seurat5k_genes_new",
    select = "gene_name",
    page_size = 1000L
  ), silent = TRUE)
  
  genes <- character()
  if (!inherits(df, "try-error") && is.data.frame(df) && "gene_name" %in% names(df)) {
    v <- as.character(df$gene_name)
    v <- v[!is.na(v) & nzchar(v)]
    if (length(v)) genes <- sort(unique(v), na.last = NA)
  }
  
  if (!length(genes)) {
    req <- supabase_request("rest/v1/seurat5k_expression") |>
      req_url_query(select = "gene_name", distinct = "", limit = 10000)
    resp <- req_perform(req)
    if (!resp_is_error(resp)) {
      j <- resp_body_json(resp, simplifyVector = TRUE)
      dd <- as.data.frame(j, stringsAsFactors = FALSE, check.names = FALSE)
      if ("gene" %in% names(dd)) {
        v <- as.character(dd$gene_name)
        v <- v[!is.na(v) & nzchar(v)]
        if (length(v)) genes <- sort(unique(v), na.last = NA)
      }
    }
  }
  
  if (!length(genes)) return(character())
  genes[seq_len(min(length(genes), as.integer(limit_total)))]
}



sb5k_fetch_image_id <- function(image) {
  print("Fetching image id for image 5k")
  image <- trimws(image);
  sb_fetch_pages_qs(
    path   = "seurat5k_images_new",             
    query  = list(image = paste0("eq.", image)),
    select = "image_id"
  )
}

sb5k_fetch_gene_id <- function(gene) {
  print("Fetching gene id for image 5k")
  gene <- trimws(gene);
  sb_fetch_pages_qs(
    path   = "seurat5k_genes_new",             
    query  = list(gene_name = paste0("eq.", gene)),
    select = "gene_id"
  )
}

sb_fetch_expression <- function(image, gene, page_size = 1000L){
  
  image_id <- sb5k_fetch_image_id(image)
  gene_id <- sb5k_fetch_gene_id(gene)
  
  print("image id and gene id:")
  print(image_id)
  print(gene_id)
  
  sb_fetch_pages_qs(
    path   = "seurat5k_expression_new",             
    query  = list(image_id = paste0("eq.", image_id[1, 1]),
                  gene_id  = paste0("eq.", gene_id[1, 1])),
    select = "cell_id,value",
    page_size = page_size
  )
}

sb_fetch_cells_full <- function(image, page_size = 1000L) {
  
  print("sb_fetch_cells_full!!!!")
  image <- trimws(image); stopifnot(nzchar(image))
  sel <- "cell_id,cellannotationbroad,x,y,umap_1,umap_2"  
  if (sb_can_parallel()) {
    print("I'm parallel :)")
    sb_fetch_pages_parallel(
      path   = "seurat5k_metadata_new",
      query  = list(sample = paste0("eq.", image)),
      select = sel,
      #order  = "cell.asc",
      page_size = page_size,
      workers   = getOption("supabase.workers", 8L)
    )
  } else {
    print("I'm not parallel :(")
    
    sb_fetch_pages_qs(
      path   = "seurat5k_metadata_new", 
      query  = list(sample = paste0("eq.", image)),
      select = sel,
      #order  = "cell.asc",
      page_size = page_size
    )
    #print("query done")
    
  }
  
  
}


#########XENIUM 480

sb480_distinct_images <- function(limit_total = 100000L) {
  df <- try(sb_fetch_pages_qs(
    path   = "seurat480_images_new",
    select = "image",
    page_size = 1000L
  ), silent = TRUE)
  
  imgs <- character()
  if (!inherits(df, "try-error") && is.data.frame(df) && "image" %in% names(df)) {
    v <- as.character(df$image)
    v <- v[!is.na(v) & nzchar(v)]
    if (length(v)) imgs <- sort(unique(v), na.last = NA)
  }
  

  if (!length(imgs)) {
    req <- supabase_request("rest/v1/seurat480_spatial") |>
      req_url_query(select = "image", distinct = "", order = "image", limit = 10000)
    resp <- req_perform(req)
    if (!resp_is_error(resp)) {
      dd <- sb_json_df(resp_body_json(resp, simplifyVector = TRUE))
      if ("image" %in% names(dd)) {
        v <- as.character(dd$image)
        v <- v[!is.na(v) & nzchar(v)]
        if (length(v)) imgs <- sort(unique(v), na.last = NA)
      }
    }
  }
  if (!length(imgs)) return(character())
  imgs[seq_len(min(length(imgs), as.integer(limit_total)))]
}

sb480_distinct_genes <- function(limit_total = 100000L) {
  df <- try(sb_fetch_pages_qs(
    path   = "seurat480_genes_new",
    select = "gene_name",
    #order  = "gene",
    page_size = 1000L
  ), silent = FALSE)
  
  genes <- character()
  if (!inherits(df, "try-error") && is.data.frame(df) && "gene_name" %in% names(df)) {
    v <- as.character(df$gene_name)
    v <- v[!is.na(v) & nzchar(v)]
    if (length(v)) genes <- sort(unique(v), na.last = NA)
  }
  
  if (!length(genes)) {
    req <- supabase_request("rest/v1/seurat480_expression") |>
      req_url_query(select = "gene_name", distinct = "",  limit = 10000)
    resp <- req_perform(req)
    if (!resp_is_error(resp)) {
      dd <- sb_json_df(resp_body_json(resp, simplifyVector = TRUE))
      if ("gene" %in% names(dd)) {
        v <- as.character(dd$gene)
        v <- v[!is.na(v) & nzchar(v)]
        if (length(v)) genes <- sort(unique(v), na.last = NA)
      }
    }
  }
  if (!length(genes)) return(character())
  genes[seq_len(min(length(genes), as.integer(limit_total)))]
}



sb480_fetch_expression <- function(image, gene, page_size = 1000L){
  print("whats going on?")
  print(image)
  print(gene)
  image_id <- sb480_fetch_image_id(image)
  gene_id <- sb480_fetch_gene_id(gene)
  
  print("1 image id and gene id:")
  print(image_id)
  print(gene_id)
  
  sb_fetch_pages_qs(
    path   = "seurat480_expression_new",             
    query  = list(image_id = paste0("eq.", image_id[1, 1]),
                  gene_id  = paste0("eq.", gene_id[1, 1])),
    select = "cell_id,value",
    page_size = page_size
  )
}

sb480_fetch_image_id <- function(image) {
  print("Fetching image id for image 480")
  image <- trimws(image);
  sb_fetch_pages_qs(
    path   = "seurat480_images_new",             
    query  = list(image = paste0("eq.", image)),
    select = "image_id"
  )
}



sb480_fetch_gene_id <- function(gene) {
  print("Fetching gene id for image 480")
  gene <- trimws(gene);
  sb_fetch_pages_qs(
    path   = "seurat480_genes_new",             
    query  = list(gene_name = paste0("eq.", gene)),
    select = "gene_id"
  )
}


sb480_fetch_cells_full <- function(image, page_size = 1000L) {
  
  print("sb480_fetch_cells_full!!!!")
  image <- trimws(image); stopifnot(nzchar(image))
  sel <- "cell_id,cellannotationbroad,x,y,umap_1,umap_2"  
  if (sb_can_parallel()) {
    print("I'm parallel :)")
    sb_fetch_pages_parallel(
      path   = "seurat480_metadata_new",
      query  = list(sample = paste0("eq.", image)),
      select = sel,
      #order  = "cell.asc",
      page_size = page_size,
      workers   = getOption("supabase.workers", 8L)
    )
  } else {
    print("I'm not parallel :(")
    
    sb_fetch_pages_qs(
      path   = "seurat480_metadata_new", #TODO this doesnt actually have an image column!!!!! FIX this first, make sure sample and image ID are the same in the db
      query  = list(sample = paste0("eq.", image)),
      select = sel,
      #order  = "cell.asc",
      page_size = page_size
    )
    #print("query done")
    
  }
  
  
}






################VISIUM##############
sbvis_fetch_gene_id <- function(gene) {
  print("Fetching gene id for image visium")
  gene <- trimws(gene);
  sb_fetch_pages_qs(
    path   = "seuratvis_genes_new",             
    query  = list(gene_name = paste0("eq.", gene)),
    select = "gene_id"
  )
}

sbvis_fetch_image_id <- function(image) {
  print("Fetching image id for image visium")
  image <- trimws(image);
  sb_fetch_pages_qs(
    path   = "seuratvis_images_new",             
    query  = list(image = paste0("eq.", image)),
    select = "image_id"
  )
}

sbvis_fetch_expression <- function(image, gene, page_size = 1000L){
  
  image_id <- sbvis_fetch_image_id(image)
  gene_id <- sbvis_fetch_gene_id(gene)
  
  print("image id and gene id:")
  print(image_id)
  print(gene_id)
  
  sb_fetch_pages_qs(
    path   = "seuratvis_expression_new",             
    query  = list(image_id = paste0("eq.", image_id[1, 1]),
                  gene_id  = paste0("eq.", gene_id[1, 1])),
    select = "cell_id,value",
    page_size = page_size
  )
}

sbvis_fetch_cells_full <- function(image, page_size = 1000L) {
  
  print("sbvis_fetch_cells_full!!!!")
  image <- trimws(image); stopifnot(nzchar(image))
  sel <- "cell_id,cellannotationbroad,x,y,umap_1,umap_2"  
  if (sb_can_parallel()) {
    print("I'm parallel :)")
    sb_fetch_pages_parallel(
      path   = "seuratvisium_metadata_new",
      query  = list(sample = paste0("eq.", image)),
      select = sel,
      #order  = "cell.asc",
      page_size = page_size,
      workers   = getOption("supabase.workers", 8L)
    )
  } else {
    print("I'm not parallel :(")
    
    sb_fetch_pages_qs(
      path   = "seuratvisium_metadata_new", 
      query  = list(sample = paste0("eq.", image)),
      select = sel,
      #order  = "cell.asc",
      page_size = page_size
    )
    #print("query done")
    
  }
}

sbvis_distinct_images <- function(limit_total = 100000L) {
  
  df <- try(sb_fetch_pages_qs(
    path   = "seuratvis_images_new",
    select = "image",
    page_size = 1000L
  ), silent = TRUE)
  
  imgs <- character()
  
  if (!inherits(df, "try-error") && is.data.frame(df) && "image" %in% names(df)) {
    v <- df[["image"]]
    v <- as.character(v)
    v <- v[!is.na(v) & nzchar(v)]
    if (length(v)) imgs <- sort(unique(v), na.last = NA)
  }
  
  if (!length(imgs)) {
    req <- supabase_request("rest/v1/seurat5k_spatial") |>
      req_url_query(select = "image", distinct = "", order = "image", limit = 10000)
    resp <- req_perform(req)
    if (!resp_is_error(resp)) {
      j <- resp_body_json(resp, simplifyVector = TRUE)
      if (is.list(j) || is.data.frame(j)) {
        dd <- as.data.frame(j, stringsAsFactors = FALSE, check.names = FALSE)
        if ("image" %in% names(dd)) {
          v <- as.character(dd$image)
          v <- v[!is.na(v) & nzchar(v)]
          if (length(v)) imgs <- sort(unique(v), na.last = NA)
        }
      }
    }
  }
  
  if (!length(imgs)) return(character())
  imgs[seq_len(min(length(imgs), as.integer(limit_total)))]
}


sbvis_distinct_genes <- function(limit_total = 100000L) {
  df <- try(sb_fetch_pages_qs(
    path   = "seuratvis_genes_new",
    select = "gene_name",
    #order  = "gene",
    page_size = 1000L
  ), silent = FALSE)
  
  genes <- character()
  if (!inherits(df, "try-error") && is.data.frame(df) && "gene_name" %in% names(df)) {
    v <- as.character(df$gene_name)
    v <- v[!is.na(v) & nzchar(v)]
    if (length(v)) genes <- sort(unique(v), na.last = NA)
  }
  
  if (!length(genes)) {
    req <- supabase_request("rest/v1/seurat480_expression") |>
      req_url_query(select = "gene_name", distinct = "",  limit = 10000)
    resp <- req_perform(req)
    if (!resp_is_error(resp)) {
      dd <- sb_json_df(resp_body_json(resp, simplifyVector = TRUE))
      if ("gene" %in% names(dd)) {
        v <- as.character(dd$gene)
        v <- v[!is.na(v) & nzchar(v)]
        if (length(v)) genes <- sort(unique(v), na.last = NA)
      }
    }
  }
  if (!length(genes)) return(character())
  genes[seq_len(min(length(genes), as.integer(limit_total)))]
}

