#' Read and validate a population crosswalk
#'
#' @keywords internal
#' @noRd
read_population_crosswalk <- function(x){

  # No custom crosswalk was supplied
  if(is.null(x) || length(x) == 0L ||
     (length(x) == 1L && is.atomic(x) && is.na(x))){
    return(NULL)
  }

  # A named numeric vector is a convenient programmatic alternative
  if(is.numeric(x) && !is.null(names(x))){
    x <- data.frame(
      location = names(x),
      population = as.numeric(x),
      stringsAsFactors = FALSE
    )
  }

  # Read a CSV path
  if(is.character(x) && length(x) == 1L && is.null(names(x))){
    if(!file.exists(x)){
      stop("Population crosswalk file not found:\n  ", x, call. = FALSE)
    }
    x <- utils::read.csv(x, check.names = FALSE, stringsAsFactors = FALSE)
  }

  if(!is.data.frame(x)){
    stop("`population_crosswalk` must be NULL, a CSV path, a data.frame, ",
         "or a named numeric vector.", call. = FALSE)
  }

  needed <- c("location", "population")
  missing_cols <- setdiff(needed, names(x))
  if(length(missing_cols) > 0L){
    stop("Population crosswalk is missing required column(s): ",
         paste(missing_cols, collapse = ", "), ".\n\n",
         "The file needs exactly these essential fields:\n",
         "  location,population", call. = FALSE)
  }

  out <- data.frame(
    location = trimws(as.character(x$location)),
    population = suppressWarnings(as.numeric(x$population)),
    stringsAsFactors = FALSE
  )
  out <- out[!is.na(out$location) & nzchar(out$location), , drop = FALSE]

  bad <- is.na(out$population) | !is.finite(out$population) | out$population <= 0
  if(any(bad)){
    stop("Population must be one positive number for: ",
         paste(unique(out$location[bad]), collapse = ", "), ".",
         call. = FALSE)
  }

  normalized <- tolower(out$location)
  conflicts <- vapply(split(out$population, normalized), function(values){
    length(unique(values)) > 1L
  }, logical(1))
  if(any(conflicts)){
    stop("Population crosswalk contains conflicting values for: ",
         paste(names(conflicts)[conflicts], collapse = ", "), ".",
         call. = FALSE)
  }

  out <- out[!duplicated(normalized), , drop = FALSE]
  row.names(out) <- NULL
  out
}

#' Resolve locations to built-in and custom population values
#'
#' @keywords internal
#' @noRd
resolve_population_values <- function(locations, custom_crosswalk = NULL,
                                      location_crosswalk = NULL){

  locations <- trimws(as.character(locations))
  locations <- unique(locations[!is.na(locations) & nzchar(locations)])
  custom <- read_population_crosswalk(custom_crosswalk)

  # Build a long alias table from every searchable built-in field
  built <- default_population_crosswalk
  alias_rows <- list(
    data.frame(key = built$location, population = built$population),
    data.frame(key = built$location_code, population = built$population),
    data.frame(key = built$abbreviation, population = built$population)
  )

  extra_aliases <- lapply(seq_len(nrow(built)), function(i){
    aliases <- as.character(built$aliases[i])
    if(is.na(aliases) || !nzchar(aliases)) return(NULL)
    data.frame(
      key = strsplit(aliases, "|", fixed = TRUE)[[1]],
      population = built$population[i],
      stringsAsFactors = FALSE
    )
  })

  alias_table <- do.call(rbind, c(alias_rows, extra_aliases))
  alias_table$key <- trimws(as.character(alias_table$key))
  alias_table <- alias_table[!is.na(alias_table$key) & nzchar(alias_table$key), ]
  alias_table$normalized <- tolower(alias_table$key)
  alias_table <- alias_table[!duplicated(alias_table$normalized), ]

  # Custom values take priority and only need to contain unmatched locations
  if(!is.null(custom)){
    custom_aliases <- data.frame(
      key = custom$location,
      population = custom$population,
      normalized = tolower(custom$location),
      stringsAsFactors = FALSE
    )
    alias_table <- rbind(
      custom_aliases,
      alias_table[!alias_table$normalized %in% custom_aliases$normalized, ]
    )
  }

  # A display-name crosswalk may turn an otherwise unknown raw code into a
  # recognized county or region name. Try raw values first, then display names.
  display_locations <- locations
  if(!is.null(location_crosswalk) && length(location_crosswalk) > 0L){
    display_index <- match(tolower(locations),
                           tolower(trimws(names(location_crosswalk))))
    has_display <- !is.na(display_index)
    display_locations[has_display] <- trimws(as.character(
      unname(location_crosswalk[display_index[has_display]])
    ))
  }

  raw_index <- match(tolower(locations), alias_table$normalized)
  display_index <- match(tolower(display_locations), alias_table$normalized)
  final_index <- ifelse(!is.na(raw_index), raw_index, display_index)

  values <- alias_table$population[final_index]
  names(values) <- locations
  as.numeric_values <- suppressWarnings(as.numeric(values))
  names(as.numeric_values) <- locations
  as.numeric_values
}

#' Stop with a report-specific population crosswalk prompt
#'
#' @keywords internal
#' @noRd
stop_for_missing_population <- function(locations, options_file = NULL){
  locations <- unique(as.character(locations))
  example_rows <- paste0("  ", locations, ",<population>", collapse = "\n")
  option_text <- if(is.null(options_file)) "'path/to/report_options.R'" else
    paste0("'", options_file, "'")

  stop(
    "Population crosswalk required.\n\n",
    "No built-in population was found for:\n  - ",
    paste(locations, collapse = "\n  - "), "\n\n",
    "Create a CSV with `location` and `population` columns, for example:\n",
    "  location,population\n", example_rows, "\n\n",
    "Then run:\n",
    "  generate_report(\n",
    "    ", option_text, ",\n",
    "    population_crosswalk = 'path/to/population_crosswalk.csv'\n",
    "  )\n\n",
    "Location matching ignores capitalization and surrounding spaces. A custom\n",
    "file only needs rows that are not already in the built-in table.",
    call. = FALSE
  )
}
