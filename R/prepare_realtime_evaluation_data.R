#' Prepare the real-time evaluation dataset
#'
#' Builds the dataset used for real-time (operational) forecast evaluation by
#' combining the archived operational forecasts -- the per-week forecast CSVs
#' written to the `Forecasts/` directory by `extract_implementation_data()` --
#' with the observed (truth) outcome values from `master_data`. All quantile
#' rows are retained: the point-metric helpers filter to the median internally
#' and the traditional-metric helper uses the full distribution. Rows whose
#' `target_end_date` has no realized observed value carry `Observed = NA` and
#' are dropped by the metric helpers, so only realized weeks are scored.
#'
#' @param impl_meta Metadata list from `extract_implementation_data()`. Must
#'   contain `forecast_path`; its parent directory is globbed for the archive.
#'   `impl_meta$locations` (raw code -> display name) is used for the truth join.
#' @param master_data Assembled master data frame. Outcome rows supply observed.
#' @param locations Optional named vector mapping raw codes (names) to display
#'   names (values). When `NULL`, falls back to `impl_meta$locations`, then to
#'   an identity map.
#' @param time_step Integer days per step (default 7). Used only to backfill
#'   `reference_date` when an archive file lacks it.
#' @param truth_type `variable_type` tag whose rows in `master_data` supply the
#'   `Observed` truth for the join. Defaults `"outcome_data"`; pass
#'   `"training_data"` to score against the training series instead.
#'
#' @return A data frame with columns `reference_date`, `location` (raw code),
#'   `target_end_date`, `value`, `horizon`, `output_type_id`, and `Observed`,
#'   or an empty frame when no archived forecasts are available.
#'
#' @keywords internal
#' @noRd
prepare_realtime_evaluation_data <- function(impl_meta,
                                             master_data,
                                             locations  = NULL,
                                             time_step  = 7,
                                             truth_type = "outcome_data") {

#------------------------------------------------------------------------------#
# Preparing the empty data frame -----------------------------------------------
#------------------------------------------------------------------------------#
# About: This section prepares the empty data frame that will return if there  #
# is an issue in any of the below checks.                                      #
#------------------------------------------------------------------------------#

  ########################################
  # Creating the empty result data frame #
  ########################################
  empty_result <- data.frame(
    reference_date  = as.Date(character(0)),
    location        = character(0),
    target_end_date = as.Date(character(0)),
    value           = numeric(0),
    horizon         = numeric(0),
    output_type_id  = numeric(0),
    Observed        = numeric(0),
    stringsAsFactors = FALSE
  )

#------------------------------------------------------------------------------#
# Initial data checks ----------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section runs initial data checks to ensure that the data should  #
# be created and can be created.                                               #
#------------------------------------------------------------------------------#

  ############################################
  # Checking for non-null meta data elements #
  ############################################
  if(is.null(impl_meta) || is.null(impl_meta$forecast_path) ||
     length(impl_meta$forecast_path) == 0 ||
     is.na(impl_meta$forecast_path[1])){

    # Error: Returning empty data frame
    return(empty_result)

  }

  ###########################################
  # Checking if implementation files exisit #
  ###########################################

  # Pulling the directory name for implementation files
  forecast_dir <- dirname(impl_meta$forecast_path[1])

  # Error: Returning empty data frame
  if(!dir.exists(forecast_dir)) return(empty_result)

#------------------------------------------------------------------------------#
# Pulling the list of file names -----------------------------------------------
#------------------------------------------------------------------------------#
# About: This section pulls the list of file names for multiple location files #
# and single location files. They use different naming schemes for each, so    #
# how those file names are pulled must differ.                                 #
#------------------------------------------------------------------------------#

  ##################################
  # Running if multi-location file #
  ##################################
  if(length(impl_meta[["locations"]]) > 1){

    # Multiple locations: forecasts are saved as Forecast-<date>.csv
    files <- list.files(forecast_dir, pattern = "\\.csv$", full.names = TRUE)

  ###################################
  # Running if single location file #
  ###################################
  }else{

    # Pulling base-location name: use in file name
    loc_code <- names(impl_meta[["locations"]])[1]

    # extract_implementation_data() sanitizes the location into the saved
    # filename (spaces -> underscores, etc.), so "Pee Dee" is stored as
    # "Pee_Dee". Match on the sanitized tag, not the raw code, or two-word
    # locations never match and the real-time section silently drops out.
    loc_tag <- sanitize_location_tag(loc_code)

    # Single Location: Pulling the forecast files
    files <- list.files(
      forecast_dir,
      pattern    = paste0("Forecast-", loc_tag, "-.*\\.csv$"),
      full.names = TRUE,
      ignore.case = TRUE
    )

  }

  ###########################################
  # Error occurred: Return empty data frame #
  ###########################################
  if(length(files) == 0) return(empty_result)

  ###############################################
  # Location crosswalk (raw code -> display)  #
  ###############################################
  if(is.null(locations) || length(locations) == 0){
    locations <- if(!is.null(impl_meta$locations) &&
                    length(impl_meta$locations) > 0){
      impl_meta$locations
    }else{
      NULL
    }
  }

  ###############################################
  # Read + bind the archived forecasts          #
  ###############################################
  # About: Each file is read under its own tryCatch so a single unreadable or  #
  # malformed CSV (e.g. a truncated/locked write, or a stray non-forecast      #
  # file) is skipped and counted rather than abandoning the entire archive.    #
  # Per-file failures are tallied and reported once at the end.                #
  ###############################################

  # Per-file outcome tallies (reported after the loop)
  skipped_unreadable <- character(0)   # read.csv() itself errored
  skipped_missing    <- character(0)   # required columns absent
  skipped_empty      <- character(0)   # file had zero data rows

  read_one <- function(fp){

    # Read the file; a hard read failure returns NULL (file is skipped).
    df <- tryCatch(
      utils::read.csv(fp, stringsAsFactors = FALSE),
      error = function(e){
        skipped_unreadable <<- c(skipped_unreadable, basename(fp))
        NULL
      }
    )
    if(is.null(df)) return(NULL)

    # An archived forecast must carry these columns. horizon is only present in
    # hubverse-format files; it is derived from the dates below when absent, so
    # it is not required here.
    needed <- c("location", "target_end_date", "value", "output_type_id")
    if(length(setdiff(needed, names(df))) > 0){
      skipped_missing <<- c(skipped_missing, basename(fp))
      return(NULL)
    }

    # Empty data files contribute nothing.
    if(nrow(df) == 0){
      skipped_empty <<- c(skipped_empty, basename(fp))
      return(NULL)
    }

    # Coercing the per-row pieces under tryCatch so a parsing surprise in one
    # file (bad date encoding, etc.) skips that file alone.
    piece <- tryCatch({

      ref <- if("reference_date" %in% names(df)){
        anytime::anydate(df$reference_date)
      }else{
        rep(as.Date(NA), nrow(df))
      }
      ted <- anytime::anydate(df$target_end_date)

      # Horizon: use the file's column when present (hubverse). Otherwise derive
      # it from the forecast relationship
      #     reference_date + time_step * horizon = target_end_date
      # =>  horizon = (target_end_date - reference_date) / time_step
      # rounded to the nearest whole step to absorb any off-by-a-day storage.
      hzn <- if("horizon" %in% names(df)){
        suppressWarnings(as.numeric(df$horizon))
      }else{
        round(as.numeric(ted - ref) / time_step)
      }

      data.frame(
        location        = as.character(df$location),
        reference_date  = ref,
        target_end_date = ted,
        value           = suppressWarnings(as.numeric(df$value)),
        horizon         = hzn,
        output_type_id  = suppressWarnings(as.numeric(df$output_type_id)),
        stringsAsFactors = FALSE
      )

    }, error = function(e){
      skipped_unreadable <<- c(skipped_unreadable, basename(fp))
      NULL
    })

    piece
  }

  pieces <- lapply(files, read_one)
  pieces <- pieces[!vapply(pieces, is.null, logical(1))]

  # Report the per-file accounting once, so a partial archive is visible but
  # never silent.
  n_skipped <- length(skipped_unreadable) + length(skipped_missing) +
               length(skipped_empty)
  if(n_skipped > 0){
    message("prepare_realtime_evaluation_data(): read ", length(pieces),
            " of ", length(files), " forecast file(s); skipped ", n_skipped,
            ".")
    if(length(skipped_unreadable) > 0)
      message("  - unreadable: ", paste(skipped_unreadable, collapse = ", "))
    if(length(skipped_missing) > 0)
      message("  - missing required columns: ",
              paste(skipped_missing, collapse = ", "))
    if(length(skipped_empty) > 0)
      message("  - empty: ", paste(skipped_empty, collapse = ", "))
  }

  if(length(pieces) == 0) return(empty_result)

  raw <- tryCatch(
    do.call(rbind, pieces),
    error = function(e){
      message("prepare_realtime_evaluation_data(): could not combine the ",
              "forecast files, so an empty result is returned.\n  - Reason: ",
              conditionMessage(e))
      NULL
    }
  )

  if(is.null(raw) || nrow(raw) == 0) return(empty_result)

  ###############################################
  # De-duplicate forecast rows                  #
  ###############################################
  # About: A re-run or a stale file left in the archive can repeat a forecast  #
  # row, which would double-count it in every downstream average. Exact        #
  # duplicates are dropped silently. If two rows share the same forecast key   #
  # (location/reference_date/target_end_date/horizon/output_type_id) but carry #
  # different values, that is a genuine conflict in the archive -- it is       #
  # surfaced as a warning and the first occurrence is kept.                    #
  ###############################################
  raw <- raw[!duplicated(raw), , drop = FALSE]

  key_cols <- c("location", "reference_date", "target_end_date",
                "horizon", "output_type_id")
  dup_key  <- duplicated(raw[key_cols])
  if(any(dup_key)){
    warning("prepare_realtime_evaluation_data(): ", sum(dup_key),
            " forecast row(s) share a key but differ in value; keeping the ",
            "first occurrence of each. Check the Forecasts/ archive for ",
            "stale or re-run files.", call. = FALSE)
    raw <- raw[!dup_key, , drop = FALSE]
  }

  ###############################################
  # Drop rows that cannot be scored             #
  ###############################################
  # About: A row with no target_end_date can be neither joined to truth nor    #
  # scored, so it is removed before the join. (value = NA rows are retained:   #
  # the metric helpers handle them, and dropping them here could hide a real   #
  # gap in a submitted forecast.)                                              #
  ###############################################
  no_target <- is.na(raw$target_end_date)
  if(any(no_target)){
    message("prepare_realtime_evaluation_data(): dropping ", sum(no_target),
            " forecast row(s) with no target_end_date (unscoreable).")
    raw <- raw[!no_target, , drop = FALSE]
  }

  if(nrow(raw) == 0) return(empty_result)

  ###############################################
  # Backfill reference_date where missing       #
  ###############################################
  miss_ref <- is.na(raw$reference_date)
  if(any(miss_ref) && is.numeric(time_step) && length(time_step) == 1 &&
     !is.na(time_step) && time_step > 0){
    raw$reference_date[miss_ref] <-
      raw$target_end_date[miss_ref] - (raw$horizon[miss_ref] * time_step)
  }

  ###############################################
  # Canonical location key for the truth join   #
  ###############################################
  # About: The forecast side and the observed (truth) side may label the same  #
  # place differently -- e.g. a forecast carries the FIPS code "45" (shown as  #
  # "South Carolina") while the truth feed stores "SC". Joining on the display #
  # label alone then matches nothing and Observed comes back all NA. This      #
  # helper maps any of {FIPS code, abbreviation, full name} to one canonical   #
  # name (the hubverse location_name), case- and whitespace-insensitively, so  #
  # both sides of the join collapse onto the same key. Anything not found in   #
  # the crosswalk (e.g. county / MetroCast labels that already agree on both   #
  # sides) is returned trimmed but otherwise unchanged, so the helper is a     #
  # no-op outside the state/national space where the divergence occurs.        #
  ###############################################
  canonicalize_location <- function(x){

    x_chr <- trimws(as.character(x))
    out   <- x_chr  # fallback: the value as-is

    hv <- tryCatch(forecastEvalReport::hubverse_locations,
                   error = function(e) NULL)

    if(!is.null(hv) && is.data.frame(hv) &&
       all(c("location", "location_name", "abbreviation") %in% names(hv))){

      # Tier 1: exact FIPS code -> name
      idx1 <- match(x_chr, hv$location)
      # Tier 2: abbreviation -> name (case-insensitive)
      idx2 <- match(toupper(x_chr), toupper(hv$abbreviation))
      # Tier 3: full name -> canonical-cased name (case-insensitive)
      idx3 <- match(tolower(x_chr), tolower(hv$location_name))

      hit1 <-                         !is.na(idx1)
      hit2 <- !hit1 &                 !is.na(idx2)
      hit3 <- !hit1 & !hit2 &         !is.na(idx3)

      out[hit1] <- hv$location_name[idx1[hit1]]
      out[hit2] <- hv$location_name[idx2[hit2]]
      out[hit3] <- hv$location_name[idx3[hit3]]
    }

    out
  }

  ###############################################
  # Map raw location -> display name            #
  ###############################################
  if(!is.null(locations)){
    raw$location_display <- unname(locations[raw$location])
    raw$location_display[is.na(raw$location_display)] <-
      raw$location[is.na(raw$location_display)]
  }else{
    raw$location_display <- raw$location
  }

  # Forecast-side join key: canonicalize from the raw code (most reliable),
  # falling back to the display label where the code did not resolve.
  raw$location_canonical <- canonicalize_location(raw$location)

  ###############################################
  # Observed (truth) values from master_data    #
  ###############################################
  observed <- tryCatch({
    if(!is.null(master_data) && is.data.frame(master_data) &&
       "variable_type" %in% names(master_data)){

      obs_rows <- master_data[
        !is.na(master_data$variable_type) &
          master_data$variable_type == truth_type, ]

      if(nrow(obs_rows) > 0 &&
         all(c("location", "date", "value") %in% names(obs_rows))){

        obs_df <- data.frame(
          location_canonical = canonicalize_location(obs_rows$location),
          target_end_date    = anytime::anydate(obs_rows$date),
          Observed           = suppressWarnings(as.numeric(obs_rows$value)),
          stringsAsFactors   = FALSE
        )

        obs_df[!duplicated(
          obs_df[c("location_canonical", "target_end_date")]), , drop = FALSE]

      }else NULL
    }else NULL
  }, error = function(e) NULL)

  ###############################################
  # Join observed onto the forecasts            #
  ###############################################
  # About: A left join keeps every forecast row; targets not yet observed get  #
  # Observed = NA and are dropped by the metric helpers. The merge is guarded  #
  # so a type/key surprise degrades to Observed = NA rather than aborting the  #
  # whole evaluation.                                                          #
  ###############################################
  if(!is.null(observed)){
    raw <- tryCatch(
      merge(raw, observed,
            by    = c("location_canonical", "target_end_date"),
            all.x = TRUE, sort = FALSE),
      error = function(e){
        warning("prepare_realtime_evaluation_data(): could not join observed ",
                "truth values; proceeding with Observed = NA.\n  - Reason: ",
                conditionMessage(e), call. = FALSE)
        raw$Observed <- NA_real_
        raw
      }
    )
    # A failed/degenerate join can leave Observed absent; backfill defensively.
    if(!"Observed" %in% names(raw)) raw$Observed <- NA_real_
  }else{
    raw$Observed <- NA_real_
  }

  ###############################################
  # Final clean frame                           #
  ###############################################
  needed <- c("reference_date", "location", "target_end_date",
              "value", "horizon", "output_type_id", "Observed")

  if(length(setdiff(needed, names(raw))) > 0) return(empty_result)

  res <- tryCatch(
    raw %>%
      dplyr::select(dplyr::all_of(needed)) %>%
      dplyr::arrange(location, reference_date, horizon, output_type_id),
    error = function(e){
      message("prepare_realtime_evaluation_data(): final tidy step failed, ",
              "returning an empty result.\n  - Reason: ", conditionMessage(e))
      empty_result
    }
  )

  rownames(res) <- NULL
  res
}
