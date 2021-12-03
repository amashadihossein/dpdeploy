#' @title Sync Input Data to Remote
#' @description Sync input data for a data product build to a remote like a
#' labkey or AWS S3
#' @param conf environment containing all config.R variables. See `dpconf_get()`
#' @param input_map object containing all input data to be synced. See `map_input()`
#' @param verbose T/F
#' @param ... other parameters e.g. verbose = T
#' @return synced_map this is input_map with sync status added to metadata
#' @importFrom dplyr .data
#' @export

dpinput_sync <- function(conf, input_map, verbose = F, ...) {
  if (verbose) {
    cli::cli_alert_info(glue::glue(
      "Starting sync to the",
      "{conf$board_params$board_type} remote: ",
      "{conf$board_params$board_alias}"
    ))
  }

  skip_sync <- input_map$input_manifest %>%
    dplyr::filter(.data$to_be_synced == FALSE | .data$synced == TRUE &
      .data$to_be_synced == TRUE) %>%
    dplyr::pull(.data$id)

  to_be_synced <- setdiff(names(input_map$input_obj), skip_sync)

  if (length(to_be_synced) == 0) {
    if (verbose) {
      cli::cli_alert_info("No new unsynced data was found to be synced")
    }
    return(input_map$input_obj)
  }

  # Add pin version and description
  input_map <- purrr::map(.x = input_map$input_obj, .f = function(input_i) {
    if (!input_i$metadata$id %in% skip_sync) {
      input_i$metadata$description <- to_description(input_i = input_i)
      input_i$metadata$pin_version <-
        get_pin_version(
          d = input_i$data,
          pin_name = input_i$metadata$name,
          pin_description = input_i$metadata$description
        )
    }
    input_i
  })


  init_board(conf = conf)

  synced_map <- sync_iterate(
    input_map = input_map,
    inputboard_alias = get_inputboard_alias(conf),
    skip_sync = skip_sync,
    verbose = verbose
  )

  synced_map <- syncedmap_rename(synced_map = synced_map)


  was_synced <- purrr::map(to_be_synced,
    .f = function(di) {
      purrr::pluck(synced_map, di, "metadata", "synced") %>%
        isTRUE()
    }
  ) %>% unlist()


  sync_fails <- to_be_synced[!was_synced]

  if ((n_failed <- length(sync_fails)) > 0) {
    warning(cli::format_warning("{n_failed} sync failures! See synced_map"))
  }

  return(synced_map)
}


#' @keywords internal
init_board <- function(conf) {
  UseMethod(generic = "init_board", object = conf)
}

#' @keywords internal
init_board.labkey_board <- function(conf) {
  input_params <- conf$board_params
  input_params$api_key <- conf$creds$api_key
  pins::board_register(
    board = "labkey",
    name = get_inputboard_alias(conf),
    api_key = input_params$api_key,
    base_url = input_params$url,
    folder = input_params$folder,
    path = "dpinput",
    versions = TRUE
  )
}

#' @keywords internal
init_board.s3_board <- function(conf) {
  aws_creds <- conf$creds
  if (aws_creds$key == "" | aws_creds$secret == "") {
    if (aws_creds$profile_name == "") {
      stop(cli::format_error(glue::glue(
        "Please check aws credentials. You need",
        "to provide either key and secret or ",
        "valid profile name"
      )))
    }
    aws_creds$key <-
      aws.signature::locate_credentials(profile = aws_creds$profile_name)$key
    aws_creds$secret <-
      aws.signature::locate_credentials(profile = aws_creds$profile_name)$secret
  }

  input_params <- list(
    board_alias = conf$board_params$board_alias,
    bucket_name = conf$board_params$bucket_name,
    region = conf$board_params$region,
    aws_key = aws_creds$key,
    aws_secret = aws_creds$secret
  )

  pins::board_register_s3(
    name = get_inputboard_alias(conf),
    bucket = input_params$bucket_name,
    key = input_params$aws_key,
    secret = input_params$aws_secret,
    path = "dpinput",
    region = input_params$region,
    versions = TRUE
  )
}

#' @keywords internal
get_inputboard_alias <- function(conf) {
  inputboard_alias <- paste0(conf$board_params$board_alias, "_dpinput")
  return(inputboard_alias)
}

#' @keywords internal
to_description <- function(input_i) {
  dsc <- glue::glue(
    "{input_i$metadata$file_name}_sha1_",
    "{substr(input_i$metadata$file_sha1,start = 1,stop = 7)}"
  )
  return(as.character(dsc))
}

#' @keywords internal
syncedmap_rename <- function(synced_map) {
  parsed_paths <- fs::path_split(names(synced_map)) %>%
    `names<-`(names(synced_map))

  rename_map <- sapply(parsed_paths, function(path_i) {
    rename_i <- path_i
    if (length(path_i) > 1) {
      rename_i <- paste0(path_i[which(path_i == "input_files"):length(path_i)],
        collapse = "/"
      )
    }
    rename_i
  }, simplify = T, USE.NAMES = T)

  names(synced_map) <- rename_map[names(synced_map)]
  invisible(synced_map)
}

#' @keywords internal
sync_iterate <- function(input_map, inputboard_alias, skip_sync, verbose) {
  synced_map <- purrr::map(.x = input_map, .f = function(input_i) {

    # This version conincidetally also addresses pins bug where data.txt can be
    # overwritten
    synced_versions <- pins::pin_versions(
      name = input_i$metadata$name,
      board = inputboard_alias
    )$version

    input_i$metadata$synced <- input_i$metadata$pin_version %in% synced_versions

    if (verbose & input_i$metadata$synced & !input_i$metadata$id %in% skip_sync) {
      cli::cli_alert_info(glue::glue(
        "Input {input_i$metadata$name}",
        ", version {input_i$metadata$pin_version}",
        " is already synced"
      ))
    }

    if (!input_i$metadata$id %in% skip_sync & !input_i$metadata$synced) {
      tmp_pind <- try(pins::pin(
        x = input_i$data,
        name = input_i$metadata$name,
        board = inputboard_alias,
        description = input_i$metadata$description
      ))

      sync_attempt_state <- "failed"
      sync_alrt <- cli::cli_alert_warning
      if (!"try-error" %in% class(tmp_pind)) {
        input_i$metadata$synced <- TRUE
        sync_attempt_state <- "completed"
        sync_alrt <- cli::cli_alert_success
      }

      if (verbose) {
        sync_alrt(glue::glue(
          "Input {input_i$metadata$name}, version ",
          "{input_i$metadata$pin_version} --> sync",
          " {sync_attempt_state}"
        ))
      }
    }

    input_i
  })

  invisible(synced_map)
}
