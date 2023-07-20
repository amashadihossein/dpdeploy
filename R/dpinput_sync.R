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
  # grab rewrite_ok if passed in ...
  args <- list(...)
  rewrite_ok <- args$rewrite_ok
  if (length(rewrite_ok) == 0) {
    rewrite_ok <- F
  }

  if (verbose) {
    cli::cli_alert_info(glue::glue(
      "Starting sync to the ",
      "{conf$board_params$board_type} remote: ",
      "{conf$board_params$board_alias}"
    ))
  }

  skip_sync <- input_map$input_manifest %>%
    dplyr::filter(.data$to_be_synced == FALSE | .data$synced == TRUE &
      .data$to_be_synced == TRUE) %>%
    dplyr::pull(.data$id)

  to_be_synced <- setdiff(names(input_map$input_obj), skip_sync)

  if (length(to_be_synced) == 0 & !rewrite_ok) {
    if (verbose) {
      cli::cli_alert_info("No new unsynced data was found to be synced")
    }
    return(input_map$input_obj)
  }

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

  board <- init_board(conf = conf)

  synced_map <- sync_iterate(
    input_map = input_map,
    board_object = board, #get_inputboard_alias(conf),
    skip_sync = skip_sync,
    rewrite_ok = rewrite_ok,
    verbose = verbose
  )

  synced_map <- syncedmap_rename(synced_map = synced_map)
  to_be_synced <- pathnames_reroot(pathnames = to_be_synced) %>% unname()

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
        "Please check aws credentials. You need ",
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

  pins::board_s3(
    prefix = "dpinput/",
    bucket = conf$board_params$bucket_name,
    region = conf$board_params$region,
    access_key = aws_creds$key,
    secret_access_key = aws_creds$secret,
    versioned = TRUE
  )
}

#' @keywords internal
init_board.local_board <- function(conf) {
  input_params <- conf$board_params
  pins::board_register(
    board = "local",
    name = get_inputboard_alias(conf),
    cache = file.path(conf$board_params$folder, "dpinput"),
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

# TODO: move syncedmap_rename and pathnames_reroot upstream to dpinput
#' @keywords internal
syncedmap_rename <- function(synced_map) {
  rename_map <- pathnames_reroot(
    pathnames = names(synced_map),
    new_root = "input_files"
  )

  names(synced_map) <- rename_map[names(synced_map)]

  # update id accordingly in metadata
  synced_map <- purrr::modify_in(
    .x = synced_map,
    .where = list(1, "metadata", "id"),
    .f = ~ .x %>%
      pathnames_reroot(pathnames = .) %>%
      unname()
  )

  invisible(synced_map)
}

#' @title Re-root path names
#' @description if pathnames are of path format, it sets the root to `new_root`
#' dropping all upstream paths beyond `new_root`. If not of path format, it
#' keeps the pathnames unchanged
#' @param pathnames a vector of characters to be re-rooted
#' @param new_root a directory relative to which all paths be renamed
#' @keywords internal
pathnames_reroot <- function(pathnames, new_root = "input_files") {
  parsed_paths <- fs::path_split(pathnames) %>% `names<-`(pathnames)

  pathnames_rerooted <- sapply(parsed_paths, function(path_i) {
    rename_i <- path_i
    if (length(path_i) > 1 & new_root %in% path_i) {
      rename_i <- paste0(path_i[which(path_i == new_root):length(path_i)],
        collapse = "/"
      )
    }
    rename_i
  }, simplify = T, USE.NAMES = T)

  return(pathnames_rerooted)
}

#' @keywords internal
sync_iterate <- function(input_map, board_object, skip_sync, rewrite_ok = F,
                         verbose) {
  synced_map <- purrr::map(.x = input_map$input_obj, .f = function(input_i) {

    synced_versions <- pins::pin_versions(
      name = input_i$metadata$name,
      board = board_object
    ) %>%
      dplyr::pull(hash)

    input_i$metadata$synced <- input_i$metadata$pin_version %in% synced_versions

    skip_pin_to_remote <- T
    if (!input_i$metadata$id %in% skip_sync) {
      if (!input_i$metadata$synced | rewrite_ok) {
        skip_pin_to_remote <- F
      }
    }

    if (verbose & skip_pin_to_remote) {
      cli::cli_alert_info(glue::glue(
        "Input {input_i$metadata$name}",
        ", version {input_i$metadata$pin_version}",
        " is already synced or chosen to be skipped"
      ))
    }

    if (!skip_pin_to_remote) {
      tmp_pind <- try(pins::pin_write(
        x = input_i$data,
        name = input_i$metadata$name,
        board = board_object,
        description = input_i$metadata$description
      ))

      input_i$metadata$synced <- TRUE
      sync_attempt_state <- "completed"
      sync_alrt <- cli::cli_alert_success
      if ("try-error" %in% class(tmp_pind)) {
        input_i$metadata$synced <- FALSE
        sync_attempt_state <- "failed"
        sync_alrt <- cli::cli_alert_warning
      }

      # get_remote_pin_version <- pins::pin_versions(
      #   name = input_i$metadata$name,
      #   board = board_object
      #   ) %>%
      #   dplyr::arrange(dplyr::desc(version)) %>%
      #   dplyr::filter(dplyr::row_number()==1) %>%
      #   dplyr::pull(hash)
      #
      # input_i$metadata$description <- to_description(input_i = input_i)
      #
      # if (length(get_remote_pin_version) > 1) {
      #   latest_pin_version  <- sort(get_remote_pin_version)[length(get_remote_pin_version)]
      # } else {
      #   latest_pin_version <- get_remote_pin_version
      # }
      #
      # input_i$metadata$pin_version  <- latest_pin_version

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
