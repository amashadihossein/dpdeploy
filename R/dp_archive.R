#' @title Archive data product
#' @description This undoes dp_deploy. It removes the deployed data product and
#' updates the dpboard-log
#' @param project_path path to project
#' @param ... other parameters e.g. verbose = T
#' @return TRUE
#' @export
dp_archive <- function(project_path = ".", ...) {
  if (!is_valid_dp_repository(path = project_path)) {
    stop(cli::format_error(glue::glue(
      "project_path, {project_path},",
      " does not point to a valid dp project.",
      " Make sure project path is set up with ",
      "dp_init Run ",
      "dpbuild:::dp_repository_check"
    )))
  }

  # validate and retrieve git info
  git_info <- gitinfo_validate(project_path = project_path, verbose = F)

  # get daap content and info
  conf <- dpconf_get(project_path = project_path)

  args <- list(...)
  dp_specified <- is.character(args$dp_name) & length(args$dp_name) > 0 &
    is.character(args$pin_version) & length(args$pin_version) > 0

  if (dp_specified) {
    dp_name <- args$dp_name
    pin_version <- args$pin_version
  } else {
    dlog <- get_dlog(project_path = project_path)
    dp_name <- dlog[[dlog$HEAD]][["dp_name"]]
    pin_version <- dlog[[dlog$HEAD]][["pin_version"]]
  }

  dp_archiveCore(
    conf = conf, dp_name = dp_name, pin_version = pin_version,
    git_info = git_info, ...
  )
}

#' The dp_archive is a wrapper around this.
#' Reason: With S3 generic methods, function calls as defaults parameters are
#' not recognized as the class of the object they return
#' @keywords internal
dp_archiveCore <- function(conf, dp_name, pin_version, git_info, ...) {
  ellipsis::check_dots_used()
  UseMethod("dp_archiveCore", object = conf)
}



#' @keywords internal
dp_archiveCore.labkey_board <- function(conf, dp_name, pin_version,
                                        git_info, verbose = F, ...) {
  if (verbose) {
    print(glue::glue(
      "Archiving {dp_name}, version {pin_version} from Labkey ",
      "remote"
    ))
  }


  # define board and pin dp to labkey
  pins::board_register(
    board = "labkey",
    name = conf$board_params$board_alias,
    api_key = conf$creds$api_key,
    base_url = conf$board_params$url,
    folder = conf$board_params$folder,
    path = "daap",
    versions = T
  )

  # This is force data.txt sync prior to pinning to address pins bug where
  # versions can be lost
  ver_current <- pins::pin_versions(
    name = dp_name,
    board = conf$board_params$board_alias
  )

  pins::pin_remove(name = dp_name, board = conf$board_params$board_alias)


  # Update dpboard_log
  dpboardlog_update(
    conf = conf, git_info = git_info, dp_name = dp_name,
    pin_version = pin_version
  )

  return(TRUE)
}


#' @keywords internal
dp_archiveCore.s3_board <- function(conf, dp_name, pin_version,
                                    git_info, verbose = F, ...) {
  if (verbose) {
    print(glue::glue(
      "Archiving {dp_name}, version {pin_version} ",
      "from S3 remote"
    ))
  }

  # define board and pin dp to S3
  aws_creds <- conf$creds
  if (aws_creds$key == "" | aws_creds$secret == "") {
    if (aws_creds$profile_name == "") {
      stop(cli::format_error(glue::glue(
        "Please check aws credentials. You ",
        "need to provide either key and secret",
        " or valid profile name"
      )))
    }
    aws_creds$key <-
      aws.signature::locate_credentials(profile = aws_creds$profile_name)$key
    aws_creds$secret <-
      aws.signature::locate_credentials(profile = aws_creds$profile_name)$secret
  }

  pins::board_register(
    board = "s3",
    name = conf$board_params$board_alias,
    bucket = conf$board_params$bucket_name,
    versions = TRUE,
    key = aws_creds$key,
    secret = aws_creds$secret,
    region = conf$board_params$region,
    path = "daap"
  )

  # This is force data.txt sync prior to pinning to address pins bug where
  # versions can be lost
  ver_current <- pins::pin_versions(
    name = dp_name,
    board = conf$board_params$board_alias
  )

  pins::pin_remove(name = dp_name, board = conf$board_params$board_alias)


  # Update dpboard_log
  dpboardlog_update(
    conf = conf, git_info = git_info, dp_name = dp_name,
    pin_version = pin_version
  )


  return(TRUE)
}
