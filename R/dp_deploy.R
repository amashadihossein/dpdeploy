#' @title Deploy data product
#' @description Deploys a data product to the where the board is set (e.g.
#' AWS s3)
#' @param project_path path to project
#' @param ... other parameters e.g. verbose = T
#' @return TRUE
#' @export
dp_deploy <- function(project_path = ".", ...) {
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
  dlog <- get_dlog(project_path = project_path)
  d <- readRDS(file = glue::glue(
    "{project_path}/output_files/RDS_format/",
    "data_object.RDS"
  ))

  dp_deployCore(
    conf = conf, project_path = project_path, d = d, dlog = dlog,
    git_info = git_info, ...
  )
}

#' The dp_deploy is a wrapper around this.
#' Reason: With S3 generic methods, function calls as defaults parameters are
#' not recognized as the class of the object they return
#' @keywords internal
dp_deployCore <- function(conf, project_path, d, dlog, git_info, ...) {
  ellipsis::check_dots_used()
  UseMethod("dp_deployCore", object = conf)
}



#' @keywords internal
dp_deployCore.s3_board <- function(conf, project_path, d, dlog, git_info,
                                   verbose = F, ...) {
  if (verbose) {
    print(glue::glue("Deploying to S3 remote"))
  }

  # define board and pin dp to S3
  aws_creds <- conf$creds
  if (aws_creds$key == "" | aws_creds$secret == "") {
    if (aws_creds$profile_name == "") {
      stop(cli::format_error(
        "Please check aws credentials. You need to ",
        "provide either key and secret or valid profile ",
        "name"
      ))
    }

    aws_creds$key <-
      aws.signature::locate_credentials(profile = aws_creds$profile_name)$key
    aws_creds$secret <-
      aws.signature::locate_credentials(profile = aws_creds$profile_name)$secret
  }

  board <- pins::board_s3(
    prefix = file.path("daap/"),
    bucket = conf$board_params$bucket_name,
    region = conf$board_params$region,
    access_key = aws_creds$key,
    secret_access_key = aws_creds$secret,
    versioned = T
  )

  # This is force data.txt sync prior to pinning to address pins bug where
  # versions can be lost
  # ver_current <- pins::pin_versions(
  #   name = as.character(attr(d, "dp_name")),
  #   board = conf$board_params$board_alias
  # )

  pins::pin_write(
    x = d,
    name = as.character(attr(d, "dp_name")),
    board = board,
    description = as.character(attr(d, "branch_description"))
  )

  # Update dpboard_log
  dpboardlog_update(conf = conf, dlog = dlog, git_info = git_info)

  return(TRUE)
}


#' @keywords internal
dp_deployCore.local_board <- function(conf, project_path, d, dlog, git_info,
                                      verbose = F, ...) {
  if (verbose) {
    print(glue::glue("Deploying to local or mounted drive"))
  }


  # define board and pin dp to local board
  board_object <- pins::board_folder(path = file.path(conf$board_params$folder, "daap"),
                                     versioned = T)

  # This is force data.txt sync prior to pinning to address pins bug where
  # versions can be lost. not sure necessary for local board but not harmful
  # ver_current <- pins::pin_versions(
  #   name = as.character(attr(d, "dp_name")),
  #   board = conf$board_params$board_alias
  # )

  pins::pin_write(
    x = d,
    name = attr(d, "dp_name"),
    board = board_object,
    description = attr(d, "branch_description")
  )


  # Update dpboard_log
  dpboardlog_update(conf = conf, dlog = dlog, git_info = git_info)

  return(TRUE)
}
