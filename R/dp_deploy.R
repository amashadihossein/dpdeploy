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
  d <- object_read(project_path = project_path)

  dp_deployCore(
    conf = conf, project_path = project_path, d = d, dlog = dlog,
    git_info = git_info, ...
  )
}

object_read <- function(project_path){
  type = fs::dir_ls(file.path(project_path,'output_files/'), recurse = T, regexp = 'data_object') |> tools::file_ext() |> tolower()
  switch(type, 
  rds = readRDS(file = glue::glue("{project_path}/output_files/RDS_format/data_object.RDS"),
  qs = read_qs(project_path)
  ))
}

read_qs <- function(path){
  rlang::check_installed("qs")
  dataobj_path <- glue::glue(
    "{path}/",
    "output_files/qs_format/data_object.qs"
  )
  qs::qread(dataobj_path)
  return(dataobj_path)
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
    prefix = "daap/",
    bucket = conf$board_params$bucket_name,
    region = conf$board_params$region,
    access_key = aws_creds$key,
    secret_access_key = aws_creds$secret,
    versioned = T
  )

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
dp_deployCore.labkey_board <- function(conf, project_path, d, dlog, git_info,
                                       verbose = F, ...) {
  if (verbose) {
    print(glue::glue("Deploying to LabKey remote"))
  }

  # define board and pin dp to LabKey
  labkey_creds <- conf$creds
  if (labkey_creds$api_key == "") {
    stop("Please check LabKey credentials. You need to provide api_key")
  }

  board <- pinsLabkey::board_labkey(
    cache_alias = conf$board_params$cache_alias,
    api_key = labkey_creds$api_key,
    base_url = conf$board_params$url,
    folder = conf$board_params$folder,
    versioned = T,
    subdir = "daap/"
  )

  pinsLabkey::pin_write(
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
  board_object <- pins::board_folder(
    path = file.path(conf$board_params$folder, "daap"),
    versioned = T
  )

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
