#' @importFrom dpbuild dpconf_get
#' @export
#' @name dpconf_get
dpbuild::dpconf_get

#' @importFrom dpbuild is_valid_dp_repository
#' @export
#' @name is_valid_dp_repository
dpbuild::is_valid_dp_repository


#' @title Validate git info for deploy
#' @description Validates and extracts gitinfo per deploy requirements
#' @param project_path path to project
#' @param verbose F if TRUE prints process details
#' @return git_info, a list containing git information
#' @keywords internal
gitinfo_validate <- function(project_path, verbose = F){
  
  #--- Check git set up-------
  repo <- git2r::repository(path = project_path)
  last_commit <- git2r::last_commit(repo = repo)
  
  git_info_valid <- nchar(git_sha <- as.character(last_commit$sha)) > 0 & 
    nchar(git_uname <- as.character(last_commit$author$name)) > 0 & 
    nchar(git_uemail <-  as.character(last_commit$author$email)) > 0 &
    nchar(git_timestamp <- paste0(last_commit$author$when,collapse = " ")) > 0
  
  if(!git_info_valid)
    stop(cli::format_error(glue::glue("Failed to retrieve git info.",
                                      " Info retrieved from last commit git sha: {git_sha},",
                                      " author: {git_uname}, email: {git_uemail}.",
                                      " Ensure dp_commit is executed before dpdeploy")))
  git_info <- list(git_sha = git_sha, git_uname = git_uname,
                   git_uemail = git_uemail, git_timestamp = git_timestamp)
  
  #-----Check remote git url-------------
  remote_url <- try(git2r::remote_url(repo = ".",remote = git2r::remotes()),silent = T)
  has_remote_url <- class(remote_url) != "try-error"
  if(verbose){
    if(has_remote_url)
      print(glue::glue("has remote git url ",paste(remote_url,collapse = ", and ")) )
    if(!has_remote_url)
      print("No remote git url found. Have you pushed to GitHub before deploy?")
  }
  
  git_info$remote_url <- remote_url
  
  return(git_info)
}



#' @title Update dpboard log
#' @description Updates the metadata associated with the board and retrievable
#' with dp_list. When deploying dlog is needed when archiving dp_name and
#' pin_version are needed.
#' @param conf output of `dpconf_get` 
#' @param git_info a list returned from `gitinfo_validate`
#' @param dlog daap_log. This is only needed when adding record
#' @param dp_name name of the pin to be archived. Ignored when dlog is provided.
#' @param pin_version version of the pin to be archived. 
#' Ignored when dlog is provided
#' @return TRUE
#' @keywords internal
dpboardlog_update <- function(conf, git_info, dlog = NULL, 
                              dp_name = character(0), 
                              pin_version = character(0)){
  
  board_info <- dpconnect_check(board_params = conf$board_params)
  
  if(board_info$subpath != "daap")
    stop(cli::format_error(glue::glue("dpboard is not pointing to daap ",
                                      "subfolder on remote. Check board.")))
  
  dpboard_log <- try(pins::pin_get(name = "dpboard-log",
                                   board = conf$board_params$board_alias,
                                   files = F, cache = F))
  if(!"data.frame" %in% class(dpboard_log))
    dpboard_log <- NULL
  
  if(length(dlog) == 0){
    if(length(dp_name) == 0 | length(pin_version) == 0)
      stop(cli::format_error(glue::glue("Cannot update. dlog, dp_name and ",
                                        "pin_version all have length 0")))
    if(is.null(dpboard_log))
      stop(cli::format_error(glue::glue("dpboard-log was not found. Make sure ",
                                        "dpboard-log exists for this board")))
    
    # update the records based on composite key dp_name, dp_version, and git_sha 
    dpboard_log_tmp <- dpboard_log %>% 
      dplyr::filter(.data$dp_name != dp_name | .data$pin_version != pin_version |
                      .data$git_sha != git_info$git_sha)
    
    tmp <- dpboard_log %>% 
      dplyr::filter(.data$dp_name == dp_name & .data$pin_version == pin_version &
                      .data$git_sha == git_info$git_sha)
    if(nrow(tmp) == 0)
      stop(cli::format_error(glue::glue("The provided compbination of dp_name ",
                                        "{dp_name}, dp_version {dp_version}, ",
                                        "and git_sha {git_info$git_sha} is not",
                                        " in dpboard-log. Verify the values ","
                                        are correct!")))
    
    tmp <- tmp %>% dplyr::mutate(archived = TRUE)
    dpboard_log <- dplyr::bind_rows(dpboard_log_tmp, tmp) %>% 
      dplyr::distinct()
    
    # This is force data.txt sync prior to pinning to address pins bug where 
    # versions can be lost
    ver_current <- pins::pin_versions(name = "dpboard-log",
                                      board = conf$board_params$board_alias)
    
    pins::pin(x = dpboard_log,
              name = "dpboard-log", 
              board = conf$board_params$board_alias,
              description = "Data Product Log")
    
    return(TRUE)
  }
  
  # Update dp manifest
  daap_log_i <- dlog[dlog$HEAD]
  
  # Augment with git info
  daap_log_i[[1]]$git_sha <- git_info$git_sha
  daap_log_i[[1]]$commit_time <- git_info$git_timestamp
  daap_log_i[[1]]$git_author_name <- git_info$git_uname
  daap_log_i[[1]]$git_author_email <- git_info$git_uemail
  daap_log_i[[1]]$git_remote <- git_info$remote_url
  
  # Convert to table
  daap_log_i <- daap_log_i %>% dplyr::bind_rows(.id = "rdsid") %>% 
    dplyr::mutate(rdsid = gsub("rds_", "", .data$rdsid)) %>%
    dplyr::mutate(dp_name = gsub(pattern = "_",replacement = "-", x = .data$dp_name)) %>%
    dplyr::relocate(.data$dp_name) %>%
    dplyr::mutate(last_deployed = format(Sys.time(), tz = "GMT", usetz = TRUE)) %>%
    dplyr::mutate(archived = FALSE)
  
  
  if(is.null(dpboard_log))
    dpboard_log <- daap_log_i %>% dplyr::slice(0)
  
  # Update deploy time if same pin/git_sha exist otherwise append
  tmp <- dpboard_log %>% 
    dplyr::filter(.data$dp_name != daap_log_i$dp_name | 
                    .data$pin_version != daap_log_i$pin_version | 
                    .data$git_sha != daap_log_i$git_sha)
  
  dpboard_log <- dplyr::bind_rows(tmp, daap_log_i) %>%
    dplyr::distinct()
  
  # This is force data.txt sync prior to pinning to address pins bug where 
  # versions can be lost
  ver_current <- pins::pin_versions(name = "dpboard-log",
                                    board = conf$board_params$board_alias)
  pins::pin(x = dpboard_log,
            name = "dpboard-log", 
            board = conf$board_params$board_alias,
            description = "Data Product Log")
  
  return(TRUE)
}


#' @title Get Pins Version Pre Deploy
#' @description  This get the pins version pre-deploy
#' @param d data object
#' @param pin_name what the pin will be named. For data products, it is encoded in dp_param
#' @param pin_description what the pin description will be. For data products, it is encoded in dp_params
#' @return a character version
#' @importFrom dplyr .data
#' @keywords internal
get_pin_version <- function(d, pin_name, pin_description){
  
  pin_name <- as.character(pin_name)
  pin_description <- as.character(pin_description)
  
  pins::board_register_local(name = "daap_internal", version = T)
  
  
  pins::pin_remove(name =pin_name, board = "daap_internal")
  pins::pin(x = d,
            name =pin_name,
            board = "daap_internal",
            description = pin_description)
  
  pin_version <-  pins::pin_versions(name =pin_name,
                                     board = "daap_internal",
                                     full = F) %>% dplyr::pull(.data$version)
  pins::pin_remove(name =pin_name, board = "daap_internal")
  
  return(pin_version)
}


#' @title Get dlog
#' @description Reads and format daap_log.yml making pasting values is key:value
#' pairs with delimmitter " > "
#' @return dlog
#' @keywords internal
get_dlog <- function(project_path){
  dlog <- yaml::read_yaml(file = glue::glue("{project_path}/.daap/daap_log.yaml"))
  dlog <- purrr::modify_depth(.x = dlog,.depth = 2,
                              .f = function(x) paste0(x,collapse = " > "))
  return(dlog)
}


#' @title  Check dpconnect executed
#' @description This checks state whether dpconnect is already executed
#' @param board_params board_params (only the alias needed)
#' @keywords internal
dpconnect_check <- function(board_params){
  board_info <- try(pins::board_get(name = board_params$board_alias),silent = T)
  if("try-error" %in% class(board_info))
    stop(cli::format_error(glue::glue("You are not currently connected to ",
                                      "{board_params$board_alias}. Use ",
                                      "dp_connect to connect first!")))
  invisible(board_info)
}

