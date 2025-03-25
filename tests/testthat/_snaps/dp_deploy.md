# properly checks valid repository 

    Code
      path <- "."
      dp_deploy(project_path = path)
    Condition
      Error in `dp_deploy()`:
      ! project_path, ., does not point to a valid dp project. Make sure project path is set up with dp_init Run dpbuild:::dp_repository_check

