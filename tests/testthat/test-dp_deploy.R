test_that("properly checks valid repository ", {
  local_mocked_bindings(is_valid_dp_repository = function(path) FALSE )
  expect_snapshot(error = TRUE, {
    path <- '.'
    dp_deploy(project_path = path)
  })
})


test_that("object_read properly detects type", {
    
    project_path = withr::local_tempfile()
    path = file.path(project_path, "output_files/qs_format/")
    dir.create(path, recursive = TRUE)
    qs::qsave(structure(list(), class = "dp"), file = file.path(path, "data_object.qs"))
    expect_equal(
                detect_type(project_path), 
                'qs'
                )

    project_path = withr::local_tempfile()
    path = file.path(project_path, "output_files/RDS_format/")
    dir.create(path, recursive = TRUE)
    saveRDS(structure(list(), class = "dp"), file = file.path(path, "data_object.RDS"))
    expect_equal(
                detect_type(project_path), 
                'rds'
                )

})