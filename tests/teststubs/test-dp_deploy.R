test_that("dp_deploy", {
	stub(dp_deployCore, 'dp_deploy', 100)
	stub(dp_deployCore, 'labkey_board', 100)
	stub(dp_deployCore, 'labkey_board', 100)
	stub(dp_deployCore, 's3_board', 100)
	stub(dp_deployCore, 's3_board', 100)
	dp_deploy(project_path = ".", ...) 
	expect_equal(2 * 2, 4)
})


test_that("dp_deployCore", {
	stub(dp_deployCore, 'dp_deploy', 100)
	stub(dp_deployCore, 'labkey_board', 100)
	stub(dp_deployCore, 'labkey_board', 100)
	stub(dp_deployCore, 's3_board', 100)
	stub(dp_deployCore, 's3_board', 100)
	dp_deployCore(conf, project_path, d, dlog, git_info, ...) 
	expect_equal(2 * 2, 4)
})


test_that("dp_deployCore.labkey_board", {
	
	dp_deployCore.labkey_board(conf, project_path, d, dlog, git_info, verbose = F, ...) 
	expect_equal(2 * 2, 4)
})


test_that("dp_deployCore.s3_board", {
	
	dp_deployCore.s3_board(conf, project_path, d, dlog, git_info, verbose = F, ...) 
	expect_equal(2 * 2, 4)
})


