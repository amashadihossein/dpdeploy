test_that("dp_archive", {
	stub(dp_archiveCore, 'dp_archive', 100)
	stub(dp_archiveCore, 'labkey_board', 100)
	stub(dp_archiveCore, 'labkey_board', 100)
	stub(dp_archiveCore, 's3_board', 100)
	stub(dp_archiveCore, 's3_board', 100)
	dp_archive(project_path = ".", ...) 
	expect_equal(2 * 2, 4)
})


test_that("dp_archiveCore", {
	stub(dp_archiveCore, 'dp_archive', 100)
	stub(dp_archiveCore, 'labkey_board', 100)
	stub(dp_archiveCore, 'labkey_board', 100)
	stub(dp_archiveCore, 's3_board', 100)
	stub(dp_archiveCore, 's3_board', 100)
	dp_archiveCore(conf, dp_name, pin_version, git_info, ...) 
	expect_equal(2 * 2, 4)
})


test_that("dp_archiveCore.labkey_board", {
	
	dp_archiveCore.labkey_board(conf, dp_name, pin_version, git_info, verbose = F, ...) 
	expect_equal(2 * 2, 4)
})


test_that("dp_archiveCore.s3_board", {
	
	dp_archiveCore.s3_board(conf, dp_name, pin_version, git_info, verbose = F, ...) 
	expect_equal(2 * 2, 4)
})


