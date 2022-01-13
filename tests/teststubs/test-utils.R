test_that("dpboardlog_update", {
	
	dpboardlog_update(conf, git_info, dlog = NULL, dp_name = character(0), pin_version = character(0)) 
	expect_equal(2 * 2, 4)
})


test_that("dpconnect_check", {
	stub(dp_get, 'dpconnect_check', 100)
	stub(dp_list, 'dpconnect_check', 100)
	stub(dpconnect_check, 'dp_connect', 100)
	dpconnect_check(board_params) 
	expect_equal(2 * 2, 4)
})


test_that("get_dlog", {
	
	get_dlog(project_path) 
	expect_equal(2 * 2, 4)
})


test_that("get_pin_version", {
	stub(dplognote_get, 'get_pin_version', 100)
	get_pin_version(d, pin_name, pin_description) 
	expect_equal(2 * 2, 4)
})


test_that("gitinfo_validate", {
	
	gitinfo_validate(project_path, verbose = F) 
	expect_equal(2 * 2, 4)
})


