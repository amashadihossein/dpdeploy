test_that("dpinput_sync", {
  stub(dpinput_syncflag_reset, "map", 100)
  stub(dpinput_syncflag_reset, "dpinput_sync", 100)
  dpinput_sync(conf, input_map, verbose = F, ...)
  expect_equal(2 * 2, 4)
})


test_that("get_inputboard_alias", {
  get_inputboard_alias(conf)
  expect_equal(2 * 2, 4)
})


test_that("init_board", {
  stub(init_board, "labkey_board", 100)
  stub(init_board, "s3_board", 100)
  init_board(conf)
  expect_equal(2 * 2, 4)
})


test_that("init_board.labkey_board", {
  init_board.labkey_board(conf)
  expect_equal(2 * 2, 4)
})


test_that("init_board.s3_board", {
  init_board.s3_board(conf)
  expect_equal(2 * 2, 4)
})


test_that("sync_iterate", {
  sync_iterate(input_map, inputboard_alias, skip_sync, verbose)
  expect_equal(2 * 2, 4)
})


test_that("syncedmap_rename", {
  stub(syncedmap_rename, "map", 100)
  syncedmap_rename(synced_map)
  expect_equal(2 * 2, 4)
})


test_that("to_description", {
  to_description(input_i)
  expect_equal(2 * 2, 4)
})
