context('elapsedMinSec')

test_that('formats object', {

  ptm <- proc.time()
  Sys.sleep(1)
  str <- format(elapsedMinSec(ptm))
  expect_equal(str,'Elapsed time is 0 min and 1 sec')

})
