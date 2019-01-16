context('dfExtent')

load(system.file('data/test_data.rda', package="bencmisc"))
#load(file='/Users/benc/projects/anno/data/test_data.rda')

test_that('dfExtent, simple example', {

  expect_true(
    class(dfExtent(dat=test_data))=='list'
  )

})

test_that('geeExtentStr, simple example', {

  expect_true(
    class(geeExtentStr(dat=test_data))=='character'
  )

})
