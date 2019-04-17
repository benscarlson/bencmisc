context('ggmap')

test_that('get map using centroid object', {

  centroid <- c(X=-97.1161,Y=31.55098)
  class(centroid) <- 'centroid'

  m <- getMapRetry(centroid, 12, "satellite")

  expect_true(
    'ggmap' %in% class(m)
  )
})

test_that('get map using sf object', {

  dat <- data.frame(lon=rnorm(100,11,.2), lat=rnorm(100,53,.2))

  pts <- dat %>% st_as_sf(coords=c('lon','lat'),crs=4326)

  m <- getMapRetry(pts, 12, "satellite")

  expect_true(
    'ggmap' %in% class(m)
  )
})

test_that('get map using data.frame object', {

  dat <- data.frame(lon=rnorm(100,11,.2), lat=rnorm(100,53,.2))

  m <- getMapRetry(dat, 12, "satellite")

  expect_true(
    'ggmap' %in% class(m)
  )
})
