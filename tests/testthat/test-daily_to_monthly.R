test_that("Read data set from Cedex site", {

  # Examples of daily data.
  url_daily <- c('9020' = "https://ceh.cedex.es/anuarioaforos/temp/resultado8187.txt",
                 '9021' = "https://ceh.cedex.es/anuarioaforos/temp/resultado7691.txt")


  # All stations at once.
  x1 <- read_cedex_basin()


  # Only one station at the time.
  for (i in url_daily) {
    x2 <- read_cedex_txt(i, "daily")
    y2 <- daily_to_monthly(x2, table = "afliq")

    # Filter station.
    xx1 <- x1[x1$Station == y2$Station[1], ]
    yy1 <- daily_to_monthly(xx1)


    # Match.
    j <- match(paste0(y2$Date), paste0(yy1$Date))
    expect_equal(y2$Flow, yy1$Flow[j])
    expect_equal(y2$Height, yy1$Height[j])

  }

})
