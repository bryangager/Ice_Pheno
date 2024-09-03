#Function for calculating water-year DOY. This will help facilitate plotting and analysizing trends in ice-in since they span either side of the winter-year (e.g., 2011-2012). For example, an IceInDayofYear_fed value of 150 means Ice-In occured 150 days after the start of the water-year (Oct1)

hydro.day = function(x, start.month = 10L) {
  start.yr = year(x) - (month(x) < start.month)
  start.date = make_date(start.yr, start.month, 1L)
  as.integer(x - start.date + 1L)
}

