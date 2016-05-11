source("functions/get_data.R")
source("functions/plot_tc.R")

stderr <- function(x) sqrt( var(x,na.rm=TRUE) / nobs(x) )

