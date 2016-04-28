library(devtools)
install_github("Rfacebook", "pablobarbera", subdir = "Rfacebook")
library(Rfacebook)

source("define_keys.R")

token <- "CAACEdEose0cBAGZBFtvf8xS7dlyAddbt1pw3Q8LbaGFb7kz6OKabhQ36X886xBArRvLSxbWEKoZBT6zi4ZAzRGApp12MCRl9ys840MnVmvGhYG91cCv7KXpqG0uxZANTOgkZA8kOlrziY9g8YQkN6hWY64LMV5x3rko81dMfDJOQc0dehePFupF4mjCjBRAlZAAs5WjZAaGcPwLy78lzn23"

# fb_oauth <- fbOAuth(app_id=fAppID, app_secret=fSecret,extended_permissions = TRUE)
# save(fb_oauth, file="fb_oauth")

load("fb_oauth")

my_likes <- getLikes(user="me", token=fb_oauth)

posts <- searchFacebook(string = "upworthy", 
                        token, n = 500, since = "25 november 2013 00:00", until = "25 november 2013 23:59")

page <- getPage("humansofnewyork", token, n = 5000)

me <- searchFacebook("Trump", fb_oauth, n=5)
me$name