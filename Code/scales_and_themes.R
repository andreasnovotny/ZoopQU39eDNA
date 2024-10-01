
# Defining metazoan phyla for subseetting
metazoa <- c("Arthropoda",
             "Cnidaria",
             "Chaetognatha",
             "Annelida",
             "Porifera",
             "Echinodermata",
             "Mollusca",
             "Phoronida",
             "Bryozoa",
             "Nemertea",
             "Ctenophora",
             "Priapulida",
             "Rotifera",
             "Chordata",
             "Platyhelminthes")

# Quarternal breaks for plotting:
datebreaks <- c(
  "2015-01-01", "2015-04-01", "2015-07-01", "2015-10-01",
  "2016-01-01", "2016-04-01", "2016-07-01", "2016-10-01",
  "2017-01-01", "2017-04-01", "2017-07-01", "2017-10-01",
  "2018-01-01", "2018-04-01", "2018-07-01", "2018-10-01",
  "2019-01-01", "2019-04-01", "2019-07-01", "2019-10-01") %>% as_date()

# Quartal breaks for plotting:
yearbreaks <- c(
  "2015-01-01", "2016-01-01",
  "2017-01-01", "2018-01-01", "2019-01-01") %>% as_date()



# Seasonal monthly color theme (By Anna Mc.Laskey)
seasonal_colors = c("#053061", "#3288BD", "#66C2A5", "#7FBC41",
                    "#A6D96A", "#FEE08B", "#FDAE61", "#F46D43",
                    "#D53E4F", "#9E0142", "#67001F", "#40004B")

# Depth color theme
depth_colors = c("#60dd8e", "#3f9f7f", "#188a8d", "#17577e", "#141163")

