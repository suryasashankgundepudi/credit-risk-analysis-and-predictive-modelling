# # load the library
library(RCurl)

# specify the URL for the Iris data CSV
urlfile <-'https://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data'

# download the file
df <- read.csv(urlfile, sep = " ", header=FALSE)

# preview the first 5 rows
head(df)

write.csv(df, "data/german_credit_data", row.names = FALSE)