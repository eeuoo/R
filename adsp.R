install.packages("reshape")
library(reshape)

install.packages("reshape2")

data("airquality")     # airquality 
airquality

colnames(airquality)
tolower( colnames(airquality) )

colnames(airquality) <- tolower( colnames(airquality) )

head(airquality)
head(airquality, 3)
