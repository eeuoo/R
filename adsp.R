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


??melt

T = reshape::melt(airquality, id = c("month", "day"), na.rm = TRUE)


reshape::cast(T, day~month~variable)

??cast

b = reshape2::acast(T, month~variable, mean)

b


