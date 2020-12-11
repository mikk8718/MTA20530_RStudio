library("dplyr")
library("pastecs")
library("xlsx")

getmode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


data <- group_by(MECSortedCSV,Layout.ID, Q1, Q2, Q3, Q4, Q5, Q6, Q7, Q8 )
#data <- summarise(data)
linear <- subset(data, Layout.ID == 1)
circular <- subset(data, Layout.ID == 2)
grid <- subset(data, Layout.ID == 3)
unrestricted <- subset(data, Layout.ID == 4)

#gridModes <- c(getmode(grid$Q1),getmode(grid$Q2), getmode(grid$Q3),getmode(grid$Q4),getmode(grid$Q5),getmode(grid$Q6), getmode(grid$Q7),getmode(grid$Q8))
gridMedian <- c(median(grid$Q1), median(grid$Q2), median(grid$Q3), median(grid$Q4), median(grid$Q5), median(grid$Q6), median(grid$Q7), median(grid$Q8))
linearMedian <- c(median(linear$Q1), median(linear$Q2), median(linear$Q3), median(linear$Q4), median(linear$Q5), median(linear$Q6), median(linear$Q7), median(linear$Q8))
circularMedian <- c(median(circular$Q1), median(circular$Q2), median(circular$Q3), median(circular$Q4), median(circular$Q5), median(circular$Q6), median(circular$Q7), median(circular$Q8))
unrestrictedMedian <- c(median(unrestricted$Q1), median(unrestricted$Q2), median(unrestricted$Q3), median(unrestricted$Q4), median(unrestricted$Q5), median(unrestricted$Q6), median(unrestricted$Q7), median(unrestricted$Q8))


shapiro.test(data$Q1)
kruskal.test(data)
stat.desc(data)

