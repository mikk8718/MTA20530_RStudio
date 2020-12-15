library("dplyr")
library("pastecs")
library("xlsx")
library("ggplot2")

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


sumTable <- table(MECSPEC_SUM)

#gridModes <- c(getmode(grid$Q1),getmode(grid$Q2), getmode(grid$Q3),getmode(grid$Q4),getmode(grid$Q5),getmode(grid$Q6), getmode(grid$Q7),getmode(grid$Q8))
gridMedian <- c(median(grid$Q1), median(grid$Q2), median(grid$Q3), median(grid$Q4), median(grid$Q5), median(grid$Q6), median(grid$Q7), median(grid$Q8))
linearMedian <- c(median(linear$Q1), median(linear$Q2), median(linear$Q3), median(linear$Q4), median(linear$Q5), median(linear$Q6), median(linear$Q7), median(linear$Q8))
circularMedian <- c(median(circular$Q1), median(circular$Q2), median(circular$Q3), median(circular$Q4), median(circular$Q5), median(circular$Q6), median(circular$Q7), median(circular$Q8))
unrestrictedMedian <- c(median(unrestricted$Q1), median(unrestricted$Q2), median(unrestricted$Q3), median(unrestricted$Q4), median(unrestricted$Q5), median(unrestricted$Q6), median(unrestricted$Q7), median(unrestricted$Q8))


shapiro.test(data$Q1)
kruskal.test((Q8)~Layout.ID, data = data)
pairwise.wilcox.test(data$Q1, data$Layout.ID, p.adjust.method = "BH")
stat.desc(data)

sumLinear <- sum(c(sum(linear$Q1), sum(linear$Q2), sum(linear$Q3), sum(linear$Q4), sum(linear$Q5), sum(linear$Q6), sum(linear$Q7), sum(linear$Q8)))
sumCircular <- sum(c(sum(circular$Q1),sum(circular$Q2), sum(circular$Q3), sum(circular$Q4), sum(circular$Q5), sum(circular$Q6), sum(circular$Q7), sum(circular$Q8)))
sumGrid <- sum(c(sum(grid$Q1),sum(grid$Q2), sum(grid$Q3), sum(grid$Q4), sum(grid$Q5), sum(grid$Q6), sum(grid$Q7), sum(grid$Q8)))
sumUnrestricted <- sum(c(sum(unrestricted$Q1), sum(unrestricted$Q2), sum(unrestricted$Q3), sum(unrestricted$Q4), sum(unrestricted$Q5), sum(unrestricted$Q6), sum(unrestricted$Q7), sum(unrestricted$Q8)))


barplot(MECSPEC_SUM$Sum.total, main = "sum of total", 
        xlab = "inventory layouts", 
        ylab = "sum of scores",  
        names.arg=c("Grid", "Circular", "Linear", "Unrestricted")) + grid(nx = NULL, ny = 20)


barplot(MECSPEC_SUM$Sum.median, main = "sum of median",
        xlab = "inventory layouts",
        ylab = "median of scores",
        names.arg=c("Grid", "Circular", "Linear", "Unrestricted")) + grid(nx = NULL, ny = 20)



ggplot(MECSPEC_SUM, aes(x = Task.ID, y = Sum.total, fill = Task.ID)) + 
  geom_bar(stat = "identity") + scale_y_continuous(breaks = seq(0,400,25)) + 
  labs(x = "Inventory", y = "sum of scores") + 
  scale_fill_discrete(name = "Inventory")

ggplot(MECSPEC_SUM, aes(x = Task.ID, y = Sum.median, fill = Task.ID, geom_hline(yintercept=24, show.legend = TRUE))) + 
  geom_bar(stat = "identity")+ scale_y_continuous(breaks = seq(0,34,2)) + 
  labs(x = "Inventory", y = "median of scores") + 
  scale_fill_discrete(name = "Inventory")







