library("ggplot2")



#scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))
ggplot(data = Rankings, aes(x = as.factor(Inventory), y = as.factor(reorder(Rank, sort(as.numeric(Rank)))), fill = as.factor(Rank))) + geom_bar(stat="identity") + scale_x_continuous(breaks = c(0, 10, 1))

