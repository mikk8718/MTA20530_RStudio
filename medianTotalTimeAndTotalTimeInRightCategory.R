library("ggplot2")

#if (TRUE) 1 else 3

ggplot(data=medianCorrectAndFinalReformatted, aes(x=Inventory, y=Time, fill=Medians)) + geom_bar(stat="identity", position=position_dodge()) + 
  xlab("Total time in right category") + geom_text(aes(label=Time), vjust=-1, hjust=ifelse(medianCorrectAndFinalReformatted$Medians == "Total time", 2.4, -1.2), size=3.5, inherit.aes = TRUE) + 
  theme_bw() +  theme(axis.text.x=element_text(size=rel(2)), 
                      axis.text.y=element_text(size=rel(2)),
                      axis.title.x = element_text(size=rel(2)),
                      axis.title.y = element_text(size=rel(2)),
                      legend.text=element_text(size=rel(1.8)),
                      legend.title = element_text(size=rel(2)))
