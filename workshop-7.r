
library(arules)
library(arulesViz)

TR <- read.transactions("LEBENSMITTEL_TR.CSV", sep = ",")
TR
summary(TR)

itemFrequencyPlot(TR, topN = 15, type = "absolute")
#image(TR)

RULES <- apriori(TR,
	parameter = list(
		supp = 0.001,
		conf = 0.5,
		target = "rules"))
RULES

inspect(sort(RULES, by = "lift"))
plot(RULES, method = "graph")

inspect(subset(RULES, subset = lhs %ain% as.vector(c("ROLLS.BUNS", "SODA"))))
