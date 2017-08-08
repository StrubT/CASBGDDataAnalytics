
WORLD <- read.csv('WORLD.CSV')
str(WORLD)
plot(WORLD)

WORLD_CLEAN <- na.omit(WORLD[,3:15])
str(WORLD_CLEAN)
plot(WORLD_CLEAN)

WORLD_COR <- cor(WORLD_CLEAN)
WORLD_COR
library(corrplot)
corrplot(WORLD_COR)
corrplot(WORLD_COR, method = 'number')
corrplot(WORLD_COR, method = 'ellipse')
# +0.96: EDUC / GNPCAP
# -0.96: LIFEEXP / BABYMORT
