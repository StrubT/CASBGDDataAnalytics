
WORLD <- read.csv('WORLD.CSV')
row.names(WORLD) <- WORLD$COUNTRY
str(WORLD)
head(WORLD)

WORLD_CLEAN <- na.omit(WORLD[, c(3, 5:16, 18)]) #c(3:16, 18)
str(WORLD_CLEAN)
head(WORLD_CLEAN)

library(factoextra)
library(cluster)
WORLD_PAM <- pam(scale(WORLD_CLEAN), 5) #WORLD_CLEAN[, -1]), 5)
fviz_cluster(WORLD_PAM)

WORLD_CLEAN$CL = WORLD_PAM$clustering
WORLD_PCOMP <- prcomp(scale(WORLD_CLEAN))
plot(WORLD_PCOMP)
biplot(WORLD_PCOMP, cex = 0.5)

WORLD_PRED <- predict(WORLD_PCOMP)
plot(WORLD_PRED, col = WORLD_CLEAN$CL)

library('corrplot')
corrplot(cor(data.frame(WORLD_PRED[,1:2], WORLD_CLEAN)), method = 'number')
