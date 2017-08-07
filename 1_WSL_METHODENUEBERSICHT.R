########################################################################
###### 1_WSL_METHODENUEBERSICHT.R
###### Lösungsvorschläge zu Workshops
########################################################################


########################################################################
###### Vorbereitungen
########################################################################

setwd("C:/PRJ/BFH/2017_CAS_BGD/3_DATA")
options(digits=6, width = 72)
rm(list=ls())


########################################################################
###### 1 Einführung
########################################################################



########################################################################
###### 2 Daten Laden und erste Sichtung
########################################################################



########################################################################
###### 3 Univariate Methoden mit metrischen Variablen
########################################################################



########################################################################
###### 4 Univariate Methoden mit kategorialen Variablen
########################################################################

rm(list=ls())

WORLD_DF <- read.csv("WORLD.CSV")
attach(WORLD_DF)

table(GOVERN_S, GOVERN)
subset(WORLD_DF, GOVERN == 5, select = c(COUNTRY, GOVERN, GOVERN_S))
subset(WORLD_DF, GOVERN == 6, select = c(COUNTRY, GOVERN, GOVERN_S))

table(RELIGION_S, RELIGION)
subset(WORLD_DF, RELIGION == 3, select = c(COUNTRY, RELIGION, RELIGION_S))
subset(WORLD_DF, RELIGION == 8, select = c(COUNTRY, RELIGION, RELIGION_S))

subset(WORLD_DF, RELIGION == 2, select = c(COUNTRY, RELIGION, RELIGION_S))
subset(WORLD_DF, RELIGION == 7, select = c(COUNTRY, RELIGION, RELIGION_S))

detach(WORLD_DF)



########################################################################
###### 5 Bivariate Methoden (metrische Variablen)
########################################################################

rm(list=ls())

## Lesen des Datasets in einen data frame
WORLD_DF <- read.csv("WORLD.CSV")

## entfernen aller Zeilen mit NAs
WORLD_DF <- na.omit(WORLD_DF)

## entfernen aller nicht numerischer Variablen 
## sowie ID, welche als Label auch nicht verwendet werden sollte
WORLD_DF <- WORLD_DF[,c(-1, -2, -17, -19)]

## Korrelationsmatrix erstellen und ausgeben
WORLD_COR <- cor(WORLD_DF)
WORLD_COR
## oops, das ist etwas unübersichtlich..
## daher
library(corrplot)
corrplot(cor(WORLD_DF))
## immer noch suboptimal
## Beispielcode aus 2_METHODENUEBERSICHT.R, angepasst
library(GGally)
ggcorr(WORLD_DF, 
       label = TRUE, 
       label_size = 3,
       hjust = 0.8, 
       size = 2.5, 
       color = "black", 
       layout.exp = 2,
       low="#F21A00",
       mid="#EEEEEE",
       high="#3B9AB2")



########################################################################
###### 6 Multivariate Methoden ohne Target
########################################################################

rm(list=ls())

## Dataset lesen
WORLD_DF <- read.csv("WORLD.CSV")

## Ländername als Rowname definieren
row.names(WORLD_DF) <- WORLD_DF[,2]

## nicht numerische Variablen entfernen
WORLD_DF <- WORLD_DF[,c(-1, -2, -17, -19)]

## Zeilen mit Missing Values entfernen
WORLD_DF <- na.omit(WORLD_DF)

## alle Variablen standardisieren
WORLD_DF <- scale(WORLD_DF)

## Kombinierte Clusteranalyse und PCA
WORLD_PAM <- pam(WORLD_DF[,-1], 5)

## Visualisieren
fviz_cluster(WORLD_PAM)



########################################################################
###### 7 Multivariate Methoden mit Target (Klassifikation)
########################################################################



########################################################################
###### 8 Assoziationsanalyse
########################################################################

rm(list=ls())

TAO <- read.transactions("LEBENSMITTEL_TR.CSV", sep=",")
summary(TAO)
itemFrequencyPlot(TAO, topN=5, type="absolute")
RULES <- apriori(TAO,
                 parameter = list(
                   supp=0.001,
                   conf=0.5,
                   target="rules"))

inspect(head(sort(RULES , by="lift")))
plot(RULES,method="graph")
plot(RULES, method="graph",interactive=TRUE,shading=NA)

## Regeln mit ROLLS.BUNS und SODA
TESTSET <- as.vector(c("ROLLS.BUNS","SODA"))
RULES_MATCH_LHS <- subset(RULES, subset=lhs %ain% TESTSET)
inspect(RULES_MATCH_LHS)

