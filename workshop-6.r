
## INIT

BANK_DF <- read.csv("bank-full.csv", sep = ";")

NDX <- 1:nrow(BANK_DF)
N <- 10000
NDX_SMPL <- sample(NDX, N)
BANK_DF <- BANK_DF[NDX_SMPL,]

BANK_FORM <- y ~ .

N <- nrow(BANK_DF)
FOLD <- 10
BANK_DF <- BANK_DF[sample(1:N, N),]
SEGMENT <- trunc((0:(N - 1)) / N * FOLD) + 1
BANK_DF <- data.frame(BANK_DF, SEGMENT)

FNC_GET_PERFORMANCE <- function(DF, FORM, SEG) {

	TRAIN <- subset(DF, SEGMENT != SEG)
	TEST <- subset(DF, SEGMENT == SEG)

	MODEL <- randomForest(FORM, TRAIN[, - ncol(TRAIN)])
	PRED <- predict(MODEL, TEST[, - ncol(TEST)], type = "class")
	TAB <- table(TEST$y, PRED)
	RESULT <- sum(diag(TAB)) / sum(TAB)
	return(RESULT)
}

library(randomForest)
library(doParallel)

## LOOP

CORES <- detectCores() - 1
print(paste("CORES:", CORES))

registerDoParallel(cores = CORES)
CL <- makeCluster(CORES)

system.time({
	RESULT <- foreach(I = 1:FOLD,
										.combine = rbind,
										.packages = "randomForest") %dopar%
		FNC_GET_PERFORMANCE(BANK_DF, BANK_FORM, I)
})
mean(RESULT)

stopCluster(CL)

# NTB
# %do%: 27.92
#   5 :  8.23
#   7 :  7.95
# (10):  6.97

# SRV
# %do%: 50.764
#   5 : 12.196
#  10 :  5.922
#  20 :  5.721
#  30 :  6.105
#  87 :  6.007
