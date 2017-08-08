
WORLD <- read.csv('WORLD.CSV')
str(WORLD)

with(WORLD, table(RELIGION, RELIGION_S))
COL_RELIGION <- c('COUNTRY', 'RELIGION', 'RELIGION_S')

subset(WORLD, RELIGION_S == 'Catholic', COL_RELIGION)
subset(WORLD, RELIGION == 3, COL_RELIGION)
subset(WORLD, RELIGION == 8, COL_RELIGION)
# 3 --> african & south american countries
# 8 --> european & north american countries

subset(WORLD, RELIGION_S == 'Protestant', COL_RELIGION)
subset(WORLD, RELIGION == 2, COL_RELIGION)
subset(WORLD, RELIGION == 7, COL_RELIGION)
# 2 --> african & south american countries
# 7 --> european & north american countries

with(WORLD, table(GOVERN, GOVERN_S))
COL_GOVERN <- c('COUNTRY', 'GOVERN', 'GOVERN_S')
subset(WORLD, GOVERN_S == 'Military', COL_GOVERN)
subset(WORLD, GOVERN == 5, COL_GOVERN)
subset(WORLD, GOVERN == 6, COL_GOVERN)
