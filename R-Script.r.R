
# BACO model analysis

#____________________________________________________________________________________
#
# PACKAGES 
#____________________________________________________________________________________
#

library (Rcmdr)
library(randomForest)
library(corrplot)
library (ggplot2)

#____________________________________________________________________________________
#
# DATA
#____________________________________________________________________________________
#

setwd("C:/Users/usuario/Desktop/Pen/Greece/BACO-ANALISIS") ### Use the directory where you saved the downloaded files/data (all files should be placed in the same directory)

Dataset <- read.table("BACO-Data.txt", 
  header=TRUE, sep="\t", na.strings="NA", dec=",", strip.white=TRUE) ### This data file contains the evolution of all variables after 30 iterations. All parameters are
### set with constant default values.

Dataset <- within(Dataset, {
  Experiment <- as.factor(Experiment)
})

Average <- read.table("AVERAGE.txt", 
  header=TRUE, sep="\t", na.strings="NA", dec=",", strip.white=TRUE) ### This data file contains the mean value of each variable after 30 interations. This file is used for
### visual representation purposes and it was obtained from "BACO-Data.txt."

#____________________________________________________________________________________
#
# RESULTS
#____________________________________________________________________________________
#

#____________________________________________________________________________________
###
### PLOTS 
###__________________________________________________________________________________
#

# POPULATION SIZE (OIKOI)

ggplot() + 
 geom_line(data = Dataset, aes(x = rev(Dataset$Calendar), y = Dataset$oikoi, z = Dataset$Experiment), color = "blue") +
  geom_line(data = Average, aes(x = rev(Average$Calendar), y = Average$avoikoi), color = "red", size = 1.5) +
  xlab('Calendar') +
  ylab('Oikoi (count)') + 
  theme_minimal(base_size = 14, base_family = "sans")

# SETTLEMENTS
 
ggplot() + 
 geom_line(data = Dataset, aes(x = rev(Dataset$Calendar), y = Dataset$settlement, z = Dataset$Experiment), color = "blue") +
  geom_line(data = Average, aes(x = rev(Average$Calendar), y = Average$avsettlements), color = "red", size = 1.5) +
  xlab('Calendar') +
  ylab('Total settlements (count)') + 
  theme_minimal(base_size = 14, base_family = "sans")

# ISLAND-SETTLEMENTS

ggplot() + 
 geom_line(data = Dataset, aes(x = rev(Dataset$Calendar), y = Dataset$INLAND, z = Dataset$Experiment), color = "blue") +
  geom_line(data = Average, aes(x = rev(Average$Calendar), y = Average$avinlandsettl), color = "red", size = 1.5) +
  xlab('Calendar') +
  ylab('Inland settlements (count)') + 
  theme_minimal(base_size = 14, base_family = "sans")

# COAST-SETTLEMENTS
ggplot() + 
 geom_line(data = Dataset, aes(x = rev(Dataset$Calendar), y = Dataset$COAST, z = Dataset$Experiment), color = "blue") +
  geom_line(data = Average, aes(x = rev(Average$Calendar), y = Average$avcoastsettl), color = "red", size = 1.5) +
  xlab('Calendar') +
  ylab('Coastal settlements (count)') + 
  theme_minimal(base_size = 14, base_family = "sans")

# SETTLEMENTS' ELEVATION
ggplot() + 
 geom_line(data = Dataset, aes(x = rev(Dataset$Calendar), y = Dataset$Elevation, z = Dataset$Experiment), color = "blue") +
  geom_line(data = Average, aes(x = rev(Average$Calendar), y = Average$avelevation), color = "red", size = 1.5) +
  xlab('Calendar') +
  ylab('Settlements elevation (m)') + 
  theme_minimal(base_size = 14, base_family = "sans")

# COMMERCIAL EXCHANGES
ggplot() + 
 geom_line(data = Dataset, aes(x = rev(Dataset$Calendar), y = Dataset$tradeflows, z = Dataset$Experiment), color = "blue") +
  geom_line(data = Average, aes(x = rev(Average$Calendar), y = Average$avtradeflow), color = "red", size = 1.5) +
  xlab('Calendar') +
  ylab('Commercial exchanges (count)') + 
  theme_minimal(base_size = 14, base_family = "sans")

# TRADE DISTANCE
ggplot() + 
 geom_line(data = Dataset, aes(x = rev(Dataset$Calendar), y = Dataset$tradedistance, z = Dataset$Experiment), color = "blue") +
  geom_line(data = Average, aes(x = rev(Average$Calendar), y = Average$avtradedistance), color = "red", size = 1.5) +
  xlab('Calendar') +
  ylab('Exchanges distance (km)') + 
  theme_minimal(base_size = 14, base_family = "sans")

# MIGRATIONS
ggplot() + 
 geom_line(data = Dataset, aes(x = rev(Dataset$Calendar), y = Dataset$migrations, z = Dataset$Experiment), color = "blue") +
  geom_line(data = Average, aes(x = rev(Average$Calendar), y = Average$avmigrations), color = "red", size = 1.5) +
  xlab('Calendar') +
  ylab('Migrations (count)') + 
  theme_minimal(base_size = 14, base_family = "sans")

# AREA CULTIVATED
ggplot() + 
 geom_line(data = Dataset, aes(x = rev(Dataset$Calendar), y = Dataset$ecultivated, z = Dataset$Experiment), color = "blue") +
  geom_line(data = Average, aes(x = rev(Average$Calendar), y = Average$avecultivated), color = "red", size = 1.5) +
  xlab('Calendar') +
  ylab('Area cultivated (ha)') + 
  theme_minimal(base_size = 14, base_family = "sans")

# RAI
ggplot() + 
  geom_line(data = Average, aes(x = rev(Average$Calendar), y = Average$RAI), color = "red", size = 1.5) +
  xlab('Calendar') +
  ylab('Area cultivated (ha)') + 
  theme_minimal(base_size = 14, base_family = "sans")

#____________________________________________________________________________________
###
### RANDOM FOREST TEST
###__________________________________________________________________________________
#

#____________________________________________________________________________________
#
# DATA
#____________________________________________________________________________________
#

Dataset <- read.table("ALL-PARAMETER-COMBINATIONS.txt", 
  header=TRUE, sep="\t", na.strings="NA", dec=",", strip.white=TRUE) ### This data file contains the simulation outputs after all the combinations of parameters explored (n= 1296 combinations)
### For each combination of variables, 30 simulations were used.

#____________________________________________________________________________________
#
# ANALYSES
#____________________________________________________________________________________
#

set.seed(101)
size.total <- nrow(Dataset)
size.train <- round(size.total*0.7)
data.ind <- sample(1:size.total , size=size.train)
size.train <- Dataset[data.ind,]
data.test <- Dataset[-data.ind,]


OIKOI <- randomForest(oikoi~ MaritimeFoodResources + OtherCalorieSources + RainfallVariability + RecoverRate + DegradationRate + DegradationFactor 
 + SeaPeople + MaxDebt + StorageLimit + FAF, data=size.train
, importance = T, keep.forest = TRUE)

summary(OIKOI)
OIKOI$importance

SETTLEMENTS <- randomForest(settlements~ MaritimeFoodResources + OtherCalorieSources + RainfallVariability + RecoverRate + DegradationRate + DegradationFactor 
 + SeaPeople + MaxDebt + StorageLimit + FAF, data=size.train
, importance = T, keep.forest = TRUE)

summary(SETTLEMENTS)
SETTLEMENTS$importance

AREA-CULTIVATED <- randomForest(extension~ MaritimeFoodResources + OtherCalorieSources + RainfallVariability + RecoverRate + DegradationRate + DegradationFactor 
 + SeaPeople + MaxDebt + StorageLimit + FAF, data=size.train
, importance = T, keep.forest = TRUE)

summary(AREA-CULTIVATED)
AREA-CULTIVATED$importance

MIGRATIONS <- randomForest(migrations~ MaritimeFoodResources + OtherCalorieSources + RainfallVariability + RecoverRate + DegradationRate + DegradationFactor 
 + SeaPeople + MaxDebt + StorageLimit + FAF, data=size.train
, importance = T, keep.forest = TRUE)

summary(MIGRATIONS)
MIGRATIONS$importance

TRADEFLOWS <- randomForest(commercialexchanges~ MaritimeFoodResources + OtherCalorieSources + RainfallVariability + RecoverRate + DegradationRate + DegradationFactor 
 + SeaPeople + MaxDebt + StorageLimit + FAF, data=size.train
, importance = T, keep.forest = TRUE)

summary(TRADEFLOWS)
TRADEFLOWS$importance

TRADEDISTANCE <- randomForest(distance~ MaritimeFoodResources + OtherCalorieSources + RainfallVariability + RecoverRate + DegradationRate + DegradationFactor 
 + SeaPeople + MaxDebt + StorageLimit + FAF, data=size.train
, importance = T, keep.forest = TRUE)

summary(TRADEDISTANCE)
TRADEDISTANCE$importance

#____________________________________________________________________________________
###
### CORRELATION PLOT
###__________________________________________________________________________________
#

#____________________________________________________________________________________
#
# DATA
#____________________________________________________________________________________
#

Dataset <- read.table("Phase1.txt",
  header=TRUE, sep="\t", na.strings="NA", dec=",", strip.white=TRUE) ### To study correlation between variables and parameters, the "ALL-PARAMETER-COMBINATIONS.txt" file was
### broken up into three chronological phases. Phase 1 = 1350-1220 B.C.

# ANALYSIS

M <- cor(Dataset, method = "spearman", use="pairwise.complete.obs")

corrplot(M, method = "pie", type = "upper")

#____________________________________________________________________________________
#
# DATA
#____________________________________________________________________________________
#

Dataset <- read.table("Phase2.txt",
header=TRUE, sep="\t", na.strings="NA", dec=",", strip.white=TRUE) ### Phase 2 = 1220-1150 B.C.

# ANALYSIS

M <- cor(Dataset, method = "spearman", use="pairwise.complete.obs")

corrplot(M, method = "pie", type = "upper")

#____________________________________________________________________________________
#
# DATA
#____________________________________________________________________________________
#

Dataset <- read.table("Phase3.txt",
header=TRUE, sep="\t", na.strings="NA", dec=",", strip.white=TRUE) ### Phase 3 = 1010-980 B.C.

# ANALYSIS

M <- cor(Dataset, method = "spearman", use="pairwise.complete.obs")

corrplot(M, method = "pie", type = "upper")
