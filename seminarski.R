library(tidyverse)
library(ggplot2)
library(reshape2)
library(dplyr)
library(readr)
library(scales)

data.train = read.csv("./data/train.csv")
View(data.train)

data.test = read.csv("./data/test.csv")
summary(data.test)

summary(data.train)

names(data.train)
str(data.train)

# plotovanje svih numerickih karakteristika

ggplot(data.train, aes(x = MSSubClass, y = SalePrice)) + geom_point()

ggplot(data.train, aes(x = LotFrontage, y = SalePrice)) + geom_point()
data.train %>% filter(LotFrontage > 300) %>% select(Id)
# 935, 1299

ggplot(data.train, aes(x = LotArea, y = SalePrice)) + geom_point()
data.train %>% filter(LotArea > 55000) %>% select(Id)
# 250, 314, 336, 707, 1299, 1397

ggplot(data.train, aes(x = OverallQual, y = SalePrice)) + geom_point()
data.train %>% filter(OverallQual == 10 & SalePrice < 200000) %>% select(Id)
# 524, 1299

ggplot(data.train, aes(x = OverallCond, y = SalePrice)) + geom_point()
data.train %>% filter(OverallCond == 6 & SalePrice > 600000) %>% select(Id)
# 692
data.train %>% filter(OverallCond == 2 & SalePrice > 300000) %>% select(Id)
# 379
data.train %>% filter(OverallCond == 5 & SalePrice > 700000) %>% select(Id)
# 1183

ggplot(data.train, aes(x = YearBuilt, y = SalePrice)) + geom_point()
data.train %>% filter(YearBuilt < 1900 & SalePrice > 400000) %>% select(Id)
# 186

ggplot(data.train, aes(x = YearRemodAdd, y = SalePrice)) + geom_point()
data.train %>% filter(YearRemodAdd < 1970 & SalePrice > 300000) %>% select(Id)
# 314

ggplot(data.train, aes(x = MasVnrArea, y = SalePrice)) + geom_point()
data.train %>% filter(MasVnrArea > 1500) %>% select(Id)
# 298

ggplot(data.train, aes(x = BsmtFinSF1, y = SalePrice)) + geom_point()
data.train %>% filter(BsmtFinSF1 > 4000) %>% select(Id)
# 1299

ggplot(data.train, aes(x = BsmtFinSF2, y = SalePrice)) + geom_point()
data.train %>% filter(BsmtFinSF2 > 500 & SalePrice > 500000) %>% select(Id)
# 441

ggplot(data.train, aes(x = BsmtUnfSF, y = SalePrice)) + geom_point()

ggplot(data.train, aes(x = TotalBsmtSF, y = SalePrice)) + geom_point()
data.train %>% filter(TotalBsmtSF > 6000) %>% select(Id)
# 1299

ggplot(data.train, aes(x = X1stFlrSF, y = SalePrice)) + geom_point()
data.train %>% filter(X1stFlrSF > 4000) %>% select(Id)
# 1299

ggplot(data.train, aes(x = X2ndFlrSF, y = SalePrice)) + geom_point()

ggplot(data.train, aes(x = LowQualFinSF, y = SalePrice)) + geom_point()
data.train %>% filter(LowQualFinSF > 550) %>% select(Id)
# 186

ggplot(data.train, aes(x = GrLivArea, y = SalePrice)) + geom_point()
data.train %>% filter(GrLivArea > 4500 & SalePrice < 200000) %>% select(Id)
# 524, 1299

ggplot(data.train, aes(x = BsmtFullBath, y = SalePrice)) + geom_point()
data.train %>% filter(BsmtFullBath == 3) %>% select(Id)
# 739

ggplot(data.train, aes(x = BsmtHalfBath, y = SalePrice)) + geom_point()
data.train %>% filter(BsmtHalfBath == 2) %>% select(Id)
# 598, 955
data.train %>% filter(BsmtHalfBath == 1 & SalePrice > 700000) %>% select(Id)
# 692

ggplot(data.train, aes(x = FullBath, y = SalePrice)) + geom_point()

ggplot(data.train, aes(x = HalfBath, y = SalePrice)) + geom_point()

ggplot(data.train, aes(x = BedroomAbvGr, y = SalePrice)) + geom_point()
data.train %>% filter(BedroomAbvGr == 8) %>% select(Id)
# 636

ggplot(data.train, aes(x = KitchenAbvGr, y = SalePrice)) + geom_point()
data.train %>% filter(KitchenAbvGr == 3) %>% select(Id)
# 49, 810
data.train %>% filter(KitchenAbvGr == 0) %>% select(Id)
# 955

ggplot(data.train, aes(x = TotRmsAbvGrd, y = SalePrice)) + geom_point()
data.train %>% filter(TotRmsAbvGrd == 14) %>% select(Id)
# 636

ggplot(data.train, aes(x = Fireplaces, y = SalePrice)) + geom_point()

ggplot(data.train, aes(x = GarageYrBlt, y = SalePrice)) + geom_point()

ggplot(data.train, aes(x = GarageCars, y = SalePrice)) + geom_point()

ggplot(data.train, aes(x = GarageArea, y = SalePrice)) + geom_point()
data.train %>% filter(GarageArea > 1200 & SalePrice < 300000) %>% select(Id)
# 1062, 1191

ggplot(data.train, aes(x = WoodDeckSF, y = SalePrice)) + geom_point()

ggplot(data.train, aes(x = OpenPorchSF, y = SalePrice)) + geom_point()
data.train %>% filter(OpenPorchSF > 500 & SalePrice < 100000) %>% select(Id)
# 496

ggplot(data.train, aes(x = EnclosedPorch, y = SalePrice)) + geom_point()
data.train %>% filter(EnclosedPorch > 500) %>% select(Id)
# 198

ggplot(data.train, aes(x = X3SsnPorch, y = SalePrice)) + geom_point()

ggplot(data.train, aes(x = ScreenPorch, y = SalePrice)) + geom_point()

ggplot(data.train, aes(x = PoolArea, y = SalePrice)) + geom_point()

values = c(935, 1299, 250, 314, 336, 707, 1397, 524, 692, 379, 1183, 
           186, 739, 598, 955, 636, 49, 810, 1062, 1191, 496, 198)

data.train <- subset(data.train, !(Id %in% values))

#missing_percent <- colSums(is.na(data)) / nrow(data) * 100
#missing_percent[missing_percent > 0]

# ----------------------------------------------------
# NA VREDNOSTI

data.test$SalePrice <- NA

data <- rbind(data.train, data.test)
dim(data.train)
dim(data.test)

na_columns = sort(colSums(is.na(data))[colSums(is.na(data)) > 0])

na_columns
# Electrical - elektricni sistem
# ima jednu NA
# chr je, a treba da se pretvori u kat. TODO
# posto nedostaje samo jedna vr, mozemo uzeti onu koja se najcesce javlja

unique(data$Electrical)
# View(data[is.na(data$Electrical),])

# najcesce ponavljanja vrednost
tbl <- xtabs(~Electrical, data = data)
# najcesca vrednost je "SBrkr" - standardni prekidacii i Romex (tip kabla)
most_common <- names(which.max(tbl))
data$Electrical[is.na(data$Electrical)] <- most_common

View(data)

# MasVnrType - tip malterisanja
# takodje chr -> kat.
# takodje ima malo NA 23, pa se moze iskoristiti najcesca vrednost
str(data$MasVnrType)

View(data %>%
       filter(is.na(MasVnrType) & !is.na(MasVnrArea)))
# Postoji jedna vrednost gde je MasVnrType NA, a MasVnrArea nije 0, sto ukazuje da je tu doslo do greske
# uzimamo najcesce vrednosti gde area nije 0
tbl <- xtabs(~MasVnrType, data = data[data$MasVnrArea != 0, ])
most_common <- names(which.max(tbl))
data$MasVnrType[is.na(data$MasVnrType) & data$MasVnrArea != 0] <- most_common


tbl <- xtabs(~MasVnrType, data = data)
# za ceo skup najcesca vrednost je None
most_common <- names(which.max(tbl))
data$MasVnrType[is.na(data$MasVnrType)] <- most_common

# MasVnrArea - povrsina zida koji je pokriven dekorativnim materijalom
# numericka
str(data$MasVnrArea)

shapiro.test(data$MasVnrArea)
# posto test ukazuje da raspodela nije normalna, koristimo medijanu
ggplot(data, aes(x = MasVnrArea)) +
  geom_histogram(binwidth = 50, fill = "#1f78b4", color = "black", alpha = 0.7) +
  labs(
    title = "Raspodela MasVnrArea",
    x = "Povrsina",
    y = "Broj kuca"
  ) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  scale_x_continuous(breaks = seq(0, max(data$MasVnrArea, na.rm = TRUE), by = 50))

data_median = median(data$MasVnrArea[!is.na(data$MasVnrArea)])
# medijana je 0
data$MasVnrArea[is.na(data$MasVnrArea)] <- data_median

# BsmtQual - visina podruma 
# kategorijska ORDINALNA - excellent (100+ in), good(90-99 in), ...
# NA znaci da nema podruma, mozemo dodati novu kategoriju "NONE"
# ima 81 NA vrednosti

View(data %>%
       filter(is.na(BsmtCond) & TotalBsmtSF > 0))
# postoje 3 reda gde nema podataka za BsmtCond, a postoje ostali podaci vezani za Bsmt
tbl <- xtabs(~ BsmtCond, data = data[data$TotalBsmtSF > 0 & !is.na(data$BsmtCond), ])
most_common <- names(which.max(tbl))
data$BsmtCond[is.na(data$BsmtCond) & data$TotalBsmtSF > 0] <- most_common


View(data %>%
       filter(is.na(BsmtExposure) & TotalBsmtSF > 0))
# postoje 3 reda gde nema podataka za BsmtExposure, a postoje ostali podaci vezani za Bsmt
tbl <- xtabs(~ BsmtExposure, data = data[data$TotalBsmtSF > 0 & !is.na(data$BsmtExposure), ])
most_common <- names(which.max(tbl))
data$BsmtExposure[is.na(data$BsmtExposure) & data$TotalBsmtSF > 0] <- most_common


View(data %>%
       filter(is.na(BsmtFinType2) & TotalBsmtSF > 0))
# postoje 3 reda gde nema podataka za BsmtFinType2, a postoje ostali podaci vezani za Bsmt
tbl <- xtabs(~ BsmtFinType2, data = data[data$BsmtFinSF2 > 0 & !is.na(data$BsmtFinType2), ])
most_common <- names(which.max(tbl))
data$BsmtFinType2[is.na(data$BsmtFinType2) & data$BsmtFinSF2 > 0] <- most_common

View(data %>%
       filter(is.na(BsmtQual) & TotalBsmtSF > 0))
# postoje 2 reda gde nema podataka za BsmtQual, a postoje ostali podaci vezani za Bsmt
tbl <- xtabs(~ BsmtQual, data = data[data$TotalBsmtSF > 0 & !is.na(data$BsmtQual), ])
most_common <- names(which.max(tbl))
data$BsmtQual[is.na(data$BsmtQual) & data$TotalBsmtSF > 0] <- most_common

# postoji jedan red gde su BsmtFinSF1, BsmtFinSF2, BsmtUnfSF i TotalBsmtSF NA, tu stavljamo 0
data$BsmtFinSF1[is.na(data$BsmtFinSF1)] <- 0
data$BsmtFinSF2[is.na(data$BsmtFinSF2)] <- 0
data$BsmtUnfSF[is.na(data$BsmtUnfSF)] <- 0
data$TotalBsmtSF[is.na(data$TotalBsmtSF)] <- 0

# ---------------------------
View(data %>%
       filter(is.na(BsmtQual) ))

str(data$BsmtQual)

data$BsmtQual[is.na(data$BsmtQual)] <- "NoBasement"

# BsmtCond - stanje podruma
# isto kao i BsmtQual kat. ORDINALNA
# NA znaci da ne postoji podrum 
str(data$BsmtCond)

data$BsmtCond[is.na(data$BsmtCond)] <- "NoBasement"


# BsmtFinType1 - kvalitet zavrsene povrsine podruma TODO PROVERI (unf ili no)
# kategorijska ORDINALNA
# isto kao prethodne 
# NA znaci da ne postoji podrum
str(data$BsmtFinType1)

data$BsmtFinType1[is.na(data$BsmtFinType1)] <- "NoBasement"

# Bsmt FinType2 - kvaliltet druge zavrsene povrsine podruma (ako postoji)
# kategorijska ORDINALNA
# isto kao prethodna 
# NA znaci da ne postoji podrum
str(data$BsmtFinType2)

data$BsmtFinType2[is.na(data$BsmtFinType2)] <- "NoBasement"

# BsmtExposure - da li podrum ima prozore ili vrata koja izlaze u dvoriste
# kategorijska ORDINALNA
# isto kao prethodna 
# NA znaci da ne postoji podrum

str(data$BsmtExposure)
data$BsmtExposure[is.na(data$BsmtExposure)] <- "NoBasement"

# GarageType, GarageYrBlt, GarageFinish, GarageQual, GarageCond
# kategorijska ORDINALNA
# NA znaci da ne postoji garaza

View(data %>%
       filter(is.na(GarageYrBlt)))
# postoji red gde su prisutni podaci za GarageType, GarageCars i GarageArea, a svi ostali su NA
tbl <- xtabs(~ GarageFinish, data = data[!is.na(data$GarageFinish), ])
most_common_finish <- names(which.max(tbl))
data$GarageFinish[is.na(data$GarageFinish) & data$GarageArea > 0] <- most_common_finish

tbl <- xtabs(~ GarageQual, data = data[!is.na(data$GarageQual), ])
most_common_qual <- names(which.max(tbl))
data$GarageQual[is.na(data$GarageQual) & data$GarageArea > 0] <- most_common_qual

tbl <- xtabs(~ GarageCond, data = data[!is.na(data$GarageCond), ])
most_common_cond <- names(which.max(tbl))
data$GarageCond[is.na(data$GarageCond) & data$GarageArea > 0] <- most_common_cond

most_common_yr <- round(median(data$GarageYrBlt, na.rm = TRUE))
data$GarageYrBlt[is.na(data$GarageYrBlt) & data$GarageArea > 0] <- most_common_yr

# -----------------------------------

View(data %>%
       filter(is.na(GarageCars)))
View(data %>%
       filter(GarageType=="Detchd"))
# postoji jedan red gde je GarageType Detachd, a sve ostalo NA
median_area <- round(median(data$GarageArea[data$GarageType == "Detchd"], na.rm = TRUE))
data$GarageArea[is.na(data$GarageCars)] <- median_area

median_yr <- round(median(data$GarageYrBlt, na.rm = TRUE))
data$GarageYrBlt[is.na(data$GarageCars)] <- median_yr

most_common_finish <- names(which.max(table(data$GarageFinish)))
data$GarageFinish[is.na(data$GarageCars)] <- most_common_finish

most_common_qual <- names(which.max(table(data$GarageQual)))
data$GarageQual[is.na(data$GarageCars)] <- most_common_qual

most_common_cond <- names(which.max(table(data$GarageCond)))
data$GarageCond[is.na(data$GarageCars)] <- most_common_cond
data$GarageCars[is.na(data$GarageCars)] <- 1


View(data[which(is.na(data$GarageType)), ])

data$GarageType[is.na(data$GarageType)] <- "NoGarage"
data$GarageYrBlt[is.na(data$GarageYrBlt)] <- "NoGarage"
data$GarageFinish[is.na(data$GarageFinish)] <- "NoGarage"
data$GarageQual[is.na(data$GarageQual)] <- "NoGarage"
data$GarageCond[is.na(data$GarageCond)] <- "NoGarage"

# LotFrontage - duzina placa koja se granici sa ulicom 
# numericka
# 481 NA
# Impute by grouping by Neighborhood and taking the median LotFrontage for that group. 
# This is much better than using the overall median.

# uradi NA kao da je 0 (ne granici se sa ulicom)?
str(data$LotFrontage)
View(data[which(is.na(data$LotFrontage)), ])

# popunjavamo vrednostima medijane za neighbourhood kom pripada


data <- data %>%
  group_by(Neighborhood) %>%
  mutate(LotFrontage = ifelse(is.na(LotFrontage),
                              median(LotFrontage, na.rm = TRUE),
                              LotFrontage))

data$LotFrontage <- as.integer(data$LotFrontage)
typeof(data$LotFrontage)

# FireplaceQu - kvalitet kamina
# kat. Ordinalna 
# NA - nema kamina

data$FireplaceQu[is.na(data$FireplaceQu)] <- "NoFireplace"

# Fence - kvalitet ograde
# kat. nominalna
# NA - nema ograde

data$Fence[is.na(data$Fence)] <- "NoFence"

# Alley - zadnji prilaz sa puta
# kat. nominalna
# NA - nema prilaz

data$Alley[is.na(data$Alley)] <- "NoAccess"

# MiscFeature - dodatni featuri
# kat. nominalna
# NA - nema dodatnih featura

# MiscFeature nema poente posto postoji MiscVal
# da li dropovati
# MiscVal uvek tačno odražava postojanje/odsustvo MiscFeature
View(data[is.na(data$MiscFeature), ])

data$MiscFeature <- NULL

# PoolQC - kvalitet bazena
# kat. Ordinalna 
# NA - nema bazena
# postoji i poolArea; mozda dropovati PoolQC?

data$PoolQC[is.na(data$PoolQC)] <- "NoPool"

ggplot(data, aes(x = PoolQC, y = PoolArea)) +
  geom_boxplot(fill = "#1f78b4", alpha = 0.7) +
  labs(
    title = "Povezanost između kvaliteta bazena (PoolQC) i površine bazena (PoolArea)",
    x = "Kvalitet bazena",
    y = "Površina bazena (sq ft)"
  ) +
  theme_minimal()

View(data)

# ------------------------------------- dodatak iz test skupa
sort(colSums(is.na(data))[colSums(is.na(data)) > 0])

View(data %>%
       filter(is.na(Exterior1st)))

tbl <- xtabs(~Exterior1st, data = data)
most_common <- names(which.max(tbl))
data$Exterior1st[is.na(data$Exterior1st)] <- most_common

tbl <- xtabs(~Exterior2nd, data = data)
most_common <- names(which.max(tbl))
data$Exterior2nd[is.na(data$Exterior2nd)] <- most_common

View(data %>%
       filter(is.na(KitchenQual)))

tbl <- xtabs(~KitchenQual, data = data)
most_common <- names(which.max(tbl))
data$KitchenQual[is.na(data$KitchenQual)] <- most_common

View(data %>%
       filter(is.na(SaleType)))

tbl <- xtabs(~SaleType, data = data)
most_common <- names(which.max(tbl))
data$SaleType[is.na(data$SaleType)] <- most_common

View(data %>%
       filter(is.na(Utilities)))

tbl <- xtabs(~Utilities, data = data)
most_common <- names(which.max(tbl))
data$Utilities[is.na(data$Utilities)] <- most_common

View(data)

# 2 NA za BsmtFullBath i BsmtHalfBath su za redove gde ne postoji Basement
# tu cemo staviti 0
View(data %>%
       filter(is.na(BsmtFullBath)))

data$BsmtFullBath[is.na(data$BsmtFullBath)] <- 0
data$BsmtHalfBath[is.na(data$BsmtHalfBath)] <- 0

View(data %>%
       filter(is.na(Functional)))
tbl <- xtabs(~Functional, data = data)
most_common <- names(which.max(tbl))
data$Functional[is.na(data$Functional)] <- most_common

View(data %>%
       filter(is.na(MSZoning)))
tbl <- xtabs(~MSZoning, data = data)
most_common <- names(which.max(tbl))
data$MSZoning[is.na(data$MSZoning)] <- most_common

sort(colSums(is.na(data))[colSums(is.na(data)) > 0])

# -----------------------------------------------------

data.train <- data[1:nrow(data.train), ]
data.test  <- data[(nrow(data.train)+1):nrow(data), ] 

data.test$SalePrice <- NULL

summary(data.train)
# -----------------------------------------------------

# odabir znacajnih atributa za EDA

# 1. koristimo domensko znanje da pretpostavimo koji atributi bi mogli da budu bitni
# veca kuca kosta vise - GrLivArea, TotalBsmtSF, LotArea
# novija kuca kosta vise - YearBuilt, YearRemodAdd
# kvalitet utice na cenu - OverallQual, OverallCond, KitchenQual
# lokacija je bitna - Neighborhood
# pogodnosti uticu na cenu - GarageCars, Fireplaces, FullBath

ggplot(data.train, aes(x = GrLivArea, y = SalePrice)) +
  geom_point(alpha = 0.7) +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Odnos GrLivArea i SalePrice",
    x = "GrLivArea",
    y = "SalePrice"
  ) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggplot(data.train, aes(x = TotalBsmtSF, y = SalePrice)) +
  geom_point(alpha = 0.7) +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Odnos TotalBsmtSF i SalePrice",
    x = "TotalBsmtSF",
    y = "SalePrice"
  ) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggplot(data.train, aes(x = LotArea, y = SalePrice)) +
  geom_point(alpha = 0.7) +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Odnos LotArea i SalePrice",
    x = "LotArea",
    y = "SalePrice"
  ) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

###

ggplot(data.train, aes(x = YearBuilt, y = SalePrice)) +
  geom_point(alpha = 0.7) +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Odnos YearBuilt i SalePrice",
    x = "YearBuilt",
    y = "SalePrice"
  ) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggplot(data.train, aes(x = YearRemodAdd, y = SalePrice)) +
  geom_point(alpha = 0.7) +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Odnos YearRemodAdd i SalePrice",
    x = "YearRemodAdd",
    y = "SalePrice"
  ) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

###

ggplot(data.train, aes(x = as.factor(OverallQual), y = SalePrice)) +
  geom_boxplot() +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Odnos OverallQual i SalePrice",
    x = "OverallQual",
    y = "SalePrice"
  ) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggplot(data.train, aes(x = as.factor(OverallCond), y = SalePrice)) +
  geom_boxplot() +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Odnos OverallCond i SalePrice",
    x = "OverallCond",
    y = "SalePrice"
  ) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggplot(data.train, aes(x = as.factor(KitchenQual), y = SalePrice)) +
  geom_boxplot() +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Odnos KitchenQual i SalePrice",
    x = "KitchenQual",
    y = "SalePrice"
  ) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

###

ggplot(data.train, aes(x = as.factor(Neighborhood), y = SalePrice)) +
  geom_boxplot() +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Odnos Neighborhood i SalePrice",
    x = "Neighborhood",
    y = "SalePrice"
  ) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

###

ggplot(data.train, aes(x = as.factor(GarageCars), y = SalePrice)) +
  geom_boxplot() +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Odnos GarageCars i SalePrice",
    x = "GarageCars",
    y = "SalePrice"
  ) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggplot(data.train, aes(x = as.factor(Fireplaces), y = SalePrice)) +
  geom_boxplot() +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Odnos Fireplaces i SalePrice",
    x = "Fireplaces",
    y = "SalePrice"
  ) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggplot(data.train, aes(x = as.factor(FullBath), y = SalePrice)) +
  geom_boxplot() +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Odnos FullBath i SalePrice",
    x = "FullBath",
    y = "SalePrice"
  ) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# 2. koristimo koeficijente korelacije za numericke atribute
# cor() funkcija
# OverallQual → 0.79
# GrLivArea → 0.71
# GarageCars → 0.64
# GarageArea → 0.62
# TotalBsmtSF → 0.61
# 1stFlrSF → 0.60
# FullBath → 0.56

numeric_data.train <- data.train[sapply(data.train, is.numeric)]
cor_matrix <- cor(numeric_data.train, use = "pairwise.complete.obs")
cor_with_saleprice <- cor_matrix[, "SalePrice"]
cor_with_saleprice <- sort(cor_with_saleprice, decreasing = TRUE)
print(cor_with_saleprice)

# Convert to data.trainframe for ggplot
cor_df <- data.frame(
  Variable = names(cor_with_saleprice),
  Correlation = cor_with_saleprice
)

# Remove SalePrice itself
cor_df <- cor_df[cor_df$Variable != "SalePrice", ]

# Plot
ggplot(cor_df, aes(x = reorder(Variable, Correlation), y = Correlation)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Correlation of Numeric Features with SalePrice",
       x = "Variable", y = "Correlation Coefficient")

ggplot(data.train, aes(x = X1stFlrSF, y = SalePrice)) +
  geom_point(alpha = 0.7) +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Odnos X1stFlrSF i SalePrice",
    x = "X1stFlrSF",
    y = "SalePrice"
  ) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggplot(data.train, aes(x = as.factor(TotRmsAbvGrd), y = SalePrice)) +
  geom_boxplot() +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Odnos TotRmsAbvGrd i SalePrice",
    x = "TotRmsAbvGrd",
    y = "SalePrice"
  ) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggplot(data.train, aes(x = LotFrontage, y = SalePrice)) +
  geom_point(alpha = 0.7) +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Odnos LotFrontage i SalePrice",
    x = "LotFrontage",
    y = "SalePrice"
  ) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# 3. koristimo boxplotove i violin plotove za kategorijske atribute
# Neighborhood, HouseStyle, OverallQual
# za ordinalne atribute moze se koristiti Spearman's rank correlation
# inace Pearson

ggplot(data.train, aes(x = as.factor(Neighborhood), y = SalePrice)) +
  geom_boxplot() +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Odnos Neighborhood i SalePrice",
    x = "Neighborhood",
    y = "SalePrice"
  ) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggplot(data.train, aes(x = as.factor(HouseStyle), y = SalePrice)) +
  geom_boxplot() +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Odnos HouseStyle i SalePrice",
    x = "HouseStyle",
    y = "SalePrice"
  ) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggplot(data.train, aes(x = as.factor(OverallQual), y = SalePrice)) +
  geom_boxplot() +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Odnos OverallQual i SalePrice",
    x = "OverallQual",
    y = "SalePrice"
  ) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# 4. proveravamo multikolinearnost
# GarageArea i GarageCars
# GrLivArea i 1stFlrSF
# TotalBsmtSF i BsmtFinSF1
# proveriti pomocu korelacione matrice ili Variance Inflation Factor (VIF)
# zadrzati samo jednu reprezentativnu promenljivu

# Select numeric columns
numeric_vars <- data.train[sapply(data.train, is.numeric)]

# Compute correlation matrix
corr_matrix <- cor(numeric_vars, use = "pairwise.complete.obs", method = "pearson")

# Set the correlation threshold
threshold <- 0.8

# Get the upper triangle of the matrix to avoid duplicates
corr_matrix[lower.tri(corr_matrix, diag = TRUE)] <- NA

# Convert to a long-format table
corr_table <- as.data.frame(as.table(corr_matrix))

# Filter by absolute correlation greater than threshold
high_corr <- subset(corr_table, !is.na(Freq) & abs(Freq) > threshold)

# Sort descending by correlation
high_corr <- high_corr[order(-abs(high_corr$Freq)), ]

# Round for readability
high_corr$Freq <- round(high_corr$Freq, 3)

# View results
high_corr

# GarageCars   GarageArea 0.886
# GrLivArea TotRmsAbvGrd 0.833
# TotalBsmtSF    X1stFlrSF 0.804
# OverallQual    SalePrice 0.801

# dropujemo jednu, zadrzavamo jednu (ako ih uopste koristimo za modele)

# -----------------------------------------------------

na_columns

# GarageYrBlt - Fill with 0 -1 or YearBuilt ?

# pretvoriti sve chr u kategorijske TODO


# uticajne kolone:
# overallQual, TotalBsmtSF, X1stFlrSF, GrLivArea, FullBath, TotRmsAbvGrd, 
# GarageCars i GarageArea (koje su zajedno u korelaciji pa cemo da izbacimo jednu od te dve)



