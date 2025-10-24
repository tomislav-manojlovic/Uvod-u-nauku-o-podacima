library(tidyverse)

data = read.csv("./data/train.csv")
View(data)

names(data)
str(data)

# plotovanje svih numerickih karakteristika

ggplot(data, aes(x = MSSubClass, y = SalePrice)) + geom_point()

ggplot(data, aes(x = LotFrontage, y = SalePrice)) + geom_point()
data %>% filter(LotFrontage > 300) %>% select(Id)
# 935, 1299

ggplot(data, aes(x = LotArea, y = SalePrice)) + geom_point()
data %>% filter(LotArea > 55000) %>% select(Id)
# 250, 314, 336, 707, 1299, 1397

ggplot(data, aes(x = OverallQual, y = SalePrice)) + geom_point()
data %>% filter(OverallQual == 10 & SalePrice < 200000) %>% select(Id)
# 524, 1299

ggplot(data, aes(x = OverallCond, y = SalePrice)) + geom_point()
data %>% filter(OverallCond == 6 & SalePrice > 600000) %>% select(Id)
# 692
data %>% filter(OverallCond == 2 & SalePrice > 300000) %>% select(Id)
# 379
data %>% filter(OverallCond == 5 & SalePrice > 700000) %>% select(Id)
# 1183

ggplot(data, aes(x = YearBuilt, y = SalePrice)) + geom_point()
data %>% filter(YearBuilt < 1900 & SalePrice > 400000) %>% select(Id)
# 186

ggplot(data, aes(x = YearRemodAdd, y = SalePrice)) + geom_point()
data %>% filter(YearRemodAdd < 1970 & SalePrice > 300000) %>% select(Id)
# 314

ggplot(data, aes(x = MasVnrArea, y = SalePrice)) + geom_point()
data %>% filter(MasVnrArea > 1500) %>% select(Id)
# 298

ggplot(data, aes(x = BsmtFinSF1, y = SalePrice)) + geom_point()
data %>% filter(BsmtFinSF1 > 4000) %>% select(Id)
# 1299

ggplot(data, aes(x = BsmtFinSF2, y = SalePrice)) + geom_point()
data %>% filter(BsmtFinSF2 > 500 & SalePrice > 500000) %>% select(Id)
# 441

ggplot(data, aes(x = BsmtUnfSF, y = SalePrice)) + geom_point()

ggplot(data, aes(x = TotalBsmtSF, y = SalePrice)) + geom_point()
data %>% filter(TotalBsmtSF > 6000) %>% select(Id)
# 1299

ggplot(data, aes(x = X1stFlrSF, y = SalePrice)) + geom_point()
data %>% filter(X1stFlrSF > 4000) %>% select(Id)
# 1299

ggplot(data, aes(x = X2ndFlrSF, y = SalePrice)) + geom_point()

ggplot(data, aes(x = LowQualFinSF, y = SalePrice)) + geom_point()
data %>% filter(LowQualFinSF > 550) %>% select(Id)
# 186

ggplot(data, aes(x = GrLivArea, y = SalePrice)) + geom_point()
data %>% filter(GrLivArea > 4500 & SalePrice < 200000) %>% select(Id)
# 524, 1299

ggplot(data, aes(x = BsmtFullBath, y = SalePrice)) + geom_point()
data %>% filter(BsmtFullBath == 3) %>% select(Id)
# 739

ggplot(data, aes(x = BsmtHalfBath, y = SalePrice)) + geom_point()
data %>% filter(BsmtHalfBath == 2) %>% select(Id)
# 598, 955
data %>% filter(BsmtHalfBath == 1 & SalePrice > 700000) %>% select(Id)
# 692

ggplot(data, aes(x = FullBath, y = SalePrice)) + geom_point()

ggplot(data, aes(x = HalfBath, y = SalePrice)) + geom_point()

ggplot(data, aes(x = BedroomAbvGr, y = SalePrice)) + geom_point()
data %>% filter(BedroomAbvGr == 8) %>% select(Id)
# 636

ggplot(data, aes(x = KitchenAbvGr, y = SalePrice)) + geom_point()
data %>% filter(KitchenAbvGr == 3) %>% select(Id)
# 49, 810
data %>% filter(KitchenAbvGr == 0) %>% select(Id)
# 955

ggplot(data, aes(x = TotRmsAbvGrd, y = SalePrice)) + geom_point()
data %>% filter(TotRmsAbvGrd == 14) %>% select(Id)
# 636

ggplot(data, aes(x = Fireplaces, y = SalePrice)) + geom_point()

ggplot(data, aes(x = GarageYrBlt, y = SalePrice)) + geom_point()

ggplot(data, aes(x = GarageCars, y = SalePrice)) + geom_point()

ggplot(data, aes(x = GarageArea, y = SalePrice)) + geom_point()
data %>% filter(GarageArea > 1200 & SalePrice < 300000) %>% select(Id)
# 1062, 1191

ggplot(data, aes(x = WoodDeckSF, y = SalePrice)) + geom_point()

ggplot(data, aes(x = OpenPorchSF, y = SalePrice)) + geom_point()
data %>% filter(OpenPorchSF > 500 & SalePrice < 100000) %>% select(Id)
# 496

ggplot(data, aes(x = EnclosedPorch, y = SalePrice)) + geom_point()
data %>% filter(EnclosedPorch > 500) %>% select(Id)
# 198

ggplot(data, aes(x = X3SsnPorch, y = SalePrice)) + geom_point()

ggplot(data, aes(x = ScreenPorch, y = SalePrice)) + geom_point()

ggplot(data, aes(x = PoolArea, y = SalePrice)) + geom_point()

values = c(935, 1299, 250, 314, 336, 707, 1397, 524, 692, 379, 1183, 
           186, 739, 598, 955, 636, 49, 810, 1062, 1191, 496, 198)

data <- subset(data, !(Id %in% values))

#missing_percent <- colSums(is.na(data)) / nrow(data) * 100
#missing_percent[missing_percent > 0]

# ----------------------------------------------------
# NA VREDNOSTI

sort(colSums(is.na(data))[colSums(is.na(data)) > 0])

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
# takodje ima malo NA (osam), pa se moze iskoristiti najcesca vrednost
str(data$MasVnrType)

tbl <- xtabs(~MasVnrType, data = data)
# najcesca vrednost je None
most_common <- names(which.max(tbl))
data$MasVnrType[is.na(data$MasVnrType)] <- most_common

# povrsina zida koji je pokriven dekorativnim materijalom
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
# ima 37 NA vrednosti
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

View(data[which(is.na(data$GarageType)), ])

data$GarageType[is.na(data$GarageType)] <- "NoGarage"
data$GarageYrBlt[is.na(data$GarageYrBlt)] <- "NoGarage"
data$GarageFinish[is.na(data$GarageFinish)] <- "NoGarage"
data$GarageQual[is.na(data$GarageQual)] <- "NoGarage"
data$GarageCond[is.na(data$GarageCond)] <- "NoGarage"


# LotFrontage - duzina placa koja se granici sa ulicom
# numericka
# 254 NA

# uradi NA kao da je 0 (ne granici se sa ulicom)
str(data$LotFrontage)
View(data[which(is.na(data$LotFrontage)), ])


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


View(data)


sort(colSums(is.na(data))[colSums(is.na(data)) > 0])



#View(data[which(is.na(data$BsmtExposure)), ]) 

# TODO test skup NA 

data.test = read.csv("./data/test.csv")
summary(data.test)

summary(data)
