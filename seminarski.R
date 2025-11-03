library(tidyverse)
library(ggplot2)
library(reshape2)
library(dplyr)
library(readr)
library(scales)

data = read.csv("./data/AmesHousing.csv")
names(data) <- gsub("\\.", "", names(data))
# View(data)

summary(data)
names(data)
str(data)

# uklanjanje duplih redova

names(data[, colSums(is.na(data)) > 0])

dupes = data[duplicated(data %>% 
                          select(MSSubClass, MSZoning, LotArea, SalePrice))
             |
               duplicated(data %>% 
                            select(MSSubClass, MSZoning, LotArea, SalePrice), fromLast = T), ]

nrow(dupes)
# dupes %>% select(Order)

# View the duplicates
# View(dupes)

# primecujemo da kolona Lot.Frontage negde ima vrednost a negde je NA pa cemo odbaciti sve duplikate
nrow(data)
data <- data %>%
  filter(!(Order %in% dupes$Order)) # izbacili smo duplikate
nrow(data)



### NA VREDNOSTI ###############################################################

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
data$Electrical[data$Electrical == ""] <- most_common # promenjeno


# View(data)

# MasVnrType - tip malterisanja
# takodje chr -> kat.
# takodje ima malo NA 23, pa se moze iskoristiti najcesca vrednost
str(data$MasVnrType)

# View(data %>%
#       filter(MasVnrType == ""))
# View(data %>%
#       filter(is.na(MasVnrArea)))


tbl <- xtabs(~MasVnrType, data = data)
# za ceo skup najcesca vrednost je None
most_common <- names(which.max(tbl))
data$MasVnrType[is.na(data$MasVnrType)] <- most_common

# MasVnrArea - povrsina zida koji je pokriven dekorativnim materijalom
# numericka
# 29 NA
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
# ima 79 NA vrednosti

#View(data %>%
 #      filter(is.na(BsmtCond) & TotalBsmtSF > 0))


#View(data %>%
 #      filter(is.na(BsmtExposure) & TotalBsmtSF > 0))

#View(data %>%
 #      filter(is.na(BsmtFinType2) & TotalBsmtSF > 0))

#View(data %>%
 #      filter(is.na(BsmtQual) & TotalBsmtSF > 0))

#View(data %>%
 #      filter(TotalBsmtSF == 0))

# postoji jedan red gde su BsmtFinSF1, BsmtFinSF2, BsmtUnfSF i TotalBsmtSF NA, tu stavljamo 0
data$BsmtFinSF1[is.na(data$BsmtFinSF1)] <- 0
data$BsmtFinSF2[is.na(data$BsmtFinSF2)] <- 0
data$BsmtUnfSF[is.na(data$BsmtUnfSF)] <- 0
data$TotalBsmtSF[is.na(data$TotalBsmtSF)] <- 0

data$BsmtQual[data$BsmtQual == ""] <- "NoBasement"
data$BsmtCond[data$BsmtCond == ""] <- "NoBasement"
data$BsmtExposure[data$BsmtExposure == ""] <- "NoBasement"
data$BsmtFinType1[data$BsmtFinType1 == ""] <- "NoBasement"
data$BsmtFinType2[data$BsmtFinType2 == ""] <- "NoBasement"

#View(data %>%
#       filter(is.na(BsmtQual) ))

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

# 2 NA za BsmtFullBath i BsmtHalfBath su za redove gde ne postoji Basement
# tu cemo staviti 0
#View(data %>%
#       filter(is.na(BsmtFullBath)))

data$BsmtFullBath[is.na(data$BsmtFullBath)] <- 0
data$BsmtHalfBath[is.na(data$BsmtHalfBath)] <- 0



sort(colSums(is.na(data))[colSums(is.na(data)) > 0])


# GarageType, GarageYrBlt, GarageFinish, GarageQual, GarageCond
# kategorijska ORDINALNA
# NA znaci da ne postoji garaza



# View(data %>% filter(is.na(GarageYrBlt)))
# View(data)

# View(data[data$Order == 1357 | data$Order == 2237,])

#Postoji jedan red gde je GarageType = "Detachd", a svi ostali podaci o garazi su NA.

data$GarageArea[data$Order == 2237] <- 0
data$GarageCars[data$Order == 2237] <- 0
data$GarageType[data$Order == 2237] <- NA
data$GarageQual[data$Order == 2237] <- NA
data$GarageCond[data$Order == 2237] <- NA
data$GarageFinish[data$Order == 2237] <- NA
data$GarageYrBlt[data$Order == 2237] <- -1

#Postoji red gde su prisutni podaci za GarageType, GarageCars i GarageArea, a svi ostali podaci o garazi su NA.

tbl <- xtabs(~ GarageFinish, data = data[!is.na(data$GarageFinish), ])
most_common_finish <- names(which.max(tbl))
data$GarageFinish[data$Order == 1357] <- most_common_finish

tbl <- xtabs(~ GarageQual, data = data[!is.na(data$GarageQual), ])
most_common_qual <- names(which.max(tbl))
data$GarageQual[data$Order == 1357] <- most_common_qual

tbl <- xtabs(~ GarageCond, data = data[!is.na(data$GarageCond), ])
most_common_cond <- names(which.max(tbl))
data$GarageCond[data$Order == 1357] <- most_common_cond

data$GarageYrBlt[data$Order == 1357] <- data$YearBuilt[data$Order == 1357]


data$GarageType[is.na(data$GarageType)] <- "NoGarage"
data$GarageFinish[is.na(data$GarageFinish)] <- "NoGarage"
data$GarageQual[is.na(data$GarageQual)] <- "NoGarage"
data$GarageCond[is.na(data$GarageCond)] <- "NoGarage"
data$GarageYrBlt[is.na(data$GarageYrBlt)] <- -1


sort(colSums(is.na(data))[colSums(is.na(data)) > 0])



# LotFrontage - duzina placa koja se granici sa ulicom 
# numericka
# 490 NA
# Impute by grouping by Neighborhood and taking the median LotFrontage for that group. 
# This is much better than using the overall median.

# uradi NA kao da je 0 (ne granici se sa ulicom)?
str(data$LotFrontage)
#View(data[which(is.na(data$LotFrontage)), ])

# popunjavamo vrednostima medijane za neighbourhood kom pripada

# View(data %>% filter(is.na(LotFrontage)))
# View(data)

data <- data %>%
  group_by(Neighborhood) %>%
  mutate(LotFrontage = ifelse(is.na(LotFrontage),
                               median(LotFrontage, na.rm = TRUE),
                               LotFrontage)) %>%
  ungroup()


# tri vrednosti ostaju nepopunjene, jer za su za taj neighborhood svi NA
# popunicemo ih medijanom celog skupa 

data$LotFrontage[is.na(data$LotFrontage)] <- median(data$LotFrontage, na.rm = TRUE)

data$LotFrontage <- as.integer(data$LotFrontage)
typeof(data$LotFrontage)

sort(colSums(is.na(data))[colSums(is.na(data)) > 0])


# FireplaceQu - kvalitet kamina
# kat. Ordinalna 
# NA - nema kamina
# 1422

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
#View(data[is.na(data$MiscFeature), ])

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

# View(data)

sort(colSums(is.na(data))[colSums(is.na(data)) > 0])

### ENKODIRANJE PROMENLJIVIH ###################################################

data$ExterQual <- factor(data$ExterQual, levels = c("Po", "Fa", "TA", "Gd", "Ex"), ordered = TRUE)
data$ExterCond <- factor(data$ExterCond, levels = c("Po", "Fa", "TA", "Gd", "Ex"), ordered = TRUE)
data$BsmtQual <- factor(data$BsmtQual, levels = c("NoBasement", "Po", "Fa", "TA", "Gd", "Ex"), ordered = TRUE)
data$BsmtCond <- factor(data$BsmtCond, levels = c("NoBasement", "Po", "Fa", "TA", "Gd", "Ex"), ordered = TRUE)
data$HeatingQC <- factor(data$HeatingQC, levels = c("Po", "Fa", "TA", "Gd", "Ex"), ordered = TRUE)
data$KitchenQual <- factor(data$KitchenQual, levels = c("Po", "Fa", "TA", "Gd", "Ex"), ordered = TRUE)
data$FireplaceQu <- factor(data$FireplaceQu, levels = c("NoFireplace", "Po", "Fa", "TA", "Gd", "Ex"), ordered = TRUE)
data$GarageQual <- factor(data$GarageQual, levels = c("NoGarage", "Po", "Fa", "TA", "Gd", "Ex"), ordered = TRUE)
data$GarageCond <- factor(data$GarageCond, levels = c("NoGarage", "Po", "Fa", "TA", "Gd", "Ex"), ordered = TRUE)

data$PoolQC <- factor(data$PoolQC, levels = c("NoPool", "Fa", "TA", "Gd", "Ex"), ordered = TRUE)

data$BsmtExposure <- factor(data$BsmtExposure, levels = c("NoBasement", "No", "Mn", "Av", "Gd"), ordered = TRUE)

data$BsmtFinType1 <- factor(data$BsmtFinType1, levels = c("NoBasement", "Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ"), ordered = TRUE)
data$BsmtFinType2 <- factor(data$BsmtFinType2, levels = c("NoBasement", "Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ"), ordered = TRUE)

data$Functional <- factor(data$Functional, levels = c("Sal", "Sev", "Maj2", "Maj1", "Mod", "Min2", "Min1", "Typ"), ordered = TRUE)

data$GarageFinish <- factor(data$GarageFinish, levels = c("NoGarage", "Unf", "RFn", "Fin"), ordered = TRUE)

# pretvoricemo ordinalne u integer
# kako bi model razlikovao nivoe

data$ExterQual <- as.integer(data$ExterQual)
data$ExterCond <- as.integer(data$ExterCond)
data$BsmtQual <- as.integer(data$BsmtQual)
data$BsmtCond <- as.integer(data$BsmtCond)
data$HeatingQC <- as.integer(data$HeatingQC)
data$KitchenQual <- as.integer(data$KitchenQual)
data$FireplaceQu <- as.integer(data$FireplaceQu)
data$GarageQual <- as.integer(data$GarageQual)
data$GarageCond <- as.integer(data$GarageCond)
data$PoolQC <- as.integer(data$PoolQC)
data$BsmtExposure <- as.integer(data$BsmtExposure)
data$BsmtFinType1 <- as.integer(data$BsmtFinType1)
data$BsmtFinType2 <- as.integer(data$BsmtFinType2)
data$Functional <- as.integer(data$Functional)
data$GarageFinish <- as.integer(data$GarageFinish)

str(data)

nominalne <- data %>% 
  select(where(is.character)) %>% 
  names()
data[nominalne] <- lapply(data[nominalne], factor)

# Moze se primetiti da je MSSubClass numericka, a u stvari predstavlja nominalnu, kategorijsku promenljivu.
table(data$MSSubClass)
data$MSSubClass <- as.factor(data$MSSubClass)
str(data$MSSubClass)

#str(data)



### NEOBICNE VREDNOSTI / GRESKE ################################################

#Postoji jedan red u test skupu gde je godina izgranje garaze 2207. Ocigledno je da je doslo do greske.  
#Takode, ta kuca je izgradena 2006, a renovirana 2007. godine, pa cemo zameniti tu vrednost.

data$GarageYrBlt[data$GarageYrBlt==2207] <- 2007

# View(data %>% filter(data$YrSold - data$YearRemodAdd < 0))
# postoje dva reda gde je YearRemodAdd nakon YrSold
# zamenicemo YearRemodAdd sa YearBuilt
data$YearRemodAdd[data$YearRemodAdd > data$YrSold] <- data$YearBuilt[data$YearRemodAdd > data$YrSold]

#Mogu se uociti dva reda gde je HouseAge manji od 0, sto znaci da je kuca prodata pre nego sto je izgradena,
#odnosno da je doslo do greske.  
#Obrisacemo ta dva reda. Takode, postoji red gde je YearBuilt > YearRemodAdd.

data <- data %>% 
  filter(YrSold - YearBuilt >= 0)
data <- data %>% 
  filter(YearBuilt <= YearRemodAdd)

### TRAIN TEST SPLIT ###########################################################

set.seed(123)
train_idx = sample(seq_len(nrow(data)), size = 0.8 * nrow(data))

data.train = data[train_idx, ]
data.test = data[-train_idx, ]

### OUTLIERI ###################################################################

# plotovanje svih numerickih karakteristika

ggplot(data.train, aes(x = MSSubClass, y = SalePrice)) + geom_point()

ggplot(data.train, aes(x = LotFrontage, y = SalePrice)) + geom_point()
data.train %>% filter(LotFrontage > 300) %>% select(Order)
# 1499, 1266

ggplot(data.train, aes(x = LotArea, y = SalePrice)) + geom_point()
data.train %>% filter(LotArea > 100000) %>% select(Order)
# 2072, 1571, 957, 2116

ggplot(data.train, aes(x = OverallQual, y = SalePrice)) + geom_point()
data.train %>% filter(OverallQual == 10 & SalePrice < 200000) %>% select(Order)
# 1499, 2181

ggplot(data.train, aes(x = OverallCond, y = SalePrice)) + geom_point()
data.train %>% filter(OverallCond == 6 & SalePrice > 600000) %>% select(Order)
# 1768
data.train %>% filter(OverallCond == 2 & SalePrice > 300000) %>% select(Order)
# 18
data.train %>% filter(OverallCond == 5 & SalePrice > 700000) %>% select(Order)
# 1761

ggplot(data.train, aes(x = YearBuilt, y = SalePrice)) + geom_point()
data.train %>% filter(YearBuilt < 1900 & SalePrice > 400000) %>% select(Order)
# 2667

ggplot(data.train, aes(x = YearRemodAdd, y = SalePrice)) + geom_point()
data.train %>% filter(YearRemodAdd < 1970 & SalePrice > 300000) %>% select(Order)
# 957

ggplot(data.train, aes(x = MasVnrArea, y = SalePrice)) + geom_point()

ggplot(data.train, aes(x = BsmtFinSF1, y = SalePrice)) + geom_point()
data.train %>% filter(BsmtFinSF1 > 4000) %>% select(Order)
# 1499

ggplot(data.train, aes(x = BsmtFinSF2, y = SalePrice)) + geom_point()
data.train %>% filter(BsmtFinSF2 > 500 & SalePrice > 500000) %>% select(Order)
# 424

ggplot(data.train, aes(x = BsmtUnfSF, y = SalePrice)) + geom_point()

ggplot(data.train, aes(x = TotalBsmtSF, y = SalePrice)) + geom_point()
data.train %>% filter(TotalBsmtSF > 6000) %>% select(Order)
# 1499

ggplot(data.train, aes(x = X1stFlrSF, y = SalePrice)) + geom_point()
data.train %>% filter(X1stFlrSF > 4500) %>% select(Order)
# 1499

ggplot(data.train, aes(x = X2ndFlrSF, y = SalePrice)) + geom_point()

ggplot(data.train, aes(x = LowQualFinSF, y = SalePrice)) + geom_point()
data.train %>% filter(LowQualFinSF > 500 & SalePrice > 400000) %>% select(Order)
# 2667

ggplot(data.train, aes(x = GrLivArea, y = SalePrice)) + geom_point()
data.train %>% filter(GrLivArea > 4500) %>% select(Order)
# 1499, 2181

ggplot(data.train, aes(x = BsmtFullBath, y = SalePrice)) + geom_point()

ggplot(data.train, aes(x = BsmtHalfBath, y = SalePrice)) + geom_point()
data.train %>% filter(BsmtHalfBath == 2) %>% select(Order)
# View(data.train %>% filter(BsmtHalfBath == 2))
# 1734, 2821, 2499
data.train %>% filter(BsmtHalfBath == 1 & SalePrice > 700000) %>% select(Order)
# 1768

ggplot(data.train, aes(x = FullBath, y = SalePrice)) + geom_point()

ggplot(data.train, aes(x = HalfBath, y = SalePrice)) + geom_point()

ggplot(data.train, aes(x = BedroomAbvGr, y = SalePrice)) + geom_point()
data.train %>% filter(BedroomAbvGr == 8) %>% select(Order)
# 2195

ggplot(data.train, aes(x = KitchenAbvGr, y = SalePrice)) + geom_point()
data.train %>% filter(KitchenAbvGr == 3) %>% select(Order)
# 713, 716
data.train %>% filter(KitchenAbvGr == 0) %>% select(Order)
# 2821, 2254 

ggplot(data.train, aes(x = TotRmsAbvGrd, y = SalePrice)) + geom_point()
data.train %>% filter(TotRmsAbvGrd > 12) %>% select(Order)
# 2195

ggplot(data.train, aes(x = Fireplaces, y = SalePrice)) + geom_point()
data.train %>% filter(Fireplaces == 4) %>% select(Order)
# 2499

ggplot(data.train %>% filter(GarageYrBlt > 0), aes(x = GarageYrBlt, y = SalePrice)) + geom_point()

ggplot(data.train, aes(x = GarageCars, y = SalePrice)) + geom_point()
data.train %>% filter(GarageCars == 5) %>% select(Order)
# 747

ggplot(data.train, aes(x = GarageArea, y = SalePrice)) + geom_point()

ggplot(data.train, aes(x = WoodDeckSF, y = SalePrice)) + geom_point()
data.train %>% filter(WoodDeckSF > 1000) %>% select(Order)
# 2294

ggplot(data.train, aes(x = OpenPorchSF, y = SalePrice)) + geom_point()
data.train %>% filter(OpenPorchSF > 520) %>% select(Order)
# 2066, 727

ggplot(data.train, aes(x = EnclosedPorch, y = SalePrice)) + geom_point()
data.train %>% filter(EnclosedPorch > 1000) %>% select(Order)
# 2090

ggplot(data.train, aes(x = X3SsnPorch, y = SalePrice)) + geom_point()

ggplot(data.train, aes(x = ScreenPorch, y = SalePrice)) + geom_point()

ggplot(data.train, aes(x = PoolArea, y = SalePrice)) + geom_point()

values = c(1499, 1266, 2072, 1571, 957, 2116, 2181, 1734, 2499, 1768, 18,
           1761, 2667, 2821, 2195, 713, 716, 2254, 926, 747, 2294, 2066, 727, 2090)

nrow(data.train)
data.train <- data.train %>%
  filter(!(Order %in% values)) # izbacili smo outliere
nrow(data.train)



### EDA ########################################################################

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

ggplot(data.train, aes(x = Neighborhood, y = SalePrice)) +
  geom_boxplot(fill = "lightblue") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Odnos Neighborhood i SalePrice",
    x = "Neighborhood",
    y = "SalePrice"
  ) +
  theme_minimal() +
  theme( plot.title = element_text(
    hjust = 0.5, face ="bold"),    axis.text.x = element_text(angle = 45, hjust = 1))

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

ggplot(data, aes(x = as.factor(HouseStyle), y = SalePrice)) +
  geom_boxplot() +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Odnos HouseStyle i SalePrice",
    x = "HouseStyle",
    y = "SalePrice"
  ) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# 2. koristimo koeficijente korelacije za numericke atribute

numeric_data <- data[sapply(data, is.numeric)]
cor_matrix <- cor(numeric_data, use = "pairwise.complete.obs")
cor_with_saleprice <- cor_matrix[, "SalePrice"]
cor_with_saleprice <- sort(cor_with_saleprice, decreasing = TRUE)
print(cor_with_saleprice)

cor_df <- data.frame(
  Variable = names(cor_with_saleprice),
  Correlation = cor_with_saleprice
)

cor_df <- cor_df[cor_df$Variable != "SalePrice", ]

ggplot(cor_df, aes(x = reorder(Variable, Correlation), y = Correlation)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Korelacija numerickih atributa sa SalePrice",
       x = "Atribut", y = "Koeficijent korelacije")

ggplot(data, aes(x = X1stFlrSF, y = SalePrice)) +
  geom_point(alpha = 0.7) +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Odnos X1stFlrSF i SalePrice",
    x = "X1stFlrSF",
    y = "SalePrice"
  ) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggplot(data, aes(x = as.factor(TotRmsAbvGrd), y = SalePrice)) +
  geom_boxplot() +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Odnos TotRmsAbvGrd i SalePrice",
    x = "TotRmsAbvGrd",
    y = "SalePrice"
  ) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# 3. proveravamo multikolinearnost
# GarageArea i GarageCars
# GrLivArea i 1stFlrSF
# TotalBsmtSF i BsmtFinSF1
# proveriti pomocu korelacione matrice ili Variance Inflation Factor (VIF)
# zadrzati samo jednu reprezentativnu promenljivu

numeric_vars <- data.train[sapply(data.train, is.numeric)]

corr_matrix <- cor(numeric_vars, use = "pairwise.complete.obs", method = "pearson")
threshold <- 0.8
corr_matrix[lower.tri(corr_matrix, diag = TRUE)] <- NA

corr_table <- as.data.frame(as.table(corr_matrix))

high_corr <- subset(corr_table, !is.na(Freq) & abs(Freq) > threshold)
high_corr <- high_corr[order(-abs(high_corr$Freq)), ]
high_corr$Freq <- round(high_corr$Freq, 3)
high_corr

# dropujemo jednu, zadrzavamo jednu (ako ih uopste koristimo za modele)



### Feature engineering ########################################################

data <- rbind(data.train, data.test)

table(data$YrSold)

# View(data %>% filter(is.na(BsmtFullBath)))

# View(data)

data$TotalSF <- data$TotalBsmtSF + data$GrLivArea

ggplot(data = data[!is.na(data$SalePrice),], aes(x = TotalSF, y = SalePrice)) +
  geom_point(col = 'blue') +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  scale_y_continuous(breaks = seq(0, 800000, by = 100000), labels = scales::comma) +
  theme_minimal()

# Postoji jaka korelacija 


data$TotalFinishedSF <- data$GrLivArea + data$BsmtFinSF1 + data$BsmtFinSF2

ggplot(data = data[!is.na(data$SalePrice),], aes(x = TotalFinishedSF, y = SalePrice)) +
  geom_point(col = 'blue') +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  scale_y_continuous(breaks = seq(0, 800000, by = 100000), labels = scales::comma) +
  theme_minimal()

# Postoji jaka korelacija

data$HouseAge <- data$YrSold - data$YearBuilt

data$HouseRemodAge <- data$YrSold - data$YearRemodAdd


ggplot(data=data[!is.na(data$SalePrice),], aes(x=HouseAge, y=SalePrice))+
  geom_point(col='blue') + geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma)
# Starije kuce imaju manju cenu

data$Remodeled <- ifelse(data$YearBuilt==data$YearRemodAdd, 0, 1) 
# 0 - ne
# 1 - da

data$Remodeled <- as.factor(data$Remodeled)

# Analiza pokazuje da kuće koje nisu renovirane imaju veću medijanu cene.
ggplot(data=data[!is.na(data$SalePrice),], aes(x = Remodeled, y = SalePrice)) +
  geom_boxplot(fill = "skyblue", alpha = 0.7) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Odnos između renoviranja i cene kuce",
    x = "Da li je kuca renovirana",
    y = "Cena kuce (SalePrice)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
  )


data$TotalPorchSF <- data$OpenPorchSF + data$EnclosedPorch +
  data$X3SsnPorch + data$ScreenPorch 
cor(data$SalePrice, data$TotalPorchSF, use= "pairwise.complete.obs")
# Slaba korelacija

#Dodata je promenljiva TotalBaths, koja oznacava ukupan broj kupatila. Od nje cemo napraviti ordinalnu, kategorijsku promenljivu sa nivoima: malo (< 2), srednje (2,3), mnogo (> 3).  
#Postoji korelacija izmedu ciljne promenljive i TotalBaths.

data$TotalBaths <- data$BsmtFullBath + data$FullBath + 
  0.5 * (data$BsmtHalfBath + data$HalfBath)

data$TotalBaths <- cut(
  data$TotalBaths,
  breaks = c(-Inf, 1.5, 3, Inf),
  labels = c("Malo", "Srednje", "Mnogo"),
  right = TRUE
)
#
table(data$TotalBaths)

ggplot(data=data, aes(x = TotalBaths, y = SalePrice)) +
  geom_boxplot(fill = "skyblue", alpha = 0.7) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Odnos izmedu ukupnog broja kupatila i cene kuce",
    x = "Ukupan broj kupatila",
    y = "Cena kuce (SalePrice)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
  )

data$TotalBaths <- as.integer(data$TotalBaths)



data$IsNew <- ifelse(data$YrSold==data$YearBuilt, 1, 0)
data$IsNew = as.factor(data$IsNew)

data$HasBath <- ifelse(data$BsmtFullBath + data$FullBath + 0.5 * (data$BsmtHalfBath + data$HalfBath) > 0, 1, 0)
data$HasBath <- as.factor(data$HasBath)

data$HasBsmt <- ifelse(data$TotalBsmtSF > 0, 1, 0)
data$HasBsmt <- as.factor(data$HasBsmt)

# garage, fireplace, pool
data$HasGarage <- ifelse(data$GarageArea > 0, 1, 0)
data$HasGarage <- as.factor(data$HasGarage)

data$HasFireplace <- ifelse(data$Fireplaces > 0, 1, 0)
data$HasFireplace <- as.factor(data$HasFireplace)

data$HasPool <- ifelse(data$PoolArea > 0, 1, 0)
data$HasPool <- as.factor(data$HasPool)

# Cene zavise od toga da li je kuca nova.

ggplot(data, aes(x = IsNew, y = SalePrice)) +
  geom_boxplot(fill = "skyblue", alpha = 0.7) +
  scale_y_continuous(labels = comma) +
  labs(title = "Odnos izmedu novosti kuce i cene",
       x = "IsNew",
       y = "Cena kuce") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Svaka kuca ima makar jedno kupatilo, pa nam ovaj atribut nije znacajan.

ggplot(data, aes(x = HasBath, y = SalePrice)) +
  geom_boxplot(fill = "skyblue", alpha = 0.7) +
  scale_y_continuous(labels = comma) +
  labs(title = "Odnos izmedu prisustva kupatila i cene",
       x = "HasBath",
       y = "Cena kuce") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

data$HasBath <- NULL

# Postojanje podruma utice na SalePrice.

ggplot(data, aes(x = HasBsmt, y = SalePrice)) +
  geom_boxplot(fill = "skyblue", alpha = 0.7) +
  scale_y_continuous(labels = comma) +
  labs(title = "Odnos izmedu prisustva podruma i cene",
       x = "HasBsmt",
       y = "Cena kuce") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


# Postojanje garaze utice na SalePrice

ggplot(data, aes(x = HasGarage, y = SalePrice)) +
  geom_boxplot(fill = "skyblue", alpha = 0.7) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Odnos izmedju posedovanja garaze i cene kuce",
    x = "HasGarage",
    y = "Cena kuce"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Postojanje kamina utice na SalePrice.

ggplot(data, aes(x = HasFireplace, y = SalePrice)) +
  geom_boxplot(fill = "skyblue", alpha = 0.7) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Odnos izmedju posedovanja kamina i cene kuce",
    x = "HasFireplace",
    y = "Cena kuce"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# bazen
# malo kuca ima bazen
ggplot(data, aes(x = HasPool, y = SalePrice)) +
  geom_boxplot(fill = "skyblue", alpha = 0.7) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Odnos izmedju posedovanja bazena i cene kuce",
    x = "HasPool",
    y = "Cena kuce"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


data.train = data[train_idx, ]

nrow(data.train)
data.train = drop_na(data.train)
nrow(data.train)

data.test = data[-train_idx, ]

nrow(data.test)
data.train = drop_na(data.train)
nrow(data.test)



### IMPLEMENTACIJA I PROCENA MODELA ############################################

library(tidyverse)
library(caret)
library(car)
library(glmnet)
library(Metrics)

str(data)
summary(data$SalePrice)

numeric_data <- data[sapply(data, is.numeric)]

cor_matrix <- cor(numeric_data, use = "pairwise.complete.obs")
cor_with_saleprice <- cor_matrix[, "SalePrice"]
cor_with_saleprice <- sort(cor_with_saleprice, decreasing = TRUE)
print(cor_with_saleprice)

# proveravamo simetricnost vrednosti atributa SalePrice

hist(data.train$SalePrice)

data.train = data.train %>% mutate(SalePrice = log1p(SalePrice))

hist(data.train$SalePrice)

# primecujemo neke vrednosti iza 10
# da proverimo sta se tu radi

data.train %>% filter(SalePrice < 10) %>% select(Order, SalePrice)
data.train = data.train %>% filter(SalePrice >= 10)

hist(data.train$SalePrice)

# koristimo metod forward selection za konstruisanje modela linearne regresije

model1 = lm(SalePrice ~ TotalSF, data = data.train) # atribut sa najboljom korelacijom
summary(model1)

plot(data$TotalSF, data$SalePrice)

model2 = lm(SalePrice ~ TotalSF + OverallQual, data = data.train) # atribut sa drugom najboljom korelacijom
summary(model2)

plot(data.train$OverallQual, data.train$SalePrice)

# gledamo korelaciju izmedju atributa da bismo izbegli dodavanje atributa
# koji bi prouzrokovali multikoliearnost

cor(data.train$TotalSF, data.train$TotalFinishedSF)
cor(data.train$OverallQual, data.train$TotalFinishedSF)
cor(data.train$TotalSF, data.train$GrLivArea)

cor(data.train$TotalSF, data.train$GarageCars)
cor(data.train$OverallQual, data.train$GarageCars)

model3 = lm(SalePrice ~ TotalSF + OverallQual + GarageCars, data = data.train)
summary(model3)

plot(data.train$GarageCars, data.train$SalePrice)

model4 = lm(SalePrice ~ TotalSF + OverallQual + GarageCars + TotalBaths, data = data.train)
summary(model4)

vif(model4) # vidimo da nema multikolinearnosti (vif < 5)

# mozemo da nastavimo sa dodavanjem

model5 = lm(SalePrice ~ TotalSF + OverallQual + GarageCars + TotalBaths + HouseAge, data = data.train)
summary(model5)

vif(model5) # vidimo da nema multikolinearnosti (vif < 5)

# sada da ubacimo neku kategorijsku promenljivu

model6 = lm(SalePrice ~ TotalSF + OverallQual + GarageCars + TotalBaths + HouseAge + 
              Neighborhood, data = data.train)
summary(model6)

model7 = lm(SalePrice ~ TotalSF + OverallQual + GarageCars + TotalBaths + HouseAge + 
              Neighborhood + TotRmsAbvGrd, data = data.train)
summary(model7)

vif(model7)

cor(data.train$TotalSF, data.train$TotRmsAbvGrd)

plot(data.train$TotRmsAbvGrd, data.train$TotalSF)

model8 = lm(SalePrice ~ TotalSF * TotRmsAbvGrd + OverallQual + GarageCars + TotalBaths + HouseAge
            + Neighborhood, data = data.train)
summary(model8)

# ipak cemo da izbacimo TotRmsAbvGrd

model9 = lm(SalePrice ~ TotalSF + OverallQual + GarageCars + TotalBaths + HouseAge + Neighborhood + 
              KitchenQual, data = data.train)
summary(model9)

vif(model9)

model10 = lm(SalePrice ~ TotalSF + OverallQual + GarageCars + TotalBaths + HouseAge + Neighborhood + 
               KitchenQual + Fireplaces, data = data.train)
summary(model10)

vif(model10)

model11 = lm(SalePrice ~ TotalSF * OverallQual + GarageCars + TotalBaths + HouseAge + HasGarage + 
               Neighborhood + KitchenQual + Fireplaces, data = data.train)
summary(model11)
vif(model11)

## Ridge i Lasso

x_train <- model.matrix(SalePrice ~ ., data = data.train)[, -1]
y_train <- data.train$SalePrice

###############

for (col in names(data.test)) {
  if (is.factor(data.train[[col]])) {
    data.test[[col]] <- factor(data.test[[col]], levels = levels(data.train[[col]]))
  }
}

x_test <- model.matrix(~ ., data = data.test)[, -1]
x_test <- x_test[, colnames(x_train)]
y_test <- data.test$SalePrice

set.seed(123)
ridge_cv <- cv.glmnet(x_train, y_train, alpha = 0)
lasso_cv <- cv.glmnet(x_train, y_train, alpha = 1)

ridge_cv$lambda.min
lasso_cv$lambda.min

ridge_preds = expm1(predict(ridge_cv, newx = x_test, s = "lambda.min"))
lasso_preds = expm1(predict(lasso_cv, newx = x_test, s = "lambda.min"))
# model11_preds <- expm1(predict(model11, newdata = data.test))

rmse(y_test, ridge_preds)
mae(y_test, ridge_preds)

rmse(y_test, lasso_preds)
mae(y_test, lasso_preds)

# rmse(y_test, model11_preds)
# mae(y_test, model11_preds)

###############

## Random Forest

library(randomForest)

dummies <- dummyVars(~ ., data = data.train[, !names(data.train) %in% "SalePrice"])

data.train.num <- data.frame(predict(dummies, newdata = data.train))
data.train.num$SalePrice <- data.train$SalePrice

data.test.num <- data.frame(predict(dummies, newdata = data.test))

rf_model <- randomForest(SalePrice ~ ., data = data.train.num, ntree = 50, mtry = 10, importance = TRUE)

rf_preds <- expm1(predict(rf_model, newdata = data.test.num))

rmse(data.test$SalePrice, rf_preds)
mae(data.test$SalePrice, rf_preds)

## XGBoost

library(xgboost)

train_idx <- sample(seq_len(nrow(data)), size = 0.8 * nrow(data))
train_data <- data[train_idx, ]
val_data   <- data[-train_idx, ]

dummies <- dummyVars(~ ., data = train_data[, !names(train_data) %in% "SalePrice"])
train_data_num <- data.frame(predict(dummies, newdata = train_data))
train_data_num$SalePrice <- train_data$SalePrice

val_data_num <- data.frame(predict(dummies, newdata = val_data))

train_matrix <- as.matrix(train_data_num[, !names(train_data_num) %in% "SalePrice"])
train_label  <- train_data_num$SalePrice

val_matrix <- as.matrix(val_data_num)
val_label  <- val_data$SalePrice

xgb_model = xgboost(data = train_matrix, label = train_label, nrounds = 100, objective = "reg:squarederror", verbose = 0)

pred_xgb = predict(xgb_model, newdata = val_matrix)

rmse(val_data$SalePrice, pred_xgb)
mae(val_data$SalePrice, pred_xgb)

## Support Vector Regression (SVR)

library(e1071)

svr_model = svm(SalePrice ~ ., data = data.train)
pred_svr = predict(svr_model, newdata = data.test)

rmse(data.test$SalePrice, pred_svr)
mae(data.test$SalePrice, pred_svr)

## Neural Networks (ANNs)

library(neuralnet)

dummies <- dummyVars(~ TotalSF + OverallQual + GarageCars + TotalBaths +
                       HouseAge + HasGarage + Neighborhood + KitchenQual + Fireplaces,
                     data = data.train)

train.num <- data.frame(predict(dummies, newdata = data.train))
train.num$SalePrice <- data.train$SalePrice

test.num <- data.frame(predict(dummies, newdata = data.test))

nn_model = neuralnet(SalePrice ~ .,
                     data = train.num, hidden = c(10, 5), stepmax = 1e6, linear.output = T)
# plot(nn_model)

nn_preds <- compute(nn_model, test.num[, names(test.num) != "SalePrice"])
nn_preds <- expm1(nn_preds$net.result)

rmse(data.test$SalePrice, nn_preds)
mae(data.test$SalePrice, nn_preds)
