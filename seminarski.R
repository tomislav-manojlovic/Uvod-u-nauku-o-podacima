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
