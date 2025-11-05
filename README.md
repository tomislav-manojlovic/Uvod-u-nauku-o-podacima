## Pokretanje projekta

### Potrebni paketi

Pre pokretanja projekta, potrebno je instalirati i učitati sledeće R pakete:

```r
library(tidyverse)
library(ggplot2)
library(reshape2)
library(dplyr)
library(readr)
library(scales)
library(caret)
library(car)
library(glmnet)
library(Metrics)
library(randomForest)
library(xgboost)
library(e1071)
```

### Instalacija paketa

Ako neki od paketa nije instaliran, možete ga instalirati pomoću funkcije:

```r
install.packages("naziv_paketa")
```

## Pokretanje projekta

1. Preuzmite ili klonirajte ceo GitHub repo na svoj lokalni računar.
2. Otvorite RStudio ili drugi R IDE.
3. Postavite radni direktorijum na folder sa projektom:

```r
setwd("putanja/do/foldera/projekta")
```

4. Pokrenite R skriptu

```r
source("seminarski.R")
```

## Predviđanje cena kuća

Predviđanje cena kuća ima veliki značaj u savremenom svetu jer omogućava donošenje boljih ekonomskih i investicionih odluka.  
Tačne procene vrednosti nekretnina pomažu kupcima da ne plate više nego što objekat zaista vredi, dok prodavcima omogućavaju da postave realnu i konkurentnu cenu.  
Investitori koriste predviđanja cena kako bi odabrali najisplativije lokacije i trenutke za ulaganje, dok banke i finansijske institucije procenjuju rizik prilikom odobravanja kredita i hipoteka.  
Takođe, vlasti i urbanisti mogu na osnovu takvih podataka da planiraju razvoj gradova i prate trendove u stanogradnji.  
Na širem nivou, predviđanje cena kuća doprinosi stabilnosti tržišta nekretnina i sprečava formiranje ekonomskih balona koji mogu dovesti do finansijskih kriza.

### Opis problema

Iz ugla nauke o podacima, problem predviđanja cena kuća predstavlja zadatak **regresije**, gde se na osnovu poznatih karakteristika nekretnina (lokacija, površina, broj soba, starost i sl.) pokušava proceniti njihova tržišna vrednost.  
Cilj je izgraditi model koji može naučiti obrasce i odnose između tih karakteristika i cene, a zatim ih koristiti za predviđanje cena novih, nepoznatih kuća.

### Opis podataka

Za potrebe ovog istraživanja korišćen je **Ames Housing Dataset**, preuzet sa **Kaggle** platforme u okviru takmičenja [House Prices: Advanced Regression Techniques](https://www.kaggle.com/c/house-prices-advanced-regression-techniques).

- Skup sadrži detaljne informacije o **2919 stambenih objekata** u gradu **Ames (Iowa, SAD)**, prikupljene između **2006. i 2010. godine**.
- **1460 primera** u trening skupu
- **1459 primera** u test skupu
- Svaki primer opisan je pomoću **81 atributa**, uključujući jedinstveni identifikator `Id` i ciljnu promenljivu `SalePrice`.
- Nakon učitavanja podataka, primećuje se da postoji:
  - 43 obeležja znakovnog tipa (`chr`)
  - 38 obeležja numeričkog tipa (`int`)

> Detaljan spisak svih promenljivih može se pronaći u fajlu: [`Spisak obelezja.txt`](Spisak%20obelezja.txt)

## Čišćenje i obrada podataka

Pre treniranja modela, dataset je pažljivo očišćen i pripremljen:

- **Uklanjanje outliera**:  
  Ekstremne vrednosti mogu iskriviti model i smanjiti tačnost. Scatter plotovi su korišćeni za identifikaciju outliera u odnosu na `SalePrice`.  
  Odabrani redovi sa neuobičajeno visokim ili niskim vrednostima su uklonjeni (`Order` vrednosti uklonjene: 1499, 1266, 2072, ...).

- **Popunjavanje nedostajućih vrednosti (NA)**:  
  Numeričke kolone su popunjene medijanom, dok su kategorijske kolone popunjene modom.  
  Kolone sa NA vrednostima uključuju: `BsmtQual`, `GarageYrBlt`, `LotFrontage`, `FireplaceQu`, `PoolQC` i dr.

- **Kategorijske i ordinalne promenljive**:

  - Sve kategorijske promenljive pretvorene su u faktore.
  - Ordinalne kategorijske promenljive kodirane su kao integer vrednosti da bi model prepoznao nivoe.

- **Ispravljanje neobičnih vrednosti i grešaka**:
  - `GarageYrBlt = 2207` ispravljeno na 2007
  - `YearRemodAdd > YrSold` zamenjeno sa `YearBuilt`
  - Redovi sa negativnim `HouseAge` ili gde je `YearBuilt > YearRemodAdd` obrisani

---

## Podela podataka

- Skup je podeljen na **trening i test** u odnosu **80:20**.
- Trening skup: koristi se za učenje modela i detekciju obrazaca
- Test skup: koristi se za evaluaciju modela na neviđenim podacima

---

## Eksplorativna analiza podataka (EDA)

Fokus EDA bio je na atributima koji značajno utiču na cenu nekretnine:

- **Veličina kuće**: `GrLivArea`, `TotalBsmtSF`, `LotArea`
- **Starost kuće**: `YearBuilt`
- **Kvalitet**: `OverallQual`, `OverallCond`, `KitchenQual`
- **Lokacija**: `Neighborhood`
- **Pogodnosti**: `GarageCars`, `Fireplaces`, `FullBath`

Vizuelizacije i korelacije su korišćene za identifikaciju najrelevantnijih prediktora.

---

## Feature Engineering

Nove promenljive su kreirane kako bi se poboljšala prediktivna moć modela:

- **Ukupna površina kuće**: `TotalSF`, `TotalFinishedSF`
- **Starost kuće i renoviranja**: `HouseAge`, `HouseRemodAge`
- **Binarne promenljive**:
  - `IsNew` – da li je kuća nova
  - `HasBsmt`, `HasBath`, `HasGarage`, `HasFireplace`, `HasPool`
  - `Remodeled` – da li je kuća renovirana
- **Dodatni atributi**:

  - `TotalPorchSF` – ukupna površina terasa i verandi
  - `TotalBaths` – ukupan broj kupatila (ordinalna kategorija: malo <2, srednje 2-3, mnogo >3)

- Promenljive sa najjačom korelacijom sa `SalePrice` su zadržane, dok su one sa slabom korelacijom isključene.
- Ovaj pristup smanjuje dimenzionalnost i poboljšava efikasnost modela, zadržavajući najrelevantnije prediktore.

## Implementacija i procena modela

Cilj projekta je kreiranje robustnih modela koji predviđaju cenu kuće (`SalePrice`) na osnovu vrednosti ostalih atributa. Predviđanje cene, koja je kontinuirani tip podatka, predstavlja regresioni problem. To znači da ćemo birati između modela koji su namenjeni za rešavanje regresionih problema poput Random Forest, XGBoost, Support Vector Regression itd.

### Proces kreiranja modela

- Korišćen je **iterativni forward-selection** proces:
  1. Početak sa jednim atributom (najviše obećavajući).
  2. Dodavanje atributa jedan po jedan radi smanjenja greške modela.
  3. Kontrola da se izbegne overfitting.
- Odabir atributa baziran je na EDA, domenskom znanju i korelaciji između prediktora.

### Konačni linearni model

```r
model11 <- lm(SalePrice ~ TotalSF * OverallQual + GarageCars + TotalBaths +
              HouseAge + HasGarage + Neighborhood + KitchenQual + Fireplaces,
              data = data.train)
```

### Rezultati linearnog modela

- **Residual Standard Error (RSE):** 0.1357
- **Multiple R-squared:** 0.8888
- **Adjusted R-squared:** 0.8871
- **F-statistic:** 517.1 (35 i 2264 DF, p < 2.2e-16)

> Model objašnjava **88.88% varijabilnosti `SalePrice`** korišćenjem izabranih prediktora.
> VIF koeficijenti ne pokazuju značajnu multikolinearnost.

---

### Poređenje modela

Rezultati korišćenih modela sa metrikama RMSE i MAE:

| Model                    | RMSE      | MAE       |
| ------------------------ | --------- | --------- |
| Ridge Regression         | 18,774.8  | 13,329.79 |
| Lasso Regression         | 18,485.85 | 13,109.65 |
| Random Forest            | 25,591.85 | 16,300.5  |
| XGBoost                  | 26,245.31 | 15,249.45 |
| Support Vector Regressor | 192,396.7 | 178,205.1 |

**Zaključci:**

- Najbolje performanse: **Lasso Regression** (najniža RMSE i MAE)
- Drugi najbolji: **Ridge Regression**
- Solidni, ali nešto lošiji: **Random Forest** i **XGBoost**
- Najlošiji: **Support Vector Regressor** (nije skalirano, što negativno utiče na performanse)

> Ovi rezultati naglašavaju važnost pripreme podataka i odabira odgovarajućeg modela za tačno predviđanje cena kuća.

## Zaključak

U radu smo predviđali cene kuća korišćenjem različitih modela. Proces je uključivao prikupljanje i čišćenje podataka, EDA, feature engineering, izgradnju i evaluaciju modela.

Najznačajniji faktori koji utiču na cenu su: ukupna površina (`TotalSF`), kvalitet kuće (`OverallQual`), broj garažnih mesta, površina iznad zemlje, starost kuće, kvalitet kuhinje i lokacija (`Neighborhood`). Kreiranjem novih promenljivih, poput `TotalFinishedSF`, `HouseAge`, `HouseRemodAge`, i binarnih promenljivih za podrum, garažu, kamin i bazen, unapređena je prediktivna moć modela.

Projekat se može dodatno unaprediti dodavanjem novih promenljivih, skaliranjem podataka za modele osetljive na veličinu vrednosti (npr. SVR) i preciznijim otkrivanjem outliera pomoću Z-score pristupa.

Rad pokazuje da pravilna analiza i priprema podataka, kvalitetan feature engineering i EDA igraju ključnu ulogu u preciznom predviđanju cena kuća. Linearna regresija se pokazala efikasnim i interpretabilnim pristupom, dok napredniji modeli mogu dodatno poboljšati performanse.
