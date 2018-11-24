Bilaga: agedecision
================
Mikael Elenius
2018-11-24

Introduktion
============

Detta R-paket utgör en bilaga till uppsatsen:

[M. Elenius (2018). *Beslutsanalys av medicinska åldersbedömningar inom asylprocessen*.](http://www.diva-portal.org/)

Installation
============

Observera ifall du använder Windows behöver du ha installerat [Rtools](https://cran.r-project.org/bin/windows/Rtools/).

``` r
if (!require("devtools")) {
  install.packages("devtools")
}
devtools::install_github("elenius/agedecision", dependencies = TRUE)

library(agedecision)
```

Resultat
========

Specificitet och sensitivitet i åldersintervallet från 15 till 21 år ges av:

``` r
library(agedecision)
spec_sens_all(age.min = 15, age.max = 21)
        method specificity sensitivity
1      Hand-18   0.8396955   0.8397065
2      Hand-19   0.9196535   0.6129775
3      Hand-20   0.9563853   0.3975878
4         Tand   0.8413553   0.7501116
5          Kna   0.9707983   0.5028349
6          RMV   0.8569327   0.7937823
7 Singla slant   0.5000000   0.5000000
8         Barn   1.0000000   0.0000000
9        Vuxen   0.0000000   1.0000000
```

Andel som klassificeras som vuxna, både korrekt och felaktigt för samtliga metoder då prevalensen varieras från 0 till 1 i skalsteg om 0.01. Dessutom anges accuracy (andel korrekt klassificerade). Även specificitet och sensitivitet anges, även om dessa är oberoende av prevalensen. Åldersintervallet anges även här från 15 till 21 år.

``` r
dat <- classified_adults(by = 0.01, age.min = 15, age.max = 21)

# Resultat prevalens = 0.84
print.data.frame(subset(dat, prevalence == 0.84), row.names = FALSE)
       method specificity sensitivity prevalence  accuracy
      Hand-18   0.8396955   0.8397065       0.84 0.8397048
      Hand-19   0.9196535   0.6129775       0.84 0.6620456
      Hand-20   0.9563853   0.3975878       0.84 0.4869954
         Tand   0.8413553   0.7501116       0.84 0.7647106
          Kna   0.9707983   0.5028349       0.84 0.5777091
          RMV   0.8569327   0.7937823       0.84 0.8038864
 Singla slant   0.5000000   0.5000000       0.84 0.5000000
         Barn   1.0000000   0.0000000       0.84 0.1600000
        Vuxen   0.0000000   1.0000000       0.84 0.8400000
 classified_adults
         0.7310022
         0.5277565
         0.3409521
         0.6554769
         0.4270536
         0.6896679
         0.5000000
         0.0000000
         1.0000000
```

Beräkning av samtliga förväntade nyttor då nyttan för en felklassificerad vuxen och prevalensen varieras från 0 till 1 i skalsteg om 0.01, vilket ger upphov till 10 201 kombinationer för varje metod (9 st) och nyttofunktioner (2 st), vilket ger upphov till 183 618 olika resultat. Beräkningarna tar här ungefär en halv minut att utföra.

``` r
system.time(resultat <- eu_all_comb(by = 0.01, age.min = 15, age.max = 21))
   user  system elapsed 
 34.319   0.278  35.302 

# Antal rader = 10201 * 9 * 2
format(nrow(resultat), big.mark = " ")
[1] "183 618"

# Resultat då Uvo = 0.5 och prevalens = 0.84
print.data.frame(subset(resultat, uvo == 0.5 & prevalence == 0.84), row.names = FALSE)
         E prevalence uvo       method nyttomodell
 0.9070280       0.84 0.5      Hand-18     diskret
 0.9758323       0.84 0.5      Hand-18      linjär
 0.8245951       0.84 0.5      Hand-19     diskret
 0.9397635       0.84 0.5      Hand-19      linjär
 0.7400085       0.84 0.5      Hand-20     diskret
 0.8935275       0.84 0.5      Hand-20      linjär
 0.8696637       0.84 0.5         Tand     diskret
 0.9587507       0.84 0.5         Tand      linjär
 0.7865184       0.84 0.5          Kna     diskret
 0.9246053       0.84 0.5          Kna      linjär
 0.8904978       0.84 0.5          RMV     diskret
 0.9711486       0.84 0.5          RMV      linjär
 0.7100000       0.84 0.5 Singla slant     diskret
 0.8550000       0.84 0.5 Singla slant      linjär
 0.5800000       0.84 0.5         Barn     diskret
 0.7900000       0.84 0.5         Barn      linjär
 0.8400000       0.84 0.5        Vuxen     diskret
 0.9200000       0.84 0.5        Vuxen      linjär
```

Funktioner och dataset
======================

Se dokumentationen för dataset och funktioner med hjälp av `?`

``` r

# Dataset
?tand # data för visdomstand
?hand # data för handled
?kna # data för knäled
?logcoef # samtliga regressionskoefficienter

# Täthetsfunktion (två likformiga fördelningar)
?dunif2

# Sannolikhetsfunktioner
?logistic_function # logistisk funktion
?logistic_center # ger den centrerade åldern av en logistisk funktion
?pM # listar samtliga sannolikhetsfunktioner

# Nyttofunktioner
?utility_discrete_adult 
?utility_discrete_child
?utility_linear_adult
?utility_linear_child

# Förväntad nytta
?eu_i
?eu_all
?eu_all_comb

# Resultat specificitet, sensitivitet, accuracy, andel klassificerade vuxna
?spec_sens
?spec_sens_all
?classified_adults
```

Exempel regressionsanalys
=========================

Resultatet av de regressionsanalyser som utförts ges av regressionskoefficienterna i datasetet `?logcoef` och av sannolikhetsfunktionerna `?pM`. Här visas exempel på hur regressionen går till för visdomstand och knäled. Se också [data-raw/datasets.R](data-raw/datasets.R) där datainläsning och regressionsanalyser görs för samtliga metoder.

``` r
# Visdomstand (Simonsson m. fl. (2017))
mod.tand <- glm(formula = cbind(moget, omoget) ~ age, family = binomial, data = tand)
coef(mod.tand)
(Intercept)         age 
 -19.903828    1.087887 

# Knäled (Krämer m. fl. (2014), Saint-Martin m. fl. (2015))
mod.kna <- glm(formula = cbind(moget, omoget) ~ age, family = binomial, data = kna)
coef(mod.kna)
(Intercept)         age 
 -26.707824    1.370405 
```

Referens
========

Om du använder programkoden, ange gärna nedanstående referens:

``` r
citation("agedecision")

  Mikael Elenius (2018). Beslutsanalys av medicinska
  åldersbedömningar inom asylprocessen. Magisteruppsats i Beslut-,
  risk- och policyanalys. Högskolan i Gävle.

A BibTeX entry for LaTeX users is

  @MastersThesis{,
    title = {{Beslutsanalys av medicinska åldersbedömningar inom asylprocessen}},
    author = {Mikael Elenius},
    year = {2018},
    school = {{Högskolan i Gävle}},
    url = {https://github.com/elenius/agedecision},
  }
```
