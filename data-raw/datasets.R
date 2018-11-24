library(dplyr)
library(tidyr)
library(readr)
source("R/zzz.R", encoding = "UTF-8")

# Krämer 2014 ####
dat.kramer <-
  "10 3 2
11 0 4
12 6 5
13 5 6
14 6 5
15 9 2
16 8 9
17 7 6
18 7 5
19 7 6
20 9 4
21 13 9
22 8 6
23 9 11
24 10 7
25 12 6
26 9 5
27 12 4
28 8 6
29 8 8
30 10 8" %>%
  read_delim(., delim = " ", col_names = c("age", "Male", "Female")) %>%
# 9 st mellan 18 - 19 år har moget knä, vet dock inte hur många
  select(-Female) %>% # tar bort kvinnor
  rename(n = Male) %>%
  filter(n > 0) %>%
  mutate(
    moget = ifelse(age <= 17, 0L, NA),
    moget = ifelse(age >= 20, n, moget),
    moget = ifelse(age == 18, 4, moget), # antagande
    moget = ifelse(age == 19, 5, moget), # antagande
    omoget = n - moget,
    p = moget / n,
    age = age + 0.5 # antagande
  ) %>%
  filter(!(age < 20 & age > 18)) %>%
  bind_rows(tibble(age = 19, n = 14, moget = 9, omoget = 5, p = moget/n)) %>%
  arrange(age)

# Saint-Martin 2015 (knäled) ####
dat.saintmartin <-
  "Group 14 15 16 17 18 19 20
omoget 37 45 35 31 32 27 7
moget 0 0 0 0 15 18 6"  %>%
  read_delim(., delim = " ") %>%
  gather(age, n, -Group) %>%
  spread(Group, n) %>%
  mutate(
    age = as.numeric(age) + 0.5, # antagande
    p = moget / (moget + omoget),
    n = moget + omoget
  ) %>%
  select(age, n, moget, omoget, p) %>%
  arrange(age)

# Krämer 2014 + Saint-Martin 2015 (knäled) ####
dat.kna <-
  bind_rows(
    dat.kramer,
    dat.saintmartin
  ) %>%
  group_by(age) %>%
  summarise(
    n = sum(n),
    moget = sum(moget),
    omoget = sum(omoget)) %>%
  mutate(p = moget / n)

# Simonsson 2017 (visdomstand) ####
dat.tand <-
  read_csv("data-raw/simonsson2017_males.csv") %>%
  rename(age = Age) %>%
  mutate_all(na_to_zero) %>%
  gather(Stages, n, -age) %>%
  mutate(Stages = ifelse(Stages  == "H", "moget", "omoget")) %>%
  group_by(age, Stages) %>%
  summarise(n = sum(n)) %>%
  ungroup %>%
  spread(Stages, n) %>%
  mutate(
    age = as.numeric(age) + 0.5,
    p = moget / (moget + omoget),
    n = moget + omoget
  ) %>%
  select(age, n, moget, omoget, p)

# Thodberg 2017 ####
dat.hand <-
  tibble(
    age = 15:20,
    mean_ba = c(14.69, 15.82, 17.01, 17.85, 18.50, 18.79),
    sd_ba = c(0.99, 1.12, 1.15, 1.02, 0.78, 0.67)
    # 18.432 ger centrerad ålder 19 år
    # 18.873 ger centrerad ålder 20 år
  ) %>%
  mutate(
    # andel med mogen handled givet faktisk ålder, centrerade vid 18, 19
    # respektive 20 år
    p_18 = 1 - pnorm(17.828, mean = mean_ba, sd = sd_ba),
    p_19 = 1 - pnorm(18.432, mean = mean_ba, sd = sd_ba),
    p_20 = 1 - pnorm(18.873, mean = mean_ba, sd = sd_ba)
  )


# Logistisk regression ####
# regressionskoefficienter
coef.kna <- glm(formula = cbind(moget, omoget) ~ age, family = binomial,
                  data = dat.kna) %>% coef

names(coef.kna) <- c("intercept", "slope")

coef.tand <- glm(formula = cbind(moget, omoget) ~ age, family = binomial,
                data = dat.tand) %>% coef

names(coef.tand) <- c("intercept", "slope")

coef.hand.18 <- coef(glm(formula = p_18 ~ age, family = quasibinomial,
                      data = dat.hand))

names(coef.hand.18) <- c("intercept", "slope")

coef.hand.19 <- coef(glm(formula = p_19 ~ age, family = quasibinomial,
                         data = dat.hand))

names(coef.hand.19) <- c("intercept", "slope")

coef.hand.20 <- coef(glm(formula = p_20 ~ age, family = quasibinomial,
                         data = dat.hand))

names(coef.hand.20) <- c("intercept", "slope")

# dataset med de logistiska regressionskoefficienterna
dat.coef <-
  bind_rows(coef.kna, coef.tand, coef.hand.18, coef.hand.19, coef.hand.20) %>%
  mutate(method = c("Knä", "Tand", "Hand-18", "Hand-19", "Hand-20"))

# Sparar datasets ####
logcoef <- dat.coef %>% as.data.frame
tand <- dat.tand %>% as.data.frame
kna <- dat.kna %>% as.data.frame
hand <- dat.hand %>% select(age, mean_ba, sd_ba)  %>% as.data.frame

save(logcoef, tand, kna, hand, file = "data/datasets.rda")
save(coef.kna, coef.tand, coef.hand.18, coef.hand.19, coef.hand.20, file = "R/sysdata.rda")
