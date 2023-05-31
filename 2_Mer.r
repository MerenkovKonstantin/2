
rm(list=ls())
library("tidyverse") 
library("readr")     
library("stringr")   
library("dplyr")     
library("ggplot2") 
library("tidyr")
library("stringr")
library("lubridate")

#открываем таблицу и убираем NA
tbl = read_csv("eddypro.csv", skip = 1, na =c("","NA","-9999","-9999.0"), comment=c("["))

#убираем первую строку и производим некоторые манипуляции с таблицей (названия, символы)
tbl = tbl[-1,]; tbl
tbl = select(tbl, -(roll))
tbl = tbl %>% mutate_if(is.character, factor)
names(tbl) = names(tbl) %>% 
  str_replace_all("[!]","_emph_") %>% 
  str_replace_all("[?]","_quest_") %>% 
  str_replace_all("[*]","_star_") %>% 
  str_replace_all("[+]","_plus_") %>%
  str_replace_all("[-]","_minus_") %>%
  str_replace_all("[@]","_at_") %>%
  str_replace_all("[$]","_dollar_") %>%
  str_replace_all("[#]","_hash_") %>%
  str_replace_all("[/]","_div_") %>%
  str_replace_all("[%]","_perc_") %>%
  str_replace_all("[&]","_amp_") %>%
  str_replace_all("[\\^]","_power_") %>%
  str_replace_all("[()]","_") 
glimpse(tbl)

#выбираем весенний период и ночь
tbl = filter(tbl,DOY >= 1 & DOY <= 365)
tbl = filter(tbl, daytime ==FALSE)

#отбираем числовые данные, проводим корелляционный анализ и выбираем необходимые переменные
tbl_numeric = tbl[,sapply(tbl,is.numeric) ]
cor_td = cor(drop_na(tbl_numeric), method = "spearman") %>% as.data.frame %>% select(h2o_flux)
vars = row.names(cor_td)[cor_td$h2o_flux^2 > 0.2] %>% na.exclude

#формула для моделей
formula = as.formula(paste("h2o_flux~", paste(vars,collapse = "+"), sep=""));formula

row_numbers = 1:length(tbl_numeric$h2o_flux)
teach = sample(row_numbers, floor(length(tbl_numeric$h2o_flux)*.7))
test = row_numbers[-teach]

teaching_tbl = tbl_numeric[teach,]
testing_tbl = tbl_numeric[test,]

#строим первую модель
model0=lm(h2o_flux ~ (H  + rand_err_co2_flux + co2_molar_density + 
                        co2_mole_fraction + co2_mixing_ratio + T_star_ + un_H + un_co2_flux + 
                        w_div_ts_cov + w_div_co2_cov + co2...125 + co2...127), data = teaching_tbl)

#вывод всех коэффициентов, сводка и дисперсионный анализ модели
coef(model0)
resid(model0)
confint(model0)
summary(model0)
anova(model0)

#строим вторую модель
model1=lm(h2o_flux ~ (H  +
                        co2_mole_fraction + T_star_ + un_H 
                        ), data = teaching_tbl)

#вывод всех коэффициентов, сводка и дисперсионный анализ модели
coef(model1)
resid(model1)
confint(model1)
summary(model1)
anova(model1)
anova(model0, model1)

