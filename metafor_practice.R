library(metafor)
library(here)
library(tidyverse)
DatRich <- read.csv(here("Gardner.csv"))

DatRich <- DatRich %>% 
  mutate(md = MeanvalueatHighFauna - MeanvalueatLowFauna)


# Pooled standard deviation 
DatRich <- DatRich %>% 
  mutate(s_pooled = sqrt((((HighFaunaN - 1) * SDatHighFauna^2) + ((LowFaunaN - 1) * SDatLowFauna^2)) / ((HighFaunaN - 1) + (LowFaunaN - 1))))

DatRich <- DatRich %>% 
  mutate(umd_se = s_pooled*(sqrt((1/HighFaunaN) +(1/LowFaunaN))))

DatRich <- DatRich %>% 
  mutate(smd_between = (MeanvalueatHighFauna - MeanvalueatLowFauna) / s_pooled)

DatRich <- DatRich %>% 
  mutate(j = (1 - (3/ (4*(HighFaunaN + LowFaunaN - 2) - 1))))

DatRich <- DatRich %>% 
  mutate(g = smd_between * j)

DatRich <- DatRich %>% 
  mutate(g_se = sqrt((HighFaunaN + LowFaunaN) / (HighFaunaN * LowFaunaN) + (g^2) / (2* (HighFaunaN + LowFaunaN))))

DatRich <- DatRich %>% 
  mutate(w = 1 / (g_se ^2))

DatRich <- DatRich %>% 
  mutate(gw = g * w)

DatRich <- DatRich %>% 
  mutate(pooled_effect_size = sum(gw) / sum(w))

DatRich <- escalc(n1i = HighFaunaN, n2i = LowFaunaN, m1i = MeanvalueatHighFauna, m2i = MeanvalueatLowFauna, sd1i = SDatHighFauna, sd2i = SDatLowFauna, data = DatRich, measure = "SMD", append = TRUE)

Rich.Overall <- rma(yi, vi, method = "FE", data = DatRich)
forest(Rich.Overall)

par(mfrow = c(2,1))
funnel(Rich.Overall, main = "Standard Error")
funnel(Rich.Overall, yaxis = "seinv", main = "Inverse Standard Error")
