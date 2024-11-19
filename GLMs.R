# Modelling count data using Generalised Linear Models
# 环境搭建
library(tidyverse)
library(car)

# 1.数据加载
# -------------------------------------------------------
EIA <- read.csv('EIA.csv')
EIA$impact <- as.factor(EIA$impact)
EIA$MonthOfYear <- as.factor(EIA$MonthOfYear)
EIA$Year <- as.factor(EIA$Year)
attach(EIA)  # 将数据框（data frame）中的变量直接导入到 R 的工作环境中

# 2.Initial Model Fitting 初始模型拟合
# -------------------------------------------------------
fit.pois.sqrt <- glm(
  count ~ tidestate + observationhour + DayOfMonth + MonthOfYear + 
    Year + x.pos + y.pos + Year:x.pos + Year:y.pos,
  data = EIA,
  family = poisson(link = 'sqrt')
)

fit.pois <- glm(
  count ~ tidestate + observationhour + DayOfMonth + MonthOfYear + 
    Year + x.pos + y.pos + Year:x.pos + Year:y.pos,
  data = EIA,
  family = poisson
)


# 3.
# -------------------------------------------------------