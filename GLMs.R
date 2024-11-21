# Modelling count data using Generalised Linear Models
# 环境搭建
library(tidyverse)
library(car)
library(MuMIn)

# 1.数据加载
# -------------------------------------------------------
EIA <- read.csv('EIA.csv')
EIA$impact <- as.factor(EIA$impact)
EIA$MonthOfYear <- as.factor(EIA$MonthOfYear)
EIA$Year <- as.factor(EIA$Year)
attach(EIA)  # 将数据框（data frame）中的变量直接导入到 R 的工作环境中

# 2.Initial Model Fitting 初始模型拟合
# -------------------------------------------------------
# 平方根链接函数
fit.pois.sqrt <- glm(
  count ~ tidestate + observationhour + DayOfMonth + MonthOfYear + 
          Year + x.pos + y.pos + Year:x.pos + Year:y.pos,
  data = EIA,
  family = poisson(link = 'sqrt')
)

# 对数链接函数
fit.pois <- glm(
  count ~ tidestate + observationhour + DayOfMonth + MonthOfYear + 
          Year + x.pos + y.pos + Year:x.pos + Year:y.pos,
  data = EIA,
  family = poisson
)

# 对数链接函数&面积偏移量
fit.pois.offset <- glm(
  count ~ tidestate + observationhour + DayOfMonth + MonthOfYear + 
    Year + x.pos + y.pos + Year:x.pos + Year:y.pos,
  data = EIA,
  family = poisson,
  offset = log(area)
)

vif(fit.pois.offset)



# 3.模型选择
# -------------------------------------------------------
# AIC方法比较3个模型
AIC(fit.pois.sqrt,fit.pois,fit.pois.offset)

# step函数--AIC准则
stepAIC.pois.offset <- step(fit.pois.offset,direction = "both")

# step函数--BIC准则
n = nrow(EIA)
stepBIC.pois.offset <- step(fit.pois.offset,direction = "both",k = log(n))

# ANOVA函数--方差分析
anova(stepBIC.pois.offset)
Anova(stepBIC.pois.offset)

# 重新拟合模型，明确指定na.action
stepBIC.pois.offset2 <- glm(count ~ tidestate + observationhour + MonthOfYear + 
                              Year + x.pos + y.pos + Year:x.pos + Year:y.pos,
                            data = EIA,
                            family = poisson,
                            offset = log(area),
                            na.action = na.fail)  # 明确指定na.action

# 然后执行dredge
library(MuMIn)
options(na.action = "na.fail")  # 设置全局选项
dredge_results <- dredge(stepBIC.pois.offset2)

# 预测
newdata <- data.frame(
  tidestate = "EBB",
  observationhour = 10,
  MonthOfYear = factor(1, levels = levels(EIA$MonthOfYear)),  
  Year = factor(11, levels = levels(EIA$Year)), 
  x.pos = -2061,
  y.pos = -1158,
  area = mean(EIA$area)
)

predict_value <- predict(stepBIC.pois.offset, newdata = newdata, type = "link")
real_predict_value <- round(exp(predict_value),3)

# round(predict(stepBIC.pois.offset, newdata = newdata, type = "response"),3)

# 1月和5月预测数量的比率
newdata_2 <- newdata
newdata_2$MonthOfYear = factor(1, levels = levels(EIA$MonthOfYear))
newdata_3 <- newdata
newdata_3$MonthOfYear = factor(5, levels = levels(EIA$MonthOfYear))

ratio <- round(
         (predict(stepBIC.pois.offset, newdata = newdata_2, type = "response")/
         predict(stepBIC.pois.offset, newdata = newdata_3, type = "response"))
         ,3)

# 4.过度分散
# -------------------------------------------------------
stepBIC.pois.offset.OD <- glm(count ~ tidestate + observationhour + MonthOfYear + 
                              Year + x.pos + y.pos + Year:x.pos + Year:y.pos,
                              data = EIA,
                              family = quasipoisson,
                              offset = log(area))

# 5.模型诊断
# -------------------------------------------------------
final_model <- glm(count ~ tidestate + observationhour + MonthOfYear + Year + x.pos +  y.pos,
                   family = quasipoisson,
                   data = EIA,
                   offset = log(area))

# I.残差图
residualPlots(stepBIC.pois.offset.OD)

# II.观测值 vs 拟合值
fitted_values <- fitted(stepBIC.pois.offset.OD)
observed_values <- EIA$count

plot(fitted_values, observed_values, # 创建散点图
     xlab = "Fitted Values",
     ylab = "Observed Values",
     main = "Observed vs Fitted Values") 

# III.均值-方差关系
# 获取拟合值和皮尔逊残差
fitted_values <- fitted(stepBIC.pois.offset.OD)
pearson_residuals <- residuals(stepBIC.pois.offset.OD, type = "pearson")

# 基本散点图
plot(fitted_values, pearson_residuals,
     xlab = "Fitted Values",
     ylab = "Pearson Residuals",
     main = "Residuals vs Fitted Values")
abline(h = 0, col = "red", lty = 2)

# 添加平滑线来帮助识别趋势
lines(lowess(fitted_values, pearson_residuals), col = "blue")

# 分箱分析
# 创建分箱数据
binned_data <- data.frame(
  fitted = fitted_values,
  residuals = pearson_residuals
) %>%
  mutate(bin = cut(fitted, breaks = 30)) %>%
  group_by(bin) %>%
  summarise(
    mean_fit = mean(fitted),
    var_resid = var(residuals)
  )

# 绘制分箱后的方差关系
plot(var_resid ~ mean_fit, data = binned_data,
     xlab = "Mean (Binned)",
     ylab = "Variance of Residuals",
     main = "Mean-Variance Relationship (Binned)")
abline(0, 1, col = "red", lty = 2)  # 添加参考线


# IV.ACF图--自相关性

# 如果需要按网格代码排序
# 先创建带网格码的数据框
res_df <- data.frame(
  GridCode = EIA$GridCode,
  residuals = pearson_residuals
)
# 按网格码排序
ordered_residuals <- res_df[order(res_df$GridCode), "residuals"]

# 绘制ACF图
par(mfrow=c(1,2))

# 原始排序的ACF图
acf(pearson_residuals, main="ACF of Original Order")

# 按gridcode排序后的ACF图
acf(ordered_residuals, main="ACF of Gridcode Order")

par(mfrow=c(1,1))








