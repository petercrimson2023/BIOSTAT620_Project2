data(mtcars)

# 查看数据结构
str(mtcars)

# 转换数据以适用于泊松回归的例子
# 我们将hp视为响应变量，disp作为解释变量
# 因为泊松回归需要非负计数数据，我们确保hp列中没有负值或零
# 这里我们直接使用hp作为计数，这是为了演示目的

# 使用glm函数进行泊松回归，链接函数为log
poisson_model <- glm(hp ~ disp, data = mtcars, family = poisson(link = "log"))

# 查看模型的摘要
summary(poisson_model)

## qqplot

rstd = rstudent(poisson_model)
qqnorm(rstd)
qqline(rstd)

## 残差图

plot(poisson_model, which = 1)
