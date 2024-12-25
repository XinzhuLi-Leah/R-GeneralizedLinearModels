#logistic回归
data(Affairs, package="AER")
table(Affairs$affairs)
#将情感危机这个因子转为二分类 只要有过就是1 没有就是0

Affairs$affair[Affairs$affairs > 0] <- 1
Affairs$affair[Affairs$affairs == 0] <- 0
#转化为因子
Affairs$affair <- factor(Affairs$affair, levels=c(0,1),labels=c("No","Yes"))
table(Affairs$affair)

attach(Affairs)
fit1 <- glm(affair ~ gender + age + yearsmarried + children + religiousness + education + occupation +rating,data=Affairs,family=binomial())
summary(fit1)
#然后只把显著的变量放进模型
fit2 <- glm(affair ~ age + yearsmarried + religiousness + rating, data=Affairs, family=binomial())
summary(fit2)
# compare models 嵌套模型
anova(fit1, fit2, test="Chisq")
#p值大于0.05 简单模型 fit2 与 fit1 没有显著差异，可能可以优先考虑简单模型

exp(coef(fit2))
##	•	如果 exp(coef) 大于 1，表示该变量的增加会增加响应变量为 1 的可能性；
# •	如果小于 1，表示该变量的增加会降低响应变量为 1 的可能性。

#测试数据 用这种办法 可以预测每一个预测变量对于结果概率的影响
testdata <- data.frame(rating = c(1, 2, 3, 4, 5),age = mean(Affairs$age),yearsmarried = mean(Affairs$yearsmarried),religiousness = mean(Affairs$religiousness))
testdata$prob <- predict(fit2, newdata=testdata, type="response")
testdata

testdata <- data.frame(rating = mean(Affairs$rating),age = seq(17, 57, 10), yearsmarried = mean(Affairs$yearsmarried),religiousness = mean(Affairs$religiousness))
testdata$prob <- predict(fit2, newdata=testdata, type="response")
testdata
#查看 testdata 数据框，新增的 prob 列会显示预测概率，即给定这些自变量的情况下，affair 为 “Yes” 的概率。



#泊松回归
data(breslow.dat, package="robust")
names(breslow.dat)
summary(breslow.dat[c(6, 7, 8, 10)])
attach(breslow.dat)
# fit regression
#假设我们希望基础，分析治疗和年龄对哮喘发作次数的影响，可以使用泊松回归模型
fit <- glm(sumY ~ Base + Age + Trt, data=breslow.dat, family=poisson)
#	•	sumY：随访期间的总哮喘发作次数（这是计数变量，适合使用泊松回归建模）。
summary(fit)

# interpret model parameters
coef(fit)
#age的系数是0.0227 表明保持其他变量不变时，年龄增加一岁，癫痫发病数的对数均值将增加0.02
#而通常我们会关注发病数而并非发病数的均值对数，所以我们系数指数化
exp(coef(fit))
#表明保持其他变量不变，年龄增加一岁，期望的癫痫发病数将乘以1.0229