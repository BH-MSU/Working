CarData <- read.table(url('http://archive.ics.uci.edu/ml/machine-learning-databases/car/car.data'),
                      sep=',',col.names=c('buying','maintenance','doors','persons','lug_boot','safety','rating'))
summary(CarData)
str(CarData)

logisticModel <- glm(rating!='unacc' ~ buying + maintenance + doors + persons + lug_boot +safety,
                     family=binomial(link = "logit"),data=CarData)
# rating!='unacc' MEANS:
# 2 categories in the response variable:
# 1. if rating != "unacc": 1
# 2. if rating = "unacc": 0

# link = "logit" MEANS: 
# 1. sum(coef) < 0: P(Y=1|X) < P(Y=0|X)
# 2. sum(coef) > 0: P(Y=1|X) > P(Y=0|X)

summary(logisticModel)
table(CarData$rating,predict(logisticModel,type='response')>=0.5)
#       FALSE TRUE
# acc      32  352
# good      0   69
# unacc  1166   44
# vgood     0   65
# acc: acceptable; unacc: unacceptable
#    FALSE TRUE
# 0   1166   44
# 1     32  484
# sensitivity = 484 / (32+484) = 0.9379845
# specificity = 1166/(1166+44) = 0.9636364

library(lattice)
densityplot(predict(logisticModel,type='link'),
            groups=CarData$rating!='unacc',auto.key=T)
sd(predict(logisticModel,type='link'))

densityplot(predict(logisticModel,type='response'),
            groups=CarData$rating!='unacc',auto.key=T)





