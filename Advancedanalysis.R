if (!require(lme4)) install.packages('lme4')
library(lme4)
data(sleepstudy)
View(sleepstudy)
if(!require(ggplot2)) install.packages('ggplot2')
library(ggplot2)

ggplot(sleepstudy, aes(x= Days, y= Reaction)) + geom_point(
  colour = 'blue', alpha = 0.7) + geom_smooth(method = 
                                                'lm', colour = 'red')+
  theme_bw() + facet_wrap(~Subject)

ggplot(sleepstudy[1:50,], aes(x = Days, y=Reaction)) +
  geom_point(colour = 'blue', alpha = 0.7) +
  geom_smooth(method = 'lm', colour = 'black') +
  theme_bw() + facet_wrap(~subject)

boxplot(Reaction ~ Days, data = sleepstudy)

summary(null <- lmer(Reaction~1 + (1|Subject), 
                     data = sleepstudy))

if(!require(performance)) install.packages('performance')
library(performance)
performance::icc(null)

reg1 <- lm(Reaction~Days + Subject, data = sleepstudy)
summary(reg1)

reg1 <- lm(Reaction~Days + Subject+sex+age,
           data = sleepstudy)
if(!require(jtools)) install.packages('jtools')
library(jtools)
summ(reg1, confint = TRUE)

if(!require(plm)) install.packages('plm')
library(plm)
 Reg2 <- plm(Reaction~Days, data = sleepstudy,
             model = 'within', index = c('Subject'))
summary(Reg2)
fixef(Reg2)
confint(Reg2)

library(lme4)
if(!require(lme4)) install.packages('lme4')
library(lme4)
reg3 <- lmer(Reaction~Days + (1|Subject),
             data = sleepstudy)
summary(reg3)
coef(reg3)
library(jtools)
summ(reg3, confint=TRUE)

reg4 <- lmer(Reaction~Days + (Days | Subject),
             sleepstudy)
summary(reg4)
library(stargazer)
stargazer(Reg2,reg3,reg4, type = "text", ci=TRUE,keep.stat=c("rsq", "n"),
          add.lines=list(c("AIC", round(AIC(reg2),1), round(AIC(reg3),1),round(AIC(reg4),1)))) 

Reg2 <- plm(Reaction~Days, data = sleepstudy,
            model = 'within', index = c('Subject'))


AIC(Reg2, reg3, reg4)
if(!require(foreign)) install.packages('foreign')
library(foreign)


diabetes <- read.spss("C:/Users/XPS/Downloads/diabetes.sav",
                      to.data.frame = T)

d <- diabetes
if(!require(dplyr)) install.packages('dplyr')
library(dplyr)
 d <- diabetes %>% 
   dplyr::select(total_cholesterol,age,gender,weight)
names(d)
if(!require(missForest)) install.packages('missForest')
library(missForest)
d.na <- prodNA(d, noNA = 0.3)
head(d, n=10)
head(d.na, n=10)
if(!require(mice)) install.packages('mice')
library(mice)
md.pattern(d.na)

md.pattern(d)
md.pairs(d.na)

summary(reg.true <- lm(total_cholesterol ~ weight +
                         age + gender, data = d))


if(!require(mice)) install.packages("mice")
library(mice)
if(!require(foreign)) install.packages('foreign')
library(foreign)

summary(reg.del <- lm(total_cholesterol~weight+age+gender,
                      data = d.na))
summary(reg.del<- lm(total_cholesterol~weight+age+gender,
                     data = d))

nobs(reg.del)
NewData <- d %>%
  dplyr::select(age,weight,gender)
EvenNewerData <- na.omit(NewData)
NewData1 <- d.na %>%
  dplyr::select(total_cholesterol,age,weight)
EvenNewerData1 <- na.omit(NewData1)
summary(reg.new <- lm(total_cholesterol~weight+age,
                      data = EvenNewerData1))

summary(d.na)
head(d.na)
d.na.mean <- d.na
d.na.mean$total_cholesterol <- ifelse(is.na(d.na$total_cholesterol),
                                      mean(d.na$total_cholesterol,
                                           na.rm=T),
                                      d.na.mean$total_cholesterol)
d.na.mean$age <- ifelse(is.na(d.na$age), mean(d.na$age, na.rm=T),
                        d.na.mean$age)
d.na.mean$weight <- ifelse(is.na(d.na$weight), mean(d.na$weight,
                                                    na.rm=T),
                           d.na.mean$weight)
d.na.mean[which(is.na(d.na.mean$gender)),]$gender<-1
head(d.na.mean, n=10)

summary(reg.mean <- lm(total_cholesterol~weight+age+gender,
                       data = d.na.mean))


if(!require(mice)) install.packages('mice')
library(mice)

NewData  <- d.na %>%
  dplyr::select(total_cholesterol,age,weight)
EvenNewerData <- na.omit(NewData)

summary(reg.new <- lm(total_cholesterol~weight+age,
                      data = EvenNewerData))
library(mice)
d.na.mice <- mice(d.na)
class(d.na.mice)
temp <- complete(d.na.mice, 1)
head(temp, n=10)

reg.mice <- with(d.na.mice, lm(total_cholesterol~weight+age+gender))
reg.mice.pooled <- pool(reg.mice)
summary(reg.mice.pooled)

d.na.mice
reg.mice.pooled

demo1 <- mtcars
demo1$cyl[demo1$cyl==8] <- NA
View(demo1)

nrow(d)
d5 <- d[sample(nrow(d), 20),]
reg.true <- lm(total_cholesterol~weight+age+gender, data = d)
summary(reg.true)

confint(reg.true)

summary(reg.lim <- lm(total_cholesterol~weight+age+gender,
                      data = d5))

confint(reg.lim)
library(car)
if(!require(car)) install.packages('car')
library(car)

reg.lim <- lm(total_cholesterol~weight+age+gender,
              data = d5)
reg.boot <- Boot(reg.lim, R=10000)

print(confint(reg.boot, level = .95, type='norm'))

hist(reg.boot)
summary(reg.boot)














