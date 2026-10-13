
# edx R data analysis training 2026
## Data wrangling, we use mutate, filter, select & pipe operator

library(dplyr)
library(dslabs)
data('murders')
murders <- mutate(murders, rate=total/population*100000)
head(murders)
filter(murders, rate<=0.71)
new_table <- select(murders, state,region,rate)
filter(new_table, rate<=0.71)
murders %>% select(state,region,rate) %>% 
  filter(rate<=0.71)
## Creating data frame
grades <- data.frame(names=c('Abe','Bety','Kony','Juju'),
                     exam1=c(95,55,65,100),
                     exam2=c(90,45,77,90))

grades
class(grades$names)

## summarize, group_by and arrange functions

library(tidyverse)
s <- murders %>% 
  filter(region=='West') %>% 
  summarise(minimum=min(rate),
            medium=median(rate),
            maximum=max(rate))
s
us_murder_rate <- murders %>% 
  summarise(rate=sum(total)/sum(population)*100000)
us_murder_rate
quantile(x, c(0,0.5,1))
murders %>% filter(region=='West') %>% 
  summarize(range=quantile(rate,c(0,0.5,1)))
class(us_murder_rate)
# since us murder rate stored as data frame, in order to make
# it numeric, we use the pull function
us_murder_rate %>% pull(rate)
# or if we want the whole operation as numeric as follow
us_murder_rate <- murders %>% 
  summarize(rate=sum(total)/sum(population)*10^5) %>% 
  pull(rate)
# dot as a place holder (.$rate) for the above eg
us_murder_rate
class(us_murder_rate)
# group_by followed by summarize function is the most used operation

murders %>% group_by(region)
murders %>% group_by(region) %>% 
  summarise(median=median(rate))
# sorting data, eg to sort by pop size or rate as below
# note that the default is in ascending order
murders %>% arrange(population) %>% head()
murders %>% arrange(rate) %>% head()
# if want it in descending order, then as below
murders %>% arrange(desc(rate)) %>% head()
# if we want orders b/n two column, the 2nd break the ties
murders %>% arrange(region,rate) %>% head()
murders %>% top_n(10, rate)  # but this is not ordered
murders %>% arrange(desc(rate)) %>% top_n(10) # ordered now

### data.table (very popular in data analysis)
library(data.table)
# data frame should be converted as follows
murders<- setDT
murders[order(population)]|>head()
murders %>% group_by(region)


library(dplyr)
library(dslabs)
data(murders)
library(data.table)
murders <- setDT(murders)
murders <- mutate(murders, rate = total/population *100000)
murders [, rate := total/population *10^5]
 # sebsetting in dplyr
filter(murders, rate <=0.7)

# subsetting in data.table
murders [rate<=0.7]
murders[rate <=0.7, .(state,rate)]
 # combining filter & select in dplyr
murders %>% filter(rate <=0.7) %>% select(state, rate)

data(heights)
heights <- setDT(heights)

# summarie in dplyr
s <- heights %>% 
  summarize(average=mean(height), standard_deviation=sd(height))

# summarize in data.table
s <- heights[, .(average=mean(height), standard_deviation=sd(height))]
heights[, .(median_min_max(height))]

murders[order(population)] |> head()
murders[order(region, rate)]

## tibble
murders %>% group_by(region)

library(dslabs)
data(heights)
library(tidyverse)
library(dplyr)
s <- heights %>% 
  summarize(average=mean(height), standard_dev=sd(height))
s
heights <- mutate(heights, average= mean(height))
index <- heights$height > 68.3
sum(index)
sex <- heights$sex =='Female'
height <- heights$height > 68.3
index <- sex & height
sum(index)
proportion <- heights$sex == 'Female'
sum(proportion)
summary(heights)
index <- heights$height==50
sum(index)
index <- heights$height==50
sex[index]
library(dslabs)
data(heights)
head(heights)
dim(heights)

heights[5, ]
heights[,2]
heights[c(2,5,6), ]
heights[, c(1,2)]

heights[,"height"]
heights$height
heights$sex =='Male'
heights[heights$sex=='Male', ]
heights[heights$height >70, ]
summary(heights)
heights[heights$height==50, ]
str(heights)
## suppose I want both e.g. male and >70 (use &)

heights[heights$sex=='Male'& 
          heights$height>70, ]
## now we want either male or taller than 72 (use |)
heights[
  heights$sex=='Male'|
    heights$height>72, 
]
heights[!(heights$sex=='Male'), ]
heights$height %in% c(65,66,67)
heights[heights$height %in% c(65,66,67), ]

## instead of the row itself, we wanted the row number
which(heights$height>75)   # showing the row position

## using which for indexing
heights[which(heights$height > 75), ]
heights[order(heights$height),]

# the top 10 tallest people 
head(heights[order(-heights$height),], 10)
# the shortest 10 people
head(heights[order(heights$height),], 10)

subset(heights, sex=='Male')
subset(heights, sex=='Female' & height>75)

# find the top 5 female tallest
female <- heights[heights$sex=='Female',]

female_sorted <- female[order(-female$height),]
head(female_sorted, 5)

# how many males taller than 72inches

sum(heights$sex=='Male' & heights$height>72)

x <- c('Male','Femal')
match(heights$height==50, x)

library(dslabs)
library(tidyverse)
data(heights)
head(heights)
heights$sex=='Male'
heights[heights$sex=='Male',]
heights[heights$height>70,]
heights[10,]
heights[20:30,]
heights[,'sex']
heights[heights$height>72,]
heights[heights$sex=='Female',]
heights[1:100,]

heights$height>70
heights[heights$height>70,]
## suppose we want male or people taller than 72 (or condition)
heights[heights$sex=='Male'|heights$height>72,]

heights[!heights$sex=='Male',]
# is equivalent to heights[heights$sex=='Female]

# people not taller than 70, using !
heights[!(heights$height>70),]
 heights$height %in% c(65,66,67)
heights[heights$height %in% c(65,66,67),]

which(heights$height>75)   # result is row numbers

heights[which(heights$height>75),] 

# suppose we wanted the data sorted from shortest to tallest

heights[order(heights$height),]
heights[order(-heights$height),] # from tallest to shortest
# top 10 tallest people will be ..
head(heights[order(-heights$height),],10)
# top 10 shortest people
head(heights[order(heights$height),],10)
 # using subset function

subset(heights, sex=='Male') #instead of...indexing seen before
subset(heights, sex=='Female'& height>65)
subset(heights, sex=='Male'& order(heights$height))
min(heights$height)
max(heights$height)
heights2 <- heights %>% 
  mutate(heights, ht_cm=height*2.54)
heights2
heights2[18,]
mean(heights2$ht_cm)
female_df <- filter(heights2, sex=='Female')
female_df
mean(female_df$ht_cm)

### CONDITIONALS (if, else, ifelse, any, all)

if(a!=0) {
  print (1/a)
} else{
  print ("No reciprocal for 0.")
}
library(dslabs)
data("murders")
murder_rate <- murders$total/murders$population * 100000
ind <- which.min(murder_rate)
if(murder_rate[ind] < 0.5){
  print(murders$state[ind])
  }else{
    print("No state has murder rate that low")
    }
if(murder_rate[ind] < 0.25){
  print(murders$state[ind])
} else{
  print("No state has murder rate that low")
}
a <- 0
ifelse(a>0,1/a,NA)

data("na_example")
sum(is.na(na_example))
no_nas <- ifelse(is.na(na_example), 0, na_example)
sum(is.na(no_nas))
 z <- c(TRUE, TRUE, FALSE)
any(z)
all(z)

library(dslabs)
data("heights")
sex_code <- ifelse(heights$sex=='Female', 1, 2)
sum(sex_code)
ifelse(heights$sex=='Female', 1, 2)
sum(heights$sex=='Female')
sum(heights$sex=='Male')
ht_code <- ifelse(heights$height>72, heights$height, 0)
mean(ht_code)
  ## basic function

inch_to_ft <- function(x){
  inch<-length(x)
  x/12
}
inch_to_ft(144)
height_2 <- heights %>% 
  mutate(height_ft = inch_to_ft(heights$height))
height_2 %>% filter(height_ft<5)

compute_s_n <- function(n){
  x <- 1:n
  sum(x)
}
compute_s_n(3)

 ## For loops
for (i in 1:5) {
  print(i)
  
}
data("heights")
# write an ifelse statement, that returns 1 if sex is female
# and 2 if sex is male
s <- ifelse(heights$sex=='Female', 1, 2)
sum(s)
# write ifelse statement, that takes height column & returns
# the height if greater than 75 and returns 0 otherwise
ht <- ifelse(heights$height>75, heights$height, 0)
mean(ht)

x <- 25
s <- 5
test <- function(x){
  s<-1/x
  s
}
test(x)

test <- vector(length=5)
for (i in 1:5){
  test[i]<- i^2
}
test[i]

   ### Importing files
filename <- "murders.csv"
dir <- system.file('extdata', package = 'dslabs')
fullpath <- file.path(dir, filename)
file.copy(fullpath, "murders.csv")
library(tidyverse)
dat <- read_csv('murders.csv')

# an example of full path on my computer
system.file(package = 'dslabs')
# use the function list.files
# an example of relative path as below
dir <- system.file(package = 'dslabs')
list.files(path = dir)

wd <- getwd()

library(readr)
read_lines('murders.csv', n_max = 3)
dat <- read_csv(filename)
view(dat)
dat <- read_csv(fullpath)
library(readxl)

url <- "https://raw.githubusercontent.com/rafalab/dslabs/master/inst/extdata/murders.csv"
dat <- read_csv(url)
download.file(url, 'murders.csv')
tmp_filename <- tempfile()
download.file(url, tmp_filename)
dat <- read_csv(tmp_filename)
file.remove(tmp_filename)
path <- system.file("extdata", package = "dslabs")
filename <- "murders.csv"
x <- scan(file.path(path, filename), sep = ",", what = "c")
x[1:10]

  ### ChatGPT exercise 
library(dplyr)
library(tidyverse)
library(dslabs)
data('tb_data')

tb_data <- data.frame(
  year = c(2023, 2024, 2025),
  tested = c(10000, 12000, 14000),
  positive = c(800, 900, 1000),
  notified = c(1200, 1400, 1600),
  population = c(1000000, 1000000, 1000000)
)

tb_data
tb_data$positivity <- tb_data$positive/tb_data$tested
notification_rate <- tb_data$notified/tb_data$population*10^5

str(tb_data)

tb_data$positivity
tb_data
tb_data$positivity_percent <- tb_data$positivity*100
tb_data

notification_rate <- tb_data$notified/tb_data$population*10^5
notification_rate
tb_data[tb_data$year==2025,]

tb_data[tb_data$year==2025,"positivity_percent"]

tb_data[, c("year","notified","positivity_percent")]

tb_summary <- aggregate(
  cbind(tested,positive,notified,population)~year,
  data=tb_data,
  FUN = sum
)
tb_summary
tb_summary$positivity <- tb_summary$positive/tb_summary$tested
tb_summary$notification_rate <-
  tb_summary$notified/tb_summary$population*10^5
library(ggplot2)
ggplot(tb_summary,aes(x=year,y=notification_rate))+
  geom_line()+
  geom_point()+
  theme_minimal()

tb_data$detection_rate <- tb_data$notified/tb_data$tested*100
tb_data
tb_data[tb_data$detection_rate>11.5,]

high_detection <- tb_data[tb_data$detection_rate>11.5,]
high_detection

if(!require(openintro)) install.packages('openintro')
library(openintro)
install.packages('airports','cherryblossom','usdata')

data("hsb2")
data('schtype')
ols <- lm(science ~ gender + schtype + read + socst, data = hsb2)


## Matching data

if (!require(MatchIt)) install.packages('MatchIt')
library('MatchIt')
data('lalonde')
View(lalonde)
d <- lalonde
# lets see what the variables are:
names(d)

## Initial balance (treatment & control)

mNULL <- matchit(treat ~ age + educ + race + married +
                   nodegree + re74 + re75 + re78, data = lalonde,
                 method = NULL)
summary(mNULL)

plot(mNULL, type = 'jitter', interactive = FALSE)

plot(summary(mNULL))

m1 <- matchit(treat ~ age+educ+race+married+nodegree+
                re74+re75+re78, data = d, method = 'nearest',
              distance = 'glm')
summary(m1)

m2 <- matchit(treat ~ age+educ+race+married+nodegree+
                re74+re75+re78, data = d, method = 'cem')
summary(m2)
plot(summary(m2))

d.matched <- match.data(m2)
dim(d.matched)
names(d.matched)

reg1 <- lm(re78 ~ treat +age + educ + race + married + nodegree +
             re74 + re75, data = d.matched, weights = weights)
summary(reg1)





























































































