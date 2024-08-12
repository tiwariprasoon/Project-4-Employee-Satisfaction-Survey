#1.1.	Import the data to check its class and structure and display the head and tail of the data.

setwd('e:/R Assignment')

getwd()

Data = read.csv('new-data.csv')

class(Data)

structure(Data)

head(Data)

tail(Data)

##2.	Calculate the:
#a.	Difference in the means of the pre and post variables

M1 =mean(Data$Pre)
M1
M2=mean(Data$Post)
M2
M2-M1


#b.	Values that divide the pre and post variable data into equal halves
M1=median(Data$Pre)
M1

M2=median(Data$Post)
M2


#c.	Mode for the pre variable

install.packages('statip')
library('statip')
mfv(Data$Pre)


#d.	First and third quantile for the pre and post variables

quantile(Data$Pre, 0.25)
quantile(Data$Pre, 0.75)

quantile(Data$Post,0.25)
quantile(Data$Post,0.75)


#e.	Range of the pre and post variables

range(Data$Pre)
range(Data$Post)


#f.	Variance and standard deviation for the pre and post variables

var(Data$Pre)
var(Data$Post)

sd(Data$Pre)
sd(Data$Post)
#g.	Coefficient of variation and mean absolute deviation for the pre and post variables
Coefficient_Pre = sd(Data$Pre)/mean(Data$Pre)*100
Coefficient_Pre
Coefficient_Post = sd(Data$Post)/mean(Data$Post)*100
Coefficient_Post


mad(Data$Pre,na.rm = T)
mad(Data$Post,na.rm = T)


#h.	Interquartile range of the pre and post variables

IQR(Data$Pre)
IQR(Data$Post)


## 3.	Measure the skewness for pre and post variables and apply the Agostino test to check the skewness

install.packages('moments')
library(moments)


skewness(Data$Pre)
skewness(Data$Post)

#agostino test

agostino.test(Data$Pre)
agostino.test(Data$Post)


#kurtosis

kurtosis(Data$Pre)
kurtosis(Data$Post)

anscombe.test(Data$Pre)
anscombe.test(Data$Post)

#5.	Plot a graph to check the skewness and peakedness in the distribution of pre and post variables

plot(density(Data$Pre), main = '', xlab = 'Data', col='red' ,lwd =3, xlim=c(2,7))
lines(density(Data$Post),col=('blue'),lwd=3)

#6.	Compute the frequency and relative frequency for each brand of cold drink

table(Data$Cold.Drink)
table(Data$Cold.Drink)/length(Data$Cold.Drink)


#7.	Create a pie chart and bar chart to show the preferences of the cold drinks available and provide the necessary labels
table(Data$Cold.Drink)

X=table(Data$Cold.Drink)/length(Data$Cold.Drink)

Cold.drink_data =c('Coca-Cola','Cold-Drink', 'Diet Coke', 'DrPepper', 'Pepsi', 'Sprite' )

pie(X,Cold.drink_data)

####BAR CHart
barplot(X, main = 'cold drinks available',
               names.arg = Cold.drink_data)


##
#8.	Plot a density graph on the cold-drink frequency and comment on the skewness and kurtosis
plot(density(table(Data$Cold.Drink)),main = 'cold-drink frequency',lwd=2)
x=table(Data$Cold.Drink)
skewness(x)
#0.5504878

kurtosis(x)
#1.996571


#9.	Convert the ‘Status’, ‘Rating’, and ‘Outlook’ variables into factor types and summarize them
Data$StatusFactor =as.factor(Data$Status)
str(Data)
summary(Data$Status)
head(Data)

Data$RatingFactor =as.factor(Data$Rating)
summary(Data$Rating)

class(Data$Rating)

Data$OutlookFactor =as.factor(Data$Outlook)
summary(Data$Outlook)
class(Data$Outlook)






# #10.	Calculate the difference in the average pre-training satisfaction ratings of member 
# #and observer status and for the post-training member and observer status
member=subset(Data, Data$Status=='Member')
observer=subset(Data, Data$Status=='Observer')
mempre=mean(member$Pre)
obspre=mean(observer$Pre)

diffpre=mempre-obspre
diffpre


mempost=mean(member$Post)
obspost=mean(observer$Post)

diffpost=mempost-obspost
diffpost
#11.	Compute the average pre-satisfaction and post-satisfaction ratings of employees 
# with a ‘Stable’ Outlook

outlook = subset(Data,Data$Outlook=='Stable')

meanpre = mean(outlook$Pre,na.rm = T)
meanpre
meanpost =mean(outlook$Post,na.rm = T)
meanpost
#12.	Construct a confidence interval at a 2.5%, 5%, and 1% level of significance for the salary variable#

Salary=Data$Salary
DOF=length(Salary)-1
a=.025
t=qt(p=a/2,df=DOF,lower.tail = F)
SE=sd(Salary)/sqrt(length(Salary))
MOE=t*SE
lower=mean(Salary)-MOE
upper=mean(Salary)+MOE
lower
upper
mean(Data$Salary)



#5% 

Salary=Data$Salary
DOF=length(Salary)-1
a=.5
t=qt(p=a/2,df=DOF,lower.tail = F)
SE=sd(Salary)/sqrt(length(Salary))
MOE=t*SE
lower=mean(Salary)-MOE
upper=mean(Salary)+MOE
lower
upper
mean(Data$Salary)

#1%

Salary=Data$Salary
DOF=length(Salary)-1
a=.10
t=qt(p=a/2,df=DOF,lower.tail = F)
SE=sd(Salary)/sqrt(length(Salary))
MOE=t*SE
lower=mean(Salary)-MOE
upper=mean(Salary)+MOE
lower
upper
mean(Data$Salary)
#13.	Construct a 99%, 95%, and 90% confidence interval estimate for the pre and post variables


#95%
a=0.05
1-a
SE=sd(Data$Pre)/sqrt(length(Data$Pre))
SE
z= quantile(Data$Pre,1-a/2)
z
MOE= z*SE
MOE
lower = mean(Data$Pre)- MOE
lower
upper = mean(Data$Pre)+ MOE
upper

#99%
a=0.05
1-a
SE=sd(Data$Pre)/sqrt(length(Data$Pre))
SE
z= quantile(Data$Pre,1-a/2)
z
MOE= z*SE
MOE
lower = mean(Data$Pre)- MOE
lower
upper = mean(Data$Pre)+ MOE
upper
#90%
a=0.1
1-a
SE=sd(Data$Pre)/sqrt(length(Data$Pre))
z= quantile(Data$Pre,1-a/2)

MOE= z*SE

lower = mean(Data$Pre)- MOE
lower
upper = mean(Data$Pre)+ MOE
upper

#Post Variable
#95%
a=0.05
1-a
SE=sd(Data$Post)/sqrt(length(Data$Post))
SE
z= quantile(Data$Pre,1-a/2)
z
MOE= z*SE
MOE
lower = mean(Data$Post)- MOE
lower
upper = mean(Data$Post)+ MOE
upper

#90%
a=0.1
1-a
SE=sd(Data$Post)/sqrt(length(Data$Post))
SE
z= quantile(Data$Pre,1-a/2)
z
MOE= z*SE
MOE
lower = mean(Data$Post)- MOE
lower
upper = mean(Data$Post)+ MOE
upper

#99%
a=0.01
1-a
SE=sd(Data$Post)/sqrt(length(Data$Post))
SE
z= quantile(Data$Pre,1-a/2)
z
MOE= z*SE
MOE
lower = mean(Data$Post)- MOE
lower
upper = mean(Data$Post)+ MOE
upper


#14.	Considering the Data.xlsx as a population:
#a.	Take a sample of 50 observations from the pre and post dataset (without replacement)
sample(Data$Pre,50, replace = F)
set.seed(50)

Pre= sample(Data$Pre,50, replace = F)
Post= sample(Data$Post,50, replace = F)
mean(Pre)
mean(Post)
mean(Data$Pre)


#b.	Construct a null hypothesis to examine whether the sample (50 observations) mean score of pre and post variables 
#is significantly different from the population (1000 observations)


install.packages('BSDA')


library(BSDA)


z.test(Pre,Data$Pre, mu=0, alternative = 'two.sided', conf.level = .95,sigma.x = sd(Pre),sigma.y = sd(Data$Pre))


z.test(Pre,Data$Pre, mu=0, alternative = 'greater', conf.level = .95,sigma.x = sd(Pre),sigma.y = sd(Data$Pre))


z.test(Pre,Data$Pre, mu=0, alternative = 'less', conf.level = .95,sigma.x = sd(Pre),sigma.y = sd(Data$Pre))

###Post Data Z value

z.test(Post,Data$Post, mu=0, alternative = 'two.sided', conf.level = .95,sigma.x = sd(Post),
       sigma.y = sd(Data$Post))


z.test(Post,Data$Post, mu=0, alternative = 'greater', conf.level = .95,sigma.x = sd(Post),
       sigma.y = sd(Data$Post))


z.test(Post,Data$Post, mu=0, alternative = 'less', conf.level = .95,sigma.x = sd(Post),
       sigma.y = sd(Data$Post))


#c.	Compute corresponding Z values for pre and post variables in the sample

z.test(Pre, mu=mean(Data$Pre), alternative = 'two.sided', conf.level = .95,sigma.x = sd(Data$Pre))
qnorm(p=.05,lower.tail = T)

z.test(Post, mu=mean(Data$Post), alternative = 'two.sided', conf.level = .95,sigma.x = sd(Data$Post))
qnorm(p=.05,lower.tail = T)



#15.	Using the p-value method, determine whether the sample mean for the pre and post variables
#differs significantly from the population mean at the 10% significance level 

z.test(Pre, mu=mean(Data$Pre), alternative = 'greater', conf.level = .90,sigma.x = sd(Data$Pre))

z.test(Post, mu=mean(Data$Post), alternative = 'two.sided', conf.level = .90,sigma.x = sd(Data$Post))
qqnorm(Data$Pre)


#16.	Calculate the critical Z value for the 10% level of significance and 
#the decision rule using the critical value approach

qnorm(p=.1,lower.tail = T)
qnorm(.95)


#17.	Compute the T-statistics value for the pre and post variables
t.test(Pre)
t.test(Post)

#18.	Calculate the p-value and the decision using the p-value approach for pre and post variables 
#at a 10% level of significance

z.test(Pre, mu=4.5, alternative = 'two.sided', conf.level = .90,sigma.x = sd(Data$Pre))

z.test(Pre, mu=4.5, alternative = 'greater', conf.level = .90,sigma.x = sd(Data$Pre))

z.test(Pre, mu=4.5, alternative = 'less', conf.level = .90,sigma.x = sd(Data$Pre))


z.test(Post, mu=4.5, alternative = 'two.sided', conf.level = .90,sigma.x = sd(Data$Post))

z.test(Post, mu=4.5, alternative = 'less', conf.level = .90,sigma.x = sd(Data$Post))

z.test(Post, mu=4.5, alternative = 'greater', conf.level = .90,sigma.x = sd(Data$Post))

#19.	Calculate the critical T value for the level of significance of 10% and 
#the decision rule using the critical value approach

t.test(Pre, mu=4.5, alternative = 'two.sided', conf.level = .90)
DOF=length(Pre)-1
qt(p=0.10/2,df=DOF,lower.tail = T)
