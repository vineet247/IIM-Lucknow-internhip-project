# Analysis of MBA SALARIES
# NAME: Vineet Viswakumar
# EMAIL: vineetmilan@gmail.com
# COLLEGE / COMPANY: SRM University, Chennai

View(MBA.df)
attach(MBA.df)
summary(MBA.df)

#mean, median and standard deviation of variables
mean(age)
mean(gmat_tot)
mean(salary)
mean(work_yrs)
median(salary)
median(age)
median(gmat_tot)
mean(s_avg)
mean(f_avg)

no_data.df <- subset(MBA.df, satis!=998 & satis!=999)
mean(no_data.df$satis)

#Drow scatter plots to find relationships bewteen variables
plot(gmat_tot, salary)
plot(work_yrs, salary)
plot(no_data.df$satis, no_data.df$salary)
plot(quarter, salary)

#draw boxplots
boxplot(salary, xlab='Salary')
boxplot(gmat_tot, xlab='GMAT score')
boxplot(no_data.df$satis)

#corrogram
library(corrgram)
corrgram(MBA.df, order=TRUE, upper.panel=panel.pie, text.panel=panel.txt)

# subset of the dataset consisting of only those people who actually got a job
job <- subset(MBA.df, salary>0 & salary!=998 & salary!=999 & work_yrs>0)
View(job)
mean(job$age)
library(lattice)
bwplot(sex~salary, data = job, horizontal = TRUE)
bwplot(frstlang~salary, data=job, horizontal = TRUE)

#Run chi square test
mytable<-xtabs(~sex+salary, data = job)
addmargins(mytable)
chisq.test(mytable)

mytable<-xtabs(~gmat_tot+salary, data = job)
addmargins(mytable)
chisq.test(mytable)
           
mytable<-xtabs(~frstlang+salary, data = job)
addmargins(mytable)
chisq.test(mytable)

mytable<-xtabs(~work_yrs+salary, data = job)
addmargins(mytable)
chisq.test(mytable)

#Fitting regression models
plot(job$gmat_tot, job$salary)
plot(job$satis, job$salary)
model1 = lm(salary~., data = job)
summary(model1)
model2 = lm(salary~work_yrs+gmat_tpc+age+sex+s_avg+frstlang, data = job)
summary(model2)
model3 = lm(salary~frstlang+gmat_tot+gmat_tpc+age+sex, data = job)
summary(model3)
model = lm(salary~gmat_tot+s_avg+gmat_tpc+quarter+work_yrs+satis, data = job)
summary(model)


#comparison between those who got placed and those who didn't
nojob <- subset(MBA.df, MBA.df$salary==0 )
summary(nojob)
View(nojob[order(nojob$age),])
mean(nojob$age)
median(nojob$age)
mean(nojob$work_yrs)
View(nojob)

#chi squared test 
mytable<-xtabs(~age+salary, data = nojob)
addmargins(mytable)
chisq.test(mytable)
