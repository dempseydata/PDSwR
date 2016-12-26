# car data
uciCar <- read.table(file = 'Chpt2/car.data.csv', sep = ',', header = T)
class(uciCar)
summary(uciCar)
head(uciCar)
tail(uciCar)
dim(uciCar)

# credit apps
#d <- read.table(paste('http://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data',sep=''), stringsAsFactors=F,header=F)

# on line data source no available?
load('Chpt2/GCDData.RData')
print(d[1:3,'Purpose'])

summary(d$Purpose)
table(d$Purpose, d$Good.Loan)

#install.packages('RJDBC')


# once the data has been pumped into H@ following the readme instrcutions, query it
options( java.parameters = "-Xmx2g" )
library(RJDBC)
drv = JDBC("org.h2.Driver","ignore/h2-1.3.170.jar",identifier.quote="'")
options=";LOG=0;CACHE_SIZE=65536;LOCK_MODE=0;UNDO_LOG=0"
# after the "h2", is where the path to the DB file is specified.
conn = dbConnect(drv,paste("jdbc:h2:ignore/H2DB",options,sep=''),"u","u")
dhus = dbGetQuery(conn,"SELECT * FROM hus WHERE ORIGRANDGROUP<=1")
dpus = dbGetQuery(conn,"SELECT pus.* FROM pus WHERE pus.SERIALNO IN (SELECT DISTINCT hus.SERIALNO FROM hus WHERE hus.ORIGRANDGROUP<=1)")
dbDisconnect(conn)
save(dhus,dpus,file='phsample.RData')


psub <- subset(dpus, with(dpus, (PINCP>1000) & (ESR ==1) & (PINCP<=250000) & (PERNP>1000) & (PERNP<=250000) & (WKHP>=40) & (AGEP>=20) & (AGEP<=50) & (PWGTP1>0) & (COW %in% (1:7)) & (SCHL %in% (1:24))))


# fix up factors - replace what is an ID, with the appropriate string factors
psub$SEX <- as.factor(ifelse(psub$SEX==1,'M','F'))
psub$SEX <- relevel(psub$SEX,'M')
cowmap <- c("Employee of a private for-profit",
            "Private not-for-profit employee",
            "Local government employee",
            "State government employee",
            "Federal government employee",
            "Self-employed not incorporated",
            "Self-employed incorporated")
psub$COW <- as.factor(cowmap[psub$COW])
psub$COW <- relevel(psub$COW,cowmap[1])
schlmap <- c(
  rep("no high school diploma",15),
  "Regular high school diploma",
  "GED or alternative credential",
  "some college credit, no degree",
  "some college credit, no degree",
  "Associate's degree",
  "Bachelor's degree",
  "Master's degree",
  "Professional degree",
  "Doctorate degree")
psub$SCHL <- as.factor(schlmap[psub$SCHL])
psub$SCHL <- relevel(psub$SCHL,schlmap[1])

summary(psub)

#get training and test data sets
dtrain <- subset (psub, ORIGRANDGROUP >= 500)
dtest <- subset(psub, ORIGRANDGROUP < 500)


summary(dtrain$COW)


#chapter 3

custdata <- read.table('Chpt3/custdata.tsv',header=T,sep='\t')

summary(custdata$income)

#section 3.2.1

#install.packages("ggplot2")

library(ggplot2)
ggplot(custdata) +
  geom_histogram(aes(x=age),
                 binwidth=5, fill="grey")

#install.packages("scales") # 0.4.1 is already in the R
library(scales) # brings in dollar x axis notation

ggplot(custdata) +
  geom_density(aes(x=income)) +
  scale_x_continuous(labels=dollar)


ggplot(custdata) +
  geom_density(aes(x=income)) +
  scale_x_log10(breaks=c(100,100,10000,100000), labels=dollar) +
  annotation_logticks(sides="bt")

# low cardinality categorical
ggplot(custdata) + geom_bar(aes(x=marital.stat), fill="gray")

# higher cadinality categorical
ggplot(custdata) + geom_bar(aes(x=state.of.res), fill="gray") +
  coord_flip() +
  theme(axis.text.y=element_text(size=rel(0.8)))

# reordering the fctors of a column, by descending size
statesums <- table(custdata$state.of.res) #table with standard order
statef <- as.data.frame(statesums) #stick in a data frame - could have done this with one line
colnames(statef) <- c("state.of.res", "count") # rename the columns in the new data frame
summary(statef) # default ordering is still alpha

statef <- transform(statef, state.of.res = reorder(state.of.res,count))
# order is now by count ascending

ggplot(statef) + geom_bar(aes(x=state.of.res,y=count), stat="identity", fill="gray") +
  coord_flip() +
  theme(axis.text.y=element_text(size=rel(0.8)))


#3.2.2

custdata2 <- subset(custdata, (custdata$age > 0 & custdata$age < 100 & custdata$income > 0))

cor(custdata2$age, custdata2$income)

ggplot(custdata2, aes(x=age, y=income)) +
  geom_point() +
  stat_smooth() +
  ylim(0, 200000)

install.packages("hexbin")

#library(hexbin)

ggplot(custdata2, aes(x=age, y=income))+
  geom_hex(binwidth=c(5,10000))+
  geom_smooth(color="white", se=F)+ # se=F removes the standard error boundaries
  ylim(0,200000)

ggplot(custdata2) +
  geom_bar(aes(x=marital.stat), position="dodge", fill="darkgray")+
  facet_wrap(~housing.type, scales="free_y")+ #free y loosens up each y axis to be independent, else all have same scale
  theme(axis.text.x =element_text(angle=45,hjust=1))

# chpater 4
load("Chpt4/exampleData.rData")
summary(custdata)
summary(hhdata)
summary(medianincome)

# looking at the columns each with 56 missing housing.type
summary(custdata[is.na(custdata$housing.type),c("recent.move","num.vehicles")])
# and they are the same 56 records with 3 col values of NA

# replace NA in is.employed with "missing"

#well, more like ADDING a new column so as to not alter the original data
custdata$is.employed.fix <- ifelse(is.na(custdata$is.employed),"missing",
       ifelse(custdata$is.employed==T,"employed","not employed"))

summary(as.factor(custdata$is.employed.fix))

# in 4.1.1, about pg140

summary(custdata$Income)
meanIncome <- mean(custdata$Income, na.rm=T)
income.fix <- ifelse(is.na(custdata$Income),meanIncome, custdata$Income)
summary(income.fix)
# assign the issing ones to the mean, and on average, it will be okay
# other options would be to impute the income based on other factors, as per original R book back in 2011
# eg regression, decision trees etc.
# but only if the records are missing the data by chance, if they are missing it for a systematic reason....

breaks <- c(0,10000,50000,100000,250000,1000000)
Income.groups <- cut(custdata$Income, breaks=breaks, include.lowest=T)

# NA is still there, but this variable is now a categorical, rather than continuous
summary(Income.groups)

# so make is a char
Income.groups <- as.character(Income.groups)

Income.groups <- ifelse(is.na(Income.groups), "no income", Income.groups)

summary(as.factor(Income.groups))
# but they are scientific!!!

# or, flag the missing records with a new variable and replace with 0
missingIncome <- is.na(custdata$Income)
Income.fix <- ifelse(is.na(custdata$Income), 0, custdata$Income)

summary(medianincome)

#merge two data frames together - full outer joins are by all = T
#custdata <- merge(custdata, medianincome, by.x="state.of.res", by.y ="State")
# already there??????

summary(custdata[,c("state.of.res", "income", "Median.Income")])

# now normalize the income, into a ratio with the states median income - avoids state bte state magnitude issues

custdata$income.norm <- with(custdata, income/Median.Income)

summary(custdata$income.norm)
#normalizing and rescaling ~157
# listing 4.7
<<<<<<< HEAD

meanage <- mean(custdata$age)

custdata$age.normalized <- custdata$age / meanage
summary(custdata$age.normalized)

stdage <- sd(custdata$age)

#normalize the age to the difference from the mean, in standard deviations
custdata$age.normalized <- (custdata$age - meanage) / stdage

summary(custdata$age.normalized)
# useful when distributions are roughly symmetrical

# log transforms only work for non negative data, unless you use a signed log...???

summary(custdata$gp)

# runif gens a uniform distribution of numbers between 0-1 for the number of rows in the data frame
custdata$gp <- runif(dim(custdata)[1])

testset <- subset(custdata, custdata$gp <= 0.1)
trainingSet <- subset(custdata, custdata$gp > 0.1)

dim(testset)[1]
dim(trainingSet)[1]

# once the gp is generated, then we can repeatedly pull the same sample and training split as we continue to develop
# rather than using the sample function, which draws a random sample each time it is called
# ie. reproducible

# but, this way means each record must be individual cases / customers
# for a household level analysis, each member of a household MUST be in the same test / training split
# meaning that the split should be done at the household level, not the customer level

# listing 4.10
<<<<<<< HEAD

# listing 5.1

spamD <- read.table('Ignore/spamD.tsv',header=T,sep='\t')
spamTrain <- subset(spamD, spamD$rgroup >= 10)
spamTest <- subset(spamD, spamD$rgroup < 10)
spamVars <- setdiff(colnames(spamD),list('rgroup','spam'))

spamFormula <- as.formula(paste('spam=="spam"',paste(spamVars,collapse=' + '), sep='~'))

spamModel <- glm(spamFormula, family=binomial(link='logit'), 
                 data=spamTrain)

spamTrain$prod <- predict(spamModel, newdata=spamTrain, type='response')
print(with(spamTrain, table(y=spam, glmPred=prod>0.5)))

spamTest$prod <- predict(spamModel, newdata=spamTest, type='response')
print(with(spamTest, table(y=spam, glmPred=prod>0.5)))


# accuracy
# precision = 
# recall
# F1

# sensitivity = recall = true psotivie rate
# specificity = true negative rate

library(ggplot2)
ggplot(data=spamTest) +
  geom_density(aes(x=prod,color=spam,linetype=spam))

#install.packages('ROCR')
library('ROCR')
eval <- prediction(spamTest$prod,spamTest$spam)
plot(performance(eval,"tpr","fpr"))
print(attributes(performance(eval,'auc'))$y.values[[1]])

# log likelihood ~211


set.seed(32297)
d <- data.frame(x=runif(100),y=runif(100))
clus <- kmeans(d,centers=5)
d$cluster <- clus$cluster


library('ggplot2')
library('grDevices')
h <- do.call(rbind,
             lapply(unique(clus$cluster),
                    function(c) { f <- subset(d,cluster==c); f[chull(f),]}))
ggplot() +
  geom_text(data=d,aes(label=cluster,x=x,y=y,
                       color=cluster),size=3) +
  geom_polygon(data=h,aes(x=x,y=y,group=cluster,fill=as.factor(cluster)),
               alpha=0.4,linetype=0) +
  theme(legend.position = "none")

# chapter 6

load('Ignore/KDD2009.Rdata')

d <- read.table('Ignore/orange_small_train.data', header=T,sep = '\t', na.strings = c('NA',''))

set.seed(729375)
d$rgroup <- runif(dim(d)[[1]])

dTrainAll <- subset(d, rgroup <= 0.9)
dTest <- subset(d, rgroup > 0.9)
outcomes <- c('churn','appetency','upselling')
vars <- setdiff(colnames(dTrainAll), c(outcomes, 'rgroup'))

catVars <- vars[sapply(dTrainAll[,vars], class) %in% c('factor','character')]
numericVars <- vars[sapply(dTrainAll[,vars], class) %in% c('numeric','integer')]

rm(list=c('d','churn','appetency','upselling'))

outcome <- 'churn'
pos <- '1'

# sets useForCal into TRUE/FALSE using a binomial distribution
# not sure WHY they used this method - no explanation given
useForCal <- rbinom(n=dim(dTrainAll)[[1]], size=1,prob=0.1) > 0
dCal < subset(dTrainAll, useForCal)
dTrain <- subset(dTrainAll, !useForCal)

table218 <- table(
  Var281=dTrain[,'Var218'],
  churn=dTrain[,'churn'],
  useNA='ifany')
print(table218)

print(table218[,2]/(table218[,1]+table218[,2]))

mkPredC <- function(outCol, varCol, appCol) {
  pPos <- sum(outCol==pos) / length(outCol)
  naTab <- table(as.factor(outCol[is.na(varCol)]))
  pPosWna <- (naTab/sum(naTab)) [pos]
  vTab <- table(as.factor(outCol),varCol)
  pPosWv <- (vTab[pos,]+1.0e-3*pPos)/(colSums(vTab)+1.0e-3)
  
  pred <- pPosWv[appCol]
  pred[is.na(appCol)] <- pPosWna
  pred[is.na(pred)] <- pPos
  
  pred
}

# repeatedly do simple, single comulmn preictorsfor categorical variables
for(v in catVars) {
  pi <- paste('pred',v,sep='')
  dTrain[,pi] <- mkPredC(dTrain[,outcome],dTrain[,v],dTrain[,v])
  dCal[,pi] <- mkPredC(dTrain[,outcome],dTrain[,v],dCal[,v])
  dTest[,pi] <- mkPredC(dTrain[,outcome],dTrain[,v],dTest[,v])
}

library('ROCR')

calcAUC <- function(predcol,outcol) {
  perf <- performance(prediction(predcol,outcol==pos),'auc')
  as.numeric(perf@y.values)
}


for(v in catVars) {
  pi <- paste('pred',v,sep='')
  aucTrain <- calcAUC(dTrain[,pi],dTrain[,outcome])
  if(aucTrain>=0.8) {
    aucCal <- calcAUC(dCal[,pi],dCal[,outcome])
    print(sprintf("%s, trainAUC: %4.3f calibrationAUC: %4.3f",
                  pi,aucTrain,aucCal))
  }
}


mkPredN <- function(outCol,varCol,appCol) {
  cuts <- unique(as.numeric(quantile(varCol,
                                     probs=seq(0, 1, 0.1),na.rm=T)))
  varC <- cut(varCol,cuts)
  
  appC <- cut(appCol,cuts)
  mkPredC(outCol,varC,appC)
}

for(v in numericVars) {
  pi <- paste('pred',v,sep='')
  dTrain[,pi] <- mkPredN(dTrain[,outcome],dTrain[,v],dTrain[,v])
  dTest[,pi] <- mkPredN(dTrain[,outcome],dTrain[,v],dTest[,v])
  dCal[,pi] <- mkPredN(dTrain[,outcome],dTrain[,v],dCal[,v])
  aucTrain <- calcAUC(dTrain[,pi],dTrain[,outcome])
  if(aucTrain>=0.55) {
    aucCal <- calcAUC(dCal[,pi],dCal[,outcome])
    print(sprintf("%s, trainAUC: %4.3f calibrationAUC: %4.3f",
                  pi,aucTrain,aucCal))
  }
} 

var <- 'Var217'
aucs <- rep(0,100)

for(rep in 1:length(aucs)){
  useForCalRep <- rbinom(n=dim(dTrainAll)[[1]], size=1, prob=0.1) >0
  predRep <- mkPredC(dTrainAll[!useForCalRep, outcome],
                     dTrainAll[!useForCalRep, var],
                     dTrainAll[useForCalRep, var])
  aucs[rep] <- calcAUC(predRep, dTrainAll[useForCalRep, outcome])
}
mean(aucs)
sd(aucs)


fCross <- function() {
  useForCalRep <- rbinom(n=dim(dTrainAll)[[1]],size=1,prob=0.1)>0
  predRep <- mkPredC(dTrainAll[!useForCalRep,outcome],
                     dTrainAll[!useForCalRep,var],
                     
                     dTrainAll[useForCalRep,var])
  calcAUC(predRep,dTrainAll[useForCalRep,outcome])
}
# wrapper for sapply to repeat the function above 100 times
aucs <- replicate(100,fCross())
# sig quicker

# listing 6.11???
# truth be told. I dont think I am learning much from this book, as it is not exercise driven...
# not as good as previous books.
# SWITCH to Hadley Wickams

