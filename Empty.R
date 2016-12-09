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
# changed
