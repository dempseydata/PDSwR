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
drv = JDBC("org.h2.Driver","h2-1.3.170.jar",identifier.quote="'")
options=";LOG=0;CACHE_SIZE=65536;LOCK_MODE=0;UNDO_LOG=0"
conn = dbConnect(drv,paste("jdbc:h2:H2DB",options,sep=''),"u","u")
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