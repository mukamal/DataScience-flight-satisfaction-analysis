# improting library
library(tm)
library(wordcloud)
library(RCurl)
library(jsonlite)
library(tidyverse)
library(arules)
library(maptools)
library(ggmap)
library(maps)
library(arulesViz)
library(kernlab)


# set directory and import
getwd()
setwd("C:/Users/19294/OneDrive - Syracuse University/DS/Proj")

#there are many different versions of fromJSON, so the syntax jsonlite::fromJSON ensures we are using the fromJSON function from the jsonlite package.
df <- jsonlite::fromJSON("fall2019-survey-M06(2).json")
summary (df)
original_df <-df

df$satis


# prints out indeces with NA and num of NA.
for(i in 1:length(df)) {
  
  if(length(which(is.na(df[,i])))>0)
    print(paste(i,length(which(is.na(df[,i])))))
  
}



# Fix NAS in numeric
a<-0
for(i in 1:length(df)) {
  
  if (mode(df[,i])=="numeric") {
    
    if(length(which(is.na(df[,i])))>0)
    {
      print(i)
      a<- a+length(which(is.na(df[,i])))
      df[is.na(df[,i]),i]<-mean (df[,i],na.rm=TRUE)
      
    }
    
    
  }
}


#Use comments in your code to document how many missing data values you had to repair. 
#a
# 725
a



# for(i in 1:length(df)) 
# {
#   
#   if(length(which(is.na(df[,i])))>0)
#     print(i)
#   
# }
# 
# 
# head(df[,32], 5)
# tail(df[,32], 5)
# length(which(is.na(df[,32])))
# 



#Remove Free Text and create fdfM
dfN <-df[,1:31]
#dim(dfN)
dfM <-df


##################################################################################################################
#Q2


#For each numeric variable, create a histogram

for(i in 1:length(dfM)) {
  
  if (mode(dfM[,i])=="numeric") {
    
    # print(paste(i,colnames(df)[i] ))
    print(mean(dfM[,i])-median(dfM[,i]))
    hist(dfM[,i], main = paste("Histogram of" , colnames(dfM)[i]))
    if(mean(dfM[,i])-median(dfM[,i])>3)
    {  
      print(paste(colnames(dfM)[i],"is right skewed" ))
    }
    else if(mean(dfM[,i])-median(dfM[,i])< -3)
    {
      print(paste(colnames(dfM)[i],"is left skewed" ))
      
    }
    
    else
    {
      print(paste(colnames(dfM)[i],"symmetric" ))
    }
  }
}






#For each factor variable (e.g., Gender), use the table() command to summarize how many observations are in each category.
for(i in 1:length(dfN)) {
  
  if (mode(dfN[,i])=="character") {
 
    print(i)
    print(table(dfN[,i]))
     }
}




#•	Phase 3: Predict Satisfaction from Other Variables. 

#print modes of all columns
for(i in 1:length(dfN)) {
  
  print(mode(dfN[,i]))
  
}



#Print names of character mode columns
for(i in 1:length(dfN)) {
  
  if (mode(dfN[,i])=="character") {
    print(colnames(dfN)[i])

  }
  }

# Convert Chracter to Factor  columns 
dfN$Destination.City<- as.factor(dfN$Destination.City)
dfN$Origin.City<- as.factor(dfN$Origin.City)
dfN$Airline.Status<- as.factor(dfN$Airline.Status)
dfN$Gender<- as.factor(dfN$Gender)
dfN$Type.of.Travel<- as.factor(dfN$Type.of.Travel)
dfN$Class<- as.factor(dfN$Class)
dfN$Flight.date<- as.factor(dfN$Flight.date)
dfN$Partner.Code<- as.factor(dfN$Partner.Code)
dfN$Partner.Name<- as.factor(dfN$Partner.Name)
dfN$Origin.State<- as.factor(dfN$Origin.State)
dfN$Destination.State<- as.factor(dfN$Destination.State)
dfN$Flight.cancelled<- as.factor(dfN$Flight.cancelled)



# Check if any character mode columns
for(i in 1:length(dfN)) {
  if (mode(dfN[,i])=="character") {
    print(colnames(dfN)[i])
    
  }  
}


#testing all columns     
m <- lm(formula=Likelihood.to.recommend ~ Destination.City  +Origin.City +Airline.Status+ Age +Gender +Price.Sensitivity +Year.of.First.Flight +Flights.Per.Year+ Loyalty+ Type.of.Travel+ Total.Freq.Flyer.Accts +Shopping.Amount.at.Airport+ Eating.and.Drinking.at.Airport +Class+ Day.of.Month +Flight.date +Partner.Code+ Partner.Name+ Origin.State+ Destination.State+ Scheduled.Departure.Hour+ Departure.Delay.in.Minutes+ Arrival.Delay.in.Minutes+ Flight.cancelled +Flight.time.in.minutes+ Flight.Distance+ olong+olat+dlong+dlat, data=dfN)
summary(m)
#r2= .4056

#testing only significant columns
m1 <- lm(formula=Likelihood.to.recommend ~Airline.Status+ Age +Gender +Price.Sensitivity  +Flights.Per.Year+ Type.of.Travel+ Eating.and.Drinking.at.Airport +Class+ Day.of.Month +Flight.date +Partner.Code+ Scheduled.Departure.Hour+ Departure.Delay.in.Minutes+ Arrival.Delay.in.Minutes+olat, data=dfN)
summary(m1)





#mPataNai <- lm(formula=Likelihood.to.recommend ~ Age+ Price.Sensitivity+ Year.of.First.Flight+ Flights.Per.Year+Loyalty+ Total.Freq.Flyer.Accts+ Shopping.Amount.at.Airport+ Eating.and.Drinking.at.Airport+Day.of.Month+ Scheduled.Departure.Hour+ Departure.Delay.in.Minutes+ Arrival.Delay.in.Minutes+Flight.time.in.minutes+ Flight.Distance+ olong+ olat+dlong+dlat ,data=df)
#summary(mPatanai)

#(Intercept)                    -3.045e+01  1.418e+01  -2.148  0.03172 *  
#Age                            -2.512e-02  1.390e-03 -18.069  < 2e-16 ***
#Price.Sensitivity              -4.725e-01  3.845e-02 -12.289  < 2e-16 ***
#Year.of.First.Flight            1.966e-02  7.065e-03   2.783  0.00539 ** 
#Flights.Per.Year               -3.132e-02  2.086e-03 -15.009  < 2e-16 ***
#Total.Freq.Flyer.Accts         -6.357e-02  2.121e-02  -2.997  0.00273 ** 
#Eating.and.Drinking.at.Airport  3.126e-03  4.076e-04   7.670 1.87e-14 ***
#Day.of.Month                    4.071e-03  2.421e-03   1.681  0.09272 .  
#Departure.Delay.in.Minutes      5.178e-03  1.946e-03   2.661  0.00781 ** 
#Arrival.Delay.in.Minutes       -1.038e-02  1.932e-03  -5.372 7.96e-08 ***
#Flight.Distance                 3.058e-04  1.688e-04   1.812  0.07004 .  
#olat                            1.150e-02  3.820e-03   3.010  0.00262 ** 

#m1 <- lm(formula=Likelihood.to.recommend ~ Age+ Price.Sensitivity+ Year.of.First.Flight+ Flights.Per.Year+ Total.Freq.Flyer.Accts+ Eating.and.Drinking.at.Airport+Day.of.Month+ Departure.Delay.in.Minutes+ Arrival.Delay.in.Minutes+ Flight.Distance+ olat ,data=df)
#summary(m1)

# !!!!!! Flight Distance


# mp <- NULL
# mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
# 
# df %>%
#   filter(Likelihood.to.recommend<7) %>%
#   ggplot() + mapWorld+
#   geom_point(aes(x=olong, y=olat) ,color="red", size=3)   + 
#   geom_point(aes(x=dlong, y=dlat) ,color="blue", size=3)   + 
#   ggtitle("Unsatisfied Customers loc n lat") 
# 


# plotting less satisfactory customers
dfNLow<-dfN %>% filter(Likelihood.to.recommend<7)

mp <- NULL
mapWorld <- borders("usa", colour="gray50", fill="gray50") # create a layer of borders
dfNLow %>%
  ggplot() + mapWorld+
  geom_point(aes(x=olong, y=olat) ,color="red", size=3)   + 
  geom_point(aes(x=dlong, y=dlat) ,color="blue", size=3)   + 
  ggtitle("Unsatisfied Customers loc n lat") +coord_map()


#length(df$Likelihood.to.recommend[df$Likelihood.to.recommend<7])
#length(df$Likelihood.to.recommend[df$Likelihood.to.recommend<2])


mp <- NULL
mapWorld <- borders("usa", colour="gray50", fill="gray50") # create a layer of borders

dfN %>%
  filter(Likelihood.to.recommend<3) %>%
  ggplot() + mapWorld+
  geom_point(aes(x=olong, y=olat) ,color="red", size=3)   + 
  geom_point(aes(x=dlong, y=dlat) ,color="blue", size=3)   + 
  geom_segment(aes(x = olong, y = olat, xend = dlong, yend = dlat),
               arrow = arrow(length = unit(0.25,"cm"))) + 
  coord_equal()
ggtitle("Unsatisfied Customers loc n lat") 



#geom_curve(aes(x = olong, y = olat, xend = dlong, yend = dlat, colour = "curve"), data = dfN)




###############################################################################################################

# 
# y <- as.matrix(dfN)
# 
# rules <- apriori(y, parameter = list(supp = 0.1, conf = 0.8))
# 
# itemFrequencyPlot(rules, support = 0.1, cex.names = 0.8)
# 
# z <- class
# 
# tData <- as (dfNew, "transactions")
# 
# drops <- c("Price.Sensitivity","Total.Freq.Flyer.Accts","Shopping.Amount.at.Airport","Departure.Delay.in.Minutes","Arrival.Delay.in.Minutes")
# 
# 
# dfNew <- dfN[ , !(names(dfN) %in% drops)]
# tData <- as (dfNew, "transactions")
# frequentItems <- eclat (tData, parameter = list(supp = 0.07, maxlen = 15)) # calculates support for frequent items
# head(inspect(frequentItems))
# 
# 
# 
# 
# summary(df$Price.Sensitivity)
# 




#aa <- data.frame(x1 = 2.62, x2 = 3.57, y1 = 21.0, y2 = 15.0)





#####################################################################################
##########################          Arules          ###########################################


dfModel <- data.frame(dfN$Likelihood.to.recommend,dfN$Airline.Status,dfN$Age, dfN$Gender,dfN$Price.Sensitivity,dfN$Flights.Per.Year, dfN$Type.of.Travel, dfN$Eating.and.Drinking.at.Airport, dfN$Class, dfN$Day.of.Month, dfN$Flight.date, dfN$Partner.Code, dfN$Scheduled.Departure.Hour, dfN$Departure.Delay.in.Minutes, dfN$Arrival.Delay.in.Minutes, dfN$olat )
#dfModel <- data.frame(dfNLow$Likelihood.to.recommend,dfNLow$Airline.Status,dfNLow$Age, dfNLow$Gender,dfNLow$Price.Sensitivity,dfNLow$Flights.Per.Year, dfNLow$Type.of.Travel, dfNLow$Eating.and.Drinking.at.Airport, dfNLow$Class, dfNLow$Day.of.Month, dfNLow$Flight.date, dfNLow$Partner.Code, dfNLow$Scheduled.Departure.Hour, dfNLow$Departure.Delay.in.Minutes, dfNLow$Arrival.Delay.in.Minutes, dfNLow$olat )

dim(dfModel)

#colnames(dfModel)



#by_Recom <- dfModel %>% group_by(Likelihood.to.recommend)
#we have like 31 different groupings that cna be used in arules


#dfMat <- as.matrix(dfModel)
#rules <- apriori(dfMat, parameter = list(supp = 0.1, conf = 0.8))
#itemFrequencyPlot(rules, support = 0.1, cex.names = 0.8)
#tData <- as (dfModel, "transactions")

colnames(dfModel)
drops <- c("dfN.Price.Sensitivity","dfN.Total.Freq.Flyer.Accts","dfN.Shopping.Amount.at.Airport","dfN.Departure.Delay.in.Minutes","dfN.Arrival.Delay.in.Minutes")
#drops <- c("dfNLow.Price.Sensitivity","dfNLow.Total.Freq.Flyer.Accts","dfNLow.Shopping.Amount.at.Airport","dfNLow.Departure.Delay.in.Minutes","dfNLow.Arrival.Delay.in.Minutes")
dfNew <- dfModel[ , !(names(dfModel) %in% drops)]
tData <- as (dfNew, "transactions")

summary(tData)
dev.off()
par(mar = rep(2, 4))
itemFrequencyPlot(tData,support=0.5)

frequentItems <- eclat (tData, parameter = list(supp = 0.07, maxlen = 15)) # calculates support for frequent items


ruleset <- apriori(tData,parameter =list(support = 0.09,confidence = 0.6), appearance = list(default="lhs",rhs=("dfN.Likelihood.to.recommend=[1,7)")))
ruleset <- apriori(tData,parameter =list(support = 0.09,confidence = 0.6), appearance = list(default="lhs",rhs=("dfN.Likelihood.to.recommend  [1,7)")))

summary(ruleset)
inspect(ruleset)

plot(ruleset, main="KAKA")


goodrules <- ruleset[quality(ruleset)$lift > 1.1]
summary(goodrules)
inspect(goodrules)











#####################################  USP   #########################
sba<- dfM[,32][!is.na(dfM[,32])]
head(sba, 3)
words.vec <- VectorSource(sba)
words.corpus <- Corpus(words.vec)
words.corpus
words.corpus <- tm_map(words.corpus,content_transformer(tolower))
words.corpus <- tm_map(words.corpus, removePunctuation)
words.corpus <- tm_map(words.corpus, removeNumbers)
words.corpus <- tm_map(words.corpus, removeWords,stopwords("english"))
tdm <- TermDocumentMatrix(words.corpus)
tdm


m <- as.matrix(tdm)
wordCounts <- rowSums(m)
wordCounts <- sort(wordCounts, decreasing=TRUE)
head(wordCounts)

cloudFrame<-data.frame(word=names(wordCounts),freq=wordCounts)
wordcloud(cloudFrame$word,cloudFrame$freq)


wordcloud(names(wordCounts), wordCounts, min.freq=2, max.words=50, rot.per=0.35, colors=brewer.pal(8,"Dark2"))




################################### Sentiment ######################


posWords <- scan("positive-words.txt", character(0), sep = "\n")
negWords <- scan("negative-words.txt", character(0), sep = "\n")
posWords <- posWords[-1:-34]
negWords <- negWords[-1:-34]

charVector<-sba
words.vec <- VectorSource(charVector)
words.corpus <- Corpus(words.vec)

words.corpus <- tm_map(words.corpus,content_transformer(tolower))
words.corpus <- tm_map(words.corpus, removePunctuation)
words.corpus <- tm_map(words.corpus, removeNumbers)
words.corpus <- tm_map(words.corpus, removeWords, stopwords("english"))
tdm <- TermDocumentMatrix(words.corpus)
m <- as.matrix(tdm)
wordCounts <- rowSums(m)
wordCounts <- sort(wordCounts, decreasing=TRUE)
matchedP <- match(names(wordCounts), posWords, nomatch = 0) 
matchedN <- match(names(wordCounts), negWords, nomatch = 0) 
positiveCounts <- wordCounts[which(matchedP != 0)]
positiveNames <- posWords[matchedP]
p<-data.frame(positiveCounts,positiveNames)


g <- ggplot(p, aes(x=reorder(positiveNames,-positiveCounts), y=positiveCounts)) 
g <- g + geom_col() + ggtitle("bar chart of the positive matches")
g <- g + theme(axis.text.x =element_text(angle = 90, hjust = 1))
g

barplot(postiveCounts)


#B.	Using ggplot,  create a bar chart of the top 20 negative matches. 
negativeCounts <- wordCounts[which(matchedN != 0)]
negativeNames <- negWords[matchedN]
n<-data.frame(negativeNames,negativeCounts)
n %>%
  arrange(desc(negativeCounts)) %>%
  slice(1:20) %>%
  ggplot() +
  aes(x=reorder(negativeNames,-negativeCounts), y=negativeCounts) +
  geom_col() +
  ggtitle("bar chart of the nagetive matches")+
  theme(axis.text.x =element_text(angle = 90, hjust = 1))

postiveCounts <- wordCounts[which(matchedP != 0)]
positiveNames <- posWords[matchedP]
p<-data.frame(positiveNames,postiveCounts)
p %>%
  arrange(desc(postiveCounts)) %>%
  slice(1:20) %>%
  ggplot() +
  aes(x=reorder(positiveNames,-postiveCounts), y=postiveCounts) +
  geom_col() +
  ggtitle("bar chart of the positive matches")+
  theme(axis.text.x =element_text(angle = 90, hjust = 1))









perN<- sum(negativeCounts)/sum(wordCounts)
perN

perP<- sum(positiveCounts)/sum(wordCounts)
perP






####################################         MVP     ######
#table(dfN$Airline.Status)

# data(spam)
# mode(spam)
# install.packages("fastDummies")
# library(fastDummies)
# g<-as.factor(dfN$Gender) 
# results <- fastDummies::dummy_cols(dfN$Gender)
# knitr::kable(results)
# length(g)
# sum(g)

dfsmv0 <- data.frame(dfN$Likelihood.to.recommend,dfN$Airline.Status,dfN$Age, dfN$Gender,dfN$Price.Sensitivity,dfN$Flights.Per.Year, dfN$Type.of.Travel, dfN$Eating.and.Drinking.at.Airport, dfN$Class, dfN$Day.of.Month, dfN$Flight.date, dfN$Partner.Code, dfN$Scheduled.Departure.Hour, dfN$Departure.Delay.in.Minutes, dfN$Arrival.Delay.in.Minutes, dfN$olat )

str(dfsmv0)



# 
# for(i in 1:length(dfsmv)) {
#   
#   print(mode(dfsmv[,i]))
#   
# }
# 




dfsmv <- data.frame(dfN$Likelihood.to.recommend,dfN$Airline.Status,dfN$Age, dfN$Gender,dfN$Price.Sensitivity,dfN$Flights.Per.Year, dfN$Type.of.Travel, dfN$Eating.and.Drinking.at.Airport, dfN$Class, dfN$Day.of.Month, dfN$Flight.date, dfN$Partner.Code, dfN$Scheduled.Departure.Hour, dfN$Departure.Delay.in.Minutes, dfN$Arrival.Delay.in.Minutes, dfN$olat )

randIndex <- sample(1:dim(dfsmv)[1])
cutPoint2_3 <- floor(2 * dim(dfsmv)[1]/3)
cutPoint2_3
trainData <- dfsmv[randIndex[1:cutPoint2_3],]
testData <-dfsmv[randIndex[(cutPoint2_3+1):dim(dfsmv)[1]],]
#trainDataMat <- as.matrix(trainData)


svmOutput <- ksvm(dfN.Likelihood.to.recommend ~ ., data=trainData, kernel = "rbfdot",kpar="automatic",C=5,cross=3,prob.model=TRUE)
#svmOutput <- ksvm(type~ ., data=trainData, kernel = "rbfdot",kpar="automatic",C=5,cross=3,prob.model=TRUE)

svmOutput
hist(alpha(svmOutput)[[1]])

svmPred <- predict(svmOutput, testData, type ="votes")

compTable <- data.frame(testData[,1],svmPred[1,])
length(testData[,1])
length(svmPred)
tail(compTable)

table(compTable)


colnames(svmPred)[1]





############################################# Yatish #############################3

install.packages('caret', dependencies = TRUE)
library(caret)
dfN <-df[,1:31]
dfN<- as.data.frame(dfN)
dfsvm <- data.frame(dfN$Likelihood.to.recommend,dfN$Airline.Status,dfN$Age, dfN$Gender,dfN$Price.Sensitivity,dfN$Flights.Per.Year, dfN$Type.of.Travel, dfN$Eating.and.Drinking.at.Airport, dfN$Class, dfN$Day.of.Month, dfN$Flight.date, dfN$Partner.Code, dfN$Scheduled.Departure.Hour, dfN$Departure.Delay.in.Minutes, dfN$Arrival.Delay.in.Minutes, dfN$olat )
dfsvm$dfN.Likelihood.to.recommend <- cut(dfsvm$dfN.Likelihood.to.recommend,breaks=c(0,8,10))
  
dim(dfsvm)

trainList <- createDataPartition(y=dfsvm$dfN.Likelihood.to.recommend, p=.65,list=FALSE)
trainData <- dfsvm[trainList,]
testData <- dfsvm[-trainList,]



svmOutput <- ksvm(dfN.Likelihood.to.recommend ~ ., data=trainData, kernel = "rbfdot", kpar="automatic",C=5,cross=3,prob.model=TRUE)
svmOutput



svmPred <- predict(svmOutput, testData)
str(svmPred)
str(testData)



confusionMatrix(svmPred, testData$dfN.Likelihood.to.recommend)
#Cross validation error : 0.233393 

compTable <- data.frame(testData[,1],svmPred)
table(compTable)
(464+410)/(3598)
#0.2429127
