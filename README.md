###########################################################################
Prediction of march madness groups and the outcome of individual matches. 
Code includes data gathering, feature creation and shortlisting, modeling, accuracy determination, and validation. 
###########################################################################

End to End code

###########################################################################

01. Team features extraction
# loading libraries
library(data.table)
library(dplyr)
# setting working directory
setwd(
"C:/Users/Susmitha Reddy/Desktop/ms project/march-madness-analytics-2020/MDataFiles_Stage2"
)
# Importing data
# getting season, tournament and secondary tourney match datasets
MNCAATourneyDetailedResults <-
read.csv("MNCAATourneyDetailedResults.csv", stringsAsFactors = FALSE)
MRegularSeasonDetailedResults <-
read.csv("MRegularSeasonDetailedResults.csv", stringsAsFactors = FALSE)
# MNCAATourneyCompactResults <-
# read.csv("MNCAATourneyCompactResults.csv", stringsAsFactors = FALSE)
# MRegularSeasonCompactResults <-
# read.csv("MRegularSeasonCompactResults.csv", stringsAsFactors = FALSE)
# MSecondaryTourneyCompactResults <-
# read.csv("MSecondaryTourneyCompactResults.csv", stringsAsFactors = FALSE)
# creating master file for teams with all scores
# Not considering compact results files as they are exact replica of Detailed results
# retaining secondary tourney results as there is no detailed file
# rm(MNCAATourneyCompactResults, MRegularSeasonCompactResults)
# consider only data after 2003
# creating column with score difference - for future Y variable
for (df in c('MNCAATourneyDetailedResults','MRegularSeasonDetailedResults')) {
data = get(df)
data = as.data.table(data)
data <- data[Season >= 2003,':='(Score_dif=(WScore - LScore),ds = df) ]
dataw = data[,.(Season, DayNum, WLoc, NumOT, WTeamID, WScore, WFGM, WFGA, WFGM3, WFGA3, WFTM, WFTA, WOR, WDR, WAst, WTO, WStl, WBlk, WPF,Score_dif ,ds)]
datal = data[,.(Season, DayNum, WLoc, NumOT, LTeamID, LScore, LFGM, LFGA, LFGM3, LFGA3, LFTM, LFTA, LOR, LDR, LAst, LTO, LStl, LBlk, LPF,ds)]
datal = datal[,Score_dif := - data$Score_dif]
rm(data)
# changing names to match before appending
setnames(dataw,c('Season','DayNum','WLoc','NumOT','WTeamID','WScore','WFGM','WFGA','WFGM3','WFGA3','WFTM','WFTA','WOR','WDR','WAst','WTO','WStl','WBlk','WPF','Score_dif','ds')
,c('Season','DayNum','WLoc','NumOT','TeamID','Score','FGM','FGA','FGM3','FGA3','FTM','FTA','OR','DR','Ast','TO','Stl','Blk','PF','Score_dif','ds'))
setnames(datal,c('Season','DayNum','WLoc','NumOT','LTeamID','LScore','LFGM','LFGA','LFGM3','LFGA3','LFTM','LFTA','LOR','LDR','LAst','LTO','LStl','LBlk','LPF','Score_dif','ds')
,c('Season','DayNum','WLoc','NumOT','TeamID','Score','FGM','FGA','FGM3','FGA3','FTM','FTA','OR','DR','Ast','TO','Stl','Blk','PF','Score_dif','ds'))
# appending datasets
if (exists('MatchResults') && is.data.frame(get('MatchResults'))) {
MatchResults = rbind(MatchResults,dataw,datal)
} else {
MatchResults = rbind(dataw,datal)
}
print(paste0('dataset created for ',df))
rm(dataw,datal)
}
rm(df)
## getting all details at team level
# Team features
# Number of matches played, max days survived in season, Number of wins
MatchResults = as.data.table(MatchResults)
MatchResults = MatchResults[,ID := paste(Season,TeamID)]
Teamf1 = MatchResults[Season <2019,.(Level = max(DayNum),
Num_matches = length(ID),
Num_wins = length(ID[Score_dif>0])),
by = c("TeamID","Season")]
Teamf2 = Teamf1[,.(Level = round(mean(Level),0),
Num_matches = round(mean(Num_matches),0),
Num_wins = round(mean(Num_wins),0) ),
by = c("TeamID")]
##Get teamID - key variable() as base dataset
Teamf = Teamf2
rm(Teamf1,Teamf2)
#get average rank
MMasseyOrdinals <-
read.csv("MMasseyOrdinals.csv", stringsAsFactors = FALSE)
MMasseyOrdinals = as.data.table(MMasseyOrdinals)
Teamf1 = MMasseyOrdinals[,.(OrdinalRank = mean(OrdinalRank)),
by = c("TeamID","Season")]
Teamf2 = Teamf1[,.(OrdinalRank = round(mean(OrdinalRank),0)),
by = c("TeamID")]
setkeyv(Teamf2,c("TeamID"))
setkeyv(Teamf,c("TeamID"))
Teamf = Teamf2[Teamf]
rm(Teamf1,Teamf2)
rm(MMasseyOrdinals)
# Tourney games score
MSecondaryTourneyCompactResults <-
read.csv("MSecondaryTourneyCompactResults.csv", stringsAsFactors = FALSE)
MSecondaryTourneyCompactResults = as.data.table(MSecondaryTourneyCompactResults)
MST_win = MSecondaryTourneyCompactResults[Season >=2003 & Season <= 2018,]
MST_win = MST_win[,":="(Score=(WScore - LScore))]
MST_win = MST_win[,c("WTeamID","SecondaryTourney","Score")]
setnames(MST_win,c("WTeamID","SecondaryTourney","Score"),c("TeamID","SecondaryTourney","Score"))
MST_los = MSecondaryTourneyCompactResults[Season >=2003 & Season <= 2018,]
MST_los = MST_los[,":="(Score=(LScore - WScore))]
MST_los = MST_los[,c("LTeamID","SecondaryTourney","Score")]
setnames(MST_los,c("LTeamID","SecondaryTourney","Score"),c("TeamID","SecondaryTourney","Score"))
MST = rbind(MST_win,MST_los)
rm(MST_win,MST_los)
MST$SecondaryTourney = trimws(MST$SecondaryTourney)
Teamf1 = MST[,.(Score = round(mean(Score),0)), by = c("TeamID","SecondaryTourney")]
Teamf1w <- dcast(Teamf1, TeamID ~ SecondaryTourney , value.var="Score", fill = 0)
Teamf1w = as.data.table(Teamf1w)
setkeyv(Teamf1w,c("TeamID"))
setkeyv(Teamf,c("TeamID"))
Teamf = Teamf1w[Teamf]
rm(Teamf1,Teamf1w,MST)
rm(MSecondaryTourneyCompactResults)
#Coach influence
MTeamCoaches <-
read.csv("MTeamCoaches.csv", stringsAsFactors = FALSE)
MTeamCoaches = as.data.table(MTeamCoaches)
MTeamCoaches = MTeamCoaches[Season >= 2003,]
Teamf1 = MTeamCoaches[,.(Num_coaches = uniqueN(CoachName))
, by = c("TeamID")]
setkeyv(Teamf1,c("TeamID"))
setkeyv(Teamf,c("TeamID"))
Teamf = Teamf1[Teamf]
rm(Teamf1,MTeamCoaches)
Teamf[is.na(Teamf)] = 0
save(Teamf,file = "Teamfactors.rda")
rm(list = ls(all.names = TRUE))
02. Model datasets creation
# loading libraries
library(data.table)
library(dplyr)
# setting working directory
setwd(
"C:/Users/Susmitha Reddy/Desktop/ms project/march-madness-analytics-2020/MDataFiles_Stage2"
)
# Importing data
# getting season, tournament and secondary tourney match datasets
MNCAATourneyDetailedResults <-
read.csv("MNCAATourneyDetailedResults.csv", stringsAsFactors = FALSE)
MRegularSeasonDetailedResults <-
read.csv("MRegularSeasonDetailedResults.csv", stringsAsFactors = FALSE)
pre_2019 = rbind(MNCAATourneyDetailedResults,MRegularSeasonDetailedResults)
pre_2019 = as.data.table(pre_2019)
pre_2019 = pre_2019[Season >= 2003 & Season <= 2018,]
# Getting data to XY type
data = pre_2019[,-c("WLoc")]
data_ai = data[WTeamID < LTeamID,]
data_flip = data[WTeamID > LTeamID,]
setnames(data_ai,colnames(data_ai),
c("Season", "DayNum", "XTeamID", "XScore", "YTeamID", "YScore", "NumOT", "XFGM", "XFGA", "XFGM3", "XFGA3", "XFTM", "XFTA", "XOR", "XDR", "XAst", "XTO", "XStl", "XBlk", "XPF", "YFGM", "YFGA", "YFGM3", "YFGA3", "YFTM", "YFTA", "YOR", "YDR", "YAst", "YTO", "YStl", "YBlk", "YPF"))
setnames(data_flip,colnames(data_flip),
c("Season", "DayNum", "YTeamID", "YScore", "XTeamID", "XScore", "NumOT", "YFGM", "YFGA", "YFGM3", "YFGA3", "YFTM", "YFTA", "YOR", "YDR", "YAst", "YTO", "YStl", "YBlk", "YPF", "XFGM", "XFGA", "XFGM3", "XFGA3", "XFTM", "XFTA", "XOR", "XDR", "XAst", "XTO", "XStl", "XBlk", "XPF"))
# re-ordering column names before appending
data_flip = data_flip[,c("Season", "DayNum", "XTeamID", "XScore", "YTeamID", "YScore", "NumOT", "XFGM", "XFGA", "XFGM3", "XFGA3", "XFTM", "XFTA", "XOR", "XDR", "XAst", "XTO", "XStl", "XBlk", "XPF", "YFGM", "YFGA", "YFGM3", "YFGA3", "YFTM", "YFTA", "YOR", "YDR", "YAst", "YTO", "YStl", "YBlk", "YPF")]
rm(data)
data = rbind(data_ai,data_flip)
rm(data_ai,data_flip)
data = data[,':='(Score = XScore - YScore,
FGM = XFGM - YFGM,
FGA = XFGA - YFGA,
FGM3 = XFGM3 - YFGM3,
FGA3 = XFGA3 - YFGA3,
FTM = XFTM - YFTM,
FTA = XFTA - YFTA,
OR = XOR - YOR,
DR = XDR - YDR,
Ast = XAst - YAst,
TO = XTO - YTO,
Stl = XStl - YStl,
Blk = XBlk - YBlk,
PF = XPF - YPF,
WLY = ifelse(XScore > YScore,1,0))]
data_season = data[,.(Score = round(mean(Score),0),
FGM = round(mean(FGM),0),
FGA = round(mean(FGA),0),
FGM3 = round(mean(FGM3),0),
FGA3 = round(mean(FGA3),0),
FTM = round(mean(FTM),0),
FTA = round(mean(FTA),0),
OR = round(mean(OR),0),
DR = round(mean(DR),0),
Ast = round(mean(Ast),0),
TO = round(mean(TO),0),
Stl = round(mean(Stl),0),
Blk = round(mean(Blk),0),
PF = round(mean(PF),0),
WLY = ifelse(mean(WLY)==0.5,
ifelse(mean(Score)==0,
ifelse(sum(Score*DayNum)>0,1,0),
ifelse(mean(Score)>0,1,0) ),
round(mean(WLY),0) ) )
, by = c("Season","XTeamID","YTeamID")]
load("Teamfactors.rda")
setkeyv(data_season,c("XTeamID"))
setkeyv(Teamf,c("TeamID"))
data_season = Teamf[data_season]
setnames(data_season,colnames(Teamf),
c("XTeamID", "XNum_coaches", "XCBI", "XCIT", "XNIT", "XV16", "XOrdinalRank", "XLevel", "XNum_matches", "XNum_wins"))
setkeyv(data_season,c("YTeamID"))
setkeyv(Teamf,c("TeamID"))
data_season = Teamf[data_season]
setnames(data_season,colnames(Teamf),
c("YTeamID", "YNum_coaches", "YCBI", "YCIT", "YNIT", "YV16", "YOrdinalRank", "YLevel", "YNum_matches", "YNum_wins"))
#calculating relative values X - Y
data_season = data_season[,':='(Num_coaches = XNum_coaches - YNum_coaches,
CBI = XCBI - YCBI,
CIT = XCIT - YCIT,
NIT = XNIT - YNIT,
V16 = XV16 - YV16,
OrdinalRank = XOrdinalRank - YOrdinalRank,
Level = XLevel - YLevel,
Num_matches = XNum_matches - YNum_matches,
Num_wins = XNum_wins - YNum_wins,
MID = paste(Season,XTeamID,YTeamID, sep = "_"))]
#removing columns not required and re ordering
data_season = data_season[,c("Score", "FGM", "FGA", "FGM3", "FGA3", "FTM", "FTA", "OR", "DR", "Ast", "TO", "Stl", "Blk", "PF",
"Num_coaches", "CBI", "CIT", "NIT", "V16", "OrdinalRank", "Level", "Num_matches", "Num_wins",
"WLY","Season","XTeamID","YTeamID","MID")]
pre_2019 = data_season
rm(data,data_season)
# ######################################################
## getting model data ready for prediction
MRegularSeasonDetailedResults = as.data.table(MRegularSeasonDetailedResults)
season_2019 = MRegularSeasonDetailedResults[Season == 2019,]
# Getting data to XY type
data = season_2019[,-c("WLoc")]
data_ai = data[WTeamID < LTeamID,]
data_flip = data[WTeamID > LTeamID,]
setnames(data_ai,colnames(data_ai),
c("Season", "DayNum", "XTeamID", "XScore", "YTeamID", "YScore", "NumOT", "XFGM", "XFGA", "XFGM3", "XFGA3", "XFTM", "XFTA", "XOR", "XDR", "XAst", "XTO", "XStl", "XBlk", "XPF", "YFGM", "YFGA", "YFGM3", "YFGA3", "YFTM", "YFTA", "YOR", "YDR", "YAst", "YTO", "YStl", "YBlk", "YPF"))
setnames(data_flip,colnames(data_flip),
c("Season", "DayNum", "YTeamID", "YScore", "XTeamID", "XScore", "NumOT", "YFGM", "YFGA", "YFGM3", "YFGA3", "YFTM", "YFTA", "YOR", "YDR", "YAst", "YTO", "YStl", "YBlk", "YPF", "XFGM", "XFGA", "XFGM3", "XFGA3", "XFTM", "XFTA", "XOR", "XDR", "XAst", "XTO", "XStl", "XBlk", "XPF"))
# re-ordering column names before appending
data_flip = data_flip[,c("Season", "DayNum", "XTeamID", "XScore", "YTeamID", "YScore", "NumOT", "XFGM", "XFGA", "XFGM3", "XFGA3", "XFTM", "XFTA", "XOR", "XDR", "XAst", "XTO", "XStl", "XBlk", "XPF", "YFGM", "YFGA", "YFGM3", "YFGA3", "YFTM", "YFTA", "YOR", "YDR", "YAst", "YTO", "YStl", "YBlk", "YPF")]
rm(data)
data = rbind(data_ai,data_flip)
rm(data_ai,data_flip)
data = data[,':='(Score = XScore - YScore,
FGM = XFGM - YFGM,
FGA = XFGA - YFGA,
FGM3 = XFGM3 - YFGM3,
FGA3 = XFGA3 - YFGA3,
FTM = XFTM - YFTM,
FTA = XFTA - YFTA,
OR = XOR - YOR,
DR = XDR - YDR,
Ast = XAst - YAst,
TO = XTO - YTO,
Stl = XStl - YStl,
Blk = XBlk - YBlk,
PF = XPF - YPF,
WLY = ifelse(XScore > YScore,1,0))]
data_season = data[,.(Score = round(mean(Score),0),
FGM = round(mean(FGM),0),
FGA = round(mean(FGA),0),
FGM3 = round(mean(FGM3),0),
FGA3 = round(mean(FGA3),0),
FTM = round(mean(FTM),0),
FTA = round(mean(FTA),0),
OR = round(mean(OR),0),
DR = round(mean(DR),0),
Ast = round(mean(Ast),0),
TO = round(mean(TO),0),
Stl = round(mean(Stl),0),
Blk = round(mean(Blk),0),
PF = round(mean(PF),0),
WLY = ifelse(mean(WLY)==0.5,
ifelse(mean(Score)==0,
ifelse(sum(Score*DayNum)>0,1,0),
ifelse(mean(Score)>0,1,0) ),
round(mean(WLY),0) ) )
, by = c("Season","XTeamID","YTeamID")]
setkeyv(data_season,c("XTeamID"))
setkeyv(Teamf,c("TeamID"))
data_season = Teamf[data_season]
setnames(data_season,colnames(Teamf),
c("XTeamID", "XNum_coaches", "XCBI", "XCIT", "XNIT", "XV16", "XOrdinalRank", "XLevel", "XNum_matches", "XNum_wins"))
setkeyv(data_season,c("YTeamID"))
setkeyv(Teamf,c("TeamID"))
data_season = Teamf[data_season]
setnames(data_season,colnames(Teamf),
c("YTeamID", "YNum_coaches", "YCBI", "YCIT", "YNIT", "YV16", "YOrdinalRank", "YLevel", "YNum_matches", "YNum_wins"))
#calculating relative values X - Y
data_season = data_season[,':='(Num_coaches = XNum_coaches - YNum_coaches,
CBI = XCBI - YCBI,
CIT = XCIT - YCIT,
NIT = XNIT - YNIT,
V16 = XV16 - YV16,
OrdinalRank = XOrdinalRank - YOrdinalRank,
Level = XLevel - YLevel,
Num_matches = XNum_matches - YNum_matches,
Num_wins = XNum_wins - YNum_wins,
MID = paste(Season,XTeamID,YTeamID, sep = "_"))]
#removing columns not required and re ordering
data_season = data_season[,c("Score", "FGM", "FGA", "FGM3", "FGA3", "FTM", "FTA", "OR", "DR", "Ast", "TO", "Stl", "Blk", "PF",
"Num_coaches", "CBI", "CIT", "NIT", "V16", "OrdinalRank", "Level", "Num_matches", "Num_wins",
"WLY","Season","XTeamID","YTeamID","MID")]
season_2019 = data_season
rm(data,data_season)
save(pre_2019,file = "pre2019.rda")
save(season_2019,file = "season2019.rda")
rm(list = ls(all.names = TRUE))
03. Variable selection
# Importing libraries
library(data.table)
library(dplyr)
library(reshape2)
library(caret)
library(car)
library(e1071)
library(Hmisc)
# setting working directory
setwd(
"C:/Users/Susmitha Reddy/Desktop/ms project/march-madness-analytics-2020/MDataFiles_Stage2"
)
# Importing data
load("pre2019.rda")
# removing team details
# removing redundant variables
modelbase = pre_2019[,-c(1,3,5,7,25:28)]
rm(pre_2019)
## creating train and test datasets as required
temp = sample(1:nrow(modelbase), size = round(0.7*nrow(modelbase)))
train = modelbase[temp,]
test = modelbase[-temp,]
rm(temp)
## normalizing the dataset
norm_val=preProcess(train[, -c(20)], method=c("center", "scale"))
train_norm = as.data.frame(predict(norm_val,train))
test_norm = as.data.frame(predict(norm_val,test))
rm(test,train,norm_val)
# correlation matrix
flattenCorrMatrix <- function(cormat, pmat) {
ut <- upper.tri(cormat)
data.frame(
row = rownames(cormat)[row(cormat)[ut]],
column = rownames(cormat)[col(cormat)[ut]],
cor =(cormat)[ut],
p = pmat[ut]
)
}
res <- rcorr(as.matrix(train_norm))
corresults = flattenCorrMatrix(res$r, res$P)
corresults = corresults[(corresults$row == 'WLY' | corresults$column=='WLY')&(abs(corresults$cor)> 0.5),]
modelvariables = setdiff(colnames(train_norm),corresults[,c("row")])
rm(corresults,res,flattenCorrMatrix)
# checking vif for selected variables
model = glm(WLY~.,family = binomial(link = "logit"),maxit=100,data = train_norm[,c(modelvariables)])
summary(model)
car::vif(model)
## check for vif higher than 5
save(modelvariables,file = "modelvariables.rda")
save(train_norm,file = "train_norm.rda")
save(test_norm,file = "test_norm.rda")
rm(list = ls(all.names = TRUE))
04. Data modelling
# Importing libraries
library(data.table)
library(dplyr)
library(reshape2)
library(caret)
library(car)
library(e1071)
library(Hmisc)
library(glmnet)
library(randomForest)
library(gbm)
library(naivebayes)
library(caTools)
library(MASS)
# setting working directory
setwd(
"C:/Users/Susmitha Reddy/Desktop/ms project/march-madness-analytics-2020/MDataFiles_Stage2"
)
# Importing data
load("train_norm.rda")
load("test_norm.rda")
load("modelvariables.rda")
results = NULL
results = as.data.table(results)
pred_results = NULL
pred_results = as.data.table(pred_results)
train_fin = train_norm[,c(modelvariables)]
test_fin = test_norm[,c(modelvariables)]
train_fin = as.data.table(train_fin)
test_fin =as.data.table(test_fin)
rm(train_norm,test_norm)
## Logistic regression
logreg = glm(WLY~.,family = binomial(link = "logit"),maxit=100,data = train_fin)
summary(logreg)
cf = confusionMatrix(table(predict(logreg,test_fin, type="response") > 0.5,
test_fin$WLY == 1))
logreg_acc = sum(diag(table(predict(logreg,test_fin, type="response") > 0.5,test_fin$WLY == 1))) / sum(table(predict(logreg,test_fin, type="response") > 0.5,test_fin$WLY == 1))
results$Model = "Log regression"
results$Accuracy = logreg_acc
pred_results$variables = row.names(as.data.frame(logreg$coefficients))
pred_results$logreg_coeff = logreg$coefficients
rm(logreg_acc,logreg,cf)
# glmnet
trainmatrix = as.matrix(train_fin)
testmatrix = as.matrix(test_fin)
glm_reg = glmnet(trainmatrix[,colnames(trainmatrix) != "WLY"],
trainmatrix[,colnames(trainmatrix) == "WLY"],
family = "binomial", relax=TRUE, path=TRUE)
confusionMatrix(table(predict(glm_reg, s=0.01, testmatrix[,colnames(testmatrix) != "WLY"],
type="response") > 0.5,
testmatrix[,colnames(testmatrix) == "WLY"] == 1))
pred = table(predict(glm_reg, s=0.01, testmatrix[,colnames(testmatrix) != "WLY"],
type="response") > 0.5,testmatrix[,colnames(testmatrix) == "WLY"] == 1)
glmreg_acc = sum(diag(pred)) / sum(pred)
#Appending results
results <- rbind(results,data.frame(Model ="glmnet regression", Accuracy = glmreg_acc))
pred_results$glm_coeff = as.data.frame(coef(glm_reg, s = 0.01)[,1])
rm(glm_reg,glmreg_acc,trainmatrix,testmatrix,pred)
#Randomforest
rand_for = randomForest(as.factor(train_fin$WLY)~., data=train_fin)
# , ntree=30, proximity=T)
pred = table(predict(rand_for,test_fin), test_fin$WLY)
ranfor_acc = sum(diag(pred)) / sum(pred)
#Appending results
results <- rbind(results,data.frame(Model ="RandForest regression", Accuracy = ranfor_acc))
rm(rand_for,pred,ranfor_acc)
#Gradient boosting
grad_boos = gbm(WLY ~., data = train_fin,distribution = "gaussian",cv.folds = 10,
shrinkage = .01,n.minobsinnode = 10,n.trees = 200)
pred = table(predict.gbm(object = grad_boos, newdata = test_fin,n.trees = 200, type = "response")>0.5, test_fin$WLY)
gradboos_acc = sum(diag(pred)) / sum(pred)
#Appending results
results <- rbind(results,data.frame(Model ="Grad Boost regression", Accuracy = gradboos_acc))
rm(grad_boos,pred,gradboos_acc)
#Naive Bayes
nb = naive_bayes(as.factor(WLY) ~ . , data = train_fin,usekernel = T)
pred = table(predict(nb,test_fin[,-c("WLY")],type = "prob")[,1] > 0.5,test_fin$WLY == 1)
nb_acc = sum(diag(pred)) / sum(pred)
#Appending results
results <- rbind(results,data.frame(Model ="Naive bayes", Accuracy = nb_acc))
rm(nb,pred,nb_acc)
# Support vector machines
svm_class = svm( as.factor(WLY) ~ ., data = train_fin, type = 'C-classification', kernel = 'linear')
pred = table(test_fin$WLY, predict(svm_class, newdata = test_fin[, -c("WLY")]))
nb_svm = sum(diag(pred)) / sum(pred)
#Appending results
results <- rbind(results,data.frame(Model ="Support Vector Machine", Accuracy = nb_svm))
rm(svm_class,pred,nb_svm)
# SVM Radial kernel
svm_radial = svm( as.factor(WLY) ~ ., data = train_fin, kernel = "radial", cost = 5)
pred = table(test_fin$WLY, predict(svm_radial, newdata = test_fin[, -c("WLY")]))
nb_svm_rad = sum(diag(pred)) / sum(pred)
#Appending results
results <- rbind(results,data.frame(Model ="Support Vector Radial", Accuracy = nb_svm_rad))
rm(svm_radial,pred,nb_svm_rad)
#LDA
lda = lda(WLY~., data = train_fin)
pred = table(predict(lda,test_fin[,-c("WLY")])$posterior[,1] > 0.5,test_fin$WLY == 1)
lda_acc = sum(diag(pred)) / sum(pred)
#Appending results
results <- rbind(results,data.frame(Model ="Linear Discriminant Analysis", Accuracy = lda_acc))
rm(pred,lda,lda_acc)
write.csv(results,"Modelresults.csv", row.names = FALSE)
rm(list = ls(all.names = TRUE))
05. 2019 Model data setup
# loading libraries
library(data.table)
library(dplyr)
# setting working directory
setwd(
"C:/Users/Susmitha Reddy/Desktop/ms project/march-madness-analytics-2020/MDataFiles_Stage2"
)
# Importing data
# getting season, tournament and secondary tourney match datasets
MNCAATourneyDetailedResults <-
read.csv("MNCAATourneyDetailedResults.csv", stringsAsFactors = FALSE)
MRegularSeasonDetailedResults <-
read.csv("MRegularSeasonDetailedResults.csv", stringsAsFactors = FALSE)
MNCAATourneyDetailedResults = as.data.table(MNCAATourneyDetailedResults)
MRegularSeasonDetailedResults = as.data.table(MRegularSeasonDetailedResults)
load("autoselected.rda")
load("2019allteams.rda")
load("atlargeoptions.rda")
# Getting data ready for predicting next 36 teams and final predictions
data2019_seas = MRegularSeasonDetailedResults[Season == 2019,]
teamX = data2019_seas[,c('Season','WTeamID','WLoc','NumOT','WScore','WFGM','WFGA','WFGM3','WFGA3','WFTM','WFTA','WOR','WDR','WAst','WTO','WStl','WBlk','WPF')]
teamY = data2019_seas[,c('Season','LTeamID','WLoc','NumOT','LScore','LFGM','LFGA','LFGM3','LFGA3','LFTM','LFTA','LOR','LDR','LAst','LTO','LStl','LBlk','LPF')]
teamY = teamY[,":="(WLoc = ifelse(WLoc == "H","A",ifelse(WLoc=="A","H","N")))]
setnames(teamY,c('Season','LTeamID','WLoc','NumOT','LScore','LFGM','LFGA','LFGM3','LFGA3','LFTM','LFTA','LOR','LDR','LAst','LTO','LStl','LBlk','LPF'),
c('Season','WTeamID','WLoc','NumOT','WScore','WFGM','WFGA','WFGM3','WFGA3','WFTM','WFTA','WOR','WDR','WAst','WTO','WStl','WBlk','WPF'))
team_match_all = rbind(teamX,teamY)
setnames(team_match_all,colnames(team_match_all),c("Season", "TeamID", "WLoc", "NumOT", "Score", "FGM", "FGA", "FGM3", "FGA3", "FTM", "FTA", "OR", "DR", "Ast", "TO", "Stl", "Blk", "PF"))
team_all = team_match_all[,.(NumOT = round(mean(NumOT),0),
Score = round(mean(Score),0),
FGM = round(mean(FGM),0),
FGA = round(mean(FGA),0),
FGM3 = round(mean(FGM3),0),
FGA3 = round(mean(FGA3),0),
FTM = round(mean(FTM),0),
FTA = round(mean(FTA),0),
OR = round(mean(OR),0),
DR = round(mean(DR),0),
Ast = round(mean(Ast),0),
TO = round(mean(TO),0),
Stl = round(mean(Stl),0),
Blk = round(mean(Blk),0),
PF = round(mean(PF),0),
A = length(TeamID[WLoc == "A"]),
H = length(TeamID[WLoc == "H"]),
N = length(TeamID[WLoc == "N"]))
,by = c("Season","TeamID")]
team_all = team_all[,":="(WLoc = ifelse((H >= A & H>=N),"H",ifelse((A>=H & A>=N),"A","N")))]
team_all = team_all[,":="(A = NULL,H = NULL, N = NULL)]
rm(teamX,teamY,team_match_all)
## creating all team combinations
temp = team_all[,c("Season","TeamID")]
setkeyv(temp,c("Season"))
cross = temp[temp,allow.cartesian=TRUE]
setnames(cross, colnames(cross),c("Season","TeamX","TeamY"))
cross = cross[TeamX < TeamY,]
rm(temp)
setkeyv(cross,c("TeamX"))
setkeyv(team_all,c("TeamID"))
base2019 = team_all[cross]
setnames(base2019,c("Season", "TeamID", "NumOT", "Score", "FGM", "FGA", "FGM3", "FGA3", "FTM", "FTA",
"OR", "DR", "Ast", "TO", "Stl", "Blk", "PF", "WLoc", "i.Season", "TeamY")
,c("Season", "TeamX", "XNumOT", "XScore", "XFGM", "XFGA", "XFGM3", "XFGA3", "XFTM", "XFTA", "XOR", "XDR", "XAst", "XTO", "XStl", "XBlk", "XPF", "XWLoc", "i.Season", "TeamY"))
base2019$i.Season = NULL
setkeyv(base2019,c("TeamY"))
setkeyv(team_all,c("TeamID"))
base2019 = team_all[base2019]
setnames(base2019,c("Season", "TeamID", "NumOT", "Score", "FGM", "FGA", "FGM3", "FGA3", "FTM", "FTA", "OR", "DR", "Ast", "TO", "Stl", "Blk", "PF", "WLoc", "i.Season", "TeamX")
,c("Season", "TeamY", "YNumOT", "YScore", "YFGM", "YFGA", "YFGM3", "YFGA3", "YFTM", "YFTA", "YOR", "YDR", "YAst", "YTO", "YStl", "YBlk", "YPF", "YWLoc", "i.Season", "TeamX"))
base2019$i.Season = NULL
#Now calculate relative scores for all metrics
base2019 = base2019[,':='(Score = XScore - YScore,
FGM = XFGM - YFGM,
FGA = XFGA - YFGA,
FGM3 = XFGM3 - YFGM3,
FGA3 = XFGA3 - YFGA3,
FTM = XFTM - YFTM,
FTA = XFTA - YFTA,
OR = XOR - YOR,
DR = XDR - YDR,
Ast = XAst - YAst,
TO = XTO - YTO,
Stl = XStl - YStl,
Blk = XBlk - YBlk,
PF = XPF - YPF,
WLY = ifelse((XScore > YScore),1,0),
NumOT = ifelse((XScore > YScore),XNumOT,YNumOT),
WLoc = ifelse((XScore > YScore),XWLoc,YWLoc))]
base2019 = base2019[,c("Season","TeamX","TeamY","WLoc", "NumOT","Score", "FGM", "FGA", "FGM3", "FGA3", "FTM", "FTA", "OR", "DR", "Ast", "TO", "Stl", "Blk", "PF","WLY")]
base2019 = base2019[,':='(MID = paste(Season,TeamX,TeamY, sep = "_"))]
rm(cross)
# getting extra factors
load("Teamfactors.rda")
setkeyv(base2019,c("TeamX"))
setkeyv(Teamf,c("TeamID"))
base2019 = Teamf[base2019]
setnames(base2019,c("Num_coaches", "CBI", "CIT", "NIT", "V16", "OrdinalRank", "Level", "Num_matches", "Num_wins")
,c("XNum_coaches", "XCBI", "XCIT", "XNIT", "XV16", "XOrdinalRank", "XLevel", "XNum_matches", "XNum_wins"))
setkeyv(base2019,c("TeamY"))
setkeyv(Teamf,c("TeamID"))
base2019 = Teamf[base2019]
setnames(base2019,c("Num_coaches", "CBI", "CIT", "NIT", "V16", "OrdinalRank", "Level", "Num_matches", "Num_wins")
,c("YNum_coaches", "YCBI", "YCIT", "YNIT", "YV16", "YOrdinalRank", "YLevel", "YNum_matches", "YNum_wins"))
base2019 = base2019[,":="(Num_coaches = XNum_coaches - YNum_coaches,
CBI = XCBI - YCBI,
CIT = XCIT - YCIT,
NIT = XNIT - YNIT,
V16 = XV16 - YV16,
OrdinalRank = XOrdinalRank - YOrdinalRank,
Level = XLevel - YLevel,
Num_matches = XNum_matches - YNum_matches,
Num_wins = XNum_wins - YNum_wins)]
base2019 = base2019[,c("YNum_coaches", "YCBI", "YCIT", "YNIT", "YV16", "YOrdinalRank", "YLevel", "YNum_matches", "YNum_wins",
"XNum_coaches", "XCBI", "XCIT", "XNIT", "XV16", "XOrdinalRank", "XLevel", "XNum_matches", "XNum_wins")
:= NULL]
base2019$Neutral = ifelse(base2019$WLoc == 'N',1,0)
base2019$Home = ifelse(base2019$WLoc == 'H',1,0)
setnames(base2019,c("i.TeamID","TeamID"),c("TeamX","TeamY"))
rm(Teamf)
## 2019 Seasonal data for the purpose of modelling is ready
#saving datasets
save(base2019,file = "Modelbase2019_season.rda")
rm(list = ls(all.names = TRUE))
06. All teams prediction
# Importing libraries
library(data.table)
library(dplyr)
library(reshape2)
library(caret)
library(car)
library(e1071)
library(glmnet)
library(randomForest)
library(caTools)
library(MASS)
# setting working directory
setwd(
"C:/Users/Susmitha Reddy/Desktop/ms project/march-madness-analytics-2020/MDataFiles_Stage2"
)
## loading data
load("Modelbase2019_season.rda")
load("modelvariables.rda")
load("train_norm.rda")
load("test_norm.rda")
##################################################
#Modelling for all teams
#removing columns not required and re ordering
all_base_model = base2019[,c("Score", "FGM", "FGA", "FGM3", "FGA3", "FTM", "FTA", "OR", "DR", "Ast", "TO", "Stl", "Blk", "PF",
"Num_coaches", "CBI", "CIT", "NIT", "V16", "OrdinalRank", "Level", "Num_matches", "Num_wins",
"WLY","Season","TeamX","TeamY","MID")]
#removing teams due to lack of data - These are new teams
all_base_model = all_base_model[!(TeamX %in% c("1465","1466","1467") | TeamY %in% c("1465","1466","1467"))
,-c(25:28)]
Results = base2019[!(TeamX %in% c("1465","1466","1467") | TeamY %in% c("1465","1466","1467"))
,c("MID")]
## normalizing the dataset
norm_val=preProcess(all_base_model[, -c(24)], method=c("center", "scale"))
all_base_model_norm = as.data.frame(predict(norm_val,all_base_model))
rm(norm_val,base2019,all_base_model)
#model_data
model_data = rbind(train_norm[,c(modelvariables)],test_norm[,c(modelvariables)])
rm(train_norm,test_norm)
#prediction data
pred_data = all_base_model_norm[,c(modelvariables)]
pred_data = as.data.table(pred_data)
rm(all_base_model_norm)
# glmnet
model_data_mat = as.matrix(model_data)
pred_data_mat = as.matrix(pred_data)
glm_reg = glmnet(model_data_mat[,colnames(model_data_mat) != "WLY"],
model_data_mat[,colnames(model_data_mat) == "WLY"],
family = "binomial", relax=TRUE, path=TRUE)
pred_glm_reg = ifelse(predict(glm_reg, s=0.01, pred_data_mat[,colnames(pred_data_mat) != "WLY"],
type="response")>0.5 , 1, 0)
rm(glm_reg,model_data_mat, pred_data_mat)
# SVM linear kernel
svm_class = svm( as.factor(WLY) ~ ., data = model_data, type = 'C-classification', kernel = 'linear')
pred_svm = predict(svm_class, newdata = pred_data[, -c("WLY")])
rm(svm_class)
# SVM radial
svm_radial = svm( as.factor(WLY) ~ ., data = model_data, kernel = "radial", cost = 5)
pred_svm_radial = predict(svm_radial, newdata = pred_data[, -c("WLY")])
rm(svm_radial)
#Randomforest
rand_for = randomForest(as.factor(model_data$WLY)~., data=model_data)
pred_randf = predict(rand_for,pred_data[, -c("WLY")])
## Logistic regression
logreg = glm(WLY~.,family = binomial(link = "logit"),maxit=100,data = model_data)
pred_logreg = ifelse(predict(logreg,pred_data[, -c("WLY")], type="response") > 0.5,1,0)
###################################################
#Adding predictions to the dataset
Results$pred_svm = pred_svm
Results$pred_randf = pred_randf
Results$pred_logreg = pred_logreg
Results$pred_svm_radial = pred_svm_radial
Results$pred_glm_reg = pred_glm_reg
write.csv(Results,"Results.csv",row.names = F)
save(Results, file = "results_allteams_0412.rda")
07. Performance Evaluation
# Importing libraries
library(data.table)
library(dplyr)
# setting working directory
setwd(
"C:/Users/Susmitha Reddy/Desktop/ms project/march-madness-analytics-2020/MDataFiles_Stage2"
)
#getting data
# load("results_0412.rda")
load("results_allteams_0412.rda")
# actual match results
MNCAATourneyDetailedResults <-
read.csv("MNCAATourneyDetailedResults.csv", stringsAsFactors = FALSE)
MNCAATourneyDetailedResults = as.data.table(MNCAATourneyDetailedResults)
Matchdata = MNCAATourneyDetailedResults[Season == 2019,]
rm(MNCAATourneyDetailedResults)
Matchdata = Matchdata[,':='(Score = WScore - LScore,
FGM = WFGM - LFGM,
FGA = WFGA - LFGA,
FGM3 = WFGM3 - LFGM3,
FGA3 = WFGA3 - LFGA3,
FTM = WFTM - LFTM,
FTA = WFTA - LFTA,
OR = WOR - LOR,
DR = WDR - LDR,
Ast = WAst - LAst,
TO = WTO - LTO,
Stl = WStl - LStl,
Blk = WBlk - LBlk,
PF = WPF - LPF,
WLY = 1)]
Matchdata_ai = Matchdata[WTeamID < LTeamID,c("Season","WTeamID","LTeamID","WLoc", "NumOT","Score", "FGM", "FGA", "FGM3", "FGA3", "FTM", "FTA", "OR", "DR", "Ast", "TO", "Stl", "Blk", "PF","WLY")]
Matchdata_flip = Matchdata[(WTeamID > LTeamID),]
Matchdata_flip = Matchdata_flip[,':='(Season1 = Season,
WTeamID1 = LTeamID,
LTeamID1 = WTeamID,
WLoc1 = WLoc,
NumOT1 = NumOT,
Score1 = -Score,
LFGM1 = -LFGM,
LFGA1 = -LFGA,
LFGM31 = -LFGM3,
LFGA31 = -LFGA3,
LFTM1 = -LFTM,
LFTA1 = -LFTA,
LOR1 = -LOR,
LDR1 = -LDR,
LAst1 = -LAst,
LTO1 = -LTO,
LStl1 = -LStl,
LBlk1 = -LBlk,
LPF1 = -LPF,
WLY1 = 0)]
Matchdata_flip = Matchdata_flip[,c("Season1", "WTeamID1", "LTeamID1", "WLoc1", "NumOT1", "Score1", "LFGM1", "LFGA1", "LFGM31", "LFGA31", "LFTM1", "LFTA1", "LOR1", "LDR1", "LAst1", "LTO1", "LStl1", "LBlk1", "LPF1", "WLY1")]
setnames(Matchdata_flip,
c("Season1", "WTeamID1", "LTeamID1", "WLoc1", "NumOT1", "Score1", "LFGM1", "LFGA1", "LFGM31", "LFGA31", "LFTM1", "LFTA1", "LOR1", "LDR1", "LAst1", "LTO1", "LStl1", "LBlk1", "LPF1", "WLY1"),
c("Season","WTeamID","LTeamID","WLoc", "NumOT","Score", "FGM", "FGA", "FGM3", "FGA3", "FTM", "FTA", "OR", "DR", "Ast", "TO", "Stl", "Blk", "PF","WLY")
)
rm(Matchdata)
Matchdata = rbind(Matchdata_ai,Matchdata_flip)
Matchdata = Matchdata[,':='(MID = paste(Season,WTeamID,LTeamID, sep = "_"))]
actuals_tourney = Matchdata[,c("MID","WLY")]
rm(Matchdata,Matchdata_ai,Matchdata_flip)
## getting predictions on the comparision data
setkeyv(actuals_tourney,"MID")
setkeyv(Results,"MID")
comp = Results[actuals_tourney, nomatch = 0]
write.csv(comp,"Comparison.csv",row.names = F)
