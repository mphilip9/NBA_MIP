library("openxlsx")
library("dplyr")
library(tidyr)
library(ggplot2)

#Import Data table
Players <- read.xlsx("C:/Users/Maxwell/Documents/R/NBA_MIP/Seasons_Stats.xlsx", 1)
#Cleaning up (Adding PPG, MPG; Removing data prior to 1986; Combining Year with Player name) 
Players <- Players %>% mutate(PPG = PTS/G)
Players <- Players %>% mutate(MPG = MP/G)
Players <- Players %>% subset(Year > 1984)
Players <- Players %>% unite("YearPlayer", Year, Player, sep = "~")

#Importing MIP Candidates
MIP_Candidates <- read.xlsx("C:/Users/Maxwell/Documents/R/NBA_MIP/MIP_candidates.xlsx", 1)
#Cleaning up the MIP candidate table
MIP_Candidates <- MIP_Candidates %>% gather("Year", "Player", na.rm = TRUE)
MIP_Candidates <- MIP_Candidates %>% unite("YearPlayer", Year, Player, sep = "~")

#Adding column for whether or not a player is a MIP candidate or not
MIP_Candidates$MIP_Candidate = "MIP Candidate"
#Joining MIP candidates to Players table
MIP <- left_join(Players, MIP_Candidates, by = "YearPlayer")
#Replacing NA with value in MIP_Candidate column
MIP$MIP_Candidate <- replace_na(MIP$MIP_Candidate, "Not_MIP")
#Removing Blank columns
MIP <- MIP[, -21]
MIP <- MIP[, -25]
#Checking Average minutes and Games played and STD for MIP/Not_MIP
ggplot(MIP, aes(x = MPG, y = PPG)) + geom_point(aes(color = MIP_Candidate))
MIP %>% group_by(MIP_Candidate) %>% summarize(avgmpg = mean(MPG))
MIP %>% group_by(MIP_Candidate) %>% summarize(std = sd(MPG))
MIP %>% group_by(MIP_Candidate) %>% summarize(avgG = mean(G))
MIP %>% group_by(MIP_Candidate) %>% summarize(std = sd(G))
#Removing % sign from variables
grep("%$", colnames(MIP))
colnames(MIP) <- gsub("%", "perc", colnames(MIP))
colnames(MIP) <- gsub("3", "Three", colnames(MIP))
#Removing Dan Dickau's extra stats
MIP <- MIP[-3590, ]
MIP <- MIP[-3591, ]
#Displaying difference from the previous year
MIP <- MIP %>% separate(YearPlayer, into = c("Year", "Player"), sep = "~")
MIP <- MIP %>% arrange(Player, Year) %>% mutate(dif_PPG = ifelse(Player == lag(Player), PPG - lag(PPG), 0)) %>%
  mutate(dif_VORP = ifelse(Player == lag(Player), VORP - lag(VORP), 0)) %>% mutate(dif_MP = ifelse(Player == lag(Player), MP - lag(MP), 0))  %>% 
  mutate(dif_PER = ifelse(Player == lag(Player), PER - lag(PER), 0))  %>% mutate(dif_WS = ifelse(Player == lag(Player), WS - lag(WS), 0))  %>% 
  mutate(dif_FTr = ifelse(Player == lag(Player), FTr - lag(FTr), 0)) %>% mutate(dif_TSperc = ifelse(Player == lag(Player), TSperc - lag(TSperc), 0)) %>% 
  mutate(dif_OWS = ifelse(Player == lag(Player), OWS - lag(OWS), 0)) %>% mutate(dif_DWS = ifelse(Player == lag(Player), DWS - lag(DWS), 0)) %>% 
  mutate(dif_MPG = ifelse(Player == lag(Player), MPG - lag(MPG), 0)) %>% mutate(dif_GS = ifelse(Player == lag(Player), GS - lag(GS), 0)) %>% 
  mutate(dif_G = ifelse(Player == lag(Player), G - lag(G), 0)) %>% mutate(dif_BPM = ifelse(Player == lag(Player), BPM - lag(BPM), 0)) %>% 
  mutate(dif_GS = ifelse(Player == lag(Player), GS - lag(GS), 0)) %>% mutate(dif_ASTperc = ifelse(Player == lag(Player), ASTperc - lag(ASTperc), 0)) %>% 
  mutate(dif_OBPM = ifelse(Player == lag(Player), OBPM - lag(OBPM), 0)) %>% mutate(dif_eFGperc = ifelse(Player == lag(Player), eFGperc - lag(eFGperc), 0)) %>% 
  mutate(dif_FG = ifelse(Player == lag(Player), FG - lag(FG), 0)) %>% mutate(dif_FGperc = ifelse(Player == lag(Player), FGperc - lag(FGperc), 0)) %>% 
  mutate(dif_ThreeP = ifelse(Player == lag(Player), ThreeP - lag(ThreeP), 0)) %>% 
  mutate(dif_ThreePperc = ifelse(Player == lag(Player), ThreePperc - lag(ThreePperc), 0)) %>% mutate(dif_ThreeP = ifelse(Player == lag(Player), ThreeP - lag(ThreeP), 0)) %>% 
  mutate(dif_AST = ifelse(Player == lag(Player), AST - lag(AST), 0)) %>% mutate(dif_STL = ifelse(Player == lag(Player), STL - lag(STL), 0)) %>% 
  mutate(dif_BLK = ifelse(Player == lag(Player), BLK - lag(BLK), 0)) %>% mutate(dif_USGperc = ifelse(Player == lag(Player), USGperc - lag(USGperc), 0)) %>% 
  mutate(dif_TOVperc = ifelse(Player == lag(Player), TOVperc - lag(TOVperc), 0)) %>% mutate(dif_BLKperc = ifelse(Player == lag(Player), BLKperc - lag(BLKperc), 0)) %>% 
  mutate(dif_STLperc = ifelse(Player == lag(Player), STLperc - lag(STLperc), 0)) %>% mutate(dif_BLK = ifelse(Player == lag(Player), BLK - lag(BLK), 0))

#Checking for NA values
which(is.na(MIP), arr.ind=TRUE)
MIP <- MIP %>% mutate_if(is.numeric , replace_na, replace = 0)

#Viewing differences between MIP and non_MIP
ggplot(MIP, aes(x = dif_MP, y = dif_PPG)) + geom_point(aes(color = MIP_Candidate))
ggplot(MIP, aes(x = dif_WS, y = dif_PPG)) + geom_point(aes(color = MIP_Candidate))
ggplot(MIP, aes(x = dif_TSperc, y = dif_PER)) + geom_point(aes(color = MIP_Candidate))
ggplot(MIP, aes(x = dif_OWS, y = dif_DWS)) + geom_point(aes(color = MIP_Candidate))
ggplot(MIP, aes(x = MPG, y = PPG)) + geom_line(aes(color = Pos))


#Checking averages for differences
which(is.na(MIP), arr.ind=TRUE)
MIP <- MIP %>% mutate_if(is.numeric , replace_na, replace = 0)
MIP %>% group_by(MIP_Candidate) %>% summarize(avgdif_WS = mean(dif_WS))
MIP %>% group_by(MIP_Candidate) %>% summarize(avgdif_PER = mean(dif_PER))
MIP %>% group_by(MIP_Candidate) %>% summarize(avgdif_MP = mean(dif_MP))
MIP %>% group_by(MIP_Candidate) %>% summarize(meandif_PPG = mean(dif_PPG))
#Possible outliers
MIP %>% filter(MIP_Candidate == "MIP Candidate" & dif_PPG < 0 & dif_WS < 0)
#Overall outliers
boxplot(MIP$dif_BPM)
MIP %>% group_by(MIP_Candidate) %>% summarize(maxBPM = max(dif_BPM))
MIP %>% group_by(MIP_Candidate) %>% summarize(maxBPM = max(PPG))
MIP %>% group_by(MIP_Candidate) %>% summarize(minMPG = min(MPG))
MIP %>% group_by(MIP_Candidate) %>% summarize(minG = min(G))
#Saving MIP to an rds file
saveRDS(MIP, file = "my_data.rds")
save(MIP, file = "MIP.RData")

#Ensuring two tables match up correctly
MIP %>% filter(Player == "CJ Mcollum")

#Cleaning Current Season Data to input into models
NewData <- read.xlsx("C:/Users/Maxwell/Documents/R/NBA_MIP/2018-19SeasonStats.xlsx")
NewData <- NewData %>% mutate(PPG = PTS/G)
NewData <- NewData %>% mutate(MPG = MP/G)
NewData <- NewData %>% mutate_if(is.numeric , replace_na, replace = 0)
NewData$Year <- 2019

MIPnew <- merge(MIP, NewData, by = )
View(MIPnew)
#Add in Year, add to MIP data frame with a new name. Create new columns. Remove all years but the current! Try it on the model and see if it works
