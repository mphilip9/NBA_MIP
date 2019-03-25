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
#Checking Average minutes and STD for MIP/Not_MIP
ggplot(MIP, aes(x = MPG, y = PPG)) + geom_point(aes(color = MIP_Candidate))
MIP %>% group_by(MIP_Candidate) %>% summarize(avgmpg = mean(MPG))
MIP %>% group_by(MIP_Candidate) %>% summarize(std = sd(MPG))
#Removing Players with less than 23 minutes/game
MIP <- MIP %>% subset(MPG > 23)

#Displaying difference from the previous year
MIP <- MIP %>% separate(YearPlayer, into = c("Year", "Player"), sep = "~")
MIP <- MIP %>% arrange(Player, Year) 
MIP <- MIP %>% arrange(Player, Year) 
MIP <- MIP %>% arrange(Player, Year) 
MIP <- MIP %>% arrange(Player, Year) 
MIP <- MIP %>% arrange(Player, Year) 
MIP <- MIP %>% arrange(Player, Year) %>% mutate(dif_PPG = PPG - lag(PPG)) %>% 
  mutate(dif_VORP = VORP - lag(VORP))%>% mutate(dif_G = G -lag(G)) %>% mutate(dif_MP = MP - lag(MP)) %>% 
  mutate(dif_PER = PER - lag(PER)) %>% mutate(dif_WS = WS - lag(WS)) %>% mutate(dif_FTr = FTr - lag(FTr))
#Viewing differences between MIP and non_MIP
ggplot(MIP, aes(x = dif_MP, y = dif_PPG)) + geom_point(aes(color = MIP_Candidate))
ggplot(MIP, aes(x = dif_WS, y = dif_PPG)) + geom_point(aes(color = MIP_Candidate))

#Checking for NA values
which(is.na(MIP), arr.ind=TRUE)
MIP <- MIP %>% mutate_if(is.numeric , replace_na, replace = 0)

#Checking averages for differences
MIP %>% group_by(MIP_Candidate) %>% summarize(avgmpg = mean(dif_WS))
which(is.na(MIP), arr.ind=TRUE)
MIP <- MIP %>% mutate_if(is.numeric , replace_na, replace = 0)
  
