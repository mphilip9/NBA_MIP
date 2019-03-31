#Viewing differences between MIP and non_MIP
ggplot(MIP, aes(x = dif_MP, y = dif_PPG)) + geom_point(aes(color = MIP_Candidate))
ggplot(MIP, aes(x = dif_WS, y = dif_PPG)) + geom_point(aes(color = MIP_Candidate))
ggplot(MIP, aes(x = dif_TSperc, y = dif_PER)) + geom_point(aes(color = MIP_Candidate))
ggplot(MIP, aes(x = dif_OWS, y = dif_DWS)) + geom_point(aes(color = MIP_Candidate))
ggplot(MIP, aes(x = GS, y = PPG)) + geom_point(aes(color = MIP_Candidate))


#Check mean statistics for MIP and non_MIP candidates
#Checking averages for differences

WSdifs <- MIP %>% group_by(MIP_Candidate) %>% summarize(dif_WS = mean(dif_WS))
MIP %>% group_by(MIP_Candidate) %>% summarize(avgdif_PER = mean(dif_PER))
MIP %>% group_by(MIP_Candidate) %>% summarize(avgdif_MP = mean(dif_MP))
MIP %>% group_by(MIP_Candidate) %>% summarize(avgdif_PPG = mean(dif_PPG))
MIP %>% group_by(MIP_Candidate) %>% summarize(avgdif_TSperc = mean(dif_TSperc))
MIP %>% group_by(MIP_Candidate) %>% summarize(avgdif_MPG = mean(dif_MPG))
#There certainly appears to be a significant difference between MIP and non_MIP candidates using these metrics

#Graphing differences
library(Rmisc)
ggplot(WSdifs, aes(y = dif_WS, x = MIP_Candidate)) + geom_point()


MIP$Candidate <- as.factor(MIP$MIP_Candidate)