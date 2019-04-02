library(RColorBrewer)

#Viewing differences between MIP and non_MIP
ggplot(MIP, aes(x = dif_MP, y = dif_PPG)) + geom_point(aes(color = MIP_Candidate))
ggplot(MIP, aes(x = dif_WS, y = dif_PPG)) + geom_point(aes(color = MIP_Candidate))
ggplot(MIP, aes(x = dif_TSperc, y = dif_PER)) + geom_point(aes(color = MIP_Candidate))
ggplot(MIP, aes(x = dif_OWS, y = dif_DWS)) + geom_point(aes(color = MIP_Candidate))
ggplot(MIP, aes(x = dif_GS, y = dif_BPM)) + geom_point(aes(color = MIP_Candidate))


#Check mean statistics for MIP and non_MIP candidates
  #Checking averages
WSPPG <- MIP %>% group_by(MIP_Candidate) %>% summarize(WS = mean(WS), PPG = mean(PPG))
GMP <- MIP %>% group_by(MIP_Candidate) %>% summarize(G = mean(WS), MP = mean(MP))

  #Checking averages for differences
difWSPPG <- MIP %>% group_by(MIP_Candidate) %>% summarize(dif_WS = mean(dif_WS), dif_PPG = mean(dif_PPG))
difPERMP <- MIP %>% group_by(MIP_Candidate) %>% summarize(dif_PER = mean(dif_PER), dif_MP = mean(dif_MP))
MIP %>% group_by(MIP_Candidate) %>% summarize(avgdif_PPG = mean(dif_PPG), avgdif_TSperc = mean(dif_TSperc))
MIP %>% group_by(MIP_Candidate) %>% summarize(avgdif_TSperc = mean(dif_TSperc))
MIP %>% group_by(MIP_Candidate) %>% summarize(avgdif_MPG = mean(dif_MPG))
#There certainly appears to be a significant difference between MIP and non_MIP candidates using these metrics

#Graphing means
ggplot(WSPPG, aes(y = WS, x = PPG, color = MIP_Candidate)) + geom_point(size = 10, aes(shape = MIP_Candidate))
ggplot(GMP, aes(y = MP, x = G, color = MIP_Candidate)) + geom_point(size = 10, aes(shape = MIP_Candidate))
#Graphing mean differences
library(Rmisc)
ggplot(MIP, aes(y = dif_WS, x = dif_PPG, color = MIP_Candidate)) + geom_point(alpha = 0.1) + geom_point(data = difWSPPG, size = 10, aes(shape = MIP_Candidate))
ggplot(difWSPPG, aes(y = dif_WS, x = dif_PPG, color = MIP_Candidate)) + geom_point(size = 10, aes(shape = MIP_Candidate))
ggplot(difPERMP, aes(y = dif_PER, x = dif_MP, color = MIP_Candidate)) + geom_point(size = 10, aes(shape = MIP_Candidate)) +xlim(-100, 600) + ylim(-1.0, 4.0)

#Overall outliers
boxplot(MIP$dif_BPM)
ggplot(MIP, aes(x = as.factor(MIP_Candidate), y = PPG)) +geom_boxplot(fill = "slateblue", alpha = 0.2, outlier.color = "red") + xlab("Most Improved Player")
MIP %>% group_by(MIP_Candidate) %>% summarize(maxBPM = max(dif_BPM))
MIP %>% group_by(MIP_Candidate) %>% summarize(maxPPG = max(PPG))
MIP %>% group_by(MIP_Candidate) %>% summarize(minMPG = min(MPG))
MIP %>% group_by(MIP_Candidate) %>% summarize(minG = min(G))
MIP %>% group_by(MIP_Candidate) %>% summarize(meanG = mean(G))
MIP %>% group_by(MIP_Candidate) %>% summarize(STDG = sd(G))
MIP %>% group_by(MIP_Candidate) %>% summarize(minGS = mean(GS))
MIP %>% group_by(MIP_Candidate) %>% summarize(STDGS = sd(GS))
#Removing outliers for comparison
MIPplay <- MIP %>% subset(G > 55)
MIPplay <- MIP %>% subset(MPG > 23)#Removing Players with less than 23 minutes/game
#Boxplots to show means and outliers
ggplot(MIPplay, aes(x = as.factor(MIP_Candidate), y = dif_PPG)) + geom_boxplot(fill = "slateblue", alpha = 0.2, outlier.color = "red") + xlab("Most Improved Player")
ggplot(MIPplay, aes(x = as.factor(MIP_Candidate), y = dif_VORP)) + geom_boxplot(fill = "slateblue", alpha = 0.2, outlier.color = "red") + xlab("Most Improved Player")
ggplot(MIPplay, aes(x = as.factor(MIP_Candidate), y = dif_BPM)) + geom_boxplot(fill = "slateblue", alpha = 0.2, outlier.color = "red") + xlab("Most Improved Player")
ggplot(MIPplay, aes(x = as.factor(MIP_Candidate), y = dif_FGperc)) + geom_boxplot(fill = "slateblue", alpha = 0.2, outlier.color = "red") + xlab("Most Improved Player")


#Code for the report
ggplot(MIP, aes(x = MPG, y = PPG)) + geom_line()
ggplot(MIP, aes(x = MPG, y = PPG)) + geom_point(aes(color = MIP_Candidate))
ggplot(MIP, aes(x = PPG)) + geom_histogram(bins = 50, aes(color = MIP_Candidate))
MIPplay <- MIP %>% subset(G > 55)
MIPplay <- MIP %>% subset(MPG > 23)#Removing Players with less than 23 minutes/game
ggplot(MIP, aes(x = as.factor(MIP_Candidate), y = PPG)) + geom_jitter(aes(color = MPG))
difGraph <- ggplot(MIPplay, aes(x = dif_MPG, y = dif_PPG)) + geom_point(aes(color = MIP_Candidate)) + xlab("Difference in MPG") + ylab("Difference in PPG")

