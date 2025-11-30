# Data loading
data <- read.csv("20240613 Yeast brine AFU vs CFU study v3.csv")
data2 <- read.csv("20231220S_cerevisiae_flow_rate_in_brine_PBS(live).csv")

# Data formatting/pre-processing
# DS1
data <- data[,-c(12:13)] # removing the last two columns because of empty columns in the .csv file (from Excel)
data$Brine.sample <- factor(data$Brine.sample)
data$Grab.sample <- factor(data$Grab.sample)
data$Method <- factor(data$Method)
data$Dup <- factor(data$Dup)
dataPlate <- data[data$Method == "Plate",]
dataFlow <- data[data$Method == "FCM",]
# DS2
yeastlogAFU_scaled <- data2$Live.yeast..LOG.AFU.mL.
meanYeast <- mean(data2$Live.yeast..LOG.AFU.mL.)
sdYeast <- sd(data2$Live.yeast..LOG.AFU.mL.)
dataL <- list(LYeast = yeastlogAFU_scaled, FlowN = as.numeric(as.factor(data2$Flow.rate)), DayN = as.numeric(as.factor(data2$Day)), MediaN = as.numeric(as.factor(data2$Media)))#, Repeat = as.factor(data$Repeat), Rep = as.factor(data$Rep))

  ## DS 1 ##
# Violin plots
library(vioplot)
# for all the log-yeast vs incub.day:
# par(cex.main=1)
# B.Sample A
dataPlateA <- data[(data$Method == "Plate")&(data$Brine.sample == "A"),]
dataFlowA <- data[(data$Method == "FCM")&(data$Brine.sample == "A"),]
dataFlowA <- data.frame(Log.Yeast = rep(dataFlowA$Log.Yeast.CFU.mL.or.Log.AFU.mL, 4), DaysF = c(rep(3, 12), rep(5, 12), rep(7, 12), rep(10, 12)), Exp = rep(dataFlowA$Exp, 4))
#vioplot(Log.Yeast.CFU.mL.or.Log.AFU.mL ~ Plate.incubation.days, data = dataPlateA, col="lightblue", xlab = "Incubation day", ylab = "", plotCentre = "line", side = "left", ylim = c(min(dataFlowA$Log.Yeast, dataPlateA$Log.Yeast.CFU.mL.or.Log.AFU.mL), max(dataFlowA$Log.Yeast, dataPlateA$Log.Yeast.CFU.mL.or.Log.AFU.mL)), main = "Log-Yeast concentration in Yeast in Sample A \n(light blue: plate count in CFU/mL, red: FCM in AFU/mL)")
#vioplot(Log.Yeast ~ DaysF, data = dataFlowA, col = "palevioletred", plotCentre = "line", side = "right", add = T)
#per Exp
min1 <- min(dataPlateA$Log.Yeast.CFU.mL.or.Log.AFU.mL, dataFlowA$Log.Yeast)
max1 <- max(dataPlateA$Log.Yeast.CFU.mL.or.Log.AFU.mL, dataFlowA$Log.Yeast)
#vioplot(Log.Yeast.CFU.mL.or.Log.AFU.mL ~ Plate.incubation.days*Exp, data = dataPlateA, xaxt="n", plotCentre = "line", side = "left", col = "lightblue", ylim = c(min1, max1), xlab = "Plate count incubation day", ylab = "Number of yeast (Log CFU/mL or Log AFU/mL)", main = "Log-Yeast concentration in Yeast in Sample A extracted\n on 14Jun (left), 21Jun (centre) and 5Jul (right)\n(light blue: plate count in CFU/mL, red: FCM in AFU/mL)")
#axis(1, at= 1:12, labels= rep(c("3", "5", "7", "10"), 3))
#vioplot(Log.Yeast ~ DaysF*Exp, data = dataFlowA, plotCentre = "line", side = "right", col = "palevioletred", add = T)
#abline(v = 4.5)
#abline(v = 8.5)

#B.Sample B
dataPlateB <- data[(data$Method == "Plate")&(data$Brine.sample == "B"),]
dataFlowB <- data[(data$Method == "FCM")&(data$Brine.sample == "B"),]
dataFlowB <- data.frame(Log.Yeast = rep(dataFlowB$Log.Yeast.CFU.mL.or.Log.AFU.mL, 4), DaysF = c(rep(3, 12), rep(5, 12), rep(7, 12), rep(10, 12)), Exp = rep(dataFlowB$Exp, 4))
#vioplot(Log.Yeast.CFU.mL.or.Log.AFU.mL ~ Plate.incubation.days, data = dataPlateB, col="lightblue", xlab = "Incubation day", ylab = "", plotCentre = "line", side = "left", ylim = c(min(dataFlowB$Log.Yeast, dataPlateB$Log.Yeast.CFU.mL.or.Log.AFU.mL), max(dataFlowB$Log.Yeast, dataPlateB$Log.Yeast.CFU.mL.or.Log.AFU.mL)), main = "Log-Yeast concentration in Yeast in Sample B \n(light blue: plate count in CFU/mL, red: FCM in AFU/mL)")
#vioplot(Log.Yeast ~ DaysF, data = dataFlowB, col = "palevioletred", plotCentre = "line", side = "right", add = T)
#per Exp
min2 <- min(dataPlateB$Log.Yeast.CFU.mL.or.Log.AFU.mL, dataFlowB$Log.Yeast)
max2 <- max(dataPlateB$Log.Yeast.CFU.mL.or.Log.AFU.mL, dataFlowB$Log.Yeast)
#vioplot(Log.Yeast.CFU.mL.or.Log.AFU.mL ~ Plate.incubation.days*Exp, data = dataPlateB, xaxt="n", plotCentre = "line", side = "left", col = "lightblue", ylim = c(min2, max2),  xlab = "Plate count incubation day", ylab = "Number of yeast (Log CFU/mL or Log AFU/mL)", main = "Log-Yeast concentration in Yeast in Sample B extracted\n on 14Jun (left), 21Jun (centre) and 5Jul (right)\n(light blue: plate count in CFU/mL, red: FCM in AFU/mL)")
#axis(1, at= 1:12, labels= rep(c("3", "5", "7", "10"), 3))
#vioplot(Log.Yeast ~ DaysF*Exp, data = dataFlowB, plotCentre = "line", side = "right", col = "palevioletred", add = T)
#abline(v = 4.5)
#abline(v = 8.5)

#B.Sample C
dataPlateC <- data[(data$Method == "Plate")&(data$Brine.sample == "C"),]
dataFlowC <- data[(data$Method == "FCM")&(data$Brine.sample == "C"),]
dataFlowC <- data.frame(Log.Yeast = rep(dataFlowC$Log.Yeast.CFU.mL.or.Log.AFU.mL, 4), DaysF = c(rep(3, 12), rep(5, 12), rep(7, 12), rep(10, 12)), Exp = rep(dataFlowC$Exp, 4))
#vioplot(Log.Yeast.CFU.mL.or.Log.AFU.mL ~ Plate.incubation.days, data = dataPlateC, col="lightblue", xlab = "Incubation day", ylab = "", plotCentre = "line", side = "left", ylim = c(min(dataFlowC$Log.Yeast, dataPlateC$Log.Yeast.CFU.mL.or.Log.AFU.mL), max(dataFlowC$Log.Yeast, dataPlateC$Log.Yeast.CFU.mL.or.Log.AFU.mL)), main = "Log-Yeast concentration in Yeast in Sample C \n(light blue: plate count in CFU/mL, red: FCM in AFU/mL)")
#vioplot(Log.Yeast ~ DaysF, data = dataFlowC, col = "palevioletred", plotCentre = "line", side = "right", add = T)
#per Exp
min3 <- min(dataPlateC$Log.Yeast.CFU.mL.or.Log.AFU.mL, dataFlowC$Log.Yeast)
max3 <- max(dataPlateC$Log.Yeast.CFU.mL.or.Log.AFU.mL, dataFlowC$Log.Yeast)
#vioplot(Log.Yeast.CFU.mL.or.Log.AFU.mL ~ Plate.incubation.days*Exp, data = dataPlateC, xaxt="n", plotCentre = "line", side = "left", col = "lightblue", ylim = c(min3, max3), xlab = "Plate count incubation day", ylab = "Number of yeast (Log CFU/mL or Log AFU/mL)", main = "Log-Yeast concentration in Yeast in Sample C extracted\n on 14Jun (left), 21Jun (centre) and 5Jul (right)\n(light blue: plate count in CFU/mL, red: FCM in AFU/mL)")
#axis(1, at= 1:12, labels= rep(c("3", "5", "7", "10"), 3))
#vioplot(Log.Yeast ~ DaysF*Exp, data = dataFlowC, plotCentre = "line", side = "right", col = "palevioletred", add = T)
#abline(v = 4.5)
#abline(v = 8.5)

#B.Sample D
dataPlateD <- data[(data$Method == "Plate")&(data$Brine.sample == "D"),]
dataFlowD <- data[(data$Method == "FCM")&(data$Brine.sample == "D"),]
dataFlowD <- data.frame(Log.Yeast = rep(dataFlowD$Log.Yeast.CFU.mL.or.Log.AFU.mL, 4), DaysF = c(rep(3, 12), rep(5, 12), rep(7, 12), rep(10, 12)), Exp = rep(dataFlowD$Exp, 4))
#vioplot(Log.Yeast.CFU.mL.or.Log.AFU.mL ~ Plate.incubation.days, data = dataPlateD, col="lightblue", xlab = "Incubation day", ylab = "", plotCentre = "line", side = "left", ylim = c(min(dataFlowD$Log.Yeast, dataPlateD$Log.Yeast.CFU.mL.or.Log.AFU.mL), max(dataFlowD$Log.Yeast, dataPlateD$Log.Yeast.CFU.mL.or.Log.AFU.mL)), cex.main=2, main = "Log-Yeast concentration in Yeast in Sample D \n(light blue: plate count in CFU/mL, red: FCM in AFU/mL)")
#vioplot(Log.Yeast ~ DaysF, data = dataFlowD, col = "palevioletred", plotCentre = "line", side = "right", add = T)
#per Exp
min4 <- min(dataPlateD$Log.Yeast.CFU.mL.or.Log.AFU.mL, dataFlowD$Log.Yeast)
max4 <- max(dataPlateD$Log.Yeast.CFU.mL.or.Log.AFU.mL, dataFlowD$Log.Yeast)
#vioplot(Log.Yeast.CFU.mL.or.Log.AFU.mL ~ Plate.incubation.days*Exp, data = dataPlateD, xaxt="n", plotCentre = "line", side = "left", col = "lightblue", ylim = c(min4, max4), xlab = "Plate count incubation day", ylab = "Number of yeast (Log CFU/mL or Log AFU/mL)", main = "Log-Yeast concentration in Yeast in Sample D extracted\n on 14Jun (left), 21Jun (centre) and 5Jul (right)\n(light blue: plate count in CFU/mL, red: FCM in AFU/mL)")
#axis(1, at= 1:12, labels= rep(c("3", "5", "7", "10"), 3))
#vioplot(Log.Yeast ~ DaysF*Exp, data = dataFlowD, plotCentre = "line", side = "right", col = "palevioletred", add = T)
#abline(v = 4.5)
#abline(v = 8.5)

#B.Sample E
dataPlateE <- data[(data$Method == "Plate")&(data$Brine.sample == "E"),]
dataFlowE <- data[(data$Method == "FCM")&(data$Brine.sample == "E"),]
dataFlowE <- data.frame(Log.Yeast = rep(dataFlowE$Log.Yeast.CFU.mL.or.Log.AFU.mL, 4), DaysF = c(rep(3, 12), rep(5, 12), rep(7, 12), rep(10, 12)), Exp = rep(dataFlowE$Exp, 4))
#vioplot(Log.Yeast.CFU.mL.or.Log.AFU.mL ~ Plate.incubation.days, data = dataPlateE, col="lightblue", xlab = "Incubation day", ylab = "", plotCentre = "line", side = "left", ylim = c(min(dataFlowE$Log.Yeast, dataPlateE$Log.Yeast.CFU.mL.or.Log.AFU.mL), max(dataFlowE$Log.Yeast, dataPlateE$Log.Yeast.CFU.mL.or.Log.AFU.mL)), cex.main=2, main = "Log-Yeast concentration in Yeast in Sample E \n(light blue: plate count in CFU/mL, red: FCM in AFU/mL)")
#vioplot(Log.Yeast ~ DaysF, data = dataFlowE, col = "palevioletred", plotCentre = "line", side = "right", add = T)
#per Exp
min5 <- min(dataPlateE$Log.Yeast.CFU.mL.or.Log.AFU.mL, dataFlowE$Log.Yeast)
max5 <- max(dataPlateE$Log.Yeast.CFU.mL.or.Log.AFU.mL, dataFlowE$Log.Yeast)
#vioplot(Log.Yeast.CFU.mL.or.Log.AFU.mL ~ Plate.incubation.days*Exp, data = dataPlateE, xaxt="n", plotCentre = "line", side = "left", col = "lightblue", ylim = c(min5, max5), xlab = "Plate count incubation day", ylab = "Number of yeast (Log CFU/mL or Log AFU/mL)", main = "Log-Yeast concentration in Yeast in Sample E extracted\n on 14Jun (left), 21Jun (centre) and 5Jul (right)\n(light blue: plate count in CFU/mL, red: FCM in AFU/mL)")
#axis(1, at= 1:12, labels= rep(c("3", "5", "7", "10"), 3))
#vioplot(Log.Yeast ~ DaysF*Exp, data = dataFlowE, plotCentre = "line", side = "right", col = "palevioletred", add = T)
#abline(v = 4.5)
#abline(v = 8.5)
#par(cex.main=2)

# Poster plot version
#par(mar = c(5.1, 5.1, 4.1, 2.1))
minPos <- min(dataFlow$Log.Yeast.CFU.mL.or.Log.AFU.mL[(dataFlow$Exp == 1)], dataPlate$Log.Yeast.CFU.mL.or.Log.AFU.mL[(dataPlate$Exp == 1)&(dataPlate$Plate.incubation.days == 5)])
maxPos <- max(dataFlow$Log.Yeast.CFU.mL.or.Log.AFU.mL[(dataFlow$Exp == 1)], dataPlate$Log.Yeast.CFU.mL.or.Log.AFU.mL[(dataPlate$Exp == 1)&(dataPlate$Plate.incubation.days == 5)])
#vioplot(dataFlowA$Log.Yeast[dataFlowA$Exp == 1], dataFlowB$Log.Yeast[dataFlowB$Exp == 1], dataFlowC$Log.Yeast[dataFlowC$Exp == 1], dataFlowD$Log.Yeast[dataFlowD$Exp == 1], dataFlowE$Log.Yeast[dataFlowE$Exp == 1], xaxt="n", yaxt="n", plotCentre = "line", side = "right", col = "violetred", xlab = "", main = "", ylim = c(minPos, maxPos), ylab = "")
#axis(1, at = 1:5, labels= c("A", "B", "C", "D", "E"))
#axis(2, at = seq(2, 3.8, by = 0.4), labels = seq(2, 3.8, by = 0.4))
#mtext("Yeast number (Log CFU/mL or Log AFU/mL)", side = 2, cex = 0.9, line = 2.5)
#vioplot(dataPlateA$Log.Yeast[(dataPlateA$Exp == 1)&(dataPlateA$Plate.incubation.days == 5)], dataPlateB$Log.Yeast[(dataPlateB$Exp == 1)&(dataPlateB$Plate.incubation.days == 5)], dataPlateC$Log.Yeast[(dataPlateC$Exp == 1)&(dataPlateC$Plate.incubation.days == 5)], dataPlateD$Log.Yeast[(dataPlateD$Exp == 1)&(dataPlateD$Plate.incubation.days == 5)], dataPlateE$Log.Yeast[(dataPlateE$Exp == 1)&(dataPlateE$Plate.incubation.days == 5)], plotCentre = "line", side = "left", col = "lightblue", add = T)
#abline(v = 1.5, lty = 3)
#abline(v = 2.5, lty = 3)
#abline(v = 3.5, lty = 3)
#abline(v = 4.5, lty = 3)
#par(mar = c(5.1, 4.1, 4.1, 2.1))

# Paper plot version
tiff(filename = "paperDay5Plate_vs_FCM.tiff", width = 960, height = 3200)
par(mfrow = c(5,1), mar = c(1.1, 9.0, 4.1, 2.1))
# brine sample A
vioplot(Log.Yeast ~ Exp, data = dataFlowA, xaxt="n", yaxt="n", plotCentre = "line", side = "right", col = "violetred", xlab = "", main = "", ylab = "", ylim = c(min(dataFlowA$Log.Yeast, dataPlateA$Log.Yeast.CFU.mL.or.Log.AFU.mL), max(dataFlowA$Log.Yeast, dataPlateA$Log.Yeast.CFU.mL.or.Log.AFU.mL)))
axis(2, cex.axis=3.2)
#axis(1, at = 1:3, labels= c("Exp 1", "Exp 2", "Exp 3"))
vioplot(Log.Yeast.CFU.mL.or.Log.AFU.mL ~ Exp, data = subset(dataPlateA, subset = Plate.incubation.days == 5), plotCentre = "line", side = "left", col = "lightblue", add = T)
abline(v = 1.5, lty = 3)
abline(v = 2.5, lty = 3)
# brine sample B
#par(mar = c(1.1, 4.1, 1.1, 2.1))
vioplot(Log.Yeast ~ Exp, data = dataFlowB, xaxt="n", yaxt="n", plotCentre = "line", side = "right", col = "violetred", xlab = "", main = "", ylab = "", ylim = c(min(dataFlowB$Log.Yeast, dataPlateB$Log.Yeast.CFU.mL.or.Log.AFU.mL), max(dataFlowB$Log.Yeast, dataPlateB$Log.Yeast.CFU.mL.or.Log.AFU.mL)))
axis(2, cex.axis=3.2)
#axis(1, at = 1:3, labels= c("Exp 1", "Exp 2", "Exp 3"))
vioplot(Log.Yeast.CFU.mL.or.Log.AFU.mL ~ Exp, data = subset(dataPlateB, subset = Plate.incubation.days == 5), plotCentre = "line", side = "left", col = "lightblue", add = T)
abline(v = 1.5, lty = 3)
abline(v = 2.5, lty = 3)
# brine sample C
vioplot(Log.Yeast ~ Exp, data = dataFlowC, xaxt="n", yaxt="n", plotCentre = "line", side = "right", col = "violetred", xlab = "", main = "", ylab = "", ylim = c(min(dataFlowC$Log.Yeast, dataPlateC$Log.Yeast.CFU.mL.or.Log.AFU.mL), max(dataFlowC$Log.Yeast, dataPlateC$Log.Yeast.CFU.mL.or.Log.AFU.mL)))
axis(2, cex.axis=3.2)
#axis(1, at = 1:3, labels= c("Exp 1", "Exp 2", "Exp 3"))
vioplot(Log.Yeast.CFU.mL.or.Log.AFU.mL ~ Exp, data = subset(dataPlateC, subset = Plate.incubation.days == 5), plotCentre = "line", side = "left", col = "lightblue", add = T)
abline(v = 1.5, lty = 3)
abline(v = 2.5, lty = 3)
mtext(text = "Number of yeasts (Log CFU/mL or Log AFU/mL)", side = 2, line = 5.0, cex = 3.2)
# brine sample D
vioplot(Log.Yeast ~ Exp, data = dataFlowD, xaxt="n", yaxt="n", plotCentre = "line", side = "right", col = "violetred", xlab = "", main = "", ylab = "", ylim = c(min(dataFlowD$Log.Yeast, dataPlateD$Log.Yeast.CFU.mL.or.Log.AFU.mL), max(dataFlowD$Log.Yeast, dataPlateD$Log.Yeast.CFU.mL.or.Log.AFU.mL)))
axis(2, cex.axis=3.2)
#axis(1, at = 1:3, labels= c("Exp 1", "Exp 2", "Exp 3"))
vioplot(Log.Yeast.CFU.mL.or.Log.AFU.mL ~ Exp, data = subset(dataPlateD, subset = Plate.incubation.days == 5), plotCentre = "line", side = "left", col = "lightblue", add = T)
abline(v = 1.5, lty = 3)
abline(v = 2.5, lty = 3)
# brine sample E
par(mar = c(5.1, 9.0, 4.1, 2.1))
vioplot(Log.Yeast ~ Exp, data = dataFlowE, xaxt="n", yaxt="n", plotCentre = "line", side = "right", col = "violetred", xlab = "", main = "", ylab = "", ylim = c(min(dataFlowE$Log.Yeast, dataPlateE$Log.Yeast.CFU.mL.or.Log.AFU.mL), max(dataFlowE$Log.Yeast, dataPlateE$Log.Yeast.CFU.mL.or.Log.AFU.mL)))
axis(2, cex.axis=3.2)
axis(1, cex.axis=3.2, line = 2, lty = 0, at = 1:3, labels= c("Exp 1", "Exp 2", "Exp 3"))
vioplot(Log.Yeast.CFU.mL.or.Log.AFU.mL ~ Exp, data = subset(dataPlateE, subset = Plate.incubation.days == 5), plotCentre = "line", side = "left", col = "lightblue", add = T)
abline(v = 1.5, lty = 3)
abline(v = 2.5, lty = 3)
par(mar = c(5.1, 4.1, 4.1, 2.1), mfrow = c(1,1))
dev.off()

# Computing mean and SD within (between could be computed from the SDs between the means...)
MEAN.P <- array(NA, dim = c(3, 5, 4))
MEAN.F <- matrix(NA, ncol = 5, nrow = 3)
pval.FmP <- matrix(NA, ncol = 5, nrow = 3)
SDwithinPlate <- array(NA, dim = c(3, 5, 4)) # 4(inoc.days)*3(Exp)*5(Brine.sample)
SDwithinFCM <- matrix(NA, ncol = 5, nrow = 3) # 3(Exp)*5(Brine.sample)
for(i in 1:3){
  for(j in 1:5){
    MEAN.F[i,j] <- mean(dataFlow$Log.Yeast.CFU.mL.or.Log.AFU.mL[(dataFlow$Exp == i)&(dataFlow$Brine.sample == levels(dataFlow$Brine.sample)[j])])
    SDwithinFCM[i,j] <- sd(dataFlow$Log.Yeast.CFU.mL.or.Log.AFU.mL[(dataFlow$Exp == i)&(dataFlow$Brine.sample == levels(dataFlow$Brine.sample)[j])])
    for(k in 1:4){
      MEAN.P[i,j,k] <- mean(dataPlate$Log.Yeast.CFU.mL.or.Log.AFU.mL[(dataPlate$Exp == i)&(dataPlate$Plate.incubation.days == unique(dataPlate$Plate.incubation.days)[k])&(dataPlate$Brine.sample == levels(dataPlate$Brine.sample)[j])])
      SDwithinPlate[i,j,k] <- sd(dataPlate$Log.Yeast.CFU.mL.or.Log.AFU.mL[(dataPlate$Exp == i)&(dataPlate$Plate.incubation.days == unique(dataPlate$Plate.incubation.days)[k])&(dataPlate$Brine.sample == levels(dataPlate$Brine.sample)[j])])
      if(k == 2){
        xx <- dataFlow$Log.Yeast.CFU.mL.or.Log.AFU.mL[(dataFlow$Exp == i)&(dataFlow$Brine.sample == levels(dataFlow$Brine.sample)[j])]
        yy <- dataPlate$Log.Yeast.CFU.mL.or.Log.AFU.mL[(dataPlate$Exp == i)&(dataPlate$Plate.incubation.days == unique(dataPlate$Plate.incubation.days)[2])&(dataPlate$Brine.sample == levels(dataPlate$Brine.sample)[j])]
        pval.FmP [i,j] <- t.test(x = xx, y = yy)$p.value
        rm(xx, yy)
      }
    }
  }
}

# differences in the 15 cases (Exp and brine sample) between plate counts and fcm measurements
t(MEAN.F-MEAN.P[,,2])
mean(MEAN.F-MEAN.P[,,2])

# p-values for t-test of the differences (same as above) between the 4 values (Dup x2 x Grab.sample x2) in each of the 15 situations
t(pval.FmP)
temp1 <- t(pval.FmP)*15 # Bonferroni corrected
temp1[temp1>1] <- 1
temp1

#sd's of values for the 15 cases
t(SDwithinFCM)
t(SDwithinPlate[,,2]) # at incub.day = 5 for Plate counts

  ## DS 2 ##
boxplot(data2$Live.yeast..LOG.AFU.mL.~ interaction(data2$Day, data2$Media), col = c(1:3), xlab = "Day and Media", ylab = "log(AFU/mL)")
abline(v = 3.5, lty = 3)
text(x = 2, y = 4, "Brine")
text(x = 5, y = 2.5, "PBS")
# Day per flow rate, ignoring the Medium
boxplot(data2$Live.yeast..LOG.AFU.mL.~ interaction(data2$Day, data2$Flow.rate), col = rep(1:6, each = 3), xlab = "Day and Flow rate", ylab = "log(AFU/mL)")
# Plot all factors together
boxplot(Live.yeast..LOG.AFU.mL.~ Day*Flow.rate*Media, data = data2, notch = F, col = rep(c(1:6), each = 3), xlab = "Day, Flow rate and Media", ylab = "log(AFU/mL)")
abline(v = 18.5, lty = 3)
text(x = 8, y = 4, "Brine")
text(x = 27, y = 2.5, "PBS")

# Modelling
library(rethinking) # installation see here https://github.com/rmcelreath/rethinking
# Modelling AFU as an interaction of 2 fixed effects (Flow.rate and Media)
model1 <- alist(
  # data modelling
  LYeast ~  dnorm(mu, sigma),
  mu <- alpha[FlowN, MediaN],
  sigma ~  dexp(1),
  # (fixed) Flow and Media effects in one matrix (interaction)
  matrix[6,2]:alpha ~ normal(0, 1)
)
fit.mo1 <- ulam(model1, data = dataL, chains = 4, cores = 4)
precis(fit.mo1, depth = 3)
#Good diagnostics and mimics what was seen on Ben's histogram (an issue with it, Ben, the bars start at 0!!)
plot(fit.mo1, depth = 3)

# Modelling AFU as an interaction of 2 fixed effects (Flow.rate and Media) and an additive random Day effect:
model2 <- alist(
  # data modelling
  LYeast ~  dnorm(mu, sigma),
  mu <- alpha[FlowN, MediaN]+beta[DayN],
  sigma ~  dexp(1),
  # (fixed) Flow and Media effects in one matrix (interaction)
  matrix[6,2]:alpha ~ normal(0, 1),
  # (adaptive and additive) Day effect
  beta[DayN] ~ dnorm(mu_d, sigma_d),
  #beta[DayN] ~ dnorm(0, 1)
  mu_d ~ dnorm(0,1),
  sigma_d ~ dexp(1)
)
fit.mo2 <- ulam(model2, data = dataL, chains = 4, cores = 4, iter = 5000)
#, start = list(mu = 0, sigma = 1, alpha = matrix(0, nrow = 6, ncol = 2), beta = c(0,0), mu_d = 0, sigma_d = 1)

# This is this model
# (see more options here https://cran.r-project.org/web/packages/igraph/vignettes/igraph.html)
library(igraph)
g2 <- graph_from_literal(
  Flow -+ X,
  Medium -+ X,
  X -+ Yeast,
  Day -+ Yeast
)
plot(g2, layout = layout_nicely(g2), vertex.label.dist = c(3,0,3,3,3), vertex.color = c(1,2,1,1,1))
# Model coefficients
precis(fit.mo2, depth = 3)
# Diagnostic R.hat better with 5,000 iterations, but some low n_eff...
plot(fit.mo2, depth = 3)

## Answer to research questions via simulations:

# Q1: any difference between the flow rates in PBS (MediaN =2) ? Clearly no!
# Day 1 (DayN = 1)
sim1 <- sim(fit.mo2, data = list(MediaN = rep(2, 6), FlowN = c(1:6), DayN = rep(1, 6)))
dens(sim1[,1]-sim1[,2])
dens(sim1[,1]-sim1[,3])
dens(sim1[,1]-sim1[,4])
# Day 2 (DayN = 2)
sim2 <- sim(fit.mo2, data = list(MediaN = rep(2, 6), FlowN = c(1:6), DayN = rep(2, 6)))
dens(sim2[,1]-sim2[,2])
dens(sim2[,1]-sim2[,3])
dens(sim2[,1]-sim2[,4])
# Day 3 (DayN = 3)
sim3 <- sim(fit.mo2, data = list(MediaN = rep(2, 6), FlowN = c(1:6), DayN = rep(3, 6)))
dens(sim3[,1]-sim3[,2])
dens(sim3[,1]-sim3[,3])
dens(sim3[,1]-sim3[,4])
# All days together (first 6 are day 1, etc.)
simAlldays <- sim(fit.mo2, data = list(MediaN = rep(2, 18), FlowN = rep(1:6, 3), DayN = rep(1:3, each = 6)))
dens(simAlldays[,c(1,7,13)]-simAlldays[,c(2,8,14)])
dens(simAlldays[,c(1,7,13)]-simAlldays[,c(3,9,15)])
dens(simAlldays[,c(1,7,13)]-simAlldays[,c(4,10,16)])
dens(simAlldays[,c(1,7,13)]-simAlldays[,c(5,11,17)])
dens(simAlldays[,c(1,7,13)]-simAlldays[,c(6,12,18)])
# Model without day effect -> see effect of 12.5 very slightly lower
sim.noDay <- sim(fit.mo1, data = list(MediaN = rep(2, 6), FlowN = 1:6))
dens(sim.noDay[,1]-sim.noDay[,2]) #"12.5"-"25" in PBS
dens(sim.noDay[,1]-sim.noDay[,3]) #"12.5"-"100" in PBS
dens(sim.noDay[,1]-sim.noDay[,4]) #"12.5"-"200" in PBS
dens(sim.noDay[,1]-sim.noDay[,5]) #"12.5"-"500" in PBS
dens(sim.noDay[,1]-sim.noDay[,6]) #"12.5"-"1000" in PBS
dens(sim.noDay[,3]-sim.noDay[,4]) #"100"-"200" in PBS
dens(sim.noDay[,4]-sim.noDay[,6]) #"200"-"1000" in PBS

# Q2: In brine (MediaN = 1), 1000 (Flown = 6) higher than other flows? -> Yes, see all the comparisons below
simAlldaysBrine <- sim(fit.mo2, data = list(MediaN = rep(1, 18), FlowN = rep(1:6, 3), DayN = rep(1:3, each = 6)))
dens(simAlldaysBrine[, c(6,12,18)]-simAlldaysBrine[, c(5,11,17)]) # "1000"-"500" This is on average for all 3 days
# And same effect for the 3 days:
dens(simAlldaysBrine[, 6]-simAlldaysBrine[, 5]) # "1000"-"500" on Day 1
dens(simAlldaysBrine[, 12]-simAlldaysBrine[, 11]) # "1000"-"500" on Day 2
dens(simAlldaysBrine[, 18]-simAlldaysBrine[, 17]) # "1000"-"500" on Day 3
# Other differences:
dens(simAlldaysBrine[, c(6,12,18)]-simAlldaysBrine[, c(4,10,16)]) # "1000"-"200" averaged on all 3 days
dens(simAlldaysBrine[, c(6,12,18)]-simAlldaysBrine[, c(3,9,15)]) # "1000"-"100" averaged on all 3 days
dens(simAlldaysBrine[, c(6,12,18)]-simAlldaysBrine[, c(2,8,14)]) # "1000"-"25" averaged on all 3 days
dens(simAlldaysBrine[, c(6,12,18)]-simAlldaysBrine[, c(1,7,13)]) # "1000"-"12.5" averaged on all 3 days
# In the model without day effect:
sim.noDayBrine <- sim(fit.mo1, data = list(MediaN = rep(1, 6), FlowN = 1:6))
dens(sim.noDayBrine[,6]-sim.noDayBrine[,1]) #"1000"-"12.5" in Brine
dens(sim.noDayBrine[,6]-sim.noDayBrine[,2]) #"1000"-"25" in Brine
dens(sim.noDayBrine[,6]-sim.noDayBrine[,3]) #"1000"-"100" in Brine
dens(sim.noDayBrine[,6]-sim.noDayBrine[,4]) #"1000"-"200" in Brine
dens(sim.noDayBrine[,6]-sim.noDayBrine[,5]) #"1000"-"500" in Brine

# Q3: is there less variances/more precision in 500muL/min (FlowN = 5) and 1000muL/min (FlowN = 6) in brine (MediaN = 1)? Not really. This could appear surprising!
plot(fit.mo1, depth = 3) #model without Day effect
plot(fit.mo2, depth = 3) #model with Day effect
# In both cases, have a look at alpha[5,1] (500 flow, brine medium) and alpha[6,1] (1000 flow, brine medium) compared to others: same spread in mean, we could use simulations for those values:
apply(sim.noDay[,1:4], 2, sd)
apply(sim.noDay[,5:6], 2, sd)

# Q4 at 1000muL/min (FlowN = 6), Yeast count in PBS (MediaN = 2) is more than in Brine (MediaN = 1)? -> Yes, clearly!
simu.1000.noDay <-  sim(fit.mo1, data = list(MediaN = c(1,2), FlowN = rep(6, 2)))
dens(simu.1000.noDay[,1]-simu.1000.noDay[,2]) # "Brine"-"PBS"
sum(simu.1000.noDay[,1]-simu.1000.noDay[,2]>0)
