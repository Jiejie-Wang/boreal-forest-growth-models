#******************************************************************************
# Purpose: R script for fitting non-linear regression functions 
#          for climate growth modifiers (thermal transfer functions, TTFs)  
#
# Author: Anthony Taylor				Last Update: March 17, 2022
#
#******************************************************************************

#-------------------------------------------------------------------------------
# Non-linear Beta Function Information:
#-------------------------------------------------------------------------------
# Y ~ d*(((X-Xb)/(Xo-Xb))*((Xc-X)/(Xc-Xo))^((Xc-Xo)/(Xo-Xb)))^b)
#
# Order of parameters in start list or upper/lower limits call: (b, d, Xb, Xo, Xc)

# b = shape parameter
# d = max expected level of y response
# Xb = minimum threshold of x
# Xo = abscissa of x at max expected level of y (i.e., d)
# Xc = maximum threshold of x
#
#-------------------------------------------------------------------------------
# Non-linear Bragg's Function Information:
#-------------------------------------------------------------------------------
# d = max expected level of y response
# e = abscissa of x at max expected level of y (i.e., d)
# b = shape parameter (relates to the slope at the inflection points)
#-------------------------------------------------------------------------------

# You can install aomisc from GitHub
# install.packages("devtools")
# devtools::install_github("onofriAndreaPG/aomisc")
#-------------------------------------------------------------------------------

library(aomisc) # Package created Andrea Onofri for fitting non-linear models

# Remove all previous R objects
rm(list=ls())

# Set Working Directory
setwd("C:/Users/antho/Documents/Work/2_Research/1_My_Projects/1_ClimateTreeGrowthModifiersProject/DataAnalysis")

# Read-in PICUS species parameters (used in graphs)
picus <- read.csv("picus_spp_para.csv",header=T)

# Read-in all my cleaned species data
rs <- read.csv("./1_RedSpruce/spp97_finaldata.csv", header=T)
ws <- read.csv("./2_WhiteSpruce/spp94_finaldata.csv", header=T)
bs <- read.csv("./3_BlackSpruce/spp95_finaldata.csv", header=T)
bf <- read.csv("./4_BalsamFir/spp12_finaldata.csv", header=T)
eh <- read.csv("./5_EasternHemlock/spp261_finaldata.csv", header=T)
wc <- read.csv("./6_WhiteCedar/spp241_finaldata.csv", header=T)
jp <- read.csv("./7_JackPine/spp105_finaldata.csv", header=T)
rp <- read.csv("./8_RedPine/spp125_finaldata.csv", header=T)
wp <- read.csv("./9_WhitePine/spp129_finaldata.csv", header=T)
la <- read.csv("./10_Larch/spp71_finaldata.csv", header=T)
sm <- read.csv("./11_SugarMaple/spp318_finaldata.csv", header=T)
rm <- read.csv("./12_RedMaple/spp316_finaldata.csv", header=T)
wb <- read.csv("./13_WhiteBirch/spp375_finaldata.csv", header=T)
yb <- read.csv("./14_YellowBirch/spp371_finaldata.csv", header=T)
ab <- read.csv("./15_AmericanBeech/spp531_finaldata.csv", header=T)
ta <- read.csv("./16_TremblingAspen/spp746_finaldata.csv", header=T)
ro <- read.csv("./17_RedOak/spp833_finaldata.csv", header=T)
wa <- read.csv("./18_WhiteAsh/spp541_finaldata.csv", header=T)

# Pull-out the variables I want for each analysis one at a time
###UPDATE REGULARLY###
spp <- "wa"
tmp1 <- get(spp)
y <- tmp1$DBHI
x <- tmp1$DD5bin

# Run regression
# Use drm() or nls() function to get starting values --- from Andrea Onofri andrea.onofri@unipg.it

m1 <- drm(y ~ x, fct = DRC.beta(),
		  lowerl = c(0, 0, 0, 500, 3000),      # Sets lower and upper parameter limits
		  upperl = c(20, 2, 2000, 3000, 6000))

#m1 <- nls(y ~ NLS.beta(x, b, d, Xb, Xo, Xc))


#m1 <- nls(y ~ NLS.bragg.3(x, b, d, e))

summary(m1)

# Calculate R2
R2 <- round(R2nls(m1)$PseudoR2,2)

# Create predicted values for plot
x <- seq(0,8000,1)
newdata <- data.frame(x)
y_e <- predict(m1, newdata, type="response")
y_ci <- predict(m1, newdata, type="confidence")

# Find Min. and Max. GDD using % of max DBHI from fitted model
pd <- data.frame(y_e,x)
i <- pd$x[pd$y_e==max(y_e)]
j1 = max(pd$y_e)*0.01
j5 = max(pd$y_e)*0.05

low_pd <- pd[pd$x < i,]
high_pd <- pd[pd$x > i,]

minGrowth_1 <- low_pd[which.min(abs(low_pd$y_e - j1)),]
maxGrowth_1 <- high_pd[which.min(abs(high_pd$y_e - j1)),] 

minGrowth_5 <- low_pd[which.min(abs(low_pd$y_e - j5)),]
maxGrowth_5 <- high_pd[which.min(abs(high_pd$y_e - j5)),] 

# Plot data and model 
tiff(filename = paste(spp,"_model",".tiff"),
     width = 11, height = 8.5,
     units = "in",
     res=150)

### UPDATE ###
#Create new x variable for the plotting
x1 <- tmp1$DD5bin

plot(x1,y,type="n",xlim=c(0,6000),ylim=c(0,1.2),xlab="Growing Degree Days (Base: 5°C)",ylab="DBH Increment (cm/year)", 
     main=(paste("Species:",spp)),cex.axis = 1.5, cex.lab=1.5, xaxs="i", yaxs="i")

#rect(0, 0, 697, 20, col = "light grey", border = "transparent")
#rect(1797, 0, 2310, 20, col = "light grey", border = "transparent")

points(x1,y,col="blue",pch=16)

lines(x,y_e,lty=1,col="red",lwd="8.0")

# Add r2 to graph
text(500,0.9,paste("R2 = ",R2, sep=""),cex=1.5)

# Add lines showing old DD5 values used in PICUS
segments(x0=picus$min[picus$spp==spp],y0=0,x1=picus$lo[picus$spp==spp],y1=max(y_e),col="grey",lty="solid",lwd="5.0") 
segments(x0=picus$lo[picus$spp==spp],y0=max(y_e),x1=picus$ro[picus$spp==spp],y1=max(y_e),col="grey",lty="solid",lwd="5.0") 
segments(x0=picus$ro[picus$spp==spp],y0=max(y_e),x1=picus$max[picus$spp==spp],y1=0,col="grey",lty="solid",lwd="5.0") 

# Add lines showing min and max based on 1% rule
segments(x0=minGrowth_1$x,y0=0,x1=minGrowth_1$x,y1=minGrowth_1$y_e,col="green",lty="solid",lwd="8.0") 
segments(x0=maxGrowth_1$x,y0=0,x1=maxGrowth_1$x,y1=maxGrowth_1$y_e,col="green",lty="solid",lwd="8.0")

# Add lines showing min and max based on 5% rule
segments(x0=minGrowth_5$x,y0=0,x1=minGrowth_5$x,y1=minGrowth_5$y_e,col="green",lty="solid",lwd="8.0") 
segments(x0=maxGrowth_5$x,y0=0,x1=maxGrowth_5$x,y1=maxGrowth_5$y_e,col="green",lty="solid",lwd="8.0")  

legend("topright", legend=c("Fitted NLR model","5% Min/Max limits","Old PICUS Parameters"), 
       lty = c("solid","solid","solid"), col = c("red","green","grey"), lwd = 8)


dev.off()


# Create a second plot using the percent scale on Y axis 

y_e_2 <- (y_e/max(y_e))*100

tiff(filename = paste(spp,"model_PercentScale",".tiff"),
     width = 11, height = 8.5,
     units = "in",
     res=150)
																		### UPDATE ###
plot(x1,y,type = "n",xlim=c(0,6000),ylim=c(0,102.5),main=(paste("Species:",spp)),
xlab="Growing Degree Days (Base: 5°C)",ylab="Percent Growth",cex.axis = 1.5, cex.lab=1.5, xaxs="i", yaxs="i")

lines(x,y_e_2,col="red",lwd="8.0")

# Add lines showing old DD5 values used in PICUS
segments(x0=picus$min[picus$spp==spp],y0=0,x1=picus$lo[picus$spp==spp],y1=100,col="grey",lty="solid",lwd="5.0") 
segments(x0=picus$lo[picus$spp==spp],y0=100,x1=picus$ro[picus$spp==spp],y1=100,col="grey",lty="solid",lwd="5.0") 
segments(x0=picus$ro[picus$spp==spp],y0=100,x1=picus$max[picus$spp==spp],y1=0,col="grey",lty="solid",lwd="5.0") 

# Add lines showing min and max based on 1% rule
segments(x0=minGrowth_1$x,y0=0,x1=minGrowth_1$x,y1=((minGrowth_1$y_e)/max(y_e))*100,col="green",lty="solid",lwd="8.0") 
segments(x0=maxGrowth_1$x,y0=0,x1=maxGrowth_1$x,y1=((maxGrowth_1$y_e)/max(y_e))*100,col="green",lty="solid",lwd="8.0")

# Add lines showing min and max based on 5% rule
segments(x0=minGrowth_5$x,y0=0,x1=minGrowth_5$x,y1=((minGrowth_5$y_e)/max(y_e))*100,col="green",lty="solid",lwd="8.0") 
segments(x0=maxGrowth_5$x,y0=0,x1=maxGrowth_5$x,y1=((maxGrowth_5$y_e)/max(y_e))*100,col="green",lty="solid",lwd="8.0")  

legend("topright",legend=c("Fitted NLR model","5% Min/Max limits","Old PICUS Parameters"), 
       lty = c("solid","solid","solid"), col = c("red","green","grey"), lwd = 8)

dev.off()


# End of script
#******************************************************************************
