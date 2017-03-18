## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(
	message = FALSE,
	warning = FALSE,
	include = FALSE
)
# Checking  for required packages and installing if not present

 if (!"tidyverse" %in% installed.packages()) {cat("Package 'tidyverse' is required but not available. Installing Tidyverse now.")
    install.packages("tidyverse", dep = TRUE)
    library("tidyverse")} else {require("tidyverse")}

if (!"reshape2" %in% installed.packages()) {cat("Package 'reshape2' is required but not available. Installing reshape2 now.")
    install.packages("reshape2", dep = TRUE)
    library("reshape2")} else {require("reshape2")}

if (!"naturalsort" %in% installed.packages()) {cat("Package 'naturalsort' is required but not available. Installing naturalsort now.")
    install.packages("naturalsort", dep = TRUE)
    library("naturalsort")} else {require("naturalsort")}


#library(tidyverse)
#library (reshape2)


## ---- include=T, eval=F--------------------------------------------------
## install.packages ("gplots")

## ---- include=T, eval=F--------------------------------------------------
## library ("gplots")

## ----include=T-----------------------------------------------------------
sessionInfo()


## ---- include=T----------------------------------------------------------
# creating a dataframe from scratch
(df <- data.frame(first = c("a","b","c"), second =  c(1,2,3), third =   rnorm(3)))


## ---- include=T----------------------------------------------------------

str(df)


## ---- include=T----------------------------------------------------------
sd <- read.csv("sample_data.csv")


## ---- include=T----------------------------------------------------------
# dimensions of sd data frame
dim(sd)

## ---- include=T----------------------------------------------------------
ncol(sd)# columns

nrow(sd)#rows

## ---- include=T----------------------------------------------------------
head(sd,3)


## ---- include=T----------------------------------------------------------

names(sd)


colnames(sd)


## ---- include=T, eval=F--------------------------------------------------
## 
## View(sd)
## 

## ---- include=T----------------------------------------------------------

sd1 <- read_csv("sample_data.csv")


## ---- include=T----------------------------------------------------------
#this is classic dataframe view. Please, always use head() - or you will flood your console with output
head(sd,3)

#this is a Tidyverse output

sd1 


## ---- include=T----------------------------------------------------------
# a a vector of characters

a <- c("a","b","c","d","e")

a

a[3]

a[1:3]

a[c(1,4,5)]

a[c(5,4,1)]


## ---- include=T----------------------------------------------------------

# Accsessing a column by name:  Output - a vector (of factors)
head(sd$genes,3)

#Accessing a column by index with single brackets: Output - a single-column dataframe
head(sd[1],3)

#accessing a column by index with double brackets: Output - a vector (of factors)
head(sd[[1]],3)

# same for a tibble
# Accsessing a column by name:  Output - a vector (of characters)
head(sd1$genes,3)

#Accessing a column by index with single brackets: Output - a tibble - a single-column dataframe
sd1[1]

#accessing a column by index with double brackets: Output - a vector (of characters)
head(sd1[[1]],3)


## ---- include=T----------------------------------------------------------
sd1$chr <- as.factor(sd1$chr)

#now lets look at sd1 again
head(sd1,3)

## ---- include=T----------------------------------------------------------
nlevels(sd1$chr)

## ---- include=T----------------------------------------------------------
levels(sd1$chr)

## ---- include=T----------------------------------------------------------
# Can we select rows where chr IS "MT"?
sd1[sd1$chr == "MT",]

## ---- include=T----------------------------------------------------------
sd1 <- sd1[!(sd1$chr == "MT"),]

## ---- include=T----------------------------------------------------------
# look at levels. "MT" still persists, although it has no values associated with it
levels(sd1$chr)

#droplevel() removes all unused factors
sd1$chr <- droplevels(sd1$chr)

levels(sd1$chr) # and now "MT" is gone

## ---- include=T----------------------------------------------------------
#subsetting based on two conditions
(long.genes <- sd1[(sd1$chr=="1" & sd1$ExonLength>10000),])

## ---- include=T----------------------------------------------------------
# using dim()
dim(long.genes)[1]

#using length()
length(long.genes[[1]])

## ---- include=T----------------------------------------------------------
an.table <- sd1[,(1:5)]

#or

an.table <-  sd1[,c(1:5)]


## ---- include=T----------------------------------------------------------
# by name
an.table$ExonLength <- NULL

#by index
an.table[2] <- NULL

#several columns at the same time
an.table[2:3] <- NULL

#let's restore an.table to its former glory
an.table <- an.table <- sd1[,(1:5)]

## ---- include=T----------------------------------------------------------
# by index
an.table <- an.table[-(1:1000),]

# by a pattern. I would like to delete all genes that start with "A"
an.table <- an.table[-(grep("^A",an.table$genes)),]


#lets restore an.table to its original state
an.table <- an.table <- sd1[,(1:5)]

## ---- include=T----------------------------------------------------------
#selecting rows based on a pattern
(ccl <- sd1[(grep("^Ccl",sd1$genes)),])

## ---- include=T----------------------------------------------------------
colnames(sd1) # returns a vector with all column names

dko <-  sd1[, c("genes", grep("^DKO", colnames(sd1), value = T))]

## ---- include=T----------------------------------------------------------
grep("^DKO",colnames(sd1),value=T)

## ---- include=T----------------------------------------------------------

# adding empty column 
an.table[,"new_column"] <- NA

#or

an.table$new_column1 <- NA

# adding new column by modifying an existing column
an.table$new_column2 <-  log2(an.table$ExonLength)

## ---- include=T----------------------------------------------------------
# adding multiple colums at once use cbind()
sd1 <- cbind(sd1, log2(sd1[,(6:16)]+1)) 

## ---- include=T----------------------------------------------------------
# changing names on new columns by adding "log_" 

colnames(sd1)[17:27] <- paste0("log_",colnames(sd1)[17:27])

## ---- include=T----------------------------------------------------------
sd2 <- bind_cols(sd1, log2(sd1[,(6:16)]+1))

colnames(sd2)[17:27] <- paste0("log_",colnames(sd2)[17:27])

## ---- include=T----------------------------------------------------------
#Rbase
sd1 <- sd1[order(sd1$chr),] 

#tidyverse
sd1 <- arrange(sd1, chr)

## ---- include=T----------------------------------------------------------
#Rbase + naturalorder package
sd1 <- sd1[naturalorder(sd1$chr),] 


## ---- include=T----------------------------------------------------------
#lets set levels of factor sd1$chr in a correct order
sd1$chr <- factor(sd1$chr, levels=c((1:19),"X","Y"))

#lets look at the value of levels here. what does this construct create?
#finally you can see that the levels in a correct order
levels(sd1$chr)


## ---- include=T----------------------------------------------------------
summary(sd1)

## ---- include=T----------------------------------------------------------
# or you can get a summary for a single column
summary(sd1$chr)

## ---- include=T----------------------------------------------------------
#mean
mean(sd1$ExonLength)

#standard deviation
sd(sd1$ExonLength)

#median
median(sd1$ExonLength)

#median absolute deviation
mad(sd1$ExonLength)

#quantiles
quantile(sd1$ExonLength, probs = seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 8)


## ---- include=T----------------------------------------------------------
#calculating the meanvalue of a group of columns
sd1$wt_mean <- apply(sd1[,17:20],1,mean)
#st.dev for the same
sd1$wt_stdev <- apply(sd1[,17:20],1,sd)

#same as above but without refering to specific column locations
sd1$ko_mean <- apply(sd1[,grep("^log_KO",colnames(sd1))],1,mean)

#same as above for standard deviation
sd1$ko_sd <- apply(sd1[,grep("^log_KO",colnames(sd1))],1,sd)


## ---- include=T----------------------------------------------------------
# Histogram plot with the default parameters
hist(sd1$ExonLength)

# add some bins
hist (sd1$ExonLength,
      breaks = 100)

# set a limit to X axis (xlim()) and add main title (main=)
hist (sd1$ExonLength,
      breaks = 100,
      xlim=c(0,40000),
      main= "Gene lenght distribution in expressed mouse genes")
#change add red fill
hist (sd1$ExonLength,
      breaks = 100,
      xlim=c(0,40000),
      main= "Gene lenght distribution in expressed mouse genes",
      col= "red")
# change the legend of the X-axis
hist (sd1$ExonLength,
      breaks = 100,
      xlim=c(0,40000),
      main= "Exon lenght distribution in expressed mouse genes",
      col= "red",
      xlab=c("Gene Length(bp)"),
      las=1)
# and turn Y axis labels horisontally (las), increasing axis numbers by 20% relative to the default (cex.axis), and increase axis legendsby 20% (cex.lab)
hist (sd1$ExonLength,
      breaks = 100,
      xlim=c(0,40000),
      main= "Gene lenght distribution in expressed mouse genes",
      col= "red",
      xlab=c("Exon Length(bp)"),
      las=1,
      cex.axis=1.2,
      cex.lab = 1.2
      )

#and finally, change the size of plot margins with par
par(mar=c(5,6,4,2)+0.1) #see help(par) for many more posibilities

hist (sd1$ExonLength,
      breaks = 100,
      xlim=c(0,40000),
      main= "Gene lenght distribution in expressed mouse genes",
      col= "red",
      xlab=c("Gene Length(bp)"),
      ylab="", #removes Y-label
      las=1, #turns Y axis numbers by 90 degrees
      cex.axis=1.2, #font size of the axes number is increased by 20% relative to default
      cex.lab = 1.2) #font size of axex labels is increased by 20% relative to default.
      

mtext("Frequency",side=2,line=4,cex=1.2) #adds new y label a bit further from the axis

#and finally add legend at position 20000,2800, that shows a square (pch=15) of red color 
legend(17000, 2800, legend=c("Gene frequencies"),
       col=c("red"), pch=15, cex=1.2)

## ---- include=T----------------------------------------------------------
plot(density(sd1$ExonLength), xlim=c(0,20000), col="blue", lwd=2)

## ---- include=T----------------------------------------------------------
# Ploting one log-transformed column versus the other

plot(sd1$log_WT_rep_1, sd1$log_WT_rep_3,
    pch=19,
    col="steelblue")

abline(lm(sd1$log_WT_rep_1~sd1$log_WT_rep_3), col="red",lwd=2)

## ---- include=T----------------------------------------------------------

# Corelation plot for all WT replicas that shows pairwise correlations in the upper half
# panel.cor() function is stolen from manual page for pairs()

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- abs(cor(x, y))
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste0(prefix, txt)
    if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
    text(0.5, 0.5, txt, cex = cex.cor * r)
}

pairs(~ sd1$log_WT_rep_1 + sd1$log_WT_rep_2 + sd1$log_WT_rep_3 + sd1$log_WT_rep_4, 
      data=sd1,
      col="steelblue",
      upper.panel = panel.cor,
      lower.panel= panel.smooth) #for symmetric matrixes ommits the upper half


## ---- include=T----------------------------------------------------------
#simple box plot of a single column
boxplot(sd1$log_WT_rep_1,
        col="pink")
#now we can actually add datapoints to this using stripchart()

stripchart(sample(sd1$log_WT_rep_1,1000), #uses the same column as boxplot, but takes random 1000 points - to avoid showing 15000 datapoints
          vertical=T,
          method = "jitter", #spread around overlapping values
          add = TRUE, #adds to the previous plot
          pch = 19, #large circle 
          col = 'steelblue')


#This is how you can add text to a plot
text(1.2,9.5,"Median",cex=1)
text(1.2,6.6,"First Quartile",cex=1)
text(1.2,11.2, "Third Quartile", cex=1)
text(1.2,17.8,"+1.5*(Interquartile range)",cex=1)
text(1.2,0.3,"-1.5*(Interquartile range)",cex=1)
text(1.2,19, " Likely outliers", cex=1)

# a boxplot of a continous variable based on a categoriacal variable - plot gene lenght distribution per chromosome
boxplot(sd1$ExonLength ~ sd1$chr)

# we can log transform gene length
boxplot(log2(sd1$ExonLength) ~ sd1$chr)


#a boxplot of multiple columns
par(mar=c(5,8,4,2)+0.1) # margins on the  left side (8) were increased to accomodate long labels 
boxplot(sd1[,(17:27)],horizontal = T,#boxplot was turned 90 degrees (horizontal=T)
        las=1, #labeles were rotated 90 degrees
        col=c(rep("pink",4),rep("steelblue",4),rep("darkgreen",3)) #and colored by experimental groups
        ) 

## ----include=T-----------------------------------------------------------
summary(sd1$chr)

# you can either asign the out put of summary() above to a variable or you can simply use it as a argument of barplot()
barplot (summary(sd1$chr))


## ----include=T-----------------------------------------------------------
pie(summary(sd1$chr),
    col=rainbow(nlevels(sd1$chr)),
    main="rainbow()")


par(mfrow=c(3,2))
par(mar=c(0.5,0.5,0.5,0.5)+0.1)

pie(summary(sd1$chr),
    col=rainbow(nlevels(sd1$chr)),
    main="rainbow()")
pie(summary(sd1$chr),
    col=heat.colors(nlevels(sd1$chr)),
    main="heat.colors()")
pie(summary(sd1$chr),
    col=terrain.colors(nlevels(sd1$chr)),
    main="terrain.colors()")
pie(summary(sd1$chr),
    col=topo.colors(nlevels(sd1$chr)),
    main="topo.colors()")
pie(summary(sd1$chr),
    col=cm.colors(nlevels(sd1$chr)),
    main="cm.colors()")


