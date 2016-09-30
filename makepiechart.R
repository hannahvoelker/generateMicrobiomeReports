
#!/usr/bin/env Rscript
#
# makepiechart.r
# Author: Hannah Voelker
#Last modified: 5/5/16
#Adapted from printPieChart.r
# Given a set of the 10 most prevalent bacteria in a sample, creates a corresponding pie chart 
#

#import
require(plotrix)
library(extrafont)

# get the csv file
genuscounts <<- read.csv(file.choose(), header = TRUE, sep = ",")

pdf(file = "genus_Piecharts.pdf", width = 8.5, height = 11, onefile = TRUE, family = "Arial Unicode MS")

#loop through the data frame and find the top ten for each 
par(mfrow=c(2,1))
for(i in 2:ncol(genuscounts)) {
    thisColumn <- genuscounts[,c(1,i)]
    samplename <- names(genuscounts[i])
    topten <- head(thisColumn[order(thisColumn[,2], decreasing = TRUE), ], 10)
    #We have to create an "other" factor using the remaining data
    # I did this by making a new data frame
    other <- sum(genuscounts[,i])-sum(topten[,2])
    otherdata <- data.frame("Genus" = character(1), samplename = numeric(1), stringsAsFactors = FALSE)
    names(otherdata)[2] = samplename
    otherdata[1,] <- c("Other", other)
    #now that the names align, we can attach the two frames together
    topten <- rbind(topten, otherdata)
    
    #now to make the proportions
    totalcount <- sum(genuscounts[,i])
    slices = as.integer(topten[,2])/totalcount
    
  # add labels
  slices = round(slices*100,2)
  lbls <- topten[,1]
  lbls = paste(lbls,as.character(slices))
  lbls = paste(lbls,"%",sep="")


  colors = rainbow(length(slices), s = 0.5)
  pie(slices, '', radius = 0.55, col=colors)
  # can use font.main = '' to adjust the font of main title
  textX = 0
  textY = 0.88
  text(textX,textY, samplename, cex=3.5, font= 6, family = "Arial Rounded MT Bold")
  legend("bottomleft", lbls, cex=0.75, fill=colors)
  textX = 0
  textY = 0.65
  text(textX,textY,'This sample\' s microbiome, broken down by genus:', cex=1.25, font=3, family="Arial Unicode MS")
  textX = 0.5
  textY = -0.75
  text(textX,textY,' DISCLAIMER:\n Results cannot be\n used to draw any\n conclusions about\n health information.', cex=0.7, adj=c(0,NA), font=0.75, col = "gray46", family="Georgia")
} 
dev.off()
# this is the end of the for loop
