#!/usr/bin/env Rscript
#
# makevenndiag.r
# Modified by Hannah Voelker
# Copyright (c) 2016 BioSeq
#
# makes a pdf of venn diagrams for a set of samples 
#
#import
require(plotrix)
library(extrafont)

genuscounts <<- read.csv(file.choose(), header = TRUE, sep = ",")
genuspairs <<- read.table(file.choose(), header= FALSE)

pdf(file= "Sizer_venndiagrams.pdf", height = 11, width = 8.5, onefile = TRUE)
#loop through the .txt file of pairs and make a Venn diagram for each
for (j in 1:nrow(genuspairs)){
sample1 <- genuspairs[j,1]
sample2 <- genuspairs[j,2]

#now loop through all the columns to find these two samples
# set up the top ten names
for(i in 2:ncol(genuscounts)){
  if (sample1 == names(genuscounts[i])){
    thisColumn <- genuscounts[,c(1,i)]
    topten1 <- head(thisColumn[order(thisColumn[,2],decreasing = TRUE), ], n = 10)
  }
  if(sample2 == names(genuscounts[i])){
    thisColumn <- genuscounts[,c(1,i)]
    topten2 <- head(thisColumn[order(thisColumn[,2], decreasing = TRUE), ], n = 10)
  }
}
# we have to make these samples ready for the Venn diagram
makeVennList <- function(vennList){
  vennList <- unlist(strsplit(as.character(vennList), ","))
  indexofOther = match("Other", vennList)
  return(vennList)
}
vennList1 <- makeVennList(topten1[,1])
vennList2 <- makeVennList(topten2[,1])

par(mfrow=c(2,1))

#parameters for layout of the Venn diagram and text
#vennX1 = 0.375 centers the Venn diagram on the page
radius = 0.24
vennX1=0.375; vennY1=0.45
vennX2=vennX1+radius; vennY2=vennY1
#placement of lists of microbes within each circle
textX1=vennX1-0.05; textY1=vennY1+0.3
textX2=vennX1+0.22; textY2=textY1
textSize = 0.88
#placement of the title near the top of each circle (e.g. SH07_RC)
titleX1=textX1+0.03; titleY1=textY1+0.06
titleX2=titleX1+0.28; titleY2=titleY1
titleSize = 1.80
#first Venn diagram has message 1, second and third Venn diagrams have message 2
message1 = "SAME person, DIFFERENT body sites"
message2 = "DIFFERENT people, SAME body site"



drawVennDiagram <- function(message, leftSampleName, rightSampleName, leftVennList, rightVennList){
  draw.circle(vennX1,vennY1,radius,nv=100,border=NULL,col=NA,lty=1,lwd=1)
  draw.circle(vennX2,vennY1,radius,nv=100,border=NULL,col=NA,lty=1,lwd=1)
  legend(textX1,textY1, leftVennList, cex=textSize, bty="n",adj=c(1,NA))
  legend(textX2,textY2, rightVennList, cex=textSize, bty="n")
  text(titleX1,titleY1,leftSampleName, cex=titleSize, font=2, family = "Arial Rounded MT Bold")
  text(titleX2,titleY2,rightSampleName, cex=titleSize, font=2, family = "Arial Rounded MT Bold")
  text(titleX1+0.02,titleY1+0.08,"Sample 1", cex=0.8, font=3)
  text(titleX2-0.02,titleY2+0.08,"Sample 2", cex=0.8, font=3)
  text(0,0.99, message, cex=2, font=2, adj=c(0,NA), family = "Arial Rounded MT Bold")
}

#Draw all three Venn diagrams
plot.new()
drawVennDiagram(message1, sample1, sample2, vennList1, vennList2)
plot.new()
drawVennDiagram(message2, sample1, "", vennList1, "")
plot.new()
drawVennDiagram(message2, sample2, "", vennList2, "")
plot.new()
text(0,0.5, " Calculate for every pair of samples: \n 
  How many microbes are found in BOTH Sample 1 and Sample 2?
  Answer 1: \n\n\n\n
  How many microbes are found in either sample?
  Answer 2: \n\n\n\n
  Calculate the Jaccard Similarity:
  Divide Answer 1 by Answer 2 and multiple by 100%", 
  cex = 1, font = 1.75, adj=c(0,NA), family = "Arial Unicode MS"
)
}
dev.off()
