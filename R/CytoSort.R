# @import plyr
# @import readr
# @import ggplot2
# @import GGally
# @import dplyr
# @import mlbench
CytoSort<-function(importPath,exportPath)
{
  if(!(file.exists(importPath)))
  {
    return("Import Path Does Not Exist Try Again")
  }
  if(file.exists(exportPath))
  {
    return("Export Path Already Exists. Try A New Path")
  }
  cytoData <- read.csv(file = importPath)
  TF = names(cytoData[2])
  TF = rep(c(TF), times = 20)
  top20Data <- data.frame(TF)
  #View(top20Data)
  count = 1
  while(count < ncol(cytoData))
  {
    curcol = unlist(cytoData[count+1])
    TF = names(cytoData[count+1])
    TFData <- cytoData[with(cytoData,order(-curcol)),]
    #View(TFData)
    TFData <- TFData[1:20,]
    #View(TFData)
    TFData <-TFData[c("GENE", TF)]
    colnames(TFData)[2] <- "W.Values"
    
    if(count == 1)
    {
      top20Data <- cbind(top20Data,TFData)
      
    }
    else
    {
      TFData <- data.frame(TF, TFData)
      top20Data <- rbind(top20Data,TFData)
      
    }
    #View(TFData)
    #View(top20Data)
    count=count+1
    
    
    
  }
  View(top20Data)
  write.csv(top20Data,exportPath,row.names=FALSE)
}


#Import: "C:/Users/matth/OneDrive/Desktop/W.csv"
#Export: "C:/Users/matth/OneDrive/Desktop/Data.csv"

