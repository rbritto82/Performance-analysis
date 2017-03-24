#Loading required libraries
library(ggplot2)
library(ggthemes) 
library(extrafont)
library(scales)
library(ggrepel)
library(grid) 
library(RColorBrewer)
library(gridExtra)

#Function to create the plots with dates in the x-axis
createPlotDate <- function(inputData, plotY, plotX, plotLabel, plotLineSize, plotTitle, plotColor, 
                           plotYLabel, plotXLabel, plotDateFormat="%y/%m", plotDateBreak="1 month", 
                           plotMinDate, plotMaxDate, plotMaxOffset=0, plotScaleIncrement,
                           plotObservationLabel=FALSE){ 
  
  outputPlot <- ggplot(aes(y = inputData[[plotY]], x = inputData[[plotX]], label = inputData[[plotLabel]]), data = inputData, stat="identity") +
    geom_line(size=plotLineSize, color = plotColor) + #add a line to observations
    geom_point() + #add points for each observation
    ggtitle(plotTitle) + #add title
    labs(x=plotXLabel, y=plotYLabel) + #change the labels
    theme(legend.position="bottom", legend.direction="horizontal",legend.title = element_blank()) + #set legend theme
    theme(axis.text.x = element_text(angle=90)) +
    scale_x_date(labels = date_format(plotDateFormat), date_breaks = plotDateBreak, limits = c(plotMinDate, plotMaxDate))
  
  if (min(inputData[[plotY]]) > 0){
    outputPlot <- outputPlot + scale_y_continuous(breaks=seq(0, max(inputData[[plotY]])+plotMaxOffset, plotScaleIncrement))
  }else{
    outputPlot <- outputPlot + scale_y_continuous(breaks=seq(min(inputData[[plotY]]), max(inputData[[plotY]])+plotMaxOffset, plotScaleIncrement))
  }
     
  
  if (plotObservationLabel == TRUE)
    outputPlot <-  outputPlot + geom_label_repel()
  
  return(outputPlot)
}

#Function to create the plots with dates in the x-axis
createPlotContinuos <- function(inputData, plotY, plotX, plotLabel, plotLineSize, plotTitle, plotColor, 
                                plotYLabel, plotXLabel, plotOffsetX=0, plotScaleIncrementX, 
                                plotOffsetY=0, plotScaleIncrementY, plotObservationLabel=FALSE, plotLine=TRUE){  
  
  outputPlot <- ggplot(aes(y = inputData[[plotY]], x = inputData[[plotX]], label = inputData[[plotLabel]]), data = inputData, stat="identity") +
    geom_point() + #add points for each observation
    ggtitle(plotTitle) + #add title
    labs(x=plotXLabel, y=plotYLabel) + #change the labels
    theme(legend.position="bottom", legend.direction="horizontal",legend.title = element_blank()) + #set legend theme
    theme(axis.text.x = element_text(angle=90))
  
  if (min(inputData[[plotY]]) > 0){
    outputPlot <- outputPlot + scale_y_continuous(breaks=seq(0, max(inputData[[plotY]])+plotOffsetY, plotScaleIncrementY))
  }else{
    outputPlot <- outputPlot + scale_y_continuous(breaks=seq(min(inputData[[plotY]]), max(inputData[[plotY]])+plotOffsetY, plotScaleIncrementY))
  }
  
  if (min(inputData[[plotX]]) > 0){
    outputPlot <-  outputPlot + scale_x_continuous(breaks=seq(0, max(inputData[[plotX]])+plotOffsetX, plotScaleIncrementX))
  }else{
    outputPlot <-  outputPlot + scale_x_continuous(breaks=seq(min(inputData[[plotX]]), max(inputData[plotX])+plotOffsetX, plotScaleIncrementX))
  }
  
  if (plotLine == TRUE)
    outputPlot <-  outputPlot + geom_line(size=plotLineSize, color = plotColor) #add a line to observations
  
  if (plotObservationLabel == TRUE)
    outputPlot <-  outputPlot + geom_label_repel()
  
  return(outputPlot)
} 