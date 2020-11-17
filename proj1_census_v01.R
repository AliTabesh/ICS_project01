library(ggplot2)
library(gridExtra)
library(corrplot)
library(DataExplorer)
library(stats)
library(gridExtra)

#reading the data
censusData <- read.csv("census_2019_1999.csv")
#check the data dimentions and features
str(censusData)

#colnames(censusData) <- c("region", "country", "year", "total fertility per woman", "both sexes LE", "men LE", "women LE")

#make a new dataframe consists of just the data in year 2019 
indexOf2019 <- censusData[["year"]]==2019
dataOf2019 <- censusData[indexOf2019,]

summaryOf2019 <- summary(censusData)


#-------------------------------------------------------------------TASK 1------
# histogram of data from library "DataExplorer"***********************************************************
##Data exploration library (DataExplorer) process for data analysis and model building, so that users 
##could focus on understanding data and extracting insights. The package automatically 
##scans through each variable and does data profiling. Typical graphical techniques 
##will be performed for both discrete and continuous features.
DataExplorer::plot_histogram(dataOf2019, ggtheme = ggpubr::theme_pubr())

# histogram of data from library "ggplot2"*****************************************************
fertality_hitogram <- ggplot(data = dataOf2019, aes(total.fertility.rate.per.woman)) +
    geom_histogram() + xlab("Total fertility rate per woman")
bothSexes_histogram <- ggplot(data = dataOf2019, aes(life.expectancy.both.sexes)) +
    geom_histogram() + xlab("Life expectancy of both")
maleLE_histogram <- ggplot(data = dataOf2019, aes(life.expectancy.male)) +
    geom_histogram() + xlab("Men Life expectancy") 
femaleLE_histogram <- ggplot(data = dataOf2019, aes(life.expectancy.female)) +
    geom_histogram() + xlab("Women Life expectancy")


grid.arrange(fertality_hitogram, bothSexes_histogram, maleLE_histogram, femaleLE_histogram, nrow = 1)

## If necessary then we add boxplots for first task here 
# boxplot of data from library "DataExplorer"***********************************
# boxplot of data from library "ggplot2"****************************************

#frequency table****************************************************************
as.table(summaryOf2019[,4:7])
#-------------------------------------------------------------------TASK 2------

#correlation table NO.1*********************************************************
##correlations of all numeric variables except year
cor_numVar <- cor(dataOf2019[,4:7]) 
corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt")

#correlation table NO.2*********************************************************
##This is a function from "DataExplorrer" library which creates a correlation heatmap
##for all columns. It first uses "cor()" function from "stats" library to calculate
##correlation matrix and then plots it.
### question: what does the other parameters and "theme_pubr()" do?
plot_correlation(dataOf2019[,4:7], ggtheme = ggpubr::theme_pubr(base_size = 10),
                               type = "c", cor_args = list(use = "complete.obs"))
#-------------------------------------------------------------------TASK 3------
#Scatter Plot for showing the variance BETWEEN the regions******************************
betweenRegions_plots <-list()

betweenRegions_plots[[1]] <- ggplot(data = dataOf2019, aes(x = region, y = total.fertility.rate.per.woman, colour = region)) +
    geom_boxplot() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

betweenRegions_plots[[2]] <- ggplot(data = dataOf2019, aes(x = region, y = life.expectancy.both.sexes, colour = region)) +
    geom_boxplot() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

betweenRegions_plots[[3]] <- ggplot(data = dataOf2019, aes(x = region, y = life.expectancy.male, colour = region)) +
    geom_boxplot() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

betweenRegions_plots[[4]] <- ggplot(data = dataOf2019, aes(x = region, y = life.expectancy.female, colour = region)) +
    geom_boxplot() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

grid.arrange(grobs = betweenRegions_plots, nrow = 2)


#Plot for showing the variance WITHIN each regions******************************

fertility_scatter_plots <- list()
uniqueRegions <- unique(dataOf2019$region)
for (i in 1:length(uniqueRegions)) {

    avrageInRegion <- mean(dataOf2019[["total.fertility.rate.per.woman"]][dataOf2019$region ==uniqueRegions[i]])
    
    fertility_scatter_plots[[i]] <- ggplot(dataOf2019[dataOf2019$region==uniqueRegions[i],], 
                    aes(x = country, y = total.fertility.rate.per.woman)) + 
        geom_point() + 
        geom_hline(yintercept=avrageInRegion, linetype="dashed", color = "red") +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
        xlab(uniqueRegions[i])

}
grid.arrange(grobs = fertility_scatter_plots, nrow = 2)

bothSexesLE_scatter_plots <- list()
uniqueRegions <- unique(dataOf2019$region)
for (i in 1:length(uniqueRegions)) {
    
    avrageInRegion <- mean(dataOf2019[["life.expectancy.both.sexes"]][dataOf2019$region ==uniqueRegions[i]])
    
    bothSexesLE_scatter_plots[[i]] <- ggplot(dataOf2019[dataOf2019$region==uniqueRegions[i],], 
                                 aes(x = country, y = life.expectancy.both.sexes)) + 
        geom_point() + 
        geom_hline(yintercept=avrageInRegion, linetype="dashed", color = "red") +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
        xlab(uniqueRegions[i])
    
}
grid.arrange(grobs = bothSexesLE_scatter_plots, nrow = 2)

maleLE_scatter_plots <- list()
uniqueRegions <- unique(dataOf2019$region)
for (i in 1:length(uniqueRegions)) {
    
    avrageInRegion <- mean(dataOf2019[["life.expectancy.male"]][dataOf2019$region ==uniqueRegions[i]])
    
    maleLE_scatter_plots[[i]] <- ggplot(dataOf2019[dataOf2019$region==uniqueRegions[i],], 
                                 aes(x = country, y = life.expectancy.male)) + 
        geom_point() + 
        geom_hline(yintercept=avrageInRegion, linetype="dashed", color = "red") +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
        xlab(uniqueRegions[i])
    
}
grid.arrange(grobs = maleLE_scatter_plots, nrow = 2)

femaleLE_scatter_plots <- list()
uniqueRegions <- unique(dataOf2019$region)
for (i in 1:length(uniqueRegions)) {
    
    avrageInRegion <- mean(dataOf2019[["life.expectancy.female"]][dataOf2019$region ==uniqueRegions[i]])
    
    femaleLE_scatter_plots[[i]] <- ggplot(dataOf2019[dataOf2019$region==uniqueRegions[i],], 
                                 aes(x = country, y = life.expectancy.female)) + 
        geom_point() + 
        geom_hline(yintercept=avrageInRegion, linetype="dashed", color = "red") +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
        xlab(uniqueRegions[i])
    
}
grid.arrange(grobs = femaleLE_scatter_plots, nrow = 2)


