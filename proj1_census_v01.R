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
fertality_hitogram <- ggplot(data = dataOf2019, aes(total.fertility.rate.per.woman, group = total.fertility.rate.per.woman)) +
    geom_histogram() +
    geom_vline(aes(color = "Mean"), xintercept=mean(dataOf2019$total.fertility.rate.per.woman), linetype="dashed", color = "blue", show.legend = TRUE) +
    geom_vline(aes(color = "Median"), xintercept=median(dataOf2019$total.fertility.rate.per.woman), linetype="dashed", color = "red", show.legend = TRUE) +
    scale_colour_manual("Lines", values = c("blue", "red")) +
    xlab("Total fertility rate per woman")
bothSexes_histogram <- ggplot(data = dataOf2019, aes(life.expectancy.both.sexes)) +
    geom_histogram() + xlab("Life expectancy of both")
maleLE_histogram <- ggplot(data = dataOf2019, aes(life.expectancy.male)) +
    geom_histogram() + xlab("Men Life expectancy") 
femaleLE_histogram <- ggplot(data = dataOf2019, aes(life.expectancy.female)) +
    geom_histogram() + xlab("Women Life expectancy")


grid.arrange(fertality_hitogram, bothSexes_histogram, maleLE_histogram, femaleLE_histogram, nrow = 1)

#-------------------------------------------------------------------TASK 2------
#Scatter plot
ggplot(dataOf2019,aes(x=life.expectancy.male, y=life.expectancy.female )) +
    geom_point() +
    geom_smooth(method='lm', formula= y~x, colour="green") +
    labs(x="Male Life Expectancy", y="Female Life Expectancy") 

ggplot(dataOf2019,aes(x=total.fertility.rate.per.woman, y=life.expectancy.both.sexes )) +
    geom_point() +
    geom_smooth(method='lm', formula= y~x, colour="green") + 
    labs(x="Fertility rate per Woman", y="Life Expectancy for both sexes")


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
#Scatter Plot for showing the variance BETWEEN the regions**********TASK 3.1****
betweenRegions_plots <-list()  ##list to store plots of task 3.1

regionsInOrder <- c("Northern America", "Central America", "South America", "Caribbean", 
                    "Western Europe", "Northern Europe", "Southern Europe", "Eastern Europe", 
                    "Western Africa", "Southern Africa", "Middle Africa", "Northern Africa", "Eastern Africa", 
                    "Western Asia", "South-Central Asia", "South-Eastern Asia", "Eastern Asia", 
                    "Micronesia", "Australia/New Zealand", "Melanesia", "Polynesia")

#Make a copy of dataOf2019 and then to reorder the labels of regions in boxplots add 
# uppercase letters in desired order at the beginning of each row in region column 
ordered2019 <- dataOf2019
for (i in 1:length(regionsInOrder)) {
    ordered2019[ordered2019$region==regionsInOrder[i],1] <- paste(LETTERS[i],")"," ", ordered2019[ordered2019$region==regionsInOrder[i],1], sep = "")
}

betweenRegions_plots[[1]] <- ggplot(data = ordered2019, aes(x = region, y = total.fertility.rate.per.woman, colour = region)) +
    geom_boxplot() + 
    geom_hline(yintercept=mean(ordered2019$total.fertility.rate.per.woman), linetype="dashed", color = "red") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    labs(x="Regions", y="Fertility rate per Woman")

betweenRegions_plots[[2]] <- ggplot(data = ordered2019, aes(x = region, y = life.expectancy.both.sexes, colour = region)) +
    geom_boxplot() + 
    geom_hline(yintercept=mean(dataOf2019$life.expectancy.both.sexes), linetype="dashed", color = "red") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    labs(x="Regions", y="Life Expectency for both sexes")

betweenRegions_plots[[3]] <- ggplot(data = ordered2019, aes(x = region, y = life.expectancy.male, colour = region)) +
    geom_boxplot() + 
    geom_hline(yintercept=mean(dataOf2019$life.expectancy.male), linetype="dashed", color = "red") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    labs(x="Regions", y="Male Life Expectency")

betweenRegions_plots[[4]] <- ggplot(data = ordered2019, aes(x = region, y = life.expectancy.female, colour = region)) +
    geom_boxplot() +     
    geom_hline(yintercept=mean(dataOf2019$life.expectancy.female), linetype="dashed", color = "red") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    labs(x="Regions", y="Female Life Expectancy")

grid.arrange(grobs = betweenRegions_plots, nrow = 2)


#Plot for showing the variance WITHIN each regions******************TASK 3.2.1**

uniqueRegions <- unique(ordered2019$region)

fertility_scatter_plots <- list()
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



#calculate variances of each column for the data in ****************TASK 3.2.2**
# each region and put all Vars in a dataframe called "varianceInRegions" 

varianceInRegions <- data.frame(uniqueRegions)

for (i in 1:length(uniqueRegions)) {
    
    for (j in 4:7) {
        temp <- var(dataOf2019[ordered2019$region==uniqueRegions[i] , j])
        varianceInRegions[i,j-2] <- format(round(temp, 2), nsmall = 2)
    }
    
}
colnames(varianceInRegions)[2:5] <- colnames(ordered2019)[4:7]

# Now we add the Variance of each column for whole data and save it in last row 
# and name the last row "World"
write.csv(varianceInRegions,"Variance_table.csv", row.names = FALSE)


