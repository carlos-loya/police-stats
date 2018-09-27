require(ggplot2)
require(dplyr)


income <- read.csv(file="../01 Data/census-income-data.csv", header=TRUE, sep=",")
fatalPoliceShootings <- read.csv(file="../01 Data/fatal-police-shootings-data.csv", header=TRUE, sep=",")
incomeOfTheFatallyShot <- read.csv(file="../01 Data/fatal-police-shootings-and-census-income-data.csv", header=TRUE, sep=",")

#Histogram-----------------------------------------------------------------------------

histogram <- ggplot(incomeOfTheFatallyShot) + geom_histogram(aes(Per_Capita_Income, fill = Per_Capita_Income), binwidth = 700) + ggtitle("Count of the per capita of Fatal Police Shooting Individuals")
#End Histogram-----------------------------------------------------------------------------

#Box Plot-----------------------------------------------------------------------------

boxplot <- ggplot(incomeOfTheFatallyShot) + geom_boxplot(aes(x = flee, y = Median_Family_Income, fill = flee)  ) + ggtitle("Median Family Income Boxplot of Individuals who suffered from \nFatal Police Shootings in 2015. \n\nThe x axis is the fleeing type, i.e. if anyone was fleeing and if so how.")
#End Box Plot-----------------------------------------------------------------------------

#Scatter Plot-----------------------------------------------------------------------------
scatterplot <- ggplot(incomeOfTheFatallyShot) + geom_point(aes(x = GINI, y = Median_Family_Income, color = armed)) + ggtitle("This plot shows the median family income vs. the gini index for individuals \nfrom fatal police shootings in 2015. \n\nThe color is the weapon said individuals were armed with.")

#End Scatter Plot-----------------------------------------------------------------------------


#CrossTabs-----------------------------------------------------------------------------
# First Plot

genderMentalIll <- dplyr::select(incomeOfTheFatallyShot, Per_Capita_Income, gender, signs_of_mental_illness)

countTotal <- genderMentalIll %>% mutate(Per_Capita_Range = ifelse(Per_Capita_Income < 26500, "low", ifelse(Per_Capita_Income < 31000 & Per_Capita_Income > 26500, "medium","high"))) %>% dplyr::count(Per_Capita_Range, gender, signs_of_mental_illness)


lowCapitaRange <- countTotal %>% filter(Per_Capita_Range == "low")
mediumCapitaRange <- countTotal %>% filter(Per_Capita_Range == "medium")
highCapitaRange <- countTotal %>% filter(Per_Capita_Range == "high")

capitaRangePlot <- ggplot() + 
  geom_text(data = lowCapitaRange, colour="#CC0000", aes(x=gender, y=signs_of_mental_illness, label = n),nudge_x = -0.2, size=10) + 
  geom_text(data = mediumCapitaRange, colour="000099", aes(x=gender, y=signs_of_mental_illness, label = n),nudge_x = 0, size=10) + 
  geom_text(data = highCapitaRange, colour="blue", aes(x=gender, y=signs_of_mental_illness, label = n),nudge_x = 0.2, size=10) + ggtitle("This plot shows the signs of mental illness vs. gender for individuals \nfrom fatal police shootings in 2015. \n\nThe text is ranges of count of per capita income for the individuals in \neach category. Red is the low per capita income, green is the middle \nand blue is the high.") 

# Second Plot

plotDF <- dplyr::inner_join(income, fatalPoliceShootings, by = c("State" = "state")) %>% 
  dplyr::group_by(gender, race) %>% 
  dplyr::summarize(avg_median_income = mean(Median_Income))

subset <- dplyr::inner_join(income,fatalPoliceShootings, by = c("State" = "state")) %>% dplyr::filter(Median_Income >= 46000 & Median_Income <= 62000) %>%
  dplyr::group_by(gender, race) %>% 
  dplyr::summarize(avg_median_income = mean(Median_Income)) 

genderRacePlot <- ggplot() + 
  geom_text(data = plotDF, aes(x= gender, y=race, label = avg_median_income), size=8) + 
  geom_text(data = subset, aes(x=gender, y=race, label = avg_median_income), nudge_y = -.5, size=4)   + ggtitle("This plot shows the race vs. gender for individuals \nfrom fatal police shootings in 2015. \n\nThe large text Represents being Part of the mean of the top 25% \nHighest Median Incomes. Smaller text is the average of all \nremaining median incomes.")

#Third Plot

test <- incomeOfTheFatallyShot %>% dplyr::group_by(race,flee) %>% dplyr::summarise(income = median(Median_Income), MedianFamilyIncomePerCapitaIncomeRatio = median(Median_Family_Income/Per_Capita_Income))

raceFleePlot <- ggplot(test) + 
  theme(axis.text.x=element_text(angle=90, size=16, vjust=0.5)) + 
  theme(axis.text.y=element_text(size=16, hjust=0.5)) +
  geom_text(aes(x=race, y=flee, label = income), size=6)+
  geom_tile(aes(x=race, y=flee, fill=MedianFamilyIncomePerCapitaIncomeRatio), alpha=0.50) + ggtitle("This R visualization was created using the calculated fields of \nMedian(MedianFamilyIncome/PerCapitaIncome) and \nplotting based on how the individual from the fatal police \nshooting fled against the race of the individual shot.")

#End CrossTabs-----------------------------------------------------------------------------

#BarCharts -----------------------------------------------------------------------------

# Median Income by Race

incomeByRace <- incomeOfTheFatallyShot %>% dplyr::group_by(race, gender) %>% dplyr::summarize(avg_median_income = mean(Median_Income), sum_income = sum(Median_Income)) %>% dplyr::group_by(race, gender, avg_median_income) %>% dplyr::summarize(window_avg_income = mean(sum_income))

incomeByRacePlot <- ggplot(incomeByRace, aes(x = gender, y = avg_median_income)) + 
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::comma) + 
  facet_wrap(~race, ncol=1) +
  coord_flip() +
  geom_text(mapping=aes(x=gender, y=avg_median_income, label=round(avg_median_income - window_avg_income)),colour="blue", hjust=-.5)  + ggtitle("This plot shows the race vs. gender for individuals \nfrom fatal police shootings in 2015 the bars are the \naverage median income. \n\nThe blue numbers are a table calculation (the sum of the median \nincome - the window average of the median income).")

# Median Income by Fleeing

fleeMentalIncome <- incomeOfTheFatallyShot %>% dplyr::select(flee,signs_of_mental_illness,Median_Income) %>% group_by(signs_of_mental_illness,flee) %>% dplyr::filter(flee %in% c('Car','Foot','Not fleeing')) %>% summarise(Median_income = median(Median_Income))


fleePlot <- ggplot(fleeMentalIncome, aes(x=signs_of_mental_illness, y=Median_income, fill=signs_of_mental_illness)) +
  theme(axis.text.x=element_text(angle=0, size=12, vjust=0.5)) + 
  theme(axis.text.y=element_text(size=12, hjust=0.5)) + 
  geom_bar(stat = "identity") + 
  facet_wrap(~flee, ncol=1) + 
  coord_flip() + 
  geom_hline(aes(yintercept = median(Median_income)), color="purple") + ggtitle("Income of Individuals from a Fatal Police Shooting. \nBroken up by if they were feeling and/or had signs of mental illness.")
#plot(fleePlot)


# Inequality Index for High Income Criminals

inequalityIndexforHighIncome <- incomeOfTheFatallyShot %>% dplyr::select(id,GINI,Median_Income) %>% mutate(Median_Income_Range = ifelse(Median_Income < 50000, "low", ifelse(Median_Income < 60000 & Median_Income > 50000, "medium","high"))) %>% dplyr::filter(Median_Income_Range == 'high',id > 1000) 
  
inequalityPlot <- ggplot(inequalityIndexforHighIncome, aes(x=id, y=GINI, fill=Median_Income)) +
  theme(axis.text.x=element_text(angle=0, size=12, vjust=0.5)) + 
  theme(axis.text.y=element_text(size=12, hjust=0.5)) +
  geom_bar(stat = "identity") + ggtitle("Inequality Index For High Income Individuals from 2015 Fatal Police \nShootings. Each ID represents a person.")
plot(inequalityPlot)

#End Barcharts-----------------------------------------------------------------------------


# require("grid")
# 
# setwd("../03 Visualizations")
# 
# png("allPlots.png", width = 45, height = 45, units = "in", res = 120)
# grid.newpage()
# pushViewport(viewport(layout = grid.layout(2, 2)))   
# 
# # Print Plots
# print(incomeByRacePlot, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))  
# print(fleePlot, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
# print(inequalityPlot, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
# 
# dev.off()
