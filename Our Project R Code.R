#INSTALL THE GGPLOT2 PACKAGE (MUST BE DONE ONCE)
install.packages('ggplot2')

#LOAD THE GGPLOT2 LIBRARY (MUST BE DONE EVERY TIME)
library(ggplot2)

##BUILDING OUR VISUALS USING GGPLOT2

##FIRST VISUAL - SCATTERPLOT SHOWING HOURLY MEAN WAGE AND EMPLOYMENT PER 1,000 JOBS)
ggplot(df, aes(x = emp, y = Hourlywage)) + 
  geom_point() +
  xlab('Employment per 1,000 Jobs') +
  ylab('Hourly Mean Wage') +
  geom_smooth() +
  ggtitle('Scatterplot of Hourly Mean Wage and Employment per 1,000 Jobs') +
  theme(plot.title = element_text(hjust=.5))



#SECOND VISUAL - REGION AND HOME VALUE VIOLIN PLOT
ggplot(df, aes(Region, homeval)) +
  geom_violin() +
  ylab('Typical $ Value of 3 Bed Homes') +
  ylim (0,1000000) +
  ggtitle('Violin Plot of Region and Typical Dollar Value of Three Bedroom Homes') +
  theme(plot.title = element_text(hjust=.5))
  


#THIRD - geom bar with region and employment
ggplot(df, aes(Employment, Region, color = Region)) +
  geom_point() +
  geom_smooth(method = lm, size = 1) +
  ggtitle('Geom Plot of Employment by Region') +
  theme(plot.title = element_text(hjust=.5))
  


#FOURTH- GEOM TEXT WITH HOURLY WAGE AND HOME VALUE
ggplot(df, aes(Hourlywage, homeval, color=state)) +
  geom_text(aes(label = state), check_overlap = TRUE) +
  xlab('Annual Hourly Wage') +
  ylab('Typical $ Value of 3 Bedroom Home') +
  ggtitle('Scatterplot of Annual Hourly Wage and Typical Value of 3 Bedroom Home Based on State') +
  theme(plot.title = element_text(hjust=.5))


##FIFTH- GEOM BOXPLOT WITH REGION AND LOCATION QUOTIENT
ggplot(df, aes(Region, LocationQuotient)) + geom_boxplot(aes(fill=Region))+ ylab("Location Quotient") + 
  ggtitle('Concentration of Accountant and Auditor jobs') +scale_y_continuous(breaks = c(.5,.6,.7,.8, .9,1, 1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2))+
  theme(plot.title = element_text(hjust =.5))


###SIXTH- GEOM REGION AND MEAN SALARY TO HOME PRICE RATIO 
ggplot(df, aes(Region, MeanSalaryHomePriceRatio)) + geom_hex(aes(color= Region)) + xlab("Region")+ ylab("Salary and House Price Ratio")+ggtitle("House Price to Income Ratio")+ theme(plot.title = element_text(hjust =.5))+scale_y_continuous(breaks = c(.5,.1,.15,.2,.25,.3,.35,.4,.45,.5,.55,.6,.65,.7,.75,.8,.85,.9,.95,1))

