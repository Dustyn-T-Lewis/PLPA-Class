#### Question 4 ####

z <- c(1:200) #created a vector named "z" with values 1-200

mean(z) #mean of vector "z"
sd(z) #standard deviation of vector "z"

zlog <- z>30 #Create a logical vector named zlog that is 'TRUE' for z>30 and 'FALSE' otherwise.

zdf <- data.frame(z,zlog) #Make a dataframe with z and zlog as columns. Name the dataframe zdf
colnames(zdf) <- c("zvec", "zlog") #Change the column names in your new dataframe to equal “zvec” and “zlogic”

zdf$zsquared <- zdf$zvec^2 #new column equal to zvec squared, name "zsquared" 

#subset "zdf" with and without subset function to include "zsquared" >10 & <100
subset1 <- subset(zdf,zsquared > 10 & zsquared <100) 
subset2 <- zdf[zdf$zsquared >10 & zdf$zsquared <100,]

#Subset the zdf dataframe to only include the values on row 26
subset3 <- zdf[26,]

#Subset the zdf dataframe to only include the values in the column zsquared in the 180th row.
subset4 <- zdf[180, "zsquared"]
 

