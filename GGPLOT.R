# DATA VISUALIZATION

#### Base R Plot ####
str(mtcars)
plot(x=mtcars$wt, y=mtcars$mpg,
     xlab = "car weight",
     ylab = "miles per gallon",
     pch = 20)



#### GGPLOT ####

install.packages("ggplot2")
library(ggplot2)
  
  # package has been installed

# Basic Function

ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point(aes(color = wt)) + 
  geom_smooth(method = lm, se = FALSE) + 
  xlab("weight") + ylab("miles per gallon") +
  scale_color_gradient(low = "forestgreen", high = "black")
  

#### Categorical + Numerical ####

bull.richness <- read.csv("C:/Users/Dutal/OneDrive/Desktop/Classes/PLPA 6820/Canvas HW/Bull_richness.csv.csv")

bull.richness.soy.no.till <- bull.richness[bull.richness$Crop == "Soy" & 
  bull.richness$Treatment == "No-till",] # subset to soy data

# Box Plot

ggplot(bull.richness.soy.no.till, aes(x = GrowthStage, y = richness, color = Fungicide)) + 
  geom_boxplot() + 
  xlab("") + 
  ylab("Bulleribasidiaceae richness") +
  geom_point(position=position_jitterdodge(dodge.width=0.9)) 
  
# Bar Chart

ggplot(bull.richness.soy.no.till, aes(x = GrowthStage, y = richness, color = Fungicide, fill = Fungicide)) + 
  stat_summary(fun = mean, geom ="bar", position = "dodge") +
  stat_summary(fun.data = mean_se, geom = "errorbar", position = "dodge") + 
  xlab("") + 
  ylab("Bulleribasidiaceae richness") 

# Line Connecting Means

ggplot(bull.richness.soy.no.till, aes(x = GrowthStage, y = richness, group = Fungicide, color = Fungicide)) + 
  stat_summary(fun = mean,geom ="line") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.5) +
  ylab("Bulleribasidiaceae \n richness") + 
  xlab("") 

# Faceting
ggplot(bull.richness, aes(x = GrowthStage, y = richness, group = Fungicide, color = Fungicide)) + 
  stat_summary(fun=mean,geom="line") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.5) +
  ylab("Bulleribasidiaceae \n richness") + 
  xlab("") +
  facet_wrap(~Treatment*Crop, scales = "free")
