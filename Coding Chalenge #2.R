# GGPLOT in library
library(ggplot2)

# Read in the data
data <- read.csv("C:/Users/Dutal/OneDrive/Desktop/Classes/PLPA 6820/Canvas HW/MycotoxinData.csv")

# Boxplot Creation
ggplot(data, aes(x = Treatment, y = DON, color = Cultivar)) +
  geom_boxplot() +
  xlab("") +
  ylab("DON (ppm)")



