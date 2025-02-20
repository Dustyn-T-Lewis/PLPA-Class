#### Preparing Workspace ####
# Load 
library(ggplot2)
setwd("C:/Users/Dutal/OneDrive/Desktop/Classes/PLPA 6820/R/PLPA-Class")
getwd()
data <- read.csv("MycotoxinData.csv", na.strings = "na")

# Define colorblind-friendly palette
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", 
                "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


#### Question 1 #####
Mycotox1 <- ggplot(data, aes(x = Treatment, y = DON, fill = Cultivar)) + #Created a ggplot, defined x, y, box plot color (based on the Cultivar type)
  geom_boxplot(position = position_dodge(), outlier.shape = NA) + #Black dots (possibly outliers) were colored black, I removed them
  geom_point(position = position_jitterdodge(), alpha = .6, aes(color = Cultivar), 
             shape = 21, color = "black", stroke = 0.1) + #Create outline around points so they are visible in front of background box plot
  xlab("") +
  ylab("DON (ppm)") +
  scale_fill_manual (values = c("#56B4E9", "#009E73")) +
  scale_color_manual (values = c("#56B4E9", "#009E73")) +
  facet_wrap(~Cultivar) +
  theme_classic() 
Mycotox1




#### Question 2 ####
str(data)

data$Treatment <- factor(data$Treatment, levels = c("NTC", "Fg", "Fg + 37", "Fg + 40", "Fg + 70"))

Mycotox2 <- ggplot(data, aes(x = Treatment, y = DON, fill = Cultivar)) + 
  geom_boxplot(position = position_dodge(), outlier.shape = NA) + 
  geom_point(position = position_jitterdodge(), alpha = .6, aes(color = Cultivar), 
             shape = 21, color = "black", stroke = 0.1) + 
  ylab("DON (ppm)") +
  scale_fill_manual (values = c("#56B4E9", "#009E73")) +
  scale_color_manual (values = c("#56B4E9", "#009E73")) +
  facet_wrap(~Cultivar) +
  theme_classic() 
Mycotox2



#### Question 3 ####
Mycotox3_X15ADON <- ggplot(data, aes(x = Treatment, y = X15ADON, fill = Cultivar)) + 
  geom_boxplot(position = position_dodge(), outlier.shape = NA) + 
  geom_point(position = position_jitterdodge(), alpha = .6, aes(color = Cultivar), 
             shape = 21, color = "black", stroke = 0.1) + 
  ylab("15ADON") +
  scale_fill_manual (values = c("#56B4E9", "#009E73")) +
  scale_color_manual (values = c("#56B4E9", "#009E73")) +
  facet_wrap(~Cultivar) +
  theme_classic() 

Mycotox3_MassperSeed_mg <- ggplot(data, aes(x = Treatment, y = MassperSeed_mg, fill = Cultivar)) + 
  geom_boxplot(position = position_dodge(), outlier.shape = NA) + 
  geom_point(position = position_jitterdodge(), alpha = .6, aes(color = Cultivar), 
             shape = 21, color = "black", stroke = 0.1) + 
  ylab("Seed Mass (mg)") +
  scale_fill_manual (values = c("#56B4E9", "#009E73")) +
  scale_color_manual (values = c("#56B4E9", "#009E73")) +
  facet_wrap(~Cultivar) +
  theme_classic() 

#### Question 4 ####
library(ggpubr)
combined <- ggarrange(Mycotox3_X15ADON, Mycotox3_MassperSeed_mg, Mycotox1, 
                      ncol = 3, nrow = 1, labels = c("A", "B", "C"),
                      common.legend = TRUE, legend = "right") #Created 2 plots and combined them to represent different outcome variables

#### Question 5 ####
# Plots A) Mycotox3_X15ADON, B) Mycotox3_MassperSeed_mg, C) Mycotox1, 
                     
A <- Mycotox3_X15ADON +
  geom_pwc(aes(group = Treatment), method = "t_test")

B <- Mycotox3_MassperSeed_mg+
  geom_pwc(aes(group = Treatment), method = "t_test")

C <- Mycotox1 +
  geom_pwc(aes(group = Treatment), method = "t_test")

Comb_stat <- ggarrange(A, B, C, 
                      ncol = 3, nrow = 1, labels = c("A", "B", "C"),
                      common.legend = TRUE, legend = "right") 
