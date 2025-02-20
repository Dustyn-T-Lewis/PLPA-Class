#### Load data and libraries #### 
library(tidyverse)   # Data manipulation & ggplot2
library(ggpubr)      # For arranging plots
library(ggrepel)     # For avoiding overlapping text labels

# Define a colorblind-friendly palette
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", 
                "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# Load dataset #
# Read bacterial alpha diversity dataset
sample.data.bac <- read.csv("BacterialAlpha.csv", na.strings = "na")

# Convert categorical variables to factors
sample.data.bac$Time_Point <- as.factor(sample.data.bac$Time_Point)
sample.data.bac$Crop <- factor(sample.data.bac$Crop, levels = c("Soil", "Cotton", "Soybean"))

# Check structure of dataset
str(sample.data.bac)

#### Figure 2A: Bacterial Evenness Boxplot with Jittered Points #### 
bac.even <- ggplot(sample.data.bac, aes(x = Time_Point, y = even, color = Crop)) +  
  geom_boxplot(position = position_dodge(0.85)) +  # Boxplots with dodge
  geom_point(position = position_jitterdodge(0.05), alpha = 0.5) +  # Jittered points for spread
  ylab("Pielou's evenness") +  
  xlab("Hours post sowing") +  
  scale_color_manual(values = cbbPalette, name = "", labels = c("Soil no seeds", "Cotton", "Soybean")) +  
  theme_classic()  # Clean theme
bac.even

#### Figure 2B: Water Imbibition vs Time (Grouped by Crop) #### 
sample.data.bac.nosoil <- subset(sample.data.bac, Crop != "Soil")  # Remove "Soil" category

water.imbibed <- ggplot(sample.data.bac.nosoil, aes(x = Time_Point, y = 1000 * Water_Imbibed, color = Crop)) +  
  geom_jitter(width = 0.5, alpha = 0.5) +  # Transparent jittered points
  stat_summary(fun = mean, geom = "line", aes(group = Crop)) +  # Mean trend line
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.5) +  # Standard error bars
  xlab("Hours post sowing") +  
  ylab("Water Imbibed (mg)") +  
  scale_color_manual(values = c(cbbPalette[[2]], cbbPalette[[3]]), name = "") +  
  theme_classic() +  
  theme(strip.background = element_blank(), legend.position = "none") +  
  facet_wrap(~Crop, scales = "free")  # Facet by Crop, allowing different scales
water.imbibed

#### Figure 2C: Correlation Between Water Imbibition and Evenness #### 
water.imbibed.cor <- ggplot(sample.data.bac.nosoil, aes(y = even, x = 1000 * Water_Imbibed, color = Crop)) +  
  geom_point(aes(shape = Time_Point)) +  # Different shapes for time points
  geom_smooth(se = FALSE, method = "lm") +  # Linear trend line
  xlab("Water Imbibed (mg)") +  
  ylab("Pielou's evenness") +  
  scale_color_manual(values = c(cbbPalette[[2]], cbbPalette[[3]]), name = "", labels = c("Cotton", "Soybean")) +  
  scale_shape_manual(values = c(15, 16, 17, 18), name = "", labels = c("0 hrs", "6 hrs", "12 hrs", "18 hrs")) +  
  theme_classic() +  
  theme(strip.background = element_blank(), legend.position = "none") +  
  facet_wrap(~Crop, scales = "free")  # Facet by Crop
water.imbibed.cor

#### Combine Plots into a Single Figure #### 
figure2 <- ggarrange(
  water.imbibed,  
  bac.even,  
  water.imbibed.cor,  
  labels = "auto",  # Auto-label plots (A, B, C)
  nrow = 3,  
  ncol = 1,  
  legend = FALSE  # No legend in combined figure
)
figure2

#### Adding Statistical Comparisons: ANOVA #### 
bac.even + stat_compare_means(method = "anova")  # Apply ANOVA to groups

#### Pairwise Comparisons with p-values #### 
bac.even + geom_pwc(aes(group = Crop), method = "t_test", label = "p.adj.format")

#### Displaying Correlation Data #### 
water.imbibed.cor + stat_cor()  # Correlation coefficient

#### Adding Linear Regression Equation #### 
water.imbibed.cor + 
  stat_cor(label.y = 0.7) +
  stat_regline_equation()

#### Volcano Plot for Differential Abundance #### 
diff.abund <- read.csv("diff_abund.csv")
diff.abund$log10_pvalue <- -log10(diff.abund$p_CropSoybean)
diff.abund.label <- diff.abund[diff.abund$log10_pvalue > 30,]  # Label only significant points

volcano <- ggplot() + 
  geom_point(data = diff.abund, aes(x = lfc_CropSoybean, y = log10_pvalue, color = diff_CropSoybean)) + 
  geom_text_repel(data = diff.abund.label, aes(x = lfc_CropSoybean, y = log10_pvalue, color = diff_CropSoybean, label = Label)) + 
  scale_color_manual(values = cbbPalette, name = "Significant") +
  theme_classic() + 
  xlab("Log fold change Soil vs. Soybean") +
  ylab("-log10 p-value")
volcano

#### Volcano Plot with Highlighted Significant Points #### 
volcano <- ggplot() + 
  geom_point(data = diff.abund, aes(x = lfc_CropSoybean, y = log10_pvalue)) + 
  geom_point(data = diff.abund.label, aes(x = lfc_CropSoybean, y = log10_pvalue), color = "red", shape = 17, size = 4) +
  geom_text_repel(data = diff.abund.label, aes(x = lfc_CropSoybean, y = log10_pvalue, label = Label), color = "red") + 
  theme_classic() + 
  xlab("Log fold change Soil vs. Soybean") +
  ylab("-log10 p-value")
volcano

#### Stand Counts with ANOVA Significance Letters #### 
library(lme4)
library(emmeans)
library(multcomp)
library(multcompView)

STAND <- read.csv("raw_data_valent2023_pythium_seedtreatment.csv", na.strings = "na")

#### Linear model #### 
lm <- lmer(ave.stand ~ Treatment_name * days_post_planting + (1|Rep), data = STAND)
car::Anova(lm)

#### Estimate means and perform Tukey comparisons #### 
lsmeans <- emmeans(lm, ~Treatment_name | days_post_planting)
Results_lsmeansEC <- multcomp::cld(lsmeans, alpha = 0.05, reversed = TRUE, Letters = letters)

#### Extract significance letters for plotting #### 
sig.diff.letters <- data.frame(Results_lsmeansEC$emmeans$Treatment_name, 
                               Results_lsmeansEC$emmeans$days_post_planting,
                               str_trim(Results_lsmeansEC$emmeans$.group))

#### Final bar plot with significance letters #### 
ggplot(STAND, aes(x = Treatment_name, y = ave.stand)) + 
  stat_summary(fun = mean, geom = "bar") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.5) +
  geom_text(data = sig.diff.letters, aes(label = Letters), vjust = -0.5) +
  xlab("") + ylab("Number of emerged plants") + 
  theme_classic()
