library("dplyr")  

data <- read.csv("/Users/arianabeloiu/Downloads/data_for_research/input/Sere_et_al_2015_Isle_de_la_Reunion_Disease_Data.csv")  

data$Endozoicomonas_tissue <- as.numeric(data$Endozoicomonas_tissue) 
data$Endozoicomonas_tissue <- as.numeric(data$Endozoicomonas_tissue) 
data$disease_prevalence <- as.numeric(data$disease_prevalence)  

cleaned_data <- data %>% 
  filter(!is.na(Endozoicomonas_tissue),!is.na(disease_prevalence)) 
summary(cleaned_data) 

correlation <- cor(cleaned_data$Endozoicomonas_tissue,cleaned_data$disease_prevalence, method = "pearson") 
print(correlation) 

library("vegan")

str(cleaned_data$abundance)
cleaned_data$abundance <- as.numeric(cleaned_data$abundance) 
all(is.numeric(cleaned_data$abundance)) 

unique(cleaned_data$abundance[cleaned_data$Region == "Mayotte"]) 
shannon_indices <- cleaned_data %>% 
  group_by(Region) %>%
  summarise(Shannon_Index = diversity(abundance, index = "shannon"))
print(shannon_indices)

str(cleaned_data$Shannon_Index) 

shannon_indices <- cleaned_data %>%
  group_by(Region) %>%
  summarise(Shannon_Index = diversity(abundance, index = "shannon"))  

cleaned_data <- cleaned_data %>% 
  left_join(shannon_indices, by = "Region")
threshold <- median(cleaned_data$Endozoicomonas_tissue, na.rm = TRUE)

cleaned_data <- cleaned_data %>% 
  mutate(group = ifelse(Endozoicomonas_tissue > threshold, "High", "Low")) 

wilcox_test <- wilcox.test(Shannon_Index ~ group, data = cleaned_data) 
print(wilcox_test) 

boxplot(Shannon_Index ~ group, data = cleaned_data, 
        main = "Shannon Index by Endozoicomonas Group", 
        xlab = "Endozoicomonas Abundance Group", 
        ylab = "Shannon Diversity Index") 

hist(cleaned_data$Endozoicomonas_tissue, 
     main = "Histogram of Endozoicomonas Tissue Abundance", 
     xlab = "Endozoicomonas Abundance", 
     col = "skyblue" 
     border = "white") 

plot(cleaned_data$Endozoicomonas_tissue, cleaned_data$disease_prevalence, 
     main = "Scatterplot of Endozoicomonas vs Disease Prevalence", 
     xlab = "Endozoicomonas Tissue Abundance", 
     ylab = "Disease Prevalence", 
     pch = 19, col = "darkgreen") 

abline(lm(disease_prevalence ~ Endozoicomonas_tissue, data = cleaned_data), col = "red") 
plot(cleaned_data$Endozoicomonas_tissue, cleaned_data$disease_prevalence, 
     main = "Scatterplot of Endozoicomonas vs Disease Prevalence", 
     xlab = "Endozoicomonas Tissue Abundance",  
     ylab = "Disease Prevalence", 
     pch = 19, col = "darkgreen")
     
