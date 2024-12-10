
library("miaViz") 
library("mia") 
library("ggplot2")
library("dplyr") 
library("tidyr")

data <- read.csv("/Users/arianabeloiu/Downloads/data_for_research/input/Persian_gulf_Hazraty-Kari_et_al.csv", header = TRUE)


genus_abundance <- data %>% 
  group_by(Genus) %>% 
  summarize( 
    Total_Healthy = sum(healthy_count, na.rm = TRUE), 
    Total_Diseased = sum(total_diseased_count, na.rm = TRUE) 
  ) 

long_data <- genus_abundance %>% 
  pivot_longer(cols = c(Total_Healthy, Total_Diseased),  
               names_to = "Metric", values_to = "Count")


ggplot(long_data, aes(x = Genus, y = Count, fill = Metric)) + 
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Genus-Level Abundance") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  