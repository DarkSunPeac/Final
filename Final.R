library(tidyverse)
library(ggplot2)
library(ggpubr)
library(MASS)
library(bbmle)

CoralData1 <- read.csv("CRCP_Benthic_Cover_Florida_7018_0ee5_9488.csv")

#Summary for the first coral data set latitude
coral_dataset1_summary <- CoralData1 %>%
  mutate(latitude=as.numeric(latitude))%>%
  group_by(PRIMARY_SAMPLE_UNIT,latitude,YEAR,HABITAT_TYPE,Description,PROT) %>%
  summarise(n_species = length(unique(COVER_CAT_NAME)),
            total_pct_cover_hard = sum(as.numeric(HARDBOTTOM_P),na.rm = TRUE))

coral_dataset1_summary <- coral_dataset1_summary %>%
  filter(is.finite(latitude))

coral_dataset1_summary
#Generalized Linear Model for CoralData1 (Poisson)
model_lat_pois_coraldat1 <- glm(
  n_species ~ latitude,
  data = coral_dataset1_summary,
  family = poisson()
)
summary(model_lat_pois_coraldat1)

#Generalized Linear Model for CoralData1 (Negative Binomial)
model_lat_nb_coraldat1 <- glm.nb(
  n_species ~ latitude,
  data = coral_dataset1_summary
)

summary(model_lat_nb_coraldat1)

#AICtab for both GLMs
AICtab(model_lat_pois_coraldat1, model_lat_nb_coraldat1)

#Plot for the first coral data set (Latitude vs Number of Species)
coral_dataset1_summary %>%
  filter(n_species > 0)%>%
  ggplot(mapping = aes(x=latitude,y = n_species))+
  geom_point()+
  geom_smooth(color="purple",method = "lm")+
  labs(x = "Latitude", y = "Number of Species", title = "Richness and Latitude: Summarized")

#Alternate plot for the first coral data set using Negative Binom GLM
#-Prediction Data for GLM Negative Binom Graph:
new_lat <- data.frame(latitude = seq(min(coral_dataset1_summary$latitude),
                                     max(coral_dataset1_summary$latitude),
                                     length.out = 200))

new_lat$pred <- predict(model_lat_nb_coraldat1, newdata = new_lat, type = "response")
#-Negative Binom GLM Graph itself:
ggplot() +
  geom_point(data = coral_dataset1_summary,
             aes(x = latitude, y = n_species),
             alpha = 0.5) +
  geom_line(data = new_lat,
            aes(x = latitude, y = pred),
            color = "blue", size = 1.2) +
  theme_minimal() +
  labs(
    x = "Latitude",
    y = "Species Richness",
    title = "Richness and Latitude: Negative Binom GLM"
  )

#Citations
citation()
citation("tidyverse")
citation("ggplot2")
citation("ggpubr")
citation("MASS")
citation("bbmle")