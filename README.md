
# Black Oak and Red Pine Distribution in the Eastern United States

## INTRODUCTION
What are the migration trends of the Black Oak and the Red Pine tree species in the Eastern United States? Are there more saplings taking root further north? Are there fewer adult trees in the south? 
By understanding where the saplings and adults are the most dense, we can see a clearer picture of if and/or where the Black Oak and Red Pine tree species are potentially migrating. This could lead to further understanding of how our evolving climate trends will shape our landscape in the near future. 

## DATA & METHODS

I use the FIADB to gather data on my two study species to geographically map where they based on their sapling and adult diameter at different latitudes in order to predict where they are heading. Because the FIADB predominantly covers the United States, I am limiting my research to the range of the tree species in the Eastern United States. For both species respectively, I calculate the sum of the pixels that I aggregate by 1% latitudinally and longitudinally to find their range over the eastern United States. 


### IV Data: Code and Output
![{504AB06F-DE92-48E3-823A-BD6A4108AB1C}](https://github.com/user-attachments/assets/f0644595-5d14-40e6-aee5-1ee28ad47d00)
![4EFA578D-28D9-4B11-8F24-F3E756484135](https://github.com/user-attachments/assets/87db0469-60de-4cd9-84e9-494d2dabece7)

IV: 
library(ggplot2)
library(dplyr)
library(sf)
library(rnaturalearth)
library(classInt)
library(gridExtra)
library(eHOF)

iv <- readRDS ("~/Downloads/IV_data (1).RDS")

hof_plot <- function(species){
  trees_data <- iv %>%
    filter(COMMON_NAME == species)
  lat_seq <- seq(from = floor(min(trees_data$LAT)), to = ceiling(max(trees_data$LAT)), by = 0.5)
  trees_bands <- data.frame(
    Lat_band = lat_seq,
    avg_IV = numeric(length(lat_seq))
  )
  for (i in 1:length(lat_seq)){
    lat_min <- lat_seq[i] - 0.5
    lat_max <- lat_seq[i] + 0.5
    band_data <- trees_data %>%
      filter(LAT >= lat_min & LAT <= lat_max)
    trees_bands$avg_IV[i] <- mean(band_data$IV, na.rm = TRUE)
  }
  trees_bands <- trees_bands[complete.cases(trees_bands$avg_IV),]
  hof_model <- HOF(
    trees_bands$avg_IV,
    trees_bands$Lat_band,
    modeltypes = c("I","II","III","IV","V"),
    family = gaussian,
    bootstrap = 100,
    test = 'AIC'
  )
  best_model <- pick.model(hof_model, modeltypes = c("I","II","III","IV","V"), test = 'AIC')
  predicted_response <- predict(hof_model, model = best_model,
                                newdata = seq(min(trees_bands$Lat_band), max(trees_bands$Lat_band), by = 0.1))
  scaled_response <- predicted_response * max(trees_bands$avg_IV)
  ggplot(trees_bands, aes(x = Lat_band, y = avg_IV)) +
    geom_point(color = "grey60") +
    geom_line(data = data.frame(lat_band = seq(min(trees_bands$Lat_band),
                                               max(trees_bands$Lat_band), by = 0.1),
                                predicted_IV = scaled_response),
              aes(x = lat_band, y = predicted_IV), color = "black") +
    geom_vline(xintercept = Para(hof_model, model = best_model)$opt, color = "red", linetype = "dashed") +
    labs(title = paste("Latitudinal IV pattern", species), x = "Latitude", y = "IV (Importance Value")
}
hof_plot("black oak")
unique(iv$COMMON_NAME)
hof_plot("red pine")

### FIA Coding Process
FIA:
fia <- readRDS("~/Downloads/FIA_tree_master1 (2).RDS")
fia
head(fia_spcd)
first(unique(fia$GRIDID))
mean(fia_spcd$DIA, na.rm = TRUE)
fia_grid <- fia[fia$GRIDID == unique(fia$GRIDID)[1],]
mean(fia_grid$DIA, na.rm = TRUE)
results <- list()
results
tree <- fia_spcd[fia$COMMON_NAME == "black oak"] %>%
  head(10)
target <- fia %>%
  group_by(GRIDID, LAT, LON, COMMON_NAME, DIA, HT, FGROWCFAL, FMORTCFAL, FREMVCFAL) %>%
  filter(COMMON_NAME %in% c("red pine","black oak"))

baby <- target %>%
  filter(DIA <= 3.5)
red_pine_baby <- baby %>%
  filter(COMMON_NAME == "red pine")
black_oak_baby <- baby %>%
  filter(COMMON_NAME == "black oak")

adult <- target %>%
  filter(DIA >= 3.6)
red_pine_adult <- adult %>%
  filter(COMMON_NAME == "red pine")
black_oak_adult <- adult %>%
  filter(COMMON_NAME == "black oak")

ggplot()+
  geom_point(data = red_pine_baby, aes(x = LON, y = LAT), size = 1,color = "red") +
  geom_point(data = red_pine_adult, aes(x = LON, y = LAT), size = 1, color = "grey10")
  
ggplot()+
  geom_point(data = black_oak_baby, aes(x = LON, y = LAT), size = 1, color = "red") +
  geom_point(data = black_oak_adult, aes(x = LON, y = LAT), size = 1, color = "grey10")

## RESULTS & DISCUSSION
Red Pine: Adult vs Sapling 
![EBDCEAC8-5AA1-4055-9E9D-E42A6FF87670](https://github.com/user-attachments/assets/b9a0e6c3-7120-4d29-98f9-2380d440d28d)

Black Oak: Adult vs Sapling
![A21FAB08-9AB5-4EB3-B1F6-9AF5DE1AB7BC](https://github.com/user-attachments/assets/fa3b7830-9d1c-4582-923f-54638efa7ce8)

Red Pine: Adult vs Sapling --
No sapings at southern limit and the importance value remains at a constant latitude. 

Black Oak: Adult vs Sapling --
More saplings at southern limit and sharp increase of importance value as latitude increases.

