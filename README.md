
# Black Oak and Red Pine Distribution in the United States

## INTRODUCTION
For my report, I researched what the migration trends of the Black Oak and the Red Pine tree species in the United States. Are there more saplings taking root further north? Are there fewer adult trees in the south? 
By understanding where the saplings and adults are the most dense, we can get a clearer picture of if and/or where the Black Oak and Red Pine tree species are potentially migrating. This could lead to further understanding of how our evolving climate trends will shape our landscape in the near future. 

## DATA & METHODS
To start off, I installed ggplot2, dplyr, sf, rnaturalearth, classint, gridExtra, and eHOF to use as my functions for both my IV and my FIA data collection. After downloading the libraries, I made sure my data from my files could be read. If this stage did not work, I would need to make sure everything had been loaded properly or my codes would not work. 

For my IV (Importance Value) data collection:
I set up my hof plot with the parameters I needed. I added a function to gather the species by common name from the database with a floor to ceiling count by 0.5. This data is found under the new name trees_data and the data under trees_bands functions as a data frame for latitude (x-axis) and average IV (y-axis). Within my predicted response, my new sequence data is counted by 0.1. I set my aesthetic color to black, my geometery points as grey90, and my line type as red and dashed in order to bring the viewer's attention to the peak of the IV on the latitudinal scale. To pull up the graphs, I input "hof_plot("black oak")" and "hof_plot("red pine")" to run the results. I had to trouble shoot this a few times, but used previous lines of code from our classes as an example. Finally, when I ran the code as described above and pictured below, it pulled up the graphs properly. 

For my FIA data collection:
Similarly for my FIA coding, I read my FIA file into the system and started by giving each function a name in consecutive order of operation. I used the FIA database (FIADB) to gather data on my two study species to geographically map where they currently are based on their sapling and adult diameter at different latitudes in order to predict where they are heading. I identify the "babies"--this is how they are labeled in my code--based on a diameter of less than or equal to (<=) 3.5 and adults are defined as greater than or equal to (>=) 3.6 in FIA. 
For both species respectively, I calculate the sum of the pixels that I aggregate by 1% latitudinally and longitudinally to find their range over the U.S. To illustrate the two categories I have set for the trees, I made the red dots on the map the representation for the "babies" and grey10 for the "adults" of both species. At first, I only had one map with the adults of both species coded as one color and the babies as another color. This did not work since it did not differentiate the species' densities and ranges. To make two maps, I ran a ggplot for the black oak and another ggplot for the red pine. With the species separated, I could then see the contrast I needed in both population of "baby" versus "adult" and the ranges and locations of both species separately. Now they were legible. See my code and outputs below. 


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
There were no saplings at the southern limit and the importance value (IV) remains at a constant latitude between approximately 38 degrees and 58 degrees latitude. This could mean that there is contraction at the southern limit of the Red Pine range and there is a steady population of adults in the northern range. This means there is also no increase in density as I was hypothesizing. The results therefore reject my hypothesis.

Black Oak: Adult vs Sapling --
There appear to be more saplings at the southern-most limit and a sharp increase of importance value as latitude increases. There is a much larger population of Black Oak than of Red Pine. The black oak <= 3.5 are sprawled out across the entire region, with the heaviest concentration in the south and midwest United States. A large percent of them also stretch to the southeast coast. This species also rejects my hypothesis, because there is abundant growth in the south as well as growth in the north. 

Future research into this requires studying the time steps of these species in their respective ranges to get a better picture of how their density and spread has evolved through time. Has it changed steadily or at a faster-than-expected rate? Better developed versions of my maps would also do well. Mine only show the geographic placement of the recorded trees. There is need for a better picture of the map of the country as it pertains to elevations and, preferably, hardiness zones to provide a closer look into why they are growing or shrinking in population density in certain areas. 

