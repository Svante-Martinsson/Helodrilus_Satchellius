#amazing R. User Svante Martinsson, svantemartinsson.se
#2020-12-11
#Analysing and plotting uncorrected COI p-distances for Helodrilus oculatus and Satchellius mammalis

library(ggplot2)
library(dplyr)
library(readr)

#clear R's brain
rm(list=ls())

#import data
distances <- read_csv("https://raw.githubusercontent.com/Svante-Martinsson/Helodrilus_Satchellius/main/Dist_helodrilus_satchellius.csv")

#data checks
names(distances)
glimpse(distances)

#filter the distances by species and separate them into two new datasets
S_mammalis<-filter(distances, species=="Satchellius mammalis")
H_oculatus<-filter(distances, species=="Helodrilus oculatus")

#arrange the species specific dataset by distance, and then view them
S_mammalis<-arrange(S_mammalis, dist)
View(S_mammalis)

H_oculatus<-arrange(H_oculatus, dist)
View(H_oculatus)


#summarise the species specific datasets, including summary statistics like min and max.
summary(S_mammalis)
summary(H_oculatus)


#Plotting the distances for each species
ggplot(distances, aes(x = species, y = dist, colour = species)) +
    geom_point(size=5, alpha = 1) +
  xlab("Species") +
  ylab("uncorr p-dist") +
  theme_bw() +
  scale_colour_manual(values = c("Helodrilus oculatus" = "purple", "Satchellius mammalis" ="dark green"))

#Create a histogram of uncorrected genetic distance for H. oculatus and S. mammalis. USing facet_wrap
#to get separate hsitograms for each species
ggplot(distances, aes(x = dist)) +
  geom_histogram(aes(colour=species, fill=species),
               binwidth = 0.002, alpha=0.75) +
 facet_wrap(~species, scales="free_y" , ncol = 1) +
   scale_colour_manual(values = c("Helodrilus oculatus" = "purple", "Satchellius mammalis" ="dark green")) +
  scale_fill_manual(values = c("Helodrilus oculatus" = "purple", "Satchellius mammalis" ="dark green")) +
  xlab("uncorr p-dist") +
  ylab("count") +
  theme_bw()+
  theme(axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 8), axis.text.y = element_text(size = 8),
        strip.text = element_text(size = 12, face = "italic"), strip.background = element_rect(colour="white", fill = "white"),
        legend.title = element_blank(), legend.text = element_text(size = 6, face = "italic"),
        legend.position = "none",
        panel.grid = element_blank())
        

#exporting the histogram
ggsave("histogram.png", width = 76, height = 100, units = "mm", dpi = 600)
