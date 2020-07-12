# library
library(readxl)
library(tidyverse)
library(viridis)

# Create dataset
WF97 <- read_excel("G:/Waterfootprint/2. Footprint Final Analysis/10. Data Representation/R/R_area_WF.xlsx", 
                   sheet = "WF97")
Area97 <- read_excel("G:/Waterfootprint/2. Footprint Final Analysis/10. Data Representation/NewMm3/R/R_area_WF.xlsx", 
                     sheet = "Area97")
WF07 <- read_excel("G:/Waterfootprint/2. Footprint Final Analysis/10. Data Representation/NewMm3/R/R_area_WF.xlsx", 
                   sheet = "WF07")
Area07 <- read_excel("G:/Waterfootprint/2. Footprint Final Analysis/10. Data Representation/NewMm3/R/R_area_WF.xlsx", 
                     sheet = "Area07")
WF17 <- read_excel("G:/Waterfootprint/2. Footprint Final Analysis/10. Data Representation/NewMm3/R/R_area_WF.xlsx", 
                   sheet = "WF17")
Area17 <- read_excel("G:/Waterfootprint/2. Footprint Final Analysis/10. Data Representation/NewMm3/R/R_area_WF.xlsx", 
                     sheet = "Area17")


#1997..........................1997

# WF97
# Transform data in a tidy format (long format)
data1 <- WF97 %>% gather(key = "observation", value="Wf", -c(1)) 

# Set a number of 'empty bar' to add at the end of each group
empty_bar <- 12
nObsType <- nlevels(as.factor(data1$observation))
to_add <- data.frame( matrix(NA, empty_bar, ncol(data1)))
colnames(to_add) <- colnames(data1)
data1 <- rbind(data1, to_add)
data1 <- data1 %>% arrange(District)
data1$id <- rep( seq(1, nrow(data1)/nObsType) , each=nObsType)

# Get the name and the y position of each label
label_data1 <- data1 %>% group_by(id, District) %>% summarize(tot=sum(Wf))
number_of_bar <- nrow(label_data1)
angle <- 90 - 360 * (label_data1$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data1$hjust <- ifelse( angle < -90, 1, 0)
label_data1$angle <- ifelse(angle < -90, angle+180, angle)

# Make the plot
WF1 <- ggplot(data1, aes(x=as.factor(id), y=Wf, fill=observation)) +      
  
  # Add the stacked bar
  geom_bar(stat="identity", width=0.5) + scale_fill_manual(values = c("gray","blue","green")) +
  
  # Add text showing the value of each 100/75/50/25 lines
  ggplot2::annotate("text", x = rep(max(data1$id), 2), y = c(170000000, 2500000000), label = c("170 Mm^6", "2500 Mm^6") , color="grey", size=2 , angle=0, fontface="bold", hjust=1) +
  
  ylim(-3000000000,max(label_data1$tot, na.rm=T)) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar() +
  
  # Add labels on top of each bar
  geom_text(data=label_data1, aes(x=id, y=tot+15, label=District, hjust=hjust), color="black", fontface="bold",alpha=1, size= 2, angle= label_data1$angle, inherit.aes = FALSE )
WF1




# Area97
# Transform data in a tidy format (long format)
data2 <- Area97 %>% gather(key = "observation", value="Wf", -c(1)) 

# Set a number of 'empty bar' to add at the end of each group
empty_bar <- 12
nObsType <- nlevels(as.factor(data2$observation))
to_add <- data.frame( matrix(NA, empty_bar, ncol(data2)))
colnames(to_add) <- colnames(data2)
data2 <- rbind(data2, to_add)
data2 <- data2 %>% arrange(District)
data2$id <- rep( seq(1, nrow(data2)/nObsType) , each=nObsType)

# Get the name and the y position of each label
label_data2 <- data2 %>% group_by(id, District) %>% summarize(tot=sum(Wf))
number_of_bar <- nrow(label_data2)
angle <- 90 - 360 * (label_data2$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data2$hjust <- ifelse( angle < -90, 1, 0)
label_data2$angle <- ifelse(angle < -90, angle+180, angle)

# Make the plot
Area1 <- ggplot(data2, aes(x=as.factor(id), y=Wf, fill=observation)) +      
  
  # Add the stacked bar
  geom_bar(stat="identity", width=0.5) + scale_fill_manual(values = c("lightpink","bisque","orange")) +
  
  # Add text showing the value of each 100/75/50/25 lines
  ggplot2::annotate("text", x = rep(max(data1$id), 2), y = c(15000, 200000), label = c("15000 ha", "200000 ha") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
  
  ylim(-300000,max(label_data2$tot, na.rm=T)) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar() +
  # Add labels on top of each bar
  geom_text(data=label_data2, aes(x=id, y=tot+30, label=District, hjust=hjust), color="gray", fontface="bold",alpha=0, size=.1, angle= label_data2$angle, inherit.aes = FALSE )  
 
Area1











#2007.................................2007


# WF07
# Transform data in a tidy format (long format)
data3 <- WF07 %>% gather(key = "observation", value="Wf", -c(1)) 

# Set a number of 'empty bar' to add at the end of each group
empty_bar <- 12
nObsType <- nlevels(as.factor(data3$observation))
to_add <- data.frame( matrix(NA, empty_bar, ncol(data3)))
colnames(to_add) <- colnames(data3)
data3 <- rbind(data3, to_add)
data3 <- data3 %>% arrange(District)
data3$id <- rep( seq(1, nrow(data3)/nObsType) , each=nObsType)

# Get the name and the y position of each label
label_data3 <- data3 %>% group_by(id, District) %>% summarize(tot=sum(Wf))
number_of_bar <- nrow(label_data3)
angle <- 90 - 360 * (label_data3$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data3$hjust <- ifelse( angle < -90, 1, 0)
label_data3$angle <- ifelse(angle < -90, angle+180, angle)
view(label_data3)

# Make the plot
WF2 <- ggplot(data3, aes(x=as.factor(id), y=Wf, fill=observation)) +      
  
  # Add the stacked bar
  geom_bar(stat="identity", width=0.5) + scale_fill_manual(values = c("gray","blue","green")) +
  
  # Add text showing the value of each 100/75/50/25 lines
  ggplot2::annotate("text", x = rep(max(data3$id), 2), y = c(200000000, 5000000000), label = c("200 Mm^6", "5000 Mm^6") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
  
  ylim(-4000000000,max(label_data3$tot, na.rm=T)) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar() +
  
  # Add labels on top of each bar
  geom_text(data=label_data3, aes(x=id, y=tot+15, label=District, hjust=hjust), color="black", fontface="bold",alpha=1, size=3, angle= label_data3$angle, inherit.aes = FALSE )
WF2




# Area07
# Transform data in a tidy format (long format)
data4 <- Area07 %>% gather(key = "observation", value="Wf", -c(1)) 

# Set a number of 'empty bar' to add at the end of each group
empty_bar <- 12
nObsType <- nlevels(as.factor(data4$observation))
to_add <- data.frame( matrix(NA, empty_bar, ncol(data4)))
colnames(to_add) <- colnames(data4)
data4 <- rbind(data4, to_add)
data4 <- data4 %>% arrange(District)
data4$id <- rep( seq(1, nrow(data4)/nObsType) , each=nObsType)

# Get the name and the y position of each label
label_data4 <- data4 %>% group_by(id, District) %>% summarize(tot=sum(Wf))
number_of_bar <- nrow(label_data4)
angle <- 90 - 360 * (label_data4$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data4$hjust <- ifelse( angle < -90, 1, 0)
label_data4$angle <- ifelse(angle < -90, angle+180, angle)

# Make the plot
Area2 <- ggplot(data4, aes(x=as.factor(id), y=Wf, fill=observation)) +      
  
  # Add the stacked bar
  geom_bar(stat="identity", width=0.5) + scale_fill_manual(values = c("lightpink","bisque","orange")) +
  
  # Add text showing the value of each 100/75/50/25 lines
  ggplot2::annotate("text", x = rep(max(data4$id), 2), y = c(20000, 500000), label = c("20000 ha", "500000 ha") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
  
  ylim(-500000,max(label_data4$tot, na.rm=T)) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar() +
  
  # Add labels on top of each bar
  geom_text(data=label_data4, aes(x=id, y=tot+15, label=District, hjust=hjust), color="black", fontface="bold",alpha=1, size=3, angle= label_data4$angle, inherit.aes = FALSE )
Area2







#2017....................2017


# WF17
# Transform data in a tidy format (long format)
data5 <- WF17 %>% gather(key = "observation", value="Wf", -c(1)) 

# Set a number of 'empty bar' to add at the end of each group
empty_bar <- 12
nObsType <- nlevels(as.factor(data5$observation))
to_add <- data.frame( matrix(NA, empty_bar, ncol(data5)))
colnames(to_add) <- colnames(data5)
data5 <- rbind(data5, to_add)
data5 <- data5 %>% arrange(District)
data5$id <- rep( seq(1, nrow(data5)/nObsType) , each=nObsType)

# Get the name and the y position of each label
label_data5 <- data5 %>% group_by(id, District) %>% summarize(tot=sum(Wf))
number_of_bar <- nrow(label_data5)
angle <- 90 - 360 * (label_data5$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data5$hjust <- ifelse( angle < -90, 1, 0)
label_data5$angle <- ifelse(angle < -90, angle+180, angle)
view(label_data5)

# Make the plot
WF3 <- ggplot(data5, aes(x=as.factor(id), y=Wf, fill=observation)) +      
  
  # Add the stacked bar
  geom_bar(stat="identity", width=0.5) + scale_fill_manual(values = c("gray","blue","green")) +
  
  # Add text showing the value of each 100/75/50/25 lines
  ggplot2::annotate("text", x = rep(max(data5$id), 2), y = c(240000000, 5000000000), label = c("240 Mm^6", "5000 Mm^6") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
  
  ylim(-4000000000,max(label_data5$tot, na.rm=T)) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar() +
  
  # Add labels on top of each bar
  geom_text(data=label_data5, aes(x=id, y=tot+15, label=District, hjust=hjust), color="black", fontface="bold",alpha=1, size=3, angle= label_data5$angle, inherit.aes = FALSE )
WF3






# Area17
# Transform data in a tidy format (long format)
data6 <- Area17 %>% gather(key = "observation", value="Wf", -c(1)) 

# Set a number of 'empty bar' to add at the end of each group
empty_bar <- 12
nObsType <- nlevels(as.factor(data6$observation))
to_add <- data.frame( matrix(NA, empty_bar, ncol(data6)))
colnames(to_add) <- colnames(data6)
data6 <- rbind(data6, to_add)
data6 <- data6 %>% arrange(District)
data6$id <- rep( seq(1, nrow(data6)/nObsType) , each=nObsType)

# Get the name and the y position of each label
label_data6 <- data6 %>% group_by(id, District) %>% summarize(tot=sum(Wf))
number_of_bar <- nrow(label_data6)
angle <- 90 - 360 * (label_data6$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data6$hjust <- ifelse( angle < -90, 1, 0)
label_data6$angle <- ifelse(angle < -90, angle+180, angle)

# Make the plot
Area3 <- ggplot(data6, aes(x=as.factor(id), y=Wf, fill=observation)) +      
  
  # Add the stacked bar
  geom_bar(stat="identity", width=0.5) + scale_fill_manual(values = c("lightpink","bisque","orange")) +
  
  # Add text showing the value of each 100/75/50/25 lines
  ggplot2::annotate("text", x = rep(max(data6$id), 2), y = c(20000, 500000), label = c("20000 ha", "500000 ha") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
  
  ylim(-500000,max(label_data6$tot, na.rm=T)) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar() +
  
  # Add labels on top of each bar
  geom_text(data=label_data6, aes(x=id, y=tot+15, label=District, hjust=hjust), color="black", fontface="bold",alpha=1, size=3, angle= label_data6$angle, inherit.aes = FALSE )
Area3
