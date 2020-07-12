# library
library(readxl)
library(tidyverse)
library(viridis)

# Create dataset
Aerial_ID1 <- read_excel("G:/Waterfootprint/2. Footprint Final Analysis/10. Data Representation/R/WF.Rexcel2.xlsx", 
                         sheet = "Sheet1")
Aerial_ID2 <- read_excel("G:/Waterfootprint/2. Footprint Final Analysis/10. Data Representation/R/WF.Rexcel2.xlsx", 
                         sheet = "Sheet2")
Aerial_ID3 <- read_excel("G:/Waterfootprint/2. Footprint Final Analysis/10. Data Representation/R/WF.Rexcel2.xlsx", 
                         sheet = "Sheet3")
Aus97 <- read_excel("G:/Waterfootprint/2. Footprint Final Analysis/10. Data Representation/R/WF.Rexcel.xlsx", 
                    sheet = "Aus97")
Aus07 <- read_excel("G:/Waterfootprint/2. Footprint Final Analysis/10. Data Representation/R/WF.Rexcel.xlsx", 
                    sheet = "Aus07")
Aus17 <- read_excel("G:/Waterfootprint/2. Footprint Final Analysis/10. Data Representation/R/WF.Rexcel.xlsx", 
                    sheet = "Aus17")
Aman97 <- read_excel("G:/Waterfootprint/2. Footprint Final Analysis/10. Data Representation/R/WF.Rexcel.xlsx", 
                    sheet = "Aman97")
Aman07 <- read_excel("G:/Waterfootprint/2. Footprint Final Analysis/10. Data Representation/R/WF.Rexcel.xlsx", 
                    sheet = "Aman07")
Aman17 <- read_excel("G:/Waterfootprint/2. Footprint Final Analysis/10. Data Representation/R/WF.Rexcel.xlsx", 
                    sheet = "Aman17")
Boro97 <- read_excel("G:/Waterfootprint/2. Footprint Final Analysis/10. Data Representation/R/WF.Rexcel.xlsx", 
                     sheet = "Boro97")
Boro07 <- read_excel("G:/Waterfootprint/2. Footprint Final Analysis/10. Data Representation/R/WF.Rexcel.xlsx", 
                     sheet = "Boro07")
Boro17 <- read_excel("G:/Waterfootprint/2. Footprint Final Analysis/10. Data Representation/R/WF.Rexcel.xlsx", 
                     sheet = "Boro17")
Wheat07 <- read_excel("G:/Waterfootprint/2. Footprint Final Analysis/10. Data Representation/R/WF.Rexcel.xlsx", 
                     sheet = "Wheat07")
Wheat17 <- read_excel("G:/Waterfootprint/2. Footprint Final Analysis/10. Data Representation/R/WF.Rexcel.xlsx", 
                     sheet = "Wheat17")



# Aus97
# Transform data in a tidy format (long format)
data1 <- Aus97 %>% gather(key = "observation", value="Wf", -c(1)) 

# Set a number of 'empty bar' to add at the end of each group
empty_bar <- 12
nObsType <- nlevels(as.factor(data1$observation))
to_add <- data.frame( matrix(NA, empty_bar, ncol(data1)))
colnames(to_add) <- colnames(data1)
data1 <- rbind(data1, to_add)
view(data1)
data1 <- cbind(data1,Aerial_ID1)
view(data1)
ranks<- order(data1$Aerial_ID1)
data1 <- data1[ranks,]
view(data1)
data1$id <- rep( seq(1, nrow(data1)/nObsType) , each=nObsType)
view(data1)

# Get the name and the y position of each label
label_data1 <- data1 %>% group_by(id, District) %>% summarize(tot=sum(Wf))
view(label_data1)
number_of_bar <- nrow(label_data1)
angle <- 90 - 360 * (label_data1$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data1$hjust <- ifelse( angle < -90, 1, 0)
label_data1$angle <- ifelse(angle < -90, angle+180, angle)
view(label_data1)

# Make the plot
Aus1 <- ggplot(data1, aes(x=as.factor(id), y=Wf, fill=observation)) +      
  
  # Add the stacked bar
  geom_bar(stat="identity", width=0.5) + scale_fill_manual(values = c("gray","blue","green")) +
  
  # Add text showing the value of each 100/75/50/25 lines
  ggplot2::annotate("text", x = rep(max(data1$id), 7), y = c(0, 3000, 6000, 9000, 12000, 15000, 18000), label = c("0", "3000", "6000", "9000", "12000", "15000", "18000") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
  
  ylim(-150,max(label_data1$tot, na.rm=T)) +
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
  geom_text(data=label_data1, aes(x=id, y=tot+15, label=District, hjust=hjust), color="black", fontface="bold",alpha=1, size=3, angle= label_data1$angle, inherit.aes = FALSE )
Aus1







# Aus07
# Transform data in a tidy format (long format)
data2 <- Aus07 %>% gather(key = "observation", value="Wf", -c(1)) 

# Set a number of 'empty bar' to add at the end of each group
empty_bar <- 12
nObsType <- nlevels(as.factor(data2$observation))
to_add <- data.frame( matrix(NA, empty_bar, ncol(data2)))
colnames(to_add) <- colnames(data2)
data2 <- rbind(data2, to_add)
view(data2)
data2 <- cbind(data2,Aerial_ID2)
view(data2)
ranks2<- order(data2$Aerial_ID2)
data2 <- data2[ranks2,]
view(data2)
data2$id <- rep( seq(1, nrow(data2)/nObsType) , each=nObsType)

# Get the name and the y position of each label
label_data2 <- data2 %>% group_by(id, District) %>% summarize(tot=sum(Wf))
number_of_bar <- nrow(label_data2)
angle <- 90 - 360 * (label_data2$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data2$hjust <- ifelse( angle < -90, 1, 0)
label_data2$angle <- ifelse(angle < -90, angle+180, angle)
view(label_data2)

# Make the plot
Aus2 <- ggplot(data2, aes(x=as.factor(id), y=Wf, fill=observation)) +      
  
  # Add the stacked bar
  geom_bar(stat="identity", width=0.5) + scale_fill_manual(values = c("gray","blue","green")) +
  
  # Add text showing the value of each 100/75/50/25 lines
  ggplot2::annotate("text", x = rep(max(data2$id), 5), y = c(0, 3000, 6000, 9000, 12000), label = c("0", "3000", "6000", "9000", "12000") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
  
  ylim(-150,max(label_data2$tot, na.rm=T)) +
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
  geom_text(data=label_data2, aes(x=id, y=tot+30, label=District, hjust=hjust), color="black", fontface="bold",alpha=1, size=2.6, angle= label_data2$angle, inherit.aes = FALSE )
Aus2






# Aus17
# Transform data in a tidy format (long format)
data3 <- Aus17 %>% gather(key = "observation", value="Wf", -c(1)) 

# Set a number of 'empty bar' to add at the end of each group
empty_bar <- 12
nObsType <- nlevels(as.factor(data3$observation))
to_add <- data.frame( matrix(NA, empty_bar, ncol(data3)))
colnames(to_add) <- colnames(data3)
data3 <- rbind(data3, to_add)
view(data3)
data3 <- cbind(data3,Aerial_ID3)
view(data3)
ranks3<- order(data3$Aerial_ID3)
data3 <- data3[ranks3,]
view(data3)
data3$id <- rep( seq(1, nrow(data3)/nObsType) , each=nObsType)

# Get the name and the y position of each label
label_data3 <- data3 %>% group_by(id, District) %>% summarize(tot=sum(Wf))
number_of_bar <- nrow(label_data3)
angle <- 90 - 360 * (label_data3$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data3$hjust <- ifelse( angle < -90, 1, 0)
label_data3$angle <- ifelse(angle < -90, angle+180, angle)
view(label_data3)

# Make the plot
Aus3 <- ggplot(data3, aes(x=as.factor(id), y=Wf, fill=observation)) +      
  
  # Add the stacked bar
  geom_bar(stat="identity", width=0.5) + scale_fill_manual(values = c("gray","blue","green")) +
  
  # Add text showing the value of each 100/75/50/25 lines
  ggplot2::annotate("text", x = rep(max(data3$id), 4), y = c(0, 3000, 5000, 7000), label = c("0", "3000", "5000", "7000") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
  
  ylim(-150,max(label_data3$tot, na.rm=T)) +
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
Aus3












# Aman97
# Transform data in a tidy format (long format)
data4 <- Aman97 %>% gather(key = "observation", value="Wf", -c(1)) 

# Set a number of 'empty bar' to add at the end of each group
empty_bar <- 12
nObsType <- nlevels(as.factor(data4$observation))
to_add <- data.frame( matrix(NA, empty_bar, ncol(data4)))
colnames(to_add) <- colnames(data4)
data4 <- rbind(data4, to_add)
view(data4)
data4 <- cbind(data4,Aerial_ID3)
view(data4)
ranks4<- order(data4$Aerial_ID3)
data4 <- data4[ranks4,]
view(data4)
data4$id <- rep( seq(1, nrow(data4)/nObsType) , each=nObsType)

# Get the name and the y position of each label
label_data4 <- data4 %>% group_by(id, District) %>% summarize(tot=sum(Wf))
number_of_bar <- nrow(label_data4)
angle <- 90 - 360 * (label_data4$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data4$hjust <- ifelse( angle < -90, 1, 0)
label_data4$angle <- ifelse(angle < -90, angle+180, angle)
view(label_data4)

# Make the plot
Aman1 <- ggplot(data4, aes(x=as.factor(id), y=Wf, fill=observation)) +      
  
  # Add the stacked bar
  geom_bar(stat="identity", width=0.5) + scale_fill_manual(values = c("gray","blue","green")) +
  
  # Add text showing the value of each 100/75/50/25 lines
  ggplot2::annotate("text", x = rep(max(data4$id), 5), y = c(0, 3000, 6000, 9000, 12000), label = c("0", "3000", "6000", "9000", "12000") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
  
  ylim(-150,max(label_data4$tot, na.rm=T)) +
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
Aman1








# Aman07
# Transform data in a tidy format (long format)
data5 <- Aman07 %>% gather(key = "observation", value="Wf", -c(1)) 

# Set a number of 'empty bar' to add at the end of each group
empty_bar <- 12
nObsType <- nlevels(as.factor(data5$observation))
to_add <- data.frame( matrix(NA, empty_bar, ncol(data5)))
colnames(to_add) <- colnames(data5)
data5 <- rbind(data5, to_add)
view(data5)
data5 <- cbind(data5,Aerial_ID3)
view(data5)
ranks5<- order(data5$Aerial_ID3)
data5 <- data5[ranks5,]
view(data5)
data5$id <- rep( seq(1, nrow(data5)/nObsType) , each=nObsType)

# Get the name and the y position of each label
label_data5 <- data5 %>% group_by(id, District) %>% summarize(tot=sum(Wf))
number_of_bar <- nrow(label_data5)
angle <- 90 - 360 * (label_data5$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data5$hjust <- ifelse( angle < -90, 1, 0)
label_data5$angle <- ifelse(angle < -90, angle+180, angle)
view(label_data5)

# Make the plot
Aman2 <- ggplot(data5, aes(x=as.factor(id), y=Wf, fill=observation)) +      
  
  # Add the stacked bar
  geom_bar(stat="identity", width=0.5) + scale_fill_manual(values = c("gray","blue","green")) +
  
  # Add text showing the value of each 100/75/50/25 lines
  ggplot2::annotate("text", x = rep(max(data5$id), 4), y = c(0, 3000, 6000, 9000), label = c("0", "3000", "6000", "9000") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
  
  ylim(-150,max(label_data5$tot, na.rm=T)) +
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
Aman2






# Aman17
# Transform data in a tidy format (long format)
data6 <- Aman17 %>% gather(key = "observation", value="Wf", -c(1)) 

# Set a number of 'empty bar' to add at the end of each group
empty_bar <- 12
nObsType <- nlevels(as.factor(data6$observation))
to_add <- data.frame( matrix(NA, empty_bar, ncol(data6)))
colnames(to_add) <- colnames(data6)
data6 <- rbind(data6, to_add)
view(data6)
data6 <- cbind(data6,Aerial_ID3)
view(data6)
ranks6<- order(data6$Aerial_ID3)
data6 <- data6[ranks6,]
view(data6)
data6$id <- rep( seq(1, nrow(data6)/nObsType) , each=nObsType)

# Get the name and the y position of each label
label_data6 <- data6 %>% group_by(id, District) %>% summarize(tot=sum(Wf))
number_of_bar <- nrow(label_data6)
angle <- 90 - 360 * (label_data6$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data6$hjust <- ifelse( angle < -90, 1, 0)
label_data6$angle <- ifelse(angle < -90, angle+180, angle)
view(label_data6)

# Make the plot
Aman3 <- ggplot(data6, aes(x=as.factor(id), y=Wf, fill=observation)) +      
  
  # Add the stacked bar
  geom_bar(stat="identity", width=0.5) + scale_fill_manual(values = c("gray","blue","green")) +
  
  # Add text showing the value of each 100/75/50/25 lines
  ggplot2::annotate("text", x = rep(max(data6$id), 4), y = c(0, 2000, 4000, 6000), label = c("0", "2000", "4000", "6000") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
  
  ylim(-150,max(label_data6$tot, na.rm=T)) +
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
Aman3






# Boro97
# Transform data in a tidy format (long format)
data7 <- Boro97 %>% gather(key = "observation", value="Wf", -c(1)) 

# Set a number of 'empty bar' to add at the end of each group
empty_bar <- 12
nObsType <- nlevels(as.factor(data7$observation))
to_add <- data.frame( matrix(NA, empty_bar, ncol(data7)))
colnames(to_add) <- colnames(data7)
data7 <- rbind(data7, to_add)
view(data7)
data7 <- cbind(data7,Aerial_ID3)
view(data7)
ranks7<- order(data7$Aerial_ID3)
data7 <- data7[ranks7,]
view(data7)
data7$id <- rep( seq(1, nrow(data7)/nObsType) , each=nObsType)

# Get the name and the y position of each label
label_data7 <- data7 %>% group_by(id, District) %>% summarize(tot=sum(Wf))
number_of_bar <- nrow(label_data7)
angle <- 90 - 360 * (label_data7$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data7$hjust <- ifelse( angle < -90, 1, 0)
label_data7$angle <- ifelse(angle < -90, angle+180, angle)
view(label_data7)

# Make the plot
Boro1 <- ggplot(data7, aes(x=as.factor(id), y=Wf, fill=observation)) +      
  
  # Add the stacked bar
  geom_bar(stat="identity", width=0.5) + scale_fill_manual(values = c("gray","blue","green")) +
  
  # Add text showing the value of each 100/75/50/25 lines
  ggplot2::annotate("text", x = rep(max(data7$id), 4), y = c(0, 2000, 4000, 6000), label = c("0", "2000", "4000", "6000") , color="grey", size=4 , angle=0, fontface="bold", hjust=1) +
  
  ylim(-150,max(label_data7$tot, na.rm=T)) +
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
  geom_text(data=label_data7, aes(x=id, y=tot+5, label=District, hjust=hjust), color="black", fontface="bold",alpha=1, size=3.8, angle= label_data7$angle, inherit.aes = FALSE )
Boro1






# Boro07
# Transform data in a tidy format (long format)
data8 <- Boro07 %>% gather(key = "observation", value="Wf", -c(1)) 

# Set a number of 'empty bar' to add at the end of each group
empty_bar <- 12
nObsType <- nlevels(as.factor(data8$observation))
to_add <- data.frame( matrix(NA, empty_bar, ncol(data8)))
colnames(to_add) <- colnames(data8)
data8 <- rbind(data8, to_add)
view(data8)
data8 <- cbind(data8,Aerial_ID3)
view(data8)
ranks8<- order(data8$Aerial_ID3)
data8 <- data8[ranks8,]
view(data8)
data8$id <- rep( seq(1, nrow(data8)/nObsType) , each=nObsType)

# Get the name and the y position of each label
label_data8 <- data8 %>% group_by(id, District) %>% summarize(tot=sum(Wf))
number_of_bar <- nrow(label_data8)
angle <- 90 - 360 * (label_data8$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data8$hjust <- ifelse( angle < -90, 1, 0)
label_data8$angle <- ifelse(angle < -90, angle+180, angle)
view(label_data8)

# Make the plot
Boro2 <- ggplot(data8, aes(x=as.factor(id), y=Wf, fill=observation)) +      
  
  # Add the stacked bar
  geom_bar(stat="identity", width=0.5) + scale_fill_manual(values = c("gray","blue","green")) +
  
  # Add text showing the value of each 100/75/50/25 lines
  ggplot2::annotate("text", x = rep(max(data8$id), 4), y = c(0, 2000, 4000, 6000), label = c("0", "2000", "4000", "6000") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
  
  ylim(-150,max(label_data8$tot, na.rm=T)) +
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
  geom_text(data=label_data8, aes(x=id, y=tot+15, label=District, hjust=hjust), color="black", fontface="bold",alpha=1, size=3.5, angle= label_data8$angle, inherit.aes = FALSE )
Boro2






# Boro17
# Transform data in a tidy format (long format)
data9 <- Boro17 %>% gather(key = "observation", value="Wf", -c(1)) 

# Set a number of 'empty bar' to add at the end of each group
empty_bar <- 12
nObsType <- nlevels(as.factor(data9$observation))
to_add <- data.frame( matrix(NA, empty_bar, ncol(data9)))
colnames(to_add) <- colnames(data9)
data9 <- rbind(data9, to_add)
view(data9)
data9 <- cbind(data9,Aerial_ID3)
view(data9)
ranks9<- order(data9$Aerial_ID3)
data9 <- data9[ranks9,]
view(data9)
data9$id <- rep( seq(1, nrow(data9)/nObsType) , each=nObsType)

# Get the name and the y position of each label
label_data9 <- data9 %>% group_by(id, District) %>% summarize(tot=sum(Wf))
number_of_bar <- nrow(label_data9)
angle <- 90 - 360 * (label_data9$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data9$hjust <- ifelse( angle < -90, 1, 0)
label_data9$angle <- ifelse(angle < -90, angle+180, angle)
view(label_data9)

# Make the plot
Boro3 <- ggplot(data9, aes(x=as.factor(id), y=Wf, fill=observation)) +      
  
  # Add the stacked bar
  geom_bar(stat="identity", width=0.5) + scale_fill_manual(values = c("gray","blue","green")) +
  
  # Add text showing the value of each 100/75/50/25 lines
  ggplot2::annotate("text", x = rep(max(data9$id), 4), y = c(0, 2000, 4000, 6000), label = c("0", "2000", "4000", "6000") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
  
  ylim(-150,max(label_data9$tot, na.rm=T)) +
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
  geom_text(data=label_data9, aes(x=id, y=tot+15, label=District, hjust=hjust), color="black", fontface="bold",alpha=1, size=3.5, angle= label_data9$angle, inherit.aes = FALSE )
Boro3






# Wheat07
# Transform data in a tidy format (long format)
data10 <- Wheat07 %>% gather(key = "observation", value="Wf", -c(1)) 

# Set a number of 'empty bar' to add at the end of each group
empty_bar <- 12
nObsType <- nlevels(as.factor(data10$observation))
to_add <- data.frame( matrix(NA, empty_bar, ncol(data10)))
colnames(to_add) <- colnames(data10)
data10 <- rbind(data10, to_add)
data10 <- data10 %>% arrange(District)
data10$id <- rep( seq(1, nrow(data10)/nObsType) , each=nObsType)

# Get the name and the y position of each label
label_data10 <- data10 %>% group_by(id, District) %>% summarize(tot=sum(Wf))
number_of_bar <- nrow(label_data10)
angle <- 90 - 360 * (label_data10$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data10$hjust <- ifelse( angle < -90, 1, 0)
label_data10$angle <- ifelse(angle < -90, angle+180, angle)
view(label_data10)

# Make the plot
Wheat1 <- ggplot(data10, aes(x=as.factor(id), y=Wf, fill=observation)) +      
  
  # Add the stacked bar
  geom_bar(stat="identity", width=0.5) + scale_fill_manual(values = c("gray","blue","green")) +
  
  # Add text showing the value of each 100/75/50/25 lines
  ggplot2::annotate("text", x = rep(max(data10$id), 5), y = c(0, 2000, 4000, 6000, 8000), label = c("0", "2000", "4000", "6000", "8000") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
  
  ylim(-150,max(label_data10$tot, na.rm=T)) +
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
  geom_text(data=label_data10, aes(x=id, y=tot+15, label=District, hjust=hjust), color="black", fontface="bold",alpha=1, size=3.5, angle= label_data10$angle, inherit.aes = FALSE )
Wheat1






# Wheat17
# Transform data in a tidy format (long format)
data11 <- Wheat17 %>% gather(key = "observation", value="Wf", -c(1)) 

# Set a number of 'empty bar' to add at the end of each group
empty_bar <- 12
nObsType <- nlevels(as.factor(data11$observation))
to_add <- data.frame( matrix(NA, empty_bar, ncol(data11)))
colnames(to_add) <- colnames(data11)
data11 <- rbind(data11, to_add)
data11 <- data11 %>% arrange(District)
data11$id <- rep( seq(1, nrow(data11)/nObsType) , each=nObsType)

# Get the name and the y position of each label
label_data11 <- data11 %>% group_by(id, District) %>% summarize(tot=sum(Wf))
number_of_bar <- nrow(label_data11)
angle <- 90 - 360 * (label_data11$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data11$hjust <- ifelse( angle < -90, 1, 0)
label_data11$angle <- ifelse(angle < -90, angle+180, angle)
view(label_data11)

# Make the plot
Wheat2 <- ggplot(data11, aes(x=as.factor(id), y=Wf, fill=observation)) +      
  
  # Add the stacked bar
  geom_bar(stat="identity", width=0.5) + scale_fill_manual(values = c("gray","blue","green")) +
  
  # Add text showing the value of each 100/75/50/25 lines
  ggplot2::annotate("text", x = rep(max(data11$id), 4), y = c(0, 2000, 4000, 6000), label = c("0", "2000", "4000", "6000") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
  
  ylim(-150,max(label_data11$tot, na.rm=T)) +
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
  geom_text(data=label_data11, aes(x=id, y=tot+15, label=District, hjust=hjust), color="black", fontface="bold",alpha=1, size=3.5, angle= label_data11$angle, inherit.aes = FALSE )
Wheat2

