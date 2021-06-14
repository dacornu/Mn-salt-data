library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(magrittr)
library(tibble)

#Le nom du/des fichier(s) à importer doit être inscrit ci-dessous :
usefilex1 <- "XRD-R=1,5-dried.ASC"
usefilex2 <- "XRD_R=1,5_wet.csv"
usefilex3 <- "XRD_R=2_wet.ASC"

#Le nom du/des fichier(s) JCPDS à importer doit être inscrit ci-dessous :
useJCPDS1 <- "XRD-MnOH2SO4.txt"
useJCPDS2 <- "XRD-pyrochroite.txt"

#Lecture
datadrx_x1 <- read_delim(usefilex1, delim="      ", col_names = c("twotheta", "count"))

datadrx_x1$twotheta <- as.numeric(datadrx_x1$twotheta)
datadrx_x1$count <- as.numeric(datadrx_x1$count)

datadrx_x2 <- read_delim(usefilex2, delim="      ", col_names = c("twotheta", "count"))

datadrx_x2$twotheta <- as.numeric(datadrx_x2$twotheta)
datadrx_x2$count <- as.numeric(datadrx_x2$count)


datadrx_x3 <- read_delim(usefilex3, delim="      ", col_names = c("twotheta", "count"))

datadrx_x3$twotheta <- as.numeric(datadrx_x3$twotheta)
datadrx_x3$count <- as.numeric(datadrx_x3$count)

datadrx_x1 <- as.data.frame(datadrx_x1)

datadrx_x1 <- add_column(datadrx_x1, "1,5 dried")

datadrx_x1$count <- datadrx_x1$count*0.4

colnames(datadrx_x1) <- c("twotheta", "count", "R")

datadrx_x2 <- as.data.frame(datadrx_x2)

datadrx_x2 <- add_column(datadrx_x2, "1,5 wet")

datadrx_x2$count <- datadrx_x2$count*0.35+000

colnames(datadrx_x2) <- c("twotheta", "count", "R")

datadrx_x3 <- as.data.frame(datadrx_x3)

datadrx_x3$count <- datadrx_x3$count*0.6+1400

datadrx_x3 <- add_column(datadrx_x3, "2 wet")

colnames(datadrx_x3) <- c("twotheta", "count", "R")


#Lecture fichiers JCPDS
database1 <- read_delim(useJCPDS1, delim="      ", col_names = c("A", "B", "C", "D", "E", "twotheta", "height"))

database1$height <- as.numeric(database1$height)

database1 <- subset(database1, height >= 15)

database1$height <- database1$height*1.5+200
database1$twotheta <- as.numeric(database1$twotheta)

database2 <- read_delim(useJCPDS2, delim="      ", col_names = c("A", "B", "C", "D", "E", "twotheta", "height"))

database2$height <- as.numeric(database2$height)*5+300
database2$twotheta <- as.numeric(database2$twotheta)

#The graph for the XRD files: 

usefilex1
usefilex2
usefilex3

#compared with the JCPDS file
useJCPDS1
useJCPDS2

#Graph1
datadrx <- bind_rows(datadrx_x2, datadrx_x3)

ggplot(data = datadrx, aes(x = twotheta, y = count)) +
  geom_line(aes(group = R, color =R), size=0.5) + 
  ylab("Counts (arbitrary unit)") + xlab( expression(paste(plain('2'),plain(theta), plain(' ('),plain(""^{o}),plain(")")))) + 
  xlim(5, 65) + ylim(0,2500) + 
  theme_minimal() + 
  scale_color_manual(values=c("#800000","#000000")) +
  theme(legend.position='none', text = element_text(size = 24), panel.grid = element_line(color="lightgray")) + 
  geom_text(data = database2, label = "*", aes(x=twotheta, y=1400+height), size = 8)+ 
  geom_text(aes(x=60, y=550), label = "R = 1.5", size = 8, color = "#800000")+ 
  geom_text(aes(x=60, y=2250), label = "R = 2", size = 8, color = "#000000")

#Save graph1
ggsave(filename = "XRD.png", width = 12, height = 12)

#Graph SI
datadrx <- bind_rows(datadrx_x1, datadrx_x2)

ggplot(data = datadrx, aes(x = twotheta, y = count)) +
  geom_line(aes(group = R, color =R), size=0.5) + 
  ylab("Counts (u.a.)") + xlab( expression(paste(plain('2'),plain(theta), plain(' ('),plain(""^{o}),plain(")")))) + 
  xlim(5, 70) + ylim(0,2500) + 
  theme_minimal() + 
  scale_color_manual(values=c("#999999","#800000","#000000")) +
  geom_text(data = database1, aes(x=twotheta, y=height+50), label = "+", size = 8) + 
  theme(legend.position='none', text = element_text(size = 24), panel.grid = element_line(color="lightgray")) + 
  geom_text(aes(x=60, y=550), label = "R = 1.5 dried", size = 8, color = "#999999")+ 
  geom_text(aes(x=60, y=1450), label = "R = 1.5 wet", size = 8, color = "#800000")

#Save graph1
ggsave(filename = "XRD-SI.png", width = 12, height = 12)
