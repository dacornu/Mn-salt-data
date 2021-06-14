library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)
library(magrittr)
library(tibble)

#Lecture

data1 <- read_tsv("Raman-R=0,5.txt", col_names = TRUE)
colnames(data1)[1]<-"Wavenumber"
colnames(data1)[2]<-"Absorbance"
data1$Absorbance <- data1$Absorbance/2
data1 <- add_column(data1, R = "0.5")

data2 <- read_tsv("Raman-R=1.txt", col_names = TRUE)
colnames(data2)[1]<-"Wavenumber"
colnames(data2)[2]<-"Absorbance"
data2$Absorbance <- data2$Absorbance/2 +1300
data2 <- add_column(data2, R = "1")

data3 <- read_tsv("Raman-R=1,5.txt", col_names = TRUE)
colnames(data3)[1]<-"Wavenumber"
colnames(data3)[2]<-"Absorbance"
data3$Absorbance <- data3$Absorbance +2600
data3 <- add_column(data3, R = "1.5")

data5 <- read_tsv("Raman-R=2.txt", col_names = TRUE)
colnames(data5)[1]<-"Wavenumber"
colnames(data5)[2]<-"Absorbance"
data5$Absorbance <- data5$Absorbance*2 +6800 
data5 <- add_column(data5, R = "2")


#Graph1

data <- bind_rows(data1, data2, data3, data5)

ggplot(data = data, aes(x=Wavenumber, y=Absorbance)) + 
  geom_line(aes(group = R, color =R)) +
  scale_color_manual(values=c("#F08080","#FF0000","#800000","#000000")) +
  ylab("Emission") + xlab( expression(paste(plain('Wavenumber (cm'),plain(""^{-1}),plain(")")))) +
  xlim(1075,300) + ylim(0,9000) +
  theme_minimal() + 
  theme(legend.position="none", text = element_text(size = 16), panel.grid = element_line(color="lightgray")) +
  geom_text(aes(x=325, y=8100), label = "R = 2", size = 8) +
  geom_text(aes(x=325, y=3750), label = "R = 1.5", size = 8) +
  geom_text(aes(x=325, y=2250), label = "R = 1", size = 8) +
  geom_text(aes(x=325, y=750), label = "R = 0.5", size = 8) +
  geom_text(aes(x=983, y=8450), label = "- 983", size = 8, angle = 90) +
  geom_text(aes(x=996, y=4500), label = "- 996", size = 8, angle = 90) + 
  geom_text(aes(x=967, y=6200), label = "- 967", size = 8, angle = 90) +
  geom_text(aes(x=529, y=4300), label = "- 529", size = 8, angle = 90) +
  geom_text(aes(x=459, y=4150), label = "- 459", size = 8, angle = 90) +
  geom_text(aes(x=504, y=8450), label = "- 504", size = 8, angle = 90) +
  geom_text(aes(x=406, y=8050), label = "- 406", size = 8, angle = 90) 

#Graph2
ggsave(filename = "raman.png", width = 12, height = 10)
