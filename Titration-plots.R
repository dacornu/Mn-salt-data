library("readr")
library("ggplot2")
library("dplyr")
library("tidyr")
library("magrittr")

data1 <- read_tsv("Titration-MnCl2.txt", col_names = FALSE)
colnames(data1)[1]<-"R"
colnames(data1)[2]<- "MnCl2"
data1$R <- data1$R/20
data1 <- data1 %>% select(1,2)

data2 <- read_tsv("Titration-MnSO4.txt", col_names = FALSE)
colnames(data2)[1]<-"R"
colnames(data2)[2]<-"MnSO4"
data2$R <- data2$R/20
data2 <- data2 %>% select(1,2)

data4 <- read_tsv("Titration-MnSO4.txt", col_names = FALSE)
colnames(data4)[1]<-"R"
colnames(data4)[2]<-"pH"
data4$R <- data4$R/20
data4 <- data4 %>% select(1,2)

data3 <- read_tsv("Titration-Dissolution-Mn(OH)2-fromSO4.txt", col_names = FALSE)
colnames(data3)[1]<-"R"
colnames(data3)[2]<-"pH"
data3$R <- 2-data3$R/20
data3 <- data3 %>% select(1,2)

data <- left_join(data1, data2, by="R")

data <- pivot_longer(data, cols = starts_with("Mn"), names_to = "Salt", values_to = "pH")

ggplot(data) +
  geom_line(aes(x=R, y=pH, color=Salt), size =1) + 
  theme_minimal() + 
  scale_x_continuous(expand=c(0,0.05))+scale_y_continuous(expand=c(0,0.1)) + 
  scale_color_manual(values=c("#E69F00", "#000000"), name = "", labels = c(expression("MnCl"[2]), expression("MnSO"[4]))) + 
  geom_vline(xintercept = 1.54, linetype="dotted") + 
  theme(text = element_text(size = 24), legend.position = c(0.5,0.4), legend.text.align = 0, legend.text = element_text(size=24,face="bold"), panel.grid = element_line(color="lightgray"))

ggsave(filename = "titration.png", width = 12, height = 7)

ggplot(data) +
  geom_line(aes(x=R, y=pH, color=Salt), size =1) + 
  theme_minimal() + 
  scale_x_continuous(expand=c(0,0.05))+scale_y_continuous(expand=c(0,0.1)) + 
  scale_color_manual(values=c("#E69F00", "#000000"), name = "", labels = c(expression("MnCl"[2]), expression("MnSO"[4]))) + 
  geom_vline(xintercept = 1.54, linetype="dotted") + 
  theme(text = element_text(size = 36), legend.position = c(0.5,0.4), legend.text.align = 0, legend.text = element_text(size=24,face="bold"), panel.grid = element_line(color="lightgray"))

ggsave(filename = "TOC.png", width = 9, height = 3.5)


dataMnSO4 <- bind_rows(data4,data3, .id="direction")

ggplot(dataMnSO4) +
  geom_line(aes(x=R, y=pH, color=direction), size =1) + 
  theme_minimal() + 
  scale_x_continuous(expand=c(0,0.05))+scale_y_continuous(expand=c(0,0.1)) + 
  scale_color_manual(values=c("#000000", "#999999"), name = "", labels = c("Precipitation","Dissolution")) + 
  geom_vline(xintercept = 1.54, linetype="dotted") + 
  theme(text = element_text(size = 24), legend.position = c(0.5,0.4), legend.text.align = 0, legend.text = element_text(size=24,face="bold"), panel.grid = element_line(color="lightgray"))

ggsave(filename = "titrationMnSO4.png", width = 12, height = 7)

data6 <- read_tsv("Titration-MnCl2.txt", col_names = FALSE)
colnames(data6)[1]<-"R"
colnames(data6)[2]<-"pH"
data6$R <- data6$R/20
data6 <- data6 %>% select(1,2)

data5 <- read_tsv("Titration-Mn(OH)2-dissolution.txt", col_names = FALSE)
colnames(data5)[1]<-"R"
colnames(data5)[2]<-"pH"
data5$R <- 2-data5$R/20
data5 <- data5 %>% select(1,2)

dataMnCl2 <- bind_rows(data6,data5, .id="direction")

ggplot(dataMnCl2) +
  geom_line(aes(x=R, y=pH, color=direction), size =1) + 
  theme_minimal() +
  scale_x_continuous(expand=c(0,0.05))+scale_y_continuous(expand=c(0,0.1)) + 
  scale_color_manual(values=c("#000000", "#999999"), name = "", labels = c("Precipitation","Dissolution")) +
  theme(text = element_text(size = 24), legend.position = c(0.5,0.4), legend.text.align = 0, legend.text = element_text(size=24,face="bold"), panel.grid = element_line(color="lightgray"))

ggsave(filename = "titrationMnCl2.png", width = 12, height = 7)