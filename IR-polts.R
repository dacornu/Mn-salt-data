library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)

#Read
data1.5 <- read_csv("IR-R=1.5.dpt", col_names = c("wavenumber", "1.5"))
data2.0 <- read_csv("IR-R=2.0.dpt", col_names = c("wavenumber", "2.0"))

data2.0$"2.0" <- data2.0$"2.0"+0.3

df = left_join(data1.5, data2.0, by = "wavenumber")%>%
  gather(., key ="R", value="absorbance", 2:3)

#Plot
ggplot(data = df, aes(x=wavenumber, y=absorbance)) +
  geom_line(aes(group = R, color = R)) + 
  ylab("Absorbance") + xlab( expression(paste(plain('Wavenumber (cm'),plain(""^{-1}),plain(")")))) + 
  xlim(2000, 800) + ylim(0, 0.6) +
  theme_minimal() +
  scale_color_manual(values=c("#800000","#000000")) +
  theme(legend.position="none", axis.text.y = element_text(size = 16, color ="black"), axis.title.y = element_text(size = 16), axis.text.x = element_text(size = 16, color ="black"), axis.title.x = element_text(size = 16), panel.grid = element_line(color="lightgray")) +
  geom_text(aes(x=1636, y=0.27), label = "- 1636", size = 8, angle = 90) +
  geom_text(aes(x=1119, y=0.22), label = "- 1119", size = 8, angle = 90) +
  geom_text(aes(x=1080, y=0.22), label = "- 1080", size = 8, angle = 90) +
  geom_text(aes(x=1900, y=0.4), label = "R = 2", size = 8) +
  geom_text(aes(x=1900, y=0.1), label = "R = 1.5", size = 8, color="#800000")

ggsave(filename = "IR.png", width = 12, height = 8)

ggplot(data = df, aes(x=wavenumber, y=absorbance)) +
  geom_line(aes(group = R, color = R)) + 
  ylab("Absorbance") + xlab( expression(paste(plain('Wavenumber (cm'),plain(""^{-1}),plain(")")))) + 
  xlim(3800, 2500) + ylim(0, 0.8) +
  theme_minimal() +
  scale_color_manual(values=c("#800000","#000000")) +
  theme(legend.position="none", axis.text.y = element_text(size = 16, color ="black"), axis.title.y = element_text(size = 16), axis.text.x = element_text(size = 16, color ="black"), axis.title.x = element_text(size = 16), panel.grid = element_line(color="lightgray")) +
  geom_text(aes(x=3628, y=0.55), label = "- 3628", size = 8, angle = 90) +
  geom_text(aes(x=2700, y=0.45), label = "R = 2", size = 8) +
  geom_text(aes(x=2700, y=0.15), label = "R = 1.5", size = 8, color="#800000")


ggsave("IR-OH.png", units="in", width=8, height=4, dpi=300)

ggplot(data = df, aes(x=wavenumber, y=absorbance, group=x)) + geom_line(aes(color=x)) + ylab("Absorbance") + xlab( expression(paste(plain('Wavenumber (cm'),plain(""^{-1}),plain(")")))) + xlim(1350, 900) + scale_color_viridis_d(option = "B", end = 0.7)+theme_bw()+ scale_colour_viridis_d(option = "inferno", begin = 0, end = 0.8)+ylim(0,2.3)

ggsave("SO4-IR-graph.png", units="in", width=5, height=4, dpi=300)

ggplot(data = df, aes(x=wavenumber, y=absorbance, group=x)) + geom_line(aes(color=x)) + ylab("Absorbance") + xlab( expression(paste(plain('Wavenumber (cm'),plain(""^{-1}),plain(")")))) + xlim(1350, 400) + scale_color_viridis_d(option = "B", end = 0.7)+theme_bw()+ scale_colour_viridis_d(option = "inferno", begin = 0, end = 0.8)+ylim(0,2.5)

ggsave("Far-IR-graph.png", units="in", width=8, height=4, dpi=300)

ggplot(data = df, aes(x=wavenumber, y=absorbance, group=x)) + geom_line(aes(color=x)) + ylab("Absorbance") + xlab( expression(paste(plain('Wavenumber (cm'),plain(""^{-1}),plain(")")))) + xlim(3750, 2750) + scale_color_viridis_d(option = "B", end = 0.7)+theme_bw()+ scale_colour_viridis_d(option = "inferno", begin = 0, end = 0.8)+ylim(0,2.5)+ stat_peaks(geom = "text", ignore_threshold = 0.2)
