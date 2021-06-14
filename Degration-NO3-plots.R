library("readr")
library("ggplot2")
library("dplyr")
library("tidyr")
library("magrittr")

data <- read_delim("Degradation-NO3.csv", delim = ";", col_names = TRUE)


lm1 <- lm(data = data, With_salt~Time)
lm2 <- lm(data = data, Without_salt~Time)

colnames(data)[2] <- "With salt"
colnames(data)[3] <- "Without salt"

data <- pivot_longer(data, !Time, names_to = "Salt", values_to = "Concentrations")



ggplot(data) +
  geom_point(aes(x=Time, y=Concentrations, group = Salt, color = Salt, shape = Salt), size =3)+ 
  theme_minimal() + 
  ylab(expression(paste(plain("Concentration (mol.L"),plain(""^{-1}),plain(")")))) + 
  xlab("Time (days)")+
  scale_color_manual(values=c("pink","black"))+
  scale_x_continuous(expand=c(0.05,0))+scale_y_continuous(expand=c(0.05,0)) + 
  theme(legend.position = c(0.5, 0.3), legend.title = element_blank(), text = element_text(size = 24), panel.grid = element_line(color="lightgray")) +
  geom_hline(yintercept = 0, size = 1) +
  geom_vline(xintercept = 0, size = 1) +
  geom_segment(aes(x = 0, y = lm1[["coefficients"]][1], xend = 7, yend = lm1[["coefficients"]][1]+7*lm1[["coefficients"]][2]), color = "pink", size = 1) +
  geom_segment(aes(x = 0, y = lm2[["coefficients"]][1], xend = 7, yend = lm2[["coefficients"]][1]+7*lm2[["coefficients"]][2]), size = 1) +
  geom_errorbar(aes(x =Time, ymin = Concentrations - 0.01, ymax = Concentrations + 0.01), width=.2, position=position_dodge(.9))



ggsave(filename = "NO3.png", width = 12, height = 7)
