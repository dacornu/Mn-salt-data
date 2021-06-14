library("readr")
library("ggplot2")
library("dplyr")
library("tidyr")
library("magrittr")

data <- read_delim("Quantities-ICP-CI.csv", delim = ";", col_names = TRUE)

data <- pivot_longer(data, !R, names_to = "Element", values_to = "Quantities")

data$Quantities <- data$Quantities*1000

ggplot(data) +
  geom_point(aes(x=R, y=Quantities, group = Element, color = Element, shape = Element), size =3)+ 
  theme_minimal() + 
  scale_x_continuous(expand=c(0.01,0))+scale_y_continuous(expand=c(0.01,0)) + 
  theme(text = element_text(size = 24), legend.text.align = 0, legend.text = element_text(size=24,face="bold"), panel.grid = element_line(color="lightgray")) +
  geom_hline(yintercept = 0, size = 1) +
  geom_vline(xintercept = 0, size = 1) +
  ylab ("Quantities (mmol)") + 
  scale_color_manual(values=c("black", "green"), name = "Element", labels = c("Mn","S")) +
  geom_hline(yintercept = 20) +
  geom_segment(aes(x = 0, xend = 1.5, y = 20, yend = 15), linetype = "dotted") +
  geom_segment(aes(x = 0, xend = 1.5, y = 20, yend = 0), linetype = "dotted") +
  geom_segment(aes(x = 0, xend = 1.55, y = 20, yend = 7/9*20), linetype = "dashed") +
  geom_segment(aes(x = 0, xend = 2, y = 20, yend = 0)) +
  geom_segment(aes(x = 0, xend = 1.55, y = 20, yend = 0), linetype = "dashed") 



ggsave(filename = "quantities2.png", width = 12, height = 7)
