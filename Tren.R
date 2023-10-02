library(tidyverse)
library(readxl)
library(RColorBrewer)
library(shadowtext)
library(showtext)

df <- read_excel("Buku1.xlsx", sheet = "Tren")
head(df)

#as.character(df$Tahun)

p4 <- ggplot(df, aes(factor(Tahun), Prevalensi)) +
  theme_classic() +
  geom_col(
    aes(fill = Survei),
    color = "black",
    linewidth = 0.3,
    width = 0.9
  ) + 
  geom_text(
    aes(label = paste(Prevalensi, "%")),
    color = "black",
    size = 4,
    vjust = -0.5,
    fill = "gold",
    alpha = 1,
    fontface= "bold"
  ) +
  scale_fill_manual(values = c("#618264", "#B0D9B1", "#F4D160")) +
  labs(
    x = "",
    y = "Prevalensi (%)",
    fill = "Survei:",
    title = "Tren Angka Prevalensi Stunting di Indonesia Tahun 2013 - 2022",
    subtitle = "Menurut Hasil Survei Riskesdas dan SSGI"
  ) +
  coord_cartesian(ylim = c(0, 40)) +
  theme(
    text = element_text(family = "Arial"),
    axis.text.x = element_text(size = 10, angle = 0, vjust = 0.5, hjust = 0.5, color = "black"),
    axis.text.y = element_text(size = 10, color = "black"),
    axis.title.y = element_text(size = 10, vjust = 2.5),
    axis.title.x = element_text(size = 10, hjust = 0.5, vjust = -1.5),
    legend.position = "top",
    legend.title = element_text(size = 11, color = "black", face = "bold"),
    legend.text = element_text(size = 9, color = "black"),
    legend.key.size = unit(4, "mm"),
    plot.subtitle = element_text(size = 11, color = "black", hjust = 0.5, vjust = 0.7),
    plot.title = element_text(size = 13, face = "bold", color = "black", hjust = 0.5, vjust = -0.5)
  ) + 
  annotate("rect", 
           xmin = 6.5, xmax = 9.5 , 
           ymin = 0, ymax = Inf, 
           alpha = 0.3, fill = "#B0D9B1", layer = "below") +
  annotate("rect", 
           xmin = 9.5, xmax = 11.5 , 
           ymin = 0, ymax = Inf, 
           alpha = 0.1, fill = "#F4D160", layer = "below") +
  annotate("text", 
           x = 8, y = 40, 
           label = "Pandemi COVID-19", size = 3, color = "black" 
           )
p4 

ggsave("tren.jpg", plot = p4, width = 10, height = 4)
