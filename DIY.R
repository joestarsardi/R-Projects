library(tidyverse)
library(readxl)
library(RColorBrewer)

df <- read_excel("Buku1.xlsx", sheet = "Lembar1")
head(df)
coul <- brewer.pal(5, "Set3")
#df$Kabupaten_Kota <- gsub(" ", "\n", df$Kabupaten_Kota)
df$Prevalensi <- round(df$Prevalensi)

p5 <- ggplot(df, aes(y = reorder(Daerah, Prevalensi), x = Prevalensi)) +
  geom_col(
    aes(fill = factor(Daerah)),
    position = "identity",
    color = "black",
    width = 0.8,
    linewidth = 0.3,
    just = 0.5
  ) +
  theme_classic() +
  labs(
    y = "",
    x = "Prevalensi (%)",
    title = "Tingkat Stunting di DI Yogyakarta Menurut Kabupaten/Kota Tahun 2022",
    subtitle = "Menurut Hasil Survei Status Gizi Indonesia Tahun 2023",
    fill = "",
  ) +
  coord_cartesian(xlim = c(0, 25)) +
  scale_x_continuous(breaks = seq(0, 25, by = 5)) +
  scale_fill_manual(values = c("#e9c46a", "#d17777", "#d177a1", "#77d1af", "#77a1d1")) +
  theme(
    axis.text.y = element_text(size = 10, angle = 0, vjust = 0.5, hjust = 0.5, color = "black"),
    axis.text.x = element_text(size = 10, color = "black"),
    axis.title.x = element_text(size = 10, vjust = 2.5),
    axis.title.y = element_text(size = 10, hjust = 0.5, vjust = -1.5),
    legend.position = "top",
    legend.title = element_text(size = 11, color = "black"),
    legend.text = element_text(size = 9, color = "black"),
    legend.key.size = unit(4, "mm"),
    plot.subtitle = element_text(size = 11, color = "black", hjust = 0.5, vjust = -0.5),
    plot.title = element_text(size = 13, face = "bold", color = "black", hjust = 0.5, vjust = -0.5)
  ) +
  geom_text(
    aes(label = paste(Prevalensi), fontface = "bold"),
    size = 4,
    nudge_x = -1,
    col = "black"
  ) +
  annotate("rect", 
           ymin = 4.5, ymax = 5.5 , 
           xmin = -Inf, xmax = Inf, 
           alpha = 0.3, fill = "#d17777", layer = "below") +
  geom_vline(xintercept = 21.6, slope = 0, color = "red", linetype = 2, size = 0.5, linewidth = 0.5) +
  geom_vline(xintercept = 16.4, slope = 0, color = "black", linetype = 2, size = 0.5, linewidth = 0.5) +
  annotate("text", y = 0.6, x = 18.4, label = "DI Yogyakarta", size = 2.5, color = "black") +
  annotate("text", y = 0.6, x = 23, label = "Indonesia", size = 2.5, color = "red") +
  annotate("text", y = 1:5, x = -0.6, label = c(5, 4, 3, 2, 1), fontface = "bold")
p5

ggsave("DIY.jpg", plot = p5, width = 8, height = 4)