library(tidyverse)
library(RColorBrewer)

df <- read.csv("Buku1.csv")
head(df)
glimpse(df)
as.numeric(df$X2021)
as.numeric(df$X2022)
as.numeric(df$Perubahan)

df$Pulau <- gsub(" ", "\n", df$Pulau)

low10 <- df %>%
  select(-'X2021', -'Perubahan') %>%
  arrange((df$X2022)) %>%
  top_n(10)
low10

p2 <- ggplot(low10, aes(x = reorder(Pulau,X2022), y = X2022)) +
  geom_col(
    aes(fill = factor(Provinsi)),
    position = "identity",
    color = "black",
    width = 0.7,
    linewidth = 0.3,
    just = 0.5
  ) +
  theme_classic() +
  labs(
    x = "",
    y = "Prevalensi (%)",
    title = "10 Provinsi dengan Prevalensi Stunting Tertinggi di Indonesia Tahun 2022",
    subtitle = "Menurut Hasil Survei Status Gizi Indonesia Tahun 2023",
    fill = "",
  ) +
  coord_cartesian(ylim = c(0, 36)) +
  scale_y_continuous(breaks = seq(0, 36, by = 2)) +
  scale_fill_manual(values = c("#8ecae6","#3a5a40","#bc6c25", "#dda15e","#023047")) +
  theme(
    axis.text.x = element_text(size = 9, angle = 0, vjust = 0.5, hjust = 0.5, color = "black"),
    axis.text.y = element_text(size = 9, face = "bold", color = "black"),
    axis.title.y = element_text(size = 8, vjust = 5.5),
    axis.title.x = element_text(size = 8, hjust = 0.5, vjust = -1.5),
    legend.position = "bottom",
    legend.title = element_text(size = 9, color = "black"),
    legend.text = element_text(size = 8, face = "bold", color = "black"),
    legend.key.size = unit(4, "mm"),
    plot.subtitle = element_text(size = 12, color = "black", hjust = 0.5, vjust = -0.5),
    plot.title = element_text(size = 13, face = "bold", color = "black", hjust = 0.5, vjust = -0.5)
  ) +
  geom_text(
    aes(label = paste(X2022,"%"), fontface = "bold"),
    size = 4,
    nudge_y = 0.7
  ) +
  geom_abline(intercept = 21.6, slope = 0, color = "black", linetype = 2, size = 0.5, linewidth = 1) +
  annotate("rect", xmin = 0.5, xmax = Inf , ymin = 20.5, ymax = 21.6, alpha = 0.6, fill = "red") +
  annotate("text", x = 1.3, y = 21, label = "21.6% Indonesia", size = 3.5, color = "black", fontface = "bold") +
  annotate("text", x = 1:10, y = -0.8, label = c(25:34), fontface = "bold") +
  geom_abline(intercept = 14, slope = 0, color = "black", linetype = 2, size = 0.5, linewidth = 1) +
  annotate("rect", xmin = 0.5, xmax = Inf , ymin = 13, ymax = 14.0, alpha = 0.6, fill = "lightgreen") +
  annotate("text", x = 1.03, y = 13.5, label = "Target (2024)", size = 3, color = "black", fontface = "bold")
  
p2

ggsave("low 10.jpg", plot = p2, width = 8, height = 8)