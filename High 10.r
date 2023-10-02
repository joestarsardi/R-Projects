library(tidyverse)
library(RColorBrewer)
theme_set(theme_minimal(base_family = "Atlantis", base_size = 13))

df <- read.csv("Buku1.csv")
head(df)
glimpse(df)
as.numeric(df$X2021)
as.numeric(df$X2022)
as.numeric(df$Perubahan)

df$Pulau <- gsub(" ", "\n", df$Pulau)

top10 <- df %>%
  select(-'X2021', -'Perubahan') %>%
  arrange((df$X2022)) %>%
  top_n(-10)
top10

p1 <- ggplot(top10, aes(x = reorder(Pulau,X2022), y = X2022)) +
  geom_col(
    aes(fill = factor(Provinsi)),
    position = "identity",
    color = "black",
    width = 0.8,
    linewidth = 0.3,
    just = 0.5
  ) +
  theme_classic() +
  labs(
    x = "",
    y = "Prevalensi (%)",
    title = "10 Provinsi dengan Tingkat Stunting Terendah di Indonesia Tahun 2022",
    subtitle = "Menurut Hasil Survei Status Gizi Indonesia Tahun 2023",
    fill = "",
  ) +
  coord_cartesian(ylim = c(0, 22)) +
  scale_y_continuous(breaks = seq(0, 22, by = 2)) +
  scale_fill_manual(values = c("#77d1af", "#e9c46a", "#77a1d1")) +
  theme(
    axis.text.x = element_text(size = 10, angle = 0, vjust = 0.5, hjust = 0.5, color = "black"),
    axis.text.y = element_text(size = 10, color = "black"),
    axis.title.y = element_text(size = 10, vjust = 2.5),
    axis.title.x = element_text(size = 10, hjust = 0.5, vjust = -1.5),
    legend.position = "top",
    legend.title = element_text(size = 11, color = "black"),
    legend.text = element_text(size = 9, color = "black"),
    legend.key.size = unit(4, "mm"),
    plot.subtitle = element_text(size = 11, color = "black", hjust = 0.5, vjust = -0.5),
    plot.title = element_text(size = 13, face = "bold", color = "black", hjust = 0.5, vjust = -0.5)
  ) +
  geom_text(
    aes(label = paste(X2022,"%"), fontface = "bold"),
    size = 4,
    nudge_y = 0.7
  ) +
  annotate("rect", 
           xmin = 4.5, xmax = 5.5 , 
           ymin = -Inf, ymax = Inf, 
           alpha = 0.3, fill = "#e9c46a", layer = "below") +
  geom_abline(intercept = 21.6, slope = 0, color = "black", linetype = 2, size = 0.5, linewidth = 0.5) +
  annotate("rect", xmin = 0.4, xmax = Inf , ymin = 20.7, ymax = 21.6, alpha = 0.4, fill = "maroon") +
  annotate("text", x = 1.0, y = 21.1, label = "21.6% Indonesia", size = 3, color = "black", fontface = "bold") +
  annotate("text", x = 1:10, y = -0.6, label = c(1:10), fontface = "bold") +
  geom_abline(intercept = 14, slope = 0, color = "black", linetype = 2, size = 0.5, linewidth = 0.5) +
  #annotate("rect", xmin = 0.5, xmax = Inf , ymin = 13.3, ymax = 14.0, alpha = 0.6, fill = "lightgreen") +
  annotate("text", x = 0.8, y = 13.4, label = "Target (2024)", size = 2.5, color = "black")

ggsave("High 10.jpg", plot = p1, width = 10, height = 5)
