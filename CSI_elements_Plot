# Load libraries
library(ggplot2)
library(dplyr)
library(extrafontdb)

# Read CSV file
cis_elements <- read.csv("Cis.csv", header = TRUE, stringsAsFactors = FALSE)

# Define the specific gene order
gene_order <- paste0("StSNAT", 29:1)

# Convert Gene column to a factor with specified order
cis_elements$Gene <- factor(cis_elements$Gene, levels = gene_order)

# Define motif colors
motif_colors <- c(
  "LTR" = "#1f77b4",
  "ABRE" = "#ff7f0e",
  "STRE" = "#2ca02c",
  "ARE" = "#d62728",
  "MBS" = "#9467bd",
  "DRE core" = "#8c564b",
  "W box" = "#e377c2",
  "TCA-element" = "#7f7f7f",
  "ERE" = "#bcbd22",
  "CCAAT-box" = "#17becf",
  "GARE-motif" = "#aec7e8",
  "WUN-motif" = "#ffbb78",
  "P-box" = "#98df8a",
  "TC-rich repeats" = "#ff9896",
  "RY-element" = "#c5b0d5",
  "DRE1" = "#c49c94"
)

# Plot
ggplot(cis_elements, aes(x = Position, y = Gene, color = Motif)) +
  # horizontal baseline for each gene
  geom_segment(data = distinct(cis_elements, Gene),
               aes(x = 0, xend = max(cis_elements$Position)*1.05, y = Gene, yend = Gene),
               inherit.aes = FALSE, color = "black", size = 0.6) +
  # cis-element markers as vertical lines
  geom_segment(aes(x = Position, xend = Position,
                   y = as.numeric(factor(Gene)) - 0.2,
                   yend = as.numeric(factor(Gene)) + 0.2), size = 1) +
  # 5' and 3' labels outside the x-axis
  annotate("text", x = -max(cis_elements$Position)*0.03, y = 0.7, label = "5'", 
           hjust = 1, size = 5, family = "Verdana", fontface = "bold") +
  annotate("text", x = max(cis_elements$Position)*1.08, y = 0.7, label = "3'", 
           hjust = 0, size = 5, family = "Verdana", fontface = "bold") +
  scale_color_manual(values = motif_colors) +
  theme_minimal(base_family = "Verdana", base_size = 14) +
  labs(x = "Position (bp)", y = "", color = "Cis-element") +
  theme(
    axis.text.y = element_text(size = 12, color = "black", face = "bold", family = "Verdana"),
    axis.text.x = element_text(size = 12, color = "black", family = "Verdana"),
    axis.title.x = element_text(size = 14, face = "bold", family = "Verdana"),
    legend.position = "right",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line.x = element_line(color = "black", size = 0.5),
    axis.line.y = element_blank()
  )
