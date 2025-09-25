library(ggplot2)
library(dplyr)

# Read CSV
cis_elements <- read.csv("motif_prediction.csv", header = TRUE, stringsAsFactors = FALSE)

# Ensure GENE is ordered
cis_elements$GENE <- factor(cis_elements$GENE,
                            levels = paste0("SNAT", 29:1))

# Define motif consensus mapping
motif_consensus <- c(
  "MOTIF1"  = "YVEDVAVRPSYRRRGJGKKLV",
  "MOTIF2"  = "VYLHVLRANKAAIRLYKKLGF",
  "MOTIF3"  = "RVPSIPDLFRPFGAYFLYGLGGAGADAPRLARALCRHAHNVAR",
  "MOTIF4"  = "EAFLASPPASWAVLSVWDCKDAFRLEVRGAPRLLRAAAAATRALDRAAPW",
  "MOTIF5"  = "YDIEDGDPREAAFZKYGRIRGSIEGKLSPGPPADNLPRRGVLGRWRT",
  "MOTIF6"  = "PDSSSTIGSLRLDLSASPLADPEAAAFASPRGGGRVTAGFVICFPNYSS",
  "MOTIF7"  = "GTTFAGDVWEGLRLADAEDLPGIRKJIHPLEEFGLLVDRT",
  "MOTIF8"  = "WCICRLAGAALDKYKGBQEEA",
  "MOTIF9"  = "YRIRFAGVEFFPVDIDAVLSNRLSKGTF",
  "MOTIF10" = "CWEVADTHCGSFFPGYKFPLDLVLRIDRYIALLSGFSVPPGCMRTC"
)

# Add readable motif labels for legend
cis_elements <- cis_elements %>%
  mutate(MotifLabel = motif_consensus[MOTIF])

# Distinct gene lengths
gene_lengths <- distinct(cis_elements, GENE, LENGTH)

# Determine global x-axis range
x_min <- 0
x_max <- max(gene_lengths$LENGTH)

# Plot
ggplot(cis_elements, aes(x = (START + END)/2,
                         y = GENE,
                         width = END - START,
                         height = 0.6,
                         fill = MotifLabel)) +
  # baselines
  geom_segment(data = gene_lengths,
               aes(x = 0, xend = LENGTH, y = GENE, yend = GENE),
               inherit.aes = FALSE, color = "black", size = 0.5) +
  # motif tiles
  geom_tile(color = "black") +
  # 5' and 3' labels only once at the global x-axis
  annotate("text", x = x_min, y = 0.5, label = "5'", hjust = 1.2, size = 4, family = "Verdana") +
  annotate("text", x = x_max, y = 0.5, label = "3'", hjust = -0.2, size = 4, family = "Verdana") +
  # fill colors
  scale_fill_brewer(palette = "Set3", name = "Motif Consensus") +
  # style
  theme_minimal(base_family = "Verdana") +
  labs(x = "Position (aa)", y = "Gene") +
  theme(
    axis.text.y  = element_text(size = 10, color = "black"),
    axis.text.x  = element_text(size = 10, color = "black"),
    legend.position = "right",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.x = element_line(color = "black", size = 0.5),
    axis.line.y = element_blank(),
    plot.margin = margin(10, 20, 30, 20)
  ) +
  coord_cartesian(clip = "off")
