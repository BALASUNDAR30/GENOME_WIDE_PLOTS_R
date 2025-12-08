import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.patches as patches
from matplotlib import font_manager

# ====================== SET FONT CONFIGURATION ======================

# Try enabling Times New Roman; if unavailable, fallback will prevent crash
if "Times New Roman" in [f.name for f in font_manager.fontManager.ttflist]:
    plt.rcParams["font.family"] = "Times New Roman"
else:
    print("Times New Roman not found. Using DejaVu Serif as fallback.")
    plt.rcParams["font.family"] = "DejaVu Serif"

plt.rcParams["font.size"] = 12

# ====================== LOAD INPUT FILES ======================

domains = pd.read_csv("CDD_filtered_nonoverlapping.tsv", sep="\t")
gene_length = pd.read_csv("Gene_id.csv")  # Query, Length

# ====================== CLEAN QUERY IDs ======================

domains["Query"] = domains["Query"].astype(str)
domains["Query"] = domains["Query"].str.replace(">", "", regex=False)
domains["Query"] = domains["Query"].str.replace(".p", "", regex=False)
domains["Query"] = domains["Query"].str.strip()
gene_length["Query"] = gene_length["Query"].astype(str).str.strip()

# ====================== MERGE & HANDLE LENGTH ======================

domains = domains.merge(gene_length, on="Query", how="left")
domains["Length"].fillna(domains["To"] + 50, inplace=True)

# ====================== UNIQUE COLORS FOR DOMAIN TYPES ======================

domain_names = domains["Short name"].unique()
color_map = {name: plt.cm.tab20(i / len(domain_names)) for i, name in enumerate(domain_names)}

# ====================== PLOT START ======================

genes = list(domains["Query"].unique())
genes.reverse()
y_positions = {gene: i for i, gene in enumerate(genes)}

fig, ax = plt.subplots(figsize=(12, 8))

for _, row in domains.iterrows():
    gene = row["Query"]
    y = y_positions[gene]

    # Backbone line (gene full length)
    ax.plot([0, row["Length"]], [y, y], color="black", linewidth=1)

    # Domain rectangle
    rect = patches.Rectangle(
        (row["From"], y - 0.25),
        row["To"] - row["From"],
        0.5,
        facecolor=color_map[row["Short name"]],
        edgecolor="black"
    )
    ax.add_patch(rect)

# ====================== LEGEND ======================

legend_handles = [patches.Patch(color=color_map[name], label=name) for name in domain_names]
ax.legend(handles=legend_handles, title="Domain", bbox_to_anchor=(1.02, 1), loc="upper left")

# ====================== AXIS & LABEL SETUP ======================

ax.set_yticks(list(y_positions.values()))
ax.set_yticklabels(genes, fontsize=12)

ax.set_xlabel("Position (bp)", fontsize=14)
ax.set_title("Domain Architecture of Proteins", fontsize=16, fontweight="bold")

ax.set_ylim(-1, len(genes))
ax.set_xlim(0, max(domains["Length"]) + 50)

plt.tight_layout()

# ====================== SAVE PLOT ======================

plt.savefig("Motif_Architecture_Plot_TNR.png", dpi=300, bbox_inches="tight")
plt.savefig("Motif_Architecture_Plot_TNR.pdf", dpi=300, bbox_inches="tight")

plt.show()

print("Motif architecture plot generated successfully with Times New Roman formatting.")
