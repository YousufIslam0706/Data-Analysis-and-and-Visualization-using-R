# Load required libraries
library(ComplexHeatmap)
library(circlize)
library(pheatmap)

# --- Step 1: Read and prepare the data ---
data <- read.csv(file.choose(), row.names = 1)
data[] <- lapply(data, as.numeric)     # Convert to numeric while preserving structure
mat <- as.matrix(data)
mat[is.na(mat)] <- 0                   # Replace any NAs with 0

# --- Step 2: Build dendrograms for ComplexHeatmap ---
row_dend <- as.dendrogram(hclust(dist(mat), method = "ward.D2"))
col_dend <- as.dendrogram(hclust(dist(t(mat)), method = "ward.D2"))

# --- Step 3: Color definitions ---
# For ComplexHeatmap (color function for continuous scale)
col_fun <- colorRamp2(
  c(1, 2, 3),  # Use 1 = Resistant, 2 = Intermediate, 3 = Sensitive
  c("red", "yellow", "#008080")
)

# For pheatmap (gradient color palette)
color_map <- colorRampPalette(c("red", "yellow", "#008080"))(100)

# --- Step 4: Plot with ComplexHeatmap (full control & custom legend) ---
Heatmap(
  mat,
  name = "Value",
  cluster_rows = row_dend,
  cluster_columns = col_dend,
  col = col_fun,
  show_row_names = TRUE,
  show_column_names = TRUE,
  row_names_gp = gpar(fontsize = 12, fontface = "bold"),
  column_names_gp = gpar(fontsize = 12, fontface = "bold"),
  row_dend_reorder = FALSE,
  column_dend_reorder = FALSE,
  heatmap_legend_param = list(
    title = "Scale",
    at = c(1, 2, 3),
    labels = c("Resistant", "Intermediate", "Sensitive")
  )
)

# --- Step 5: Plot with pheatmap (no custom legend allowed) ---
pheatmap(
  mat,
  clustering_method = "ward.D2",
  clustering_distance_rows = "euclidean",
  clustering_distance_cols = "euclidean",
  color = color_map,
  cutree_rows = 3,
  cutree_cols = 3,
  show_rownames = TRUE,
  show_colnames = TRUE,
  fontsize_row = 10,
  fontsize_col = 12,
  heatmap_legend_param = list(
    title = "Scale",
    at = c(1, 2, 3),
    labels = c("Resistant", "Intermediate", "Sensitive")
  )
)



pheatmap(
  mat,
  clustering_method = "ward.D2",        # Hierarchical clustering
  clustering_distance_rows = "euclidean",
  clustering_distance_cols = "euclidean",
  color = color_map,
  cutree_rows = 3,                      # Cut rows into 3 clusters
  cutree_cols = 3,                      # Cut cols into 3 clusters
  show_rownames = TRUE,
  show_colnames = TRUE,
  fontsize_row = 12 ,
  fontsize_col = 12,
  border_color = "grey70",
  heatmap_legend_param = list(
    title = "Scale",
    at = c(1, 2, 3),
    labels = c("Resistant", "Intermediate", "Sensitive")
  ))





Heatmap(
  mat,
  name = "Value",
  cluster_rows = row_dend,
  cluster_columns = col_dend,
  col = col_fun,
  
  # ✅ Cell border around each cell
  rect_gp = gpar(col = "grey70", lwd = 0.5),  # thin grey border
  
  # ✅ Cut dendrograms into 3 clusters
  row_km = 3,
  column_km = 3,
  
  # Labels and font styling
  show_row_names = TRUE,
  show_column_names = TRUE,
  row_names_gp = gpar(fontsize = 12, fontface = "bold"),
  column_names_gp = gpar(fontsize = 12, fontface = "bold"),
  
  # Keep original order of dendrograms
  row_dend_reorder = FALSE,
  column_dend_reorder = FALSE,
  
  # Custom legend
  heatmap_legend_param = list(
    title = "Scale",
    at = c(1, 2, 3),
    labels = c("Resistant", "Intermediate", "Sensitive")
  )
)






# Load required libraries
library(ComplexHeatmap)
library(circlize)
library(pheatmap)

# --- Step 1: Read and prepare the data ---
data <- read.csv(file.choose(), row.names = 1)
data[] <- lapply(data, as.numeric)     # Convert to numeric while preserving structure
mat <- as.matrix(data)
mat[is.na(mat)] <- 0                   # Replace any NAs with 0

# --- Step 2: Build dendrograms for ComplexHeatmap ---
row_dend <- as.dendrogram(hclust(dist(mat), method = "ward.D2"))
col_dend <- as.dendrogram(hclust(dist(t(mat)), method = "ward.D2"))

# --- Step 3: Color definitions ---
col_fun <- colorRamp2(
  c(1, 2, 3),  # 1 = Resistant, 2 = Intermediate, 3 = Sensitive
  c("red", "yellow", "#008080")
)

# --- Step 4: Plot ComplexHeatmap with cell borders and clusters ---
Heatmap(
  mat,
  name = "Value",
  cluster_rows = row_dend,
  cluster_columns = col_dend,
  col = col_fun,
  
  # ✅ Add thin grey border to each cell
  rect_gp = gpar(col = "grey70", lwd = 0.5),
  
  # ✅ Cut dendrograms into 3 row and 3 column clusters
  row_km = 3,
  column_km = 3,
  
  # ✅ Label styling
  show_row_names = TRUE,
  show_column_names = TRUE,
  row_names_gp = gpar(fontsize = 12, fontface = "bold"),
  column_names_gp = gpar(fontsize = 12, fontface = "bold"),
  
  # ✅ Prevent reordering (optional)
  row_dend_reorder = FALSE,
  column_dend_reorder = FALSE,
  
  # ✅ Custom legend
  heatmap_legend_param = list(
    title = "Scale",
    at = c(1, 2, 3),
    labels = c("Resistant", "Intermediate", "Sensitive")
  )
)




Heatmap(
  mat,
  name = "Value",
  cluster_rows = row_dend,
  cluster_columns = col_dend,
  col = col_fun,
  
  # ✅ Cell borders
  rect_gp = gpar(col = "grey70", lwd = 0.5),
  
  # ✅ Cut dendrogram into 3 clusters (NOT k-means)
  row_split = 3,
  column_split = 3,
  
  # ✅ Labels and fonts
  show_row_names = TRUE,
  show_column_names = TRUE,
  row_names_gp = gpar(fontsize = 12),
  column_names_gp = gpar(fontsize = 12),
  
  # ✅ Keep original dendrogram ordering
  row_dend_reorder = FALSE,
  column_dend_reorder = FALSE,
  
  # ✅ Legend
  heatmap_legend_param = list(
    title = "Scale",
    at = c(1, 2, 3),
    labels = c("Resistant", "Intermediate", "Sensitive")
  )
)





Heatmap(
  mat,
  name = "Value",
  cluster_rows = row_dend,
  cluster_columns = col_dend,
  col = col_fun,
  
  # ✅ Add cell borders
  rect_gp = gpar(col = "grey70", lwd = 0.5),
  
  # ✅ Cut into clusters
  row_split = 3,
  column_split = 3,
  
  # ✅ Remove cluster numbers from figure
  row_title = NULL,
  column_title = NULL,
  
  # ✅ Label styling
  show_row_names = TRUE,
  show_column_names = TRUE,
  row_names_gp = gpar(fontsize = 12),
  column_names_gp = gpar(fontsize = 12),
  
  # ✅ Keep original dendrogram order
  row_dend_reorder = FALSE,
  column_dend_reorder = FALSE,
  
  # ✅ Custom legend
  heatmap_legend_param = list(
    title = "Category",
    at = c(1, 2, 3),
    labels = c("Resistant", "Intermediate", "Sensitive")
  )
)





# Load required libraries
library(ComplexHeatmap)
library(circlize)

# --- Read and prepare data ---
data <- read.csv(file.choose(), row.names = 1)
data[] <- lapply(data, as.numeric)
mat <- as.matrix(data)
mat[is.na(mat)] <- 0

# --- Dendrograms ---
row_dend <- as.dendrogram(hclust(dist(mat), method = "ward.D2"))
col_dend <- as.dendrogram(hclust(dist(t(mat)), method = "ward.D2"))

# --- Color mapping ---
col_fun <- colorRamp2(
  c(1, 2, 3),
  c("red", "yellow", "#008080")
)

# --- Save heatmap to PNG file with 300 DPI ---
png("heatmap_output.png", width = 3000, height = 3000, res = 300)  # width/height in pixels = 10 inches

# --- Plot heatmap ---
Heatmap(
  mat,
  name = "Value",
  cluster_rows = row_dend,
  cluster_columns = col_dend,
  col = col_fun,
  rect_gp = gpar(col = "grey70", lwd = 0.5),
  row_split = 3,
  column_split = 3,
  row_title = NULL,
  column_title = NULL,
  show_row_names = TRUE,
  show_column_names = TRUE,
  row_names_gp = gpar(fontsize = 12),
  column_names_gp = gpar(fontsize = 12),
  row_dend_reorder = FALSE,
  column_dend_reorder = FALSE,
  heatmap_legend_param = list(
    title = "Category",
    at = c(1, 2, 3),
    labels = c("Resistant", "Intermediate", "Sensitive")
  )
)

# Close graphics device
dev.off()

