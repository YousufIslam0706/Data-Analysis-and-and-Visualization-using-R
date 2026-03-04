### Code written By Jahid Hasan
### Modified By Yousuf Islam

###Data load
data<-read.csv(file.choose())

head(data)

tail(data)
str(data)
data$Gen<-as.character(x=data$Gen)

str(data)
head(data)

rownames(data)<-c(data$Gen)
head(data)
newdata<-data[,-1]
head(newdata)

df1<-newdata
df <- na.omit(df1)
df <- scale(df1)
head(df)


##Dissimilarity matrix/distance matrix
res.dist<-dist(df1, method="euclidean")
res.hc<-hclust(d=res.dist, method="ward.D2")
plot(x=res.hc) #plot 1


library(factoextra)
fviz_dend(x=res.hc,cex=2,lwd=2,rect = T) #plot 2

###Rectangle fill with no border
fviz_dend(x=res.hc,cex=1,lwd=0.8, k=4,  k_colors="jco", xlab = " ", ylab = " ", main = " dendogram", horiz = T) #plot 3

###Rectangle fill with border # plot 4
fviz_dend(x=res.hc,cex=0.2,lwd=0.8,k=5, rect=TRUE, rect_border="jco",  k_colors="jco", rect_fill=TRUE, horiz=TRUE)

ggsave(filename = "cluster for thesis pdf.tiff", width = 8, height = 12, device='tiff', dpi=300)

###Rectangle fill with rotted # plot 5
fviz_dend(x=res.hc,cex=0.8,lwd=0.8,k=4, rect=TRUE, rect_border="jco",  k_colors="jco", rect_fill=TRUE, horiz=TRUE)

###Rectangle fill with rotted with theme #plot 6
fviz_dend(res.hc,cex=0.8,lwd=0.8,k=4,rect=TRUE,k_colors="jco",rect_border="jco",rect_fill=TRUE,ggtheme=theme_classic())

### Circular type cluster #plot 7
fviz_dend(res.hc,cex=2,lwd=1,k=4,rect=TRUE, k_colors="jco",rect_border="jco",rect_fill=TRUE, type="circular")

### Phylogenic type cluster #plot 8
fviz_dend(res.hc,cex=0.8,lwd=0.8,k=4,rect=TRUE, k_colors="jco", rect_border="jco", rect_fill=TRUE, type="phylogenic")

remove.packages("igraph")


### Phylogenic type cluster layout.gem #plot 9
fviz_dend(res.hc,cex=0.8,lwd=0.8,k=4,rect=TRUE, k_colors="jco",rect_border="jco", rect_fill=TRUE, type="phylogenic",repel=TRUE,phylo_layout="layout.gem")

### Phylogenic type cluster layout_as_tree #plot 10
fviz_dend(res.hc,cex=0.8,lwd=4,k=4,rect=T, k_colors="lancet",rect_border="jco",rect_fill=TRUE, type="phylogenic",repel=TRUE,phylo_layout="layout_as_tree")


#########b       Modifiaton

# Assuming res.hc is your hierarchical clustering result
fviz_dend(res.hc,
          cex = 0.8,                      # Label size
          lwd = 0.8,                      # Line width
          k = 4,                          # Number of clusters to cut the dendrogram into
          rect = TRUE,                    # Add rectangles around clusters
          rect_border = "white",           # Border color of the rectangle (gray)
          rect_fill = FALSE,              # No fill color for the rectangles
          color_branches = FALSE,         # No coloring for the branches
          color_labels_by_k = T,      # No coloring for the labels
          ggtheme = theme_classic() +     # Apply a classic ggplot2 theme
            theme(text = element_text(family = "Cambria", size = 12))  # Set font to Cambria, size 12
)

# Available ggpubr color palettes: "lancet", "jama", "nejm", "ucscgb", "aaas", "d3", "npg", "locuszoom", "cosmic"

