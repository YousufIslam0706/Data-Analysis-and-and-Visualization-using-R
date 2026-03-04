#####   Box plot with significance level
###    Code Written by Yousuf Islam


library(ggplot2)
library(ggsignif)
iris
View(iris)
ggplot(iris, aes(x = Species, y = Sepal.Length)) +
  geom_boxplot() + # using `ggsignif` to display comparison of interest
  geom_signif(
    comparisons = list(c("setosa","versicolor"),c("virginica","versicolor"),c("virginica", "setosa")),
    map_signif_level = TRUE
  )

#### Modification

ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) + 
  geom_boxplot() + 
  geom_signif(
    comparisons = list(c("virginica", "setosa")), 
    map_signif_level = TRUE,
    y_position = 8.5 # First layer
  ) + 
  geom_signif(
    comparisons = list(c("setosa", "versicolor")), 
    map_signif_level = TRUE,
    y_position = 7.8 # Second layer
  ) + 
  geom_signif(
    comparisons = list(c("virginica", "versicolor")), 
    map_signif_level = TRUE,
    y_position = 7.8 # Third layer
  ) + 
  scale_fill_brewer(palette = "Pastel1") +  
  theme_minimal()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
)

ggsave("boxplotwithsignificance.png", width = 3.8, height = 4, dpi = 1000)

