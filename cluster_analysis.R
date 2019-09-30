df <- readRDS("Data/Headwaters_DO/all_wq_data_summary") %>%
  left_join(read_xlsx("Data/Headwaters_DO/regression_data.xlsx")) %>%
  mutate_if(is.numeric, list(~na_if(., Inf))) %>% 
  mutate_if(is.numeric, list(~na_if(., -Inf))) %>%
  select_if(~ !any(is.na(.))) %>% 
  left_join(readRDS("Data/Headwaters_DO/DO_summary") %>%
              rename(site = Site)) %>%
  drop_na() 

df_s <- scale(dplyr::select(df, -site)) %>%
  set_rownames(df$site)

library("factoextra")
fviz_nbclust(df_s, kmeans,
             method = "gap_stat")

km.res <- kmeans(df_s, 2, nstart = 25)

# Visualize
fviz_cluster(km.res, data = df_s, 
             ellipse.type = "convex",
             palette = "jco",
             repel = TRUE,
             ggtheme = theme_minimal())


res.hc <- hclust(dist(df_s),  method = "ward.D2")
fviz_dend(res.hc, cex = 0.5, k = 4, palette = "jco") 


# Compute hierarchical k-means clustering
res.hk <-hkmeans(df_s, 5)
# Elements returned by hkmeans()
names(res.hk)
# Print the results
res.hk
# Visualize the tree
fviz_dend(res.hk, cex = 0.6, palette = "jco", 
          rect = TRUE, rect_border = "jco", rect_fill = TRUE)

# Visualize the hkmeans final clusters
fviz_cluster(res.hk, palette = "jco", repel = TRUE,
             ggtheme = theme_classic())

library(mclust)
mc <- Mclust(df_s)            # Model-based-clustering
summary(mc)  
mc$modelName                # Optimal selected model ==> "VVV"
mc$G                        # Optimal number of cluster => 3
head(mc$z, 30)              # Probality to belong to a given cluster
head(mc$classification, 30) # Cluster assignement of each observation

# BIC values used for choosing the number of clusters
fviz_mclust(mc, "BIC", palette = "jco")
# Classification: plot showing the clustering
fviz_mclust(mc, "classification", geom = "point", 
            pointsize = 1.5, palette = "jco")
# Classification uncertainty
fviz_mclust(mc, "uncertainty", palette = "jco")
