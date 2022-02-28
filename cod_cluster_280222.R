# PACKAGE
library(readxl)
library(tidyverse)
library(rgl)
library(cluster)
library(mclust)
library(dbscan)
library(fpc)
library(factoextra)

# DATA ####
df = read_excel('DATA.xlsx')
df = df %>% 
  mutate(aa = interaction(ELEVATION, YEAR))
levels(df$aa) = rainbow(nlevels(df$aa))
df1 = df %>% 
  filter(YEAR %in% c(2009, 2010))

# CLUSTER ####

M = df1[,c('DDD', 'ECU_DIAM', 'LON_DIAM')]
Ms = scale(M)
nc = 3


# k-means clustering
set.seed(123)
fviz_nbclust(x = Ms, FUNcluster = kmeans, method = "silhouette",
             k.max = 5, nstart = 50,
             diss = get_dist(Ms, method = "euclidean"))
clus_km <- kmeans(x = Ms, centers = nc)
clus_km


# K-medoids (PAM) clustering
set.seed(123)
fviz_nbclust(x = Ms, FUNcluster = pam, method = "silhouette", k.max = 5,
             diss = dist(Ms, method = "euclidean"))
clus_pam <- pam(x = Ms, k = nc, metric = "euclidean")
clus_pam


# CLARA clustering
set.seed(123)
fviz_nbclust(x = Ms, FUNcluster = clara, method = "silhouette", k.max = 5,
             diss = get_dist(Ms, method = "euclidean"))
clus_clara <- clara(x = Ms, k = nc, metric = "euclidean",
                    stand = TRUE, samples = 50, pamLike = TRUE)
clus_clara


# Hierarchical clustering
set.seed(123)
d <- dist(x = Ms, method = "euclidean")
clus_jer <- hclust(d, method = "complete")
clus_jer$grupo = cutree(clus_jer, k = nc)
clus_jer


# Fuzzy clustering
set.seed(123)
fviz_nbclust(x = Ms, FUNcluster = fanny, method = "silhouette", k.max = 5,
             diss = get_dist(Ms, method = "euclidean"))
clus_fuz = fanny(x = Ms, diss = FALSE, k = nc,
                 metric = "euclidean", stand = FALSE)
clus_fuz


# Model based clustering
set.seed(123)
clus_mod = Mclust(data = Ms, G = 1:10)
summary(clus_mod)


# PLOTS
plot3d(M, col = clus_km$cluster, size = 5)
plot3d(M, col = clus_pam$clustering, size = 5)
plot3d(M, col = clus_clara$clustering, size = 5)
plot3d(M, col = clus_jer$grupo, size = 5)
plot3d(M, col = clus_fuz$clustering, size = 5)
plot3d(M, col = clus_mod$classification, size = 5)

plot(M, col = clus_km$cluster, pch = 16)
plot(M, col = clus_pam$clustering, pch = 16)
plot(M, col = clus_clara$clustering, pch = 16)
plot(M, col = clus_jer$grupo, pch = 16)
plot(M, col = clus_fuz$clustering, pch = 16)
plot(M, col = clus_mod$classification, pch = 16)


# RESUMEN ####
df2 = df1 %>%
  select(YEAR,
         ELEVATION,
         DDD,
         ECU_DIAM,
         LON_DIAM) %>%
  mutate(
    km = clus_km$cluster,
    pam = clus_pam$clustering,
    clara = clus_clara$clustering,
    jer = clus_jer$grupo,
    fuz = clus_fuz$clustering,
    mod = clus_mod$classification
  )

my_range = function(x){
  q = quantile(x, c(0.025, 0.975))
  x1 = x[x>=q[1] & x<=q[2]]
  x2 = range(x1)
  
  return(c(min = x2[1],
           max = x2[2],
           len = length(x1)))
}
my_range(df2$ddd)

est_clus <- function(df, m){
  plot3d(df$DDD,
         df$ECU_DIAM,
         df$LON_DIAM,
         col = factor(df$ELEVATION, labels = 1:3), size = 10)
  print(m)
  f = formula(paste0('DDD~', m))
  print(aggregate(f, data = df, FUN = my_range))
}
colnames(df2)
est_clus(df2, 'km')#3
est_clus(df2, 'pam')#2
est_clus(df2, 'clara')#1
est_clus(df2, 'jer')#1
est_clus(df2, 'fuz')#2
est_clus(df2, 'mod')#3
