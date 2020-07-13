#https://towardsdatascience.com/hierarchical-clustering-on-categorical-data-in-r-a27e578f2995
#https://www.statisticalinnovations.com/wp-content/uploads/Haugton2009.pdf
#https://www.datanovia.com/en/lessons/k-medoids-in-r-algorithm-and-practical-examples/
#http://www.bigdatanalysis.com/how-to-do-successfully-customer-segmentation/

library(tidyverse)
library(cluster)
library(factoextra)
library(lubridate)

responses <- read_csv("survey_responses_1st_survey.csv") # Read in data
responses <- responses %>% # remove duplicate responses, keep their first response
  mutate(date = ymd_hms(date)) %>%
  arrange(date) %>%
  group_by(id) %>%
  mutate(row_num = row_number()) %>%
  ungroup() %>%
  filter(row_num == 1) %>%
  select(-row_num)


# KMeans ------------------------------------------------------------------

# Change all variables to binary responses
seg_data <- responses %>%
  mutate(Delicious = if_else(`Delicious vs Healthy` == "I care more about the food being delicious than healthy", 1, 0),
         Traditional = if_else(`Traditional Flavors vs Exotic` == "I prefer simple, traditional or classic flavors",1,0),
         Deal_Driven = if_else(`Deal Driven vs Regular Menu` == "Promos, deals, discounts and coupons help me decide",1,0),
         Shop_Loyal = if_else(`Shop Loyalty vs Agnostic` == "I have favorite soda shops - some are way better than others",1,0)) %>%
  dplyr::select(Delicious, Traditional, Deal_Driven, Shop_Loyal) %>%
  as.data.frame()
rownames(seg_data) <- responses$id
seg_data

# Run KMeans for 2 to 4 segment options
k2 <- kmeans(seg_data, centers = 2, nstart = 25)
k3 <- kmeans(seg_data, centers = 3, nstart = 25)
k4 <- kmeans(seg_data, centers = 4, nstart = 25)
k5 <- kmeans(seg_data, centers = 5, nstart = 25)

p1 <- fviz_cluster(k2, geom = "point", data = seg_data) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = seg_data) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = seg_data) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = seg_data) + ggtitle("k = 5")

library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)

# Determining Optimal Cluster
fviz_nbclust(seg_data, kmeans, method = "wss")
fviz_nbclust(seg_data, kmeans, method = "silhouette")
gap_stat <- clusGap(seg_data, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
fviz_gap_stat(gap_stat)
fviz_cluster(k4, data = seg_data)

seg_data$cluster <- k3$cluster

seg_data %>%
  count(cluster, Delicious)

write_csv(responses, "cluster.csv")

# The algorithm is choosing 2 or 3 variables to cluster off of and I'm getting 100%'s in each

# KMediods ----------------------------------------------------------------
k2 <- pam(seg_data, 2)
k3 <- pam(seg_data, 3)
k4 <- pam(seg_data, 4)
k5 <- pam(seg_data, 5)

p1 <- fviz_cluster(k2, geom = "point", data = seg_data) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = seg_data) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = seg_data) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = seg_data) + ggtitle("k = 5")

library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)

fviz_nbclust(seg_data, cluster::pam, method = "wss")
fviz_nbclust(seg_data, cluster::pam, method = "silhouette")

responses$cluster <- k3$cluster

write_csv(responses, "cluster.csv")

# Same thing, just splitting on some variables


# Latent Class Analysis ---------------------------------------------------

# Use MCLUST next time
library(poLCA)

seg_data <- seg_data %>%
  mutate_all(vars(. + 1))
f <- cbind(Delicious, Traditional, Deal_Driven, Shop_Loyal) ~ 1
gss.lc2 <- poLCA(f, seg_data, nclass=4)

gss.lc2
gss.lc2$predcell
gss.lc2$probs
gss.lc2$posterior %>%
  as_tibble() %>%
  mutate(class = ifelse(max(V1,V2) == V1, 1, 2))

poLCA.table(formula=Delicious~1,
            condition=list(Traditional=1,Deal_Driven=1,Shop_Loyal=2),
            lc=gss.lc2)


poLCA.predcell(lc=gss.lc2,y=c(1,1,1,1))

for (i in 2:6) {
  max_ll <- -100000
  min_bic <- 100000
  for (j in 1:10) {
    res <- poLCA(f, seg_data, nclass = i)
    if (res$bic < min_bic) {
      min_bic <- res$bic
      LCA_best_model <- res
    }
  }
}

LCA_best_model

k2 <- poLCA(f, seg_data, nclass = 2)
k3 <- poLCA(f, seg_data, nclass = 3)
k4 <- poLCA(f, seg_data, nclass = 4)
k5 <- poLCA(f, seg_data, nclass = 5)


k3
write_csv(k3$predcell, "predcell.csv")

classes <- k3$posterior %>%
  as_tibble() %>%
  mutate(id = responses$id) %>%
  pivot_longer(V1:V3, names_to = "class", values_to = "prob") %>%
  group_by(id) %>%
  filter(prob == max(prob)) %>%
  ungroup() %>%
  dplyr::select(-prob)

responses <- responses %>%
 left_join(classes)

write_csv(responses, "cluster.csv")


# Financials --------------------------------------------------------------

new_data <- read_csv("cluster.csv")

new_data <- new_data %>%
  mutate(monetary = Revenue / Frequency)

new_data %>%
  ggplot() + 
  geom_density(aes(x = Recency, fill = class, color = class),
               alpha = .3)

new_data %>%
  ggplot() + 
  geom_boxplot(aes(y = Recency, fill = class, color = class),
               alpha = .3) +
  theme_light()

new_data %>%
  ggplot() + 
  geom_boxplot(aes(y = Frequency, fill = class, color = class),
               alpha = .3) +
  theme_light()

new_data %>%
  ggplot() + 
  geom_boxplot(aes(y = Revenue, fill = class, color = class),
               alpha = .3) +
  theme_light()

new_data %>%
  group_by(class) %>%
  summarise(med_r = median(Recency, na.rm = TRUE),
            med_f = median(Frequency, na.rm = TRUE),
            med_m = median(monetary, na.rm = TRUE))
