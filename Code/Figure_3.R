setwd("/Users/atiteyk2/Documents/github_giboost")
getwd()

libs <- c("tensorflow", "RANN", "xgboost", "rjson", "ggplot2", "foreach", "doParallel", "Matrix", "phateR",
          "Rtsne", "umap", "brms", "keras", "reticulate", "stringr", "plot3D", "fastcluster", "dendextend")
lapply(libs, library, character.only = TRUE)

##################@@@@@@@@@@@@@@@@ Figure 3a
load("xgb_data/xgb_emt.rdata")
load("xgb_data/xgb_ipsc.rdata")
load("xgb_data/xgb_sperm.rdata")

###########@@@ EMT
integ <- xgb_emt[,1]
pca <- xgb_emt[,2]
tsne <- xgb_emt[,3]
umap <- xgb_emt[,4]
phate <- xgb_emt[,5]
position <- 1:length(integ)
GI_score <- c(integ, pca, tsne, umap, phate)

label <- c(rep("integ", length(integ)), rep("pca", length(pca)), rep("tsne", length(tsne)),
           rep("umap", length(umap)), rep("phate", length(phate)))
label <- data.frame(label)
df <- cbind(GI_score, label)
df <- data.frame(df)
colnames(df) <- c("rate", "label")

res = df %>%
  rstatix::wilcox_test(rate ~ label) %>%
  rstatix::add_significance() %>%
  rstatix::add_xy_position(x = "label", scales = "free_y")

p2 <- ggplot(data = df, aes(x=factor(label), y=rate))  +
  geom_boxplot(mapping = aes(fill=factor(label)), show.legend = FALSE) +
  geom_point(position = position_jitter(width = 0.2), color = "black") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(x = "DRM", y = "GI score") +
  ggpubr::stat_pvalue_manual(res, label = "{p}{p.adj.signif}", hide.ns = FALSE, tip.length = 0.01) +
  theme(axis.text = element_text(size = 18, angle=360), axis.title=element_text(size=20,face="bold"))
p2+scale_fill_manual(values = c("integ" = "firebrick3",
                                "pca" = "chartreuse2",
                                "phate" = "mediumorchid4",
                                "tsne" =  "dodgerblue3",
                                "umap" = "blue1"))

###########@@@ iPSC
integ <- xgb_ipsc[,1]
pca <- xgb_ipsc[,2]
tsne <- xgb_ipsc[,3]
umap <- xgb_ipsc[,4]
phate <- xgb_ipsc[,5]
position <- 1:length(integ)
GI_score <- c(integ, pca, tsne, umap, phate)

label <- c(rep("integ", length(integ)), rep("pca", length(pca)), rep("tsne", length(tsne)),
           rep("umap", length(umap)), rep("phate", length(phate)))
label <- data.frame(label)
df <- cbind(GI_score, label)
df <- data.frame(df)
colnames(df) <- c("rate", "label")

res = df %>%
  rstatix::wilcox_test(rate ~ label) %>%
  rstatix::add_significance() %>%
  rstatix::add_xy_position(x = "label", scales = "free_y")

p2 <- ggplot(data = df, aes(x=factor(label), y=rate))  +
  geom_boxplot(mapping = aes(fill=factor(label)), show.legend = FALSE) +
  geom_point(position = position_jitter(width = 0.2), color = "black") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(x = "DRM", y = "GI score") +
  ggpubr::stat_pvalue_manual(res, label = "{p}{p.adj.signif}", hide.ns = FALSE, tip.length = 0.01) +
  theme(axis.text = element_text(size = 18, angle=360), axis.title=element_text(size=20,face="bold"))
p2+scale_fill_manual(values = c("integ" = "firebrick3",
                                "pca" = "chartreuse2",
                                "phate" = "mediumorchid4",
                                "tsne" =  "dodgerblue3",
                                "umap" = "blue1"))

###########@@@ SPERM
integ <- xgb_sperm[,1]
pca <- xgb_sperm[,2]
tsne <- xgb_sperm[,3]
umap <- xgb_sperm[,4]
phate <- xgb_sperm[,5]
position <- 1:length(integ)
GI_score <- c(integ, pca, tsne, umap, phate)

label <- c(rep("integ", length(integ)), rep("pca", length(pca)), rep("tsne", length(tsne)),
           rep("umap", length(umap)), rep("phate", length(phate)))
label <- data.frame(label)
df <- cbind(GI_score, label)
df <- data.frame(df)
colnames(df) <- c("rate", "label")

res = df %>%
  rstatix::wilcox_test(rate ~ label) %>%
  rstatix::add_significance() %>%
  rstatix::add_xy_position(x = "label", scales = "free_y")

p2 <- ggplot(data = df, aes(x=factor(label), y=rate))  +
  geom_boxplot(mapping = aes(fill=factor(label)), show.legend = FALSE) +
  geom_point(position = position_jitter(width = 0.2), color = "black") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(x = "DRM", y = "GI score") +
  ggpubr::stat_pvalue_manual(res, label = "{p}{p.adj.signif}", hide.ns = FALSE, tip.length = 0.01) +
  theme(axis.text = element_text(size = 18, angle=360), axis.title=element_text(size=20,face="bold"))
p2+scale_fill_manual(values = c("integ" = "firebrick3",
                                "pca" = "chartreuse2",
                                "phate" = "mediumorchid4",
                                "tsne" =  "dodgerblue3",
                                "umap" = "blue1"))



##################@@@@@@@@@@@@@@@@ figure b
load("decision_data/emt_data_SL.rdata")
load("decision_data/groups_emt.rdata")
load("decision_data/emt_decision_run.rdata")

load("decision_data/ipsc_data_SL.rdata")
load("decision_data/groups_ipsc.rdata")
load("decision_data/ipsc_decision_run.rdata")

load("decision_data/sperm_data_SL.rdata")
load("decision_data/groups_sperm.rdata")
load("decision_data/sperm_decision_run_3.rdata")


###########@@@ compute dendrogram using the original data
set.seed(123)

d <- dist(sperm_data_SL)
hc <- hclust(d)
num_clusters <- 100
cluster_labels <- cutree(hc, k = num_clusters)
sperm_data_numeric <- sperm_data_SL[, sapply(sperm_data_SL, is.numeric)]
centroids <- tapply(sperm_data_numeric, cluster_labels, function(x) colMeans(x, na.rm = TRUE))
centroids <- do.call(rbind, centroids)
hc_sperm <- hclust(dist(centroids))
dendogram_sperm <- as.dendrogram(hc_sperm)
desired_leaves <- 2
pruned_dendrogram_sperm <- prune(dendogram_sperm, leaves = desired_leaves)
save(pruned_dendrogram_sperm, file="pruned_dendrogram_sperm.rdata")
save(dendogram_sperm, file = "dendogram_sperm.rdata")

load("dendogram_sperm.rdata")
load("pruned_dendrogram_sperm.rdata")

###########@@@ compute dendrogram using tsne 2D reduced data
d_tsne <- dist(sperm_decision_run_3$tsne_data)
hc_tsne <- hclust(d_tsne)
num_clusters <- 100
cluster_labels_tsne <- cutree(hc_tsne, k = num_clusters)
centroids_tsne <- sapply(split(sperm_decision_run_3$tsne_data, cluster_labels_tsne), function(x) colMeans(matrix(x, ncol = 2)))
centroids_tsne <- t(centroids_tsne)
hc_tsne <- hclust(dist(centroids_tsne))
dendogram_tsne <- as.dendrogram(hc_tsne)
desired_leaves <- 2
pruned_dendrogram_tsne <- prune(dendogram_tsne, leaves = desired_leaves)

coph_dist_sperm <- cophenetic(pruned_dendrogram_sperm) # Calculate the cophenetic distances
coph_dist_tsne <- cophenetic(pruned_dendrogram_tsne) # Calculate the correlation between cophenetic distances

c_tsne <- round(cor(coph_dist_sperm, coph_dist_tsne), 2)

labels(dendogram_sperm) <- as.character(labels(dendogram_sperm))
labels(dendogram_tsne) <- as.character(labels(dendogram_tsne))
dend_list <- dendlist(dendogram_sperm, dendogram_tsne)
dendlist(dendogram_sperm, dendogram_tsne) %>%
  untangle(method="step1side") %>%
  tanglegram()

dendlist(dendogram_sperm, dendogram_tsne) %>%
  untangle(method="step1side") %>%
  entanglement()

###########@@@ compute dendrogram using umap 2D reduced data
d_umap <- dist(sperm_decision_run_3$umap_data)
hc_umap <- hclust(d_umap)
num_clusters <- 100
cluster_labels_umap <- cutree(hc_umap, k = num_clusters)
centroids_umap <- sapply(split(sperm_decision_run_3$umap_data, cluster_labels_umap), function(x) colMeans(matrix(x, ncol = 2)))
centroids_umap <- t(centroids_umap)
hc_umap <- hclust(dist(centroids_umap))
dendogram_umap <- as.dendrogram(hc_umap)
desired_leaves <- 2
pruned_dendrogram_umap <- prune(dendogram_umap, leaves = desired_leaves)

coph_dist_sperm <- cophenetic(pruned_dendrogram_sperm) # Calculate the cophenetic distances
coph_dist_umap <- cophenetic(pruned_dendrogram_umap) # Calculate the correlation between cophenetic distances

c_umap <- round(cor(coph_dist_sperm, coph_dist_umap), 2)

labels(dendogram_sperm) <- as.character(labels(dendogram_sperm))
labels(dendogram_umap) <- as.character(labels(dendogram_umap))
dend_list <- dendlist(dendogram_sperm, dendogram_umap)
dendlist(dendogram_sperm, dendogram_umap) %>%
  untangle(method="step1side") %>%
  tanglegram()

dendlist(dendogram_sperm, dendogram_umap) %>%
  untangle(method="step1side") %>%
  entanglement()

###########@@@ compute dendrogram using pca 2D reduced data
d_pca <- dist(sperm_decision_run_3$pca_data)
hc_pca <- hclust(d_pca)
num_clusters <- 100
cluster_labels_pca <- cutree(hc_pca, k = num_clusters)
centroids_pca <- sapply(split(sperm_decision_run_3$pca_data, cluster_labels_pca), function(x) colMeans(matrix(x, ncol = 2)))
centroids_pca <- t(centroids_pca)
hc_pca <- hclust(dist(centroids_pca))
dendogram_pca <- as.dendrogram(hc_pca)
desired_leaves <- 2
pruned_dendrogram_pca <- prune(dendogram_pca, leaves = desired_leaves)

coph_dist_sperm <- cophenetic(pruned_dendrogram_sperm) # Calculate the cophenetic distances
coph_dist_pca <- cophenetic(pruned_dendrogram_pca) # Calculate the correlation between cophenetic distances

c_pca <- round(cor(coph_dist_sperm, coph_dist_pca), 2)

labels(dendogram_sperm) <- as.character(labels(dendogram_sperm))
labels(dendogram_pca) <- as.character(labels(dendogram_pca))
dend_list <- dendlist(dendogram_sperm, dendogram_pca)
dendlist(dendogram_sperm, dendogram_pca) %>%
  untangle(method="step1side") %>%
  tanglegram()

dendlist(dendogram_sperm, dendogram_pca) %>%
  untangle(method="step1side") %>%
  entanglement()

###########@@@ compute dendrogram using phate 2D reduced data
d_phate <- dist(sperm_decision_run_3$phate_data)
hc_phate <- hclust(d_phate)
num_clusters <- 100
cluster_labels_phate <- cutree(hc_phate, k = num_clusters)
centroids_phate <- sapply(split(sperm_decision_run_3$phate_data, cluster_labels_phate), function(x) colMeans(matrix(x, ncol = 2)))
centroids_phate <- t(centroids_phate)
hc_phate <- hclust(dist(centroids_phate))
dendogram_phate <- as.dendrogram(hc_phate)
desired_leaves <- 2
pruned_dendrogram_phate <- prune(dendogram_phate, leaves = desired_leaves)

coph_dist_sperm <- cophenetic(pruned_dendrogram_sperm) # Calculate the cophenetic distances
coph_dist_phate <- cophenetic(pruned_dendrogram_phate) # Calculate the correlation between cophenetic distances

c_phate <- round(cor(coph_dist_sperm, coph_dist_phate), 2)

labels(dendogram_sperm) <- as.character(labels(dendogram_sperm))
labels(dendogram_phate) <- as.character(labels(dendogram_phate))
dend_list <- dendlist(dendogram_sperm, dendogram_phate)
dendlist(dendogram_sperm, dendogram_phate) %>%
  untangle(method="step1side") %>%
  tanglegram()

dendlist(dendogram_sperm, dendogram_phate) %>%
  untangle(method="step1side") %>%
  entanglement()


###########@@@ compute dendrogram using giboost 2D reduced data
integ_data <- sperm_decision_run_3$integ_data
#integ_data <- matrix(integ_data, ncol = 2, byrow = TRUE)
#integ_data[is.na(integ_data)] <- 0
d_integ <- dist(integ_data)
hc_integ <- hclust(d_integ)
num_clusters <- 100
cluster_labels_integ <- cutree(hc_integ, k = num_clusters)
integ_data_numeric <- integ_data[, sapply(integ_data, is.numeric)]
centroids_integ <- tapply(integ_data_numeric, cluster_labels_integ, function(x) colMeans(x, na.rm = TRUE))
centroids_integ <- do.call(rbind, centroids_integ)
hc_integ <- hclust(dist(centroids_integ))
dendogram_integ <- as.dendrogram(hc_integ)
desired_leaves <- 4
pruned_dendrogram_integ <- prune(dendogram_integ, leaves = desired_leaves)

coph_dist_sperm <- cophenetic(pruned_dendrogram_sperm) # Calculate the cophenetic distances
coph_dist_integ <- cophenetic(pruned_dendrogram_integ) # Calculate the correlation between cophenetic distances

c_integ <- round(cor(coph_dist_sperm, coph_dist_integ), 2)

labels(dendogram_sperm) <- as.character(labels(dendogram_sperm))
labels(dendogram_integ) <- as.character(labels(dendogram_integ))
dend_list <- dendlist(dendogram_sperm, dendogram_integ)
dendlist(dendogram_sperm, dendogram_integ) %>%
  untangle(method="step1side") %>%
  tanglegram()

dendlist(dendogram_sperm, dendogram_integ) %>%
  untangle(method="step1side") %>%
  entanglement()

###########@@@ Calculate the correlation between cophenetic distances using 3 different number of clusters
####### EMT
load("correlation_data/correlation_emt.rdata")
correlation_emt <- data.frame(correlation_emt)
correlation <- data.frame(integ = correlation_emt$giboost,
                          pca = correlation_emt$pca,
                          phate = correlation_emt$phate,
                          tsne = correlation_emt$tsne,
                          umap = correlation_emt$umap)

correlation.summary <- data.frame(RGP=apply(correlation, 2, mean),
                                  standard_deviation=apply(correlation, 2, sd),
                                  Method=colnames(correlation))

ggplot(correlation.summary, aes(x=Method, y=RGP, fill=Method)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("integ" = "firebrick3",
                               "pca" = "chartreuse2",
                               "phate" = "mediumorchid4",
                               "tsne" =  "dodgerblue3",
                               "umap" = "blue1")) +
  geom_errorbar(aes(ymin=RGP-standard_deviation, ymax=RGP+standard_deviation), colour="black", width=.1) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

####### iPSC
load("correlation_data/correlation_ipsc.rdata")
correlation_ipsc <- data.frame(correlation_ipsc)
correlation <- data.frame(integ = correlation_ipsc$giboost,
                          pca = correlation_ipsc$pca,
                          phate = correlation_ipsc$phate,
                          tsne = correlation_ipsc$tsne,
                          umap = correlation_ipsc$umap)

correlation.summary <- data.frame(RGP=apply(correlation, 2, mean),
                                  standard_deviation=apply(correlation, 2, sd),
                                  Method=colnames(correlation))

ggplot(correlation.summary, aes(x=Method, y=RGP, fill=Method)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("integ" = "firebrick3",
                               "pca" = "chartreuse2",
                               "phate" = "mediumorchid4",
                               "tsne" =  "dodgerblue3",
                               "umap" = "blue1")) +
  geom_errorbar(aes(ymin=RGP-standard_deviation, ymax=RGP+standard_deviation), colour="black", width=.1) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

####### SPERM
load("correlation_data/correlation_sperm.rdata")
correlation_sperm <- data.frame(correlation_sperm)
correlation <- data.frame(integ = correlation_sperm$giboost,
                          pca = correlation_sperm$pca,
                          phate = correlation_sperm$phate,
                          tsne = correlation_sperm$tsne,
                          umap = correlation_sperm$umap)

correlation.summary <- data.frame(RGP=apply(correlation, 2, mean),
                                  standard_deviation=apply(correlation, 2, sd),
                                  Method=colnames(correlation))

ggplot(correlation.summary, aes(x=Method, y=RGP, fill=Method)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("integ" = "firebrick3",
                               "pca" = "chartreuse2",
                               "phate" = "mediumorchid4",
                               "tsne" =  "dodgerblue3",
                               "umap" = "blue1")) +
  geom_errorbar(aes(ymin=RGP-standard_deviation, ymax=RGP+standard_deviation), colour="black", width=.1) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

##################@@@@@@@@@@@@@@@@ figure i
load("decision_data/ipsc_decision_run.rdata")
load("decision_data/groups_ipsc.rdata")

breaks = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10",
           "11", "12", "13", "14", "15", "16", "17", "18", "19")

values = c("saddlebrown", "sienna3", "sienna3", "sienna3", "sienna3",
           "sienna3","sienna3", "sienna3", "sienna3","sienna3",
           "sienna3", "sienna3", "sienna3", "sienna3", "sienna3",
           "sienna3", "tan1", "sienna3", "sienna3")

integ.data <- ipsc_decision_run$integ_data
integ.data <- data.frame(integ.data)

p <- ggplot(data = integ.data, aes(x = integ.data[,1], y = integ.data[,2],
                                   color = groups_ipsc)) + geom_point(size = 0.7)+
  theme_bw() +
  theme(legend.position = "none") +
  theme(legend.text = element_text(size = 12))+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  guides(color = guide_legend(override.aes = list(size = 5)))

p + scale_color_manual(breaks = breaks,
                       values = values)#,

##################@@@@@@@@@@@@@@@@ figure j
load("decision_data/timeclust_spermdata.rdata")
load("decision_data/sperm_decision_run_3.rdata")
load("decision_data/groups_sperm.rdata")

days=c(5,5.05,25,15,35,30,20,10,67,80.05,80,14,18,18.05,25.05,30.05,
       6,8,0,0.5,3,3.05,6.05,7,8.05)
names(timeclust) <- days
timeclust1=timeclust
for ( i in 1:25) {
  id=which(timeclust==i)
  
  timeclust1[id]=days[i]
  print(length(timeclust1[id]))
}


new_label <- as.factor(timeclust1)

values = c("tan1", "tan1","tan1","tan1","tan1",
           "tan1", "tan1","tan1","sienna1","sienna1",
           "sienna1", "sienna1", "sienna1", "sienna1", "sienna1",
           "sienna1", "sienna1", "sienna1", "sienna1", "sienna3",
           "sienna3", "saddlebrown","saddlebrown","saddlebrown","saddlebrown")

integ.data <- sperm_decision_run_3$integ_data
integ.data <- data.frame(integ.data)

p <- ggplot(data = integ.data, aes(x = integ.data[,1], y = integ.data[,2],
                                   color = new_label)) + geom_point(size = 0.7)+ # groups_sperm
  theme_bw() +
  #theme(legend.position = "none") +
  theme(legend.text = element_text(size = 12))+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  guides(color = guide_legend(override.aes = list(size = 5)))

p + scale_color_manual(values = values)
