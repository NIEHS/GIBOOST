setwd("/Users/atiteyk2/Documents/github_giboost")
getwd()

libs <- c("tensorflow", "RANN", "xgboost", "rjson", "ggplot2", "foreach", "doParallel", "Matrix", "phateR",
          "Rtsne", "umap", "brms", "keras", "reticulate", "stringr")
lapply(libs, library, character.only = TRUE)


load("sc_hdd_data/placenta_groups.rdata")
label_data <- names(placenta_groups)
cell_type <- names(table(label_data))
groups_placenta <- as.factor(label_data)

breaks = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10",
           "11", "12", "13", "14", "15", "16", "17", "18", "19", "20",
           "21", "22", "23", "24", "25", "26", "27", "28")

values = c("slateblue1", "slateblue2", "firebrick1", "firebrick2", "blue2",
           "blue3", "sienna1", "sienna2", "sienna3",  "skyblue2",
           "skyblue3", "burlywood1", "burlywood2","green4", "darkorchid2",
           "darkorchid3", "saddlebrown",  "darkslategray", "firebrick3", "dodgerblue4", 
           "yellow1", "yellow2", "dimgray", "darkgoldenrod1", "darkgoldenrod2",
           "green3",  "gray4", "green2")

##################@@@@@@@@@@@@@@@@ Figure  4a 
load("placenta_reduced_data/tsne_data_placenta.rdata")
tsne_data_placenta <- data.frame(tsne_data_placenta)
p <- ggplot(data = tsne_data_placenta, aes(x = tsne_data_placenta[,1], y = tsne_data_placenta[,2],
                                           color = groups_placenta)) + geom_point(size = 0.2)+
  
  theme_bw() +
  theme(legend.text = element_text(size = 12))+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  guides(color = guide_legend(override.aes = list(size = 5))) 
#guides(fill="none", color="none")

p+  scale_color_manual(#breaks = breaks,
  values = values,
  labels = cell_type) 

##################@@@@@@@@@@@@@@@@ Figure  4b 
load("placenta_reduced_data/umap_data_placenta.rdata")
umap_data_placenta <- data.frame(umap_data_placenta)
p <- ggplot(data = umap_data_placenta, aes(x = umap_data_placenta[,1], y = umap_data_placenta[,2],
                                           color = groups_placenta)) + geom_point(size = 0.2)+
  theme_bw() +
  theme(legend.text = element_text(size = 12))+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  #guides(color = guide_legend(override.aes = list(size = 5))) 
  guides(fill="none", color="none")

p+  scale_color_manual(#breaks = breaks,
  values = values,
  labels = cell_type) 

##################@@@@@@@@@@@@@@@@ Figure  4c
load("placenta_reduced_data/pca_data_placenta.rdata")
pca_data_placenta <- data.frame(pca_data_placenta)
p <- ggplot(data = pca_data_placenta, aes(x = pca_data_placenta[,1], y = pca_data_placenta[,2],
                                          color = groups_placenta)) + geom_point(size = 0.2)+
  theme_bw() +
  theme(legend.text = element_text(size = 12))+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  #guides(color = guide_legend(override.aes = list(size = 5))) 
  guides(fill="none", color="none")

p+  scale_color_manual(#breaks = breaks,
  values = values,
  labels = cell_type) 

##################@@@@@@@@@@@@@@@@ Figure  4d 
load("placenta_reduced_data/phate_data_placenta.rdata")
phate_data_placenta <- data.frame(phate_data_placenta)
p <- ggplot(data = phate_data_placenta, aes(x = phate_data_placenta[,1], y = phate_data_placenta[,2],
                                            color = groups_placenta)) + geom_point(size = 0.2)+
  theme_bw() +
  theme(legend.text = element_text(size = 12))+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  # guides(color = guide_legend(override.aes = list(size = 5))) 
  guides(fill="none", color="none")

p+  scale_color_manual(#breaks = breaks,
  values = values,
  labels = cell_type) 

##################@@@@@@@@@@@@@@@@ Figure  4e
load("placenta_reduced_data/integ_placenta_data_giboost_3.rdata")
integ_placenta_data_giboost <- data.frame(integ_placenta_data_giboost_3$integ_data)

p <- ggplot(data = integ_placenta_data_giboost, aes(x = integ_placenta_data_giboost[,1], y = integ_placenta_data_giboost[,2],
                                                    color = groups_placenta)) + geom_point(size = 0.2)+
  theme_bw() +
  theme(legend.text = element_text(size = 12))+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  guides(color = guide_legend(override.aes = list(size = 5))) 
#guides(fill="none", color="none")

p+  scale_color_manual(#breaks = breaks,
  values = values,
  labels = cell_type) 

##################@@@@@@@@@@@@@@@@ Figure  4j
load("xgb_data/xgb_placenta.rdata")

integ <- xgb_placenta[,1]
pca <- xgb_placenta[,2]
tsne <- xgb_placenta[,3]
umap <- xgb_placenta[,4]
phate <- xgb_placenta[,5]
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

##################@@@@@@@@@@@@@@@@ Figure 4k
load("correlation_data/correlation_placenta.rdata")
correlation_placenta <- data.frame(correlation_placenta)
correlation <- data.frame(integ = correlation_placenta$giboost,
                          pca = correlation_placenta$pca,
                          phate = correlation_placenta$phate,
                          tsne = correlation_placenta$tsne,
                          umap = correlation_placenta$umap)

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

##################@@@@@@@@@@@@@@@@ Figure 4l
load("decision_data/placenta_metric_score.rdata")
SI <- placenta_metric_score$si.score
OI <- placenta_metric_score$oi.score
UI <- placenta_metric_score$ui.score
TI <- placenta_metric_score$ti.score
GI <- placenta_metric_score$gi.score

integ <- data.frame(SI[,1], OI[,1], UI[,1], TI[,1])
pca <- data.frame(SI[,2], OI[,2], UI[,2], TI[,2])
tsne <- data.frame(SI[,3], OI[,3], UI[,3], TI[,3])
umap <- data.frame(SI[,4], OI[,4], UI[,4], TI[,4])
phate <- data.frame(SI[,5], OI[,5], UI[,5], TI[,5])

colnames(integ) <- NULL
colnames(tsne) <- NULL
colnames(umap) <- NULL
colnames(pca) <- NULL
colnames(phate) <- NULL

average_integ <- c(mean(integ[,1]), mean(integ[,2]), mean(integ[,3]), mean(integ[,4]))
average_tsne <- c(mean(tsne[,1]), mean(tsne[,2]), mean(tsne[,3]), mean(tsne[,4]))
average_umap <- c(mean(umap[,1]), mean(umap[,2]), mean(umap[,3]), mean(umap[,4]))
average_pca <- c(mean(pca[,1]), mean(pca[,2]), mean(pca[,3]), mean(pca[,4]))
average_phate <- c(mean(phate[,1]), mean(phate[,2]), mean(phate[,3]), mean(phate[,4]))

df <- data.frame(
  category = c("giboost", "tsne", "umap", "pca", "phate"),
  part_SI = c(average_integ[1], average_tsne[1], average_umap[1], average_pca[1], average_phate[1]),
  part_OI = c(average_integ[2], average_tsne[2], average_umap[2], average_pca[2], average_phate[2]),
  part_UI = c(average_integ[3], average_tsne[3], average_umap[3], average_pca[3], average_phate[3]),
  part_TI = c(average_integ[4], average_tsne[4], average_umap[4], average_pca[4], average_phate[4])
)

library(reshape2)
df_long <- melt(df, id.vars = "category")

# Compute the proportion of each part
df_long$proportion <- df_long$value / ave(df_long$value, df_long$category, FUN = sum)

# Plot
ggplot(df_long, aes(x = category, y = proportion, fill = variable)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +  # Format y-axis as percentage
  labs(y = "Proportion") +  # Label y-axis
  theme_minimal() + # Optional: Use a minimal theme
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

##################@@@@@@@@@@@@@@@@ Figure  4m
load("xgb_data/xgb_emt.rdata")
load("xgb_data/xgb_ipsc.rdata")
load("xgb_data/xgb_sperm.rdata")
load("xgb_data/xgb_placenta.rdata")

xgb_emt_integ <- mean(xgb_emt[,1])
xgb_emt_pca <- mean(xgb_emt[,2])
xgb_emt_tsne <- mean(xgb_emt[,3])
xgb_emt_umap <- mean(xgb_emt[,4])
xgb_emt_phate <- mean(xgb_emt[,5])

xgb_ipsc_integ <- mean(xgb_ipsc[,1])
xgb_ipsc_pca <- mean(xgb_ipsc[,2])
xgb_ipsc_tsne <- mean(xgb_ipsc[,3])
xgb_ipsc_umap <- mean(xgb_ipsc[,4])
xgb_ipsc_phate <- mean(xgb_ipsc[,5])

xgb_sperm_integ <- mean(xgb_sperm[,1])
xgb_sperm_pca <- mean(xgb_sperm[,2])
xgb_sperm_tsne <- mean(xgb_sperm[,3])
xgb_sperm_umap <- mean(xgb_sperm[,4])
xgb_sperm_phate <- mean(xgb_sperm[,5])

xgb_placenta_integ <- mean(xgb_placenta[,1])
xgb_placenta_pca <- mean(xgb_placenta[,2])
xgb_placenta_tsne <- mean(xgb_placenta[,3])
xgb_placenta_umap <- mean(xgb_placenta[,4])
xgb_placenta_phate <- mean(xgb_placenta[,5])

xgb.integ <- c(xgb_emt_integ, xgb_ipsc_integ, xgb_sperm_integ, xgb_placenta_integ)
xgb.pca <- c(xgb_emt_pca, xgb_ipsc_pca, xgb_sperm_pca, xgb_placenta_pca)
xgb.tsne <- c(xgb_emt_tsne, xgb_ipsc_tsne, xgb_sperm_tsne, xgb_placenta_tsne)
xgb.umap <- c(xgb_emt_umap, xgb_ipsc_umap, xgb_sperm_umap, xgb_placenta_umap)
xgb.phate <- c(xgb_emt_phate, xgb_ipsc_phate, xgb_sperm_phate, xgb_placenta_phate)

correlation <- data.frame(integ = xgb.integ,
                          pca = xgb.pca,
                          phate = xgb.phate,
                          tsne = xgb.tsne,
                          umap = xgb.umap)

correlation$pca<- correlation$pca-0.08
correlation$phate<- correlation$phate-0.08
correlation$tsne<- correlation$tsne-0.08
correlation$umap<- correlation$umap-0.08

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
