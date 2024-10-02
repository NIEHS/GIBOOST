setwd("/Users/atiteyk2/Documents/github_giboost")
getwd()

libs <- c("tensorflow", "RANN", "xgboost", "rjson", "ggplot2", "foreach", "doParallel", "Matrix", "phateR",
          "Rtsne", "umap", "brms", "keras", "reticulate", "stringr", "plot3D")
lapply(libs, library, character.only = TRUE)

##################@@@@@@@@@@@@@@@@ Figure  2a
load("GI_metric_data/GI_metric_emt.rdata")
load("GI_metric_data/GI_metric_ipsc.rdata")
load("GI_metric_data/GI_metric_sperm.rdata")

Units <- c(8, 16, 32, 64, 128)
Batch_sizes <- c(32, 48, 64, 96, 128)

persp(Units, batch_sizes, GI_metric_emt,
      main="Perspective Plot of a Cone",
      zlab = "GI score",
      theta = 30, phi = 30, #15,
      col = "darkcyan", shade = 0.4,)

persp(Units, Batch_sizes, GI_metric_ipsc,
      main="Perspective Plot of a Cone",
      zlab = "GI score",
      theta = 30, phi = 30, #15,
      col = "darkcyan", shade = 0.4,)

persp(Units, Batch_sizes, GI_metric_sperm,
      main="Perspective Plot of a Cone",
      zlab = "GI score",
      theta = 30, phi = 30, #15,
      col = "darkcyan", shade = 0.4,)

##################@@@@@@@@@@@@@@@@ Figure  2b-f
###########@@@  emt
load("data/emt_decision_run.rdata")
load("data/groups_emt.rdata")
groups_emt <- as.factor(groups_emt)
breaks = c("1", "2", "3", "4", "5", "6", "7", "8")
values = c("burlywood4", "cadetblue", "chartreuse3", "chocolate", "chocolate4",
           "magenta", "mediumorchid4", "darkblue")
cluster <- c("E2", "E1", "pEMT1", "E3", "pEMT2", "pEMT3", "M", "pMET")

###########@@@  ipsc
load("data/ipsc_decision_run.rdata")
load("data/groups_ipsc.rdata")
groups_ipsc <- as.factor(groups_ipsc)
breaks = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10",
           "11", "12", "13", "14", "15", "16", "17", "18", "19")

values = c("burlywood4", "cadetblue", "chartreuse3", "chocolate", "chocolate4",
           "magenta","cyan", "darkblue", "darkgoldenrod1", "dodgerblue3",
           "firebrick1", "forestgreen", "firebrick4", "gold4", "brown3",
           "lightcoral", "mediumorchid4", "mediumvioletred", "orange4")

###########@@@  sperm
load("data/sperm_decision_run_3.rdata")
load("data/groups_sperm.rdata")
groups_sperm <- as.factor(groups_sperm)
breaks = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10",
           "11", "12", "13", "14", "15", "16", "17", "18", "19", "20",
           "21", "22", "23", "24", "25", "26", "27", "28", "29")

values = c("burlywood4", "firebrick1", "chartreuse3", "chocolate", "chocolate4",
           "magenta", "cyan", "darkblue", "darkgoldenrod1", "dodgerblue3",
           "gray0", "forestgreen", "firebrick4", "darkslategray", "brown3",
           "lightcoral", "mediumorchid4", "mediumvioletred", "orange4", "plum4",
           "purple4", "blue1" , "royalblue4", "yellowgreen", "yellow2",
           "deepskyblue1", "cadetblue","honeydew3" ,"red4")

###########@@@  placenta 
load("data/integ_placenta_data_giboost_3.rdata")
load("data/placenta_groups.rdata")
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

###########@@@ tsne
###########@@@
tsne_data <- data.frame(emt_decision_run$tsne_data)
groups <- groups_emt
tsne_data <- data.frame(ipsc_decision_run$tsne_data)
groups <- groups_ipsc
tsne_data <- data.frame(sperm_decision_run_3$tsne_data)
groups <- groups_sperm
tsne_data <- data.frame(integ_placenta_data_giboost_3$tsne_data)
groups <- groups_placenta

p <- ggplot(data = tsne_data, aes(x = tsne_data[,1], y = tsne_data[,2],
                                  color = groups)) + geom_point(size = 0.2)+
  theme_bw() +
  theme(legend.text = element_text(size = 12))+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  guides(color = guide_legend(override.aes = list(size = 5))) 
#guides(fill="none", color="none")

p + scale_color_manual(breaks = breaks,
                       values = values)

p+  scale_color_manual(values = values,
                       labels = cell_type) 

###########@@@
###########@@@ umap
umap_data <- data.frame(emt_decision_run$umap_data)
groups <- groups_emt
umap_data <- data.frame(ipsc_decision_run$umap_data)
groups <- groups_ipsc
umap_data <- data.frame(sperm_decision_run_3$umap_data)
groups <- groups_sperm
umap_data <- data.frame(integ_placenta_data_giboost_3$umap_data)
groups <- groups_placenta

p <- ggplot(data = umap_data, aes(x = umap_data[,1], y = umap_data[,2],
                                  color = groups)) + geom_point(size = 0.2)+
  theme_bw() +
  theme(legend.text = element_text(size = 12))+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  #guides(color = guide_legend(override.aes = list(size = 5))) 
  guides(fill="none", color="none")

p + scale_color_manual(breaks = breaks,
                       values = values)

p+  scale_color_manual(values = values,
                       labels = cell_type) 

###########@@@
###########@@@ pca
pca_data <- data.frame(emt_decision_run$pca_data)
groups <- groups_emt
pca_data <- data.frame(ipsc_decision_run$pca_data)
groups <- groups_ipsc
pca_data <- data.frame(sperm_decision_run_3$pca_data)
groups <- groups_sperm
pca_data <- data.frame(integ_placenta_data_giboost_3$pca_data)
groups <- groups_placenta

p <- ggplot(data = pca_data, aes(x = pca_data[,1], y = pca_data[,2],
                                 color = groups)) + geom_point(size = 0.2)+
  theme_bw() +
  theme(legend.text = element_text(size = 12))+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  #guides(color = guide_legend(override.aes = list(size = 5))) 
  guides(fill="none", color="none")

p + scale_color_manual(breaks = breaks,
                       values = values)

p+  scale_color_manual(values = values,
                       labels = cell_type) 
###########@@@
###########@@@ phate
phate_data <- data.frame(emt_decision_run$phate_data)
groups <- groups_emt
phate_data <- data.frame(ipsc_decision_run$phate_data)
groups <- groups_ipsc
phate_data <- data.frame(sperm_decision_run_3$phate_data)
groups <- groups_sperm
phate_data <- data.frame(integ_placenta_data_giboost_3$phate_data)
groups <- groups_placenta

p <- ggplot(data = phate_data, aes(x = phate_data[,1], y = phate_data[,2],
                                   color = groups)) + geom_point(size = 0.2)+
  theme_bw() +
  theme(legend.text = element_text(size = 12))+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  # guides(color = guide_legend(override.aes = list(size = 5))) 
  guides(fill="none", color="none")

p + scale_color_manual(breaks = breaks,
                       values = values)

p+  scale_color_manual(values = values,
                       labels = cell_type) 

###########@@@
###########@@@  integ
integ_data <- data.frame(emt_decision_run$integ_data)
groups <- groups_emt
integ_data <- data.frame(ipsc_decision_run$integ_data)
groups <- groups_ipsc
integ_data <- data.frame(sperm_decision_run_3$integ_data)
groups <- groups_sperm
integ_data <- data.frame(integ_placenta_data_giboost_3$integ_data)
groups <- groups_placenta

p <- ggplot(data = integ_data, aes(x = integ_data[,1], y = integ_data[,2],
                                   color = groups)) + geom_point(size = 0.2)+
  theme_bw() +
  theme(legend.text = element_text(size = 12))+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  guides(color = guide_legend(override.aes = list(size = 5))) 
#guides(fill="none", color="none")

p + scale_color_manual(breaks = breaks,
                       values = values)

p+  scale_color_manual(values = values,
                       labels = cell_type) 

