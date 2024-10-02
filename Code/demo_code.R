setwd("/Users/atiteyk2/Documents/github_giboost")
getwd()

libs <- c("tensorflow", "RANN", "xgboost", "rjson", "ggplot2", "foreach", "doParallel", "Matrix", "phateR",
          "Rtsne", "umap", "brms", "keras", "reticulate", "stringr")
lapply(libs, library, character.only = TRUE)

load("demo_data.rdata")
load("demo_label.rdata")
normalized_sc_data <- data.frame(demo_data)
label_vector <- factor(demo_label)  # Ensure it's a factor
desired_length <- dim(demo_data)[1]

source("giboost.R")

decision <- giboost(normalized_sc_data, label_vector, desired_length)

tsne_data <- data.frame(decision$tsne_data)   
ggplot(data = tsne_data, aes(x = tsne_data[,1], y = tsne_data[,2], color = demo_label)) + geom_point()
umap_data <- data.frame(decision$umap_data)
ggplot(data = umap_data, aes(x = umap_data[,1], y = umap_data[,2], color = demo_label)) + geom_point()
pca_data <- data.frame(decision$pca_data)
ggplot(data = pca_data, aes(x = pca_data[,1], y = pca_data[,2], color = demo_label)) + geom_point()
phate_data <- data.frame(decision$phate_data)
ggplot(data = phate_data, aes(x = phate_data[,1], y = phate_data[,2], color = demo_label)) + geom_point()
giboost_data <- data.frame(decision$integ_data)
ggplot(data = giboost_data, aes(x = giboost_data[,1], y = giboost_data[,2], color = demo_label)) + geom_point()