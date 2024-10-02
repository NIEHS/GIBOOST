
# Boosting Data Interpretation with GIBOOST to Enhance Visualization of High-Dimensional Data

$~~$

**Conventional dimensionality reduction methods (DRM) like t-SNE, UMAP, PCA, and PHATE optimize specific geometric features after data reduction such as separability and cluster sensitivity. However, these methods often fail to preserve both global and local structures, especially when used in isolation.**

**To address these limitations, we introduce GIBOOST, a novel AI tool designed to enhance the visualization and interpretability of high-dimensional single-cell data. GIBOOST integrates optimized information from various dimensionality reduction methods (DRM), ensuring alignment with the cluster sensitivity metric called the Gradient Boosting Classifier Index (GI), which aims to minimize variance and bias in the original data.**

**GIBOOST uses an optimized autoencoder to integrate reduced data from different DRMs, each optimized for distinct features such as cluster sensitivity, spatial relationships, and temporal dependencies.**

**In our application, we used four different datasets to evaluate the effectiveness of GIBOOST: EMT data, iPSC data, spermatogenesis data, and placenta development data. Each dataset presents distinct structures representing different biological phenomena. We also used four different DRM as input for GIBOOST: t-SNE, UMAP, PCA, and PHATE.**

# A quick look to the GIBOOST tool

![](Figure/Figure_2.png)

$~~$

## Steps in the GIBOOST tool for integrating data from different DRMs to optimize the visualization and interpretability of high-dimensional data.

**For example, by integrating Separably clustered information and spatially clustered information, from different methods, we can enhance visualization by providing a combined Separably and spatially clustered information.**

**a/** GIBOOST operates in four steps: First, GIBOOST presents a pool of different methods capable of providing reduced 2D output data from a given high-dimensional dataset

**b/** Second, GIBOOST introduced a metric set for data visualization and interpretability assessment, including the Separability Index (SI), Occupation Index (OI), Uniformity Index (UI), and Time Order Structure Index (TI), each evaluating different features: cluster separation, low-dimensional space coverage, uniform data spread, and spatio-temporal dependency, respectively. Given this set, GIBOOST relies on Bayesian multilevel modeling to identify the features optimized for good visualization by each method, while ensuring alignment with the feature of GI metric to minimize variance and bias.

**c/** Third, GIBOOST evaluates the additive pair scores of features from different methods in the pool and selects the most effective combination that maximizes the additive performance of GI.

**d/** Fourth, GIBOOST uses an optimized Autoencoder (AE) to integrate the two reduced datasets from the top two selected DRM. The goal is to determine the number of neurons and batch size of the AE that minimize bias and variance in the integrated data.


