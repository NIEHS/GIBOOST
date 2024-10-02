
giboost <- function(normalized_sc_data, label_vector, desired_length){
  
  
  ##############################################################################
  ############################################################################## function for metrics
  ##############################################################################
  
  drm_metrics <- function(reduced_scdata, label_vector, desired_length) {
    
    ########
    ######## functions to reduce size of the data
    
    reduced_size_atitey <- function(reduced_data, label_vector, desired_length) {
      data <- cbind(reduced_data, label_vector)
      colnames(data) <- NULL
      total_labels <- length(levels(label_vector))
      
      reduced_factor <- as.numeric(length(label_vector))/desired_length
      
      label_number <- 1:total_labels
      count.reduced <- c()
      for (value in label_number) {
        count.reduced[value] <- floor(sum(label_vector == value)/reduced_factor)
      }
      
      limit_vec <- count.reduced
      column_index <- 3 
      rows_to_keep <- integer(0)
      
      # Use a for loop to check total number of different numerical values
      for (i in seq_along(unique(data[, column_index]))) {
        value <- unique(data[, column_index])[i]
        indices <- which(data[, column_index] == value)
        limit_total <- limit_vec[i]  # Get the limit for the current value
        rows_to_keep <- c(rows_to_keep, head(indices, limit_total))
      }
      dataset_reduced <- data[rows_to_keep, , drop = FALSE]
      dataset_reduced <- data.frame(dataset_reduced)
      
      return(dataset_reduced)
    }
    
    
    sentence0 <- "Begin!" # @@@@@@@@@@@@
    print(sentence0) # @@@@@@@@@@@@
    ########################################
    ######################################## first part run
    ########################################
    
    data.reduced <- reduced_size_atitey(reduced_scdata, label_vector, desired_length)
    reduced_data <- data.reduced[,1:2]
    label_vector <- data.reduced[,3]########
    
    sentence2 <- "run GI" # @@@@@@@@@@@@
    print(sentence2) # @@@@@@@@@@@@
    
    xgb_index <- function(dataset, groups, k){
      test <- xgboost(as.matrix(dataset), as.numeric(groups) - 1, objective = "multi:softmax", num_class=length(table(groups)), nrounds = k, max.depth=3, subsample=0.8)
      return(mean((as.numeric(groups) - 1) == predict(test, as.matrix(dataset))))
    }
    
    ###############
    ############### xgb index
    num_cores <- 4 # Number of cores for parallel processing
    registerDoParallel(cores = num_cores) # Register parallel backend
    k_values <- 1:250 # Values of k for which you want to parallelize the function
    
    GI <- foreach(k = k_values, .combine = c) %dopar% { # Parallelize xgb_index for different values of k
      xgb_index(reduced_data, label_vector, k)
    }
    
    sentence3 <- "run SI" # @@@@@@@@@@@@
    print(sentence3) # @@@@@@@@@@@@
    
    ###############
    ############### Separability index
    
    similarity_index <- function(dataset, groups, k=1){
      knn1 = nn2(dataset, dataset, k=k+1)
      a = matrix(1, length(groups), 1)
      for (j in 1:k){
        a = a * (groups[knn1$nn.idx[, 1]] == groups[knn1$nn.idx[, 1+j]])
      }
      return(mean(a))
    }
    
    neighbour_number <- 1:250  # number of nearest neighbour
    neighborhood_sizes <- neighbour_number
    
    SI <- numeric(length = length(neighbour_number))
    for (i in seq_along(neighbour_number)) {
      SI[i] <- similarity_index(reduced_data, label_vector, k=i)
    }
    
    sentence4 <- "run OI" # @@@@@@@@@@@@
    print(sentence4) # @@@@@@@@@@@@
    
    ###############
    ############### occupation index
    coverage_index <- function(dataset, num_grid){ # num_grid=100
      scaled_dataset <- scale(dataset)
      result <- matrix(0, num_grid, num_grid)
      
      for (i in 1:nrow(dataset)){
        a = as.integer(0.33 * num_grid * scaled_dataset[i, 1:2] + 0.5 * num_grid)
        if (sum((a > 0)*(a <= num_grid))==2){
          result[a[1], a[2]] = result[a[1], a[2]] + 1
        }
      }
      
      result = mean(result > 0)
      return(result)
      
    }
    
    grid_number <- 1:250 # number of grid
    
    OI <- numeric(length = length(grid_number))
    for (u in seq_along(grid_number)) {
      OI[u] <- coverage_index(reduced_data, num_grid=u)
    }
    
    sentence5 <- "run UI" # @@@@@@@@@@@@
    print(sentence5) # @@@@@@@@@@@@
    
    ###############
    ############### uniformity index
    spread_index <- function(dataset, num_grid){  # num_grid=10
      scaled_dataset <- scale(dataset)
      result <- matrix(0, num_grid, num_grid)
      count <- 0
      
      for (i in 1:nrow(dataset)){
        a = as.integer(0.33 * num_grid * scaled_dataset[i, 1:2] + 0.5 * num_grid)
        if (sum((a > 0)*(a <= num_grid))==2){
          count = count + 1
          result[a[1], a[2]] = result[a[1], a[2]] + 1
        }
      }
      result = sum((((result - (count / num_grid / num_grid))**2) / (count / num_grid / num_grid)))
      
      pval = pchisq(result, df=(num_grid * num_grid)-1, lower.tail=FALSE)
      
      result = result / nrow(dataset) # David Addition
      return(result)  # David Addition
    }
    
    grid_spread <- 1:250
    L <- length(grid_spread)
    
    UI <- numeric(length = length(grid_spread))
    for (v in seq_along(grid_spread)) {
      UI[v] <- spread_index(reduced_data, num_grid=v)
      UI[v] <- UI[v] / L
    }
    UI[1] <- 0
    UI <- rep(1,250) - UI
    
    sentence6 <- "run TI" # @@@@@@@@@@@@
    print(sentence6) # @@@@@@@@@@@@
    
    ###############
    ############### TI index
    
    TimeOrderStructureLearning <- function(reduced_data, label_vector){
      
      ########
      ######## functions to create W x W matrix
      split_matrix_atitey <- function(dataset) {
        limit <- dim(data.frame(dataset))[1]-1
        dataset <- abs(dataset) / rowSums(abs(dataset))
        matrix_square <- lapply(1:limit, function(i) {rbind(dataset[i,], dataset[i+1,])})
        matrix_square <- unname(matrix_square)
        return(matrix_square)
      }
      
      run_chain_atitey <- function(square_matrix, steps) {
        first.step <- lapply(1:nrow(square_matrix), function(i){sample(nrow(square_matrix), 1, pr=square_matrix[i,])})
        second.step <- lapply(steps, function(first.step){10*rnorm(first.step)}) #
        second.step <- data.frame(second.step)
        second.step <- abs(second.step)
        second.step <- floor(second.step)
        colnames(second.step) <- "New_Name1"
        return(second.step)
      }
      
      markov_process_atitey <- function(dataset, steps) {
        transition_matrix <- split_matrix_atitey(dataset)
        run_chain <- lapply(1:limit, function(i) {run_chain_atitey(transition_matrix[[i]], steps) })
        run_chain <- data.frame(run_chain)
        colnames(run_chain) <- NULL
        return(run_chain)
      }
      
      ########################################
      ######################################## first part run: WxW matrix
      ########################################
      
      r.data <- data.frame(reduced_data)
      rm(reduced_data)
      limit <- dim(r.data)[1]-1
      data_kodjo_phate <- markov_process_atitey(r.data, limit)
      
      
      rm(split_matrix_atitey)
      rm(run_chain_atitey)
      rm(markov_process_atitey)
      rm(r.data)
      #
      groups <- as.numeric(label_vector)
      groups <- groups[1:length(groups)-1]
      groups <- data.frame(groups)
      colnames(groups) <- "cluster"
      label <- as.numeric(groups$cluster)
      rm(groups)
      rm(label_vector)
      data.set <- cbind(data_kodjo_phate,label)
      rm(data_kodjo_phate)
      
      ########
      ######## function to weight data
      
      weight_markov_process_atitey <- function(data.set) {
        counts <- table(data.set$label) # Count occurrences of each label
        num_cells <- sum(counts) # Calculate the total number of cells
        data.set$label <- as.numeric(data.set$label) # Convert label to numeric
        for (i in unique(data.set$label)) {
          
          weight <- counts[i] / num_cells # Calculate the weight for the current label
          data.set[is.na(data.set)] <- 0
          data.set[data.set$label == i, ] <- weight * data.set[data.set$label == i, ] # Update rows where label matches
        }
        data.set <- data.set[, -ncol(data.set)]# Remove the label column
        data.set <- floor(data.set) # Floor the entire matrix
        return(data.set)
      }
      
      ########################################
      ######################################## second part run: weigth WxW matrix in terms of the label
      ########################################
      
      wei.data <- weight_markov_process_atitey(data.set)
      
      ########
      ######## function cumulative matrix
      
      cumulative_markov_process_atitey <- function(run_chain_matrix) {
        cumulative_matrix <- lapply(1:limit, function(k){cumsum(run_chain_matrix[[k]] == 1)/seq_along(run_chain_matrix[[k]] == 1)})
        cumulative_matrix <- data.frame(cumulative_matrix)
        kodjo_run <- lapply(1:limit, function(l) {rowMeans(cumulative_matrix[l], na.rm = TRUE) })
        kodjo_run <- data.frame(kodjo_run)
        kodjo_run <- rowMeans(kodjo_run)
        colnames(kodjo_run) <- NULL
        return(kodjo_run)
      }
      
      ########################################
      ######################################## Third part run: compute cumulative matrix
      ########################################
      cumul.wei.data <- cumulative_markov_process_atitey(wei.data)
      
      ########
      ######## function scoring metric
      
      data_structure_index_atitey <- function(markov_data){
        frenquency <- data.frame(matrix(ncol = 250, nrow = 1)) #
        sCore <- data.frame(matrix(ncol = 250, nrow = 1))
        sequ <- seq(from = 0, to = 0.16, by = .0006)
        count <- function(x, n){
          length((which(abs(x) >= n))) }
        Limit <- 250
        for(i in 1:Limit) {
          for(u in 1:Limit) {
            frenquency[u] <- abs(markov_data[u] - mean(markov_data))
          }
          j <- sequ[i]
          sCore[i] <- count(frenquency, j)
        }
        sCore <- sCore / Limit
        return(sCore)
      }
      
      ########################################
      ######################################## fourth part run: scoring
      ########################################
      
      metric.TI <- data_structure_index_atitey(cumul.wei.data)
      metric.TI <- data.frame(metric.TI)
      metric.TI <- as.numeric(metric.TI)
      
      return(metric.TI)
    }
    TI <- TimeOrderStructureLearning(reduced_data, label_vector)
    
    sentence7 <- "Finished!" # @@@@@@@@@@@@
    print(sentence7) # @@@@@@@@@@@@
    
    return(list(GI=GI, SI=SI, OI=OI, UI=UI, TI=TI))
    
  }
  
  
  integration_decision <- function(normalized_sc_data, label_vector, desired_length){
    
    duplicate_rows <- duplicated(normalized_sc_data)
    normalized_data <- normalized_sc_data[!duplicate_rows, ]
    
    print("phate dimension reduction")
    reduced_data_1 <- phate(normalized_data, ndim = 2)
    phate_data_org <- data.frame(reduced_data_1)
    colnames(phate_data_org) <- c("X1", "X2")
    
    print("tsne dimension reduction")
    reduced_data_2 <- Rtsne(normalized_data, perplexity=30, epoch=50)
    tsne_data_org  <- data.frame(reduced_data_2$Y)
    colnames(tsne_data_org) <- c("X1", "X2")
    
    print("umap dimension reduction")
    reduced_data_3 <- umap(normalized_data, labels=label_vector, controlscale=TRUE )
    umap_data_org <- data.frame(reduced_data_3$layout)
    colnames(umap_data_org) <- c("X1", "X2")
    
    print("pca dimension reduction")
    reduced_data_4 <- prcomp(normalized_data, scale=TRUE)
    pca_data_org  <- data.frame(reduced_data_4$x)[,1:2]
    colnames(pca_data_org) <- c("X1", "X2")
    
    
    tsne_data_org[is.na(tsne_data_org)] <- 0
    umap_data_org[is.na(umap_data_org)] <- 0
    pca_data_org[is.na(pca_data_org)] <- 0
    phate_data_org[is.na(phate_data_org)] <- 0
    
    reduced_data_1_1 <- tsne_data_org$X1
    reduced_data_1_2 <- tsne_data_org$X2
    reduced_data_2_1 <- umap_data_org$X1
    reduced_data_2_2 <- umap_data_org$X2
    reduced_data_3_1 <- pca_data_org$X1
    reduced_data_3_2 <- pca_data_org$X2
    reduced_data_4_1 <- phate_data_org$X1
    reduced_data_4_2 <- phate_data_org$X2
    
    data_1_1 <- (reduced_data_1_1 - min(reduced_data_1_1)) / (max(reduced_data_1_1) - min(reduced_data_1_1))
    data_1_2 <- (reduced_data_1_2 - min(reduced_data_1_2)) / (max(reduced_data_1_2) - min(reduced_data_1_2))
    data_2_1 <- (reduced_data_2_1 - min(reduced_data_2_1)) / (max(reduced_data_2_1) - min(reduced_data_2_1))
    data_2_2 <- (reduced_data_2_2 - min(reduced_data_2_2)) / (max(reduced_data_2_2) - min(reduced_data_2_2))
    data_3_1 <- (reduced_data_3_1 - min(reduced_data_3_1)) / (max(reduced_data_3_1) - min(reduced_data_3_1))
    data_3_2 <- (reduced_data_3_2 - min(reduced_data_3_2)) / (max(reduced_data_3_2) - min(reduced_data_3_2))
    data_4_1 <- (reduced_data_4_1 - min(reduced_data_4_1)) / (max(reduced_data_4_1) - min(reduced_data_4_1))
    data_4_2 <- (reduced_data_4_2 - min(reduced_data_4_2)) / (max(reduced_data_4_2) - min(reduced_data_4_2))
    
    tsne_data <- cbind(data_1_1, data_1_2)
    umap_data <- cbind(data_2_1, data_2_2)
    pca_data <- cbind(data_3_1, data_3_2)
    phate_data <- cbind(data_4_1, data_4_2)
    
    drm_data <- list(tsne_data, umap_data, pca_data, phate_data)
    
    print("phate, tsne, umap, pca performance evaluation")
    
    ########################################
    ######################################## run metrics
    ########################################    
    
    results_list <- list()
    for(u in 1:4) {
      reduced_scdata <- drm_data[[u]]
      metrics.score <- drm_metrics(reduced_scdata, label_vector, desired_length) 
      results_list[[u]] <- metrics.score
    }
    
    tsne <- results_list[[1]]
    umap <- results_list[[2]]
    pca <- results_list[[3]]
    phate <- results_list[[4]]
    
    xgb.pca <- pca$GI
    occ.pca <- pca$OI
    sep.pca <- pca$SI
    uni.pca <- pca$UI
    ti.pca <- pca$TI
    xgb.pca[xgb.pca == 1] <- 0.999
    occ.pca[occ.pca == 1] <- 0.999
    sep.pca[sep.pca == 1] <- 0.999
    uni.pca[uni.pca == 1] <- 0.999
    ti.pca[ti.pca == 1] <- 0.999
    
    xgb.tsne <- tsne$GI
    occ.tsne <- tsne$OI
    sep.tsne <- tsne$SI
    uni.tsne <- tsne$UI
    ti.tsne <- tsne$TI
    xgb.tsne[xgb.tsne == 1] <- 0.999
    occ.tsne[occ.tsne == 1] <- 0.999
    sep.tsne[sep.tsne == 1] <- 0.999
    uni.tsne[uni.tsne == 1] <- 0.999
    ti.tsne[ti.tsne == 1] <- 0.999
    
    xgb.umap <- umap$GI
    occ.umap <- umap$OI
    sep.umap <- umap$SI
    uni.umap <- umap$UI
    ti.umap <- umap$TI
    xgb.umap[xgb.umap == 1] <- 0.999
    occ.umap[occ.umap == 1] <- 0.999
    sep.umap[sep.umap == 1] <- 0.999
    uni.umap[uni.umap == 1] <- 0.999
    ti.umap[ti.umap == 1] <- 0.999
    
    xgb.phate <- phate$GI
    occ.phate <- phate$OI
    sep.phate <- phate$SI
    uni.phate <- phate$UI
    ti.phate <- phate$TI
    xgb.phate[xgb.phate == 1] <- 0.999
    occ.phate[occ.phate == 1] <- 0.999
    sep.phate[sep.phate == 1] <- 0.999
    uni.phate[uni.phate == 1] <- 0.999
    ti.phate[ti.phate == 1] <- 0.999
    
    
    ##################################################################################################################################
    ##############################################################################
    ############################################################################## function to compute hdi
    ##############################################################################
    
    drm_hdi <- function(xgb_data, occ_data, sep_data, uni_data, ti_data){
      ###########
      ########### data preparation
      #accuracy_range <- seq(Min_value, Max_value,length=250)
      accuracy <- xgb_data
      sequence <- seq(0, 1, length=250)
      beta <- pbeta(sequence, 1, 1)
      
      metric.data <- rbind(
        #as.numeric(xgb_data),
        as.numeric(occ_data),
        as.numeric(sep_data),
        as.numeric(uni_data),
        as.numeric(ti_data))
      
      d.method <- data.frame(Accuracy = accuracy,
                             Int = beta,
                             #Xgb = metric.data[1,],
                             Occupation = metric.data[1,],
                             Separability = metric.data[2,],
                             Uniformity = metric.data[3,],
                             Time_order = metric.data[4,])
      
      ########## bayesian model
      ##########
      model.method <- bf(Accuracy ~  Occupation + Separability +  Uniformity + Time_order , #Xgb +
                         phi ~ Occupation + Separability + Uniformity + Time_order) # Xgb + 
      bm.prior.method <- get_prior(model.method, data = d.method, family = Beta())
      bm.post.method <- brm(model.method, data = d.method, family = Beta(), prior=bm.prior.method, control = NULL,
                            chains = 4, cores = getOption("mc.cores", 1), iter = 2000, warmup = floor(2000/2))
      
      ########## hdi function
      ##########
      split_matrix_atitey <- function(dataset1, dataset2, dataset3, dataset4) {
        C1.select.1 <- function(x) x[order(x)][1:((0.025)*dim(dataset1)[1])]
        C1.select.2 <- function(x) x[order(x)][1:((0.975)*dim(dataset1)[1])]
        C1.out.1 <- C1.select.1(dataset1$val)
        C1.out.2 <- C1.select.2(dataset1$val)
        C1.hdi.1 <- C1.out.2[-which(C1.out.1%in%C1.out.2)]
        C1.hdi.1 <- data.frame(C1.hdi.1)
        colnames(C1.hdi.1) <- NULL
        
        C2.select.1 <- function(x) x[order(x)][1:((0.025)*dim(dataset2)[1])]
        C2.select.2 <- function(x) x[order(x)][1:((0.975)*dim(dataset2)[1])]
        C2.out.1 <- C2.select.1(dataset2$val)
        C2.out.2 <- C2.select.2(dataset2$val)
        C2.hdi.1 <- C2.out.2[-which(C2.out.1%in%C2.out.2)]
        C2.hdi.1 <- data.frame(C2.hdi.1)
        colnames(C2.hdi.1) <- NULL
        
        C3.select.1 <- function(x) x[order(x)][1:((0.025)*dim(dataset3)[1])]
        C3.select.2 <- function(x) x[order(x)][1:((0.975)*dim(dataset3)[1])]
        C3.out.1 <- C3.select.1(dataset3$val)
        C3.out.2 <- C3.select.2(dataset3$val)
        C3.hdi.1 <- C3.out.2[-which(C3.out.1%in%C3.out.2)]
        C3.hdi.1 <- data.frame(C3.hdi.1)
        colnames(C3.hdi.1) <- NULL
        
        C4.select.1 <- function(x) x[order(x)][1:((0.025)*dim(dataset4)[1])]
        C4.select.2 <- function(x) x[order(x)][1:((0.975)*dim(dataset4)[1])]
        C4.out.1 <- C4.select.1(dataset4$val)
        C4.out.2 <- C4.select.2(dataset4$val)
        C4.hdi.1 <- C4.out.2[-which(C4.out.1%in%C4.out.2)]
        C4.hdi.1 <- data.frame(C4.hdi.1)
        colnames(C4.hdi.1) <- NULL
        
        final.data <- cbind(C1.hdi.1, C2.hdi.1, C3.hdi.1, C4.hdi.1)
        colnames(final.data) <- NULL
        final.data <- as.matrix(final.data)
        
        hdi.data <- c(final.data[,1], final.data[,2], final.data[,3], final.data[,4])
        hdi.data <- data.frame(hdi.data)
        colnames(hdi.data) <- "val"
        
        select.1 <- function(x) x[order(x)][1:((0.025)*dim(hdi.data)[1])]
        select.2 <- function(x) x[order(x)][1:((0.975)*dim(hdi.data)[1])]
        out.1 <- select.1(hdi.data$val)
        out.2 <- select.2(hdi.data$val)
        hdi.f <- out.2[-which(out.1%in%out.2)]
        # hdi.f <- data.frame(hdi.f)
        return(hdi.f)
      }
      
      ########
      ########  SI
      data.chain.S1 <- data.frame(bm.post.method$fit@sim$samples[[1]]$b_Separability[1001:2000])
      colnames(data.chain.S1) <- "val"
      data.chain.S2 <- data.frame(bm.post.method$fit@sim$samples[[2]]$b_Separability[1001:2000])
      colnames(data.chain.S2) <- "val"
      data.chain.S3 <- data.frame(bm.post.method$fit@sim$samples[[3]]$b_Separability[1001:2000])
      colnames(data.chain.S3) <- "val"
      data.chain.S4 <- data.frame(bm.post.method$fit@sim$samples[[4]]$b_Separability[1001:2000])
      colnames(data.chain.S4) <- "val"
      SI.method <- split_matrix_atitey(data.chain.S1, data.chain.S2, data.chain.S3, data.chain.S4) #
      SI.method <- as.matrix(SI.method)
      colnames(SI.method) <- "SI"
      
      ########
      ########  UI
      data.chain.U1 <- data.frame(bm.post.method$fit@sim$samples[[1]]$b_Uniformity[1001:2000])
      colnames(data.chain.U1) <- "val"
      data.chain.U2 <- data.frame(bm.post.method$fit@sim$samples[[2]]$b_Uniformity[1001:2000])
      colnames(data.chain.U2) <- "val"
      data.chain.U3 <- data.frame(bm.post.method$fit@sim$samples[[3]]$b_Uniformity[1001:2000])
      colnames(data.chain.U3) <- "val"
      data.chain.U4 <- data.frame(bm.post.method$fit@sim$samples[[4]]$b_Uniformity[1001:2000])
      colnames(data.chain.U4) <- "val"
      UI.method <- split_matrix_atitey(data.chain.U1, data.chain.U2, data.chain.U3, data.chain.U4) #
      UI.method <- as.matrix(UI.method)
      colnames(UI.method) <- "UI"
      
      ########
      ########  OI
      data.chain.O1 <- data.frame(bm.post.method$fit@sim$samples[[2]]$b_Occupation[1001:2000])
      colnames(data.chain.O1) <- "val"
      data.chain.O2 <- data.frame(bm.post.method$fit@sim$samples[[2]]$b_Occupation[1001:2000])
      colnames(data.chain.O2) <- "val"
      data.chain.O3 <- data.frame(bm.post.method$fit@sim$samples[[3]]$b_Occupation[1001:2000])
      colnames(data.chain.O3) <- "val"
      data.chain.O4 <- data.frame(bm.post.method$fit@sim$samples[[4]]$b_Occupation[1001:2000])
      colnames(data.chain.O4) <- "val"
      OI.method <- split_matrix_atitey(data.chain.O1, data.chain.O2, data.chain.O3, data.chain.O4) #
      OI.method <- as.matrix(OI.method)
      colnames(OI.method) <- "OI"
      
      ########
      ########  TI
      data.chain.T1 <- data.frame(bm.post.method$fit@sim$samples[[1]]$b_Time_order[1001:2000])
      colnames(data.chain.T1) <- "val"
      data.chain.T2 <- data.frame(bm.post.method$fit@sim$samples[[2]]$b_Time_order[1001:2000])
      colnames(data.chain.T2) <- "val"
      data.chain.T3 <- data.frame(bm.post.method$fit@sim$samples[[3]]$b_Time_order[1001:2000])
      colnames(data.chain.T3) <- "val"
      data.chain.T4 <- data.frame(bm.post.method$fit@sim$samples[[4]]$b_Time_order[1001:2000])
      colnames(data.chain.T4) <- "val"
      TI.method <- split_matrix_atitey(data.chain.T1, data.chain.T2, data.chain.T3, data.chain.T4) #
      TI.method <- as.matrix(TI.method)
      colnames(TI.method) <- "TI"
      
      hdi.method <- cbind(OI.method, SI.method, UI.method, TI.method) # GI.method, 
      hdi.method <- data.frame(hdi.method)
      
      return(hdi.method)
    }
    
    
    
    
    
    
    
    
    ##################################################################################################################################
    print("HDI of the posterior distribution")
    HDI.pca <- drm_hdi(xgb.pca, occ.pca, sep.pca, uni.pca, ti.pca)
    HDI.tsne <- drm_hdi(xgb.tsne, occ.tsne, sep.tsne, uni.tsne, ti.tsne)
    HDI.umap <- drm_hdi(xgb.umap, occ.umap, sep.umap, uni.umap, ti.umap)
    HDI.phate <- drm_hdi(xgb.phate, occ.phate, sep.phate, uni.phate, ti.phate)
    
    si.pca <- HDI.pca$SI
    si.tsne <- HDI.tsne$SI
    si.umap <- HDI.umap$SI
    si.phate <- HDI.phate$SI
    si.score.pca <- (si.pca/sd(si.pca))^2
    si.score.tsne <- (si.tsne/sd(si.tsne))^2
    si.score.umap <- (si.umap/sd(si.umap))^2
    si.score.phate <- (si.phate/sd(si.phate))^2
    
    oi.pca <- HDI.pca$OI
    oi.tsne <- HDI.tsne$OI
    oi.umap <- HDI.umap$OI
    oi.phate <- HDI.phate$OI
    oi.score.pca <- (oi.pca/sd(oi.pca))^2
    oi.score.tsne <- (oi.tsne/sd(oi.tsne))^2
    oi.score.umap <- (oi.umap/sd(oi.umap))^2
    oi.score.phate <- (oi.phate/sd(oi.phate))^2
    
    ui.pca <- HDI.pca$UI
    ui.tsne <- HDI.tsne$UI
    ui.umap <- HDI.umap$UI
    ui.phate <- HDI.phate$UI
    ui.score.pca <- (ui.pca/sd(ui.pca))^2
    ui.score.tsne <- (ui.tsne/sd(ui.tsne))^2
    ui.score.umap <- (ui.umap/sd(ui.umap))^2
    ui.score.phate <- (ui.phate/sd(ui.phate))^2
    
    ti.pca <- HDI.pca$TI
    ti.tsne <- HDI.tsne$TI
    ti.umap <- HDI.umap$TI
    ti.phate <- HDI.phate$TI
    ti.score.pca <- (ti.pca/sd(ti.pca))^2
    ti.score.tsne <- (ti.tsne/sd(ti.tsne))^2
    ti.score.umap <- (ti.umap/sd(ti.umap))^2
    ti.score.phate <- (ti.phate/sd(ti.phate))^2
    
    
    
    ###################
    ################### decision
    tsne_si <- mean(si.score.tsne)
    umap_si <- mean(si.score.umap)
    pca_si <- mean(si.score.pca)
    phate_si <- mean(si.score.phate)
    si <- c(tsne_si, umap_si, pca_si, phate_si)
    si_norm <-  si/sum(si)
    
    tsne_oi <- mean(oi.score.tsne)
    umap_oi <- mean(oi.score.umap)
    pca_oi <- mean(oi.score.pca)
    phate_oi <- mean(oi.score.phate)
    oi <- c(tsne_oi, umap_oi, pca_oi, phate_oi)
    oi_norm <-  oi/sum(oi)
    
    tsne_ui <- mean(ui.score.tsne)
    umap_ui <- mean(ui.score.umap)
    pca_ui <- mean(ui.score.pca)
    phate_ui <- mean(ui.score.phate)
    ui <- c(tsne_ui, umap_ui, pca_ui, phate_ui)
    ui_norm <-  ui/sum(ui)
    
    tsne_ti <- mean(ti.score.tsne)
    umap_ti <- mean(ti.score.umap)
    pca_ti <- mean(ti.score.pca)
    phate_ti <- mean(ti.score.phate)
    ti <- c(tsne_ti, umap_ti, pca_ti, phate_ti)
    ti_norm <-  ti/sum(ti)
    
    max_method <- data.frame(c(si_norm, oi_norm, ui_norm, ti_norm))
    name_method <- c("tsne_si", "umap_si", "pca_si", "phate_si",
                     "tsne_oi", "umap_oi", "pca_oi", "phate_oi",
                     "tsne_ui", "umap_ui", "pca_ui", "phate_ui",
                     "tsne_ti", "umap_ti", "pca_ti", "phate_ti")
    
    method <- cbind(name_method, max_method)
    colnames(method) <- c("name", "value")
    #method$value <- as.numeric(method$value)
    #order_method <- method[order(-method$value),]
    #print(paste("method", "-->",order_method$name[1:5],sep=""))
    #print("integrate the top two different methods independent of the metric's index")
    
    matrix_data <- method
    underscore_names <- gsub("^.*?_", "", matrix_data$name)
    ordered_underscore_names <- sort(unique(underscore_names))
    ordered_rows <- list()
    
    # Loop through each underscore name
    for (underscore_name in ordered_underscore_names) {
      # Extract rows with the current underscore name
      rows_with_underscore <- which(underscore_names == underscore_name)
      
      # Sort rows based on their corresponding values
      ordered_rows_with_underscore <- rows_with_underscore[order(matrix_data$value[rows_with_underscore])]
      
      # Store ordered rows in the list
      ordered_rows <- c(ordered_rows, ordered_rows_with_underscore)
    }
    
    # Combine ordered rows
    ordered_indices <- unlist(ordered_rows)
    
    # Create the ordered matrix
    ordered_matrix <- matrix_data[ordered_indices, ]
    
    # Print the ordered matrix
    
    
    #########
    underscore_sums <- tapply(ordered_matrix$value, gsub("^.*?_", "", ordered_matrix$name), sum)
    
    # Initialize variables to store the top two sums and their corresponding underscore names
    top_sum_1 <- -Inf
    top_sum_2 <- -Inf
    top_underscore_names_1 <- NULL
    top_underscore_names_2 <- NULL
    
    # Iterate through each unique underscore name
    for (underscore_name in unique(gsub("^.*?_", "", ordered_matrix$name))) {
      # Find the sum of values for the current underscore name
      current_sum <- underscore_sums[[underscore_name]]
      
      # Check if the current sum is greater than the top two sums
      if (current_sum > top_sum_1) {
        # Update the top sums and underscore names accordingly
        top_sum_2 <- top_sum_1
        top_underscore_names_2 <- top_underscore_names_1
        top_sum_1 <- current_sum
        top_underscore_names_1 <- underscore_name
      } else if (current_sum > top_sum_2) {
        # Update the second top sum and underscore names accordingly
        top_sum_2 <- current_sum
        top_underscore_names_2 <- underscore_name
      }
    }
    
    # Find the top two rows with the same underscore names
    top_rows_1 <- ordered_matrix[grep(paste0("^.*?_", top_underscore_names_1), ordered_matrix$name), ]
    
    # Print the top two rows
    order_method <- top_rows_1[order(-top_rows_1$value),] #top_rows_1[order(-top_rows_1$value),]
    
    drm <- list(tsne_data, umap_data, pca_data, phate_data)
    combined_top_drms <-function(A, drm)  {
      
      drm_fusion <-function(A, drm)  {
        
        ########
        ######## function to select the two drms for integration
        top2_drm <- function(drm_ordered)  {
          i=2
          name_drm_ordered=drm_ordered$name
          short_name=NULL
          short_name[1]=str_sub(name_drm_ordered[1], start=1, end=-4)
          while (i<=length(name_drm_ordered)) {
            short_name[i] <- str_sub(name_drm_ordered[i], start=1, end=-4)
            if  (short_name[i] != short_name[i-1])  {
              return(c(short_name[1], short_name[i]))
            }else{
              i=i+1
            }
          }
        }
        
        ########
        ######## combine the two drms for integration
        E <- c("tsne", "umap", "pca", "phate")
        C <- top2_drm(order_method)
        R <- NULL
        
        for(i in 1:2) {
          if (C[i] == E[1]) {
            R[[i]] <- drm[[1]]
          }
          if (C[i] == E[2]) {
            R[[i]] <- drm[[2]]
          }
          if (C[i] == E[3]) {
            R[[i]] <- drm[[3]]
          }
          if (C[i] == E[4]) {
            R[[i]] <- drm[[4]]
          }
        }
        R1 <- do.call(cbind, R)
        #R1 <- data.frame(R1)
        #colnames(R1) <- c("x1", "x2", "x3", "x4")
        return(list(DRMs=C, data=R1))
      }
      
      
      result <- drm_fusion(order_method, drm)
      methods <- result$DRMs
      combined_data <- result$data
      combined_data <- data.frame(combined_data)
      colnames(combined_data) <- c("x1", "x2", "x3", "x4")
      
      ##################
      ################## cbind the data
      reduced_data_1 <- combined_data$x1
      reduced_data_2 <- combined_data$x2
      reduced_data_3 <- combined_data$x3
      reduced_data_4 <- combined_data$x4
      
      int_drm_data <- cbind(reduced_data_1, reduced_data_2, reduced_data_3, reduced_data_4)
      colnames(int_drm_data) <- c("col1", "col2", "col3", "col4")
      
      return(list(DRMs=methods, combined_data=int_drm_data))
    }
    
    final <- combined_top_drms(order_method, drm)
    top_methods <- final$DRMs
    drm_data <- final$combined_data
    
    return(list(top_methods=top_methods, drm_data=drm_data,
                tsne_data=tsne_data, umap_data=umap_data,
                pca_data=pca_data, phate_data=phate_data,
                HDI.pca= HDI.pca, HDI.tsne=HDI.tsne, HDI.umap=HDI.umap, HDI.phate=HDI.phate,
                xgb.tsne=xgb.tsne,
                xgb.umap=xgb.umap,
                xgb.pca=xgb.pca,
                xgb.phate=xgb.phate))
    
  }
  
  ##############################################################################
  ############################################################################## inplementation
  ##############################################################################
  
  #############
  ############# evaluate the integrated data
  decision <- integration_decision(normalized_sc_data, label_vector, desired_length)
  
  top_methods <- decision$top_methods
  
  HDI.tsne <- decision$HDI.tsne
  HDI.umap <- decision$HDI.umap
  HDI.pca <- decision$HDI.pca
  HDI.phate <- decision$HDI.phate
  tsne_data <- decision$tsne_data
  umap_data <- decision$umap_data
  pca_data <- decision$pca_data
  phate_data <- decision$phate_data
  
  print(top_methods)
  reduced_data_1.reduced_data_2 <- decision$drm_data
  
  #############
  ############# autoencoder
  print("Run AE for data integartion")
  
  combined_data <- as.matrix(reduced_data_1.reduced_data_2)
  
  #### set training data
  units <-  64
  batch_sizes <-  128
  n_units <- units  
  batch_size <- batch_sizes  
  
  # Create and train the autoencoder model with the current number of units
  Lenght_data <- dim(data.frame(combined_data))[1]
  L <- floor(Lenght_data/3)
  x_train <- reduced_data_1.reduced_data_2[1:L, ]
  
  # Create the model
  model <- keras_model_sequential()
  model <- model %>%
    layer_dense(units = n_units, activation = "tanh", input_shape = ncol(x_train)) %>%
    layer_dense(units = n_units/2, activation = "tanh") %>%
    layer_dense(units = 2, activation = "tanh", name = "bottleneck") %>%
    layer_dense(units = n_units/2, activation = "tanh") %>%
    layer_dense(units = ncol(x_train), activation = "sigmoid")
  
  # Compile model
  model %>% compile(loss = "mean_squared_error", optimizer = "adam")
  
  # Fit model with different batch sizes
  history <- model %>% fit(x = x_train,
                           y = x_train,
                           epochs = 50,
                           verbose = 0,
                           batch_size = batch_size)
  
  # Evaluate the performance of the model
  mse.ae2 <- evaluate(model, x_train, x_train)
  print(mse.ae2)
  
  # Extract the bottleneck layer
  intermediate_layer_model <- keras_model(inputs = model$input, outputs = get_layer(model, "bottleneck")$output)
  integ_data <- predict(intermediate_layer_model, combined_data)
  integ_data <- data.frame(integ_data)
  
  return(list(integ_data=integ_data, tsne_data=tsne_data, umap_data=umap_data,
              pca_data=pca_data, phate_data=phate_data))
}



















