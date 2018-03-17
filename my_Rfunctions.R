# Cross validation
require(pROC)

# WOE
require(gtools)
require(Information)

# Create missing value indicators for all variables in X
mv_indicator <- function(X){
  Y <- X
  var_names = names(X)
  for (i in 1:ncol(X)) {
    if (sum(is.na(X[,i])) >0) {
      vname =  paste0("M_", var_names[i])
      Y[vname] = 1*is.na(X[,i]);
    }
  }
  return(Y)
}

# functions for WoE replacement

WOE_replace_df <- function(X,IV) {
  Y <- X
  var_names <- names(IV$Tables)
  for (i in var_names) {
    Y[i] <- WOE_replace(X[[i]], data.frame(IV$Tables[i]))
  }
  return(Y)
}

# replace single column
WOE_replace <- function(x,iv_table) {
  ny <- vector("numeric", length(x))
  # Loop over bins
  for (j in 1:nrow(iv_table)) {
    
    if (iv_table[j,1] == "NA"  | is.na(iv_table[j,1])) {
      # Missing values
      ny[which(is.na(x))] <- iv_table[j,4]
      
    } else if (is.factor(x)) {
      ny[which(x == iv_table[j,1])] <- iv_table[j,4]
      
    } else {
      if (j < nrow(iv_table)) {
        lower <- as.double( gsub("\\[([^,]+),[^,]+","\\1",iv_table[j,1]) )
        #upper <- as.double( gsub("\\[([^,]+),([^,]+)\\]","\\2",iv_table[j,1]))
        upper <- as.double( gsub("\\[([^,]+),([^,]+)\\]","\\1",iv_table[j+1,1]))
        ny[which(x>=lower & x<upper)] <- iv_table[j,4]
      } else{
        lower <- as.double( gsub("\\[([^,]+),[^,]+","\\1",iv_table[j,1]) )
        ny[which(x>=lower)] <- iv_table[j,4]
      }
    }
  }
  return(ny)
}

# Bin rather than substitute with WOE value
WOE_bin <- function(x,iv_table) {
  if (is.factor(x)) {
    return(x)
  }
  
  ny <- vector("numeric", length(x))
  # Loop over bins
  for (j in 1:nrow(iv_table)) {
    
    if (iv_table[j,1] == "NA"  | is.na(iv_table[j,1])) {
      # Missing values
      #ny[which(is.na(x))] <- 0 (it's already 0 so do nothing)
      
    } else {
      if (j < nrow(iv_table)) {
        lower <- as.double( gsub("\\[([^,]+),[^,]+","\\1",iv_table[j,1]) )
        #upper <- as.double( gsub("\\[([^,]+),([^,]+)\\]","\\2",iv_table[j,1]))
        upper <- as.double( gsub("\\[([^,]+),([^,]+)\\]","\\1",iv_table[j+1,1]))
        ny[which(x>=lower & x<upper)] <- j
      } else{
        lower <- as.double( gsub("\\[([^,]+),[^,]+","\\1",iv_table[j,1]) )
        ny[which(x>=lower)] <- j
      }
    }
  }
  return(factor(ny))
}

WOE_bin_df <- function(X,IV) {
  Y <- X
  var_names <- names(IV$Tables)
  for (i in var_names) {
    df <- data.frame( IV$Tables[i] )
    Y[i] <- WOE_bin(X[[i]], data.frame(IV$Tables[i]))
  }
  return(Y)
}

## Cross Validation
cv_perform <- function(X,Y,formula,folds)
{
  K <- max(folds)
  perf <- vector(mode = "numeric",length = K)
  for (i in 1:K) {
    train <- which(folds!=i)
    # build model on K-1 partitions
    lr <- glm(formula, data = X[train,], family = binomial)
    probs <- predict(lr, newdata = X[-train,], type = "response")
    perf[i] = auc(Y[-train,], probs)
  }
  return(perf)
}

crossValid <- function(X,y=NULL,folds=NULL, K=10,nmax=NULL)
{
  #set.seed(1)
  nm = names(X)
  n = nrow(X)
  ## input checking
  if (is.null(y)) {
    y <- nm[1]
    predictors <- nm[-1]
  } else {
    if (sum(nm==y) == 0) {
      print("Error: Class variable not contained in the data frame")
      return()
    }
    predictors <- nm[nm != y]
  }
  
  if (is.null(folds)) {
    if (K>1) {
      folds <- sample(1:K, n, replace = TRUE)
    } else {
      print("Error: Number of cross validation folds < 2")
      return()
    }
    
  } else if (max(folds) < 2) {
    print("Error: Cross validation folds incorrectly defined: max(folds)=1")
    return()
  }
  
  # Forward Variable selection
  models <- list( paste0(y,"~1") )
 # print(paste("Empty model:", models[[1]]))
  
  P <- matrix(nrow = K, ncol= 1 + ifelse(is.null(nmax), length(predictors), nmax))
  P[,1] <- cv_perform(X, X[y], models[[1]], folds)
  
  min_var <- ifelse(is.null(nmax),0, length(predictors)-nmax)
  iter <- 1
  while (length(predictors) > min_var) {
    cur_max = 0;
    for (i in 1:length(predictors)) {
      fml <- paste(models[[iter]], predictors[i], sep = "+")
      perf <- cv_perform(X, X[y], fml, folds)
#      print(paste("\t",fml, mean(perf)))
      
      if (mean(perf) > mean(cur_max)) {
        cur_max <- perf
        cur_mod <- fml
        pred_id <- predictors[i]
      }
    }
    
    iter <- iter+1
    models[[iter]] <- cur_mod
    P[,iter] <- cur_max
    print(paste(iter-1, "-- add", pred_id,"-- AUC:", mean(cur_max)))
    predictors <- predictors[-which(predictors==pred_id)]
    #print(paste(iter,"var model:",models[[iter]], mean(cur_max)))
  }
  av <- apply(P, 2, mean)
  thr <- max(av) - sd(P[, which.max(av)])
  select <- min(which(av>thr))
  best_model <- glm(models[[select]], data=X, family=binomial)
  return(list(BestModel=best_model,Models=models, Perf=P))
}

plotCVauc <- function(out) {
  
  cverrs <- apply(out$Perf,2,mean)
  sdCV <- apply(out$Perf,2,sd)
  CVLo <- cverrs - sdCV
  CVHi <- cverrs + sdCV
  ymax <- max(CVHi)
  ymin <- min(CVLo)
  k <- 0:(length(cverrs) - 1)
  plot(k, cverrs, xlab = "Subset Size", ylab = "CV AUC", ylim = c(ymin,ymax), type = "n", yaxt = "n")
  points(k, cverrs, cex = 1, col = "red", pch = 16)
  lines(k, cverrs, col = "red", lwd = 1)
  axis(2, yaxp = c(0.6, 1.8, 6))
  segments(k, CVLo, k, CVHi, col = "blue", lwd = 1)
  eps <- 0.15
  segments(k - eps, CVLo, k + eps, CVLo, col = "blue", lwd = 1)
  segments(k - eps, CVHi, k + eps, CVHi, col = "blue", lwd = 1)
  indBest <- length(coef(out$BestModel))
  
  abline(v = indBest - 1, lty = 2)
  indMax <- which.max(cverrs)
  points(indMax-1, cverrs[indMax], cex = 1, col = "black", pch = 8)
  cutOff <- cverrs[indMax] - sdCV[indMax]
  abline(h = cutOff, lty = 2)
}