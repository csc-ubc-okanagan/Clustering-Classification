# Kmeans and KMediods Demo

# Data provided by the following
# Cardoso, M. (2013). Wholesale customers [Dataset]. UCI Machine Learning Repository. https://doi.org/10.24432/C5030X.

set.seed(448)                                      
pkgs <- c("cluster","ggplot2","corrplot","mclust","aricode","patchwork","MASS")
to_install <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(to_install) > 0) install.packages(to_install)
library(cluster)                                          # pam(), silhouette()
library(ggplot2)                                          
library(corrplot)                                         # corrplot()
library(mclust)                                           # adjustedRandIndex()
library(aricode)                                          # NMI()
library(patchwork)                                        
library(MASS)                                             # mvrnorm()

# PART A: Application (UCI Wholesale Customers) — k-means+PAM
############################################################

DataUrl <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00292/Wholesale%20customers%20data.csv"  # src
CustomerData <- read.csv(DataUrl, stringsAsFactors = FALSE)               # load

names(CustomerData) <- c("Channel","Region","Fresh","Milk","Grocery","Frozen","DetergentsPaper","Delicassen") # rename

dim(CustomerData)                                         # shape
str(CustomerData)                                         # types
summary(CustomerData)                                     # basic stats
colSums(is.na(CustomerData))                              # NA chk

table(CustomerData$Channel)                               
table(CustomerData$Region)                                

SpendVars <- c("Fresh","Milk","Grocery","Frozen","DetergentsPaper","Delicassen")  # spend cols
SpendData <- CustomerData[, SpendVars]                    # numeric-only

par(mfrow = c(2,3))                                       
for (v in SpendVars) hist(SpendData[[v]], main = v, xlab = v, breaks = 25)  # hists
par(mfrow = c(1,1))                                       

par(mfrow = c(2,3))                                       
for (v in SpendVars) boxplot(SpendData[[v]], main = v, horizontal = TRUE)   
par(mfrow = c(1,1))                                       

SpendLog <- log1p(SpendData)                              # log(1+x) for skew handling
round(cor(SpendLog), 2)                                   # corr (rounded)
corrplot(round(cor(SpendLog), 2))                         # corrplot

# we will visualize only a select number of observations are the it will not show 
# well otherwise.
set.seed(448)                                            
idx <- sample(seq_len(nrow(SpendLog)), size = min(200, nrow(SpendLog)))    
pairs(SpendLog[idx, ], pch = 19, cex = 0.3)               

par(mfrow = c(2,3))                                      
for (v in SpendVars) boxplot(SpendLog[[v]], main = v, horizontal = TRUE)    
par(mfrow = c(1,1))                                       

X <- scale(SpendLog)                # standardize (distance-based)

par(mfrow = c(2,3))                                       
for (v in SpendVars) boxplot(X[, v], main = v, horizontal = TRUE) # box (scaled)
par(mfrow = c(1,1))                                       

# WCSS function from  cluster centers
wcssFromCenters <- function(X, cluster, centers) {         
  s <- 0
  for (k in seq_len(nrow(centers))) {
    members <- which(cluster == k)
    if (length(members) > 0) {
      diffs <- X[members, , drop = FALSE]-matrix(centers[k, ], nrow = length(members), ncol = ncol(X), byrow = TRUE)
      s <- s+sum(diffs^2)
    }
  }
  s
}

# One clustering metric, where high values are indicative of better clusterings.
calinskiHarabasz <- function(X, cluster) {                 
  X <- as.matrix(X)
  n <- nrow(X)
  k <- length(unique(cluster))
  overall <- colMeans(X)
  
  W <- 0
  for (g in sort(unique(cluster))) {
    Xg <- X[cluster == g, , drop = FALSE]
    if (nrow(Xg) > 0) {
      cg <- colMeans(Xg)
      W <- W+sum((Xg-matrix(cg, nrow(Xg), ncol(X), byrow = TRUE))^2)
    }
  }
  
  B <- 0
  for (g in sort(unique(cluster))) {
    Xg <- X[cluster == g, , drop = FALSE]
    if (nrow(Xg) > 0) {
      cg <- colMeans(Xg)
      ng <- nrow(Xg)
      B <- B+ng*sum((cg-overall)^2)
    }
  }
  
  (B/(k-1))/(W/(n-k))
}

# Davies-Bouldin index, where low values are indicative of better clusterings.
daviesBouldin <- function(X, cluster) {                    
  X <- as.matrix(X)
  groups <- sort(unique(cluster))
  k <- length(groups)
  C <- matrix(NA, nrow = k, ncol = ncol(X))
  rownames(C) <- groups
  for (i in seq_along(groups)) {
    g <- groups[i]
    C[i, ] <- colMeans(X[cluster == g, , drop = FALSE])
  }
  S <- rep(NA, k)
  for (i in seq_along(groups)) {
    g <- groups[i]
    Xg <- X[cluster == g, , drop = FALSE]
    diffs <- Xg-matrix(C[i, ], nrow(Xg), ncol(X), byrow = TRUE)
    S[i] <- mean(sqrt(rowSums(diffs^2)))
  }
  M <- as.matrix(dist(C))
  R <- rep(NA, k)
  for (i in seq_len(k)) {
    ratios <- rep(-Inf, k)
    for (j in seq_len(k)) {
      if (i != j) ratios[j] <- (S[i]+S[j])/M[i, j]
    }
    R[i] <- max(ratios[is.finite(ratios)])
  }
  mean(R)
}

# Average Silhouette Coefficient, where higher values are preferred
avgSilhouette <- function(X, cluster) {                    
  sil <- silhouette(cluster, dist(X))
  mean(sil[, 3])
}

# line search over k to choose number of clusters
KGrid <- 2:20                                              
ElbowWCSS <- numeric(length(KGrid))                        # WSS per k
SilScores <- numeric(length(KGrid))                        # sil per k

for (i in seq_along(KGrid)) {
  k <- KGrid[i]
  km <- kmeans(X, centers = k, nstart = 20, iter.max = 1000)  
  ElbowWCSS[i] <- km$tot.withinss                             # WSS
  SilScores[i] <- avgSilhouette(X, km$cluster)                # sil
}

plot(KGrid, ElbowWCSS, type = "b", xlab = "k", ylab = "WSS", main = "Elbow")                 
plot(KGrid, SilScores, type = "b", xlab = "k", ylab = "Average Silhouette", main = "Silhouette") 

k <- 2 # clearly k=2 is the better choice according to Silhouette, lets go with that.

set.seed(448)
kmres <- kmeans(X, centers = k, nstart = 50, iter.max = 10000)  # final kmeans
kmres$size                                                 # extract cluster sizes
kmCenters <- kmres$centers                                 # extract cluster centres

# PAM fits
pamresEuc <- pam(X, k = k, metric = "euclidean")           # PAM w/ Euclidean distance
pamresMan <- pam(X, k = k, metric = "manhattan")           # PAM w/ Manhattan distance

table(pamresEuc$clustering)                               
table(pamresMan$clustering)                                

kmWCSS <- kmres$tot.withinss                               # kmeans obj
kmSil  <- avgSilhouette(X, kmres$cluster)                  # sil
kmCH   <- calinskiHarabasz(X, kmres$cluster)               # CH
kmDB   <- daviesBouldin(X, kmres$cluster)                  # DB

pamEucSil <- avgSilhouette(X, pamresEuc$clustering)        # sil
pamEucCH  <- calinskiHarabasz(X, pamresEuc$clustering)     # CH
pamEucDB  <- daviesBouldin(X, pamresEuc$clustering)        # DB

pamManSil <- mean(silhouette(pamresMan$clustering, dist(X, method = "manhattan"))[, 3])  # sil (man)
pamManCH  <- calinskiHarabasz(X, pamresMan$clustering)     # CH
pamManDB  <- daviesBouldin(X, pamresMan$clustering)        # DB

Metrics <- data.frame(
  Method = c("KMeans","PAM_Euclidean","PAM_Manhattan"),
  WCSS = c(kmWCSS, NA, NA),                                # WCSS is kmeans-only
  AvgSilhouette = c(kmSil, pamEucSil, pamManSil),
  CalinskiHarabasz = c(kmCH, pamEucCH, pamManCH),
  DaviesBouldin = c(kmDB, pamEucDB, pamManDB)
)
print(Metrics) # note NA for WCSS are we are not dealing with the cluster mean for PAM

table(pamresEuc$clustering, pamresMan$clustering)          # agreement between euc/man pam clustering
table(pamresEuc$clustering, kmres$cluster)                 # agreement between euc with pam/kmeans clustering
table(pamresMan$clustering, kmres$cluster)                 # agreement between man with pam/kmeans clustering


# we will label one of the variable we did not cluster on to determine if the clusters
# reveal anything insightful regarding the regions of the customers
CustomerData$Region[CustomerData$Region == 1] <- "Lisbon"   
CustomerData$Region[CustomerData$Region == 2] <- "Porto"    
CustomerData$Region[CustomerData$Region == 3] <- "Other"    

# we will do it again for the channel of goods, HoReCa being Hospitality, Restaurants+Catering,
# while retail is obvious
CustomerData$Channel[CustomerData$Channel == 1] <- "HoReCa" # relabel
CustomerData$Channel[CustomerData$Channel == 2] <- "Retail" # relabel

# interpret clusters 
CustomerData$KMeansCluster <- factor(kmres$cluster)         # add kmeans
CustomerData$PAMEucCluster <- factor(pamresEuc$clustering)  # add PAM euc

aggregate(CustomerData[, SpendVars], list(Cluster = CustomerData$KMeansCluster), mean)   
aggregate(CustomerData[, SpendVars], list(Cluster = CustomerData$KMeansCluster), median) 

prop.table(table(CustomerData$KMeansCluster, CustomerData$Channel), margin = 1)          
prop.table(table(CustomerData$KMeansCluster, CustomerData$Region),  margin = 1)         

# PCA viz
pca <- prcomp(X, center = FALSE, scale. = FALSE)            # PCA (X already centered/scaled)
PC <- pca$x[, 1:2]                                          # PC1-2

par(mfrow = c(3,1))                                        
swapCols <- ifelse(as.integer(CustomerData$KMeansCluster) == 1, 2, 1)  # swap colors
plot(PC, col = swapCols, pch = 19, xlab = "PC1", ylab = "PC2", main = "k-means PCA Results")         
plot(PC, col = as.integer(CustomerData$PAMEucCluster), pch = 19, xlab = "PC1", ylab = "PC2", 
     main = "k-medoids (Euclidean) PCA Results") # PAM
plot(PC, col = ifelse(swapCols == as.integer(CustomerData$PAMEucCluster), NA, "red"), pch = 19,
     xlab = "PC1", ylab = "PC2", main = "Differences")      
par(mfrow = c(1,1))                                         

table(CustomerData$KMeansCluster, CustomerData$Channel)     
pairs(X, col = as.integer(CustomerData$PAMEucCluster))       


# PART B: Conceptual demo of kmeans
############################################################

set.seed(42)                                               
nPerClass <- 500                                            
Mu1 <- c(0, 0)                                              
Mu2 <- c(2, 2)                                              

Sigma1 <- matrix(c(1.0, 0.0, 0.0, 1.0), 2, 2, byrow = TRUE)  
Sigma2 <- matrix(c(1.0, -0.3, -0.3, 1.0), 2, 2, byrow = TRUE)

X1 <- MASS::mvrnorm(nPerClass, mu = Mu1, Sigma = Sigma1)     
X2 <- MASS::mvrnorm(nPerClass, mu = Mu2, Sigma = Sigma2)     
X <- rbind(X1, X2)                                           
TrueClass <- factor(c(rep("Class1", nPerClass), rep("Class2", nPerClass)))  # truth

SimData <- data.frame(X1 = X[,1], X2 = X[,2], TrueClass = TrueClass)        

# this is the true data generating process
ggplot(SimData, aes(x = X1, y = X2, color = TrueClass)) +
  geom_point(alpha = 0.7, size = 1.6) +
  scale_color_manual(values = c("navy", "darkred")) +
  theme_minimal() +
  theme(panel.border = element_rect(NA, "black", 1),
        panel.grid = element_blank(),
        legend.position = "bottom")                         

# we are just visualizing the same data where k starts at 2 and ends at 10
Ks <- 2:10 
par(mfrow = c(4,3))
for (k in Ks) {
  km <- kmeans(SimData[, c("X1","X2")], centers = k, nstart = 30, iter.max = 1000)  
  SimData$ClusterK <- factor(km$cluster)                     
  Ck <- as.data.frame(km$centers)                         
  names(Ck) <- c("X1","X2")                                  
  
  p <- ggplot(SimData, aes(X1, X2, color = ClusterK)) +
    geom_point(alpha = 0.65, size = 1.5) +
    geom_point(data = Ck, aes(X1, X2), inherit.aes = FALSE, shape = 4, stroke = 2.0, size = 5) +
    labs(title = paste0("k=", k, " Clusters")) +
    theme_minimal() +
    theme(panel.border = element_rect(NA, "black", 1),
          panel.grid = element_blank(),
          legend.position = "bottom")
  print(p)                                                
}
par(mfrow = c(1,1))

# lets do k=2 since we know that to be the true (classification problem)
km2 <- kmeans(SimData[, c("X1","X2")], centers = 2, nstart = 50, iter.max = 10000)  
PredCluster <- factor(km2$cluster)                           

Tab <- table(PredCluster, TrueClass)                        

nomatch <- which(PredCluster != as.numeric(TrueClass))      
SimData$match <- 2                                           
SimData[nomatch, ]$match <- 1                              

# this visualizes the observations we got wrong according to the true data clusters
ggplot(SimData, aes(X1, X2, color = factor(match))) +
  geom_point(size = 1.5, alpha = ifelse(SimData$match == "1", 1, 0.2)) +
  scale_color_manual(values = c("red","navy")) +
  labs(title = paste0("k=", 2, " Clusters")) +
  theme_minimal() +
  theme(panel.border = element_rect(NA, "black", 1),
        panel.grid = element_blank(),
        legend.position = "bottom")                         


# this next section gives us all the performance metrics
accA <- (Tab[1,"Class1"]+Tab[2,"Class2"])/sum(Tab)       
accB <- (Tab[1,"Class2"]+Tab[2,"Class1"])/sum(Tab)       

if (accA >= accB) {
  Map <- c("1" = "Class1", "2" = "Class2")                  
} else {
  Map <- c("1" = "Class2", "2" = "Class1")                  
}

PredClass <- factor(Map[as.character(PredCluster)], levels = levels(TrueClass))  # mapped pred

positive <- "Class2"                                         # define pos
TP <- sum(PredClass == positive & TrueClass == positive)     # TP
FP <- sum(PredClass == positive & TrueClass != positive)     # FP
FN <- sum(PredClass != positive & TrueClass == positive)     # FN
TN <- sum(PredClass != positive & TrueClass != positive)     # TN

Accuracy <- (TP+TN)/(TP+TN+FP+FN)                  # acc
Precision <- if ((TP+FP) == 0) NA else TP/(TP+FP)      # prec
Recall <- if ((TP+FN) == 0) NA else TP/(TP+FN)         # rec
F1 <- if (is.na(Precision) || is.na(Recall) || (Precision+Recall) == 0) NA else 2*Precision*Recall/(Precision+Recall)  # F1

c(Accuracy = Accuracy, Precision = Precision, Recall = Recall, F1 = F1)  # cls metrics

ARI <- adjustedRandIndex(PredCluster, TrueClass)             # ARI
NMIv <- NMI(as.integer(PredCluster), as.integer(TrueClass))  # NMI
c(ARI = ARI, NMI = NMIv)                                     # agreement metrics

SimData$PredCluster <- PredCluster                            # add
SimData$PredClass <- PredClass                                # add

pTrue <- ggplot(SimData, aes(X1, X2, color = TrueClass)) +
  geom_point(alpha = 0.7, size = 1.6) +
  scale_color_manual(values = c("navy", "darkred")) +
  labs(title = "True classes") +
  theme_minimal() +
  theme(panel.border = element_rect(NA, "black", 1),
        panel.grid = element_blank(),
        legend.position = "bottom")                         

pPred <- ggplot(SimData, aes(X1, X2, color = PredCluster)) +
  geom_point(alpha = 0.7, size = 1.6) +
  scale_color_manual(values = c("navy", "darkred")) +
  geom_point(data = as.data.frame(km2$centers), aes(X1, X2), inherit.aes = FALSE, shape = 4, stroke = 2.0, size = 6) +
  labs(title = "k-means predicted clusters (k=2)") +
  theme_minimal() +
  theme(panel.border = element_rect(NA, "black", 1),
        panel.grid = element_blank(),
        legend.position = "bottom")                         

print(pTrue); print(pPred)                                  


# PART C: Step-by-step k-means
############################################################

set.seed(448)                                               

nPerClass <- 500                                             
Mu1 <- c(0, 0)                                               
Mu2 <- c(2, 2)                                               

Sigma1 <- matrix(c(1.0, 0.0, 0.0, 1.0), 2, 2, byrow = TRUE)   
Sigma2 <- matrix(c(1.0, -0.3, -0.3, 1.0), 2, 2, byrow = TRUE) 

X1 <- MASS::mvrnorm(nPerClass, mu = Mu1, Sigma = Sigma1)      
X2 <- MASS::mvrnorm(nPerClass, mu = Mu2, Sigma = Sigma2)      
X <- rbind(X1, X2)                                            

SimData <- data.frame(
  X1 = X[, 1],
  X2 = X[, 2],
  TrueClass = factor(c(rep("Class1", nPerClass), rep("Class2", nPerClass)))
)                                                             

wcssValue <- function(X, cluster, centers) {                   # WCSS
  X <- as.matrix(X)
  k <- nrow(centers)
  s <- 0
  for (j in 1:k) {
    members <- which(cluster == j)
    if (length(members) > 0) {
      diffs <- X[members, , drop = FALSE]-matrix(centers[j, ], length(members), ncol(X), byrow = TRUE)
      s <- s+sum(diffs^2)
    }
  }
  s
}

kmeansSteps <- function(X, k, iters = 5) {                     # manual kmeans
  X <- as.matrix(X)
  n <- nrow(X)
  
  centers <- matrix(c(3,3,-3,-3), nrow = 2, byrow = TRUE)      # init (k=2)
  centersHist <- vector("list", iters+1)                     # store centers
  clusterHist <- vector("list", iters)                         # store clusters
  wcssHist <- numeric(iters)                                   # store trace
  
  centersHist[[1]] <- centers                                  
  
  for (it in 1:iters) {
    
    d2 <- sapply(1:k, function(j) rowSums((X-matrix(centers[j, ], n, ncol(X), byrow = TRUE))^2))  
    cluster <- max.col(-d2)                                
    
    newCenters <- centers                                  
    for (j in 1:k) {
      members <- which(cluster == j)
      if (length(members) > 0) newCenters[j, ] <- colMeans(X[members, , drop = FALSE])
    }
    
    wcssHist[it] <- wcssValue(X, cluster, newCenters)      
    
    clusterHist[[it]] <- cluster                            
    centers <- newCenters   # update centres
    centersHist[[it+1]] <- centers                       
  }
  
  list(clusterHist = clusterHist, centersHist = centersHist, wcssHist = wcssHist)  # out
}

k <- 2                                                     
iters <- 8                                                 
Steps <- kmeansSteps(SimData[, c("X1","X2")], k = k, iters = iters)  

for (it in 1:iters) {
  CentersBefore <- as.data.frame(Steps$centersHist[[it]])
  names(CentersBefore) <- c("X1","X2")
  CentersBefore$Cluster <- factor(1:k)
  
  CentersAfter <- as.data.frame(Steps$centersHist[[it+1]])
  names(CentersAfter) <- c("X1","X2")
  CentersAfter$Cluster <- factor(1:k)
  
  NewCluster <- factor(Steps$clusterHist[[it]], levels = 1:k)
  
  PrevCluster <- if (it == 1) rep(NA_integer_, nrow(SimData)) else Steps$clusterHist[[it-1]]
  Switched <- if (it == 1) rep(FALSE, nrow(SimData)) else (Steps$clusterHist[[it]] != PrevCluster)
  
  PlotData <- SimData
  PlotData$NewCluster <- NewCluster
  PlotData$Switched <- Switched
  
  if (it == 1) {
    pInit <- ggplot(PlotData, aes(X1, X2)) +
      geom_point(color = "black", alpha = 0.6, size = 1.5) +
      geom_point(data = CentersBefore, aes(X1, X2), inherit.aes = FALSE, shape = 4, stroke = 2.2, size = 6, color = "orange1") +
      labs(title = paste0("Iteration ", it), subtitle = "Initialize centres") +
      theme_minimal() +
      theme(panel.border = element_rect(NA, "black", 1),
            panel.grid = element_blank(),
            legend.position = "none")
    
    pAssign <- ggplot(PlotData, aes(X1, X2, color = NewCluster)) +
      geom_point(alpha = 0.65, size = 1.5) +
      scale_color_manual(values = c("navy", "darkred")) +
      geom_point(data = CentersBefore, aes(X1, X2), inherit.aes = FALSE, shape = 4, stroke = 2.2, size = 6, col = "orange") +
      labs(title = "", subtitle = "Assign clusters") +
      theme_minimal() +
      theme(panel.border = element_rect(NA, "black", 1),
            panel.grid = element_blank(),
            legend.position = "bottom")
    
    pSwitch <- ggplot(PlotData, aes(X1, X2)) +
      geom_point(aes(color = Switched), alpha = ifelse(Switched == TRUE, 1, 0), size = 1.6) +
      scale_color_manual(values = c("white", "red")) +
      geom_point(data = CentersBefore, aes(X1, X2), inherit.aes = FALSE, shape = 4, stroke = 2.2, size = 6, color = "orange") +
      labs(title = "", subtitle = "Which switched? None yet") +
      theme_minimal() +
      theme(panel.border = element_rect(NA, "black", 1),
            panel.grid = element_blank(),
            legend.position = "bottom")
    
    TraceDf <- data.frame(Iter = 1:it, WCSS = Steps$wcssHist[1:it])
    pTrace <- ggplot(TraceDf, aes(Iter, WCSS)) +
      geom_line(linewidth = 0.9) +
      geom_point(size = 2.3) +
      geom_point(data = TraceDf[it, , drop = FALSE], size = 3.5) +
      scale_x_continuous(breaks = 1:iters, limits = c(1, iters)) +
      ylim(c(1600, 2500)) +
      labs(title = "", subtitle = "WCSS Trace", x = "Iteration", y = "WCSS") +
      theme_minimal() +
      theme(panel.border = element_rect(NA, "black", 1),
            panel.grid = element_blank())
    
  } else {
    pInit <- ggplot(PlotData, aes(X1, X2)) +
      geom_point(color = "black", alpha = 0.6, size = 1.5) +
      geom_point(data = CentersBefore, aes(X1, X2), inherit.aes = FALSE, shape = 4, stroke = 2.2, size = 6, color = "orange1") +
      labs(title = paste0("Iteration ", it), subtitle = "Update centres") +
      theme_minimal() +
      theme(panel.border = element_rect(NA, "black", 1),
            panel.grid = element_blank(),
            legend.position = "none")
    
    pAssign <- ggplot(PlotData, aes(X1, X2, color = NewCluster)) +
      geom_point(alpha = 0.65, size = 1.5) +
      scale_color_manual(values = c("navy", "darkred")) +
      geom_point(data = CentersBefore, aes(X1, X2), inherit.aes = FALSE, shape = 4, stroke = 2.2, size = 6, col = "orange") +
      labs(title = "", subtitle = "Update clusters") +
      theme_minimal() +
      theme(panel.border = element_rect(NA, "black", 1),
            panel.grid = element_blank(),
            legend.position = "bottom")
    
    pSwitch <- ggplot(PlotData, aes(X1, X2)) +
      geom_point(aes(color = Switched), alpha = ifelse(Switched == TRUE, 1, 0), size = 1.6) +
      scale_color_manual(values = c("white", "red")) +
      geom_point(data = CentersBefore, aes(X1, X2), inherit.aes = FALSE, shape = 4, stroke = 2.2, size = 6, color = "orange") +
      labs(title = "", subtitle = "Which switched?") +
      theme_minimal() +
      theme(panel.border = element_rect(NA, "black", 1),
            panel.grid = element_blank(),
            legend.position = "bottom")
    
    TraceDf <- data.frame(Iter = 1:it, WCSS = Steps$wcssHist[1:it])
    pTrace <- ggplot(TraceDf, aes(Iter, WCSS)) +
      geom_line(linewidth = 0.9) +
      geom_point(size = 2.3) +
      geom_point(data = TraceDf[it, , drop = FALSE], size = 3.5) +
      scale_x_continuous(breaks = 1:iters, limits = c(1, iters)) +
      ylim(c(1600, 2500)) +
      labs(title = "", subtitle = "WCSS Trace", x = "Iteration", y = "WCSS") +
      theme_minimal() +
      theme(panel.border = element_rect(NA, "black", 1),
            panel.grid = element_blank())
  }
  
  print((pInit+pAssign+pSwitch+pTrace+plot_layout(widths = c(1,1,1,1))) & theme(legend.position = "none")) 
}

FinalCluster <- factor(Steps$clusterHist[[iters]], levels = 1:k)  

pTrue <- ggplot(SimData, aes(X1, X2, color = TrueClass)) +
  geom_point(alpha = 0.7, size = 1.6) +
  scale_color_manual(values = c("darkred", "navy")) +
  labs(title = "True Generated Clusters") +
  theme_minimal() +
  theme(panel.border = element_rect(NA, "black", 1),
        panel.grid = element_blank(),
        legend.position = "bottom")

pFinal <- ggplot(transform(SimData, FinalCluster = FinalCluster), aes(X1, X2, color = FinalCluster)) +
  geom_point(alpha = 0.7, size = 1.6) +
  scale_color_manual(values = c("navy", "darkred")) +
  labs(title = paste0("k-Means Clusters After ", iters, " Iterations")) +
  theme_minimal() +
  theme(panel.border = element_rect(NA, "black", 1),
        panel.grid = element_blank(),
        legend.position = "bottom")

print(pTrue+pFinal+plot_layout(widths = c(1, 1)))        
