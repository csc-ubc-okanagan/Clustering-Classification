library(ggplot2)     # viz
library(cluster)     # silhouette()
library(factoextra)  # (as in slides; optional)
library(dplyr)       # (as in slides; optional)

X <- as.matrix(iris[, 1:4])                         # feats (4 numeric cols)
trueLabels <- iris$Species                          # true spp labels

D <- dist(X, method = "euclidean")                  # Euclid dist on cm-scale vars

hcComplete <- hclust(D, method = "complete")        # HC w/ complete linkage
hcWard <- hclust(D, method = "ward.D2")             # HC w/ Ward.D2 (min SSE incr)

plot(hcComplete, main = "Complete Linkage", xlab = "", sub = "")  # dendro
plot(hcWard, main = "Ward's Method", xlab = "", sub = "")         # dendro

calcWSS <- function(X, clusters) {                  # within-SS given partition
  wss <- 0
  for(k in unique(clusters)) {
    clusterData <- X[clusters == k, ]
    center <- colMeans(clusterData)
    wss <- wss + sum(rowSums((clusterData - center)^2))
  }
  return(wss)
}

kVals <- 2:6                                        # k candidates
wssComplete <- numeric(length(kVals))               # WSS for complete @ each k
wssWard <- numeric(length(kVals))                   # WSS for Ward @ each k

for(i in seq_along(kVals)) {
  k <- kVals[i]
  clComplete <- cutree(hcComplete, k)               # determine cluster ids (complete)
  clWard <- cutree(hcWard, k)                       # determine cluster ids (Ward)
  wssComplete[i] <- calcWSS(X, clComplete)          # compute WSS (complete)
  wssWard[i] <- calcWSS(X, clWard)                  # compute WSS (Ward)
}

elbowData <- data.frame(
  k = rep(kVals, 2),
  WSS = c(wssComplete, wssWard),
  Method = rep(c("Complete", "Ward"), each = length(kVals))
)                                                   # long format for ggplot

ggplot(elbowData, aes(x = k, y = WSS, color = Method)) +
  geom_line(size = 1) + geom_point(size = 3) +
  labs(title = "Elbow", x = "Number of Clusters", y = "Within-Sum-of-Squares") +
  theme_minimal()

silComplete <- numeric(length(kVals))               # mean sil (complete) per k
silWard1 <- numeric(length(kVals))                  # mean sil (Ward) per k

for(i in seq_along(kVals)) {
  k <- kVals[i]
  clComplete <- cutree(hcComplete, k)               # cluster ids from complete dendro
  silComp <- silhouette(clComplete, D)              # sil widths given D + labels
  silComplete[i] <- mean(silComp[, 3])              # avg sil (complete)
  clWard <- cutree(hcWard, k)                       # cluster ids from Ward dendro
  silWard <- silhouette(clWard, D)                  # sil widths (Ward)
  silWard1[i] <- mean(silWard[, 3])                 # avg sil (Ward)
}

kOptComp <- kVals[which.max(silComplete)]           # k maximizing sil (complete)
kOptWard <- kVals[which.max(silWard1)]              # k maximizing sil (Ward)

silData <- data.frame(
  k = rep(kVals, 2),
  Silhouette = c(silComplete, silWard1),           
  Method = rep(c("Complete", "Ward"), each = length(kVals))
)                                                   # long format for ggplot

ggplot(silData, aes(x = k, y = Silhouette, color = Method)) +
  geom_line(size = 1) + geom_point(size = 3) +
  labs(title = "Silhouette Analysis", x = "Number of Clusters", y = "Mean Silhouette Width") +
  theme_minimal()

kFinal <- 2                                         # final k used for scatter demo
clusComplete <- cutree(hcComplete, kFinal)          # cluster assignment (complete, k=2)
clusWard <- cutree(hcWard, kFinal)                  # cluster assignment (Ward, k=2)

irisPlot <- iris                                    # copy for plotting
irisPlot$clusterComplete <- factor(clusComplete)    # add complete clusters as factor
irisPlot$clusterWard <- factor(clusWard)            # add Ward clusters as factor

p1 <- ggplot(irisPlot, aes(x = Petal.Length, y = Petal.Width, color = clusterComplete)) +
  geom_point(size = 2) +
  labs(title = "Complete Linkage (k=2)", color = "Cluster") +
  theme_minimal()

p2 <- ggplot(irisPlot, aes(x = Petal.Length, y = Petal.Width, color = clusterWard)) +
  geom_point(size = 2) +
  labs(title = "Ward's Method (k=2)", color = "Cluster") +
  theme_minimal()

print(p1)                                           # show complete scatter
print(p2)                                           # show Ward scatter

clusComplete <- cutree(hcComplete, 3)               # cluster assignment (complete, k=3)
clusWard <- cutree(hcWard, 3)                       # cluster assignment (Ward, k=3)
confMatComp <- table(trueLabels, clusComplete)      # confusion matrix (complete)
confMatWard <- table(trueLabels, clusWard)          # confusion matrix (Ward)
print(confMatComp)                                  # print CM
print(confMatWard)                                  # print CM

library(clue)                                       # solve_LSAP() for best label map
calcAccuracy <- function(true, pred) {
  tab <- table(true, pred)                          # counts true vs cluster id
  matches <- solve_LSAP(tab, maximum = TRUE)        # permute cluster ids to max diag
  sum(diag(tab[, matches])) / length(true)          # acc after optimal relabel
}

accComplete <- calcAccuracy(trueLabels, clusComplete) # acc (complete, k=3)
accWard <- calcAccuracy(trueLabels, clusWard)         # acc (Ward, k=3)

library(mclust)                                     # adjustedRandIndex()
ariComp <- adjustedRandIndex(trueLabels, clusComplete) # ARI (complete, k=3)
ariWard <- adjustedRandIndex(trueLabels, clusWard)     # ARI (Ward, k=3)

clusComplete_k2 <- cutree(hcComplete, 2)            # cluster assignment (complete, k=2)
clusWard_k2 <- cutree(hcWard, 2)                    # cluster assignment (Ward, k=2)

ariComp_k2 <- adjustedRandIndex(trueLabels, clusComplete_k2) # ARI (complete, k=2)
ariWard_k2 <- adjustedRandIndex(trueLabels, clusWard_k2)     # ARI (Ward, k=2)

print(list(kOptComp = kOptComp, kOptWard = kOptWard))         # best k by sil
print(list(accComplete = accComplete, accWard = accWard))     # acc (k=3)
print(list(ari_k3 = c(Complete = ariComp, Ward = ariWard),
           ari_k2 = c(Complete = ariComp_k2, Ward = ariWard_k2))) # ARI

