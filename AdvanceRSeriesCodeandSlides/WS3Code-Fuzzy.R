library(e1071)        # fuzzy c-means implementation
library(cluster)      # clustering algorithms
library(factoextra)   # clustering visualization
library(ggplot2)      # plotting
library(dplyr)        # data manipulation
library(tidyr)        # data reshaping
library(gridExtra)    # arrange multiple plots
library(viridis)      # color palettes
library(scales)       # color interpolation


set.seed(2026)
theme_set(theme_minimal(base_size = 12))

# REVIEW OF K-MEANS CLUSTERING
# -----------------------------------------------------------------------------
nObs <- 1000
cluster1X <- rnorm(nObs/2, mean = 0, sd = 1.2)
cluster1Y <- rnorm(nObs/2, mean = 0, sd = 1.2)
cluster2X <- rnorm(nObs/2, mean = 3, sd = 1.2)
cluster2Y <- rnorm(nObs/2, mean = 3, sd = 1.2)

syntheticData <- data.frame(
  x = c(cluster1X, cluster2X),
  y = c(cluster1Y, cluster2Y),
  trueCluster = factor(rep(c(1, 2), each = nObs/2))
)

pTrueLabels <- ggplot(syntheticData, aes(x = x, y = y, color = trueCluster)) +
  geom_point(alpha = 0.7, size = 3) +
  scale_color_manual(values = c("#1B4F72", "darkred")) +
  labs(title = "True Clusters",
       x = "X1", y = "X2", color = "Cluster") +
  theme(legend.position = "bottom") +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(NA, "black", 1))

print(pTrueLabels)


kmeansResult <- kmeans(syntheticData[, c("x", "y")], centers = 2, nstart = 25)
syntheticData$kmeansCluster <- factor(kmeansResult$cluster)

# cluster centroids
kmeansCentroids <- as.data.frame(kmeansResult$centers)
colnames(kmeansCentroids) <- c("x", "y")

# k-means results
pKmeans <- ggplot(syntheticData, aes(x = x, y = y, color = kmeansCluster)) +
  geom_point(alpha = 0.7, size = 3) +
  geom_point(data = kmeansCentroids, aes(x = x, y = y), 
             color = "red", size = 5, shape = 4, stroke = 2) +
  scale_color_manual(values = c("#1B4F72", "darkred")) +
  labs(title = "K-Means",
       x = "X1", y = "X2", color = "Cluster") +
  theme(legend.position = "bottom")+
  theme(legend.position = "bottom") +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(NA, "black", 1))

print(pKmeans)


# LIMITATION OF HARD CLUSTERING
# -----------------------------------------------------------------------------

# calculate distance to each centroid for all points
distToCentroid1 <- sqrt((syntheticData$x - kmeansCentroids$x[1])^2 + 
                          (syntheticData$y - kmeansCentroids$y[1])^2)
distToCentroid2 <- sqrt((syntheticData$x - kmeansCentroids$x[2])^2 + 
                          (syntheticData$y - kmeansCentroids$y[2])^2)

syntheticData$distCentroid1 <- distToCentroid1
syntheticData$distCentroid2 <- distToCentroid2

# simple uncertainty measure --- how similar are the distances?
# ratio close to 1 = high uncertainty 
syntheticData$distRatio <- pmin(distToCentroid1, distToCentroid2) / 
  pmax(distToCentroid1, distToCentroid2)

pUncertainty <- ggplot(syntheticData, aes(x = x, y = y, color = distRatio)) +
  geom_point(alpha = 0.8, size = 3) +
  scale_color_gradient2(low = "grey90", mid = "grey40", high = "darkred",
                        midpoint = 0.5, limits = c(0, 1),
                        name = "Distance\nRatio") +
  labs(x = "X1", y = "X2") +
  theme(legend.position = "right") +
  theme(legend.position = "bottom") +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(NA, "black", 1))

print(pUncertainty)

# points in the overlap region 
overlapThreshold <- 0.7
overlapPoints <- syntheticData[syntheticData$distRatio > overlapThreshold, ]
cat("Number of points with high uncertainty (ratio > 0.7):", nrow(overlapPoints), "\n")


pOverlap <- ggplot(syntheticData, aes(x = x, y = y)) +
  geom_point(aes(color = kmeansCluster), alpha = 0.5, size = 3) +
  geom_point(data = overlapPoints, aes(x = x, y = y), 
             color = "black", size = 3, shape = 1, stroke = 1) +
  scale_color_manual(values = c("#1B4F72", "darkred")) +
  labs(x = "X1", y = "X2", color = "K-Means\nCluster") +
  theme(legend.position = "bottom")+
  theme(legend.position = "bottom") +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(NA, "black", 1))

print(pOverlap)


# FUZZY C-MEANS
# -----------------------------------------------------------------------------
fcmResult <- cmeans(syntheticData[, c("x", "y")], 
                    centers = 2,    
                    m = 2,         
                    iter.max = 100, 
                    verbose = FALSE,
                    method = "cmeans")
print(head(fcmResult$membership))

# add memberships to data
syntheticData$fcmMembership1 <- fcmResult$membership[, 1]
syntheticData$fcmMembership2 <- fcmResult$membership[, 2]
syntheticData$fcmCluster <- factor(fcmResult$cluster)  # hard assignment 


# VISUALIZING FUZZY MEMBERSHIPS
# -----------------------------------------------------------------------------
interpolateMembershipColor <- function(membership1) {
  colorCluster1 <- col2rgb("#1B4F72") / 255 
  colorCluster2 <- col2rgb("darkred") / 255  
  colorUncertain <- col2rgb("#808080") / 255 
  # calculate distance from certainty (0.5 = most uncertain)
  certainty <- abs(membership1 - 0.5) * 2  
  clusterDominant <- ifelse(membership1 < 0.5, 1, 2)
  nColors <- length(membership1)
  resultColors <- character(nColors)
  for (i in 1:nColors) {
    if (clusterDominant[i] == 1) {
      r <- colorUncertain[1] + certainty[i] * (colorCluster1[1] - colorUncertain[1])
      g <- colorUncertain[2] + certainty[i] * (colorCluster1[2] - colorUncertain[2])
      b <- colorUncertain[3] + certainty[i] * (colorCluster1[3] - colorUncertain[3])
    } else {
      r <- colorUncertain[1] + certainty[i] * (colorCluster2[1] - colorUncertain[1])
      g <- colorUncertain[2] + certainty[i] * (colorCluster2[2] - colorUncertain[2])
      b <- colorUncertain[3] + certainty[i] * (colorCluster2[3] - colorUncertain[3])
    }
    resultColors[i] <- rgb(r, g, b)
  }
  return(resultColors)
}

syntheticData$membershipColor <- interpolateMembershipColor(syntheticData$fcmMembership1)

pFuzzyMembership <- ggplot(syntheticData, aes(x = x, y = y)) +
  geom_point(color = syntheticData$membershipColor, alpha = 0.8, size = 3) +
  labs(x = "X1", y = "X2") +
  theme(legend.position = "none") +
  theme(legend.position = "bottom") +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(NA, "black", 1))

print(pFuzzyMembership)

# add cluster centroids
fcmCentroids <- as.data.frame(fcmResult$centers)
colnames(fcmCentroids) <- c("x", "y")

pFuzzyWithCentroids <- ggplot(syntheticData, aes(x = x, y = y)) +
  geom_point(color = syntheticData$membershipColor, alpha = 0.8, size = 3.5) +
  geom_point(data = fcmCentroids, aes(x = x, y = y),
             color = "red", size = 6, shape = 4, stroke = 2) +
  labs(title = "Fuzzy C-Means",
       x = "X1", y = "X2") +
  theme(legend.position = "bottom") +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(NA, "black", 1))

print(pFuzzyWithCentroids)

# COMPARING K-MEANS AND FUZZY C-MEANS
# -----------------------------------------------------------------------------

pKmeansComparison <- ggplot(syntheticData, aes(x = x, y = y, color = kmeansCluster)) +
  geom_point(alpha = 0.7, size = 3) +
  scale_color_manual(values = c("#1B4F72", "darkred")) +
  labs(title = "K-Means",
       x = "X1", y = "X2") +
  theme(legend.position = "none") +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(NA, "black", 1))

pFcmComparison <- ggplot(syntheticData, aes(x = x, y = y)) +
  geom_point(color = syntheticData$membershipColor, alpha = 0.8, size = 3) +
  labs(title = "Fuzzy C-Means",
       x = "X1", y = "X2") +
  theme(legend.position = "none") +
  theme(legend.position = "bottom") +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(NA, "black", 1))

grid.arrange(pKmeansComparison, pFcmComparison, ncol = 2)

# specific points in the overlap region
highUncertaintyIdx <- which(syntheticData$distRatio > 0.8)
overlapAnalysis <- syntheticData[highUncertaintyIdx, 
                                 c("x", "y", "trueCluster", "kmeansCluster",
                                   "fcmMembership1", "fcmMembership2")]
overlapAnalysis$maxMembership <- pmax(overlapAnalysis$fcmMembership1, 
                                      overlapAnalysis$fcmMembership2)
print(head(overlapAnalysis[order(overlapAnalysis$maxMembership), ], 10))

# EFFECT OF THE FUZZIFIER PARAMETER (m)
# -----------------------------------------------------------------------------
# test different values of m
mValues <- c(1.1, 1.5, 2, 3, 5, 10)
fuzzifierResults <- list()

for (mVal in mValues) {
  fcmTemp <- cmeans(syntheticData[, c("x", "y")], centers = 2, m = mVal, iter.max = 100)
  membershipEntropy <- -rowSums(fcmTemp$membership * log(fcmTemp$membership + 1e-10))
  fuzzifierResults[[as.character(mVal)]] <- list(
    m = mVal,
    membership = fcmTemp$membership,
    meanMaxMembership = mean(apply(fcmTemp$membership, 1, max)),
    meanEntropy = mean(membershipEntropy)
  )
}

plotsM <- list()
for(i in 1:length(mValues)){
  mVal <- mValues[i]
  tempMembership <- fuzzifierResults[[as.character(mVal)]]$membership[, 1]
  tempColors <- interpolateMembershipColor(tempMembership)
  plotsM[[i]] <- ggplot(syntheticData, aes(x = x, y = y)) +
    geom_point(color = tempColors, size = 1.5, alpha = 0.8) +
    labs(title = paste("m =", mVal)) +
    theme(axis.title = element_blank(),
          plot.title = element_text(size = 10)) +
    theme(legend.position = "bottom") +
    theme(panel.grid = element_blank(),
          panel.border = element_rect(NA, "black", 1))
}

grid.arrange(grobs = plotsM, ncol = 3)

# FUZZY CLUSTERING PERFORMANCE METRICS
# -----------------------------------------------------------------------------
# for hard clustering comparison we can still use ARI and NMI
computeARI <- function(trueLabels, predictedLabels) {
  contingency <- table(trueLabels, predictedLabels)
  a <- rowSums(contingency)
  b <- colSums(contingency)
  n <- sum(contingency)
  sumCombNij <- sum(choose(contingency, 2))
  sumCombA <- sum(choose(a, 2))
  sumCombB <- sum(choose(b, 2))
  combN <- choose(n, 2)
  expectedIndex <- sumCombA * sumCombB / combN
  maxIndex <- 0.5 * (sumCombA + sumCombB)
  ari <- (sumCombNij - expectedIndex) / (maxIndex - expectedIndex)
  return(ari)
}

computeNMI <- function(trueLabels, predictedLabels) {
  n <- length(trueLabels)
  pTrue <- table(trueLabels) / n
  hTrue <- -sum(pTrue * log(pTrue + 1e-10))
  pPred <- table(predictedLabels) / n
  hPred <- -sum(pPred * log(pPred + 1e-10))
  contingency <- table(trueLabels, predictedLabels) / n
  contingency[contingency == 0] <- 1e-10
  hJoint <- -sum(contingency * log(contingency))
  mi <- hTrue + hPred - hJoint
  nmi <- 2 * mi / (hTrue + hPred)
  return(nmi)
}

ariKmeans <- computeARI(syntheticData$trueCluster, syntheticData$kmeansCluster)
nmiKmeans <- computeNMI(syntheticData$trueCluster, syntheticData$kmeansCluster)
ariFcm <- computeARI(syntheticData$trueCluster, syntheticData$fcmCluster)
nmiFcm <- computeNMI(syntheticData$trueCluster, syntheticData$fcmCluster)

cat(sprintf("%-15s %-10.4f %-10.4f\n", "K-Means", ariKmeans, nmiKmeans))
cat(sprintf("%-15s %-10.4f %-10.4f\n", "FCM (hard)", ariFcm, nmiFcm))

# FUZZY-SPECIFIC METRICS
# -----------------------------------------------------------------------------
# partition coefficient 
computePartitionCoefficient <- function(membershipMatrix) {
  n <- nrow(membershipMatrix)
  pc <- sum(membershipMatrix^2) / n
  return(pc)
}

# normalized partition coefficient
computeNormalizedPC <- function(membershipMatrix) {
  c <- ncol(membershipMatrix)
  pc <- computePartitionCoefficient(membershipMatrix)
  npc <- (c * pc - 1) / (c - 1)
  return(npc)
}

# partition entropy 
computePartitionEntropy <- function(membershipMatrix) {
  n <- nrow(membershipMatrix)
  membershipSafe <- pmax(membershipMatrix, 1e-10)
  pe <- -sum(membershipMatrix * log(membershipSafe)) / n
  return(pe)
}

# normalized partition entropy 
computeNormalizedPE <- function(membershipMatrix) {
  c <- ncol(membershipMatrix)
  pe <- computePartitionEntropy(membershipMatrix)
  npe <- pe / log(c)
  return(npe)
}

# Xie-Beni index 
computeXieBeni <- function(data, membershipMatrix, centers, m = 2) {
  n <- nrow(data)
  c <- nrow(centers)
  numerator <- 0
  for (i in 1:n) {
    for (j in 1:c) {
      dist2 <- sum((data[i, ] - centers[j, ])^2)
      numerator <- numerator + (membershipMatrix[i, j]^m) * dist2
    }
  }
  minInterCentroid <- Inf
  for (j1 in 1:(c-1)) {
    for (j2 in (j1+1):c) {
      dist2 <- sum((centers[j1, ] - centers[j2, ])^2)
      minInterCentroid <- min(minInterCentroid, dist2)
    }
  }
  denominator <- n * minInterCentroid
  xb <- numerator / denominator
  return(xb)
}

# fuzzy silhouette
computeFuzzySilhouette <- function(data, membershipMatrix) {
  n <- nrow(data)
  c <- ncol(membershipMatrix)
  silhouettes <- numeric(n)
  for (i in 1:n) {
    sortedMembership <- sort(membershipMatrix[i, ], decreasing = TRUE, index.return = TRUE)
    cluster1 <- sortedMembership$ix[1]
    cluster2 <- sortedMembership$ix[2]
    u1 <- sortedMembership$x[1]
    u2 <- sortedMembership$x[2]
    a_i <- 0
    b_i <- 0
    weightSum1 <- 0
    weightSum2 <- 0
    for (j in 1:n) {
      if (i != j) {
        distIJ <- sqrt(sum((data[i, ] - data[j, ])^2))
        a_i <- a_i + membershipMatrix[j, cluster1] * distIJ
        weightSum1 <- weightSum1 + membershipMatrix[j, cluster1]
        b_i <- b_i + membershipMatrix[j, cluster2] * distIJ
        weightSum2 <- weightSum2 + membershipMatrix[j, cluster2]
      }
    }
    a_i <- a_i / max(weightSum1, 1e-10)
    b_i <- b_i / max(weightSum2, 1e-10)
    s_i <- (b_i - a_i) / max(a_i, b_i)
    silhouettes[i] <- (u1 - u2) * s_i
  }
  return(mean(silhouettes))
}

dataMatrix <- as.matrix(syntheticData[, c("x", "y")])
membershipMatrix <- fcmResult$membership
centersMatrix <- fcmResult$centers

pc <- computePartitionCoefficient(membershipMatrix)
npc <- computeNormalizedPC(membershipMatrix)
pe <- computePartitionEntropy(membershipMatrix)
npe <- computeNormalizedPE(membershipMatrix)
xb <- computeXieBeni(dataMatrix, membershipMatrix, centersMatrix, m = 2)
fuzzySil <- computeFuzzySilhouette(dataMatrix, membershipMatrix)

cat("\n=== Fuzzy Clustering Validity Metrics ===\n")
cat(sprintf("%-35s %-15s %-10s\n", "Metric", "Value", "Optimal"))
cat(paste(rep("-", 60), collapse = ""), "\n")
cat(sprintf("%-35s %-15.4f %-10s\n", "Partition Coefficient (PC)", pc))
cat(sprintf("%-35s %-15.4f %-10s\n", "Normalized PC", npc))
cat(sprintf("%-35s %-15.4f %-10s\n", "Partition Entropy (PE)", pe))
cat(sprintf("%-35s %-15.4f %-10s\n", "Normalized PE", npe))
cat(sprintf("%-35s %-15.4f %-10s\n", "Xie-Beni Index", xb))
cat(sprintf("%-35s %-15.4f %-10s\n", "Fuzzy Silhouette", fuzzySil))

# SELECTING THE NUMBER OF CLUSTERS
# -----------------------------------------------------------------------------
clusterRange <- 2:8
clusterMetrics <- data.frame(
  k = clusterRange,
  PC = numeric(length(clusterRange)),
  NPC = numeric(length(clusterRange)),
  PE = numeric(length(clusterRange)),
  NPE = numeric(length(clusterRange)),
  XB = numeric(length(clusterRange))
)

for (i in seq_along(clusterRange)) {
  k <- clusterRange[i]
  fcmTemp <- cmeans(dataMatrix, centers = k, m = 2, iter.max = 100, verbose = FALSE)
  clusterMetrics$PC[i] <- computePartitionCoefficient(fcmTemp$membership)
  clusterMetrics$NPC[i] <- computeNormalizedPC(fcmTemp$membership)
  clusterMetrics$PE[i] <- computePartitionEntropy(fcmTemp$membership)
  clusterMetrics$NPE[i] <- computeNormalizedPE(fcmTemp$membership)
  clusterMetrics$XB[i] <- computeXieBeni(dataMatrix, fcmTemp$membership, fcmTemp$centers, m = 2)
}

print(round(clusterMetrics,3))


metricsLong <- pivot_longer(clusterMetrics, cols = -k, names_to = "Metric", values_to = "Value")
metricsMax <- metricsLong %>% filter(Metric %in% c("PC", "NPC"))
metricsMin <- metricsLong %>% filter(Metric %in% c("PE", "NPE", "XB"))

pMetricsMax <- ggplot(metricsMax, aes(x = k, y = Value, color = Metric)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  labs(title = "Metrics to Maximize",
       x = "Number of Clusters", y = "Value") +
  scale_x_continuous(breaks = clusterRange) +
  theme(legend.position = "bottom") +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(NA, "black", 1))

pMetricsMin <- ggplot(metricsMin, aes(x = k, y = Value, color = Metric)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  labs(title = "Metrics to Minimize",
       x = "Number of Clusters", y = "Value") +
  scale_x_continuous(breaks = clusterRange) +
  theme(legend.position = "bottom") +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(NA, "black", 1))

grid.arrange(pMetricsMax, pMetricsMin, ncol = 2)


# IMAGE SEGMENTATION APPLICATION
# =============================================================================
library(imager)

prepareImageData <- function(imagePath) {
  img <- load.image(airplanesPath)
  imgWidth <- dim(img)[1]
  imgHeight <- dim(img)[2]
  nChannels <- dim(img)[4]
  
  cat("Image dimensions:", imgWidth, "x", imgHeight, "\n")
  cat("Number of channels:", nChannels, "\n")
  if (nChannels == 4) {
    # RGBA image --> extract RGB
    pixelData <- data.frame(
      x = rep(1:imgWidth, imgHeight),
      y = rep(1:imgHeight, each = imgWidth),
      R = as.vector(img[,,1,1]),
      G = as.vector(img[,,1,2]),
      B = as.vector(img[,,1,3]),
      A = as.vector(img[,,1,4])  
    )
  }
  if (nChannels == 3) {
    # color image RGB
    pixelData <- data.frame(
      x = rep(1:imgWidth, imgHeight),
      y = rep(1:imgHeight, each = imgWidth),
      R = as.vector(img[,,1,1]),
      G = as.vector(img[,,1,2]),
      B = as.vector(img[,,1,3])
    )
  } else if (nChannels == 1) {
    # grayscale
    pixelData <- data.frame(
      x = rep(1:imgWidth, imgHeight),
      y = rep(1:imgHeight, each = imgWidth),
      Gray = as.vector(img[,,1,1])
    )
  }
  return(list(
    data = pixelData,
    width = imgWidth,
    height = imgHeight,
    nChannels = nChannels,
    image = img
  ))
}

reconstructImageHard <- function(imgInfo, clusterAssignments, centers) {
  nClusters <- nrow(centers)
  nFeatures <- ncol(centers)
  if (nFeatures == 3) {
    reconstructed <- array(0, dim = c(imgInfo$width, imgInfo$height, 1, 3))
    for (k in 1:nClusters) {
      idx <- which(clusterAssignments == k)
      if (length(idx) > 0) {
        for (ch in 1:3) {
          pixelCoords <- cbind(imgInfo$data$x[idx], imgInfo$data$y[idx])
          for (i in 1:nrow(pixelCoords)) {
            reconstructed[pixelCoords[i, 1], pixelCoords[i, 2], 1, ch] <- centers[k, ch]
          }
        }
      }
    }
  } else if (nFeatures == 1) {
    reconstructed <- array(0, dim = c(imgInfo$width, imgInfo$height, 1, 1))
    for (k in 1:nClusters) {
      idx <- which(clusterAssignments == k)
      if (length(idx) > 0) {
        pixelCoords <- cbind(imgInfo$data$x[idx], imgInfo$data$y[idx])
        for (i in 1:nrow(pixelCoords)) {
          reconstructed[pixelCoords[i, 1], pixelCoords[i, 2], 1, 1] <- centers[k, 1]
        }
      }
    }
  }
  return(as.cimg(reconstructed))
}

# visualize membership uncertainty for image
visualizeMembershipUncertainty <- function(imgInfo, membershipMatrix) {
  maxMembership <- apply(membershipMatrix, 1, max)
  uncertaintyMap <- maxMembership
  uncertaintyImg <- array(uncertaintyMap, dim = c(imgInfo$width, imgInfo$height, 1, 1))
  return(as.cimg(uncertaintyImg))
}

# two objects image
demoWidth <- 100
demoHeight <- 100

demoR <- matrix(0.2, nrow = demoWidth, ncol = demoHeight)
demoG <- matrix(0.3, nrow = demoWidth, ncol = demoHeight)
demoB <- matrix(0.5, nrow = demoWidth, ncol = demoHeight)

for (i in 1:demoWidth) {
  for (j in 1:demoHeight) {
    dist1 <- sqrt((i - 30)^2 + (j - 30)^2)
    dist2 <- sqrt((i - 70)^2 + (j - 70)^2)
    if (dist1 < 20) {
      demoR[i, j] <- 0.9; demoG[i, j] <- 0.7; demoB[i, j] <- 0.3
    }
    if (dist2 < 15) {
      demoR[i, j] <- 0.3; demoG[i, j] <- 0.8; demoB[i, j] <- 0.4
    }
  }
}

# add noise
demoR <- demoR + matrix(rnorm(demoWidth * demoHeight, 0, 0.05), nrow = demoWidth)
demoG <- demoG + matrix(rnorm(demoWidth * demoHeight, 0, 0.05), nrow = demoWidth)
demoB <- demoB + matrix(rnorm(demoWidth * demoHeight, 0, 0.05), nrow = demoWidth)
demoR <- pmax(pmin(demoR, 1), 0)
demoG <- pmax(pmin(demoG, 1), 0)
demoB <- pmax(pmin(demoB, 1), 0)
demoImg <- array(c(demoR, demoG, demoB), dim = c(demoWidth, demoHeight, 1, 3))
demoImg <- as.cimg(demoImg)
plot(demoImg)


demoPixelData <- data.frame(
  x = rep(1:demoWidth, demoHeight),
  y = rep(1:demoHeight, each = demoWidth),
  R = as.vector(demoR),
  G = as.vector(demoG),
  B = as.vector(demoB)
)

demoFeatures <- as.matrix(demoPixelData[, c("R", "G", "B")])
kmeansDemo <- kmeans(demoFeatures, centers = 3, nstart = 25)
fcmDemo <- cmeans(demoFeatures, centers = 3, m = 2, iter.max = 100, verbose = FALSE)

#compare centres
print(round(kmeansDemo$centers, 3))
print(round(fcmDemo$centers, 3))

# reconstruct images
kmeansReconstructed <- reconstructImageHard(
  list(data = demoPixelData, width = demoWidth, height = demoHeight),
  kmeansDemo$cluster,kmeansDemo$centers
)

fcmReconstructed <- reconstructImageHard(
  list(data = demoPixelData, width = demoWidth, height = demoHeight),
  fcmDemo$cluster, fcmDemo$centers
)

# uncertainty map
uncertaintyMap <- visualizeMembershipUncertainty(
  list(data = demoPixelData, width = demoWidth, height = demoHeight),
  fcmDemo$membership
)

par(mfrow = c(2, 2))
plot(demoImg, main = "Original Image")
plot(kmeansReconstructed, main = "K-Means")
plot(fcmReconstructed, main = "FCM")
plot(uncertaintyMap, main = "FCM Uncertainty")
par(mfrow = c(1, 1))
