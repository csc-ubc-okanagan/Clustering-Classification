# DISTRIBUTION-BASED CLUSTERING
# -----------------------------------------------------------------------------
# SETUP AND PACKAGES
library(mclust)           # gaussian mixture models
library(mixtools)         # mixture model tools
library(ggplot2)         
library(dplyr)            
library(tidyr)            
library(gridExtra)        
library(viridis)          
library(MASS)             # multivariate normal simulation
library(mvtnorm)          # multivariate normal densities
library(factoextra)       # clustering visualization
library(cluster)          # clustering algorithms
library(dirichletprocess) # Dirichlet process
library(corrplot)

set.seed(2026)
theme_set(theme_minimal(base_size = 12))

# -----------------------------------------------------------------------------
# LIMITATION OF K-MEANS
cluster1 <- mvrnorm(100, mu = c(0, 0), Sigma = diag(2) * 0.5)
sigma2 <- matrix(c(4, 2.5, 2.5, 2), nrow = 2)
cluster2 <- mvrnorm(150, mu = c(5, 3), Sigma = sigma2)
cluster3 <- mvrnorm(80, mu = c(2, 6), Sigma = diag(2) * 1.5)

syntheticData <- data.frame(
  x = c(cluster1[,1], cluster2[,1], cluster3[,1]),
  y = c(cluster1[,2], cluster2[,2], cluster3[,2]),
  trueCluster = factor(c(rep(1, 100), rep(2, 150), rep(3, 80))))

# sample dataset
pTrueLabels <- ggplot(syntheticData, aes(x = x, y = y, color = trueCluster)) +
  geom_point(alpha = 0.7, size = 2) +
  scale_color_manual(values = c("#1B4F72", "#145A32", "#7D3C98")) +
  labs(x = "X", y = "Y", color = "Cluster") +
  theme(legend.position = "bottom") +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(NA, "black", 1))
pTrueLabels

# kmeans
kmeansResult <- kmeans(syntheticData[, c("x", "y")], centers = 3, nstart = 25)
syntheticData$kmeansCluster <- factor(kmeansResult$cluster)

# visualize k-means result
pKmeans <- ggplot(syntheticData, aes(x = x, y = y, color = kmeansCluster)) +
  geom_point(alpha = 0.7, size = 2) +
  scale_color_manual(values = c("#1B4F72", "#145A32", "#7D3C98")) +
  labs(title = "K-Means Result",
       x = "X", y = "Y", color = "Cluster") +
  theme(legend.position = "bottom") +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(NA, "black", 1))
pKmeans

# -----------------------------------------------------------------------------
# GAUSSIAN MIXTURE MODELS
# model data as coming from a mixture of gaussian distributions
# each cluster is a multivariate normal with its own mean and covariance

# mclust automatically selects the number of components using BIC
# it also tests different covariance structures
gmmAuto <- Mclust(syntheticData[, c("x", "y")])
gmmAuto$modelName         # model chosen
gmmAuto$G                 # num. clusters chosen
round(gmmAuto$loglik, 2)  # model is chosen by log likelihood
round(gmmAuto$bic, 2)     # BIC model selection value

gmm3 <- Mclust(syntheticData[, c("x", "y")], G = 3)
syntheticData$gmmCluster <- factor(gmm3$classification)

# z is the posterior probabilities, which is for each observation 
# models the probability that the observation belongs in cluster j
# similar to the idea of fuzzy c-means!
posteriorProbs <- gmm3$z
colnames(posteriorProbs) <- paste0("pclust", 1:ncol(posteriorProbs))

# then just like fuzzy cmeans, we assign a hard cluster
# by doing the max probability for each observation
syntheticData$gmmMaxProb <- apply(posteriorProbs, 1, max)
round(gmm3$parameters$pro, 4)    # gmm parameters
round(gmm3$parameters$mean, 4)   # cluster centroids centroids

# cluster covariance estimates
for (k in 1:3) {
  cat("\nCluster", k, ":\n")
  print(round(gmm3$parameters$variance$sigma[,,k], 4))
}

# -----------------------------------------------------------------------------
# VISUALIZING GMM RESULTS

pGmm <- ggplot(syntheticData, aes(x = x, y = y, color = gmmCluster)) +
  geom_point(alpha = 0.7, size = 2) +
  scale_color_manual(values = c("#1B4F72", "#145A32", "#7D3C98")) +
  labs(title = "GMM Results", x = "X", 
       y = "Y", color = "Cluster") +
  theme(legend.position = "bottom") +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(NA, "black", 1))
pGmm

grid.arrange(pKmeans + ggtitle("K-Means") + labs(subtitle = ""), 
             pGmm + ggtitle("GMM") + labs(subtitle = ""), 
             ncol = 2)

# we can visualize the probabilistic cluster assignments like
# with fuzzy c-means
syntheticData$uncertainty <- 1 - syntheticData$gmmMaxProb
pUncertainty <- ggplot(syntheticData, aes(x = x, y = y, color = uncertainty)) +
  geom_point(alpha = 0.8, size = 2) +
  scale_color_gradient(low = "navy", high = "gold", name = "Uncertainty") +
  labs(title = "GMM Uncertainty",
       x = "X", y = "Y") +
  theme(legend.position = "bottom") +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(NA, "black", 1))
pUncertainty

# -----------------------------------------------------------------------------
# VISUALIZING GAUSSIAN ELLIPSES

generateEllipse <- function(mu, sigma, nPoints = 100, level = 0.95) {
  eigenDecomp <- eigen(sigma)
  eigenValues <- eigenDecomp$values
  eigenVectors <- eigenDecomp$vectors
  chiSqVal <- qchisq(level, df = 2)
  theta <- seq(0, 2 * pi, length.out = nPoints)
  circle <- cbind(cos(theta), sin(theta))
  ellipse <- circle %*% diag(sqrt(eigenValues * chiSqVal)) %*% t(eigenVectors)
  ellipse <- sweep(ellipse, 2, mu, "+")
  return(as.data.frame(ellipse))
}

ellipseData <- data.frame()
for (k in 1:3) {
  ellipse <- generateEllipse(
    mu = gmm3$parameters$mean[, k],
    sigma = gmm3$parameters$variance$sigma[,, k],
    level = 0.95
  )
  ellipse$cluster <- factor(k)
  ellipseData <- rbind(ellipseData, ellipse)
}
colnames(ellipseData)[1:2] <- c("x", "y")

pGmmEllipses <- ggplot() +
  geom_point(data = syntheticData, aes(x = x, y = y, color = gmmCluster), 
             alpha = 0.5, size = 2) +
  geom_path(data = ellipseData, aes(x = x, y = y, color = cluster), 
            linewidth = 1.2) +
  geom_point(data = as.data.frame(t(gmm3$parameters$mean)), 
             aes(x = x, y = y), color = "red", size = 4, shape = 4, stroke = 2) +
  scale_color_manual(values = c("#1B4F72", "#145A32", "#7D3C98")) +
  labs(title = "GMM with 95% Confidence Ellipses",
       x = "X", y = "Y", color = "Cluster") +
  theme(legend.position = "bottom") +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(NA, "black", 1))
pGmmEllipses

# -----------------------------------------------------------------------------
# CHOOSING NUMBER OF COMPONENTS

# BIC (Bayesian Information Criterion) is standard for GMM model selection
# BIC = -2 * log-likelihood + k * log(n)
# k is number of parameters, n is sample size
# higher BIC is better in mclust (they use a different sign convention)

bicValues <- mclustBIC(syntheticData[, c("x", "y")], G = 1:9)
summary(bicValues)
plot(bicValues, main = "BIC for GMM Model Selection")

# extract BIC for VVV model 
bicVVV <- bicValues[, "VVV"]
bicDf <- data.frame(k = 1:9, BIC = bicVVV)

pBic <- ggplot(bicDf, aes(x = k, y = BIC)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_vline(xintercept = which.max(bicVVV), linetype = "dashed", color = "red") +
  labs(title = "BIC for VVV Model (Flexible Covariance)",
       x = "Number of Components", y = "BIC") +
  scale_x_continuous(breaks = 1:9) +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(NA, "black", 1))
pBic

# we can compare to AIC or ICL 
# ICL (Integrated Complete-data Likelihood) penalizes for entropy
iclValues <- mclustICL(syntheticData[, c("x", "y")], G = 1:9)
summary(iclValues) # top 2 models the same as BIC

# THE EM ALGORITHM IS HOW A GMM IS FITTED
# -----------------------------------------------------------------------------
# lets visualize the process
dataMatrix <- as.matrix(syntheticData[, c("x", "y")])
emResult <- mvnormalmixEM(dataMatrix, k = 3, verb = TRUE, epsilon = 1e-6)

if (length(emResult$all.loglik) > 1) {
  llDf <- data.frame(
    iteration = 1:length(emResult$all.loglik),
    logLik = emResult$all.loglik)
  pLogLik <- ggplot(llDf, aes(x = iteration, y = logLik)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    labs(title = "EM Algorithm Convergence",
         subtitle = "Log-likelihood increases until convergence",
         x = "Iteration", y = "Log-Likelihood")
  print(pLogLik)
}

# GMM PERFORMANCE METRICS
# -----------------------------------------------------------------------------
# ARI as before (adjustedRandIndex in mclust does this already, 
# but I put it here to remind us of the formulation)
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

# NMI as before
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
ariGmm <- computeARI(syntheticData$trueCluster, syntheticData$gmmCluster)
nmiGmm <- computeNMI(syntheticData$trueCluster, syntheticData$gmmCluster)

rbind(cat(sprintf("%-15s %-10.4f %-10.4f\n", "K-Means", ariKmeans, nmiKmeans)),
cat(sprintf("%-15s %-10.4f %-10.4f\n", "GMM", ariGmm, nmiGmm)))

# GMM-specific metrics
# average log-likelihood per observation
avgLogLik <- gmm3$loglik / nrow(syntheticData)
round(avgLogLik, 4)

# entropy of soft assignments, where lower is more certain
entropy <- -mean(rowSums(posteriorProbs * log(posteriorProbs + 1e-10)))
round(entropy, 4)

# average maximum posterior, where higher is more confident
avgMaxPost <- mean(syntheticData$gmmMaxProb)
round(avgMaxPost, 4)

# ALTERNATIVE TO MCLUST, WE HAVE DIRICHLET PROCESS MIXTURE MODELS (DPMM)
# -----------------------------------------------------------------------------
# Basically, for GMM we estimate k from metrics such as BIC
# but what if the number of clusters is truly unknown?
# DPMM allows for k to be inferred from the data, where
# there may be potentially any number of clusters
# its a Bayesian nonparametric approach, and therefore is often slow.

scaledData <- scale(syntheticData[, c("x", "y")])
dpObj <- DirichletProcessMvnormal(scaledData, alphaPriors = c(4,8))
dpFit <- Fit(dpObj, 500, progressBar = TRUE) 

# we should do more than 500 iterations above, but its slow.
dpClusters <- dpFit$clusterLabels
syntheticData$dpmmCluster <- factor(dpClusters)
nClustersFound <- length(unique(dpClusters))
nClustersFound 
table(dpClusters)
  
ariDpmm <- computeARI(syntheticData$trueCluster, syntheticData$dpmmCluster)
nmiDpmm <- computeNMI(syntheticData$trueCluster, syntheticData$dpmmCluster)
round(ariDpmm, 4)
round(nmiDpmm, 4)
  
pDpmm <- ggplot(syntheticData, aes(x = x, y = y, color = dpmmCluster)) +
  geom_point(alpha = 0.7, size = 2) +
  labs(title = "DPMM Results",
         x = "X", y = "Y", color = "Cluster") +
  scale_color_manual(values = c("#1B4F72", "#145A32", "#7D3C98", "red")) +
  theme(legend.position = "bottom") +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(NA, "black", 1))
pDpmm
  
# EFFECT OF CONCENTRATION PARAMETER
# -----------------------------------------------------------------------------
# demonstrate how alpha affects clustering
# using stick-breaking representation
simulateStickBreaking <- function(alpha, nDraws = 20) {
  betas <- rbeta(nDraws, 1, alpha)
  weights <- numeric(nDraws)
  remaining <- 1
  for (i in 1:nDraws) {
    weights[i] <- betas[i] * remaining
    remaining <- remaining * (1 - betas[i])
  }
  return(weights)
}

alphaValues <- c(0.1, 1.0, 3.0, 5.0, 10.0, 20.0)
sbResults <- list()

for (alpha in alphaValues) {
  set.seed(2026) 
  weights <- simulateStickBreaking(alpha, nDraws = 20)
  sbResults[[as.character(alpha)]] <- data.frame(
    component = 1:20,
    weight = weights,
    alpha = alpha
  )
}
sbDf <- do.call(rbind, sbResults)

pStickBreaking <- ggplot(sbDf, aes(x = component, y = weight, fill = alpha)) +
  geom_bar(stat = "identity") +
  facet_wrap(~alpha, scales = "free_y") +
  labs(title = "Effects of Different Alpha Values",
       x = "Component", y = "Weight") +
  theme(legend.position = "none")
pStickBreaking

# alpha = 0.1: most mass on 1-2 clusters
# alpha = 1: moderate spread across clusters
# alpha = 5: more clusters receive more mass
# alpha = 20: many small clusters 

# COMPARISON OF ALL METHODS
# -----------------------------------------------------------------------------
pComparison <- grid.arrange(
  pKmeans + ggtitle("K-Means") + theme(legend.position = "none"),
  pGmm + ggtitle("GMM") + theme(legend.position = "none"),
  if (exists("pDpmm")) pDpmm + ggtitle("DPMM") + theme(legend.position = "none") 
  else pGmm + ggtitle("GMM (auto K)") + theme(legend.position = "none"),
  pTrueLabels + ggtitle("True Labels") + theme(legend.position = "none"),
  ncol = 2)

# ============================================
# APPLICATION
# ============================================
data(wdbc)
# the wdbc dataset has Diagnosis in second column
# the first column is irrelevant for clustering
Diagnosis <- wdbc[, 2] # M = malignant, B = benign
features <- wdbc[, -c(1,2)]        
table(Diagnosis)
summary(features)
# Ten features computed for each cell nucleus:
# 1. Radius as mean of distances from center to points on perimeter
# 2. Texture as standard deviation of gray-scale values
# 3. Perimeter
# 4. Area
# 5. Smoothness as local variation in radius lengths
# 6. Compactness as perimeter^2 / area - 1.0)
# 7. Concavity as severity of concave portions of contour
# 8. Concave points as number of concave portions of contour
# 9. Symmetry
# 10. Fractal dimension 

# For each feature the mean, standard deviation and extreme cases are recorded
# each in their own own column, for 30 total features

################# DISCLAIMER ##############################
##### WE HAVE NOT DISCUSSED WHICH VARIABLES ARE MOST ######
######## IMPORTANT TO DETERMINING DIAGNOSIS HERE ##########
### WE ARE ONLY EXPLORING 3 DIFFERENT POTENTIAL OPTIONS ###
######## CONSIDER TRYING DIFFERENT COMBINATIONS ###########
#### OR REACHERCHING VARIABLE SELECTION TECHNIQUES ########
#####(WHICH WE WILL DISCUSS IN A FUTURE WORKSHOP) #########

# LETS EXPLORE THE DATA
# -----------------------------------------------------------------------------

keyFeatures <- c("Radius_mean", "Texture_mean", 
                 "Perimeter_mean", "Area_mean", "Concavity_mean")
summaryByDiagnosis <- data.frame(Diagnosis = Diagnosis, features[, keyFeatures]) %>%
  group_by(Diagnosis) %>%
  summarise(across(everything(), list(mean = mean, sd = sd)))
summaryByDiagnosis

# lets visualize by plotting a couple variables against each other
wdbcDf <- data.frame(Diagnosis = Diagnosis, features)

# radius vs texture (mean values)
pScatter1 <- ggplot(wdbcDf, aes(x = Radius_mean, y = Texture_mean, color = Diagnosis)) +
  geom_point(alpha = 0.6, size = 2) +
  scale_color_manual(values = c("B" = "#3498DB", "M" = "#E74C3C"),
                     labels = c("B" = "Benign", "M" = "Malignant")) +
  labs(title = "Radius vs Texture (Mean)",
       x = "Mean Radius", y = "Mean Texture") +
  theme(legend.position = "bottom") +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(NA, "black", 1))

# area vs concavity (mean values)
pScatter2 <- ggplot(wdbcDf, aes(x = Area_mean, y = Concavity_mean, color = Diagnosis)) +
  geom_point(alpha = 0.6, size = 2) +
  scale_color_manual(values = c("B" = "#3498DB", "M" = "#E74C3C"),
                     labels = c("B" = "Benign", "M" = "Malignant")) +
  labs(title = "Area vs Concavity (Mean)",
       x = "Mean Area", y = "Mean Concavity") +
  theme(legend.position = "bottom") + 
  theme(panel.grid = element_blank(),
        panel.border = element_rect(NA, "black", 1))

grid.arrange(pScatter1, pScatter2, ncol = 1)

# boxplots
wdbcLong <- wdbcDf %>%
  dplyr::select(Diagnosis, all_of(keyFeatures)) %>%
  pivot_longer(cols = -Diagnosis, names_to = "Feature", values_to = "Value")

pBoxplots <- ggplot(wdbcLong, aes(x = Diagnosis, y = Value, fill = Diagnosis)) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~Feature, scales = "free_y", ncol = 3) +
  scale_fill_manual(values = c("B" = "#3498DB", "M" = "#E74C3C"),
                    labels = c("B" = "Benign", "M" = "Malignant")) +
  labs(title = "Distribution of Select Features by Diagnosis",
       x = "Diagnosis", y = "Value") +
  theme(legend.position = "bottom") +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(NA, "black", 1))
pBoxplots

# lets look at correlation for mean features
meanFeatures <- features[, 1:10]
corrMatrix <- cor(meanFeatures)
corrplot(corrMatrix, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45,
         title = "Mean Features",
         mar = c(0, 0, 2, 0))

# clearly many features are highly correlated but
# for demonstration, we'll use all features.
featuresScaled <- scale(features)

# for visualization, we'll work with just mean features
meanFeaturesScaled <- scale(meanFeatures)

# GMM WITH AUTOMATIC MODEL SELECTION
# -----------------------------------------------------------------------------
# use BIC to select model (SLOW)
gmmBicAll <- mclustBIC(featuresScaled, G = 1:9)
plot(gmmBicAll)
summary(gmmBicAll)

gmmOptimalAll <- Mclust(featuresScaled, x = gmmBicAll)
gmmOptimalAll$modelName
gmmOptimalAll$G               # 7 clusters!?
round(gmmOptimalAll$loglik, 2)
round(gmmOptimalAll$bic, 2)

# lets just look at the mean features only now
# as it will reduce some of the high correlations
gmmBicMean <- mclustBIC(meanFeaturesScaled, G = 1:9)
plot(gmmBicMean)
summary(gmmBicMean)

gmmOptimalMean <- Mclust(meanFeaturesScaled, x = gmmBicMean)
gmmOptimalMean$modelName
gmmOptimalMean$G              # 5 clusters...
round(gmmOptimalMean$bic, 2)

# lastly, we will fit with k = 2 in case the above
# suggested a number other than 2 (classification,
# since we know there are two clusters M and B)
gmmK2 <- Mclust(meanFeaturesScaled, G = 2)
gmmK2$modelName
round(gmmK2$loglik, 2)
round(gmmK2$bic, 2)

# lets compare the results
gmmClustersAll <- gmmOptimalAll$classification
gmmClustersMean <- gmmOptimalMean$classification
gmmClustersK2 <- gmmK2$classification
trueLabels <- as.numeric(factor(Diagnosis))

ariAll <- computeARI(trueLabels, gmmClustersAll)
nmiAll <- computeNMI(trueLabels, gmmClustersAll)
ariMean <- computeARI(trueLabels, gmmClustersMean)
nmiMean <- computeNMI(trueLabels, gmmClustersMean)
ariK2 <- computeARI(trueLabels, gmmClustersK2)
nmiK2 <- computeNMI(trueLabels, gmmClustersK2)

rbind(
cat(sprintf("%-30s %-8d %-8.4f %-8.4f\n", "GMM All Features (auto)", gmmOptimalAll$G, ariAll, nmiAll)),
cat(sprintf("%-30s %-8d %-8.4f %-8.4f\n", "GMM Mean Features (auto)", gmmOptimalMean$G, ariMean, nmiMean)),
cat(sprintf("%-30s %-8d %-8.4f %-8.4f\n", "GMM Mean Features (K=2)", 2, ariK2, nmiK2)))

confusionMatrix <- table(True = Diagnosis, GMM = gmmClustersK2)
confusionMatrix

# VISUALIZING GMM RESULTS
# -----------------------------------------------------------------------------
resultsDf <- data.frame(Diagnosis = Diagnosis,
  GMM_Cluster = factor(gmmClustersK2),
  meanFeaturesScaled)

pGmmVsTrue1 <- ggplot(resultsDf, aes(x = Radius_mean, y = Concavity_mean, color = Diagnosis)) +
  geom_point(alpha = 0.6, size = 2) +
  scale_color_manual(values = c("B" = "#3498DB", "M" = "#E74C3C"),
                     labels = c("B" = "Benign", "M" = "Malignant")) +
  labs(title = "True Diagnosis",
       x = "Radius (scaled)", y = "Concavity (scaled)") +
  theme(legend.position = "bottom") +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(NA, "black", 1))

pGmmVsTrue2 <- ggplot(resultsDf, aes(x = Radius_mean, y = Concavity_mean, color = GMM_Cluster)) +
  geom_point(alpha = 0.6, size = 2) +
  scale_color_manual(values = c("2" = "#3498DB", "1" = "#E74C3C")) +
  labs(title = "GMM Results",
       x = "Radius (scaled)", y = "Concavity (scaled)") +
  theme(legend.position = "bottom") +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(NA, "black", 1))
grid.arrange(pGmmVsTrue1, pGmmVsTrue2, ncol = 2)

# mclust also has some cool plots
plot(gmmK2, what = "classification")
plot(gmmK2, what = "uncertainty")


# POSTERIOR PROBABILITIES AND UNCERTAINTY
# -----------------------------------------------------------------------------
posteriorProbs <- gmmK2$z
colnames(posteriorProbs) <- c("pclust1", "pclust2")

resultsDf$pclust1 <- posteriorProbs[, 1]
resultsDf$pclust2 <- posteriorProbs[, 2]
resultsDf$maxPosterior <- apply(posteriorProbs, 1, max)
resultsDf$uncertainty <- 1 - resultsDf$maxPosterior
round(mean(resultsDf$uncertainty), 4) # average uncertainty
round(max(resultsDf$uncertainty), 4)  # maximum uncertainty

# lets look at uncertain cases, where posterior < 0.8
uncertainThreshold <- 0.8
uncertainCases <- resultsDf[resultsDf$maxPosterior < uncertainThreshold, ]
nrow(uncertainCases) # only 14 cases
table(uncertainCases$Diagnosis)

# visualize uncertainty
pUncertainty <- ggplot(resultsDf, aes(x = Radius_mean, y = Concavity_mean, color = uncertainty)) +
  geom_point(alpha = 0.8, size = 2) +
  scale_color_gradient(low = "navy", high = "gold", name = "Uncertainty") +
  labs(title = "GMM Uncertainty",
       x = "Radius (scaled)", y = "Concavity (scaled)") +
  theme(legend.position = "right") +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(NA, "black", 1))
pUncertainty # makes sense

# determine which cluster corresponds to which Diagnosis
clusterToDiagnosis <- if (sum(Diagnosis == "M" & 
                              gmmClustersK2 == 2) > sum(Diagnosis == "M" &
                              gmmClustersK2 == 1)) c("B", "M") else c("M", "B")  

resultsDf$predictedDiagnosis <- clusterToDiagnosis[gmmClustersK2]
resultsDf$correct <- resultsDf$Diagnosis == resultsDf$predictedDiagnosis
sum(resultsDf$correct) # correct
sum(!resultsDf$correct) # misclassified

# who'se misclassified and why?
misclassified <- resultsDf[!resultsDf$correct, ]

# False Positives (Benign classified as Malignant) 
sum(misclassified$Diagnosis == "B")
# False Negatives (Malignant classified as Benign) 
sum(misclassified$Diagnosis == "M")

# correct vs misclassified with uncertainty
pMisclass <- ggplot(resultsDf, aes(x = Radius_mean, y = Concavity_mean, 
                                   color = correct, size = uncertainty)) +
  geom_point(alpha = 0.6) +
  scale_color_manual(values = c("TRUE" = "forestgreen", "FALSE" = "red"),
                     labels = c("TRUE" = "Correct", "FALSE" = "Misclassified")) +
  scale_size_continuous(range = c(1, 5)) +
  labs(title = "Accuracy with Uncertainty",
       x = "Radius (scaled)", y = "Concavity (scaled)",
       color = "Accuracy", size = "Uncertainty") +
  theme(legend.position = "right") +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(NA, "black", 1))
pMisclass

# COMPARISON WITH K-MEANS
# -----------------------------------------------------------------------------
kmeansResult <- kmeans(meanFeaturesScaled, centers = 2, nstart = 25)
ariKmeans <- computeARI(trueLabels, kmeansResult$cluster)
nmiKmeans <- computeNMI(trueLabels, kmeansResult$cluster)
rbind(cat(sprintf("%-20s %-10.4f %-10.4f\n", "K-Means", ariKmeans, nmiKmeans)),
cat(sprintf("%-20s %-10.4f %-10.4f\n", "GMM", ariK2, nmiK2)))
resultsDf$kmeansCluster <- factor(kmeansResult$cluster)

computeARI(kmeansResult$cluster, gmmK2$classification)
computeNMI(kmeansResult$cluster, gmmK2$classification)

table(kmeansResult$cluster, gmmK2$classification)
table(trueLabels, gmmK2$classification)
table(trueLabels, kmeansResult$cluster)

round(kmeansResult$centers,2)
round(gmmK2$parameters$mean, 2)

pKmeans <- ggplot(resultsDf, aes(x = Radius_mean, y = Concavity_mean, color = kmeansCluster)) +
  geom_point(alpha = 0.6, size = 2) +
  scale_color_manual(values = c("1" = "#3498DB", "2" = "#E74C3C")) +
  labs(title = "K-Means Cluistering",
       x = "Radius (scaled)", y = "Concavity (scaled)") +
  theme(legend.position = "none") +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(NA, "black", 1))

pGmm <- ggplot(resultsDf, aes(x = Radius_mean, y = Concavity_mean, color = GMM_Cluster)) +
  geom_point(alpha = 0.6, size = 2) +
  scale_color_manual(values = c("2" = "#3498DB", "1" = "#E74C3C")) +
  labs(title = "GMM Clustering",
       x = "Radius (scaled)", y = "Concavity (scaled)") +
  theme(legend.position = "none") +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(NA, "black", 1))

pTrue <- ggplot(resultsDf, aes(x = Radius_mean, y = Concavity_mean, color = Diagnosis)) +
  geom_point(alpha = 0.6, size = 2) +
  scale_color_manual(values = c("B" = "#3498DB", "M" = "#E74C3C")) +
  labs(title = "True Diagnosis",
       x = "Radius (scaled)", y = "Concavity (scaled)") +
  theme(legend.position = "none") +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(NA, "black", 1))

grid.arrange(pTrue, pKmeans, pGmm, ncol = 3)

# PROBLEM SPECIFIC INQUIRIES
# -----------------------------------------------------------------------------
# risk groups from posterior probabilities
malignantCluster <- which.max(c(
  sum(Diagnosis == "M" & gmmClustersK2 == 1),
  sum(Diagnosis == "M" & gmmClustersK2 == 2)
))

resultsDf$malignantProbability <- posteriorProbs[, malignantCluster]

resultsDf$riskCategory <- cut(
  resultsDf$malignantProbability,
  breaks = c(0, 0.2, 0.5, 0.8, 1),
  labels = c("Low Risk", "Moderate Risk", "High Risk", "Very High Risk"),
  include.lowest = TRUE
)

riskTable <- table(resultsDf$riskCategory, resultsDf$Diagnosis)
riskTable
table(resultsDf$riskCategory)

pRisk <- ggplot(resultsDf, aes(x = Radius_mean, y = Concavity_mean, color = riskCategory)) +
  geom_point(alpha = 0.7, size = 2) +
  scale_color_manual(values = c("Low Risk" = "#27AE60", 
                                "Moderate Risk" = "blue",
                                "High Risk" = "blue",
                                "Very High Risk" = "#E74C3C")) +
  labs(title = "Risk Groups",
       x = "Radius (scaled)", y = "Concavity (scaled)",
       color = "Risk Category") +
  theme(legend.position = "right") +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(NA, "black", 1))
pRisk
