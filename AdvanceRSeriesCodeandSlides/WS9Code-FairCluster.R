# BIAS REDUCTION AND FAIRNESS IN CLUSTERING
# Workshop 9: Identifying and Mitigating Bias
library(cluster)
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
library(viridis)

set.seed(2026)
theme_set(theme_minimal(base_size = 12))


# WHY FAIRNESS IN CLUSTERING?
# =============================================================================

# clustering is often used for consequential decisions such as
# - credit scoring and loan approvals
# - healthcare resource allocation
# - criminal justice risk assessment
# - hiring and employee segmentation

# if clusters correlate with protected attributes (race, sex, age),
# the resulting clusters may be biased towards/against these attributes
# since data is by humans, and humans can be intentionally or 
# unintentionally biased. 

# So today, we will just look at a real dataset that
# has real-life consequences. This received public scrutiny, and you will soon
# see why.

# COMPAS RECIDIVISM DATA
# =============================================================================

# COMPAS (Correctional Offender Management Profiling for Alternative Sanctions)
# used by US courts to assess likelihood of reoffending
# ProPublica investigation (2016) showed racial bias in predictions
# We will investigate the cluster profiles to see why
# up until the EDA section, we are just cleaning the code to get ready
# to cluster

compasUrl <- "https://raw.githubusercontent.com/propublica/compas-analysis/master/compas-scores-raw.csv"
compasRaw <- read.csv(compasUrl)

riskLong <- compasRaw %>%
  filter(DisplayText %in% c("Risk of Violence",
                            "Risk of Recidivism",
                            "Risk of Failure to Appear")) %>%
  mutate(scale = case_when(
    DisplayText == "Risk of Violence" ~ "violence",
    DisplayText == "Risk of Recidivism" ~ "recidivism",
    DisplayText == "Risk of Failure to Appear" ~ "fta"))

scoresWide <- riskLong %>%
  select(AssessmentID, scale, RawScore) %>%
  pivot_wider(names_from = scale, values_from = RawScore, names_prefix = "raw_")

demo <- riskLong %>%
  group_by(AssessmentID) %>%
  summarise(
    DateOfBirth = first(DateOfBirth),
    Sex = first(Sex_Code_Text),
    Ethnicity = first(Ethnic_Code_Text),
    .groups = "drop")

demo <- demo %>%
  mutate(
    yy = as.integer(substr(DateOfBirth, nchar(DateOfBirth) - 1, nchar(DateOfBirth))),
    yyyy = ifelse(yy >= 30, 1900 + yy, 2000 + yy),
    mm = substr(DateOfBirth, 1, 2),
    dd = substr(DateOfBirth, 4, 5),
    dobDate = as.Date(paste(yyyy, mm, dd, sep = "-")),
    Age = as.numeric(difftime(Sys.Date(), dobDate, units = "days")) / 365.25,
    Sex = factor(Sex),
    Ethnicity = factor(Ethnicity))

compas <- scoresWide %>%
  inner_join(demo, by = "AssessmentID") %>%
  select(raw_violence, raw_recidivism, raw_fta, Age, Sex, Ethnicity)

# simplify ethnicity to binary for demonstration, as on of the
# algorithms is designed for only binary variables
compas <- compas %>%
  mutate(EthBinary = ifelse(Ethnicity == "African-American", "Black", "Non-Black"),
         EthBinary = factor(EthBinary))

compas <- compas[complete.cases(compas), ]

# The data is somewhat large, so to simplify we will randomly
# select equal sized groupings of the biased group compared against all others
compas <- compas %>%
  group_by(EthBinary) %>%
  slice_sample(n = 500) %>%
  ungroup()

dim(compas)
head(compas)
table(compas$EthBinary)

# EDA
# =============================================================================

pEthnicity <- ggplot(compas, aes(x = EthBinary, fill = EthBinary)) +
  geom_bar(alpha = 0.8) +
  scale_fill_viridis_d(option = "cividis") +
  labs(title = "Binary Ethnicity Distribution", x = "", y = "Count") +
  theme(legend.position = "none", panel.grid = element_blank(),
        panel.border = element_rect(fill = NA, color = "black", linewidth = 1))
pEthnicity

pAgeEth <- ggplot(compas, aes(x = EthBinary, y = Age, fill = EthBinary)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_viridis_d(option = "cividis") +
  labs(title = "Age by Ethnicity", x = "", y = "Age") +
  theme(legend.position = "none", panel.grid = element_blank(),
        panel.border = element_rect(fill = NA, color = "black", linewidth = 1))

pRecidEth <- ggplot(compas, aes(x = EthBinary, y = raw_recidivism, fill = EthBinary)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_viridis_d(option = "cividis") +
  labs(title = "Recidivism Score by Ethnicity", x = "", y = "Raw Score") +
  theme(legend.position = "none", panel.grid = element_blank(),
        panel.border = element_rect(fill = NA, color = "black", linewidth = 1))

pViolEth <- ggplot(compas, aes(x = EthBinary, y = raw_violence, fill = EthBinary)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_viridis_d(option = "cividis") +
  labs(title = "Violence Score by Ethnicity", x = "", y = "Raw Score") +
  theme(legend.position = "none", panel.grid = element_blank(),
        panel.border = element_rect(fill = NA, color = "black", linewidth = 1))

grid.arrange(pEthnicity, pAgeEth, pRecidEth, pViolEth, ncol = 2)

compas %>%
  group_by(EthBinary) %>%
  summarise(n = n(),
            meanAge = round(mean(Age), 1),
            meanRecid = round(mean(raw_recidivism), 2),
            meanViolence = round(mean(raw_violence), 2),
            meanFTA = round(mean(raw_fta), 2))


# BASELINE CLUSTERING 
# =============================================================================

compasForClust <- compas %>%
  select(raw_violence, raw_recidivism, raw_fta, Age)

compasScaled <- scale(compasForClust)
gowerCompas <-  as.matrix(daisy(compasForClust, metric = "gower"))

# MDS to visualize
mdsCompas <- cmdscale(gowerCompas, k = 2)
compas$mds1 <- mdsCompas[, 1]
compas$mds2 <- mdsCompas[, 2]

pMdsEth <- ggplot(compas, aes(x = mds1, y = mds2, color = EthBinary)) +
  geom_point(alpha = 0.4, size = 1) +
  scale_color_viridis_d(option = "cividis") +
  labs(title = "MDS Projection by Ethnicity", x = "MDS 1", y = "MDS 2") +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(fill = NA, color = "black", linewidth = 1),
        legend.position = "bottom")
pMdsEth

# In the interest of time, previous analysis of this data indicates there
# are three clusters based on a wide array of metrics, so we will
# just go with that

# K-means baseline
set.seed(2026)
kmeansBaseline <- kmeans(compasScaled, centers = 3, nstart = 25, iter.max = 100)
compas$clusterKmeans <- factor(kmeansBaseline$cluster)

# PAM baseline 
pamBaseline <- pam(gowerCompas, k = 3, diss = TRUE)
compas$clusterPAM <- factor(pamBaseline$clustering)

pMdsKmeans <- ggplot(compas, aes(x = mds1, y = mds2, color = clusterKmeans)) +
  geom_point(alpha = 0.4, size = 1) +
  scale_color_viridis_d(option = "cividis") +
  labs(title = "K-Means Clusters", x = "MDS 1", y = "MDS 2", color = "Cluster") +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(fill = NA, color = "black", linewidth = 1),
        legend.position = "bottom")

pMdsPAM <- ggplot(compas, aes(x = mds1, y = mds2, color = clusterPAM)) +
  geom_point(alpha = 0.4, size = 1) +
  scale_color_viridis_d(option = "cividis") +
  labs(title = "PAM Clusters", x = "MDS 1", y = "MDS 2", color = "Cluster") +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(fill = NA, color = "black", linewidth = 1),
        legend.position = "bottom")

grid.arrange(pMdsEth, pMdsKmeans, pMdsPAM, ncol = 3)


# FAIRNESS METRICS
# =============================================================================

# FIRST FAIRNESS IDEA

# BALANCE (Chierichetti et al. 2017)

# for a cluster C with r red points and b blue points,
# balance(C) = min(r, b) / max(r, b)
# balance = 1 means perfect parity, balance = 0 means one group absent
# for a specific cluster. When we look at balance over all clusters
# we can use overall balance = min over all clusters

# The paper mentioned above is considered the first published paper
# on fair clustering, and the definition of fairness is specific towards
# a GROUP. Here we define group fairness, as this definition lumps
# everyone together based on a binary indicator (the red group or the blue group)

# Here, we see our protected attribute coincides with the idea of red and blue
# points.

# We will see other definitions of fairness shortly.

computeBalance <- function(clusters, sensitive) {
  tab <- table(clusters, sensitive)
  
  # for each cluster, compute min/max ratio
  clusterBalances <- apply(tab, 1, function(row) {
    if (max(row) == 0) return(0)
    min(row) / max(row)
  })

  minBalance <- min(clusterBalances)
  return(list(minBalance = minBalance, 
              perCluster = clusterBalances,
              avgBalance = mean(clusterBalances)))
}

# SECOND DEFINITION OF FAIRNESS

# DISPARATE IMPACT RATIO (Also considered a group fairness definition)

# for a specific cluster k, ratio of assignment rates between groups
# DI_k = (P(cluster = k | group = minority)) / (P(cluster = k | group = majority))
# closer to 1 is better, < 0.8 often considered problematic


computeDisparateImpact <- function(clusters, sensitive) {
  tab <- table(clusters, sensitive)
  rates <- prop.table(tab, margin = 2)
  clusterDIs <- apply(rates, 1, function(row) min(row) / max(row))
  min(clusterDIs)
}

# THIRD DEFINITION OF FAIRNESS

# COMPOSITION DEVIATION (Also considered a group fairness definition)
# how much each cluster deviates from overall population proportions
# lower is better

# This is similar to balance, we want the clusters to be proportional to 
# the proportions of the overall data with respect to the protected attribute
# For the way we sampled from the data, this would be 50-50

computeCompositionDeviation <- function(clusters, sensitive) {
  overallProp <- prop.table(table(sensitive))
  tab <- table(clusters, sensitive)
  clusterProps <- prop.table(tab, margin = 1)
  totalDev <- 0
  for (k in 1:nrow(clusterProps)) {
    totalDev <- totalDev + sum(abs(clusterProps[k, ] - overallProp))
  }
  totalDev / nrow(clusterProps)
}


balanceKmeans <- computeBalance(compas$clusterKmeans, compas$EthBinary)
balancePAM <- computeBalance(compas$clusterPAM, compas$EthBinary)

# K-MEANS 
cat("Min Balance:", round(balanceKmeans$minBalance, 3), "\n")
cat("Per-cluster Balance:", round(balanceKmeans$perCluster, 3), "\n")
cat("Avg Balance:", round(balanceKmeans$avgBalance, 3), "\n")
cat("Composition Deviation:", round(computeCompositionDeviation(compas$clusterKmeans, compas$EthBinary), 3), "\n")
cat("Disparate Impact:", round(computeDisparateImpact(compas$clusterKmeans, compas$EthBinary), 3), "\n")


# PAM
cat("Min Balance:", round(balancePAM$minBalance, 3), "\n")
cat("Per-cluster Balance:", round(balancePAM$perCluster, 3), "\n")
cat("Avg Balance:", round(balancePAM$avgBalance, 3), "\n")
cat("Composition Deviation:", round(computeCompositionDeviation(compas$clusterPAM, compas$EthBinary), 3), "\n")
cat("Disparate Impact:", round(computeDisparateImpact(compas$clusterPAM, compas$EthBinary), 3), "\n")


# VISUALIZING (POTENTIAL) BIAS IN CLUSTERS
# =============================================================================

ethByClusterKmeans <- table(compas$clusterKmeans, compas$EthBinary)
print(ethByClusterKmeans)
print(round(prop.table(ethByClusterKmeans, margin = 1), 3))

# its clear above that one cluster is a much higher proportion of Non African Americans
# while the other two is higher proportions for African-Americans.

# But this is possible meaningless until we analyze those clusters

pClusterEthKmeans <- compas %>%
  group_by(clusterKmeans, EthBinary) %>%
  summarise(n = n(), .groups = "drop") %>%
  ggplot(aes(x = clusterKmeans, y = n, fill = EthBinary)) +
  geom_bar(stat = "identity", position = "fill", alpha = 0.8) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "red") +
  scale_fill_viridis_d(option = "cividis") +
  labs(title = "Baseline K-Means", x = "Cluster", y = "Proportion", fill = "") +
  theme(panel.grid = element_blank(), legend.position = "bottom",
        panel.border = element_rect(fill = NA, color = "black", linewidth = 1))

pClusterEthPAM <- compas %>%
  group_by(clusterPAM, EthBinary) %>%
  summarise(n = n(), .groups = "drop") %>%
  ggplot(aes(x = clusterPAM, y = n, fill = EthBinary)) +
  geom_bar(stat = "identity", position = "fill", alpha = 0.8) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "red") +
  scale_fill_viridis_d(option = "cividis") +
  labs(title = "Baseline PAM", x = "Cluster", y = "Proportion", fill = "") +
  theme(panel.grid = element_blank(), legend.position = "bottom",
        panel.border = element_rect(fill = NA, color = "black", linewidth = 1))

ggpubr::ggarrange(pClusterEthKmeans, pClusterEthPAM, ncol = 2, common.legend = T, legend = "bottom")

# cluster centroids to understand what clusters represent
compasForClust %>%
  mutate(cluster = kmeansBaseline$cluster) %>%
  group_by(cluster) %>%
  summarise(across(everything(), mean), .groups = "drop")

compasForClust %>%
  mutate(cluster = pamBaseline$clustering) %>%
  group_by(cluster) %>%
  summarise(across(everything(), mean), .groups = "drop")

# well, keeping in mind that lower scores indicates less risk
# for the categories of Risk of Violence, Risk of Recidivism, and Risk
# of Failing to Appear, we see that the the cluster with 
# the lowest amount of risk is the cluster that was proportionally
# higher in Non African-Americans, while the 'riskier' clusters
# were proportionally higher in African-Americans.

# This might motivate us to try and make these clusters more proportional,
# which leads us to our first method


# METHOD 1: FAIRLETS (CHIERICHETTI ET AL. 2017)
# =============================================================================


# Idea: decompose data into small, balanced "fairlets" first,
# then cluster the fairlets using any standard algorithm
#
# a fairlet is a minimal subset that satisfies fairness constraints
# for binary groups with balance 't'
#
# algorithm...
# 1. match points from different groups to form balanced pairs/small sets
# 2. compute a representative (centroid) for each fairlet
# 3. run standard clustering on fairlet representatives
# 4. assign each point to the cluster of its fairlet representative

# so basically we combine observations based on proportions of the data.
# in our example, since we have 50-50 split of the binary variable, a
# fairlet would consist of two observations, one from each
# protected group. Then we calculate the centroid (average) of those 
# two observations, and the centroid would be considered 1 observation to cluster

# However, creating the fairlets is the hard part, because we want to assign 
# observations to their CLOSEST 'neighbour' with respect to the other
# protected attribute. There are optimal ways to do this, but it becomes
# an optimization problem which is beyond our alloted time today!

# we implement a simplified greedy fairlet decomposition for balance = 1 (perfect parity)
# this is essentially a minimum-cost matching between groups. 

###################################################################################################
####### This is not as intricate as the original paper, but will serve as a demonstration #########
################################## for proof of concept. ##########################################
###################################################################################################

buildFairlets <- function(features, sensitive, distMat = NULL) {
  # separate indices by group
  groups <- levels(factor(sensitive))
  idx0 <- which(sensitive == groups[1])
  idx1 <- which(sensitive == groups[2])
  n0 <- length(idx0)
  n1 <- length(idx1)
  nPairs <- min(n0, n1)
  if (is.null(distMat)) distMat <- as.matrix(dist(features)) else distMat <- as.matrix(distMat)
  # greedy matching: for each point in smaller group, find closest in larger
  fairlets <- list()
  used0 <- rep(FALSE, n0)
  used1 <- rep(FALSE, n1)
  for (i in 1:nPairs) {
    bestDist <- Inf
    best0 <- NA
    best1 <- NA
    for (j in which(!used0)) {
      for (k in which(!used1)) {
        d <- distMat[idx0[j], idx1[k]]
        if (d < bestDist) {
          bestDist <- d
          best0 <- j
          best1 <- k
        }
      }
    }
    used0[best0] <- TRUE
    used1[best1] <- TRUE
    fairlets[[i]] <- c(idx0[best0], idx1[best1])
  }
  
  # any unmatched points form singleton fairlets
  if (n0 > n1) {
    for (j in which(!used0)) {
      fairlets[[length(fairlets) + 1]] <- idx0[j]
    }
  } else if (n1 > n0) {
    for (k in which(!used1)) {
      fairlets[[length(fairlets) + 1]] <- idx1[k]
    }
  }
  
  return(fairlets)
}

computeFairletCentroids <- function(features, fairlets) {
  centroids <- matrix(NA, nrow = length(fairlets), ncol = ncol(features))
  for (i in seq_along(fairlets)) {
    if (length(fairlets[[i]]) == 1) {
      centroids[i, ] <- as.numeric(features[fairlets[[i]], ])
    } else {
      centroids[i, ] <- colMeans(features[fairlets[[i]], , drop = FALSE])
    }
  }
  return(centroids)
}

assignFairletClusters <- function(fairlets, fairletClusters, n) {
  pointClusters <- rep(NA, n)
  for (i in seq_along(fairlets)) pointClusters[fairlets[[i]]] <- fairletClusters[i]
  return(pointClusters)
}

# VISUALIZING THE FAIRLET DECOMPOSITION
# =============================================================================
# build fairlets
fairlets <- buildFairlets(compasScaled, compas$EthBinary, gowerCompas)

cat("Number of fairlets:", length(fairlets), "\n")
cat("Fairlet sizes:", table(sapply(fairlets, length)), "\n")

# visualize some fairlets on MDS plot
fairletDf <- data.frame()
for (i in 1:min(100, length(fairlets))) {
  pts <- fairlets[[i]]
  for (p in pts) {
    fairletDf <- rbind(fairletDf, data.frame(
      mds1 = compas$mds1[p],
      mds2 = compas$mds2[p],
      fairletId = i,
      EthBinary = compas$EthBinary[p]
    ))
  }
}

# show connections within fairlets
fairletConnections <- data.frame()
for (i in 1:min(100, length(fairlets))) {
  pts <- fairlets[[i]]
  if (length(pts) == 2) {
    fairletConnections <- rbind(fairletConnections, data.frame(
      x1 = compas$mds1[pts[1]], y1 = compas$mds2[pts[1]],
      x2 = compas$mds1[pts[2]], y2 = compas$mds2[pts[2]],
      fairletId = i
    ))
  }
}

pFairlets <- ggplot() +
  geom_segment(data = fairletConnections,
               aes(x = x1, y = y1, xend = x2, yend = y2),
               color = "gray50", alpha = 0.5, linewidth = 0.5) +
  geom_point(data = fairletDf, 
             aes(x = mds1, y = mds2, color = EthBinary),
             alpha = 0.7, size = 2) +
  scale_color_viridis_d(option = "cividis") +
  labs(title = "Select Fairlet Decompositions", 
       x = "MDS 1", y = "MDS 2", color = "Ethnicity") +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(fill = NA, color = "black", linewidth = 1),
        legend.position = "bottom")
pFairlets


# FAIR CLUSTERING WITH THESE FAIRLETS
# compute fairlet centroids
fairletCentroids <- computeFairletCentroids(compasScaled, fairlets)
# cluster the fairlet centroids using k-means
kmeansFairlet <- kmeans(fairletCentroids, centers = 3, nstart = 25, iter.max = 100)
# assign points to clusters based on their fairlet
compas$clusterFairlet <- factor(assignFairletClusters(fairlets, kmeansFairlet$cluster, nrow(compas)))
# evaluate fairness
balanceFairlet <- computeBalance(compas$clusterFairlet, compas$EthBinary)

# FAIRLET CLUSTERING
cat("Min Balance:", round(balanceFairlet$minBalance, 3), "(baseline K-means:", round(balanceKmeans$minBalance, 3), ")\n")
cat("Per-cluster Balance:", round(balanceFairlet$perCluster, 3), "\n")
cat("Avg Balance:", round(balanceFairlet$avgBalance, 3), "(baseline:", round(balanceKmeans$avgBalance, 3), ")\n")
cat("Composition Deviation:", round(computeCompositionDeviation(compas$clusterFairlet, compas$EthBinary), 3), "\n")
cat("Disparate Impact:", round(computeDisparateImpact(compas$clusterFairlet, compas$EthBinary), 3), "\n")

# visualize
pClusterFairlet <- compas %>%
  filter(!is.na(clusterFairlet)) %>%
  group_by(clusterFairlet, EthBinary) %>%
  summarise(n = n(), .groups = "drop") %>%
  ggplot(aes(x = clusterFairlet, y = n, fill = EthBinary)) +
  geom_bar(stat = "identity", position = "fill", alpha = 0.8) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "red") +
  scale_fill_viridis_d(option = "cividis") +
  labs(title = "Fairlet-Based Clustering", x = "Cluster", y = "Proportion", fill = "Ethnicity") +
  theme(panel.grid = element_blank(), legend.position = "bottom",
        panel.border = element_rect(fill = NA, color = "black", linewidth = 1))

ggpubr::ggarrange(pClusterEthKmeans + labs(title = "Baseline K-Means"), 
             pClusterFairlet, ncol = 2, common.legend = T, legend = "bottom")

# centroids
compasForClust %>%
  mutate(cluster = compas$clusterFairlet) %>%
  group_by(cluster) %>%
  summarise(across(everything(), mean), .groups = "drop")

# METHOD 2: LINEAR RESIDUALIZATION
# =============================================================================

# idea: remove the linear effect of the protected attribute from features
# regress each continuous feature on protected attribute, use residuals for clustering
# this is a preprocessing approach

continuousVars <- c("raw_violence", "raw_recidivism", "raw_fta", "Age")

compasResid <- compas

for (var in continuousVars) {
  formula <- as.formula(paste(var, "~ EthBinary"))
  model <- lm(formula, data = compas)
  compasResid[[var]] <- residuals(model)
}

# prepare for clustering
compasResidForClust <- compasResid %>%
  select(raw_violence, raw_recidivism, raw_fta, Age)

compasResidScaled <- scale(compasResidForClust)

# K-means on residualized data
set.seed(2026)
kmeansResid <- kmeans(compasResidScaled, centers = 3, nstart = 25, iter.max = 100)
compas$clusterResid <- factor(kmeansResid$cluster)

# evaluate fairness
balanceResid <- computeBalance(compas$clusterResid, compas$EthBinary)

cat("\n=== Residualization Results ===\n")
cat("Min Balance:", round(balanceResid$minBalance, 3), "(baseline:", round(balanceKmeans$minBalance, 3), ")\n")
cat("Per-cluster Balance:", round(balanceResid$perCluster, 3), "\n")
cat("Avg Balance:", round(balanceResid$avgBalance, 3), "(baseline:", round(balanceKmeans$avgBalance, 3), ")\n")
cat("Composition Deviation:", round(computeCompositionDeviation(compas$clusterResid, compas$EthBinary), 3), "\n")

# visualize before/after residualization
pRecidBefore <- ggplot(compas, aes(x = raw_recidivism, fill = EthBinary)) +
  geom_density(alpha = 0.5) +
  scale_fill_viridis_d(option = "cividis") +
  labs(title = "Recidivism: Before", x = "Raw Score", y = "Density") +
  theme(panel.grid = element_blank(), legend.position = "bottom",
        panel.border = element_rect(fill = NA, color = "black", linewidth = 1))

pRecidAfter <- ggplot(compasResid, aes(x = raw_recidivism, fill = EthBinary)) +
  geom_density(alpha = 0.5) +
  scale_fill_viridis_d(option = "cividis") +
  labs(title = "Recidivism: After Residualization", x = "Residual", y = "Density") +
  theme(panel.grid = element_blank(), legend.position = "bottom",
        panel.border = element_rect(fill = NA, color = "black", linewidth = 1))

ggpubr::ggarrange(pRecidBefore, pRecidAfter, ncol = 2, common.legend = T, legend = "bottom")

pClusterResid <- compas %>%
  group_by(clusterResid, EthBinary) %>%
  summarise(n = n(), .groups = "drop") %>%
  ggplot(aes(x = clusterResid, y = n, fill = EthBinary)) +
  geom_bar(stat = "identity", position = "fill", alpha = 0.8) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "red") +
  scale_fill_viridis_d(option = "cividis") +
  labs(title = "After Residualization", x = "Cluster", y = "Proportion", fill = "Ethnicity") +
  theme(panel.grid = element_blank(), legend.position = "bottom",
        panel.border = element_rect(fill = NA, color = "black", linewidth = 1))


# METHOD 3: INDIVIDUAL FAIRNESS (KLEINDESSNER ET AL. 2020)
# =============================================================================

# This is a different idea of fairness, known as individual fairness
# INDIVIDUAL FAIRNESS differs from GROUP FAIRNESS

# INDIVIDUAL FAIRNESS: similar individuals should be treated similarly
# specifically: each point should, on average, be closer to points in
# its own cluster than to points in any other cluster
#
# Kleindessner et al. (2020) define a point as "individually fair" if:
# avg_dist(x, own cluster) <= avg_dist(x, any other cluster)

computeIndividualFairness <- function(distMat, clusters) {
  distMat <- as.matrix(distMat)
  n <- nrow(distMat)
  K <- length(unique(clusters))
  violations <- 0
  avgRatios <- numeric(n)
  for (i in 1:n) {
    ownCluster <- clusters[i]
    ownMembers <- which(clusters == ownCluster & (1:n) != i)
    if (length(ownMembers) == 0) {
      avgRatios[i] <- 1
      next
    }
    
    avgDistOwn <- mean(distMat[i, ownMembers])
    # compute avg distance to each other cluster
    otherClusters <- setdiff(unique(clusters), ownCluster)
    minAvgDistOther <- Inf
    for (k in otherClusters) {
      kMembers <- which(clusters == k)
      if (length(kMembers) > 0) {
        avgDistK <- mean(distMat[i, kMembers])
        minAvgDistOther <- min(minAvgDistOther, avgDistK)
      }
    }
    # violation if closer to another cluster
    if (avgDistOwn > minAvgDistOther) violations <- violations + 1
    avgRatios[i] <- min(avgDistOwn / minAvgDistOther, 1)
  }
  return(list(
    fracViolated = violations / n,
    avgRatio = mean(avgRatios),
    violations = violations,
    ratios = avgRatios
  ))
}

# compute individual fairness for baseline and fair methods
gowerMat <- as.matrix(gowerCompas)

ifBaseline <- computeIndividualFairness(gowerMat, as.numeric(compas$clusterKmeans))
ifFairlet <- computeIndividualFairness(gowerMat, as.numeric(compas$clusterFairlet))
ifResid <- computeIndividualFairness(gowerMat, as.numeric(compas$clusterResid))

# individual fairness
cat("Fraction of points violated:\n")
cat("  Baseline K-means:", round(ifBaseline$fracViolated, 3), "\n")
cat("  Fairlet-based:", round(ifFairlet$fracViolated, 3), "\n")
cat("  Residualization:", round(ifResid$fracViolated, 3), "\n")

# above you see that there's a tradeoff between individual fairness and group fairness.
# by satisfying our definitions of group fairness, we might be forcing individuals
# to change clusters that they would have been assigned to in the first place
# in order to satisfy the group fairness definition. This would be considered
# a violation of individual fairness according to the individual fairness
# definition above, which is why the fraction of points is higher
# for the fair methods vs basaeline k-means.


# VISUALIZING INDIVIDUAL FAIRNESS
# =============================================================================

# add individual fairness status to data
compas$ifRatioBaseline <- ifBaseline$ratios
compas$ifViolatedBaseline <- ifBaseline$ratios < 1

pIFBaseline <- ggplot(compas, aes(x = mds1, y = mds2, color = ifViolatedBaseline)) +
  geom_point(alpha = 0.5, size = 1) +
  scale_color_manual(values = c("FALSE" = "steelblue", "TRUE" = "coral"),
                     labels = c("Satisfied", "Violated")) +
  labs(title = "Individual Fairness: Baseline K-Means",
       subtitle = paste0(round(100 * ifBaseline$fracViolated, 1), "% of points violated"),
       x = "MDS 1", y = "MDS 2", color = "IF Status") +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(fill = NA, color = "black", linewidth = 1),
        legend.position = "bottom")

compas$ifRatioFairlet <- ifFairlet$ratios
compas$ifViolatedFairlet <- ifFairlet$ratios < 1

pIFFairlet <- ggplot(compas %>% filter(!is.na(clusterFairlet)), 
                     aes(x = mds1, y = mds2, color = ifViolatedFairlet)) +
  geom_point(alpha = 0.5, size = 1) +
  scale_color_manual(values = c("FALSE" = "steelblue", "TRUE" = "coral"),
                     labels = c("Satisfied", "Violated")) +
  labs(title = "Individual Fairness: Fairlet-Based",
       subtitle = paste0(round(100 * ifFairlet$fracViolated, 1), "% of points violated"),
       x = "MDS 1", y = "MDS 2", color = "IF Status") +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(fill = NA, color = "black", linewidth = 1),
        legend.position = "bottom")

grid.arrange(pIFBaseline, pIFFairlet, ncol = 2)


profileBaseline <- compas %>%
  group_by(clusterKmeans) %>%
  summarise(
    n = n(),
    meanAge = round(mean(Age), 1),
    meanRecid = round(mean(raw_recidivism), 2),
    meanViolence = round(mean(raw_violence), 2),
    pctBlack = round(100 * mean(EthBinary == "Black"), 1),
    .groups = "drop"
  )


print(as.data.frame(profileBaseline))

profileFairlet <- compas %>%
  filter(!is.na(clusterFairlet)) %>%
  group_by(clusterFairlet) %>%
  summarise(
    n = n(),
    meanAge = round(mean(Age), 1),
    meanRecid = round(mean(raw_recidivism), 2),
    meanViolence = round(mean(raw_violence), 2),
    pctBlack = round(100 * mean(EthBinary == "Black"), 1),
    .groups = "drop"
  )

print(as.data.frame(profileFairlet))


