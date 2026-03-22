# DIMENSIONALITY REDUCTION TECHNIQUES (1 of 2)
# Workshop 10: PCA and Factor Models
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
library(viridis)
library(cluster)
library(mclust)       
library(psych)        

set.seed(2026)
theme_set(theme_minimal(base_size=12))

# COMMUNITIES AND CRIME DATA
# =============================================================================
# UCI Communities and Crime dataset combines:
# - socio-economic data from 1990 US Census
# - law enforcement data from 1990 LEMAS survey
# - crime data from 1995 FBI UCR

dataUrl <- "https://archive.ics.uci.edu/ml/machine-learning-databases/communities/communities.data"
colNames <- c("state","county","community","communityname","fold",
              "population","householdsize","racepctblack","racePctWhite",
              "racePctAsian","racePctHisp","agePct12t21","agePct12t29",
              "agePct16t24","agePct65up","numbUrban","pctUrban","medIncome",
              "pctWWage","pctWFarmSelf","pctWInvInc","pctWSocSec","pctWPubAsst",
              "pctWRetire","medFamInc","perCapInc","whitePerCap","blackPerCap",
              "indianPerCap","AsianPerCap","OtherPerCap","HispPerCap",
              "NumUnderPov","PctPopUnderPov","PctLess9thGrade","PctNotHSGrad",
              "PctBSorMore","PctUnemployed","PctEmploy","PctEmplManu",
              "PctEmplProfServ","PctOccupManu","PctOccupMgmtProf","MalePctDivorce",
              "MalePctNevMarr","FemalePctDiv","TotalPctDiv","PersPerFam",
              "PctFam2Par","PctKids2Par","PctYoungKids2Par","PctTeen2Par",
              "PctWorkMomYoungKids","PctWorkMom","NumIlleg","PctIlleg","NumImmig",
              "PctImmigRecent","PctImmigRec5","PctImmigRec8","PctImmigRec10",
              "PctRecentImmig","PctRecImmig5","PctRecImmig8","PctRecImmig10",
              "PctSpeakEnglOnly","PctNotSpeakEnglWell","PctLargHouseFam",
              "PctLargHouseOccup","PersPerOccupHous","PersPerOwnOccHous",
              "PersPerRentOccHous","PctPersOwnOccup","PctPersDenseHous",
              "PctHousLess3BR","MedNumBR","HousVacant","PctHousOccup",
              "PctHousOwnOcc","PctVacantBoarded","PctVacMore6Mos","MedYrHousBuilt",
              "PctHousNoPhone","PctWOFullPlumb","OwnOccLowQuart","OwnOccMedVal",
              "OwnOccHiQuart","RentLowQ","RentMedian","RentHighQ","MedRent",
              "MedRentPctHousInc","MedOwnCostPctInc","MedOwnCostPctIncNoMtg",
              "NumInShelters","NumStreet","PctForeignBorn","PctBornSameState",
              "PctSameHouse85","PctSameCity85","PctSameState85","LemasSwornFT",
              "LemasSwFTPerPop","LemasSwFTFieldOps","LemasSwFTFieldPerPop",
              "LemasTotalReq","LemasTotReqPerPop","PolicReqPerOffic","PolicPerPop",
              "RacialMatchCommPol","PctPolicWhite","PctPolicBlack","PctPolicHisp",
              "PctPolicAsian","PctPolicMinor","OfficAssgnDrugUnits",
              "NumKindsDrugsSeiz","PolicAveOTWorked","LandArea","PopDens",
              "PctUsePubTrans","PolicCars","PolicOperBudg","LemasPctPolicOnPatr",
              "LemasGangUnitDeploy","LemasPctOfficDrugUn","PolicBudgPerPop",
              "ViolentCrimesPerPop")

rawData <- read.csv(dataUrl, header=FALSE, na.strings="?", col.names=colNames)


# DATA CLEANING
# =============================================================================

# columns missing values
missingPct <- colMeans(is.na(rawData)) * 100
print(sort(missingPct[missingPct > 1], decreasing=TRUE))

# keep only complete predictive features
lemasVars <- grep("^Lemas|^Polic|^Offic|^Racial", colNames, value=TRUE)
lemasVars <- setdiff(lemasVars, c("PolicPerPop", "PopDens", "PctUsePubTrans"))
excludeCols <- c("state", "county", "community", "communityname", "fold",
                 lemasVars, "ViolentCrimesPerPop")
featureCols <- setdiff(colNames, excludeCols)
crimeFeatures <- rawData[, featureCols]
whichNA <- numeric()
for(i in 1:ncol(crimeFeatures)) whichNA[i] <- ifelse(any(is.na(crimeFeatures[,i])), 1, 0)
crimeFeatures <- crimeFeatures[,-which(whichNA == 1)]

# remove rows with any remaining missing values
completeIdx <- complete.cases(crimeFeatures)
crimeFeatures <- crimeFeatures[completeIdx, ]
crimeTarget <- rawData$ViolentCrimesPerPop[completeIdx]
stateCode <- rawData$state[completeIdx]
communityName <- rawData$communityname[completeIdx]

n <- nrow(crimeFeatures)
p <- ncol(crimeFeatures)

# CREATE GROUND TRUTH CLUSTERS (HIGHER vs LOWER CRIME)
# =============================================================================
# for evaluating clustering, we create a binary crime classification
# using 60% quantile split on ViolentCrimesPerPop

crimeMedian <- quantile(crimeTarget, na.rm=TRUE, probs = 0.65)
trueClass <- factor(ifelse(crimeTarget >= crimeMedian, "HighCrime", "LowCrime"))


# THE CURSE OF DIMENSIONALITY
# =============================================================================
featureScaled <- scale(crimeFeatures)
fullDist <- dist(featureScaled)
distVec <- as.vector(fullDist)

cat("\nPairwise distances in", p, "dimensions:\n")
cat("  Mean:", round(mean(distVec), 2), "\n")
cat("  SD:", round(sd(distVec), 2), "\n")
cat("  CV (SD/Mean):", round(sd(distVec)/mean(distVec), 3), "\n")

# coefficient of variation: low implies all points roughly equidistant
# this data doesnt have enough features to have CV = 0, but it approaches

distDf <- data.frame(Distance=distVec)
pDistHist <- ggplot(distDf, aes(x=Distance)) +
  geom_histogram(bins=50, fill="steelblue", alpha=0.7) +
  geom_vline(xintercept=mean(distVec), color="red", linewidth=1) +
  labs(title=paste0("Distribution of Pairwise Distances (", p, "D)"),
       subtitle="Red line = mean; narrow spread indicates curse of dimensionality",
       x="Euclidean Distance", y="Count") +
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill=NA, color="black", linewidth=1))
pDistHist

# BASELINE: CLUSTERING IN FULL DIMENSIONAL SPACE
# =============================================================================
set.seed(2026)
kmeansFull <- kmeans(featureScaled, centers=2, nstart=25)
table(kmeansFull$cluster, trueClass)
ariFull <- adjustedRandIndex(kmeansFull$cluster, trueClass)

accFull <- max(mean(kmeansFull$cluster == as.numeric(trueClass)),
  mean(kmeansFull$cluster != as.numeric(trueClass)))

round(ariFull, 3)
round(accFull, 3)

# EXPLORATORY DATA ANALYSIS
# =============================================================================
# correlation with crime rate
featureCors <- numeric()
for(i in 1:p) featureCors[i] <- cor(crimeFeatures[,i], crimeTarget, use="complete.obs")
names(featureCors) <- colnames(crimeFeatures)

# top positive and negative correlations
topPosCor <- sort(featureCors, decreasing=TRUE)[1:10]
topNegCor <- sort(featureCors, decreasing=FALSE)[1:10]

# Top 10 features positively correlated with crime
print(round(topPosCor, 3))
# Top 10 features negatively correlated with crime
print(round(topNegCor, 3))

# visualize correlation distribution
corDf <- data.frame(Feature=names(featureCors), Correlation=featureCors)
pCorHist <- ggplot(corDf, aes(x=Correlation)) +
  geom_histogram(bins=30, fill="steelblue", alpha=0.7) +
  geom_vline(xintercept=0, linetype="dashed", color="gray50") +
  labs(title="Feature Correlations with Violent Crime Rate",
       x="Pearson Correlation", y="Count") +
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill=NA, color="black", linewidth=1))
pCorHist


# FEATURE DISTRIBUTIONS BY CRIME LEVEL
# =============================================================================
keyFeatures <- c("PctKids2Par", "PctPopUnderPov", "medFamInc", 
                 "racepctblack", "PctUnemployed", "PctHousOwnOcc")

keyLong <- crimeFeatures[, keyFeatures] %>%
  mutate(CrimeLevel=trueClass, ID=1:n) %>%
  pivot_longer(cols=all_of(keyFeatures), names_to="Feature", values_to="Value")

pKeyFeatures <- ggplot(keyLong, aes(x=CrimeLevel, y=Value, fill=CrimeLevel)) +
  geom_boxplot(alpha=0.7, outlier.size=1) +
  facet_wrap(~Feature, scales="free_y") +
  scale_fill_viridis_d(option="cividis") +
  labs(title="Variable Distributions by Crime Level") +
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill=NA, color="black", linewidth=0.5),
        strip.background=element_rect(fill="gray90"),
        legend.position="none")
pKeyFeatures

# PRINCIPAL COMPONENT ANALYSIS (PCA)
# =============================================================================
# PCA finds orthogonal directions of maximum variance
# if community structure drives many correlated features,
# that shared variation will concentrate in first few PCs

pcaResult <- prcomp(crimeFeatures, center=TRUE, scale.=TRUE)
eigenvalues <- pcaResult$sdev^2
propVar <- eigenvalues / sum(eigenvalues)
cumVar <- cumsum(propVar)

varianceDf <- data.frame(PC = 1:length(eigenvalues),
  Eigenvalue = eigenvalues, PropVariance = propVar, CumVariance = cumVar)

print(head(varianceDf, 15))


# VARIANCE DECOMPOSITION PLOTS
# =============================================================================

pScree <- ggplot(varianceDf[1:30,], aes(x=PC, y=PropVariance)) +
  geom_line(color="steelblue", linewidth=1) +
  geom_point(color="steelblue", size=2) +
  scale_x_continuous(breaks=seq(0,30,5)) +
  scale_y_continuous(labels=scales::percent) +
  labs(title="Scree Plot: Variance Per Component",
       x="Principal Component", y="Proportion of Variance") +
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill=NA, color="black", linewidth=1))

pCumVar <- ggplot(varianceDf[1:30,], aes(x=PC, y=CumVariance)) +
  geom_line(color="coral", linewidth=1) +
  geom_point(color="coral", size=2) +
  geom_hline(yintercept=0.5, linetype="dashed", color="gray50") +
  geom_hline(yintercept=0.8, linetype="dashed", color="gray50") +
  scale_x_continuous(breaks=seq(0,30,5)) +
  scale_y_continuous(labels=scales::percent, limits=c(0,1)) +
  labs(title="Cumulative Variance Explained",
       x="Number of Components", y="Cumulative Variance") +
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill=NA, color="black", linewidth=1))

grid.arrange(pScree, pCumVar, ncol=2)

n50 <- which(cumVar >= 0.50)[1]
n80 <- which(cumVar >= 0.80)[1]
# PCs needed for 50%/80% variance
n50; n80

# PCA PROJECTION: VISUALIZING STRUCTURE
# =============================================================================
pcScores <- as.data.frame(pcaResult$x[, 1:10])
pcScores$CrimeLevel <- trueClass
pcScores$CrimeRate <- crimeTarget

pPCA12 <- ggplot(pcScores, aes(x=PC1, y=PC2, color=CrimeLevel)) +
  geom_point(size=2, alpha=0.6) +
  scale_color_viridis_d(option="cividis") +
  labs(x=paste0("PC1 (", round(100*propVar[1],1), "%)"),
       y=paste0("PC2 (", round(100*propVar[2],1), "%)")) +
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill=NA, color="black", linewidth=1),
        legend.position="bottom")

pPCA13 <- ggplot(pcScores, aes(x=PC1, y=PC3, color=CrimeLevel)) +
  geom_point(size=2, alpha=0.6) +
  scale_color_viridis_d(option="cividis") +
  labs(x=paste0("PC1 (", round(100*propVar[1],1), "%)"),
       y=paste0("PC2 (", round(100*propVar[2],1), "%)")) +
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill=NA, color="black", linewidth=1),
        legend.position="bottom")

pPCA23 <- ggplot(pcScores, aes(x=PC2, y=PC3, color=CrimeLevel)) +
  geom_point(size=2, alpha=0.6) +
  scale_color_viridis_d(option="cividis") +
  labs(x=paste0("PC1 (", round(100*propVar[1],1), "%)"),
       y=paste0("PC3 (", round(100*propVar[3],1), "%)")) +
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill=NA, color="black", linewidth=1),
        legend.position="bottom")

ggpubr::ggarrange(pPCA12, pPCA13, pPCA23, ncol = 3, common.legend = T, legend = "bottom")


# CLUSTERING IN REDUCED SPACE
# =============================================================================
nPCsToTry <- 1:min(50, n-1)
results <- data.frame(nPCs=nPCsToTry, ARI=NA, Accuracy=NA)

for (i in seq_along(nPCsToTry)) {
  nPC <- nPCsToTry[i]
  scores <- pcaResult$x[, 1:nPC, drop=FALSE]
  set.seed(2026)
  km <- kmeans(scores, centers=2, nstart=25)
  results$ARI[i] <- adjustedRandIndex(km$cluster, trueClass)
  results$Accuracy[i] <- max(mean(km$cluster == as.numeric(trueClass)),
    mean(km$cluster != as.numeric(trueClass)))
}

# plot accuracy vs number of PCs
pAccuracy <- ggplot(results, aes(x=nPCs, y=Accuracy)) +
  geom_line(color="steelblue", linewidth=1) +
  geom_point(color="steelblue", size=2) +
  geom_hline(yintercept=accFull, linetype="dashed", color="red") +
  scale_x_continuous(breaks=seq(0,50,5)) +
  scale_y_continuous(limits=c(min(results$Accuracy)-0.005, max(results$Accuracy)+0.005)) +
  labs(title="Accuracy vs Number of PCs",
       x="Number of Principal Components", y="Accuracy") +
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill=NA, color="black", linewidth=1))

pARI <- ggplot(results, aes(x=nPCs, y=ARI)) +
  geom_line(color="coral", linewidth=1) +
  geom_point(color="coral", size=2) +
  geom_hline(yintercept=ariFull, linetype="dashed", color="red") +
  scale_x_continuous(breaks=seq(0,50,5)) +
  scale_y_continuous(limits=c(min(results$ARI)-0.005, max(results$ARI)+0.005)) +
  labs(title="ARI vs Number of PCs",
       x="Number of Principal Components", y="ARI") +
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill=NA, color="black", linewidth=1))

grid.arrange(pAccuracy, pARI, ncol=2)

# best result (The best result was 1, thats fine in practice
# but for demonstration lets use the second best)
bestIdx <- which.max(results$ARI[-1])
bestNPC <- results$nPCs[bestIdx]
bestARI <- results$ARI[bestIdx]
bestAcc <- results$Accuracy[bestIdx]

bestNPC
cat("ARI:", round(bestARI, 3), "\n")
cat("Accuracy:", round(bestAcc, 3), "\n")


# CLUSTERING VISUALIZATION IN PC SPACE
# =============================================================================
set.seed(2026)
kmeansPCA <- kmeans(pcaResult$x[, 1:bestNPC], centers=2, nstart=25)
pcScores$ClusterPCA <- factor(kmeansPCA$cluster)

pClusterPCA <- ggplot(pcScores, aes(x=PC1, y=PC2, color=ClusterPCA, shape=CrimeLevel)) +
  geom_point(size=2.5, alpha=0.7) +
  scale_color_viridis_d(option="cividis") +
  labs(title=paste0("K-Means Clustering (", bestNPC, " PCs)"),
       x="PC1", y="PC2", color="Cluster", shape="True Class") +
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill=NA, color="black", linewidth=1),
        legend.position="bottom")
pClusterPCA

# INTERPRETING PRINCIPAL COMPONENTS
# =============================================================================
loadings <- pcaResult$rotation[, 1:bestIdx]
# top features for PC1
pc1Loadings <- sort(loadings[,1], decreasing=TRUE)
# top variables loading positively on PC1
print(head(pc1Loadings, 10))
# top variables loading negatively on PC1
print(tail(pc1Loadings, 10))

# top features for PC2
pc2Loadings <- sort(loadings[,2], decreasing=TRUE)
print(head(pc2Loadings, 10))
print(tail(pc2Loadings, 10))

loadingDf <- data.frame(PC1 = loadings[,1], PC2 = loadings[,2], PC3 = loadings[,3]) %>%
  pivot_longer(everything(), names_to="PC", values_to="Loading")

pLoadingDist <- ggplot(loadingDf, aes(x=Loading, fill=PC)) +
  geom_histogram(bins=30, alpha=0.6, position="identity") +
  facet_wrap(~PC, ncol=1) +
  scale_fill_viridis_d(option="cividis") +
  labs(title="Distribution of Feature Loadings",
       x="Loading", y="Count") +
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill=NA, color="black", linewidth=1))
pLoadingDist

# PC1 INTERPRETATION
# =============================================================================
pc1LoadDf <- data.frame(Feature = names(pc1Loadings), Loading = pc1Loadings) %>%
  mutate(Direction = ifelse(Loading > 0, "Positive", "Negative"))
# top 15 each direction
topPC1 <- rbind(head(pc1LoadDf, 15), tail(pc1LoadDf, 15))

pPC1Loadings <- ggplot(topPC1, aes(x=reorder(Feature, Loading), y=Loading, fill=Direction)) +
  geom_bar(stat="identity", alpha=0.8) +
  coord_flip() +
  scale_fill_viridis_d(option="cividis") +
  labs(title="PC1 Loadings: Top 15 Features Each Direction",
       x="", y="Loading on PC1") +
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill=NA, color="black", linewidth=1),
        axis.text.y=element_text(size=7),
        legend.position="none")
pPC1Loadings

# CORRELATION OF PCs WITH CRIME RATE
# =============================================================================
# which PCs correlate with crime?
pcCrimeCor <- numeric(10)
for(k in 1:10) pcCrimeCor[k] <- cor(pcScores[,k], crimeTarget, use="complete.obs")
names(pcCrimeCor) <- paste0("PC", 1:10)
print(round(pcCrimeCor, 3))

pcCorDf <- data.frame(PC = factor(paste0("PC", 1:10), levels=paste0("PC", 1:10)),
  Correlation = pcCrimeCor)
pPCCrimeCor <- ggplot(pcCorDf, aes(x=PC, y=Correlation, fill=Correlation)) +
  geom_bar(stat="identity") +
  scale_fill_gradient2(low="blue", mid="white", high="red", midpoint=0) +
  labs(title="Correlation of Each PC with Violent Crime Rate",
       x="Principal Component", y="Correlation") +
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill=NA, color="black", linewidth=1),
        legend.position="none")
pPCCrimeCor


# FACTOR ANALYSIS
# =============================================================================
# factor analysis assumes latent structure generating observed correlations
# we'll use top 40 features (by variance) for stability
featureVars <- numeric()
for(i in 1:ncol(crimeFeatures)) featureVars[i] <- var(crimeFeatures[,i])
topFeatureIdx <- order(featureVars, decreasing=TRUE)[1:40]
featureSubset <- crimeFeatures[, topFeatureIdx]
kmoResult <- KMO(featureSubset)
cat("KMO MSA:", round(kmoResult$MSA, 3), "\n")
faParallel <- fa.parallel(featureSubset, fm="ml", fa="fa", n.iter=20,
                          show.legend=FALSE, main="Parallel Analysis")
nFactors <- min(faParallel$nfact, 8)

# suggested number of factors
nFactors

faResult <- fa(featureSubset, nfactors=nFactors, rotate="varimax", fm="ml")

# Root Mean Square Error of Approximation
round(faResult$RMSEA[1], 3)
# Tucker Lewis Index of factoring realiablility
round(faResult$TLI, 3)


# FACTOR SCORES FOR CLUSTERING
# =============================================================================
factorScores <- faResult$scores
set.seed(2026)
kmeansFA <- kmeans(factorScores, centers=2, nstart=25)

ariFA <- adjustedRandIndex(kmeansFA$cluster, trueClass)
accFA <- max( mean(kmeansFA$cluster == as.numeric(trueClass)),
  mean(kmeansFA$cluster != as.numeric(trueClass)))

round(ariFA, 3)
round(accFA, 3)

# visualize in factor space
faScoresDf <- as.data.frame(factorScores)
faScoresDf$CrimeLevel <- trueClass
faScoresDf$ClusterFA <- factor(kmeansFA$cluster)

pFA <- ggplot(faScoresDf, aes(x=ML1, y=ML2, color=ClusterFA, shape=CrimeLevel)) +
  geom_point(size=2.5, alpha=0.7) +
  scale_color_viridis_d(option="cividis") +
  labs(title=paste0("Factor Analysis Clustering (", ncol(factorScores), " factors)"),
       x="Factor 1", y="Factor 2", color="Cluster", shape="True Class") +
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill=NA, color="black", linewidth=1),
        legend.position="bottom")
pFA

# FACTOR INTERPRETATION
# =============================================================================
# examine factor loadings
faLoadings <- as.data.frame(unclass(faResult$loadings))
faLoadings$Feature <- rownames(faLoadings)

# top features per factor
for(f in 1:min(4, ncol(factorScores))) {
  fCol <- paste0("ML", f)
  topPos <- faLoadings %>%
    arrange(desc(!!sym(fCol))) %>%
    head(5) %>%
    select(Feature, !!fCol)
  topNeg <- faLoadings %>%
    arrange(!!sym(fCol)) %>%
    head(5) %>%
    select(Feature, !!fCol)
  
  cat("\nFactor", f, "- Top positive loadings:\n")
  print(topPos, row.names=FALSE)
  cat("Factor", f, "- Top negative loadings:\n")
  print(topNeg, row.names=FALSE)
}


# SILHOUETTE ANALYSIS (FOR PCA)
# =============================================================================

silFull <- silhouette(kmeansFull$cluster, fullDist)
avgSilFull <- mean(silFull[,3])

distPCAmat <- dist(pcaResult$x[, 1:bestNPC])
silPCA <- silhouette(kmeansPCA$cluster, distPCAmat)
avgSilPCA <- mean(silPCA[,3])

distFAmat <- dist(factorScores)
silFA <- silhouette(kmeansFA$cluster, distFAmat)
avgSilFA <- mean(silFA[,3])

round(avgSilFull, 3)
round(avgSilPCA, 3)
round(avgSilFA, 3)

# silhouette plot for PCA
silPCAdf <- data.frame(
  Sample = 1:n,
  Cluster = factor(silPCA[,1]),
  Silhouette = silPCA[,3]
) %>% arrange(Cluster, desc(Silhouette)) %>%
  mutate(Order = 1:n)

pSilhouette <- ggplot(silPCAdf, aes(x=Order, y=Silhouette, fill=Cluster)) +
  geom_bar(stat="identity", width=1) +
  geom_hline(yintercept=avgSilPCA, linetype="dashed", color="red") +
  scale_fill_viridis_d(option="cividis") +
  labs(title=paste0("Silhouette Plot (PCA, ", bestNPC, " PCs)"),
       x="Communities (ordered)", y="Silhouette Width") +
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill=NA, color="black", linewidth=1),
        axis.text.x=element_blank())
pSilhouette


# DISTANCE DISTRIBUTIONS COMPARISON
# =============================================================================

distFull <- as.vector(dist(featureScaled))
distPCA <- as.vector(dist(pcaResult$x[, 1:bestNPC]))
distFA <- as.vector(dist(factorScores))

distCompare <- data.frame(
  Distance = c(distFull, distPCA, distFA),
  Method = rep(c(paste0("Full (", p, "D)"), 
                 paste0("PCA (", bestNPC, "D)"), 
                 paste0("FA (", ncol(factorScores), "D)")), 
               each=length(distFull)))

pDistCompare <- ggplot(distCompare, aes(x=Distance, fill=Method)) +
  geom_density(alpha=0.5) +
  scale_fill_viridis_d(option="cividis") +
  labs(title="Distance Distributions by Method",
       x="Pairwise Distance", y="Density") +
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill=NA, color="black", linewidth=1),
        legend.position="bottom")
pDistCompare

# VARIABLE IMPORTANCE: LOADING MAGNITUDES
# =============================================================================
loadingsRetained <- pcaResult$rotation[, 1:bestIdx]
featureImportance <- rowSums(loadingsRetained^2)
featureImportanceWeighted <- rowSums(sweep(loadingsRetained^2, 2, propVar[1:bestNPC], "*"))

importanceDf <- data.frame(
  Feature = names(featureImportance),
  Importance = featureImportance,
  ImportanceWeighted = featureImportanceWeighted
) %>% arrange(desc(Importance))

importanceDf$CrimeCorrelation <- abs(featureCors[importanceDf$Feature])

pImportanceVsCor <- ggplot(importanceDf, aes(x=Importance, y=CrimeCorrelation)) +
  geom_point(alpha=0.5, size=2) +
  geom_smooth(method="loess", color="coral", se=T) +
  labs(title="PCA Importance vs Crime Correlation",
       x="PCA Importance", y="|Correlation with Crime Rate|") +
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill=NA, color="black", linewidth=1))
pImportanceVsCor

# TOP DISCRIMINATING FEATURES: EXPRESSION BY CLASS
# =============================================================================
# boxplots for top 12 features by class correlation
topDiscrimFeatures <- names(sort(abs(featureCors), decreasing=TRUE)[1:12])
discrimLong <- crimeFeatures[, topDiscrimFeatures] %>%
  mutate(CrimeLevel=trueClass) %>%
  pivot_longer(cols=all_of(topDiscrimFeatures), names_to="Feature", values_to="Value")

pDiscrimFeatures <- ggplot(discrimLong, aes(x=CrimeLevel, y=Value, fill=CrimeLevel)) +
  geom_boxplot(alpha=0.7, outlier.size=1) +
  facet_wrap(~Feature, scales="free_y") +
  scale_fill_viridis_d(option="cividis") +
  labs(title="Top 12 Discriminating Features",
       x="Crime Level", y="Value") +
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill=NA, color="black", linewidth=0.5),
        strip.background=element_rect(fill="gray90"),
        legend.position="none")
pDiscrimFeatures

# CUMULATIVE IMPORTANCE
# =============================================================================
importanceSorted <- sort(featureImportance, decreasing=TRUE)
cumImportance <- cumsum(importanceSorted) / sum(importanceSorted)

cumImpDf <- data.frame(nFeatures = 1:length(cumImportance),
  CumulativeImportance = cumImportance)

features50 <- which(cumImportance >= 0.50)[1]
features80 <- which(cumImportance >= 0.80)[1]
features90 <- which(cumImportance >= 0.90)[1]

pCumImportance <- ggplot(cumImpDf, aes(x=nFeatures, y=CumulativeImportance)) +
  geom_line(color="steelblue", linewidth=1) +
  geom_hline(yintercept=c(0.5, 0.8, 0.9), linetype="dashed", color="gray50") +
  geom_vline(xintercept=c(features50, features80, features90), linetype="dotted", color="coral") +
  scale_y_continuous(labels=scales::percent) +
  labs(title="Cumulative Variable Importance",
       x="Number of Features (ranked by importance)", y="Cumulative Importance") +
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill=NA, color="black", linewidth=1))
pCumImportance

# FACTOR ANALYSIS: COMMUNALITIES
# =============================================================================
communalities <- faResult$communality
commDf <- data.frame(Feature = names(communalities),
  Communality = communalities) %>% arrange(desc(Communality))

print(head(commDf, 10))

pCommunality <- ggplot(commDf, aes(x=Communality)) +
  geom_histogram(bins=20, fill="steelblue", alpha=0.7) +
  geom_vline(xintercept=mean(communalities), color="red", linewidth=1) +
  labs(subtitle=paste0("Communalities: Mean = ", round(mean(communalities), 3), 
                       "; high values = well-explained by factors"),
       x="Communality", y="Count") +
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill=NA, color="black", linewidth=1))
pCommunality

# FACTOR LOADINGS HEATMAP
# =============================================================================
topFeaturesPerFactor <- list()
for (f in 1:min(ncol(factorScores), 6)) {
  fCol <- paste0("ML", f)
  topFeaturesPerFactor[[f]] <- faLoadings %>%
    arrange(desc(abs(!!sym(fCol)))) %>%
    head(5) %>%
    pull(Feature)
}
topFAFeatures <- unique(unlist(topFeaturesPerFactor))

faLoadingsTop <- faLoadings %>% filter(Feature %in% topFAFeatures)
faLoadingsLong <- faLoadingsTop %>%
  pivot_longer(cols=starts_with("ML"), names_to="Factor", values_to="Loading")

pFALoadings <- ggplot(faLoadingsLong, aes(x=Factor, y=Feature, fill=Loading)) +
  geom_tile() +
  scale_fill_gradient2(low="blue", mid="white", high="red", midpoint=0) +
  labs(title="Factor Loadings: Top Features per Factor",
       x="Factor", y="") +
  theme(panel.grid=element_blank(),
        axis.text.y=element_text(size=7))
pFALoadings

# HEATMAP OF TOP FEATURES
# =============================================================================
topFeatureExpr <- crimeFeatures[, topDiscrimFeatures]
topFeatureScaled <- scale(topFeatureExpr)

heatDf <- as.data.frame(topFeatureScaled)
heatDf$Sample <- 1:n
heatDf$CrimeLevel <- trueClass
heatDf <- heatDf %>% arrange(CrimeLevel) %>% mutate(SampleOrder = 1:n)

heatLong <- heatDf %>%
  pivot_longer(cols=all_of(topDiscrimFeatures), names_to="Feature", values_to="Value")

pHeatmap <- ggplot(heatLong, aes(x=factor(SampleOrder), y=Feature, fill=Value)) +
  geom_tile() +
  scale_fill_viridis(option="cividis") +
  labs(title="Heatmap: Top Discriminating Features",
       x="Community", y="", fill="Scaled\nValue") +
  theme(panel.grid=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
pHeatmap

# STATE-LEVEL ANALYSIS
# =============================================================================
stateDf <- data.frame(
  State = stateCode,
  PC1 = pcScores$PC1,
  PC2 = pcScores$PC2,
  CrimeRate = crimeTarget
)

fipsToState <- c(
  "1"="AL", "2"="AK", "4"="AZ", "5"="AR", "6"="CA", "8"="CO", "9"="CT",
  "10"="DE", "11"="DC", "12"="FL", "13"="GA", "15"="HI", "16"="ID",
  "17"="IL", "18"="IN", "19"="IA", "20"="KS", "21"="KY", "22"="LA",
  "23"="ME", "24"="MD", "25"="MA", "26"="MI", "27"="MN", "28"="MS",
  "29"="MO", "30"="MT", "31"="NE", "32"="NV", "33"="NH", "34"="NJ",
  "35"="NM", "36"="NY", "37"="NC", "38"="ND", "39"="OH", "40"="OK",
  "41"="OR", "42"="PA", "44"="RI", "45"="SC", "46"="SD", "47"="TN",
  "48"="TX", "49"="UT", "50"="VT", "51"="VA", "53"="WA", "54"="WV",
  "55"="WI", "56"="WY"
)

stateAbbrev <- fipsToState[as.character(rawData$state)]
stateDf$State <- stateAbbrev

stateAgg <- stateDf %>%
  group_by(State) %>%
  summarize(
    MeanPC1 = mean(PC1, na.rm=TRUE),
    MeanPC2 = mean(PC2, na.rm=TRUE),
    MeanCrime = mean(CrimeRate, na.rm=TRUE),
    nCommunities = n(),
    .groups="drop"
  ) %>%
  filter(nCommunities >= 10)  

pStatePC1 <- ggplot(stateAgg, aes(x=reorder(factor(State), MeanPC1), y=MeanPC1, fill=MeanCrime)) +
  geom_bar(stat="identity") +
  scale_fill_viridis(option="cividis") +
  coord_flip() +
  labs(title="Mean PC1 Score by State",
       x="State Code", y="Mean PC1", fill="Mean\nCrime Rate") +
  theme(panel.grid=element_blank(),legend.position = "bottom",
        panel.border=element_rect(fill=NA, color="black", linewidth=1),
        axis.text.y=element_text(size=10))
pStatePC1

pStateCrime <- ggplot(stateAgg, aes(x=MeanPC1, y=MeanCrime)) +
  geom_point(aes(size=nCommunities), alpha=0.6, color="steelblue") +
  geom_smooth(method="loess", color="coral", se=T) +
  geom_text(aes(label=State), size=2, nudge_y=0.02) +
  labs(title="State-Level PC1 vs Crime Rate",
       x="Mean PC1 Score by State", y="Mean Crime Rate", size="# Communities") +
  theme(panel.grid=element_blank(), legend.position = "bottom",
        panel.border=element_rect(fill=NA, color="black", linewidth=1))
pStateCrime

# CONNECTION TO FAIRNESS
# =============================================================================
# the racial composition features are highly correlated with PC1
# this creates a fairness concern since clusters may encode race
raceFeatures <- c("racepctblack", "racePctWhite", "racePctAsian", "racePctHisp")

for(rf in raceFeatures) {
  if(rf %in% names(featureCors)) {
    cat(rf, "correlation with PC1:", 
        round(cor(crimeFeatures[[rf]], pcScores$PC1, use="complete.obs"), 3), "\n")
  }
}

raceDf <- crimeFeatures[, raceFeatures]
raceDf$Cluster <- kmeansPCA$cluster
raceDf$CrimeLevel <- trueClass

raceLong <- raceDf %>%
  pivot_longer(cols=all_of(raceFeatures), names_to="RaceVar", values_to="Proportion")

pRaceByCluster <- ggplot(raceLong, aes(x=factor(Cluster), y=Proportion, fill=factor(Cluster))) +
  geom_boxplot(alpha=0.7) +
  facet_wrap(~RaceVar, scales="free_y") +
  scale_fill_viridis_d(option="cividis") +
  labs(title="Racial Composition by Cluster",
       x="Cluster", y="Proportion") +
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill=NA, color="black", linewidth=0.5),
        strip.background=element_rect(fill="gray90"),
        legend.position="none")
pRaceByCluster

# PCA VISUALIZATION
# =============================================================================
pcPairs <- pcScores[, c("PC1", "PC2", "PC3", "CrimeLevel")]

p12 <- ggplot(pcPairs, aes(x=PC1, y=PC2, color=CrimeLevel)) +
  geom_point(alpha=0.5, size=1.5) +
  scale_color_viridis_d(option="cividis") +
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill=NA, color="black", linewidth=1),
        legend.position="bottom")

p13 <- ggplot(pcPairs, aes(x=PC1, y=PC3, color=CrimeLevel)) +
  geom_point(alpha=0.5, size=1.5) +
  scale_color_viridis_d(option="cividis") +
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill=NA, color="black", linewidth=1),
        legend.position="bottom")

p23 <- ggplot(pcPairs, aes(x=PC2, y=PC3, color=CrimeLevel)) +
  geom_point(alpha=0.5, size=1.5) +
  scale_color_viridis_d(option="cividis") +
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill=NA, color="black", linewidth=1),
        legend.position="bottom")

ggpubr::ggarrange(p12, p13, p23, ncol=3, common.legend = T, legend = "bottom")

# COMPARISON BAR PLOT
# =============================================================================

compLong <- comparisonDf %>%
  pivot_longer(cols=c(Accuracy, ARI, Silhouette), names_to="Metric", values_to="Value")

pComparison <- ggplot(compLong, aes(x=Method, y=Value, fill=Metric)) +
  geom_bar(stat="identity", position="dodge", alpha=0.8) +
  scale_fill_viridis_d(option="cividis") +
  labs(title="Method Comparison",
       x="Method", y="Value") +
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill=NA, color="black", linewidth=1),
        axis.text.x=element_text(angle=15, hjust=1))
pComparison
