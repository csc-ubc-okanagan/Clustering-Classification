# DIMENSIONALITY REDUCTION TECHNIQUES (2 of 2)
# Workshop 11: t-SNE and UMAP
# Data: MNIST Digits (subset) + UCI Communities and Crime
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
library(viridis)
library(cluster)
library(mclust)
library(Rtsne)
library(umap)
library(keras)
library(FNN)
library(vegan)

set.seed(2026)
theme_set(theme_minimal(base_size=12))
# PART 1: MNIST DIGITS DATASET
# MNIST: 28x28 grayscale images of handwritten digits (0-9)
# each image = 784 pixels = 784 variables
# we use a subset of digits because the dataset is huge 0, 1, 4, 7

mnist <- dataset_mnist()
trainImages <- mnist$train$x  
trainLabels <- mnist$train$y 
mnistPixels <- array_reshape(trainImages, c(nrow(trainImages), 28 * 28))
mnistLabels <- trainLabels

# subset to digits 0, 1, 4, 7 
selectedDigits <- c(0, 1, 4, 7)
digitIdx <- mnistLabels %in% selectedDigits
mnistLabelsSubset <- mnistLabels[digitIdx]
mnistPixelsSubset <- mnistPixels[digitIdx, ]

# now we are going to reduce to just 500
# of each number, because these methods can be slow on
# a laptop.
set.seed(2026)
nPerDigit <- 500
sampleIdx <- c()
for(d in selectedDigits) {
  digitRows <- which(mnistLabelsSubset == d)
  sampleIdx <- c(sampleIdx, sample(digitRows, min(nPerDigit, length(digitRows))))
}

mnistLabelsSubset <- factor(mnistLabelsSubset[sampleIdx])
mnistPixelsSubset <- mnistPixelsSubset[sampleIdx, ]
nMnist <- nrow(mnistPixelsSubset)
pMnist <- ncol(mnistPixelsSubset)

# function to plot a digit
plotDigit <- function(pixels, label="") {
  img <- matrix(as.numeric(pixels), nrow=28, ncol=28, byrow=TRUE)
  img <- t(apply(img, 2, rev))
  image(img, col=gray.colors(256, start=1, end=0), axes=FALSE, main=label)
}

# here's 2 of each number
par(mfrow=c(2, 4), mar=c(1, 1, 2, 1))
for(d in selectedDigits) {
  idx <- which(mnistLabelsSubset == d)[1:2]
  for(i in idx) plotDigit(mnistPixelsSubset[i, ])
}
par(mfrow=c(1, 1), mar=c(5, 4, 4, 2) + 0.1)
mnistScaled <- mnistPixelsSubset / 255

# lets revisit PCA from last time
pcaMnist <- prcomp(mnistScaled, center=TRUE, scale.=FALSE)
pcScoresMnist <- pcaMnist$x
varExplained <- pcaMnist$sdev^2 / sum(pcaMnist$sdev^2)
cumVar <- cumsum(varExplained)

round(varExplained[1] * 100, 1) # var explained first PCA
round(cumVar[2] * 100, 1)       # var explained first 2 PCAs
round(cumVar[10] * 100, 1)      # var explained first 10 PCAs
round(cumVar[50] * 100, 1)      # var explained first 50 PCAs

# PCA projection plot
pcaDfMnist <- data.frame(PC1 = pcScoresMnist[, 1],
  PC2 = pcScoresMnist[, 2],
  Digit = mnistLabelsSubset)

pPcaMnist <- ggplot(pcaDfMnist, aes(x=PC1, y=PC2, color=Digit)) +
  geom_point(alpha=0.6, size=1.5) +
  scale_color_viridis_d(option="cividis") +
  labs(title="PCA of MNIST",
       x="PC1", y="PC2", color="Digit") +
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill=NA, color="black", linewidth=1),
        legend.position="bottom")
pPcaMnist

# T-SNE: THE CORE IDEA
# T-SNE with 30 for perpexity
set.seed(2026)
tsneMnist <- Rtsne(mnistScaled,
                   dims = 2,
                   perplexity = 30,
                   verbose = FALSE,
                   max_iter = 1000,
                   check_duplicates = FALSE)

tsneDfMnist <- data.frame(TSNE1 = tsneMnist$Y[, 1],
  TSNE2 = tsneMnist$Y[, 2],
  Digit = mnistLabelsSubset)

pTsneMnist <- ggplot(tsneDfMnist, aes(x=TSNE1, y=TSNE2, color=Digit)) +
  geom_point(alpha=0.6, size=1.5) +
  scale_color_viridis_d(option="cividis") +
  labs(title="t-SNE of MNIST",
       x="t-SNE 1", y="t-SNE 2", color="Digit") +
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill=NA, color="black", linewidth=1),
        legend.position="bottom")
pTsneMnist

# looks pretty separated, some incorrect 1's marked at 7's

# UMAP ON MNIST
# lets look at umap for 15 neighbors and 0.1 min dist
set.seed(2026)
umapConfig <- umap.defaults
umapConfig$n_neighbors <- 15
umapConfig$min_dist <- 0.1
umapConfig$random_state <- 2026
umapMnist <- umap(mnistScaled, config=umapConfig)

umapDfMnist <- data.frame(UMAP1 = umapMnist$layout[, 1],
  UMAP2 = umapMnist$layout[, 2],
  Digit = mnistLabelsSubset)

pUmapMnist <- ggplot(umapDfMnist, aes(x=UMAP1, y=UMAP2, color=Digit)) +
  geom_point(alpha=0.6, size=1.5) +
  scale_color_viridis_d(option="cividis") +
  labs(title="UMAP of MNIST",
       x="UMAP 1", y="UMAP 2", color="Digit") +
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill=NA, color="black", linewidth=1),
        legend.position="bottom")
pUmapMnist

# well, thats quite separated! Same potential mislabelling of 1 and 7

grid.arrange(pPcaMnist, pTsneMnist, pUmapMnist, ncol=3)

# PERPLEXITY (t-SNE) 
# we looked at 30 perplexity before, lets see what its actually doing
perplexities <- c(5, 15, 30, 50)
tsneListMnist <- list()

for(perp in perplexities) {
  cat("perplexity =", perp, "...\n")
  set.seed(2026)
  tsneTemp <- Rtsne(mnistScaled,
                    dims = 2,
                    perplexity = perp,
                    verbose = FALSE,
                    max_iter = 1000,
                    check_duplicates = FALSE)
  tsneListMnist[[as.character(perp)]] <- data.frame(
    TSNE1 = tsneTemp$Y[, 1],
    TSNE2 = tsneTemp$Y[, 2],
    Digit = mnistLabelsSubset,
    Perplexity = paste("Perplexity =", perp))
}

tsnePerpMnist <- bind_rows(tsneListMnist)
tsnePerpMnist$Perplexity <- factor(tsnePerpMnist$Perplexity,
                                   levels = paste("Perplexity =", perplexities))

pTsnePerpMnist <- ggplot(tsnePerpMnist, aes(x=TSNE1, y=TSNE2, color=Digit)) +
  geom_point(alpha=0.5, size=1) +
  scale_color_viridis_d(option="cividis") +
  facet_wrap(~Perplexity, scales="free") +
  labs(x="t-SNE 1", y="t-SNE 2", color="Digit") +
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill=NA, color="black", linewidth=1),
        legend.position="bottom",
        strip.background=element_rect(fill="gray90"))
pTsnePerpMnist

# N_NEIGHBORS (UMAP)
# another parameter we need to look at is the neighbours for UMAP,
# lets take a look while keeping the other parameter of min_dist 
# left at 0.1
nNeighborValues <- c(5, 15, 30, 50)

umapListMnist <- list()
for(nn in nNeighborValues) {
  cat("n_neighbors =", nn, "...\n")
  set.seed(2026)
  umapConfig$n_neighbors <- nn
  umapConfig$min_dist <- 0.1
  umapTemp <- umap(mnistScaled, config=umapConfig)
  umapListMnist[[as.character(nn)]] <- data.frame(
    UMAP1 = umapTemp$layout[, 1],
    UMAP2 = umapTemp$layout[, 2],
    Digit = mnistLabelsSubset,
    NNeighbors = paste("n_neighbors =", nn))
}

umapNnMnist <- bind_rows(umapListMnist)
umapNnMnist$NNeighbors <- factor(umapNnMnist$NNeighbors,
                                 levels = paste("n_neighbors =", nNeighborValues))

pUmapNnMnist <- ggplot(umapNnMnist, aes(x=UMAP1, y=UMAP2, color=Digit)) +
  geom_point(alpha=0.5, size=1) +
  scale_color_viridis_d(option="cividis") +
  facet_wrap(~NNeighbors, scales="free") +
  labs(x="UMAP 1", y="UMAP 2", color="Digit") +
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill=NA, color="black", linewidth=1),
        legend.position="bottom",
        strip.background=element_rect(fill="gray90"))
pUmapNnMnist

# MIN_DIST (UMAP) 
# now we hold n_neighbors fixed at 15 and try some different min_dist values
minDistValues <- c(0.1, 0.25, 0.5, 0.75)

umapDistListMnist <- list()
for(md in minDistValues) {
  cat("Running UMAP with min_dist =", md, "...\n")
  set.seed(2026)
  umapConfig$n_neighbors <- 15
  umapConfig$min_dist <- md
  umapTemp <- umap(mnistScaled, config=umapConfig)
  umapDistListMnist[[as.character(md)]] <- data.frame(
    UMAP1 = umapTemp$layout[, 1],
    UMAP2 = umapTemp$layout[, 2],
    Digit = mnistLabelsSubset,
    MinDist = paste("min_dist =", md))
}

umapDistMnist <- bind_rows(umapDistListMnist)
umapDistMnist$MinDist <- factor(umapDistMnist$MinDist,
                                levels = paste("min_dist =", minDistValues))

pUmapDistMnist <- ggplot(umapDistMnist, aes(x=UMAP1, y=UMAP2, color=Digit)) +
  geom_point(alpha=0.5, size=1) +
  scale_color_viridis_d(option="cividis") +
  facet_wrap(~MinDist, scales="free") +
  labs(x="UMAP 1", y="UMAP 2", color="Digit") +
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill=NA, color="black", linewidth=1),
        legend.position="bottom",
        strip.background=element_rect(fill="gray90"))
pUmapDistMnist

# CLUSTERING PERFORMANCE ON MNIST
# lets cluster only the first 2 returned vectors from all three of
# PCA, UMAP, and t-SNE and see how well they do
k <- length(selectedDigits)

# PCA (2D)
set.seed(2026)
kmeansPcaMnist <- kmeans(pcScoresMnist[, 1:2], centers=k, nstart=25)
ariPcaMnist <- adjustedRandIndex(kmeansPcaMnist$cluster, mnistLabelsSubset)
accPcaMnist <- sum(diag(Thresher::matchLabels(table(kmeansPcaMnist$cluster, mnistLabelsSubset))))/2000

# PCA (50D)
set.seed(2026)
kmeansPca50Mnist <- kmeans(pcScoresMnist[, 1:50], centers=k, nstart=25)
ariPca50Mnist <- adjustedRandIndex(kmeansPca50Mnist$cluster, mnistLabelsSubset)
accPca50Mnist <- sum(diag(Thresher::matchLabels(table(kmeansPca50Mnist$cluster, mnistLabelsSubset))))/2000

# t-SNE
set.seed(2026)
kmeansTsneMnist <- kmeans(tsneMnist$Y, centers=k, nstart=25)
ariTsneMnist <- adjustedRandIndex(kmeansTsneMnist$cluster, mnistLabelsSubset)
accTsneMnist <- sum(diag(Thresher::matchLabels(table(kmeansTsneMnist$cluster, mnistLabelsSubset))))/2000

# UMAP
umapConfig$n_neighbors <- 15
umapConfig$min_dist <- 0.1
set.seed(2026)
umapMnist <- umap(mnistScaled, config=umapConfig)
set.seed(2026)
kmeansUmapMnist <- kmeans(umapMnist$layout, centers=k, nstart=25)
ariUmapMnist <- adjustedRandIndex(kmeansUmapMnist$cluster, mnistLabelsSubset)
accUmapMnist <- sum(diag(Thresher::matchLabels(table(kmeansUmapMnist$cluster, mnistLabelsSubset))))/2000

mnistResults <- data.frame(
  Method = c("PCA (2D)", "PCA (50D)", "t-SNE", "UMAP"),
  Dimensions = c(2, 50, 2, 2),
  ARI = c(ariPcaMnist, ariPca50Mnist, ariTsneMnist, ariUmapMnist),
  ACC = c(accPcaMnist, accPca50Mnist, accTsneMnist, accUmapMnist))
mnistResults

mnistLong <- mnistResults |>
  pivot_longer(cols = c(ARI, ACC), 
               names_to = "Metric", 
               values_to = "Value")

ggplot(mnistLong, aes(x = Method, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
  scale_fill_viridis_d(option = "cividis") +
  labs(title = "Clustering Performance on MNIST (K-Means, k=4)",
       x = "Method", y = "Score") +
  ylim(0, 1) +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(fill = NA, color = "black", linewidth = 1))

# so even at 50 dimensions, PCA doesnt do as well as the nonlinear methods.

# PART 2: COMMUNITIES AND CRIME DATA
# lets revisit workshop 10
# the next bit is verbatim from workshop 10 to clean the data as before
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
lemasVars <- grep("^Lemas|^Polic|^Offic|^Racial", colNames, value=TRUE)
lemasVars <- setdiff(lemasVars, c("PolicPerPop", "PopDens", "PctUsePubTrans"))
excludeCols <- c("state", "county", "community", "communityname", "fold",
                 lemasVars, "ViolentCrimesPerPop")
featureCols <- setdiff(colNames, excludeCols)
crimeFeatures <- rawData[, featureCols]
whichNA <- sapply(crimeFeatures, function(x) any(is.na(x)))
crimeFeatures <- crimeFeatures[, !whichNA]
completeIdx <- complete.cases(crimeFeatures)
crimeFeatures <- crimeFeatures[completeIdx, ]
crimeTarget <- rawData$ViolentCrimesPerPop[completeIdx]
stateCode <- rawData$state[completeIdx]
n <- nrow(crimeFeatures)
p <- ncol(crimeFeatures)
crimeQ65 <- quantile(crimeTarget, 0.65, na.rm=TRUE)
madeupClassLabel <- factor(ifelse(crimeTarget >= crimeQ65, "HighCrime", "LowCrime"))
featureScaled <- scale(crimeFeatures)
dupRows <- duplicated(featureScaled)
if(sum(dupRows) > 0) {
  featureScaledUnique <- featureScaled[!dupRows, ]
  madeupClassLabelUnique <- madeupClassLabel[!dupRows]
  crimeTargetUnique <- crimeTarget[!dupRows]
} else {
  featureScaledUnique <- featureScaled
  madeupClassLabelUnique <- madeupClassLabel
  crimeTargetUnique <- crimeTarget
}

nCrime <- nrow(featureScaledUnique)

# PCA ON CRIME DATA
pcaCrime <- prcomp(featureScaledUnique, center=FALSE, scale.=FALSE)
pcScoresCrime <- pcaCrime$x
varExpCrime <- pcaCrime$sdev^2 / sum(pcaCrime$sdev^2)
cumVarCrime <- cumsum(varExpCrime)
round(cumVarCrime[2] * 100, 1) # cumulative variance on first PC
round(cumVarCrime[10] * 100, 1) # cumulative variance on first 10 PCs
pcaDfCrime <- data.frame(PC1 = pcScoresCrime[, 1],
  PC2 = pcScoresCrime[, 2],
  CrimeLevel = madeupClassLabelUnique,
  CrimeRate = crimeTargetUnique)

pPcaCrime <- ggplot(pcaDfCrime, aes(x=PC1, y=PC2, color=CrimeLevel)) +
  geom_point(alpha=0.6, size=1.5) +
  scale_color_viridis_d(option="cividis") +
  labs(title="PCA of Communities",
       x="PC1", y="PC2", color="Crime Level") +
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill=NA, color="black", linewidth=1),
        legend.position="bottom")
pPcaCrime

# T-SNE ON CRIME DATA
# we will use 30 perplexity againb
set.seed(2026)
tsneCrime <- Rtsne(featureScaledUnique,
                   dims = 2,
                   perplexity = 30,
                   verbose = FALSE,
                   max_iter = 1000,
                   check_duplicates = FALSE)

tsneDfCrime <- data.frame(TSNE1 = tsneCrime$Y[, 1],
  TSNE2 = tsneCrime$Y[, 2],
  CrimeLevel = madeupClassLabelUnique,
  CrimeRate = crimeTargetUnique)

pTsneCrime <- ggplot(tsneDfCrime, aes(x=TSNE1, y=TSNE2, color=CrimeLevel)) +
  geom_point(alpha=0.6, size=1.5) +
  scale_color_viridis_d(option="cividis") +
  labs(title="t-SNE of Communities",
       x="t-SNE 1", y="t-SNE 2", color="Crime Level") +
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill=NA, color="black", linewidth=1),
        legend.position="bottom")
pTsneCrime

# UMAP ON CRIME DATA
# lets use 15 neighbours and min_dist of 0.1 again
set.seed(2026)
umapConfig$n_neighbors <- 15
umapConfig$min_dist <- 0.1
umapCrime <- umap(featureScaledUnique, config=umapConfig)

umapDfCrime <- data.frame(UMAP1 = umapCrime$layout[, 1],
  UMAP2 = umapCrime$layout[, 2],
  CrimeLevel = madeupClassLabelUnique,
  CrimeRate = crimeTargetUnique)

pUmapCrime <- ggplot(umapDfCrime, aes(x=UMAP1, y=UMAP2, color=CrimeLevel)) +
  geom_point(alpha=0.6, size=1.5) +
  scale_color_viridis_d(option="cividis") +
  labs(title="UMAP of Communities",
       x="UMAP 1", y="UMAP 2", color="Crime Level") +
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill=NA, color="black", linewidth=1),
        legend.position="bottom")
pUmapCrime

# lets look at some other plots
# t-SNE colored by continuous crime rate
pTsneCrimeRate <- ggplot(tsneDfCrime, aes(x=TSNE1, y=TSNE2, color=CrimeRate)) +
  geom_point(alpha=0.6, size=1.5) +
  scale_color_viridis_c(option="plasma") +
  labs(title="t-SNE",
       x="t-SNE 1", y="t-SNE 2", color="Crime Rate") +
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill=NA, color="black", linewidth=1),
        legend.position="bottom")

pUmapCrimeRate <- ggplot(umapDfCrime, aes(x=UMAP1, y=UMAP2, color=CrimeRate)) +
  geom_point(alpha=0.6, size=1.5) +
  scale_color_viridis_c(option="plasma") +
  labs(title="UMAP",
       x="UMAP 1", y="UMAP 2", color="Crime Rate") +
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill=NA, color="black", linewidth=1),
        legend.position="bottom")

grid.arrange(pTsneCrimeRate, pUmapCrimeRate, ncol=2)

# PERPLEXITY ON CRIME DATA (t-SNE)
# we will see like before perplexities effect on this data
tsneListCrime <- list()
for(perp in perplexities) {
  cat("perplexity =", perp, "...\n")
  set.seed(2026)
  tsneTemp <- Rtsne(featureScaledUnique,
                    dims = 2,
                    perplexity = perp,
                    verbose = FALSE,
                    max_iter = 1000,
                    check_duplicates = FALSE)
  tsneListCrime[[as.character(perp)]] <- data.frame(
    TSNE1 = tsneTemp$Y[, 1],
    TSNE2 = tsneTemp$Y[, 2],
    CrimeLevel = madeupClassLabelUnique,
    Perplexity = paste("Perplexity =", perp))
}

tsnePerpCrime <- bind_rows(tsneListCrime)
tsnePerpCrime$Perplexity <- factor(tsnePerpCrime$Perplexity,
                                   levels = paste("Perplexity =", perplexities))

pTsnePerpCrime <- ggplot(tsnePerpCrime, aes(x=TSNE1, y=TSNE2, color=CrimeLevel)) +
  geom_point(alpha=0.5, size=1) +
  scale_color_viridis_d(option="cividis") +
  facet_wrap(~Perplexity, scales="free") +
  labs(x="t-SNE 1", y="t-SNE 2", color="Crime Level") +
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill=NA, color="black", linewidth=1),
        legend.position="bottom",
        strip.background=element_rect(fill="gray90"))
pTsnePerpCrime
# notice the perplexities dont change too much here compared to the MNIST data

# N_NEIGHBORS ON CRIME DATA (UMAP)
# lets do the same thing as with MNIST for neighbours for UMAP
umapListCrime <- list()
for(nn in nNeighborValues) {
  cat("n_neighbors =", nn, "...\n")
  set.seed(2026)
  umapConfig$n_neighbors <- nn
  umapConfig$min_dist <- 0.1
  umapTemp <- umap(featureScaledUnique, config=umapConfig)
  umapListCrime[[as.character(nn)]] <- data.frame(
    UMAP1 = umapTemp$layout[, 1],
    UMAP2 = umapTemp$layout[, 2],
    CrimeLevel = madeupClassLabelUnique,
    NNeighbors = paste("n_neighbors =", nn))
}

umapNnCrime <- bind_rows(umapListCrime)
umapNnCrime$NNeighbors <- factor(umapNnCrime$NNeighbors,
                                 levels = paste("n_neighbors =", nNeighborValues))

pUmapNnCrime <- ggplot(umapNnCrime, aes(x=UMAP1, y=UMAP2, color=CrimeLevel)) +
  geom_point(alpha=0.5, size=1) +
  scale_color_viridis_d(option="cividis") +
  facet_wrap(~NNeighbors, scales="free") +
  labs(x="UMAP 1", y="UMAP 2", color="Crime Level") +
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill=NA, color="black", linewidth=1),
        legend.position="bottom",
        strip.background=element_rect(fill="gray90"))
pUmapNnCrime

# MIN_DIST ON CRIME DATA (UMAP)
# and again for min_dist
umapDistListCrime <- list()
for(md in minDistValues) {
  cat("min_dist =", md, "...\n")
  set.seed(2026)
  umapConfig$n_neighbors <- 15
  umapConfig$min_dist <- md
  umapTemp <- umap(featureScaledUnique, config=umapConfig)
  umapDistListCrime[[as.character(md)]] <- data.frame(
    UMAP1 = umapTemp$layout[, 1],
    UMAP2 = umapTemp$layout[, 2],
    CrimeLevel = madeupClassLabelUnique,
    MinDist = paste("min_dist =", md))
}

umapDistCrime <- bind_rows(umapDistListCrime)
umapDistCrime$MinDist <- factor(umapDistCrime$MinDist,
                                levels = paste("min_dist =", minDistValues))

pUmapDistCrime <- ggplot(umapDistCrime, aes(x=UMAP1, y=UMAP2, color=CrimeLevel)) +
  geom_point(alpha=0.5, size=1) +
  scale_color_viridis_d(option="cividis") +
  facet_wrap(~MinDist, scales="free") +
  labs(x="UMAP 1", y="UMAP 2", color="Crime Level") +
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill=NA, color="black", linewidth=1),
        legend.position="bottom",
        strip.background=element_rect(fill="gray90"))
pUmapDistCrime

# this definitely has an effect.

# CLUSTERING
# time to cluster!
# K-means with k=2
# t-SNE
set.seed(2026)
kmeansTsneCrime <- kmeans(tsneCrime$Y, centers=2, nstart=25)
ariTsneCrime <- adjustedRandIndex(kmeansTsneCrime$cluster, madeupClassLabelUnique)
accTsneCrime <- max(
  mean(kmeansTsneCrime$cluster == as.numeric(madeupClassLabelUnique)),
  mean(kmeansTsneCrime$cluster != as.numeric(madeupClassLabelUnique)))

# UMAP
umapConfig$n_neighbors <- 15
umapConfig$min_dist <- 0.1
set.seed(2026)
umapCrime <- umap(featureScaledUnique, config=umapConfig)
set.seed(2026)
kmeansUmapCrime <- kmeans(umapCrime$layout, centers=2, nstart=25)
ariUmapCrime <- adjustedRandIndex(kmeansUmapCrime$cluster, madeupClassLabelUnique)
accUmapCrime <- max(
  mean(kmeansUmapCrime$cluster == as.numeric(madeupClassLabelUnique)),
  mean(kmeansUmapCrime$cluster != as.numeric(madeupClassLabelUnique)))

tsneDfCrime$Cluster <- factor(kmeansTsneCrime$cluster)
umapDfCrime$Cluster <- factor(kmeansUmapCrime$cluster)

pTsneClusterCrime <- ggplot(tsneDfCrime, aes(x=TSNE1, y=TSNE2, color=Cluster, shape=CrimeLevel)) +
  geom_point(alpha=0.6, size=2) +
  scale_color_viridis_d(option="turbo") +
  labs(title="K-Means in t-SNE Space",
       x="t-SNE 1", y="t-SNE 2", color="Cluster", shape="True Class") +
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill=NA, color="black", linewidth=1),
        legend.position="bottom")

pUmapClusterCrime <- ggplot(umapDfCrime, aes(x=UMAP1, y=UMAP2, color=Cluster, shape=CrimeLevel)) +
  geom_point(alpha=0.6, size=2) +
  scale_color_viridis_d(option="turbo") +
  labs(title="K-Means in UMAP Space",
       x="UMAP 1", y="UMAP 2", color="Cluster", shape="True Class") +
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill=NA, color="black", linewidth=1),
        legend.position="bottom")

ggpubr::ggarrange(pTsneClusterCrime, pUmapClusterCrime, ncol=2, common.legend = T,
                  legend = "bottom")

# comparison
comparisonCrime <- data.frame(
  Method = c("Full Space", "PCA (2D)", "PCA (10D)", "t-SNE", "UMAP"),
  Dimensions = c(p, 2, 10, 2, 2),
  ARI = NA, Accuracy = NA)

# Full space
set.seed(2026)
kmeansFull <- kmeans(featureScaledUnique, centers=2, nstart=25)
comparisonCrime$ARI[1] <- adjustedRandIndex(kmeansFull$cluster, madeupClassLabelUnique)
comparisonCrime$Accuracy[1] <- max(
  mean(kmeansFull$cluster == as.numeric(madeupClassLabelUnique)),
  mean(kmeansFull$cluster != as.numeric(madeupClassLabelUnique)))

# PCA 2D
set.seed(2026)
kmeansPca2 <- kmeans(pcScoresCrime[, 1:2], centers=2, nstart=25)
comparisonCrime$ARI[2] <- adjustedRandIndex(kmeansPca2$cluster, madeupClassLabelUnique)
comparisonCrime$Accuracy[2] <- max(
  mean(kmeansPca2$cluster == as.numeric(madeupClassLabelUnique)),
  mean(kmeansPca2$cluster != as.numeric(madeupClassLabelUnique)))

# PCA 10D
set.seed(2026)
kmeansPca10 <- kmeans(pcScoresCrime[, 1:10], centers=2, nstart=25)
comparisonCrime$ARI[3] <- adjustedRandIndex(kmeansPca10$cluster, madeupClassLabelUnique)
comparisonCrime$Accuracy[3] <- max(
  mean(kmeansPca10$cluster == as.numeric(madeupClassLabelUnique)),
  mean(kmeansPca10$cluster != as.numeric(madeupClassLabelUnique)))

# t-SNE
comparisonCrime$ARI[4] <- ariTsneCrime
comparisonCrime$Accuracy[4] <- accTsneCrime

# UMAP
comparisonCrime$ARI[5] <- ariUmapCrime
comparisonCrime$Accuracy[5] <- accUmapCrime
comparisonCrime

compCrimeLong <- comparisonCrime %>%
  pivot_longer(cols=c(ARI, Accuracy), names_to="Metric", values_to="Value")

pComparisonCrime <- ggplot(compCrimeLong, aes(x=Method, y=Value, fill=Metric)) +
  geom_bar(stat="identity", position="dodge", alpha=0.8) +
  scale_fill_viridis_d(option="cividis") +
  labs(x="Method", y="Score") +
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill=NA, color="black", linewidth=1),
        legend.position="bottom",
        axis.text.x=element_text(angle=30, hjust=1))
pComparisonCrime

# Tsne and UMAP may have different results based on initialization
# so lets see what happens here
nRuns <- 10
stabilityResults <- data.frame()

for(i in 1:nRuns) {
  set.seed(i)
  print(paste0(i, " of 10"))
  tsneTmp <- Rtsne(featureScaledUnique, dims=2, perplexity=30,
                   verbose=FALSE, max_iter=1000, check_duplicates=FALSE)
  set.seed(2026)
  kmTsne <- kmeans(tsneTmp$Y, centers=2, nstart=25)
  ariTsneTmp <- adjustedRandIndex(kmTsne$cluster, madeupClassLabelUnique)
  umapConfig$random_state <- i
  umapTmp <- umap(featureScaledUnique, config=umapConfig)
  set.seed(2026)
  kmUmap <- kmeans(umapTmp$layout, centers=2, nstart=25)
  ariUmapTmp <- adjustedRandIndex(kmUmap$cluster, madeupClassLabelUnique)
  stabilityResults <- rbind(stabilityResults, data.frame(
    Run = i, tSNE = ariTsneTmp, UMAP = ariUmapTmp))
}

cat("\nt-SNE ARI: Mean =", round(mean(stabilityResults$tSNE), 3),
    ", SD =", round(sd(stabilityResults$tSNE), 3), "\n")
cat("UMAP ARI: Mean =", round(mean(stabilityResults$UMAP), 3),
    ", SD =", round(sd(stabilityResults$UMAP), 3), "\n")

stabilityLong <- stabilityResults %>%
  pivot_longer(cols=c(tSNE, UMAP), names_to="Method", values_to="ARI")

pStability <- ggplot(stabilityLong, aes(x=Method, y=ARI, fill=Method)) +
  geom_boxplot(alpha=0.7) +
  scale_fill_viridis_d(option="cividis") +
  labs(title="Stability of t-SNE and UMAP",
       x="Method", y="ARI") +
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill=NA, color="black", linewidth=1),
        legend.position="none")
pStability

# LETS LOOK BACK AT FAIRNESS IDEAS
racePctBlack <- crimeFeatures$racepctblack[!dupRows]
tsneDfCrime$racepctblack <- racePctBlack
umapDfCrime$racepctblack <- racePctBlack

pTsneRace <- ggplot(tsneDfCrime, aes(x=TSNE1, y=TSNE2, color=racepctblack)) +
  geom_point(alpha=0.6, size=1.5) +
  scale_color_viridis_c(option="plasma") +
  labs(title="t-SNE: Racial Composition",
       x="t-SNE 1", y="t-SNE 2", color="% Black") +
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill=NA, color="black", linewidth=1),
        legend.position="bottom")

pUmapRace <- ggplot(umapDfCrime, aes(x=UMAP1, y=UMAP2, color=racepctblack)) +
  geom_point(alpha=0.6, size=1.5) +
  scale_color_viridis_c(option="plasma") +
  labs(title="UMAP: Racial Composition",
       x="UMAP 1", y="UMAP 2", color="% Black") +
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill=NA, color="black", linewidth=1),
        legend.position="bottom")

grid.arrange(pTsneRace, pUmapRace, ncol=2)

# racial composition by cluster
pRaceClusterTsne <- ggplot(tsneDfCrime, aes(x=Cluster, y=racepctblack, fill=Cluster)) +
  geom_boxplot(alpha=0.7) +
  scale_fill_viridis_d(option="cividis") +
  labs(title="t-SNE Clusters",
       x="Cluster", y="% Black Population") +
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill=NA, color="black", linewidth=1),
        legend.position="none")

pRaceClusterUmap <- ggplot(umapDfCrime, aes(x=Cluster, y=racepctblack, fill=Cluster)) +
  geom_boxplot(alpha=0.7) +
  scale_fill_viridis_d(option="cividis") +
  labs(title="UMAP Clusters",
       x="Cluster", y="% Black Population") +
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill=NA, color="black", linewidth=1),
        legend.position="none")

grid.arrange(pRaceClusterTsne, pRaceClusterUmap, ncol=2)

# Lets look at some additional plots
# help functions
knnPreservation <- function(Xhigh, Xlow, k=10) {
  nnHigh <- get.knn(Xhigh, k=k)$nn.index
  nnLow  <- get.knn(Xlow,  k=k)$nn.index
  overlap <- sapply(1:nrow(Xhigh), function(i) {
    length(intersect(nnHigh[i, ], nnLow[i, ])) / k})
  mean(overlap)
}

distanceCorrelation <- function(Xhigh, Xlow) cor(as.vector(dist(Xhigh)), as.vector(dist(Xlow)))

clusterAccuracy <- function(clusterAssign, trueLabels) {
  max(mean(clusterAssign == as.numeric(trueLabels)),
    mean(clusterAssign != as.numeric(trueLabels)))}

meanSilhouette <- function(clusterAssign, X) {
  sil <- silhouette(clusterAssign, dist(X))
  mean(sil[, 3])}

# MNIST: NEIGHBORHOOD PRESERVATION
kVals <- c(5, 10, 20)
mnistNeighborhood <- data.frame()
for(k in kVals) {
  mnistNeighborhood <- rbind(mnistNeighborhood, data.frame(
    Method = c("PCA (2D)", "PCA (50D)", "t-SNE", "UMAP"),
    k = k,
    Preservation = c(
      knnPreservation(mnistScaled, pcScoresMnist[, 1:2], k),
      knnPreservation(mnistScaled, pcScoresMnist[, 1:50], k),
      knnPreservation(mnistScaled, tsneMnist$Y, k),
      knnPreservation(mnistScaled, umapMnist$layout, k))))
}

print(mnistNeighborhood)

pMnistNeighborhood <- ggplot(mnistNeighborhood, aes(x=Method, y=Preservation, fill=factor(k))) +
  geom_bar(stat="identity", position="dodge", alpha=0.8) +
  scale_fill_viridis_d(option="cividis") +
  labs(title="MNIST Neighborhood Preservation",
       x="Method", y="Mean k-NN Overlap", fill="k") +
  ylim(0, 1) +
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill=NA, color="black", linewidth=1),
        legend.position="bottom")
pMnistNeighborhood

# MNIST: GLOBAL DISTANCE PRESERVATION
mnistDistanceResults <- data.frame(
  Method = c("PCA (2D)", "PCA (50D)", "t-SNE", "UMAP"),
  DistanceCorrelation = c(
    distanceCorrelation(mnistScaled, pcScoresMnist[, 1:2]),
    distanceCorrelation(mnistScaled, pcScoresMnist[, 1:50]),
    distanceCorrelation(mnistScaled, tsneMnist$Y),
    distanceCorrelation(mnistScaled, umapMnist$layout)))
print(mnistDistanceResults)

pMnistDistance <- ggplot(mnistDistanceResults, aes(x=Method, y=DistanceCorrelation, fill=Method)) +
  geom_bar(stat="identity", alpha=0.8) +
  scale_fill_viridis_d(option="cividis") +
  labs(title="MNIST Global Distance Preservation",
       x="Method", y="Correlation of Pairwise Distances") +
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill=NA, color="black", linewidth=1),
        legend.position="none")
pMnistDistance

# MNIST: SILHOUETTE SCORES
mnistSilhouette <- data.frame(
  Method = c("PCA (2D)", "PCA (50D)", "t-SNE", "UMAP"),
  Silhouette = c(
    meanSilhouette(kmeansPcaMnist$cluster, pcScoresMnist[, 1:2]),
    meanSilhouette(kmeansPca50Mnist$cluster, pcScoresMnist[, 1:50]),
    meanSilhouette(kmeansTsneMnist$cluster, tsneMnist$Y),
    meanSilhouette(kmeansUmapMnist$cluster, umapMnist$layout)))
print(mnistSilhouette)

pMnistSilhouette <- ggplot(mnistSilhouette, aes(x=Method, y=Silhouette, fill=Method)) +
  geom_bar(stat="identity", alpha=0.8) +
  scale_fill_viridis_d(option="cividis") +
  labs(title="MNIST Mean Silhouette Width",
       x="Method", y="Mean Silhouette") +
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill=NA, color="black", linewidth=1),
        legend.position="none")
pMnistSilhouette

# MNIST: k-NN CLASSIFICATION IN REDUCED SPACE
set.seed(2026)
idxTrainMnist <- sample(1:nrow(mnistScaled), size = round(0.7 * nrow(mnistScaled)))
idxTestMnist <- setdiff(1:nrow(mnistScaled), idxTrainMnist)
kClass <- 5

predPca2Mnist <- knn(train=pcScoresMnist[idxTrainMnist, 1:2],
                     test=pcScoresMnist[idxTestMnist, 1:2],
                     cl=mnistLabelsSubset[idxTrainMnist], k=kClass)

predPca50Mnist <- knn(train=pcScoresMnist[idxTrainMnist, 1:50],
                      test=pcScoresMnist[idxTestMnist, 1:50],
                      cl=mnistLabelsSubset[idxTrainMnist], k=kClass)

predTsneMnist <- knn(train=tsneMnist$Y[idxTrainMnist, ],
                     test=tsneMnist$Y[idxTestMnist, ],
                     cl=mnistLabelsSubset[idxTrainMnist], k=kClass)

predUmapMnist <- knn(train=umapMnist$layout[idxTrainMnist, ],
                     test=umapMnist$layout[idxTestMnist, ],
                     cl=mnistLabelsSubset[idxTrainMnist], k=kClass)

mnistClassResults <- data.frame(
  Method = c("PCA (2D)", "PCA (50D)", "t-SNE", "UMAP"),
  Accuracy = c(
    mean(predPca2Mnist == mnistLabelsSubset[idxTestMnist]),
    mean(predPca50Mnist == mnistLabelsSubset[idxTestMnist]),
    mean(predTsneMnist == mnistLabelsSubset[idxTestMnist]),
    mean(predUmapMnist == mnistLabelsSubset[idxTestMnist])))

print(mnistClassResults)

pMnistClass <- ggplot(mnistClassResults, aes(x=Method, y=Accuracy, fill=Method)) +
  geom_bar(stat="identity", alpha=0.8) +
  scale_fill_viridis_d(option="cividis") +
  labs(title="MNIST k-NN Classification Accuracy",
       x="Method", y="Accuracy") +
  ylim(0, 1) +
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill=NA, color="black", linewidth=1),
        legend.position="none")
pMnistClass

# MNIST: OUTLIER ANALYSIS

mnistOutlierIdxTsne <- order(sqrt(rowSums(tsneMnist$Y^2)), decreasing=TRUE)[1:9]
mnistOutlierIdxUmap <- order(sqrt(rowSums(umapMnist$layout^2)), decreasing=TRUE)[1:9]

par(mfrow=c(3, 3), mar=c(1, 1, 2, 1))
for(i in mnistOutlierIdxTsne) plotDigit(mnistPixelsSubset[i, ], 
                                        label=paste("t-SNE outlier:", mnistLabelsSubset[i]))
par(mfrow=c(1, 1))

par(mfrow=c(3, 3), mar=c(1, 1, 2, 1))
for(i in mnistOutlierIdxUmap) plotDigit(mnistPixelsSubset[i, ],
                                        label=paste("UMAP outlier:", mnistLabelsSubset[i]))
par(mfrow=c(1, 1))

# CRIME: NEIGHBORHOOD PRESERVATION
crimeNeighborhood <- data.frame()
for(k in kVals) {
  crimeNeighborhood <- rbind(crimeNeighborhood, data.frame(
    Method = c("PCA (2D)", "PCA (10D)", "t-SNE", "UMAP"),
    k = k,
    Preservation = c(
      knnPreservation(featureScaledUnique, pcScoresCrime[, 1:2], k),
      knnPreservation(featureScaledUnique, pcScoresCrime[, 1:10], k),
      knnPreservation(featureScaledUnique, tsneCrime$Y, k),
      knnPreservation(featureScaledUnique, umapCrime$layout, k)
    )))
}

print(crimeNeighborhood)

pCrimeNeighborhood <- ggplot(crimeNeighborhood, aes(x=Method, y=Preservation, fill=factor(k))) +
  geom_bar(stat="identity", position="dodge", alpha=0.8) +
  scale_fill_viridis_d(option="cividis") +
  labs(title="Crime Data Neighborhood Preservation",
       x="Method", y="Mean k-NN Overlap", fill="k") +
  ylim(0, 1) +
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill=NA, color="black", linewidth=1),
        legend.position="bottom")
pCrimeNeighborhood

# CRIME: GLOBAL DISTANCE PRESERVATION
crimeDistanceResults <- data.frame(
  Method = c("PCA (2D)", "PCA (10D)", "t-SNE", "UMAP"),
  DistanceCorrelation = c(
    distanceCorrelation(featureScaledUnique, pcScoresCrime[, 1:2]),
    distanceCorrelation(featureScaledUnique, pcScoresCrime[, 1:10]),
    distanceCorrelation(featureScaledUnique, tsneCrime$Y),
    distanceCorrelation(featureScaledUnique, umapCrime$layout)))

print(crimeDistanceResults)

pCrimeDistance <- ggplot(crimeDistanceResults, aes(x=Method, y=DistanceCorrelation, fill=Method)) +
  geom_bar(stat="identity", alpha=0.8) +
  scale_fill_viridis_d(option="cividis") +
  labs(title="Crime Data Global Distance Preservation",
       x="Method", y="Correlation of Pairwise Distances") +
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill=NA, color="black", linewidth=1),
        legend.position="none")
pCrimeDistance

# CRIME: SILHOUETTE SCORES
crimeSilhouette <- data.frame(
  Method = c("Full Space", "PCA (2D)", "PCA (10D)", "t-SNE", "UMAP"),
  Silhouette = c(
    meanSilhouette(kmeansFull$cluster, featureScaledUnique),
    meanSilhouette(kmeansPca2$cluster, pcScoresCrime[, 1:2]),
    meanSilhouette(kmeansPca10$cluster, pcScoresCrime[, 1:10]),
    meanSilhouette(kmeansTsneCrime$cluster, tsneCrime$Y),
    meanSilhouette(kmeansUmapCrime$cluster, umapCrime$layout)))
print(crimeSilhouette)

pCrimeSilhouette <- ggplot(crimeSilhouette, aes(x=Method, y=Silhouette, fill=Method)) +
  geom_bar(stat="identity", alpha=0.8) +
  scale_fill_viridis_d(option="cividis") +
  labs(title="Crime Data Mean Silhouette Width",
       x="Method", y="Mean Silhouette") +
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill=NA, color="black", linewidth=1),
        legend.position="none",
        axis.text.x=element_text(angle=30, hjust=1))
pCrimeSilhouette

# CRIME: k-NN CLASSIFICATION IN REDUCED SPACE
set.seed(2026)
idxTrainCrime <- sample(1:nrow(featureScaledUnique), size = round(0.7 * nrow(featureScaledUnique)))
idxTestCrime <- setdiff(1:nrow(featureScaledUnique), idxTrainCrime)

predPca2Crime <- knn(train=pcScoresCrime[idxTrainCrime, 1:2],
                     test=pcScoresCrime[idxTestCrime, 1:2],
                     cl=madeupClassLabelUnique[idxTrainCrime], k=5)

predPca10Crime <- knn(train=pcScoresCrime[idxTrainCrime, 1:10],
                      test=pcScoresCrime[idxTestCrime, 1:10],
                      cl=madeupClassLabelUnique[idxTrainCrime], k=5)

predTsneCrime <- knn(train=tsneCrime$Y[idxTrainCrime, ],
                     test=tsneCrime$Y[idxTestCrime, ],
                     cl=madeupClassLabelUnique[idxTrainCrime], k=5)

predUmapCrime <- knn(train=umapCrime$layout[idxTrainCrime, ],
                     test=umapCrime$layout[idxTestCrime, ],
                     cl=madeupClassLabelUnique[idxTrainCrime], k=5)

crimeClassResults <- data.frame(
  Method = c("PCA (2D)", "PCA (10D)", "t-SNE", "UMAP"),
  Accuracy = c(
    mean(predPca2Crime == madeupClassLabelUnique[idxTestCrime]),
    mean(predPca10Crime == madeupClassLabelUnique[idxTestCrime]),
    mean(predTsneCrime == madeupClassLabelUnique[idxTestCrime]),
    mean(predUmapCrime == madeupClassLabelUnique[idxTestCrime])))

print(crimeClassResults)

pCrimeClass <- ggplot(crimeClassResults, aes(x=Method, y=Accuracy, fill=Method)) +
  geom_bar(stat="identity", alpha=0.8) +
  scale_fill_viridis_d(option="cividis") +
  labs(title="Crime Data k-NN Classification Accuracy",
       x="Method", y="Accuracy") +
  ylim(0, 1) +
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill=NA, color="black", linewidth=1),
        legend.position="none")
pCrimeClass

# CRIME: OVERLAYS USING EXISTING VARIABLES
crimeFeatureDf <- data.frame(
  UMAP1 = umapCrime$layout[, 1],
  UMAP2 = umapCrime$layout[, 2],
  TSNE1 = tsneCrime$Y[, 1],
  TSNE2 = tsneCrime$Y[, 2],
  CrimeRate = crimeTargetUnique,
  racepctblack = crimeFeatures$racepctblack[!dupRows],
  medIncome = crimeFeatures$medIncome[!dupRows],
  PctPopUnderPov = crimeFeatures$PctPopUnderPov[!dupRows],
  PctUnemployed = crimeFeatures$PctUnemployed[!dupRows],
  PctKids2Par = crimeFeatures$PctKids2Par[!dupRows]
)

pCrimeUmapIncome <- ggplot(crimeFeatureDf, aes(x=UMAP1, y=UMAP2, color=medIncome)) +
  geom_point(alpha=0.6, size=1.5) +
  scale_color_viridis_c(option="plasma") +
  labs(title="UMAP Median Income",
       x="UMAP 1", y="UMAP 2", color="") +
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill=NA, color="black", linewidth=1),
        legend.position="bottom")

pCrimeUmapPov <- ggplot(crimeFeatureDf, aes(x=UMAP1, y=UMAP2, color=PctPopUnderPov)) +
  geom_point(alpha=0.6, size=1.5) +
  scale_color_viridis_c(option="plasma") +
  labs(title="UMAP Poverty Rate",
       x="UMAP 1", y="UMAP 2", color="") +
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill=NA, color="black", linewidth=1),
        legend.position="bottom")

pCrimeUmapUnemp <- ggplot(crimeFeatureDf, aes(x=UMAP1, y=UMAP2, color=PctUnemployed)) +
  geom_point(alpha=0.6, size=1.5) +
  scale_color_viridis_c(option="plasma") +
  labs(title="UMAP Unemployment",
       x="UMAP 1", y="UMAP 2", color="") +
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill=NA, color="black", linewidth=1),
        legend.position="bottom")

pCrimeUmapFamily <- ggplot(crimeFeatureDf, aes(x=UMAP1, y=UMAP2, color=PctKids2Par)) +
  geom_point(alpha=0.6, size=1.5) +
  scale_color_viridis_c(option="plasma") +
  labs(title="UMAP %Kids in 2-Parent Households",
       x="UMAP 1", y="UMAP 2", color="") +
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill=NA, color="black", linewidth=1),
        legend.position="bottom")

ggpubr::ggarrange(pCrimeUmapIncome, pCrimeUmapPov, pCrimeUmapUnemp, pCrimeUmapFamily, ncol=2,
                 nrow = 2, common.legend = T, legend = "bottom")

# CRIME OUTLIERS
crimeOutlierTsne <- order(sqrt(rowSums(tsneCrime$Y^2)), decreasing=TRUE)[1:10]
crimeOutlierUmap <- order(sqrt(rowSums(umapCrime$layout^2)), decreasing=TRUE)[1:10]

crimeOutliersDf <- data.frame(
  Index = 1:nrow(crimeFeatureDf),
  CrimeRate = crimeTargetUnique,
  racepctblack = crimeFeatureDf$racepctblack,
  medIncome = crimeFeatureDf$medIncome,
  PctPopUnderPov = crimeFeatureDf$PctPopUnderPov,
  PctUnemployed = crimeFeatureDf$PctUnemployed,
  PctKids2Par = crimeFeatureDf$PctKids2Par)

# top 10 tSNE outliers
print(crimeOutliersDf[crimeOutlierTsne, ])
# top 10 UMAP outliers
print(crimeOutliersDf[crimeOutlierUmap, ])

