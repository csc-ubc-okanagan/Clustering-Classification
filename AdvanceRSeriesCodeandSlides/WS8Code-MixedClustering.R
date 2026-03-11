# MIXED-TYPE DATA CLUSTERING
# Workshop 8: Clustering Algorithms for Mixed-Type Data
library(cluster)
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
library(viridis)
library(kmed)
library(clustMixType)
library(dendextend)
library(aricode)
library(cowplot)


set.seed(2026)
theme_set(theme_minimal(base_size=12))

# last workshop we learned how to compute distances for mixed-type data
# today we use those distances to actually cluster the data
# 1. PAM (Partitioning Around Medoids) with Gower distance
# 2. K-Prototypes (extends K-means to mixed data)
# 3. Hierarchical clustering with Gower distance
# 4. Spectral clustering with mixed-type similarity

# this simulated data is available by loading it

url <- "https://github.com/csc-ubc-okanagan/Clustering-Classification/raw/refs/heads/main/AdvanceRSeriesCodeandSlides/WS8-simulatedMixedData.RDS"
mixedDf <- readRDS(gzcon(url(url)))
mixedForClust <- mixedDf %>% select(-trueCluster)

# METHOD 1: PAM (PARTITIONING AROUND MEDOIDS) WITH GOWER DISTANCE

# PAM is like K-means but uses medoids (actual data points) instead of centroids
# this is good for mixed-type data because we cant compute a "mean" 
# of categorical variables

# PAM works with a distance matrix, so we can use Gower distance
gowerDist <- daisy(mixedForClust, metric="gower")

# PAM with k=4 (we know true k)
pamResult4 <- pam(gowerDist, k=4, diss=TRUE)

# examine the medoids 
mixedForClust[pamResult4$medoids, ]
mixedDf$pamCluster <- factor(pamResult4$clustering)

accPam4 <- sum(diag(Thresher::matchLabels(table(mixedDf$pamCluster, mixedDf$trueCluster))))/nrow(mixedDf)
ariPam4 <- aricode::ARI(mixedDf$trueCluster, mixedDf$pamCluster)
nmiPam4 <- aricode::NMI(mixedDf$trueCluster, mixedDf$pamCluster)
accPam4; ariPam4; nmiPam4

# CHOOSING K FOR PAM: SILHOUETTE WIDTH
silWidths <- numeric(8)
for (k in 2:10) {
  pamK <- pam(gowerDist, k=k, diss=TRUE)
  silWidths[k-1] <- pamK$silinfo$avg.width
}

silDf <- data.frame(k=2:10, silhouette=silWidths)
pSilhouette <- ggplot(silDf, aes(x=k, y=silhouette)) +
  geom_line(color="steelblue", linewidth=1) +
  geom_point(color="steelblue", size=3) +
  geom_vline(xintercept=silDf$k[which.max(silDf$silhouette)], 
             linetype="dashed", color="red") +
  labs(title="Silhouette Width by k (PAM)", x="Number of Clusters (k)", 
       y="Average Silhouette Width") +
  theme(panel.grid=element_blank(), panel.border=element_rect(NA,"black",1))
pSilhouette

optimalK <- silDf$k[which.max(silDf$silhouette)]

# SILHOUETTE PLOT FOR k=4

silPam4 <- silhouette(pamResult4)
silDf4 <- data.frame(
  observation = 1:n,
  cluster = factor(silPam4[,1]),
  silWidth = silPam4[,3])
silDf4 <- silDf4 %>% arrange(cluster, desc(silWidth))
silDf4$order <- 1:n

pSilPlot <- ggplot(silDf4, aes(x=order, y=silWidth, fill=cluster)) +
  geom_bar(stat="identity", width=1) +
  geom_hline(yintercept=mean(silDf4$silWidth), linetype="dashed", color="black") +
  scale_fill_viridis_d(option="cividis") +
  coord_flip() +
  labs(title="Silhouette Plot (PAM, k=4)", x="Observations (sorted)", 
       y="Silhouette Width", fill="Cluster") +
  theme(panel.grid=element_blank(), panel.border=element_rect(NA,"black",1),
        axis.text.y=element_blank(), axis.ticks.y=element_blank())
pSilPlot

# use MDS to project Gower distances to 2D for visualization
mdsCoords <- cmdscale(gowerDist, k=2)
mixedDf$mds1 <- mdsCoords[,1]
mixedDf$mds2 <- mdsCoords[,2]

pPamMds <- ggplot(mixedDf, aes(x=mds1, y=mds2, color=pamCluster)) +
  geom_point(size=2, alpha=0.7) +
  scale_color_viridis_d(option="cividis") +
  labs(title="PAM Clustering", x="MDS Dim 1", y="MDS Dim 2",
       color="Cluster") +
  theme(panel.grid=element_blank(), panel.border=element_rect(NA,"black",1))

pTrueMds <- ggplot(mixedDf, aes(x=mds1, y=mds2, color=trueCluster)) +
  geom_point(size=2, alpha=0.7) +
  scale_color_viridis_d(option="cividis") +
  labs(title="True Clusters", x="MDS Dim 1", y="MDS Dim 2",
       color="Cluster") +
  theme(panel.grid=element_blank(), panel.border=element_rect(NA,"black",1))

ggpubr::ggarrange(pTrueMds, pPamMds,
                  ncol=2, nrow = 1, common.legend = T, legend = "bottom")


# METHOD 2 K-PROTOTYPES

# K-prototypes extends K-means to mixed data
# for numeric variables uses means (like K-means)
# for categorical variables uses modes (most frequent category)
# distance weighted sum of squared Euclidean (numeric) + simple matching (categorical)

# the clustMixType package provides kproto()
# it automatically detects numeric vs factor columns

kprotoResult <- kproto(mixedForClust, k=4, nstart=25, verbose=FALSE)
kprotoResult$centers

mixedDf$kprotoCluster <- factor(kprotoResult$cluster)

accKproto <- sum(diag(Thresher::matchLabels(table(mixedDf$kprotoCluster, mixedDf$trueCluster))))/nrow(mixedDf)
ariKproto <- aricode::ARI(mixedDf$trueCluster, mixedDf$kprotoCluster)
nmiKproto <- aricode::NMI(mixedDf$trueCluster, mixedDf$kprotoCluster)
accKproto; ariKproto; nmiKproto

# CHOOSING K FOR K-PROTOTYPES
# use within-cluster sum of distances (like WCSS for K-means)
wcssKproto <- numeric(9)
for (k in 2:10) {
  kpK <- kproto(mixedForClust, k=k, nstart=10, verbose=FALSE)
  wcssKproto[k-1] <- kpK$tot.withinss
}

wcssDf <- data.frame(k=2:10, wcss=wcssKproto)
pElbowKproto <- ggplot(wcssDf, aes(x=k, y=wcss)) +
  geom_line(color="steelblue", linewidth=1) +
  geom_point(color="steelblue", size=3) +
  labs(title="Elbow Plot (K-Prototypes)", x="Number of Clusters (k)", 
       y="Total Within-Cluster Distance") +
  theme(panel.grid=element_blank(), panel.border=element_rect(NA,"black",1))
pElbowKproto

# VISUALIZE K-PROTOTYPES RESULTS

pKprotoMds <- ggplot(mixedDf, aes(x=mds1, y=mds2, color=kprotoCluster)) +
  geom_point(size=2, alpha=0.7) +
  scale_color_viridis_d(option="cividis") +
  labs(title="K-Prototypes", x="MDS Dim 1", y="MDS Dim 2",
       color="Cluster") +
  theme(panel.grid=element_blank(), panel.border=element_rect(NA,"black",1))

ggpubr::ggarrange(pTrueMds, pPamMds, pKprotoMds, 
                  ncol=2, nrow = 2, common.legend = T, legend = "bottom")

# METHOD 3: HIERARCHICAL CLUSTERING WITH GOWER DISTANCE
# full circle back to Workshop 1

hclustComplete <- hclust(gowerDist, method="complete")
hclustAverage <- hclust(gowerDist, method="average")
hclustWard <- hclust(gowerDist, method="ward.D2")

par(mfrow=c(1,3))
plot(hclustComplete, labels=FALSE, main="Complete Linkage", xlab="", sub="")
plot(hclustAverage, labels=FALSE, main="Average Linkage", xlab="", sub="")
plot(hclustWard, labels=FALSE, main="Ward's Method", xlab="", sub="")
par(mfrow=c(1,1))

# CUT THE DENDROGRAM AT k=4
mixedDf$hclustComplete <- factor(cutree(hclustComplete, k=4))
mixedDf$hclustAverage <- factor(cutree(hclustAverage, k=4))
mixedDf$hclustWard <- factor(cutree(hclustWard, k=4))

ariComplete <- aricode::ARI(mixedDf$trueCluster, mixedDf$hclustComplete)
ariAverage <- aricode::ARI(mixedDf$trueCluster, mixedDf$hclustAverage)
ariWard <- aricode::ARI(mixedDf$trueCluster, mixedDf$hclustWard)

round(ariComplete,3); round(ariAverage,3); round(ariWard,3)

accComplete <- sum(diag(Thresher::matchLabels(table(mixedDf$trueCluster, mixedDf$hclustComplete))))/nrow(mixedDf)
accAverage <-  sum(diag(Thresher::matchLabels(table(mixedDf$trueCluster, mixedDf$hclustComplete))))/nrow(mixedDf)
accWard <- sum(diag(Thresher::matchLabels(table(mixedDf$trueCluster, mixedDf$hclustWard))))/nrow(mixedDf)
accComplete; accAverage; accWard

dend <- as.dendrogram(hclustWard)
dend <- color_branches(dend, k=4, col=viridis(4, option="plasma"))

plot(dend, main="Ward's Method", leaflab="none")

# VISUALIZE HIERARCHICAL RESULTS

pHclustMds <- ggplot(mixedDf, aes(x=mds1, y=mds2, color=hclustWard)) +
  geom_point(size=2, alpha=0.7) +
  scale_color_viridis_d(option="cividis") +
  labs(title="Hierarchical Ward", x="MDS Dim 1", y="MDS Dim 2",
       color="Cluster") +
  theme(panel.grid=element_blank(), panel.border=element_rect(NA,"black",1))

ggpubr::ggarrange(pTrueMds, pPamMds, pKprotoMds, pHclustMds,
                  ncol=2, nrow = 2, common.legend = T, legend = "bottom")



# METHOD 4 SPECTRAL CLUSTERING WITH MIXED-TYPE SIMILARITY

# spectral clustering works with a similarity matrix
# we convert Gower distance to similarity: s = 1 - d
# then apply the spectral clustering algorithm from Workshop 6

# Convert Gower distance to similarity
gowerMat <- as.matrix(gowerDist)
sigma <- 0.1
gowerSim <- exp(-gowerMat^2/(2*sigma^2))
diag(gaussianSim) <- 0

# Workshop 6 Materials
dVec <- rowSums(gowerSim)
dInvSqrt <- diag(1/sqrt(pmax(dVec, 1e-10)))
Lsym <- diag(n)-dInvSqrt %*% gowerSim %*% dInvSqrt
eigenResult <- eigen(Lsym, symmetric=TRUE)
eigenvalues <- rev(eigenResult$values)
eigenvectors <- eigenResult$vectors[, rev(seq_len(ncol(eigenResult$vectors)))]
k <- 4
U <- eigenvectors[, 1:k]
rowNorms <- sqrt(rowSums(U^2))
rowNorms[rowNorms==0] <- 1
Tnorm <- U/rowNorms
spectralKmeans <- kmeans(Tnorm, centers=k, nstart=50)
mixedDf$spectralCluster <- factor(spectralKmeans$cluster)


accSpectral <- sum(diag(Thresher::matchLabels(table(mixedDf$trueCluster, mixedDf$spectralCluster))))/nrow(mixedDf)
ariSpectral <- aricode::ARI(mixedDf$trueCluster, mixedDf$spectralCluster)
nmiSpectral <- aricode::NMI(mixedDf$trueCluster, mixedDf$spectralCluster)
accSpectral; ariSpectral; nmiSpectral

# EFFECT OF SIGMA ON GAUSSIAN SIMILARITY

sigmaVals <- seq(0.1,0.5,0.02)
sigmaResults <- data.frame(sigma=sigmaVals, ACC=NA, ARI=NA, NMI=NA)

for (i in seq_along(sigmaVals)) {
  simSigma <- exp(-gowerMat^2/(2*sigmaVals[i]^2))
  diag(simSigma) <- 0
  dVecSigma <- rowSums(simSigma)
  dInvSqrtSigma <- diag(1/sqrt(pmax(dVecSigma, 1e-10)))
  LsymSigma <- diag(n)-dInvSqrtSigma %*% simSigma %*% dInvSqrtSigma
  eigenSigma <- eigen(LsymSigma, symmetric=TRUE)
  eigvecsSigma <- eigenSigma$vectors[, rev(seq_len(ncol(eigenSigma$vectors)))]
  Usigma <- eigvecsSigma[, 1:k]
  rnSigma <- sqrt(rowSums(Usigma^2)); rnSigma[rnSigma==0] <- 1
  Tsigma <- Usigma/rnSigma
  kmSigma <- kmeans(Tsigma, centers=k, nstart=25)
  sigmaResults$ACC[i] <- sum(diag(Thresher::matchLabels(table(kmSigma$cluster, mixedDf$trueCluster))))/nrow(mixedDf)
  sigmaResults$ARI[i] <- aricode::ARI(mixedDf$trueCluster, kmSigma$cluster)
  sigmaResults$NMI[i] <- aricode::NMI(mixedDf$trueCluster, kmSigma$cluster)
}

pSigma <- sigmaResults %>%
  pivot_longer(cols=c(ACC,ARI,NMI), names_to="metric", values_to="value") %>%
  ggplot(aes(x=sigma, y=value, color=metric)) +
  geom_line(linewidth=1) +
  geom_point(size=3) +
  scale_color_manual(values=c("ARI"="steelblue","NMI"="darkorange", "ACC" = "brown")) +
  labs(title="Spectral Clustering: Effect of Sigma", x="Sigma", 
       y="Score", color="Metric") +
  theme(panel.grid=element_blank(), panel.border=element_rect(NA,"black",1))
pSigma

# maximum accuracy occurs at
max(sigmaResults$ACC)
# which is when sigma is
sigmaVals[which.max(sigmaResults$ACC)]

# VISUALIZE SPECTRAL RESULTS

pSpectralMds <- ggplot(mixedDf, aes(x=mds1, y=mds2, color=spectralCluster)) +
  geom_point(size=2, alpha=0.7) +
  scale_color_viridis_d(option="cividis") +
  labs(title="Spectral", x="MDS Dim 1", y="MDS Dim 2",
       color="Cluster") +
  theme(panel.grid=element_blank(), panel.border=element_rect(NA,"black",1))

ggpubr::ggarrange(pTrueMds, pPamMds, pKprotoMds, pHclustMds, pSpectralMds, 
                  ncol=3, nrow = 2, common.legend = T, legend = "bottom")

# METHOD COMPARISON

resultsAll <- data.frame(
  Method = c("PAM (Gower)", "K-Prototypes", "Hierarchical (Ward)", "Spectral (Gower)"),
  ACC  =c(accPam4, accKproto, accWard, accSpectral),
  ARI = c(ariPam4, ariKproto, ariWard, ariSpectral),
  NMI = c(nmiPam4, nmiKproto, 
          aricode::NMI(mixedDf$trueCluster, mixedDf$hclustWard), nmiSpectral))
pMethodCompare <- resultsAll %>%
  pivot_longer(cols=c(ACC,ARI,NMI), names_to="metric", values_to="value") %>%
  ggplot(aes(x=reorder(Method,value), y=value, fill=metric)) +
  geom_bar(stat="identity", position="dodge", alpha=0.8) +
  scale_fill_manual(values=c("ACC" = "brown", "ARI"="steelblue","NMI"="darkorange")) +
  coord_flip() +
  labs(title="Method Comparison", x="Method", y="Score", fill="Metric") +
  theme(panel.grid=element_blank(), panel.border=element_rect(NA,"black",1))
pMethodCompare

# AGREEMENT BETWEEN METHODS

# how much do the methods agree with each other?
methodClusters <- data.frame(
  PAM = mixedDf$pamCluster,
  KPrototypes = mixedDf$kprotoCluster,
  Hierarchical = mixedDf$hclustWard,
  Spectral = mixedDf$spectralCluster)

agreementMat <- matrix(NA, 4, 4)
colnames(agreementMat) <- rownames(agreementMat) <- names(methodClusters)
for (i in 1:4) {
  for (j in 1:4) {
    agreementMat[i,j] <- sum(diag(Thresher::matchLabels(table(methodClusters[,i], methodClusters[,j]))))/nrow(mixedDf)
  }
}

agreementDf <- expand.grid(method1=names(methodClusters), method2=names(methodClusters))
agreementDf$ACC <- as.vector(agreementMat)

pAgreement <- ggplot(agreementDf, aes(x=method1, y=method2, fill=ACC)) +
  geom_tile() +
  geom_text(aes(label=round(ACC,2)), color="black", size=4) +
  scale_fill_viridis(option="magma", limits=c(0,1)) +
  labs(title="Agreement Between Methods (ACC)", x="", y="") +
  theme(panel.grid=element_blank(), panel.border=element_rect(NA,"black",1),
        axis.text.x=element_text(angle=45, hjust=1))
pAgreement

# CLUSTER PROFILES

# characterize clusters by variable distributions
# using Spectral clustering as the example

clusterProfile <- mixedDf %>%
  group_by(spectralCluster) %>%
  summarise(
    n = n(),
    meanAge = round(mean(age),1),
    meanIncome = round(mean(income),0),
    meanExp = round(mean(yearsExperience),1),
    pctMastersPhD = round(100*mean(education %in% c("Masters","PhD")),1),
    pctEmployed = round(100*mean(employed),1),
    topSector = names(sort(table(sector), decreasing=TRUE))[1],
    .groups="drop")

print(as.data.frame(clusterProfile))


# REAL DATA APPLICATION
# the UCI Heart Disease dataset is a common mixed-type clustering data
# goal is to identify patient subgroups based on clinical and demographic features
# we cluster without using the diagnosis label
# then examine whether clusters correspond to meaningful patient profiles

heartUrl <- "https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data"
heartRaw <- read.csv(heartUrl, header=FALSE, na.strings="?")
colnames(heartRaw) <- c("age","sex","cp","trestbps","chol","fbs","restecg",
                        "thalach","exang","oldpeak","slope","ca","thal","num")
head(heartRaw)
heartDf <- heartRaw[complete.cases(heartRaw), ]


# convert variables to appropriate types
# continuous: age, trestbps, chol, thalach, oldpeak
# binary: sex, fbs, exang
# nominal/ordinal: cp, restecg, slope, ca, thal

heartClean <- heartDf %>%
  mutate(
    sex = factor(sex, levels=c(0,1), labels=c("Female","Male")),
    cp = factor(cp, levels=c(1,2,3,4), 
                labels=c("TypicalAngina","AtypicalAngina","NonAnginal","Asymptomatic")),
    fbs = factor(fbs, levels=c(0,1), labels=c("Normal","High")),
    restecg = factor(restecg, levels=c(0,1,2), 
                     labels=c("Normal","STAbnormal","LVH")),
    exang = factor(exang, levels=c(0,1), labels=c("No","Yes")),
    slope = factor(slope, levels=c(1,2,3), 
                   labels=c("Upsloping","Flat","Downsloping"), ordered=TRUE),
    ca = factor(ca, levels=c(0,1,2,3), ordered=TRUE),
    thal = factor(thal, levels=c(3,6,7), 
                  labels=c("Normal","FixedDefect","ReversibleDefect")),
    diagnosis = factor(ifelse(num>0, "Disease", "NoDisease")))

heartForClust <- heartClean %>% select(-num, -diagnosis)


# EXPLORATORY DATA ANALYSIS
# Summaries for cont. variables
continuousVars <- c("age","trestbps","chol","thalach","oldpeak")
continuousSummary <- heartClean %>%
  summarise(across(all_of(continuousVars), 
                   list(mean=~round(mean(.),1), sd=~round(sd(.),1), 
                        min=~min(.), max=~max(.)), .names="{.col}_{.fn}")) %>%
  pivot_longer(everything(), names_to=c("variable","stat"), names_sep="_") %>%
  pivot_wider(names_from=stat, values_from=value)

print(as.data.frame(continuousSummary))

# Distributions of cont. variables
pAgeHist <- ggplot(heartClean, aes(x=age)) +
  geom_histogram(bins=20, fill="steelblue", color="white", alpha=0.7) +
  labs(title="Age", x="Age (years)", y="Count") +
  theme(panel.grid=element_blank(), panel.border=element_rect(NA,"black",1))

pBpHist <- ggplot(heartClean, aes(x=trestbps)) +
  geom_histogram(bins=20, fill="steelblue", color="white", alpha=0.7) +
  labs(title="Resting Blood Pressure", x="BP (mmHg)", y="Count") +
  theme(panel.grid=element_blank(), panel.border=element_rect(NA,"black",1))

pCholHist <- ggplot(heartClean, aes(x=chol)) +
  geom_histogram(bins=20, fill="steelblue", color="white", alpha=0.7) +
  labs(title="Cholesterol", x="Cholesterol (mg/dl)", y="Count") +
  theme(panel.grid=element_blank(), panel.border=element_rect(NA,"black",1))

pMaxHrHist <- ggplot(heartClean, aes(x=thalach)) +
  geom_histogram(bins=20, fill="steelblue", color="white", alpha=0.7) +
  labs(title="Max Heart Rate", x="Max HR (bpm)", y="Count") +
  theme(panel.grid=element_blank(), panel.border=element_rect(NA,"black",1))

pOldpeakHist <- ggplot(heartClean, aes(x=oldpeak)) +
  geom_histogram(bins=20, fill="steelblue", color="white", alpha=0.7) +
  labs(title="ST Depression", x="Oldpeak", y="Count") +
  theme(panel.grid=element_blank(), panel.border=element_rect(NA,"black",1))

grid.arrange(pAgeHist, pBpHist, pCholHist, pMaxHrHist, pOldpeakHist, ncol=3)


# Categorical variables frequencies

pSexBar <- ggplot(heartClean, aes(x=sex, fill=sex)) +
  geom_bar(alpha=0.8) +
  scale_fill_viridis_d(option="cividis") +
  labs(title="Sex", x="", y="Count") +
  theme(legend.position="none", panel.grid=element_blank(),
        panel.border=element_rect(NA,"black",1))

pCpBar <- ggplot(heartClean, aes(x=cp, fill=cp)) +
  geom_bar(alpha=0.8) +
  scale_fill_viridis_d(option="cividis") +
  labs(title="Chest Pain Type", x="", y="Count") +
  theme(legend.position="none", panel.grid=element_blank(),
        panel.border=element_rect(NA,"black",1),
        axis.text.x=element_text(angle=45, hjust=1))

pThalBar <- ggplot(heartClean, aes(x=thal, fill=thal)) +
  geom_bar(alpha=0.8) +
  scale_fill_viridis_d(option="cividis") +
  labs(title="Thalassemia", x="", y="Count") +
  theme(legend.position="none", panel.grid=element_blank(),
        panel.border=element_rect(NA,"black",1),
        axis.text.x=element_text(angle=45, hjust=1))

pDiagBar <- ggplot(heartClean, aes(x=diagnosis, fill=diagnosis)) +
  geom_bar(alpha=0.8) +
  scale_fill_manual(values=c("NoDisease"="steelblue","Disease"="firebrick")) +
  labs(title="Diagnosis", x="", y="Count") +
  theme(legend.position="none", panel.grid=element_blank(),
        panel.border=element_rect(NA,"black",1))

plot_grid(pSexBar, pCpBar, pThalBar, pDiagBar, ncol = 2, align = "hv")


# Continuous Correlattions

corMatrix <- cor(heartClean[, continuousVars])
corDf <- expand.grid(var1=continuousVars, var2=continuousVars)
corDf$cor <- as.vector(corMatrix)

pCorHeat <- ggplot(corDf, aes(x=var1, y=var2, fill=cor)) +
  geom_tile() +
  geom_text(aes(label=round(cor,2)), color="black", size=3) +
  scale_fill_gradient2(low="steelblue", mid="white", high="firebrick", 
                       midpoint=0, limits=c(-1,1)) +
  labs(title="Correlation of Continuous Variables", x="", y="") +
  theme(panel.grid=element_blank(), panel.border=element_rect(NA,"black",1),
        axis.text.x=element_text(angle=45, hjust=1))
pCorHeat

# continuous variables by diagnosis
pAgeDiag <- ggplot(heartClean, aes(x=diagnosis, y=age, fill=diagnosis)) +
  geom_boxplot(alpha=0.7) +
  scale_fill_manual(values=c("NoDisease"="steelblue","Disease"="firebrick")) +
  labs(title="Age", x="", y="Age (years)") +
  theme(legend.position="none", panel.grid=element_blank(),
        panel.border=element_rect(NA,"black",1))

pCholDiag <- ggplot(heartClean, aes(x=diagnosis, y=chol, fill=diagnosis)) +
  geom_boxplot(alpha=0.7) +
  scale_fill_manual(values=c("NoDisease"="steelblue","Disease"="firebrick")) +
  labs(title="Cholesterol", x="", y="mg/dl") +
  theme(legend.position="none", panel.grid=element_blank(),
        panel.border=element_rect(NA,"black",1))

pThalDiag <- ggplot(heartClean, aes(x=diagnosis, y=thalach, fill=diagnosis)) +
  geom_boxplot(alpha=0.7) +
  scale_fill_manual(values=c("NoDisease"="steelblue","Disease"="firebrick")) +
  labs(title="Max Heart Rate", x="", y="bpm") +
  theme(legend.position="none", panel.grid=element_blank(),
        panel.border=element_rect(NA,"black",1))

pOldpeakDiag <- ggplot(heartClean, aes(x=diagnosis, y=oldpeak, fill=diagnosis)) +
  geom_boxplot(alpha=0.7) +
  scale_fill_manual(values=c("NoDisease"="steelblue","Disease"="firebrick")) +
  labs(title="ST Depression", x="", y="Oldpeak") +
  theme(legend.position="none", panel.grid=element_blank(),
        panel.border=element_rect(NA,"black",1))

grid.arrange(pAgeDiag, pCholDiag, pThalDiag, pOldpeakDiag, ncol=2)


# categorical variables vs diagnosis
pCpDiag <- ggplot(heartClean, aes(x=cp, fill=diagnosis)) +
  geom_bar(position="fill", alpha=0.8) +
  scale_fill_manual(values=c("NoDisease"="steelblue","Disease"="firebrick")) +
  labs(title="Chest Pain Type", x="", y="Proportion") +
  theme(legend.position="top", panel.grid=element_blank(),
        panel.border=element_rect(NA,"black",1),
        axis.text.x=element_text(angle=45, hjust=1))

pSexDiag <- ggplot(heartClean, aes(x=sex, fill=diagnosis)) +
  geom_bar(position="fill", alpha=0.8) +
  scale_fill_manual(values=c("NoDisease"="steelblue","Disease"="firebrick")) +
  labs(title="Sex", x="", y="Proportion") +
  theme(legend.position="none", panel.grid=element_blank(),
        panel.border=element_rect(NA,"black",1))

pExangDiag <- ggplot(heartClean, aes(x=exang, fill=diagnosis)) +
  geom_bar(position="fill", alpha=0.8) +
  scale_fill_manual(values=c("NoDisease"="steelblue","Disease"="firebrick")) +
  labs(title="Exercise Angina", x="", y="Proportion") +
  theme(legend.position="none", panel.grid=element_blank(),
        panel.border=element_rect(NA,"black",1))

pThalDiag2 <- ggplot(heartClean, aes(x=thal, fill=diagnosis)) +
  geom_bar(position="fill", alpha=0.8) +
  scale_fill_manual(values=c("NoDisease"="steelblue","Disease"="firebrick")) +
  labs(title="Thalassemia", x="", y="Proportion") +
  theme(legend.position="none", panel.grid=element_blank(),
        panel.border=element_rect(NA,"black",1),
        axis.text.x=element_text(angle=45, hjust=1))

plot_grid(pCpDiag, pSexDiag, pExangDiag, pThalDiag2, ncol=4, align = "hv")

pAgeMaxHR <- ggplot(heartClean, aes(x=age, y=thalach, color=diagnosis)) +
  geom_point(alpha=0.6, size=2) +
  geom_smooth(method="lm", se=FALSE, linetype="dashed") +
  scale_color_manual(values=c("NoDisease"="steelblue","Disease"="firebrick")) +
  labs(title="Age vs Max Heart Rate", x="Age", y="Max HR (bpm)") +
  theme(panel.grid=element_blank(), panel.border=element_rect(NA,"black",1))

pAgeOldpeak <- ggplot(heartClean, aes(x=age, y=oldpeak, color=diagnosis)) +
  geom_point(alpha=0.6, size=2) +
  scale_color_manual(values=c("NoDisease"="steelblue","Disease"="firebrick")) +
  labs(title="Age vs ST Depression", x="Age", y="Oldpeak") +
  theme(panel.grid=element_blank(), panel.border=element_rect(NA,"black",1))

pMaxHROldpeak <- ggplot(heartClean, aes(x=thalach, y=oldpeak, color=diagnosis)) +
  geom_point(alpha=0.6, size=2) +
  scale_color_manual(values=c("NoDisease"="steelblue","Disease"="firebrick")) +
  labs(title="Max HR vs ST Depression", x="Max HR (bpm)", y="Oldpeak") +
  theme(panel.grid=element_blank(), panel.border=element_rect(NA,"black",1))

plot_grid(pAgeMaxHR, pAgeOldpeak, pMaxHROldpeak, ncol=2, align = "hv")


# CLUSTERING ANALYSIS

gowerHeart <- daisy(heartForClust, metric="gower")
mdsHeart <- cmdscale(gowerHeart, k=2)
heartClean$mds1 <- mdsHeart[,1]
heartClean$mds2 <- mdsHeart[,2]

pMdsDiagnosis <- ggplot(heartClean, aes(x=mds1, y=mds2, color=diagnosis)) +
  geom_point(size=2, alpha=0.7) +
  scale_color_manual(values=c("NoDisease"="steelblue","Disease"="firebrick")) +
  labs(title="MDS of Gower Distance", 
       x="MDS Dim 1", y="MDS Dim 2", color="Diagnosis") +
  theme(panel.grid=element_blank(), 
        legend.position = "bottom",
        panel.border=element_rect(NA,"black",1))
pMdsDiagnosis


# CHOOSING K

silWidthsHeart <- numeric(8)
for (k in 2:9) {
  pamK <- pam(gowerHeart, k=k, diss=TRUE)
  silWidthsHeart[k-1] <- pamK$silinfo$avg.width
}

silDfHeart <- data.frame(k=2:9, silhouette=silWidthsHeart)
pSilHeart <- ggplot(silDfHeart, aes(x=k, y=silhouette)) +
  geom_line(color="steelblue", linewidth=1) +
  geom_point(color="steelblue", size=3) +
  geom_vline(xintercept=silDfHeart$k[which.max(silDfHeart$silhouette)], 
             linetype="dashed", color="red") +
  labs(title="Silhouette Width (Heart Disease)", x="Number of Clusters (k)", 
       y="Average Silhouette Width") +
  theme(panel.grid=element_blank(), 
        legend.position = "bottom",
        panel.border=element_rect(NA,"black",1))
pSilHeart

# PAM CLUSTERING

pamHeart <- pam(gowerHeart, k=optimalKHeart, diss=TRUE)
heartClean$pamCluster <- factor(pamHeart$clustering)

print(heartForClust[pamHeart$medoids, ])

pPamHeart <- ggplot(heartClean, aes(x=mds1, y=mds2, color=pamCluster)) +
  geom_point(size=2, alpha=0.7) +
  scale_color_viridis_d(option="cividis") +
  labs(title=paste0("PAM Clustering"), 
       x="MDS Dim 1", y="MDS Dim 2", color="Cluster") +
  theme(panel.grid=element_blank(), 
        legend.position = "bottom",
        panel.border=element_rect(NA,"black",1))

grid.arrange(pMdsDiagnosis, pPamHeart, ncol=2)


# K-PROTOTYPES CLUSTERING

kprotoHeart <- kproto(heartForClust, k=optimalKHeart, nstart=25, verbose=FALSE)
heartClean$kprotoCluster <- factor(kprotoHeart$cluster)

pKprotoHeart <- ggplot(heartClean, aes(x=mds1, y=mds2, color=kprotoCluster)) +
  geom_point(size=2, alpha=0.7) +
  scale_color_viridis_d(option="cividis") +
  labs(title=paste0("K-Prototypes"), 
       x="MDS Dim 1", y="MDS Dim 2", color="Cluster") +
  theme(panel.grid=element_blank(), panel.border=element_rect(NA,"black",1))

# HIERARCHICAL CLUSTERING

hclustHeart <- hclust(gowerHeart, method="ward.D2")
heartClean$hclustCluster <- factor(cutree(hclustHeart, k=optimalKHeart))

pHclustHeart <- ggplot(heartClean, aes(x=mds1, y=mds2, color=hclustCluster)) +
  geom_point(size=2, alpha=0.7) +
  scale_color_viridis_d(option="cividis") +
  labs(title=paste0("Hierarchical Ward"), 
       x="MDS Dim 1", y="MDS Dim 2", color="Cluster") +
  theme(panel.grid=element_blank(), panel.border=element_rect(NA,"black",1))

# SPECTRAL CLUSTERING

gowerMatHeart <- as.matrix(gowerHeart)
gowerSimHeart <- exp(-1/2*(gowerMatHeart^2/sigma^2))
diag(gowerSimHeart) <- 0
dVecHeart <- rowSums(gowerSimHeart)
dInvSqrtHeart <- diag(1/sqrt(pmax(dVecHeart, 1e-10)))
LsymHeart <- diag(nrow(gowerMatHeart))-dInvSqrtHeart %*% gowerSimHeart %*% dInvSqrtHeart
eigenHeart <- eigen(LsymHeart, symmetric=TRUE)
eigvecsHeart <- eigenHeart$vectors[, rev(seq_len(ncol(eigenHeart$vectors)))]
Uheart <- eigvecsHeart[, 1:optimalKHeart]
rnHeart <- sqrt(rowSums(Uheart^2)); rnHeart[rnHeart==0] <- 1
Theart <- Uheart/rnHeart
spectralHeart <- kmeans(Theart, centers=optimalKHeart, nstart=50)
heartClean$spectralCluster <- factor(spectralHeart$cluster)

pSpectralHeart <- ggplot(heartClean, aes(x=mds1, y=mds2, color=spectralCluster)) +
  geom_point(size=2, alpha=0.7) +
  scale_color_viridis_d(option="cividis") +
  labs(title=paste0("Spectral"), 
       x="MDS Dim 1", y="MDS Dim 2", color="Cluster") +
  theme(panel.grid=element_blank(), panel.border=element_rect(NA,"black",1))


grid.arrange(
  pPamHeart+theme(legend.position="none"),
  pKprotoHeart+theme(legend.position="none"),
  pHclustHeart+theme(legend.position="none"),
  pSpectralHeart+theme(legend.position="none"),
  ncol=2)


# CLUSTER INTERPRETATION AND PROFILING
# by Spectral Clusters

sum(diag(Thresher::matchLabels(table(heartClean$diagnosis, heartClean$pamCluster))))/297
sum(diag(Thresher::matchLabels(table(heartClean$diagnosis, heartClean$kprotoCluster))))/297
sum(diag(Thresher::matchLabels(table(heartClean$diagnosis, heartClean$hclustCluster))))/297
sum(diag(Thresher::matchLabels(table(heartClean$diagnosis, heartClean$spectralCluster))))/297

print(table(heartClean$spectralCluster, heartClean$diagnosis))
print(round(prop.table(table(heartClean$spectralCluster, heartClean$diagnosis), margin=1)*100, 1))

pClusterDiag <- heartClean %>%
  group_by(spectralCluster, diagnosis) %>%
  summarise(n=n(), .groups="drop") %>%
  ggplot(aes(x=spectralCluster, y=n, fill=diagnosis)) +
  geom_bar(stat="identity", position="fill", alpha=0.8) +
  geom_text(aes(label=n), position=position_fill(vjust=0.5), color="white", size=3) +
  scale_fill_manual(values=c("NoDisease"="steelblue","Disease"="firebrick")) +
  labs(title="Spectral Clusters vs Diagnosis", x="Cluster", y="Proportion", fill="Diagnosis") +
  theme(panel.grid=element_blank(), panel.border=element_rect(NA,"black",1))
pClusterDiag

clusterProfileHeart <- heartClean %>%
  group_by(spectralCluster) %>%
  summarise(
    n = n(),
    pctOfTotal = round(100*n()/nrow(heartClean), 1),
    # continuous variables
    meanAge = round(mean(age), 1),
    sdAge = round(sd(age), 1),
    meanBP = round(mean(trestbps), 1),
    meanChol = round(mean(chol), 0),
    meanMaxHR = round(mean(thalach), 0),
    meanOldpeak = round(mean(oldpeak), 2),
    # categorical summaries
    pctMale = round(100*mean(sex=="Male"), 1),
    pctExangYes = round(100*mean(exang=="Yes"), 1),
    pctHighFBS = round(100*mean(fbs=="High"), 1),
    # outcome
    pctDisease = round(100*mean(diagnosis=="Disease"), 1),
    .groups="drop")

print(as.data.frame(clusterProfileHeart))


# categorical variables by cluster

# Chest Pain Type by Cluster
print(round(prop.table(table(heartClean$spectralCluster, heartClean$cp), margin=1)*100, 1))
# Thalassemia by Cluster
print(round(prop.table(table(heartClean$spectralCluster, heartClean$thal), margin=1)*100, 1))
# Slope by Cluster 
print(round(prop.table(table(heartClean$spectralCluster, heartClean$slope), margin=1)*100, 1))
# Number of Vessels (ca) by Cluster 
print(round(prop.table(table(heartClean$spectralCluster, heartClean$ca), margin=1)*100, 1))

heartClean <- heartClean %>%
  mutate(
    spectralCluster = recode(as.character(spectralCluster),
                             "1" = "2",
                             "2" = "1"),
    spectralCluster = factor(spectralCluster, levels = c("1", "2"))
  )

# continuous variables by cluster
pAgeClust <- ggplot(heartClean, aes(x=spectralCluster, y=age, fill=spectralCluster)) +
  geom_boxplot(alpha=0.7) +
  scale_fill_viridis_d(option="cividis") +
  labs(title="Age by Cluster", x="Cluster", y="Age") +
  theme(legend.position="none", panel.grid=element_blank(),
        panel.border=element_rect(NA,"black",1))

pMaxHRClust <- ggplot(heartClean, aes(x=spectralCluster, y=thalach, fill=spectralCluster)) +
  geom_boxplot(alpha=0.7) +
  scale_fill_viridis_d(option="cividis") +
  labs(title="Max HR by Cluster", x="Cluster", y="Max HR (bpm)") +
  theme(legend.position="none", panel.grid=element_blank(),
        panel.border=element_rect(NA,"black",1))

pOldpeakClust <- ggplot(heartClean, aes(x=spectralCluster, y=oldpeak, fill=spectralCluster)) +
  geom_boxplot(alpha=0.7) +
  scale_fill_viridis_d(option="cividis") +
  labs(title="Oldpeak by Cluster", x="Cluster", y="Oldpeak") +
  theme(legend.position="none", panel.grid=element_blank(),
        panel.border=element_rect(NA,"black",1))

pCholClust <- ggplot(heartClean, aes(x=spectralCluster, y=chol, fill=spectralCluster)) +
  geom_boxplot(alpha=0.7) +
  scale_fill_viridis_d(option="cividis") +
  labs(title="Cholesterol by Cluster", x="Cluster", y="Cholesterol") +
  theme(legend.position="none", panel.grid=element_blank(),
        panel.border=element_rect(NA,"black",1))

grid.arrange(pAgeClust, pMaxHRClust, pOldpeakClust, pCholClust, ncol=2)

# categorical variables by cluster
pCpClust <- ggplot(heartClean, aes(x=spectralCluster, fill=cp)) +
  geom_bar(position="fill", alpha=0.8) +
  scale_fill_viridis_d(option="plasma") +
  labs(title="Chest Pain by Cluster", x="Cluster", y="Proportion", fill="CP Type") +
  theme(panel.grid=element_blank(), panel.border=element_rect(NA,"black",1),
        legend.position = "bottom")

pThalClust <- ggplot(heartClean, aes(x=spectralCluster, fill=thal)) +
  geom_bar(position="fill", alpha=0.8) +
  scale_fill_viridis_d(option="plasma") +
  labs(title="Thalassemia by Cluster", x="Cluster", y="Proportion", fill="Thal") +
  theme(panel.grid=element_blank(), panel.border=element_rect(NA,"black",1),
        legend.position = "bottom")

pSexClust <- ggplot(heartClean, aes(x=spectralCluster, fill=sex)) +
  geom_bar(position="fill", alpha=0.8) +
  scale_fill_viridis_d(option="cividis") +
  labs(title="Sex by Cluster", x="Cluster", y="Proportion", fill="Sex") +
  theme(panel.grid=element_blank(), panel.border=element_rect(NA,"black",1),
        legend.position = "bottom")

pExangClust <- ggplot(heartClean, aes(x=spectralCluster, fill=exang)) +
  geom_bar(position="fill", alpha=0.8) +
  scale_fill_viridis_d(option="magma") +
  labs(title="Exercise Angina by Cluster", x="Cluster", y="Proportion", fill="Exang") +
  theme(panel.grid=element_blank(), panel.border=element_rect(NA,"black",1),
        legend.position = "bottom")
grid.arrange(pCpClust, pThalClust, pSexClust, pExangClust, ncol=2)

profileNorm <- heartClean %>%
  group_by(spectralCluster) %>%
  summarise(
    Age = (mean(age)-min(heartClean$age))/(max(heartClean$age)-min(heartClean$age)),
    MaxHR = (mean(thalach)-min(heartClean$thalach))/(max(heartClean$thalach)-min(heartClean$thalach)),
    Oldpeak = (mean(oldpeak)-min(heartClean$oldpeak))/(max(heartClean$oldpeak)-min(heartClean$oldpeak)),
    Chol = (mean(chol)-min(heartClean$chol))/(max(heartClean$chol)-min(heartClean$chol)),
    BP = (mean(trestbps)-min(heartClean$trestbps))/(max(heartClean$trestbps)-min(heartClean$trestbps)),
    PctMale = mean(sex=="Male"),
    PctExang = mean(exang=="Yes"),
    PctDisease = mean(diagnosis=="Disease"),
    .groups="drop")

profileLong <- profileNorm %>%
  pivot_longer(-spectralCluster, names_to="variable", values_to="value")

pProfileBar <- ggplot(profileLong, aes(x=variable, y=value, fill=spectralCluster)) +
  geom_bar(stat="identity", position="dodge", alpha=0.8) +
  scale_fill_viridis_d(option="cividis") +
  labs(title="Normalized Cluster Profiles", x="Variable", y="Normalized Value (0-1)",
       fill="Cluster") +
  theme(panel.grid=element_blank(), panel.border=element_rect(NA,"black",1),
        axis.text.x=element_text(angle=45, hjust=1))
pProfileBar





