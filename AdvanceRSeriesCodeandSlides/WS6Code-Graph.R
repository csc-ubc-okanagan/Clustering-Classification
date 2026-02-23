# GRAPH-BASED CLUSTERING
# Workshop 6: Spectral Clustering and Modularity-Based Community Detection
library(igraph)
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
library(viridis)
library(cluster)
library(Matrix)
set.seed(2026)
theme_set(theme_minimal(base_size = 12))

# why use graph-based clustering?
# in density-based clustering we relied on local density to define clusters
# but some data is naturally relational -- nodes and edges, not coordinates
# graph-based clustering defines clusters based on graph topology:
# nodes that are tightly connected internally and loosely connected externally
# this is perfect for network data (infrastructure, social, biological networks)

# LIMITATIONS OF KMEANS ON GRAPH-STRUCTURED DATA
# -----------------------------------------------------------------------------
# generate three interlocking spirals -- k-means will fail 
# because the clusters are non-convex weave within one another

nSpiral <- 400
makeSpiral <- function(n, startAngle, noise = 0.03) {
  t <- seq(0, 2*pi, length.out = n)
  r <- 0.2+t/(2*pi)
  data.frame(x = r*cos(t+startAngle)+rnorm(n, 0, noise),
    y = r*sin(t+startAngle)+rnorm(n, 0, noise))
}

spiral1 <- makeSpiral(nSpiral, 0)
spiral2 <- makeSpiral(nSpiral, 2*pi/3)
spiral3 <- makeSpiral(nSpiral, 4*pi/3)

spiralDf <- rbind(data.frame(spiral1, trueCluster = factor(1)),
  data.frame(spiral2, trueCluster = factor(2)),
  data.frame(spiral3, trueCluster = factor(3)))

pTrueSpirals <- ggplot(spiralDf, aes(x = x, y = y, color = trueCluster)) +
  geom_point(size = 1.8) +
  scale_color_viridis_d(option = "cividis") +
  coord_fixed() +
  labs(x = "X1", y = "X2", color = "Cluster") +
  theme(legend.position = "bottom",
        panel.grid = element_blank(),
        panel.border = element_rect(NA, "black", 1))
pTrueSpirals

kmeansSpirals <- kmeans(spiralDf[, c("x", "y")], centers = 3, nstart = 25)
spiralDf$kmeansCluster <- factor(kmeansSpirals$cluster)

pKmeansSpirals <- ggplot(spiralDf, aes(x = x, y = y, color = kmeansCluster)) +
  geom_point(size = 1.8, alpha = 0.8) +
  scale_color_viridis_d(option = "cividis") +
  coord_fixed() +
  labs(title = "K-Means", x = "X1", y = "X2", color = "Cluster") +
  theme(legend.position = "bottom",
        panel.grid = element_blank(),
        panel.border = element_rect(NA, "black", 1))

grid.arrange(pTrueSpirals+ggtitle("True Labels"), pKmeansSpirals, ncol = 2)


# OVERVIEW OF GRAPH-BASED CLUSTERING
# -----------------------------------------------------------------------------

# a graph G = (V, E) consists of
# V->a set of nodes (the observations)
# E->a set of edges that are the pairwise similarity between two nodes

# we construct a weighted similarity graph from data
# W[i,j] = similarity between nodes i and j (Gaussian kernel for our case)
# W[i,j] = 0 means no edge between i and j

# GRAPH CONSTRUCTION POSSIBILITIES
# eps-neighbourhood -> connect i,j if dist(i,j) < eps
# k-NN graph -> connect i to its k nearest neighbours
# fully-connected -> do not trim graph

# GRAPH LAPLACIAN
# D = diagonal degree matrix, D[i,i] = sum_j W[i,j]
# unnormalised Laplacian: L=D-W
# normalised Laplacian: Lsym=D^{-1/2}LD^{-1/2}

# STEP 1--BUILD THE SIMILARITY MATRIX
# -----------------------------------------------------------------------------
spiralMatrix <- as.matrix(spiralDf[, c("x", "y")])
nPts <- nrow(spiralMatrix)
distMatrix <- as.matrix(dist(spiralMatrix))

# gaussian kernel: W[i,j] = exp(-d(i,j)^2/(2*sigma^2))
# we will arbitrarily set sigma, note we are not claiming
# this is the best choice (a whole other conversation!)
sigma <- 0.5
W <- exp(-distMatrix^2/(2*sigma^2))
diag(W) <- 0 

WsubFull <- W
WsubDf <- as.data.frame(WsubFull)
WsubDf$row <- seq_len(nrow(WsubDf))
WsubLong <- pivot_longer(WsubDf, cols = -row, names_to = "col", values_to = "sim")
WsubLong$col <- as.integer(sub("V", "", WsubLong$col))

# visualize pairwise similarities
pSimMatrix <- ggplot(WsubLong, aes(x = col, y = row, fill = sim)) +
  geom_raster() +
  scale_fill_gradient(low = "white", high = "steelblue", name = "Similarity") +
  coord_fixed() +
  labs(x = "Point index", y = "Point index") +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(NA, "black", 1),
        axis.text = element_text(size = 8))
pSimMatrix

# STEP 2--SPARSIFY TO k-NN GRAPH
# -----------------------------------------------------------------------------
# A fully-connected W has n^2 many edges which is computationally expensive
# We keep only the k strongest connections per node.
# This is known as k-NN sparsification
# Choosing k is another topic of conversation (common rule of thumb is cieling(log(n)))
knn <- 10
Wsparse <- matrix(0, nrow = nPts, ncol = nPts)
for (i in seq_len(nPts)) {
  topK <- order(W[i, ], decreasing = TRUE)[1:knn]
  Wsparse[i, topK] <- W[i, topK]
}
Wsparse <- pmax(Wsparse, t(Wsparse))  
rownames(Wsparse) <- seq_len(nPts)
colnames(Wsparse) <- seq_len(nPts)

gSpirals <- graph_from_adjacency_matrix(Wsparse, 
                                        mode = "undirected", 
                                        weighted = TRUE, 
                                        diag = FALSE)
edgelistSpirals <- as_data_frame(gSpirals, what = "edges")
edgelistSpirals$fromIdx <- as.integer(edgelistSpirals$from)
edgelistSpirals$toIdx <- as.integer(edgelistSpirals$to)
edgelistSpirals$x0 <- spiralDf$x[edgelistSpirals$fromIdx]
edgelistSpirals$y0 <- spiralDf$y[edgelistSpirals$fromIdx]
edgelistSpirals$x1 <- spiralDf$x[edgelistSpirals$toIdx]
edgelistSpirals$y1 <- spiralDf$y[edgelistSpirals$toIdx]

pKnnGraph <- ggplot() +
  geom_segment(data = edgelistSpirals,
               aes(x = x0, y = y0, xend = x1, yend = y1, alpha = weight),
               color = "grey50", linewidth = 0.3) +
  geom_point(data = spiralDf, aes(x = x, y = y, color = trueCluster),
             size = 1.5, alpha = 0.9) +
  scale_color_viridis_d(option = "cividis", name = "True Cluster") +
  scale_alpha_continuous(range = c(0.05, 0.5), guide = "none") +
  coord_fixed() +
  labs(title = "k-NN Similarity Graph", x = "X1", y = "X2") +
  theme(legend.position = "bottom",
        panel.grid = element_blank(),
        panel.border = element_rect(NA, "black", 1))
pKnnGraph

# lets zoom in
pKnnGraph2 <- ggplot() +
  geom_segment(data = edgelistSpirals,
               aes(x = x0, y = y0, xend = x1, yend = y1, alpha = weight),
               color = "grey50", linewidth = 0.3) +
  geom_point(data = spiralDf, aes(x = x, y = y, color = trueCluster),
             size = 1.5, alpha = 0.9) +
  scale_color_viridis_d(option = "cividis", name = "True Cluster") +
  scale_alpha_continuous(range = c(0.05, 0.5), guide = "none") +
  coord_fixed() +
  xlim(c(-0.4,0.4))+ylim(c(0.2,1.2)) +
  labs(title = "k-NN Similarity Graph", x = "X1", y = "X2") +
  theme(legend.position = "bottom",
        panel.grid = element_blank(),
        panel.border = element_rect(NA, "black", 1))
pKnnGraph2

# STEP 3--COMPUTE THE GRAPH LAPLACIAN
# -----------------------------------------------------------------------------
# use the sparse W for the Laplacian 
dVec <- rowSums(Wsparse)  
D <- diag(dVec)
L <- D-Wsparse
# normalised Laplacian: Lsym = D^{-1/2} L D^{-1/2}
dInvSqrt <- diag(1/sqrt(pmax(dVec, 1e-10)))
Lsym <- dInvSqrt %*% L %*% dInvSqrt


# STEP 4--EIGENDECOMPOSITION + EIGENGAP
# -----------------------------------------------------------------------------
eigenLsym <- eigen(Lsym, symmetric = TRUE)
eigenvalues <- rev(eigenLsym$values)  
eigenvectors <- eigenLsym$vectors[, rev(seq_len(ncol(eigenLsym$vectors)))]

# the eigengap: a large jump between eigenvalue k and k+1
# indicates k clusters (think of it like the elbow method we've seen for other methods)
eigenDf <- data.frame(index = 1:8, eigenvalue = eigenvalues[1:8])

pEigenvalues <- ggplot(eigenDf, aes(x = index, y = eigenvalue)) +
  geom_point(size = 3, color = "steelblue") +
  geom_line(color = "steelblue", linewidth = 0.7) +
  geom_vline(xintercept = 3.5, linetype = "dashed", color = "red") +
  annotate("text", x = 3.75, y = max(eigenDf$eigenvalue)*0.85,
           label = "k=3", color = "red", size = 3.5) +
  labs(x = "Index", y = "Eigenvalue") +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(NA, "black", 1))
pEigenvalues
# maybe k = 6 is a good one to try too!

# STEP 5--VISUALIZE THE SPECTRAL EMBEDDING
# -----------------------------------------------------------------------------
# the first k eigenvectors embed the nodes into R^k
k <- 3
U <- eigenvectors[, 1:k]
embedDf <- data.frame(u1 = U[, 1], u2 = U[, 2], u3 = U[, 3],
  trueCluster = spiralDf$trueCluster)

pEmbed12 <- ggplot(embedDf, aes(x = u1, y = u2, color = trueCluster)) +
  geom_point(size = 1.8, alpha = 0.8) +
  scale_color_viridis_d(option = "cividis", name = "True Cluster") +
  labs(x = "U1", y = "U2") +
  theme(legend.position = "bottom",
        panel.grid = element_blank(),
        panel.border = element_rect(NA, "black", 1))

pEmbed13 <- ggplot(embedDf, aes(x = u1, y = u3, color = trueCluster)) +
  geom_point(size = 1.8, alpha = 0.8) +
  scale_color_viridis_d(option = "cividis", name = "True Cluster") +
  labs(x = "U1", y = "U3") +
  theme(legend.position = "bottom",
        panel.grid = element_blank(),
        panel.border = element_rect(NA, "black", 1))

pEmbed23 <- ggplot(embedDf, aes(x = u2, y = u3, color = trueCluster)) +
  geom_point(size = 1.8, alpha = 0.8) +
  scale_color_viridis_d(option = "cividis", name = "True Cluster") +
  labs(x = "U2", y = "U3") +
  theme(legend.position = "bottom",
        panel.grid = element_blank(),
        panel.border = element_rect(NA, "black", 1))

grid.arrange(pEmbed12, pEmbed13, pEmbed23, ncol = 3)

# STEP 6--ROW-NORMALIZE AND RUN K-MEANS IN THE EIGENSPACE
# -----------------------------------------------------------------------------
rowNorms <- sqrt(rowSums(U^2))
rowNorms[rowNorms == 0] <- 1
T <- U/rowNorms
spectralKmeans <- kmeans(T, centers = k, nstart = 50)
spiralDf$spectralCluster <- factor(spectralKmeans$cluster)

pSpectralSpirals <- ggplot(spiralDf, aes(x = x, y = y, color = spectralCluster)) +
  geom_point(size = 1.8, alpha = 0.8) +
  scale_color_viridis_d(option = "cividis") +
  coord_fixed() +
  labs(title = "Spectral", x = "X1", y = "X2", color = "Cluster") +
  theme(legend.position = "bottom",
        panel.grid = element_blank(),
        panel.border = element_rect(NA, "black", 1))

grid.arrange(
  pTrueSpirals  +ggtitle("True Labels"),
  pKmeansSpirals+ggtitle("K-Means"),
  pSpectralSpirals,
  ncol = 3
)

# ANOTHER METHOD
# MODULARITY-BASED COMMUNITY DETECTION-- LOUVAIN
# -----------------------------------------------------------------------------
# an alternative to spectral clustering is to maximize modularity Q
# modularity Q measures how much the edge density within communities
# exceeds what we would expect under a random graph with the same degrees:

# the Louvain method is the most widely used modularity optimization algorithm
# 1. start with each node in its own community
# 2. move nodes to neighbouring communities if Q improves
# 3. aggregate communities into single 'super-nodes' and repeat 
# this gives a hierarchy of communities 

louvainResult <- cluster_louvain(gSpirals)
spiralDf$louvainCluster <- factor(membership(louvainResult))
# Louvain modularity
round(modularity(louvainResult), 4)
# Number of communities found
max(membership(louvainResult))

# Louvain on the kNN graph will likely over-partition the spirals
# because it finds locally best communities, not globally

pLouvainSpirals <- ggplot(spiralDf, aes(x = x, y = y, color = louvainCluster)) +
  geom_point(size = 1.8, alpha = 0.8) +
  scale_color_viridis_d(option = "cividis") +
  coord_fixed() +
  labs(x = "X1", y = "X2", color = "Community") +
  theme(legend.position = "bottom",
        panel.grid = element_blank(),
        panel.border = element_rect(NA, "black", 1))

pLouvainSpirals

# APPLICATION-INFRASTRUCTURE NETWORK DATA
# =============================================================================
# synthetic power grid for a regional network
# nodes->substations/relays/generators
# edges->transmission lines with capacity weights
# goal->identify community structure


# DATA GENERATION
set.seed(2026)
nNodes <- 150
nZones <- 5
zoneCenters <- data.frame(zoneId = 1:nZones,
  cx = c(-10, 10,  0, -12,  12),
  cy = c( 10, 10,  0, -10, -10))

zoneAssignment <- sample(1:nZones, nNodes, replace = TRUE)
nodeCoords <- data.frame(nodeId = 1:nNodes,
  x = zoneCenters$cx[zoneAssignment]+rnorm(nNodes, 0, 3.5),
  y = zoneCenters$cy[zoneAssignment]+rnorm(nNodes, 0, 3.5),
  trueZone = factor(zoneAssignment),
  nodeType = sample(c("Substation", "Relay", "Generator"), nNodes,
                    replace = TRUE, prob = c(0.5, 0.35, 0.15)))
knnIntra <- 4
edgeList <- data.frame(from = integer(), to = integer(), weight = numeric())
for (z in 1:nZones) {
  idx <- which(nodeCoords$trueZone == z)
  if (length(idx) < 2) next
  coords <- as.matrix(nodeCoords[idx, c("x", "y")])
  dz <- as.matrix(dist(coords))
  for (i in seq_len(nrow(dz))) {
    nn <- order(dz[i, ])[2:(knnIntra+1)]
    for (j in nn) {
      if (idx[i] < idx[j]) {
        cap <- exp(-dz[i, j]^2/(2*4^2))+rnorm(1, 0, 0.02)
        edgeList <- rbind(edgeList, data.frame(
          from = idx[i], to = idx[j], weight = max(cap, 0.01)
        ))
      }
    }
  }
}

nCross <- 20
for (cc in 1:nCross) {
  z1 <- sample(1:nZones, 1)
  z2 <- sample(setdiff(1:nZones, z1), 1)
  n1 <- sample(which(nodeCoords$trueZone == z1), 1)
  n2 <- sample(which(nodeCoords$trueZone == z2), 1)
  cap <- 0.05+rnorm(1, 0, 0.01)
  edgeList <- rbind(edgeList, data.frame(from = n1, to = n2, weight = max(cap, 0.01)))
}
edgeList <- edgeList[!duplicated(paste(edgeList$from, edgeList$to)), ]
nodeCoords$nodeName <- as.character(nodeCoords$nodeId)
edgeList$from <- as.character(edgeList$from)
edgeList$to <- as.character(edgeList$to)

gInfra <- graph_from_data_frame(edgeList, directed = FALSE,
  vertices = nodeCoords[, c("nodeName", setdiff(names(nodeCoords), "nodeName"))])

edgeDf <- as_data_frame(gInfra, what = "edges")
nodeIndex <- setNames(seq_len(nrow(nodeCoords)), nodeCoords$nodeName)
edgeDf$fromIdx <- nodeIndex[edgeDf$from]
edgeDf$toIdx <- nodeIndex[edgeDf$to]
edgeDf$x0 <- nodeCoords$x[edgeDf$fromIdx]
edgeDf$y0 <- nodeCoords$y[edgeDf$fromIdx]
edgeDf$x1 <- nodeCoords$x[edgeDf$toIdx]
edgeDf$y1 <- nodeCoords$y[edgeDf$toIdx]

# VISUALIZATION THE NETWORK
# -----------------------------------------------------------------------------
# 150 nodes, 319 edges
pRawNetwork <- ggplot() +
  geom_segment(data = edgeDf,
               aes(x = x0, y = y0, xend = x1, yend = y1, alpha = weight),
               color = "grey50", linewidth = 0.5) +
  geom_point(data = nodeCoords,
             aes(x = x, y = y, color = trueZone, shape = nodeType),
             size = 2.5) +
  scale_color_viridis_d(option = "cividis", name = "True Zone") +
  scale_alpha_continuous(range = c(0.2, 1), guide = "none") +
  labs(x = "X1 (km)", y = "X2 (km)", shape = "Node Type") +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(NA, "black", 1))
pRawNetwork

# GRAPH SUMMARY STATISTICS
# -----------------------------------------------------------------------------
nodeCoords$degree <- degree(gInfra)
nodeCoords$betweenness <- betweenness(gInfra, normalized = TRUE)
nodeCoords$strength <- strength(gInfra)

criticalNodes <- nodeCoords %>%
  arrange(desc(betweenness)) %>%
  head(10)
print(as.data.frame(criticalNodes[, c("nodeId","nodeType","trueZone","degree","betweenness")]))

pDegreeMap <- ggplot() +
  geom_segment(data = edgeDf,
               aes(x = x0, y = y0, xend = x1, yend = y1),
               color = "grey70", linewidth = 0.4, alpha = 0.6) +
  geom_point(data = nodeCoords, alpha = 0.8,
             aes(x = x, y = y, color = betweenness, size = degree)) +
  scale_color_gradient(low = "navy", high = "gold", name = "Betweenness") +
  scale_size_continuous(range = c(0.5,6), name = "Degree") +
  labs(x = "X1 (km)", y = "X2 (km)") +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(NA, "black", 1))
pDegreeMap

# SPECTRAL CLUSTERING
# -----------------------------------------------------------------------------
Winfro <- as_adjacency_matrix(gInfra, attr = "weight", sparse = FALSE)
dVecinfro <- rowSums(Winfro)
dISinfro <- diag(1/sqrt(pmax(dVecinfro, 1e-10)))
Lsyminfro <- dISinfro %*% (diag(dVecinfro)-Winfro) %*% dISinfro
eigeninfro <- eigen(Lsyminfro, symmetric = TRUE)
eigvalsinfro <- rev(eigeninfro$values)
eigvecsinfro <- eigeninfro$vectors[, rev(seq_len(ncol(eigeninfro$vectors)))]
eigenDfInfra <- data.frame(index = 1:8, eigenvalue = eigvalsinfro[1:8])

pEigenInfra <- ggplot(eigenDfInfra, aes(x = index, y = eigenvalue)) +
  geom_point(size = 3, color = "steelblue") +
  geom_line(color = "steelblue", linewidth = 0.7) +
  geom_vline(xintercept = 5.5, linetype = "dashed", color = "red") +
  annotate("text", x = 6, y = max(eigenDfInfra$eigenvalue)*0.7,
           label = "k = 5", color = "red", size = 3.5) +
  labs(x = "Index", y = "Eigenvalue") +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(NA, "black", 1))
pEigenInfra


kInfra <- 5
Uinfro <- eigvecsinfro[, 1:kInfra]
rninfro <- sqrt(rowSums(Uinfro^2)); rninfro[rninfro == 0] <- 1
Tinfro <- Uinfro/rninfro

spectralInfra <- kmeans(Tinfro, centers = kInfra, nstart = 50)
nodeCoords$spectralCluster <- factor(spectralInfra$cluster)

pSpectralInfra <- ggplot() +
  geom_segment(data = edgeDf,
               aes(x = x0, y = y0, xend = x1, yend = y1),
               color = "grey70", linewidth = 0.4, alpha = 0.5) +
  geom_point(data = nodeCoords,
             aes(x = x, y = y, color = spectralCluster, shape = nodeType),
             size = 2.5) +
  scale_color_viridis_d(option = "cividis", name = "Spectral\nCluster") +
  labs(x = "X1 (km)", y = "X2 (km)", shape = "Node Type") +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(NA, "black", 1))
pSpectralInfra

# EFFECT OF K 
# -----------------------------------------------------------------------------
kValues <- c(2, 3, 4, 5, 6, 7)
kPlots  <- list()

for (i in seq_along(kValues)) {
  ki  <- kValues[i]
  Uk  <- eigvecsinfro[, 1:ki]
  rnk <- sqrt(rowSums(Uk^2)); rnk[rnk == 0] <- 1
  Tk  <- Uk/rnk
  kmk <- kmeans(Tk, centers = ki, nstart = 30)
  tmpDf <- nodeCoords
  tmpDf$cluster <- factor(kmk$cluster)
  kPlots[[i]] <- ggplot() +
    geom_segment(data = edgeDf,
                 aes(x = x0, y = y0, xend = x1, yend = y1),
                 color = "grey70", linewidth = 0.3, alpha = 0.4) +
    geom_point(data = tmpDf, aes(x = x, y = y, color = cluster),
               size = 1.5, alpha = 0.8) +
    scale_color_viridis_d(option = "cividis") +
    labs(title = paste0("k = ", ki)) +
    theme(legend.position = "none",
          axis.title = element_blank(),
          plot.title = element_text(size = 10),
          panel.grid = element_blank(),
          panel.border = element_rect(NA, "black", 1))
}
grid.arrange(grobs = kPlots, ncol = 3)

# LOUVAIN COMMUNITY DETECTION ON INFRASTRUCTURE NETWORK
# -----------------------------------------------------------------------------
louvainInfra <- cluster_louvain(gInfra)
nodeCoords$louvainCluster <- factor(membership(louvainInfra))

# 11 communities
max(membership(louvainInfra))
pLouvainInfra <- ggplot() +
  geom_segment(data = edgeDf,
               aes(x = x0, y = y0, xend = x1, yend = y1),
               color = "grey70", linewidth = 0.4, alpha = 0.5) +
  geom_point(data = nodeCoords,
             aes(x = x, y = y, color = louvainCluster),
             size = 2.5) +
  scale_color_viridis_d(option = "cividis", name = "Community") +
  labs(x = "X1 (km)", y = "X2 (km)") +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(NA, "black", 1))
pLouvainInfra

grid.arrange(
  pRawNetwork   +ggtitle("True Zones") +theme(legend.position = "none"),
  pSpectralInfra+ggtitle("Spectral")   +theme(legend.position = "none"),
  pLouvainInfra +ggtitle("Louvain")    +theme(legend.position = "none"),
  pDegreeMap    +ggtitle("Centrality") +theme(legend.position = "none"),
  ncol = 2
)

# COMMUNITY CHARACTERISATION
communityProfile <- nodeCoords %>%
  group_by(louvainCluster) %>%
  summarise(nNodes = n(),
    meanDegree = round(mean(degree), 2),
    maxBetween = round(max(betweenness), 4),
    nGenerators = sum(nodeType == "Generator"),
    nSubstations = sum(nodeType == "Substation"),
    .groups = "drop")
print(as.data.frame(communityProfile))

# CLUSTERING PERFORMANCE METRICS
trueLabelsInfra <- as.numeric(nodeCoords$trueZone)

computeARI <- function(trueLabels, predictedLabels) {
  contingency <- table(trueLabels, predictedLabels)
  a <- rowSums(contingency)
  b <- colSums(contingency)
  n <- sum(contingency)
  sumCombNij <- sum(choose(contingency, 2))
  sumCombA <- sum(choose(a, 2))
  sumCombB <- sum(choose(b, 2))
  combN <- choose(n, 2)
  expectedIndex <- sumCombA*sumCombB/combN
  maxIndex <- 0.5*(sumCombA+sumCombB)
  ari <- (sumCombNij-expectedIndex)/(maxIndex-expectedIndex)
  return(ari)
}

computeNMI <- function(trueLabels, predictedLabels) {
  n <- length(trueLabels)
  pTrue <- table(trueLabels)/n
  hTrue <- -sum(pTrue*log(pTrue+1e-10))
  pPred <- table(predictedLabels)/n
  hPred <- -sum(pPred*log(pPred+1e-10))
  contingency <- table(trueLabels, predictedLabels)/n
  contingency[contingency == 0] <- 1e-10
  hJoint <- -sum(contingency*log(contingency))
  mi <- hTrue+hPred-hJoint
  nmi <- 2*mi/(hTrue+hPred)
  return(nmi)
}

ariSpectral <- computeARI(trueLabelsInfra, as.numeric(nodeCoords$spectralCluster))
nmiSpectral <- computeNMI(trueLabelsInfra, as.numeric(nodeCoords$spectralCluster))
ariLouvain  <- computeARI(trueLabelsInfra, as.numeric(nodeCoords$louvainCluster))
nmiLouvain  <- computeNMI(trueLabelsInfra, as.numeric(nodeCoords$louvainCluster))

print(paste0("Spectral: ARI = ", round(ariSpectral, 3), ", NMI = ", round(nmiSpectral, 3)))
print(paste0("Louvain:  ARI = ", round(ariLouvain,  3), ", NMI = ", round(nmiLouvain,  3)))


# ========================================================
# Application: North American Airport Network (US+Canada)
# Spectral Clustering+Louvain Community Detection


set.seed(2026)
theme_set(theme_minimal(base_size = 12))
# openflights.org publishes airport and route data
# airports.dat is airport metadata including IATA code, city, country, lat/lon
# routes.dat is airline route pairs by IATA code

airportUrl <- "https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat"
routeUrl <- "https://raw.githubusercontent.com/jpatokal/openflights/master/data/routes.dat"
airportRaw <- read.csv(airportUrl, header = FALSE, stringsAsFactors = FALSE)
routeRaw <- read.csv(routeUrl,   header = FALSE, stringsAsFactors = FALSE)
colnames(airportRaw)[1:8] <- c("airportId","name","city","country",
                               "iata","icao","lat","lon")
colnames(routeRaw)[1:6]   <- c("airline","airlineId","srcIATA","srcId",
                               "dstIATA","dstId")

# we will just look at Canada and the USA
targetCountries <- c("United States", "Canada")
naAirports <- airportRaw %>%
  filter(country %in% targetCountries,
         iata != "\\N", nchar(iata) == 3) %>%
  select(iata, name, city, country, lat, lon) %>%
  distinct(iata, .keep_all = TRUE)

# we will also filter routes to North American destinations only
naIata <- naAirports$iata
naRoutes <- routeRaw %>%
  filter(srcIATA %in% naIata, dstIATA %in% naIata,
         srcIATA != dstIATA) %>%
  select(srcIATA, dstIATA) %>%
  distinct()

# build the graph 
# this is just a matrix where each column is an airport
# and each row is an airport, and we place a 1 if the airport
# in row i flies to the airport in column j
gAirport <- graph_from_data_frame(naRoutes, directed = FALSE)
gAirport <- simplify(gAirport)  

# keep only the largest connected component
comp <- components(gAirport)
largestComp <- which.max(comp$csize)
gAirport <- induced_subgraph(gAirport, which(comp$membership == largestComp))

# join this information back to our airportDF
airportDf <- left_join(data.frame(iata = V(gAirport)$name, stringsAsFactors = FALSE),
  naAirports, by = "iata")
head(airportDf)

airportDf$degree <- degree(gAirport) # how many places does the given airport fly to
airportDf$betweenness <- betweenness(gAirport, normalized = TRUE)

airportEdgeDf <- as_data_frame(gAirport, what = "edges")
airportIndex <- setNames(seq_len(nrow(airportDf)), airportDf$iata)
airportEdgeDf$fromIdx <- airportIndex[airportEdgeDf$from]
airportEdgeDf$toIdx <- airportIndex[airportEdgeDf$to]
airportEdgeDf$x0 <- airportDf$lon[airportEdgeDf$fromIdx]
airportEdgeDf$y0 <- airportDf$lat[airportEdgeDf$fromIdx]
airportEdgeDf$x1 <- airportDf$lon[airportEdgeDf$toIdx]
airportEdgeDf$y1 <- airportDf$lat[airportEdgeDf$toIdx]

# Top 15 airports by degree
print(head(airportDf[order(-airportDf$degree),
                     c("iata","city","country","degree","betweenness")], 15))


# geographic layout by country
# 745 airports, 3376 routes
pRawNetwork <- ggplot() +
  geom_segment(data = airportEdgeDf,
               aes(x = x0, y = y0, xend = x1, yend = y1),
               color = "grey60", linewidth = 0.15, alpha = 0.25) +
  geom_point(data = airportDf,
             aes(x = lon, y = lat, color = country, size = degree),
             alpha = 0.85) +
  scale_color_manual(values = c("United States" = "steelblue", "Canada" = "firebrick"),
                     name = "Country") +
  scale_size_continuous(range = c(0.5, 6), name = "Degree") +
  coord_fixed(ratio = 1.3) +
  labs(x = "Longitude", y = "Latitude") +
  theme(legend.position = "bottom",
        panel.grid = element_blank(),
        panel.border = element_rect(NA, "black", 1))
pRawNetwork

# centrality map
pCentrality <- ggplot() +
  geom_segment(data = airportEdgeDf,
               aes(x = x0, y = y0, xend = x1, yend = y1),
               color = "grey70", linewidth = 0.15, alpha = 0.2) +
  geom_point(data = airportDf,
             aes(x = lon, y = lat, color = betweenness, size = degree),
             alpha = 0.9) +
  scale_color_gradient(low = "navy", high = "gold", name = "Betweenness") +
  scale_size_continuous(range = c(0.5, 6), name = "Degree") +
  coord_fixed(ratio = 1.3) +
  labs(x = "Longitude", y = "Latitude") +
  theme(legend.position = "bottom",
        panel.grid = element_blank(),
        panel.border = element_rect(NA, "black", 1))
pCentrality

# SPECTRAL CLUSTERING
# -----------------------------------------------------------------------------
Wair <- as_adjacency_matrix(gAirport, sparse = FALSE)
dVecair <- rowSums(Wair)
dISair <- diag(1/sqrt(pmax(dVecair, 1e-10)))
Lsymair <- dISair %*% (diag(dVecair)-Wair) %*% dISair
eigenair <- eigen(Lsymair, symmetric = TRUE)
eigvalsair <- rev(eigenair$values)
eigvecsair <- eigenair$vectors[, rev(seq_len(ncol(eigenair$vectors)))]

# EIGENGAP 
eigenDfAir <- data.frame(index = 1:20, eigenvalue = eigvalsair[1:20])

pEigenAir <- ggplot(eigenDfAir, aes(x = index, y = eigenvalue)) +
  geom_point(size = 3, color = "steelblue") +
  geom_line(color = "steelblue", linewidth = 0.7) +
  geom_vline(xintercept = 1.5, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 13.5, linetype = "dashed", color = "red") +
  annotate("text", x = 2.5, y = max(eigenDfAir$eigenvalue)*0.65,
           label = "k = 1", color = "red", size = 3.5) +
  annotate("text", x = 14.5, y = max(eigenDfAir$eigenvalue)*0.65,
           label = "k = 13", color = "red", size = 3.5) +
  labs(x = "Index", y = "Eigenvalue") +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(NA, "black", 1))
pEigenAir

# lets go with 13 since k=1 defeats the purpose of clustering here

# SPECTRAL EMBEDDING 
kAir <- 13
Uair <- eigvecsair[, 1:kAir]
rnair <- sqrt(rowSums(Uair^2)); rnair[rnair == 0] <- 1
Tair <- Uair/rnair

embedDfAir <- data.frame(u1 = Uair[, 1],
  u2 = Uair[, 2], u3 = Uair[, 3],
  u4 = Uair[, 4], country = airportDf$country,
  degree  = airportDf$degree)

makeEigenpair <- function(i, j) {
  ggplot(embedDfAir, aes(x = .data[[paste0("u", i)]],
             y = .data[[paste0("u", j)]], color = country,
             size = degree)) +
    geom_point(alpha = 0.7) +
    scale_color_manual(values = c("United States" = "steelblue",
                 "Canada" = "firebrick"), name = "Country") +
    scale_size_continuous(range = c(0.5, 4), guide = "none") +
    labs(x = paste("U", i), y = paste("U", j)) +
    theme_classic() +
    theme(axis.text  = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "bottom"
    )
}

p12 <- makeEigenpair(1,2)
p13 <- makeEigenpair(1,3)
p14 <- makeEigenpair(1,4)
p23 <- makeEigenpair(2,3)
p24 <- makeEigenpair(2,4)
p34 <- makeEigenpair(3,4)


(p12 | p13 | p14) /
  (p23 | p24 | p34) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

spectralAir <- kmeans(Tair, centers = kAir, nstart = 50)
airportDf$spectralCluster <- factor(spectralAir$cluster)

pSpectralAir <- ggplot() +
  geom_segment(data = airportEdgeDf,
               aes(x = x0, y = y0, xend = x1, yend = y1),
               color = "grey70", linewidth = 0.15, alpha = 0.4) +
  geom_point(data = airportDf,
             aes(x = lon, y = lat, color = spectralCluster, size = degree),
             alpha = 0.9) +
  scale_color_viridis_d(option = "cividis", name = "Region") +
  scale_size_continuous(range = c(0.8, 5), guide = "none") +
  coord_fixed(ratio = 1.3) +
  labs(x = "Longitude", y = "Latitude") +
  theme(legend.position = "bottom",
        panel.grid = element_blank(),
        panel.border = element_rect(NA, "black", 1))
pSpectralAir

# SPECTRAL GRAPH CUTS
airportEdgeDf$clusterFrom <- airportDf$spectralCluster[airportEdgeDf$fromIdx]
airportEdgeDf$clusterTo <- airportDf$spectralCluster[airportEdgeDf$toIdx]
airportEdgeDf$edgeType <- ifelse(
  airportEdgeDf$clusterFrom == airportEdgeDf$clusterTo,
  "Within cluster", "Cut edge")

cutFracAir <- mean(airportEdgeDf$edgeType == "Cut edge")
# Fraction of routes cut
round(cutFracAir, 3)


pCutMap <- ggplot() +
  geom_segment(data = airportEdgeDf,
               aes(x = x0, y = y0, xend = x1, yend = y1,
                   color = edgeType, linewidth = edgeType, alpha = edgeType)) +
  geom_point(data = airportDf,
             aes(x = lon, y = lat, fill = spectralCluster, size = degree),
             shape = 21, color = "white", stroke = 0.3, alpha = 0.9) +
  scale_color_manual(values = c("Within cluster" = "grey70", "Cut edge" = "firebrick"),
                     name = "Edge type") +
  scale_linewidth_manual(values = c("Within cluster" = 0.3, "Cut edge" = 0.3),
                         name = "Edge type") +
  scale_alpha_manual(values = c("Within cluster" = 0.05, "Cut edge" = 0.4),
                     name = "Edge type") +
  scale_fill_viridis_d(option = "cividis", name = "Region") +
  scale_size_continuous(range = c(0.8, 5), guide = "none") +
  coord_fixed(ratio = 1.3) +
  labs(x = "Longitude", y = "Latitude") +
  theme(legend.position = "bottom",
        panel.grid = element_blank(),
        panel.border = element_rect(NA, "black", 1))
pCutMap

# LOUVAIN 
louvainAir <- cluster_louvain(gAirport)
airportDf$louvainCluster <- factor(membership(louvainAir))

# 10 communities detected
max(membership(louvainAir))
    
pLouvainAir <- ggplot() +
  geom_segment(data = airportEdgeDf,
               aes(x = x0, y = y0, xend = x1, yend = y1),
               color = "grey70", linewidth = 0.15, alpha = 0.4) +
  geom_point(data = airportDf,
             aes(x = lon, y = lat, color = louvainCluster, size = degree),
             alpha = 0.9) +
  scale_color_viridis_d(option = "cividis", name = "Community") +
  scale_size_continuous(range = c(0.8, 5), guide = "none") +
  coord_fixed(ratio = 1.3) +
  labs(x = "Longitude", y = "Latitude") +
  theme(legend.position = "bottom",
        panel.grid = element_blank(),
        panel.border = element_rect(NA, "black", 1))
pLouvainAir

# LOUVAIN CUT EDGES
airportEdgeDf$louvainFrom <- airportDf$louvainCluster[airportEdgeDf$fromIdx]
airportEdgeDf$louvainTo <- airportDf$louvainCluster[airportEdgeDf$toIdx]
airportEdgeDf$louvainEdge <- ifelse(
  airportEdgeDf$louvainFrom == airportEdgeDf$louvainTo,
  "Within cluster", "Cut edge")

cutFracLouvain <- mean(airportEdgeDf$louvainEdge == "Cut edge")
round(cutFracLouvain, 3) # 33.4% edges cut

pLouvainCuts <- ggplot() +
  geom_segment(data = airportEdgeDf,
               aes(x = x0, y = y0, xend = x1, yend = y1,
                   color = louvainEdge, linewidth = louvainEdge, alpha = louvainEdge)) +
  geom_point(data = airportDf,
             aes(x = lon, y = lat, fill = louvainCluster, size = degree),
             shape = 21, color = "white", stroke = 0.3, alpha = 0.9) +
  scale_color_manual(values = c("Within cluster" = "grey70", "Cut edge" = "firebrick"),
                     name = "Edge type") +
  scale_linewidth_manual(values = c("Within cluster" = 0.3, "Cut edge" = 0.3),
                         name = "Edge type") +
  scale_alpha_manual(values = c("Within cluster" = 0.05, "Cut edge" = 0.4),
                     name = "Edge type") +
  scale_fill_viridis_d(option = "cividis", name = "Community") +
  scale_size_continuous(range = c(0.8, 5), guide = "none") +
  coord_fixed(ratio = 1.3) +
  labs(x = "Longitude", y = "Latitude") +
  theme(legend.position = "bottom",
        panel.grid = element_blank(),
        panel.border = element_rect(NA, "black", 1))
pLouvainCuts

# SIDE BY SIDE COMPARISON
# -----------------------------------------------------------------------------
grid.arrange(
  pRawNetwork +ggtitle("Raw Network")        +theme(legend.position = "none"),
  pCentrality +ggtitle("Centrality")         +theme(legend.position = "none"),
  pSpectralAir+ggtitle("Spectral Clustering")+theme(legend.position = "none"),
  pLouvainAir +ggtitle("Louvain")            +theme(legend.position = "none"),
  ncol = 2
)


# COMMUNITY PROFILES
# spectral
spectralProfile <- airportDf %>%
  group_by(spectralCluster) %>%
  summarise(
    nAirports  = n(),
    nUS = sum(country == "United States"),
    nCanada = sum(country == "Canada"),
    meanDegree = round(mean(degree), 1),
    topHub = iata[which.max(degree)],
    topHubCity = city[which.max(degree)],
    .groups = "drop"
  )
print(as.data.frame(spectralProfile))


