# DENSITY-BASED CLUSTERING
# Workshop 5: DBSCAN, HDBSCAN, OPTICS
library(dbscan)           
library(factoextra)      
library(ggplot2)          
library(dplyr)           
library(tidyr)           
library(gridExtra)        
library(viridis)          
library(cluster)          
library(fpc)    
library(ggforce)
library(data.table)

set.seed(2026)
theme_set(theme_minimal(base_size = 12))

# why use density-based clustering?
# we showed kmeans has a limitation of hard cluster boundaries
# and the assumption of circular/spherical clusters
# with fuzzy c-means we addressed the hard cluster boundaries
# with distribution-based models we relaxed the circular cluster assumption
# now we relax the assumption of distributional forms all together
# and focus on the density of the observations, which we will see is
# better at handling outliers


# LIMITATIONS OF KMEANS
# -----------------------------------------------------------------------------

data("multishapes")
# cluster 6 is the outliers in the data

shapesDf <- data.frame(x = multishapes[, 1], y = multishapes[, 2],
  trueCluster = factor(multishapes[, 3]))

pTrueShapes <- ggplot(shapesDf, aes(x = x, y = y, color = trueCluster)) +
  geom_point(size = 1.5, alpha = 0.7) +
  scale_color_viridis_d(option = "turbo") +
  labs(x = "X", y = "Y", color = "Cluster") +
  theme(legend.position = "bottom",
        panel.grid = element_blank(),
        panel.border = element_rect(NA, "black", 1))

pTrueShapes

# k-means (try k = 6 if you want but we argue based on visualizations
# above there are only 5 clear clusters)
kmeansShapes <- kmeans(shapesDf[, c("x", "y")], centers = 5, nstart = 25)
shapesDf$kmeansCluster <- factor(kmeansShapes$cluster)

# k-means results
pKmeansShapes <- ggplot(shapesDf, aes(x = x, y = y, color = kmeansCluster)) +
  geom_point(size = 1.5, alpha = 0.7) +
  scale_color_viridis_d(option = "turbo") +
  labs(title = "K-Means",
       x = "X", y = "Y", color = "Cluster") +
  theme(legend.position = "bottom",
        panel.grid = element_blank(),
        panel.border = element_rect(NA, "black", 1))

pKmeansShapes

# and we can see the circular clusters here
centers <- as.data.frame(kmeansShapes$centers)
names(centers) <- c("x0", "y0")
centers$r <- 0.9

pKmeansShapes2 <- ggplot(shapesDf, aes(x = x, y = y, color = kmeansCluster)) +
  geom_point(size = 1.5, alpha = 0.7) +
  geom_circle(
    data = centers,
    aes(x0 = x0, y0 = y0, r = r),
    inherit.aes = FALSE,
    color = "red",
    linewidth = 0.7
  ) +
  coord_fixed() +
  scale_color_viridis_d(option = "turbo") +
  labs(title = "K-Means (K=5)",
       x = "X", y = "Y", color = "Cluster") +
  theme(
    legend.position = "bottom",
    panel.grid = element_blank(),
    panel.border = element_rect(NA, "black", 1)
  )

pKmeansShapes2


grid.arrange(pTrueShapes + ggtitle("True Labels"), 
             pKmeansShapes + ggtitle("K-Means"), 
             ncol = 2)


# OVERVIEW OF DENSITY BASED CLUSTERING
# -----------------------------------------------------------------------------

# DBSCAN (Density-Based Spatial Clustering of Applications with Noise)
# worked by creating clusters where the clusters
# are dense regions separated by sparse regions
# the algorithm requires two parameters
# 1. eps: radius of neighbourhood around a point
# 2. minPts: minimum number of points required to form a dense region

# to classify a point into a cluster we have the following types of 
# points we can label as

# Core point... has >= minPts neighbours within eps radius
# Border point... within eps of a core point, but < minPts neighbours
# Noise point... neither of above so classify as an outlier

# the algorithm works as follows:
# 1. For each unvisited point p
# 1a. Find all neighbours within eps radius
# 1b. If |neighbours| >= minPts, p is a core point and we
# create new cluster, add p and all density-reachable points
# 1c. If 1b is not satisfied, mark p as noise (which may later become border point)
# 2. Continue until all points are considered.

# lets see a toy example
set.seed(2026)
conceptData <- data.frame(
  x = c(rnorm(20, 0, 0.3), rnorm(15, 2, 0.3), runif(5, -1, 3)),
  y = c(rnorm(20, 0, 0.3), rnorm(15, 2, 0.3), runif(5, -1, 3))
)

corePointIdx <- 10 # we are examining only 1 point
eps <- 0.5 # we will talk about choosing eps later
minPts <- 4 # and we will talk about minPts later too

# distances from core point
distances <- sqrt((conceptData$x - conceptData$x[corePointIdx])^2 + 
                    (conceptData$y - conceptData$y[corePointIdx])^2)
inNeighbourhood <- distances <= eps

conceptData$type <- "Other"
conceptData$type[inNeighbourhood] <- "In Neighbourhood"
conceptData$type[corePointIdx] <- "Core Point"

nNeighbours <- sum(inNeighbourhood) - 1  # exclude self

# red is a core point
# we see there is 11 points within eps radius
# and since 11 >= 4 (minPts) it is considered a core point

pConcept <- ggplot(conceptData, aes(x = x, y = y, color = type)) +
  geom_point(size = 3) +
  annotate("path",
           x = conceptData$x[corePointIdx] + eps * cos(seq(0, 2*pi, length.out = 100)),
           y = conceptData$y[corePointIdx] + eps * sin(seq(0, 2*pi, length.out = 100)),
           color = "red", linetype = "dashed", linewidth = 1) +
  scale_color_manual(values = c("Core Point" = "red", 
                                "In Neighbourhood" = "orange",
                                "Other" = "grey60")) +
  labs(title = "DBSCAN Core Point + Neighbourhood",
       subtitle = paste("eps =", eps, ", minPts =", minPts),
       x = "X", y = "Y") +
  theme(legend.position = "right") +
  theme(
    legend.position = "bottom",
    panel.grid = element_blank(),
    panel.border = element_rect(NA, "black", 1)
  )

pConcept

# BACK TO MULTISHAPE DATA WITH DBSCAN
# -----------------------------------------------------------------------------
shapesMatrix <- as.matrix(shapesDf[, c("x", "y")])
dbscanResult <- dbscan::dbscan(shapesMatrix, eps = 0.15, minPts = 5)

# notice we do not need to specify the number of clusters
max(dbscanResult$cluster)
# we found 5 clusters

sum(dbscanResult$cluster == 0)
# there is 31 noise points
# examine the number of observations in each cluster (cluster 0 is the noise)
print(table(dbscanResult$cluster))

shapesDf$dbscanCluster <- factor(dbscanResult$cluster)

# visualize DBSCAN result
pDbscanShapes <- ggplot(shapesDf, aes(x = x, y = y, color = dbscanCluster)) +
  geom_point(size = 1.5, alpha = 0.7) +
  scale_color_manual(values = c("0" = "grey40", 
                                setNames(viridis(5), as.character(1:5)))) +
  labs(title = "DBSCAN Multishapes",
       x = "X", y = "Y", color = "Cluster") +
  theme(legend.position = "right") +
  theme(
    legend.position = "right",
    panel.grid = element_blank(),
    panel.border = element_rect(NA, "black", 1)
  )

pDbscanShapes

grid.arrange(
  pTrueShapes + ggtitle("True Labels") + theme(legend.position = "none"),
  pKmeansShapes + ggtitle("K-Means") + theme(legend.position = "none"),
  pDbscanShapes + ggtitle("DBSCAN") + theme(legend.position = "none"),
  ncol = 3
)

# PARAMETER SELECTION FOR DBSCAN
# -----------------------------------------------------------------------------

# similar to WSCC from kmeans to select number of cluster k,
# for dbscan we can use something called the k-distance plot
# to determine a good choice of eps.
# like WSCC, we look for the elbow of the plot

k <- 5  
knnDist <- kNNdist(shapesMatrix, k = k)
sortedDist <- sort(knnDist)
kDistDf <- data.frame(index = 1:length(sortedDist), distance = sortedDist)

pKdist <- ggplot(kDistDf, aes(x = index, y = distance)) +
  geom_line(color = "steelblue", linewidth = 0.8) +
  geom_hline(yintercept = 0.15, linetype = "dashed", color = "red") +
  annotate("text", x = 200, y = 0.18, label = "eps = 0.15", color = "red") +
  labs(x = "Points (sorted by distance)", y = paste0(k, "-NN Distance")) +
  theme_minimal() +
  theme(
    legend.position = "right",
    panel.grid = element_blank(),
    panel.border = element_rect(NA, "black", 1)
  )

pKdist
# seems eps = 0.15 is reasonable

# we can also test different eps values
epsValues <- c(0.05, 0.10, 0.15, 0.20, 0.30, 0.50)
epsResults <- list()

for (epsVal in epsValues) {
  result <- dbscan::dbscan(shapesMatrix, eps = epsVal, minPts = 5)
  epsResults[[as.character(epsVal)]] <- list(
    eps = epsVal,
    nClusters = max(result$cluster),
    nNoise = sum(result$cluster == 0),
    clusters = result$cluster
  )
}

colmatnames <- c("eps","# Clusters found", "# Noise points")
epsMat <- matrix(NA, nrow = 6, ncol = 3)
colnames(epsMat) <- colmatnames
for (i in 1:6) {
  epsMat[i,] <- c(round(epsResults[[i]]$eps, 3), 
                  epsResults[[i]]$nClusters, epsResults[[i]]$nNoise)
}

epsMat

plotsEps <- list()
for (i in seq_along(epsValues)) {
  epsVal <- epsValues[i]
  clusters <- epsResults[[as.character(epsVal)]]$clusters
  tempDf <- data.frame(x = shapesMatrix[, 1], y = shapesMatrix[, 2],
                       cluster = factor(clusters))
  plotsEps[[i]] <- ggplot(tempDf, aes(x = x, y = y, color = cluster)) +
    geom_point(size = 1, alpha = 0.7) +
    labs(title = paste("eps =", epsVal)) +
    theme(legend.position = "none",
          axis.title = element_blank(),
          plot.title = element_text(size = 10),
          panel.grid = element_blank(),
          panel.border = element_rect(NA, "black", 1))
}

grid.arrange(grobs = plotsEps, ncol = 3,
             top = "Effect of eps (red is noise)")


# OPTICS (ORDERING POINTS TO IDENTIFY CLUSTERING STRUCTURE)
# -----------------------------------------------------------------------------

# Addresses DBSCAN's sensitivity to eps parameter
# Creates an ordering of points based on reachability
opticsResult <- optics(shapesMatrix, eps = 1, minPts = 5)

cat("Reachability distances range is from ", 
    round(min(opticsResult$reachdist[is.finite(opticsResult$reachdist)]), 3), "to",
    round(max(opticsResult$reachdist[is.finite(opticsResult$reachdist)]), 3), "\n\n")

reachDf <- data.frame(order = 1:length(opticsResult$order),
  reachability = opticsResult$reachdist[opticsResult$order])

# replace Inf with a large value for plotting
maxReach <- max(reachDf$reachability[is.finite(reachDf$reachability)])
reachDf$reachability[!is.finite(reachDf$reachability)] <- maxReach * 1.1

# from the reachability plot, in the valleys are clear clusters
# the peaks are clear cluster boundaries
pReachability <- ggplot(reachDf, aes(x = order, y = reachability)) +
  geom_bar(stat = "identity", width = 1, fill = "steelblue") +
  geom_hline(yintercept = 0.15, linetype = "dashed", color = "red") +
  annotate("text", x = 100, y = 0.18, label = "eps = 0.15", color = "red") +
  labs(title = "OPTICS Reachability",
       x = "Point Order", y = "Reachability Distance") +
  coord_cartesian(ylim = c(0, 0.5)) +
  theme_minimal() +
  theme(
    legend.position = "right",
    panel.grid = element_blank(),
    panel.border = element_rect(NA, "black", 1)
  )
  
pReachability

# examine two diff epsilon values (cuts)
opticsDbscan015 <- extractDBSCAN(opticsResult, eps_cl = 0.15)
opticsDbscan010 <- extractDBSCAN(opticsResult, eps_cl = 0.10)

shapesDf$opticsCluster015 <- factor(opticsDbscan015$cluster)
shapesDf$opticsCluster010 <- factor(opticsDbscan010$cluster)

# OPTICS results
pOptics015 <- ggplot(shapesDf, aes(x = x, y = y, color = opticsCluster015)) +
  geom_point(size = 1.5, alpha = 0.7) +
  labs(title = "OPTICS (eps = 0.15)",
       subtitle = paste(max(opticsDbscan015$cluster), "clusters"),
       x = "X", y = "Y") +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    panel.border = element_rect(NA, "black", 1)
  )

pOptics010 <- ggplot(shapesDf, aes(x = x, y = y, color = opticsCluster010)) +
  geom_point(size = 1.5, alpha = 0.7) +
  labs(title = "OPTICS (eps = 0.10)",
       subtitle = paste(max(opticsDbscan010$cluster), "clusters"),
       x = "X", y = "Y") +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    panel.border = element_rect(NA, "black", 1)
  )

grid.arrange(pOptics015, pOptics010, ncol = 2)


# HDBSCAN - HIERARCHICAL DBSCAN
# -----------------------------------------------------------------------------

# advantages of using the hierarchical dbscan
# find arbitrary-shaped clusters
# Hierarchical clustering's multi-scale view (think back to first workshop!)

hdbscanResult <- hdbscan(shapesMatrix, minPts = 15)

max(hdbscanResult$cluster) # 7 identified clusters
sum(hdbscanResult$cluster == 0) # 100 identified outliers

# added bonus: assigns probabilistic cluster assignments as well
# (think back to fuzzy c-means from workshop 3)
head(hdbscanResult$membership_prob)

# cluster stability scores (higher = more stable)
print(round(hdbscanResult$cluster_scores, 4))

shapesDf$hdbscanCluster <- factor(hdbscanResult$cluster)
shapesDf$hdbscanProb <- hdbscanResult$membership_prob
shapesDf$hdbscanOutlierScore <- hdbscanResult$outlier_scores

# visualize HDBSCAN clusters
pHdbscan <- ggplot(shapesDf, aes(x = x, y = y, color = hdbscanCluster)) +
  geom_point(size = 1.5, alpha = 0.7) +
  scale_color_manual(values = c("0" = "grey40",
                                setNames(turbo(max(hdbscanResult$cluster)), 
                                         as.character(1:max(hdbscanResult$cluster))))) +
  labs(title = "HDBSCAN Result",
       x = "X", y = "Y", color = "Cluster") +
  theme(
    legend.position = "bottom",
    panel.grid = element_blank(),
    panel.border = element_rect(NA, "black", 1)
  )

pHdbscan

# visualize outlier scores (higher = more likely outlier)
pOutlierScores <- ggplot(shapesDf, aes(x = x, y = y, color = hdbscanOutlierScore)) +
  geom_point(size = 1.5, alpha = 0.8) +
  scale_color_gradient(low = "navy", high = "gold", name = "Outlier\nScore") +
  labs(title = "HDBSCAN Outlier Scores",
       x = "X", y = "Y") +
  theme(
    legend.position = "bottom",
    panel.grid = element_blank(),
    panel.border = element_rect(NA, "black", 1)
  )

pOutlierScores

# visualize cluster hierarchy (think back to workshop 1)
plot(hdbscanResult)

# CLUSTERING PERFORMANCE METRICS
# -----------------------------------------------------------------------------

trueLabels <- as.numeric(shapesDf$trueCluster)

# ARI (as seen in previous workshops)
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

# NMI (as seen in previous workshops)
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


ariKmeans <- computeARI(trueLabels, as.numeric(shapesDf$kmeansCluster))
nmiKmeans <- computeNMI(trueLabels, as.numeric(shapesDf$kmeansCluster))

ariDbscan <- computeARI(trueLabels, as.numeric(shapesDf$dbscanCluster))
nmiDbscan <- computeNMI(trueLabels, as.numeric(shapesDf$dbscanCluster))

ariHdbscan <- computeARI(trueLabels, as.numeric(shapesDf$hdbscanCluster))
nmiHdbscan <- computeNMI(trueLabels, as.numeric(shapesDf$hdbscanCluster))

print(paste0("K-Means: ARI = ", round(ariKmeans,3), ", NMI = ", round(nmiKmeans,3)))
print(paste0("DBSCAN: ARI = ", round(ariDbscan,3), ", NMI = ", round(nmiDbscan,3)))
print(paste0("HDBSCAN: ARI = ", round(ariHdbscan,3), ", NMI = ", round(nmiHdbscan,3)))





# APPLICATION - CHICAGO CRIME HOTSPOT DETECTION
# =============================================================================
# Real GPS data from Chicago crime reports
# Density-based clustering for identifying crime hotspots
# =============================================================================
set.seed(2026)
theme_set(theme_minimal(base_size = 12))

# you can change 5000 in the link below to more (I did up to 20000, or less)
url <- "https://data.cityofchicago.org/resource/ijzp-q8t2.csv?$limit=5000"
crimeRaw <- fread(url)

# variables
print(names(crimeRaw))

# so we are only looking at reported crimes in this data
# and we are interested in determining crime clusters
# therefore, for sake of simplicity, we will only
# be clustering on the gps coordinates of the crime
# GPS coordinates
gps <- crimeRaw[, .(
  lat = as.numeric(latitude),
  lon = as.numeric(longitude),
  primaryType = primary_type,
  description = description,
  locationDesc = location_description,
  arrest = arrest,
  date = date
)]

# lets look at the types of crimes
# we could filter this and focus only on specific
# crime types (like most violent crimes)
table(gps$primaryType)

# remove NA for gps coordinates
gps <- gps[!is.na(lat) & !is.na(lon)]


# EXPLORATORY DATA ANALYSIS
# -----------------------------------------------------------------------------
# distribution of crime types
crimeTypes <- gps %>%
  group_by(primaryType) %>%
  summarise(n = n()) %>%
  arrange(desc(n))
# top 10 crimes
print(head(crimeTypes, 10))

# the data to which we want to cluster
pRawCrime <- ggplot(gps, aes(x = lon, y = lat)) +
  geom_point(alpha = 0.5, size = 1, color = "darkred") +
  coord_fixed(ratio = 1.3) +  # adjust for latitude
  labs(title = "Chicago Crime Locations",
       subtitle = paste(nrow(gps), "incidents"),
       x = "Longitude", y = "Latitude") +
  theme(legend.position = "none") +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(NA, "black", 1))

pRawCrime

# DISTANCE CALCULATION FOR GPS DATA
# -----------------------------------------------------------------------------

# choosing a distance is important, which we have a full workshop
# on coming up

# since gps coordinates are on a spherical earth
# we use the HAVERSINE distance
# for DBSCAN, we'll use scaled coordinates

# at Chicago's latitude (-41.8 degrees north approx), 
# 1 degree latitude is approximately 111 km
# 1 degree longitude is approximately 111*cos(41.8) = 82.5 km

latScale <- 111000  
lonScale <- 111000 * cos(41.8 * pi / 180)  
gps$latScaled <- (gps$lat - min(gps$lat)) * latScale
gps$lonScaled <- (gps$lon - min(gps$lon)) * lonScale
gpsMatrix <- as.matrix(gps[, .(lonScaled, latScaled)])

# K-DISTANCE PLOT FOR EPS SELECTION
# -----------------------------------------------------------------------------
# we will set k higher so we dont have too many small clusters
# only for sake of demonstration/visualization
k <- 10
kdist <- kNNdist(gpsMatrix, k = k)
kdistSorted <- sort(kdist)

# plot k-distance
kdistDf <- data.frame(
  index = 1:length(kdistSorted),
  distance = kdistSorted
)

pKdist <- ggplot(kdistDf, aes(x = index, y = distance)) +
  geom_line(color = "steelblue", linewidth = 0.8) +
  geom_hline(yintercept = 500, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 1000, linetype = "dashed", color = "orange") +
  labs(title = "K-Distance for Chicago Crime Data",
       subtitle = paste0("k = ", k, "; distance in meters"),
       x = "Points (sorted by distance)", y = paste0(k, "-NN Distance (m)")) +
  annotate("text", x = nrow(gps) * 0.7, y = 550, label = "eps = 500m", color = "red") +
  annotate("text", x = nrow(gps) * 0.7, y = 1050, label = "eps = 1000m", color = "orange") +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(NA, "black", 1))

pKdist
# here it seems eps = 1000m is a good start
# feel free to play around with it

# DBSCAN CLUSTERING
# -----------------------------------------------------------------------------
epsMeters <- 1000
minPts <- 10 # we are going 

dbscanResult <- dbscan::dbscan(gpsMatrix, eps = epsMeters, minPts = minPts)

max(dbscanResult$cluster) # 8 clusters
sum(dbscanResult$cluster == 0) # 110 noise points

print(table(dbscanResult$cluster))

gps$dbscanCluster <- factor(dbscanResult$cluster)

nClusters <- max(dbscanResult$cluster)
clusterColors <- c("0" = "grey60", setNames(turbo(nClusters), as.character(1:nClusters)))

pDbscan <- ggplot(gps, aes(x = lon, y = lat, color = dbscanCluster)) +
  geom_point(alpha = 0.7, size = 1) +
  scale_color_manual(values = clusterColors) +
  coord_fixed(ratio = 1.3) +
  labs(title = "DBSCAN Crime Hotspots",
       x = "Longitude", y = "Latitude", color = "Cluster") +
  theme(legend.position = "bottom") +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(NA, "black", 1))

pDbscan
### interesting it has the core area then we
# start to see little pockets in what we might deem the suburbs.
# it would be interesting to compare this to a map
# and examine these suburbs
# could it be the commercial centers of suburbs towns?

# EFFECT OF EPS PARAMETER
# -----------------------------------------------------------------------------
epsValues <- c(150, 500, 750, 1000, 1500)
epsPlots <- list()

for (i in seq_along(epsValues)) {
  eps <- epsValues[i]
  dbTemp <- dbscan::dbscan(gpsMatrix, eps = eps, minPts = minPts)
  gps$tempCluster <- factor(dbTemp$cluster)
  nCl <- max(dbTemp$cluster)
  nNoise <- sum(dbTemp$cluster == 0)
  tempColors <- c("0" = "grey60")
  if (nCl > 0) {
    tempColors <- c(tempColors, setNames(turbo(nCl), as.character(1:nCl)))
  }
  
  epsPlots[[i]] <- ggplot(gps, aes(x = lon, y = lat, color = tempCluster)) +
    geom_point(alpha = 0.7, size = 0.5) +
    scale_color_manual(values = tempColors) +
    coord_fixed(ratio = 1.3) +
    labs(title = paste0("eps = ", eps, "m")) +
    theme(legend.position = "none",
          axis.title = element_blank(),
          plot.title = element_text(size = 11),
          plot.subtitle = element_text(size = 9)) +
    theme(panel.grid = element_blank(),
          panel.border = element_rect(NA, "black", 1))
}

grid.arrange(grobs = epsPlots, ncol = 5)

# HDBSCAN CLUSTERING
# -----------------------------------------------------------------------------
#
hdbscanResult <- hdbscan(gpsMatrix, minPts = minPts)

max(hdbscanResult$cluster) # a lot with hdbscan
sum(hdbscanResult$cluster == 0) 
  
print(table(hdbscanResult$cluster))

gps$hdbscanCluster <- factor(hdbscanResult$cluster)
gps$outlierScore <- hdbscanResult$outlier_scores
gps$membershipProb <- hdbscanResult$membership_prob

nClH <- max(hdbscanResult$cluster)
clusterColorsH <- c("0" = "grey60")
if (nClH > 0) {
  clusterColorsH <- c(clusterColorsH, setNames(turbo(nClH), as.character(1:nClH)))
}

pHdbscan <- ggplot(gps, aes(x = lon, y = lat, color = hdbscanCluster)) +
  geom_point(alpha = 0.7, size = 1) +
  scale_color_manual(values = clusterColorsH) +
  coord_fixed(ratio = 1.3) +
  labs(title = paste0("HDBSCAN Crime Hotspots"),
       x = "Longitude", y = "Latitude", color = "Cluster") +
  theme(legend.position = "none") +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(NA, "black", 1))

pHdbscan

# outlier scores
pOutlier <- ggplot(gps, aes(x = lon, y = lat, color = outlierScore)) +
  geom_point(alpha = 0.7, size = 1) +
  scale_color_gradient(low = "darkgreen", high = "red", name = "Outlier\nScore") +
  coord_fixed(ratio = 1.3) +
  labs(title = "HDBSCAN Outlier Scores",
       subtitle = "Red = isolated incidents?, Green = within hotspots?",
       x = "Longitude", y = "Latitude") +
  theme(legend.position = "bottom") +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(NA, "black", 1))

pOutlier

# HOTSPOT CHARACTERIZATION
# -----------------------------------------------------------------------------

# analyze crime types by cluster
hotspotAnalysis <- gps %>%
  filter(hdbscanCluster != 0) %>%
  group_by(hdbscanCluster) %>%
  summarise(
    nCrimes = n(),
    centerLat = mean(lat),
    centerLon = mean(lon),
    radiusM = max(sqrt((latScaled - mean(latScaled))^2 + (lonScaled - mean(lonScaled))^2)),
    topCrimeType = names(sort(table(primaryType), decreasing = TRUE))[1],
    arrestRate = round(mean(arrest == TRUE, na.rm = TRUE),3),
    .groups = "drop"
  )


print(as.data.frame(hotspotAnalysis))

# COMPARISON OF METHODS
# -----------------------------------------------------------------------------

pComparison <- grid.arrange(
  pDbscan + ggtitle("DBSCAN") + theme(legend.position = "none"),
  pHdbscan + ggtitle("HDBSCAN") + theme(legend.position = "none"),
  ncol = 2,
  top = "Crime Hotspot Detection: DBSCAN vs HDBSCAN"
)

# agreement between methods
agreementTable <- table(DBSCAN = gps$dbscanCluster == 0, HDBSCAN = gps$hdbscanCluster == 0)
print(agreementTable)

agreementRate <- sum(diag(agreementTable)) / sum(agreementTable)
round(agreementRate * 100, 1) # agreement rate (%)

