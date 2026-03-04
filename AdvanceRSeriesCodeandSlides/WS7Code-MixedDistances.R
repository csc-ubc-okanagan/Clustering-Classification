# MIXED-TYPE DATA QUANTIFICATION
# Workshop 7: Distances and Similarities for Mixed-Type Data
library(cluster)
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
library(viridis)
library(kmed)
library(maps)
library(ggrepel)
library(ggpubr)

set.seed(2026)
theme_set(theme_minimal(base_size=12))
# THE PROBLEM: REAL DATA IS MIXED-TYPE
# so far we have clustered continuous (numeric) data
# but real datasets often contain:
# - continuous variables (age, income, temperature)
# - categorical/nominal variables (colour, country, diagnosis)
# - ordinal variables (education level, satisfaction rating)
# - binary variables (yes/no, true/false)
# how do we compute distances when variables are on different scales and types?

# SIMULATED MIXED-TYPE DATA
# =============================================================================
n <- 150
mixedDf <- data.frame(
  age = round(rnorm(n,45,15)),
  income = round(rlnorm(n,10.5,0.8)),
  yearsExperience = pmax(0, round(rnorm(n,15,8))),
  education = factor(sample(c("HighSchool","Bachelors","Masters","PhD"), n, 
                            replace=TRUE, prob=c(0.3,0.4,0.2,0.1)), 
                     levels=c("HighSchool","Bachelors","Masters","PhD"), ordered=TRUE),
  region = factor(sample(c("North","South","East","West"), n, replace=TRUE)),
  employed = sample(c(TRUE,FALSE), n, replace=TRUE, prob=c(0.7,0.3)),
  satisfaction = factor(sample(1:5,n,replace=TRUE), levels=1:5, ordered=TRUE),
  sector = factor(sample(c("Tech","Finance","Healthcare","Retail","Education"), n, replace=TRUE)))
# artificial cluster labels
mixedDf$trueCluster <- factor(ifelse(mixedDf$income>50000 & mixedDf$age>40, 1,
                                     ifelse(mixedDf$education %in% c("Masters","PhD") & mixedDf$employed, 2, 3)))
head(mixedDf)
str(mixedDf)

# PART 1: NAIVE APPROACHES / LIMITATIONS
# =============================================================================
# APPROACH 1: TREAT EVERYTHING AS NUMERIC 
naiveNumeric <- mixedDf %>%
  mutate(education=as.numeric(education), region=as.numeric(region),
         employed=as.numeric(employed), satisfaction=as.numeric(satisfaction),
         sector=as.numeric(sector)) %>%
  select(-trueCluster)
# the scale problem
pScaleProblem <- naiveNumeric %>%
  pivot_longer(everything(), names_to="variable", values_to="value") %>%
  ggplot(aes(x=reorder(variable,value,median), 
             y=value + min(naiveNumeric[naiveNumeric > 0], na.rm = TRUE) / 2)) +
  geom_boxplot(fill="steelblue", alpha=0.7) +
  scale_y_log10() +
  labs(title="The Scale Problem", x="", y="log(Value)") +
  theme(panel.grid=element_blank(), panel.border=element_rect(NA,"black",1),
        axis.text.x=element_text(angle=45,hjust=1))
pScaleProblem

# and if we calculate distance, clearly income takes over.
distRaw <- dist(naiveNumeric, method="euclidean")
summary(as.vector(distRaw))

# APPROACH 2: Z-SCORE STANDARDIZATION
# subtract mean, divide by sd -> mean=0, sd=1
zScoreDf <- naiveNumeric %>% mutate(across(everything(), ~(.-mean(.))/sd(.)))
round(colMeans(zScoreDf), 10)
round(apply(zScoreDf,2,sd), 10)
distZscore <- dist(zScoreDf, method="euclidean")
pZscore <- zScoreDf %>%
  pivot_longer(everything(), names_to="variable", values_to="value") %>%
  ggplot(aes(x=variable, y=value)) +
  geom_boxplot(fill="forestgreen", alpha=0.7) +
  labs(title="Z-Score Standardization", x="Variable", y="Standardized Value") +
  theme(panel.grid=element_blank(), panel.border=element_rect(NA,"black",1),
        axis.text.x=element_text(angle=45,hjust=1))
pZscore

# APPROACH 3: MIN-MAX SCALING
# scale to [0,1] range
minMaxDf <- naiveNumeric %>% 
  mutate(across(everything(), ~(.-min(.))/(max(.)-min(.))))
distMinMax <- dist(minMaxDf, method="euclidean")
pMinMax <- minMaxDf %>%
  pivot_longer(everything(), names_to="variable", values_to="value") %>%
  ggplot(aes(x=variable, y=value)) +
  geom_boxplot(fill="darkorange", alpha=0.7) +
  labs(title="Min-Max", x="", y="Scaled Value") +
  theme(panel.grid=element_blank(), panel.border=element_rect(NA,"black",1),
        axis.text.x=element_text(angle=45,hjust=1))

grid.arrange(pScaleProblem, pZscore, pMinMax, ncol=3)

# APPROACH 4: DUMMY/ONE-HOT ENCODING
# convert categorical variables to binary indicators
dummyDf <- model.matrix(~.-1, data=mixedDf %>% select(-trueCluster))
head(dummyDf)
dim(dummyDf)
# we went from 8 variables to many more
cat("Original variables:", ncol(mixedDf)-1, "\n")
cat("After dummy encoding:", ncol(dummyDf), "\n")
dummyScaled <- scale(dummyDf)
distDummy <- dist(dummyScaled, method="euclidean")

# LIMITATIONS OF HOMOGENIZATION
# LIMITATION 1
# Ordinal variables lose ordering information with dummy coding
# education: HighSchool < Bachelors < Masters < PhD
# with dummy coding, distance(HighSchool, Bachelors) = distance(HighSchool, PhD)
eduDummy <- model.matrix(~education-1, data=mixedDf)
head(eduDummy)

# LIMITATION 2
# Nominal variables get artificial ordering when converted to numeric
# region: North=1, South=2, East=3, West=4
# this implies West > East > South > North, which is meaningless...
regionNumeric <- as.numeric(mixedDf$region)
print(table(mixedDf$region, regionNumeric))
# distance calculations dont make sense...
# d(North, South) = |1-2| = 1
# d(North, West) = |1-4| = 3
# but there's no reason West should be 3x further from North than South is..

# LIMITATION 3
# Binary variables may need asymmetric treatment
# employed: TRUE/FALSE
# if we're clustering job seekers, joint unemployment might be informative
# if we're clustering the general population, it might not be

# LIMITATION 4
# Z-score assumes normality, outliers affect both methods
incomeSkew <- round(moments::skewness(mixedDf$income), 2)
cat("\nIncome skewness:", incomeSkew, "(highly right-skewed)\n")

pIncomeHist <- ggplot(mixedDf, aes(x=income)) +
  geom_histogram(bins=30, fill="steelblue", alpha=0.7, color="white") +
  labs(title="Income Distribution", x="Income", y="Count") +
  theme(panel.grid=element_blank(), panel.border=element_rect(NA,"black",1))
pIncomeHist

# LIMITATION 5 
# Equal weighting may not reflect variable importance
# is age as important as income? is region as important as education?

# Also, what happens when we plot the distances against each other?
# If two distances captures the same "distances" then when we plot against
# each other, the observations should be along the main diagonal
plotDistCompare <- function(d1, d2, name1="D1", name2="D2", logAx=FALSE) {
  stopifnot(length(d1) == length(d2))
  df <- tibble(x = as.vector(d1), y = as.vector(d2))
  p <- ggplot(df, aes(x = x, y = y)) +
    geom_point(alpha = 0.25, size = 1) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
    labs(title = paste(name2, "vs", name1), x = name1, y = name2) +
    theme_minimal() + 
    theme(panel.grid=element_blank(), panel.border=element_rect(NA,"black",1),
          axis.text.x=element_text(angle=45,hjust=1))
  if (logAx) p <- p + scale_x_log10() + scale_y_log10()
  p
}

pRawZ <- plotDistCompare(scale(distRaw), scale(distZscore), "Raw", "Z-score", logAx=FALSE)
pRawMM <- plotDistCompare(scale(distRaw), scale(distMinMax), "Raw", "Min-Max", logAx=FALSE)
pZMM <- plotDistCompare(scale(distZscore), scale(distMinMax), "Z-score", "Min-Max", logAx=FALSE)
pRawD <- plotDistCompare(scale(distRaw), scale(distDummy), "Raw", "Dummy Coding", logAx=FALSE)
pZD <- plotDistCompare(scale(distZscore), scale(distDummy), "Z-score", "Dummy Coding", logAx=FALSE)
pMMD <- plotDistCompare(scale(distMinMax), scale(distDummy), "Min-Max", "Dummy Coding", logAx=FALSE)

ggarrange(pRawZ, pRawMM, pZMM, pRawD, pZD, pMMD, ncol = 3, nrow = 2)

# PART 2: MIXED-TYPE DISTANCE MEASURES
# =============================================================================
# GOWER DISTANCE
# continuous: |x_i - x_j| / range(x) 
# ordinal: same as continuous applied to ranks
# nominal: 0 if same, 1 if different
# binary: 0 if same, 1 if different (or asymmetric)
# final distance: weighted average of components
# d_ij = sum_k(w_k * d_ijk) / sum_k(w_k * delta_ijk)
# where delta_ijk = 1 if comparison is valid, 0 if missing

mixedForDist <- mixedDf %>% select(-trueCluster)
gowerDist <- daisy(mixedForDist, metric="gower")
summary(gowerDist)

# MANUAL GOWER CALCULATION 
obs1 <- mixedForDist[1,]
obs2 <- mixedForDist[2,]

# continuous 
ageRange <- max(mixedForDist$age)-min(mixedForDist$age)
incomeRange <- max(mixedForDist$income)-min(mixedForDist$income)
expRange <- max(mixedForDist$yearsExperience)-min(mixedForDist$yearsExperience)
dAge <- abs(obs1$age-obs2$age)/ageRange
dIncome <- abs(obs1$income-obs2$income)/incomeRange
dExp <- abs(obs1$yearsExperience-obs2$yearsExperience)/expRange
# ordinal 
eduRange <- length(levels(mixedForDist$education))-1
satRange <- length(levels(mixedForDist$satisfaction))-1
dEducation <- abs(as.numeric(obs1$education)-as.numeric(obs2$education))/eduRange
dSatisfaction <- abs(as.numeric(obs1$satisfaction)-as.numeric(obs2$satisfaction))/satRange
# nominal 
dRegion <- as.numeric(obs1$region!=obs2$region)
dSector <- as.numeric(obs1$sector!=obs2$sector)
# binary 
dEmployed <- as.numeric(obs1$employed!=obs2$employed)
# final Gower's distance
components <- c(dAge,dIncome,dExp,dEducation,dRegion,dEmployed,dSatisfaction,dSector)
manualGower <- mean(components)

cat("Manual Gower:", round(manualGower,4), "\n")
cat("daisy Gower:", round(as.matrix(gowerDist)[1,2],4), "\n")

# VISUALIZING DISTANCE MATRICES 
plotDistMatrix <- function(distMat, clusterLabels, title) {
  sortOrder <- order(clusterLabels)
  sortedMat <- distMat[sortOrder, sortOrder]
  sortedClusters <- clusterLabels[sortOrder]
  nObs <- nrow(sortedMat)
  dfLong <- expand.grid(row=1:nObs, col=1:nObs)
  dfLong$dist <- as.vector(sortedMat)
  dfLong$cluster1 <- sortedClusters[dfLong$row]
  dfLong$cluster2 <- sortedClusters[dfLong$col]
  clusterBreaks <- cumsum(table(sortedClusters))
  p <- ggplot(dfLong, aes(x=col, y=row, fill=dist)) +
    geom_raster() +
    scale_fill_viridis(option="cividis", name="Distance") +
    geom_vline(xintercept=clusterBreaks+0.5, color="white", linewidth=0.5) +
    geom_hline(yintercept=clusterBreaks+0.5, color="white", linewidth=0.5) +
    scale_y_reverse() +
    coord_fixed() +
    labs(title=title, x="", y="") +
    theme(panel.grid=element_blank(), panel.border=element_rect(NA,"black",1),
          axis.text=element_blank(), axis.ticks=element_blank(),
          legend.position = "bottom")
  return(p)
}

gowerMat <- as.matrix(gowerDist)
pGowerSorted <- plotDistMatrix(gowerMat, mixedDf$trueCluster, "Gower Distance")
pGowerSorted
# in the three different sized squares along the main diagonal,
# we'd hope to see darker because these are the artificial KNOWN clusters 
# compare with euclidean on scaled data
euclideanMat <- as.matrix(distMinMax)
pEuclideanSorted <- plotDistMatrix(euclideanMat, mixedDf$trueCluster, 
                                   "Euclidean Min-Max")
ggarrange(pGowerSorted, pEuclideanSorted, ncol = 2, common.legend = T, legend = "right")
# not so great as we see more dark colour in the clusters
# on the euclidean distance plot... we will come back to this


# PART 3: ALTERNATIVE MIXED-TYPE DISTANCES 
# =============================================================================
# the kmed package provides distmix() with multiple mixed type methods
# type ?kmed::distmix in the console to view the help file :D
# gower: standard Gower distance
# podani: Podani's extension (better ordinal handling)
# wishart: Wishart's distance (better continuous handling, arguably)
# huang: Huang's distance (used in k-prototypes, which we talk about next workshop)
# more...
mixedMatrix <- mixedForDist %>%
  mutate(education=as.numeric(education), satisfaction=as.numeric(satisfaction),
         region=as.numeric(region), sector=as.numeric(sector),
         employed=as.numeric(employed)) %>%
  as.data.frame()
# column indices
idnum <- c(1,2,3,4,7)  
idbin <- c(6)          
idcat <- c(5,8)   

mixedMatrixSc <- mixedMatrix
for(i in 1:length(idnum)) mixedMatrixSc[,idnum[i]] <- scale(mixedMatrixSc[,idnum[i]])

distGowerKmed <- distmix(mixedMatrix, method="gower", idnum=idnum, idbin=idbin, idcat=idcat)
distPodani <- distmix(mixedMatrix, method="podani", idnum=idnum, idbin=idbin, idcat=idcat)
distWishart <- distmix(mixedMatrix, method="wishart", idnum=idnum, idbin=idbin, idcat=idcat)
# need to scale for Huang method 
distHuang <- distmix(mixedMatrixSc, method="huang", idnum=idnum, idbin=idbin, idcat=idcat)

distComparison <- data.frame(gower = as.vector(distGowerKmed),
  podani = as.vector(distPodani), wishart = as.vector(distWishart),
  huang = as.vector(distHuang))
# correlation between methods
corrplot::corrplot(cor(distComparison), method = "shade", type = "lower", tl.col = "black")
# visualize distributions
pDistMethods <- distComparison %>%
  pivot_longer(everything(), names_to="method", values_to="distance") %>%
  ggplot(aes(x=distance, fill=method)) +
  geom_density(alpha=0.5) +
  scale_fill_viridis_d() +
  facet_wrap(~method, scales="free") +
  labs(title="Distribution of Distances by Method", x="Distance", y="Density") +
  theme(legend.position="none", panel.grid=element_blank(),
        panel.border=element_rect(NA,"black",1))
pDistMethods

pGowerKmed <- plotDistMatrix(as.matrix(distGowerKmed), mixedDf$trueCluster, "Gower")
pPodani <- plotDistMatrix(as.matrix(distPodani), mixedDf$trueCluster, "Podani")
pWishart <- plotDistMatrix(as.matrix(distWishart), mixedDf$trueCluster, "Wishart")
pHuang <- plotDistMatrix(as.matrix(distHuang), mixedDf$trueCluster, "Huang")
ggarrange(pGowerKmed, pPodani, pWishart, pHuang, ncol = 4, common.legend = T, legend = "bottom")

GP <- plotDistCompare(scale(as.matrix(distGowerKmed)), scale(as.matrix(distPodani)), "Gower", "Podani", logAx=FALSE)
GW <- plotDistCompare(scale(as.matrix(distGowerKmed)), scale(as.matrix(distWishart)), "Gower", "Wishart", logAx=FALSE)
GH <- plotDistCompare(scale(as.matrix(distGowerKmed)), scale(as.matrix(distHuang)), "Gower", "Huang", logAx=FALSE)
PW <- plotDistCompare(scale(as.matrix(distPodani)), scale(as.matrix(distWishart)), "Podani", "Wishart", logAx=FALSE)
PH <- plotDistCompare(scale(as.matrix(distPodani)), scale(as.matrix(distHuang)), "Podani", "Huang", logAx=FALSE)
WH <- plotDistCompare(scale(as.matrix(distWishart)), scale(as.matrix(distHuang)), "Wishart", "Huang", logAx=FALSE)
ggarrange(GP, GW, GH, PW, PH, WH, ncol = 3, nrow = 2)


# PART 4: WEIGHTED DISTANCES
# =============================================================================
# not all variables are equally important
# we can assign weights to reflect domain knowledge
# example: income and education are most important
weightsImportant <- c(age=0.5, income=2, yearsExperience=0.5, 
                      education=2, region=0.5, employed=1, satisfaction=1, sector=0.5)
gowerWeighted <- daisy(mixedForDist, metric="gower", weights=weightsImportant)

# compare weighted vs unweighted
pWeightCompare <- data.frame(
  unweighted = as.vector(gowerDist),
  weighted = as.vector(gowerWeighted)) %>%
  ggplot(aes(x=unweighted, y=weighted)) +
  geom_point(alpha=0.3, size=1) +
  geom_abline(slope=1, intercept=0, linetype="dashed", color="red") +
  labs(x="Unweighted Gower", y="Weighted Gower") +
  theme(panel.grid=element_blank(), panel.border=element_rect(NA,"black",1))
pWeightCompare

pUnweighted <- plotDistMatrix(as.matrix(gowerDist), mixedDf$trueCluster, "Unweighted")
pWeighted <- plotDistMatrix(as.matrix(gowerWeighted), mixedDf$trueCluster, "Weighted")
ggarrange(pUnweighted, pWeighted, ncol=2, common.legend = T, legend = "right")

# there's other measures we should consider... here's just one

# PART 5: LAT/LONG DATA
# =============================================================================
# why Euclidean distance fails for geographic coordinates
airportUrl <- "https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat"
airportRaw <- read.csv(airportUrl, header = FALSE, stringsAsFactors = FALSE)
colnames(airportRaw)[1:8] <- c("airportId","name","city","country",
                               "iata","icao","lat","lon")
targetCountries <- c("United States", "Canada")
cities <- airportRaw %>%
  filter(country %in% targetCountries) %>%
  select(city, lat, lon) 
cities <- cities[!duplicated(cities$city), ]
nCities <- nrow(cities)
highlightCities <- c("Vancouver","Toronto","Miami","Seattle","NewYork",
                     "LosAngeles", "Anchorage","Honolulu","Denver","Chicago")
cities$highlight <- cities$city %in% highlightCities

naMap <- map_data("world", region=c("USA","Canada"))
pTrueMap <- ggplot() +
  geom_polygon(data=naMap, aes(x=long, y=lat, group=group), 
               fill="grey90", color="grey70", linewidth=0.3) +
  geom_point(data=cities, aes(x=lon, y=lat), 
             color="steelblue", size=1.5, alpha=0.7) +
  geom_point(data=cities %>% filter(highlight), aes(x=lon, y=lat),
             color="firebrick", size=3) +
  geom_text_repel(data=cities %>% filter(highlight), aes(x=lon, y=lat, label=city),
                  size=3, color="firebrick", max.overlaps=20) +
  coord_fixed(ratio=1.3, xlim=c(-170,-60), ylim=c(20,70)) +
  labs(x="Longitude", y="Latitude") +
  theme(panel.grid=element_blank(), panel.border=element_rect(NA,"black",1),
        panel.background=element_rect(fill="lightblue"))
pTrueMap
# interestingly, even displaying the above plot on a two-D map such as that was incorrect
# why? the earth is a sphere... 
# at equator: 1 degree longitude is approx 111 km
# at 45 degree latitude: 1 degree longitude is approx 78 km  
# at 60 degree latitude: 1 degree longitude is approx 55 km
# Euclidean treats all degrees as equal...

# the best distance for handling geographic coordinates is the Haversine distance
haversine <- function(lat1, lon1, lat2, lon2) {
  R <- 6371
  dLat <- (lat2-lat1)*pi/180
  dLon <- (lon2-lon1)*pi/180
  lat1r <- lat1*pi/180
  lat2r <- lat2*pi/180
  a <- sin(dLat/2)^2+cos(lat1r)*cos(lat2r)*sin(dLon/2)^2
  c <- 2*atan2(sqrt(a), sqrt(1-a))
  return(R*c)
}

haversineMat <- matrix(0, nCities, nCities)
for (i in 1:nCities) {
  for (j in 1:nCities) {
    haversineMat[i,j] <- haversine(cities$lat[i], cities$lon[i],
                                   cities$lat[j], cities$lon[j])
  }
}
rownames(haversineMat) <- colnames(haversineMat) <- cities$city
# we will compare to the euclidean distance
euclideanMat <- as.matrix(dist(cities[,c("lat","lon")]))
rownames(euclideanMat) <- colnames(euclideanMat) <- cities$city
# lets examine some cases here...
pairs <- list(
  c("Vancouver","Seattle"),    # both high latitude, close
  c("Vancouver","Toronto"),    # both high latitude, far east-west
  c("Miami","Houston"),        # both low latitude
  c("Anchorage","Miami"),      # very far apart in lat/long
  c("New York","Los Angeles")) # cross-country

for (p in pairs) {
  hav <- haversineMat[p[1],p[2]]
  euc <- euclideanMat[p[1],p[2]]
  cat(sprintf("%s to %s:\n", p[1], p[2]))
  cat(sprintf(" Haversine: %d km\n", round(hav)))
  cat(sprintf(" Euclidean: %.2f (raw lat/lon units)\n", euc))
  cat(sprintf(" Ratio (Hav/Euc): %.2f km per unit\n\n", hav/euc))
}
# the ratio varies because longitude degrees shrink at high latitudes
# even though the units are off, if the measures were comparable, the ratios
# would be about the same. Clearly not...

mdsHav <- cmdscale(haversineMat, k=2)
mdsEuc <- cmdscale(euclideanMat, k=2)
cities$mdsHavX <- mdsHav[,1]
cities$mdsHavY <- mdsHav[,2]
cities$mdsEucX <- mdsEuc[,1]
cities$mdsEucY <- mdsEuc[,2]

highlightIdx <- which(cities$highlight)
highlightPairs <- expand.grid(i=highlightIdx, j=highlightIdx) %>% filter(i<j)
linesDf <- data.frame(
  city1 = cities$city[highlightPairs$i],
  city2 = cities$city[highlightPairs$j],
  x1_true = cities$lon[highlightPairs$i],
  y1_true = cities$lat[highlightPairs$i],
  x2_true = cities$lon[highlightPairs$j],
  y2_true = cities$lat[highlightPairs$j],
  x1_hav = cities$mdsHavX[highlightPairs$i],
  y1_hav = cities$mdsHavY[highlightPairs$i],
  x2_hav = cities$mdsHavX[highlightPairs$j],
  y2_hav = cities$mdsHavY[highlightPairs$j],
  x1_euc = cities$mdsEucX[highlightPairs$i],
  y1_euc = cities$mdsEucY[highlightPairs$i],
  x2_euc = cities$mdsEucX[highlightPairs$j],
  y2_euc = cities$mdsEucY[highlightPairs$j])

linesDf$trueDist <- haversineMat[cbind(highlightPairs$i, highlightPairs$j)]
# compare pairwise distance rankings
distCompare <- data.frame(
  pair = paste(linesDf$city1, "-", linesDf$city2),
  haversine = linesDf$trueDist,
  euclidean = euclideanMat[cbind(
    match(linesDf$city1, cities$city),
    match(linesDf$city2, cities$city))])

distCompare$havNorm <- distCompare$haversine/max(distCompare$haversine)
distCompare$eucNorm <- distCompare$euclidean/max(distCompare$euclidean)

pDistortionScatter <- ggplot(distCompare, aes(x=eucNorm, y=havNorm)) +
  geom_point(size=3, alpha=0.7, color="steelblue") +
  geom_abline(slope=1, intercept=0, linetype="dashed", color="red") +
  geom_text_repel(aes(label=pair), size=2.5, max.overlaps=15) +
  labs(title="Distance Distortion: Euclidean vs Haversine",
       subtitle="Points above line: Euclidean underestimates true distance",
       x="Euclidean (normalized)", y="Haversine (normalized)") +
  theme(panel.grid=element_blank(), panel.border=element_rect(NA,"black",1))
pDistortionScatter


round(cor(distCompare$euclidean, distCompare$haversine), 3)
# but correlation hides the problem - look at specific distortions
distCompare$distortion <- distCompare$havNorm/distCompare$eucNorm
distCompare <- distCompare %>% arrange(desc(distortion))
# most distorted pairs
print(head(distCompare[,c("pair","distortion")], 10))

