
MAP.DATA <- map_data("world")

LANG.TO.STATE <- read.delim("languages-to-states.txt", header = FALSE, sep = ":", col.names = c("lang.code", "region"))

CENTROIDS <- aggregate(cbind(long, lat) ~ region, data=MAP.DATA, FUN=function(x) mean(range(x)))
rownames(CENTROIDS) <- CENTROIDS$region
CENTROIDS <- CENTROIDS[as.character(LANG.TO.STATE$region), ]
CENTROIDS <- set.russian.label.location(CENTROIDS)
