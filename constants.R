
library("maps")

MAP.DATA <- map_data("world")

LANG.TO.STATE <- read.delim("languages-to-states.txt", header = FALSE, sep = ":", col.names = c("lang.code", "region"))

CENTROIDS <- aggregate(cbind(long, lat) ~ region, data=MAP.DATA, FUN=function(x) mean(range(x)))
rownames(CENTROIDS) <- CENTROIDS$region
CENTROIDS <- CENTROIDS[as.character(LANG.TO.STATE$region), ]
CENTROIDS <- set.russian.label.location(CENTROIDS)

DESCRIPTION <- "<div style='font-size: small; color: gray'>
<div align='center' style='margin-top: 10px; margin-bottom: 5px'>- - -</div>
What does it do? It takes a title of an English Wikipedia article,
 searches for its language variations and measures their length in characters. Each state on the map is assigned the language with
 the most speakers there, with color representing the coverage of the topic. Feel free to give it a try!
 For more information, head to <a href='https://github.com/havranej/world-of-wiki' target='_blank' style='color: #404040'>GitHub</a>.
</div>"