# Script: Mapa do diplomky
# - Map for diploma thesis

# LOAD PACKAGES----------
library(tidyverse)
library(ggmap)
library(ggrepel)
library(xtable)

citation("ggmap")

# #  Repair ggmap - updated from Github, need v7
# install.packages("devtools")
# devtools::install_github("dkahle/ggmap")
# 
# # READ DATA---------------
# Lok <- read_csv("../Db.Lok.csv")
# Lok <- Lok[order(Lok$Name),]
# # Lok <- subset(Lok, select = -c(5,6))
# 
# Lok <- cbind(Lok, sumStatLok$n, sumStatLok$`sum(Mas)`)
# 
# # GEOCODE SITES-----------
# LatLon <- geocode(Lok$Address, source = "dsk")
# Lok <- cbind(Lok, LatLon)

# LOAD DATA--------
# write_csv(Lok, path = "../Db.Lok.FromR.csv")
Lok <- read_csv("../Db.Lok.FromR.csv")

TabLok <- subset(Lok, select = c(Name, Type, n))
TabLok <- TabLok[order(Lok$Num),]
TabLokExp <- xtable(TabLok, caption = "Přehled lokalit, viz Obr. `\ref{ObrMapaLok}`, zkratky viz Tab. `\ref{TabKod}`")
print(TabLokExp, file = "../../Text/2Kap/TabLok.tex", booktabs = T, caption.placement = "top")

# BOUNDING BOX
Lok.bbox <-  make_bbox(lon = Lok$lon, lat = Lok$lat, f = 0.2)
Eur.bbox <- c(left = -12, bottom = 35, right = 30, top = 63)

LokMap <- get_map(location = Lok.bbox, maptype = "terrain-background", source = "stamen")
EurMap <- get_map(location = Eur.bbox, maptype = "terrain-background", source = "stamen")

save(LokMap, file = "./Archive/Map.CZ")
save(EurMap, file = "./Archive/Map.Eu")
# stamen: toner-lite toner-background

# MAPA LOKALITY-----------
# ggmap(LokMap) +
#   geom_point(data = Lok, mapping = aes(x = lon, y = lat, shape = factor(Type)), na.rm = FALSE) +
#   scale_shape(solid = FALSE) +
#   geom_text(data = Lok, aes(label = paste("  ", as.character(Num), sep="")), hjust = 0, vjust = 0, size = 3)

FinMap <- ggmap(LokMap) +
  geom_point(data = Lok, mapping = aes(x = lon, y = lat, shape = factor(Type)), na.rm = FALSE) +
  scale_shape(solid = FALSE, labels=c("Poh.", "Dep.", "Síd.", "Zdroj sur.")) +
  geom_text_repel(data = Lok, aes(label = Num), size = 2.8, fontface = "bold") +
  theme(legend.position = c(0.1, 0.16), legend.title = element_blank())

pdf(file = "Mapa.Lok.pdf", height = 5)
FinMap
dev.off()

# MAPA EUR-----
xx <- cbind(c(12, 12, 19, 19), c(48, 60, 60, 48))
colnames(xx) <- c("lon", "lat")

FinEur <- ggmap(EurMap) +
  annotate("rect", xmin = 12, xmax = 19, ymin = 48, ymax = 51.2, colour = "black", fill = NA, alpha = .3)

pdf(file = "Mapa.Eur.pdf")
FinEur
dev.off()
