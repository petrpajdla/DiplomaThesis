# INDIVIDUAL SITES
# - PCA and clustering for individual sites

# LIBRARIES ====================================================================
library(Momocs)

# DATA =========================================================================
# - load data, after elliptical Fourier transform
load("./Archive/Adzes/Dt.efProf")
load("./Archive/Adzes/Dt.efSide")
load("./Archive/Adzes/Dt.efTop")

# filter (need to unload tidyverse!)
VedProf <- filter(efProf, Lok == "Ved")
KraProf <- filter(efProf, Lok == "Kra")
KleProf <- filter(efProf, Lok == "Kle")
NitProf <- filter(efProf, Lok == "Nit")

VedSide <- filter(efSide, Lok == "Ved")
KraSide <- filter(efSide, Lok == "Kra")
KleSide <- filter(efSide, Lok == "Kle")
NitSide <- filter(efSide, Lok == "Nit")

VedTop <- filter(efTop, Lok == "Ved")
KraTop <- filter(efTop, Lok == "Kra")
KleTop <- filter(efTop, Lok == "Kle")
NitTop <- filter(efTop, Lok == "Nit")

# PCA
pcaLok <- function(pcaSlot, Type, LokName) {
  plot(PCA(pcaSlot), pos.shp = "xy", size.shp = 0.5, 
       density = T, chull = F, chull.filled = F,
       title = sprintf("PCA %s: %s", Type, LokName), unit = T)
  }

pcaLok(VedProf, "prof", "Ved")
pcaLok(KraProf, "prof", "Kra")
pcaLok(KleProf, "prof", "Kle")
pcaLok(NitProf, "prof", "Nit")

pcaLok(VedSide, "side", "Ved")
pcaLok(KraSide, "side", "Kra")
pcaLok(KleSide, "side", "Kle")
pcaLok(NitSide, "side", "Nit")

pcaLok(VedTop, "top", "Ved")
pcaLok(KraTop, "top", "Kra")
pcaLok(KleTop, "top", "Kle")
pcaLok(NitTop, "top", "Nit")

# Clustering
clustLok <- function(Slot) {
  CLUST(PCA(Slot), type = "phylogram", 
        dist_method = "maximum", hclust_method = "ward.D2")
}

pdf(file = "./Adzes/TreeVedProf.wardD2.pdf")
CLUST(PCA(VedProf), type = "phylogram", dist_method = "maximum", hclust_method = "ward.D2") 
  rect(xleft = 4.5, ybottom = 17.6, xright = 5.8, ytop = 21.5, border = "red")
  rect(xleft = 4.5, ybottom = 12.6, xright = 5.8, ytop = 17.4, border = "green")
  rect(xleft = 4.5, ybottom = 8.6, xright = 5.8, ytop = 12.4, border = "blue")
  rect(xleft = 4.5, ybottom = 1.6, xright = 5.8, ytop = 3.5, border = "purple")
  text(x = 1, y = 20, labels = "Vedrovice (prof, Ward D2)")
dev.off()

pdf(file = "./Adzes/clustLokProf.pdf")
  layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
  clustLok(VedProf)
  clustLok(KraProf)
  clustLok(KleProf)
  clustLok(NitProf)
  title(sub = "Momocs::clust (Ward D2) Prof")
dev.off()

pdf(file = "./Adzes/clustLokSide.pdf")
  layout(matrix(c(1, 2, 3, 4), 2, 2, byrow = TRUE))
  clustLok(VedSide)
  clustLok(KraSide)
  clustLok(KleSide)
  clustLok(NitSide)
  title(sub = "Momocs::clust (Ward D2) Side")
dev.off()

pdf(file = "./Adzes/clustLokTop.pdf")
  layout(matrix(c(1, 2, 3, 4), 2, 2, byrow = TRUE))
  clustLok(VedTop)
  clustLok(KraTop)
  clustLok(KleTop)
  clustLok(NitTop)
  title(sub = "Momocs::clust (Ward D2) Top")
dev.off()

pdf(file = "./Adzes/clustLokVed.pdf", width = 12, height = 5)
layout(matrix(c(1,2,3), 1, 3, byrow = TRUE))
  clustLok(VedProf)
    title(sub = "Prof")
  clustLok(VedSide)
    title(sub = "Side")
  clustLok(VedTop)
    title(sub = "Top")
dev.off()  

pdf(file = "./Adzes/clustLokKra.pdf", width = 12, height = 5)
layout(matrix(c(1,2,3), 1, 3, byrow = TRUE))
clustLok(KraProf)
title(sub = "Prof")
clustLok(KraSide)
title(sub = "Side")
clustLok(KraTop)
title(sub = "Top")
dev.off()  

pdf(file = "./Adzes/clustLokKle.pdf", width = 12, height = 5)
layout(matrix(c(1,2,3), 1, 3, byrow = TRUE))
clustLok(KleProf)
title(sub = "Prof")
clustLok(KleSide)
title(sub = "Side")
clustLok(KleTop)
title(sub = "Top")
dev.off()  

pdf(file = "./Adzes/clustLokNit.pdf", width = 12, height = 5)
layout(matrix(c(1,2,3), 1, 3, byrow = TRUE))
clustLok(NitProf)
title(sub = "Prof")
clustLok(NitSide)
title(sub = "Side")
clustLok(NitTop)
title(sub = "Top")
dev.off()  

# the end
graphics.off()  
rm(list = ls())
gc()
q("no")
