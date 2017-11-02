# ALL SHAPES
# - Adzes, axes and wedzes, basic analysis of whole dataset

# LIBRARIES ====================================================================
library(Momocs)
library(dplyr)
library(readr)
library(gridExtra)
library(grid)

# INPUT ========================================================================
inNPA <- list.files("Morpho/Adze/ProfileAll/", full.names = TRUE)
inNSA <- list.files("Morpho/Adze/SideView/", full.names = TRUE)
inNTA <- list.files("Morpho/Adze/TopView/", full.names = TRUE)

inNPW <- list.files("Morpho/Wed/Profile/", full.names = TRUE)
inNSW <- list.files("Morpho/Wed/Side/", full.names = TRUE)
inNTW <- list.files("Morpho/Wed/Top/", full.names = TRUE)

inNPx <- list.files("Morpho/Axe/ForProfile/", full.names = TRUE)
inNSx <- list.files("Morpho/Axe/ForSideView/", full.names = TRUE)
inNTx <- list.files("Morpho/Axe/ForTopView/", full.names = TRUE)

inNProf <- c(inNPA, inNPx, inNPW)
inNSide <- c(inNSA, inNSx, inNSW)
inNTop <- c(inNTA, inNTx, inNTW)

inProf <- import_jpg(jpg.path = inNProf, auto.notcentered = TRUE, 
                     threshold = 0.5, verbose = TRUE)
inSide <- import_jpg(jpg.path = inNSide, auto.notcentered = TRUE, 
                     threshold = 0.5, verbose = TRUE)
inTop <- import_jpg(jpg.path = inNTop, auto.notcentered = TRUE, 
                    threshold = 0.5, verbose = TRUE)

# Clean mem
rm(list = "inNProf", "inNSide", "inNTop")
rm(list = "inNPA", "inNSA", "inNTA")
rm(list = "inNPW", "inNSW", "inNTW")
rm(list = "inNPx", "inNSx", "inNTx")

# Fac slot data
prime <- read_csv("../Db.PRIME.csv")
secon <- select(prime, -Lokalita, -Note, -Ident)

secon$Lok <- as.factor(secon$Lok)
secon$Orig <- as.factor(secon$Orig)
secon$Cult <- as.factor(secon$Cult)
secon$Dat <- as.factor(secon$Dat)
secon$RM <- as.factor(secon$RM)
secon$Morph <- as.factor(secon$Morph)
secon$OpSeq <- as.factor(secon$OpSeq)
secon$SalVar <- as.factor(secon$SalVar)
secon$SalProfVys <- as.factor(secon$SalProfVys)
secon$SalProfTvar <- as.factor(secon$SalProfTvar)
secon$SalTylBok <- as.factor(secon$SalTylBok)
secon$StUpev <- as.factor(secon$StUpev)
secon$StPrac <- as.factor(secon$StPrac)
secon$Pouzitelnost <- as.factor(secon$Pouzitelnost)
secon$Half <- as.factor(secon$Half)

facProf <- filter(secon, Prof == TRUE) %>% select(-(Top:Prof))
facSide <- filter(secon, Side == TRUE) %>% select(-(Top:Prof))
facTop <- filter(secon, Top == TRUE) %>% select(-(Top:Prof))

facProf <- arrange(facProf, Morph, ID)
facSide <- arrange(facSide, Morph, ID)
facTop <- arrange(facTop, Morph, ID)

# MORPHOMETRICS ================================================================
# Outlines ---------------------------------------------------------------------
outProf <- Out(inProf, fac = facProf)
outSide <- Out(inSide, fac = facSide)
outTop <- Out(inTop, fac = facTop)

panel(outProf, fac = "Orig", names = TRUE)
panel(outSide, fac = "Orig", names = TRUE)
panel(outTop, fac = "Orig", names = TRUE)

# Clean mem.
rm(list = "prime", "secon")
rm(list = "facProf", "facSide", "facTop")
rm(list = "inProf", "inSide", "inTop")

# Save =========================================================================
save(outProf, file = "./Archive/AllShapes/Dt.outProf")
save(outSide, file = "./Archive/AllShapes/Dt.outSide")
save(outTop, file = "./Archive/AllShapes/Dt.outTop")

load(file = "./Archive/AllShapes/Dt.outProf")
load(file = "./Archive/AllShapes/Dt.outSide")
load(file = "./Archive/AllShapes/Dt.outTop")

# Manipulate -------------------------------------------------------------------
manProf <- outProf %>% 
  coo_slidedirection("N") %>% 
  coo_center() %>% 
  coo_smooth(50) %>% 
  coo_sample(200)

manSide <- outSide %>% 
  coo_alignxax() %>%
  coo_slidedirection("W") %>% 
  coo_center() %>% 
  coo_smooth(200) %>% 
  coo_sample(400)

manTop <- outTop %>% 
  coo_alignxax() %>%
  coo_slidedirection("W") %>% 
  coo_center() %>% 
  coo_smooth(200) %>% 
  coo_sample(400)

# Elliptical Fourier Transform -------------------------------------------------
# - normalized side and top view - "better" results
efProf <- efourier(manProf, nb.h = 11, norm = FALSE, smooth.it = 0, 
                   start = TRUE, verbose = TRUE)
efSide <- efourier(manSide, nb.h = 14, norm = TRUE, smooth.it = 0, 
                   start = TRUE, verbose = TRUE)
efTop <- efourier(manTop, nb.h = 11, norm = TRUE, smooth.it = 0, 
                  start = TRUE, verbose = TRUE)

# ANALYSIS =====================================================================
# PCA --------------------------------------------------------------------------
pcProf <- PCA(efProf)
pcSide <- PCA(efSide)
pcTop <- PCA(efTop)

# Scree plots
scree(pcProf, 1:5)
scree_min(pcProf, 0.98)
sp <- scree_plot(pcProf, 1:4)
pdf(file = "./AllShapes/pcContribProf.pdf", height = 5)
PCcontrib(pcProf, nax = 1:3, sd.r = c(-1, -0.5, 0, 0.5, 1))
dev.off()

scree(pcSide, 1:5)
scree_min(pcSide, 0.98)
ss <- scree_plot(pcSide, 1:4)
pdf(file = "./AllShapes/pcContribSide.pdf", height = 2.5, width = 10)
PCcontrib(pcSide, sd.r = c(-1.5, -1, -0.5, 0, 0.5, 1, 1.5))
dev.off()

scree(pcTop, 1:5)
scree_min(pcTop, 0.98)
st <- scree_plot(pcTop, 1:4)
pdf(file = "./AllShapes/pcContribTop.pdf", height = 2.5, width = 10)
PCcontrib(pcTop, nax = 1:3, sd.r = c(-1, -0.5, 0, 0.5, 1))
dev.off()

tp <- textGrob("(a) Prof (profil)")
ts <- textGrob("(b) Side (bokorys)")
tt <- textGrob("(c) Top (nárys)")

lay <- rbind(c(1,3,5),
             c(1,3,5),
             c(1,3,5),
             c(1,3,5),
             c(1,3,5),
             c(2,4,6))

pdf(file = "./AllShapes/screePlots.pdf", width = 10, height = 4)
grid.arrange(sp, tp, ss, ts, st, tt, layout_matrix = lay)
dev.off()

rm(list = "sp", "ss", "st", "tp", "ts", "tt", "lay")

# Plot PCA ---------------------------------------------------------------------
plot(pcProf, fac = "Morph")

# KMEANS -----------------------------------------------------------------------
# Count number of clusters K and perform kmeans clustering 
# - elbow method (wss - within-clusters sum of squares)
# - stolen and modified from rbloggers (DataScience+, by Sunny Anand)
k.max <- 8

# Prof
wssProf <- sapply(1:k.max, 
              function(k){kmeans(pcProf$x, k, nstart=50,
                                     iter.max = 15)$tot.withinss})

plot(1:k.max, wssProf,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K (Prof)",
     ylab="Total within-clusters sum of squares")

# Side
wssSide <- sapply(1:k.max, 
              function(k){kmeans(pcSide$x, k, nstart=50,
                                 iter.max = 15)$tot.withinss})

plot(1:k.max, wssSide,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K (Side)",
     ylab="Total within-clusters sum of squares")

# Top
wssTop <- sapply(1:k.max, 
              function(k){kmeans(pcTop$x, k, nstart=50,
                                 iter.max = 15)$tot.withinss})

plot(1:k.max, wssTop,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K (Top)",
     ylab="Total within-clusters sum of squares")

# K means clustering
KMEANS(pcProf, 4, nax = 1:2)
KMEANS(pcSide, 2)
KMEANS(pcTop, 3)

# Hierarchical clustering ------------------------------------------------------
CLUST(pcProf, fac = "SalProfTvar", type = "phylogram",
      hclust_method = "ward.D2", tip_labels = "SalProfTvar")
CLUST(pcSide, fac = "SalTylBok", type = "phylogram",
      hclust_method = "ward.D2", tip_labels = "SalTylBok")
CLUST(pcTop, fac = "RM", type = "phylogram",
      hclust_method = "ward.D2", tip_labels = "RM")

# # Linear dicriminant analysis ------------------------------------------------
# plot(LDA(pcProf, fac = "Morph"))
# plot(LDA(pcSide, fac = "Morph"))
# plot(LDA(pcTop, fac = "Cult"))

# END ==========================================================================
graphics.off()
rm(list = ls())
gc()
.rs.restartR()
q("no")
