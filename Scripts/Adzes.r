# ADZES
# - Main script to perform morphometric analysis on adzes (only)

# LIBRARIES ====================================================================
library(Momocs)
library(tidyverse)
library(stringr)
library(gridExtra)
library(grid)
library(xtable)

# INPUT ========================================================================
inNProf <- list.files("Morpho/Adze/ProfileAll/", full.names = TRUE)
inNSide <- list.files("Morpho/Adze/SideView/", full.names = TRUE)
inNTop <- list.files("Morpho/Adze/TopView/", full.names = TRUE)

inProf <- import_jpg(
  jpg.path = inNProf,
  auto.notcentered = TRUE,
  threshold = 0.5,
  verbose = TRUE
)
inSide <- import_jpg(
  jpg.path = inNSide,
  auto.notcentered = TRUE,
  threshold = 0.5,
  verbose = TRUE
)
inTop <- import_jpg(
  jpg.path = inNTop,
  auto.notcentered = TRUE,
  threshold = 0.5,
  verbose = TRUE
)

# Clean mem
rm(list = "inNProf", "inNSide", "inNTop")
rm(list = "inNPA", "inNSA", "inNTA")

# Save import data
save(inProf, file = "./Archive/Adzes/Dt.inProf")
save(inSide, file = "./Archive/Adzes/Dt.inSide")
save(inTop, file = "./Archive/Adzes/Dt.inTop")

# Fac slot data
prime <- read_csv("../Db.PRIME.csv")
secon <- select(prime, -Lokalita, -Note, -Ident)
secon <- secon %>% filter(Morph == "Ad") %>% select(-Include)

# Mutate Cult + Dat
secon <- secon %>%
  mutate(CultDat = paste(Cult, Dat, sep = " "))
# String to edit CultDat
secon$CultDat <-
  str_replace(secon$CultDat, pattern = "(.*) NA", replacement = "\\1")

# secon$Lok <- as.factor(secon$Lok)
# secon$Orig <- as.factor(secon$Orig)
# secon$Cult <- as.factor(secon$Cult)
# secon$Dat <- as.factor(secon$Dat)
# secon$RM <- as.factor(secon$RM)
# secon$Morph <- as.factor(secon$Morph)
# secon$OpSeq <- as.factor(secon$OpSeq)
# secon$SalVar <- as.factor(secon$SalVar)
# secon$SalProfVys <- as.factor(secon$SalProfVys)
# secon$SalProfTvar <- as.factor(secon$SalProfTvar)
# secon$SalTylBok <- as.factor(secon$SalTylBok)
# secon$StUpev <- as.factor(secon$StUpev)
# secon$StPrac <- as.factor(secon$StPrac)
# secon$Pouzitelnost <- as.factor(secon$Pouzitelnost)
# secon$Half <- as.factor(secon$Half)
# secon$CultDat <- as.factor(secon$CultDat)

facProf <- filter(secon, Prof == TRUE) %>% select(-(Top:Prof))
facSide <- filter(secon, Side == TRUE) %>% select(-(Top:Prof))
facTop <- filter(secon, Top == TRUE) %>% select(-(Top:Prof))

facProf <- arrange(facProf, ID)
facSide <- arrange(facSide, ID)
facTop <- arrange(facTop, ID)

colsToFac <- c("Lok", "Orig", "Cult", "Dat", "CultDat", "RM", "Morph", "OpSeq", 
               "SalVar", "SalProfVys", "SalProfTvar", "SalTylBok", 
               "StUpev", "StPrac", "Pouzitelnost", "Half")

facProf %<>%
  mutate_at(funs(factor(.)), .vars =  colsToFac)

facSide %<>%
  mutate_at(funs(factor(.)), .vars =  colsToFac)

facTop %<>%
  mutate_at(funs(factor(.)), .vars =  colsToFac)

detach("package:tidyverse", unload = TRUE)
detach("package:stringr", unload = TRUE)

# t-test korelace stop
StPrac <- secon$StPrac
StUpev <- secon$StUpev
St <- data.frame(StPrac, StUpev)
StPracN <- as.numeric(St$StPrac)
StUpevN <- as.numeric(St$StUpev)
StN <- data.frame(StPracN, StUpevN)
t.test(StN)
rcorr(as.matrix(StN))

# MORPHOMETRICS ================================================================
# Outlines ---------------------------------------------------------------------
outProf <- Out(inProf, fac = facProf)
outSide <- Out(inSide, fac = facSide)
outTop <- Out(inTop, fac = facTop)

# Check whether outlines fit with fac!
panel(outProf, fac = "Orig", names = TRUE)
panel(outSide, fac = "Orig", names = TRUE)
panel(outTop, fac = "Orig", names = TRUE)

# Clean mem.
rm(list = "prime", "secon")
rm(list = "facProf", "facSide", "facTop")
rm(list = "inProf", "inSide", "inTop")

# Save
save(outProf, file = "./Archive/Adzes/Dt.outProf")
save(outSide, file = "./Archive/Adzes/Dt.outSide")
save(outTop, file = "./Archive/Adzes/Dt.outTop")

load(file = "./Archive/Adzes/Dt.outProf")
load(file = "./Archive/Adzes/Dt.outSide")
load(file = "./Archive/Adzes/Dt.outTop")

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

# # Calibrating harmonics ------------------------------------------------------
# # Profile
# # devProf <- calibrate_deviations(manProf, method = "efourier")
# harmProf <- calibrate_harmonicpower(manProf, method = "efourier",
#                                     nb.h = 12, plot = TRUE)
# pdf("./Adzes/panelHarmProf.pdf")
# calibrate_reconstructions(manProf, "efourier", id = 30, range = 1:12)
# dev.off()
# 
# # Side
# # devSide <- calibrate_deviations(manSide, method = "efourier")
# harmSide <- calibrate_harmonicpower(manSide, method = "efourier",
#                                     nb.h = 16, plot = TRUE)
# pdf("./Adzes/panelHarmSide.pdf", width = 7, height = 3)
# calibrate_reconstructions(manSide, "efourier", id = 23, range = 1:16)
# dev.off()
# 
# # Top
# # devTop <- calibrate_deviations(manTop, method = "efourier")
# harmTop <- calibrate_harmonicpower(manTop, method = "efourier",
#                                    nb.h = 12, plot = TRUE)
# pdf("./Adzes/panelHarmTop.pdf", width = 7, height = 2.5)
# calibrate_reconstructions(manTop, "efourier", id = 17, range = 1:12)
# dev.off()
# 
# # rm(list = "devProf", "devSide", "devTop")
# 
# # Exporting table with harmonic power to Latex
# harmonicPower <- rbind(harmProf$minh, harmSide$minh, harmTop$minh)
# row.names(harmonicPower) <- c("Profile", "Side", "Top")
# harmonicPower <- xtable(harmonicPower,
#                  caption = "Příspěvek harmonických proměnných k popisu tvaru")
# print(harmonicPower, file = "./Adzes/Tab.HarmPow.tex",
#       booktabs = T, caption.placement = "top")
# #
# # rm(list = "harmonicPower", "harmProf", "harmSide", "harmTop")

# Elliptical Fourier Transform -------------------------------------------------
efProf <- efourier(
  manProf,
  nb.h = 10,
  norm = FALSE,
  smooth.it = 0,
  start = FALSE,
  verbose = TRUE
)
efSide <- efourier(
  manSide,
  nb.h = 12,
  norm = F,
  smooth.it = 0,
  start = FALSE,
  verbose = TRUE
)
efTop <- efourier(
  manTop,
  nb.h = 10,
  norm = F,
  smooth.it = 0,
  start = FALSE,
  verbose = TRUE
)

# Save and clean memory
save(efProf, file = "./Archive/Adzes/Dt.efProf")
save(efSide, file = "./Archive/Adzes/Dt.efSide")
save(efTop, file = "./Archive/Adzes/Dt.efTop")

load("./Archive/Adzes/Dt.efProf")
load("./Archive/Adzes/Dt.efSide")
load("./Archive/Adzes/Dt.efTop")

# ANALYSIS =====================================================================
# Symmetry ---------------------------------------------------------------------
# - rm_sym/rm_asym works only for Prof, why? or does it?
# hokuspokus <- rm_sym(efProf)
# panel(hokuspokus)
# panel(efProf)

symProf <- symmetry(efProf)
symSide <- symmetry(efSide)
symTop <- symmetry(efTop)

# hSymProf <- qplot(symProf[, "sym"],
# xlab = "(a) sym (Prof)", ylab = "frequency") +
# stat_bin(bins = 10, fill = "white", color = "black") + theme_bw()

# hSymSide <- qplot(symSide[, "sym"],
# xlab = "(b) sym (Side)", ylab = "frequency") +
# stat_bin(bins = 10, fill = "white", color = "black") + theme_bw()

# hSymTop <- qplot(symTop[, "sym"],
# xlab = "(c) sym (Top)", ylab = "frequency") +
# stat_bin(bins = 10, fill = "white", color = "black") + theme_bw()

pdf(file = "./Adzes/Symmetry.pdf",
    height = 2,
    width = 7)
layout(matrix(c(1, 2, 3), 1, 3, byrow = TRUE))
hist(
  symProf[, "sym"],
  xlab = "(a) sym (Prof)",
  ylab = "density",
  main = NA,
  freq = FALSE
)
hist(
  symSide[, "sym"],
  xlab = "(b) sym (Side)",
  ylab = "density",
  main = NA,
  freq = FALSE
)
hist(
  symTop[, "sym"],
  xlab = "(c) sym (Top)",
  ylab = "density",
  main = NA,
  freq = FALSE
)
# grid.arrange(hSymProf, hSymSide, hSymTop, nrow = 1)
dev.off()

# PCA ==========================================================================
pcProf <- PCA(efProf)
pcSide <- PCA(efSide)
pcTop <- PCA(efTop)

save(pcProf, file = "./Archive/Adzes/Dt.pcProf")
save(pcSide, file = "./Archive/Adzes/Dt.pcSide")
save(pcTop, file = "./Archive/Adzes/Dt.pcTop")

load("./Archive/Adzes/Dt.pcProf")
load("./Archive/Adzes/Dt.pcSide")
load("./Archive/Adzes/Dt.pcTop")

# # Scree plots
# scree(pcProf, 1:5)
# scree_min(pcProf, 0.98)
# sp <- scree_plot(pcProf, 1:4)
# pdf(file = "./Adzes/pcContribProf.pdf", height = 5)
# PCcontrib(pcProf,
#           nax = 1:3,
#           sd.r = c(-1.5,-1,-0.5, 0, 0.5, 1, 1.5))
# dev.off()
# 
# scree(pcSide, 1:5)
# scree_min(pcSide, 0.98)
# ss <- scree_plot(pcSide, 1:5)
# pdf(file = "./Adzes/pcContribSide.pdf",
#     height = 2.5,
#     width = 10)
# PCcontrib(pcSide,
#           nax = 1:3,
#           sd.r = c(-1.5,-1,-0.5, 0, 0.5, 1, 1.5))
# dev.off()
# 
# scree(pcTop, 1:5)
# scree_min(pcTop, 0.98)
# st <- scree_plot(pcTop, 1:4)
# pdf(file = "./Adzes/pcContribTop.pdf",
#     height = 2.5,
#     width = 10)
# PCcontrib(pcTop, nax = 1:3, sd.r = c(-1,-0.5, 0, 0.5, 1))
# dev.off()
# 
# tp <- textGrob("(a) Prof (profil)")
# ts <- textGrob("(b) Side (bokorys)")
# tt <- textGrob("(c) Top (nárys)")
# lay <- rbind(c(1, 3, 5),
#              c(1, 3, 5),
#              c(1, 3, 5),
#              c(1, 3, 5),
#              c(1, 3, 5),
#              c(2, 4, 6))
# 
# pdf(file = "./Adzes/screePlots.pdf",
#     width = 10,
#     height = 4)
# grid.arrange(sp, tp, ss, ts, st, tt, layout_matrix = lay)
# dev.off()
# 
# rm(list = "sp", "ss", "st", "tp", "ts", "tt", "lay")

# Basic PCA plots --------------------------------------------------------------
pdf(
  file = "./Adzes/pcProf.pdf",
  paper = "a4r",
  width = 12,
  height = 8
)
plot(
  pcProf,
  points = TRUE,
  morphospace = TRUE,
  pos.shp = "xy",
  size.shp = 0.5,
  amp.shp = 0.9,
  pch = 4,
  center.origin = FALSE,
  zoom = 1.2,
  density = T,
  contour = F,
  axisvar = T,
  box = TRUE,
  cex.labelsgroups = 2
)
dev.off()

pdf(
  file = "./Adzes/pc2Prof.pdf",
  paper = "a4r",
  width = 12,
  height = 8
)
plot(
  pcProf,
  xax = 2,
  yax = 3,
  points = TRUE,
  morphospace = TRUE,
  pos.shp = "xy",
  size.shp = 0.5,
  amp.shp = 0.9,
  pch = 4,
  center.origin = FALSE,
  zoom = 1.2,
  density = T,
  contour = F,
  axisvar = T,
  box = TRUE,
  cex.labelsgroups = 2
)
dev.off()

pdf(
  file = "./Adzes/pcSide.pdf",
  paper = "a4r",
  width = 12,
  height = 8
)
plot(
  pcSide,
  points = TRUE,
  morphospace = TRUE,
  pos.shp = "xy",
  size.shp = 1,
  amp.shp = 0.9,
  flipx.shp = F,
  flipy.shp = T,
  pch = 4,
  center.origin = F,
  zoom = 1.6,
  density = T,
  contour = F,
  axisvar = T,
  box = TRUE,
  cex.labelsgroups = 2
)
dev.off()

pdf(
  file = "./Adzes/pc2Side.pdf",
  paper = "a4r",
  width = 12,
  height = 8
)
plot(
  pcSide,
  xax = 2,
  yax = 3,
  points = TRUE,
  morphospace = TRUE,
  pos.shp = "xy",
  size.shp = 1,
  amp.shp = 0.9,
  flipx.shp = F,
  flipy.shp = T,
  pch = 4,
  center.origin = T,
  zoom = 1.4,
  density = T,
  contour = F,
  axisvar = T,
  box = TRUE,
  cex.labelsgroups = 2
)
dev.off()

pdf(
  file = "./Adzes/pcTop.pdf",
  paper = "a4r",
  width = 12,
  height = 8
)
plot(
  pcTop,
  points = TRUE,
  morphospace = TRUE,
  pos.shp = "xy",
  size.shp = 0.8,
  amp.shp = 0.6,
  pch = 4,
  center.origin = F,
  zoom = 1.4,
  density = T,
  contour = F,
  axisvar = T,
  box = TRUE,
  cex.labelsgroups = 2
)
dev.off()

pdf(
  file = "./Adzes/pc2Top.pdf",
  paper = "a4r",
  width = 12,
  height = 8
)
plot(
  pcTop,
  xax = 2,
  yax = 3,
  points = TRUE,
  morphospace = TRUE,
  pos.shp = "xy",
  size.shp = 1,
  amp.shp = 0.8,
  pch = 4,
  center.origin = TRUE,
  zoom = 1,
  density = T,
  contour = F,
  axisvar = T,
  box = TRUE,
  cex.labelsgroups = 2
)
dev.off()

# PCA 3 plots ------------------------------------------------------------------
# color palettes (see color brewer)
pal2 <- c("#e41a1c", "#33a02c")
pal3 <- c("#377eb8", "#33a02c", "#e41a1c")
palN <- c("#e41a1c", "gray50", "#33a02c")
palN3 <- c("gray50", "#33a02c", "#e41a1c") ##377eb8
palN2 <- c("#33a02c", "#e41a1c", "gray50")
pal4 <- c("#377eb8", "#33a02c", "#e41a1c", "gray50")

# pca3Plot <- function(pcaSlot, facSlot) {
#   plot3(pcaSlot, fac = facSlot,
#         size.shp = 0.5, amp.shp = 0.8, pch = c(15, 4, 19),
#         morphospace = T, pos.shp = "range_axes",
#         chull = T, chull.filled = T, chull.lty = 0,
#         ellipses = F,
#         labelsgroups = TRUE, abbreviate.labelsgroups = TRUE)
# }

# Stopy
pdf(file = "./Obr/pc3-Top_StUpev.pdf")
plot3(
  pcTop,
  fac = "StUpev",
  size.shp = 1,
  amp.shp = 1,
  col = palN3,
  pch = c(15, 4, 19),
  morphospace = T,
  pos.shp = "range_axes",
  chull = T,
  chull.filled = T,
  chull.lty = 0,
  ellipses = F,
  zoom = 1.2,
  labelsgroups = TRUE,
  abbreviate.labelsgroups = TRUE
)
dev.off()

pdf(file = "./Adzes/pc3-Top_StPrac.pdf")
plot3(
  pcTop,
  fac = "StPrac",
  size.shp = 1,
  amp.shp = 1,
  col = palN,
  pch = c(15, 4, 19),
  morphospace = T,
  pos.shp = "range_axes",
  chull = T,
  chull.filled = T,
  chull.lty = 0,
  ellipses = F,
  zoom = 1.2,
  labelsgroups = TRUE,
  abbreviate.labelsgroups = TRUE
)
dev.off()

pdf(file = "./Obr/pc3-Side_StUpev.pdf")
plot3(
  pcSide,
  fac = "StUpev",
  size.shp = 1,
  amp.shp = 1,
  col = palN3,
  pch = c(15, 4, 19),
  morphospace = T,
  pos.shp = "range_axes",
  chull = T,
  chull.filled = T,
  chull.lty = 0,
  ellipses = F,
  labelsgroups = TRUE,
  abbreviate.labelsgroups = TRUE,
  flipx.shp = T,
  zoom = 1.2
)
dev.off()

pdf(file = "./Adzes/pc3-Side_StPrac.pdf")
plot3(
  pcSide,
  fac = "StPrac",
  size.shp = 1,
  amp.shp = 1,
  col = palN,
  pch = c(15, 4, 19),
  morphospace = T,
  pos.shp = "range_axes",
  chull = T,
  chull.filled = T,
  chull.lty = 0,
  ellipses = F,
  labelsgroups = TRUE,
  abbreviate.labelsgroups = TRUE,
  flipx.shp = T,
  zoom = 1.2
)
dev.off()

# plot3(pcProf, fac = "Half",
#      size.shp = 0.5, amp.shp = 0.8, col = pal2, pch = c(4, 19),
#      morphospace = T, pos.shp = "range_axes",
#      chull = T, chull.filled = T, chull.lty = 0,
#      ellipses = F,
#      labelsgroups = TRUE, abbreviate.labelsgroups = TRUE)

# SalVar
pdf(file = "./Adzes/pc3-Prof_SalProfVys.pdf")
plot3(
  pcProf,
  fac = "SalProfVys",
  size.shp = 1,
  amp.shp = 1,
  col = pal3,
  pch = c(15, 4, 19),
  morphospace = T,
  pos.shp = "range_axes",
  chull = T,
  chull.filled = T,
  chull.lty = 0,
  ellipses = F,
  zoom = 1,
  labelsgroups = TRUE,
  abbreviate.labelsgroups = TRUE
)
dev.off()

pdf(file = "./Obr/pc3-Prof_SalProfTvar.pdf")
plot3(
  pcProf,
  fac = "SalProfTvar",
  size.shp = 1,
  amp.shp = 1,
  pch = c(15, 4, 19),
  morphospace = T,
  pos.shp = "range_axes",
  chull = T,
  chull.filled = T,
  chull.lty = 0,
  ellipses = F,
  zoom = 1,
  labelsgroups = TRUE,
  abbreviate.labelsgroups = TRUE
)
dev.off()

pdf(file = "./Obr/pc3-Side_SalTylBok.pdf")
plot3(
  pcSide,
  fac = "SalTylBok",
  size.shp = 1,
  amp.shp = 1,
  pch = c(15, 4, 19),
  morphospace = T,
  pos.shp = "range_axes",
  chull = T,
  chull.filled = T,
  chull.lty = 0,
  ellipses = F,
  zoom = 1.3,
  flipx.shp = T,
  labelsgroups = TRUE,
  abbreviate.labelsgroups = TRUE
)
dev.off()

# Orig
pdf(file = "./Adzes/pc3-Prof_Orig.pdf")
plot3(
  pcProf,
  fac = "Orig",
  size.shp = 0.5,
  amp.shp = 0.8,
  col = pal3,
  pch = 4,
  morphospace = T,
  pos.shp = "range_axes",
  chull = T,
  chull.filled = T,
  chull.lty = 0,
  ellipses = F,
  labelsgroups = TRUE,
  abbreviate.labelsgroups = TRUE
)
dev.off()

pdf(file = "./Adzes/pc3-Side_Orig.pdf")
plot3(
  pcSide,
  fac = "Orig",
  size.shp = 0.5,
  amp.shp = 0.8,
  col = pal3,
  pch = 4,
  morphospace = T,
  pos.shp = "range_axes",
  chull = T,
  chull.filled = T,
  chull.lty = 0,
  ellipses = F,
  labelsgroups = TRUE,
  abbreviate.labelsgroups = TRUE
)
dev.off()

pdf(file = "./Adzes/pc3-Top_Orig.pdf")
plot3(
  pcTop,
  fac = "Orig",
  size.shp = 0.5,
  amp.shp = 0.8,
  col = pal3,
  pch = 4,
  morphospace = T,
  pos.shp = "range_axes",
  chull = T,
  chull.filled = T,
  chull.lty = 0,
  ellipses = F,
  labelsgroups = TRUE,
  abbreviate.labelsgroups = TRUE
)
dev.off()

# Clustering ===================================================================
# Count number of clusters K and perform kmeans clustering
# - elbow method (wss - within-clusters sum of squares)
# - stolen and modified from rbloggers (DataScience+, by Sunny Anand)
k.max <- 8

# Prof
wssProf <- sapply(1:k.max,
                  function(k) {
                    kmeans(pcProf$x, k, nstart = 50,
                           iter.max = 15)$tot.withinss
                  })

plot(
  1:k.max,
  wssProf,
  type = "b",
  pch = 19,
  frame = FALSE,
  xlab = "Number of clusters K (Prof)",
  ylab = "Total within-clusters sum of squares"
)

# Side
wssSide <- sapply(1:k.max,
                  function(k) {
                    kmeans(pcSide$x, k, nstart = 50,
                           iter.max = 15)$tot.withinss
                  })

plot(
  1:k.max,
  wssSide,
  type = "b",
  pch = 19,
  frame = FALSE,
  xlab = "Number of clusters K (Side)",
  ylab = "Total within-clusters sum of squares"
)

# Top
wssTop <- sapply(1:k.max,
                 function(k) {
                   kmeans(pcTop$x, k, nstart = 50,
                          iter.max = 15)$tot.withinss
                 })

plot(
  1:k.max,
  wssTop,
  type = "b",
  pch = 19,
  frame = FALSE,
  xlab = "Number of clusters K (Top)",
  ylab = "Total within-clusters sum of squares"
)

# grid arrange
pdf(file = "./Adzes/elbow.pdf",
    width = 10,
    height = 4)
layout(matrix(c(1, 2, 3), 1, 3, byrow = TRUE))
plot(
  1:k.max,
  wssProf,
  type = "b",
  pch = 19,
  frame = FALSE,
  xlab = "Number of clusters K (Prof)",
  ylab = "Total within-clusters sum of squares"
)
plot(
  1:k.max,
  wssSide,
  type = "b",
  pch = 19,
  frame = FALSE,
  xlab = "Number of clusters K (Side)",
  ylab = NA
)
plot(
  1:k.max,
  wssTop,
  type = "b",
  pch = 19,
  frame = FALSE,
  xlab = "Number of clusters K (Top)",
  ylab = NA
)
dev.off()

# K means clustering -----------------------------------------------------------
pdf(file = "./Adzes/kmeansProf.pdf")
KMEANS(pcProf, 3)
dev.off()

pdf(file = "./Adzes/kmeansSide.pdf")
KMEANS(pcSide, 3)
dev.off()

pdf(file = "./Adzes/kmeansTop.pdf")
KMEANS(pcTop, 3)
dev.off()

# Hierarchical clustering ------------------------------------------------------
# CLUST(
#   pcTop,
#   fac = "StPrac",
#   type = "phylogram",
#   hclust_method = "ward.D2",
#   tip_labels = "StPrac"
# )
# CLUST(
#   pcSide,
#   fac = "SalTylBok",
#   type = "phylogram",
#   hclust_method = "ward.D2",
#   tip_labels = "SalTylBok"
# )
# CLUST(
#   pcTop,
#   fac = "RM",
#   type = "phylogram",
#   hclust_method = "ward.D2",
#   tip_labels = "RM"
# )

# Linear dicriminant analysis ==================================================
load("./Archive/Adzes/Dt.efProf")
load("./Archive/Adzes/Dt.efSide")
load("./Archive/Adzes/Dt.efTop")

funLDA <- function(efSLot, facSlot, view) {
  plot(LDA(efSLot, fac = facSlot), 
       chull.filled = TRUE,
       ellipsesax = FALSE,
       title = sprintf("Linear discriminant analysis (%s: %s)", view, facSlot))
}

pdf(file = "./Adzes/ldaProf_Vys.pdf")
funLDA(efProf, "SalProfVys", "Prof")
dev.off()

pdf(file = "./Adzes/ldaProf_Tvar.pdf")
#funLDA(efProf, "SalProfTvar", "Prof")
plot(LDA(efProf, fac = "SalProfTvar"), zoom = 2.4, 
     chull.filled = TRUE,
     ellipsesax = FALSE,
     title = ("Linear discriminant analysis (Prof: Tvar)"))
dev.off()

pdf(file = "./Adzes/ldaSide_Bok.pdf")
#funLDA(efSide, "SalTylBok", "Side")
plot(LDA(efSide, fac = "SalTylBok"), zoom = 4.5, 
     chull.filled = TRUE,
     ellipsesax = FALSE,
     title = ("Linear discriminant analysis (Side: Tyl)"))
dev.off()

pdf(file = "./Adzes/ldaTop_StUpev.pdf")
funLDA(efTop, "StUpev", "Top")
dev.off()
pdf(file = "./Adzes/ldaTop_StPrac.pdf")
funLDA(efTop, "StPrac", "Top")
dev.off()
pdf(file = "./Adzes/ldaSide_StUpev.pdf")
funLDA(efSide, "StUpev", "Side")
dev.off()
pdf(file = "./Adzes/ldaSide_StPrac.pdf")
funLDA(efSide, "StPrac", "Side")
dev.off()

pdf(file = "./Adzes/ldaProf_Orig.pdf")
funLDA(efProf, "Orig", "Prof")
dev.off()
pdf(file = "./Adzes/ldaSide_Orig.pdf")
funLDA(efSide, "Orig", "Side")
dev.off()
pdf(file = "./Adzes/ldaTop_Orig.pdf")
funLDA(efTop, "Orig", "Top")
dev.off()

# Other
# - it is possible to distinguish Lbk I vs II in Top view thanks to LDA
plot3(PCA(efTop), fac = "CultDat")
funLDA(efTop, "CultDat", "Top")

# - same for OpSeq in Side and Top views
funLDA(efProf, "OpSeq", "Prof")

pdf(file = "./Adzes/ldaSide_OpSeq.pdf")
funLDA(efSide, "OpSeq", "Side")
dev.off()

pdf(file = "./Adzes/ldaTop_OpSeq.pdf")
funLDA(efTop, "OpSeq", "Top")
dev.off()

# MANOVA =======================================================================
load("./Archive/Adzes/Dt.pcProf")
load("./Archive/Adzes/Dt.pcSide")
load("./Archive/Adzes/Dt.pcTop")

# Normality tests --------------------------------------------------------------
topStUpevT <- filter(pcTop, StUpev == "T")
topStUpevF <- filter(pcTop, StUpev == "F")

topStPracT <- filter(pcTop, StPrac == "T")
topStPracF <- filter(pcTop, StPrac == "F")

sideStUpevT <- filter(pcSide, StUpev == "T")
sideStUpevF <- filter(pcSide, StUpev == "F")

sideStPracT <- filter(pcSide, StPrac == "T")
sideStPracF <- filter(pcSide, StPrac == "F")

# Q-Q plot
# qqnorm(topStUpevT$x[, 1])
library(ggpubr)
library(mvnormtest)
ggqqplot(pcTop$x[, 1])
ggqqplot(pcSide$x[, 1])
ggqqplot(pcTop$x[, 1])

ggqqplot(pcTop$x[, 2])
ggqqplot(pcSide$x[, 2])
ggqqplot(pcTop$x[, 2])

# ggqqplot(topStUpevT$x[, 1])
# ggqqplot(topStUpevF$x[, 1])
# 
# ggqqplot(topStPracT$x[, 1])
# ggqqplot(topStPracF$x[, 1])
# 
# ggqqplot(sideStUpevT$x[, 1])
# ggqqplot(sideStUpevF$x[, 1])
# 
# ggqqplot(sideStPracT$x[, 1])
# ggqqplot(sideStPracF$x[, 1])

# Shapiro--Wilk normality test
shapiro.test(pcTop$x[, 1])
shapiro.test(pcSide$x[, 1])

shapiro.test(pcTop$x[, 2])
shapiro.test(pcSide$x[, 2])

# shapiro.test(topStUpevT$x)
# shapiro.test(topStUpevF$x)
# 
# shapiro.test(topStPracT$x)
# shapiro.test(topStPracF$x)
# 
# shapiro.test(sideStUpevT$x)
# shapiro.test(sideStUpevF$x)
# 
# shapiro.test(topStPracT$x)
# shapiro.test(topStPracF$x)

# Q-Q plots -> somewhat normal distributions, but
# shapiro--wilk's tests - non-normal distributions

# Tests of homogeneity of variances
# - Bartlett for Top ~StUpev, ~StPrac; pc1:pc2 
bartlett.test(pcTop$x[, 1], pcTop$fac$StUpev) # rejected
bartlett.test(pcTop$x[, 2], pcTop$fac$StUpev) # not

bartlett.test(pcTop$x[, 1], pcTop$fac$StPrac) # not
bartlett.test(pcTop$x[, 2], pcTop$fac$StPrac) # not

# - Fligner-Killeen for Top ~StUpev, ~StPrac; pc1:pc2 
fligner.test(pcTop$x[, 1], pcTop$fac$StUpev) # not
fligner.test(pcTop$x[, 2], pcTop$fac$StUpev) # not

fligner.test(pcTop$x[, 1], pcTop$fac$StPrac) # not
fligner.test(pcTop$x[, 2], pcTop$fac$StPrac) # not

# - Bartlett for Side ~StUpev, ~StPrac; pc1:pc2 
bartlett.test(pcSide$x[, 1], pcSide$fac$StPrac) # not
bartlett.test(pcSide$x[, 2], pcSide$fac$StPrac) # not

bartlett.test(pcSide$x[, 1], pcSide$fac$StUpev) # rejected
bartlett.test(pcSide$x[, 2], pcSide$fac$StUpev) # not

# - Fligner-Killeen for Side ~StUpev, ~StPrac; pc1:pc2
fligner.test(pcSide$x[, 1], pcSide$fac$StUpev) # not
fligner.test(pcSide$x[, 2], pcSide$fac$StUpev) # not

fligner.test(pcSide$x[, 1], pcSide$fac$StPrac) # not
fligner.test(pcSide$x[, 2], pcSide$fac$StPrac) # not

bartlett.test(efTop$coe[, 1], efTop$fac$StUpev) # rejected

# Results: rejected for:
# - StUpev - pc1 - Top and 
# - StUpev - pc1 - Side

# MANOVA(pcProf, fac = "SalProfVys", retain = 0.99)
# MANOVA(pcProf, fac = "SalProfTvar", retain = 0.99)
# MANOVA(pcSide, fac = "SalTylBok", retain = 0.99)
# 
# MANOVA(pcSide, fac = "StPrac", retain = 0.99)
# MANOVA(pcSide, fac = "StUpev", retain = 0.99)
# 
# MANOVA(pcTop, fac = "StPrac", retain = 0.99)
# MANOVA(pcTop, fac = "StUpev", retain = 0.99)

# m <- manova(pcTop$x[, 2:3] ~ pcTop$fac$StPrac) # * pcTop$fac$StUpev
# summary(m)
# summary.aov(m)

# Kruskal--Wallis test
# boxplot(sideStPracT$x[, 1], sideStPracF$x[, 1])
kruskal.test(pcTop$x[, 1], pcTop$fac$StPrac)
kruskal.test(pcSide$x[, 1], pcSide$fac$StPrac)
# kruskal.test(pcTop$x[, 2], pcTop$fac$StPrac)
# kruskal.test(pcSide$x[, 2], pcSide$fac$StPrac)

kruskal.test(pcTop$x[, 1], pcTop$fac$StUpev)
kruskal.test(pcSide$x[, 1], pcSide$fac$StUpev)
# kruskal.test(pcTop$x[, 2], pcTop$fac$StUpev)
# kruskal.test(pcSide$x[, 2], pcSide$fac$StUpev)

kruskal.test(pcSide$x[, 1], pcSide$fac$SalTylBok)
kruskal.test(pcProf$x[, 1], pcProf$fac$SalProfTvar)
kruskal.test(pcProf$x[, 1], pcProf$fac$SalProfVys)
kruskal.test(pcProf$x[, 2], pcProf$fac$SalProfTvar)
kruskal.test(pcProf$x[, 2], pcProf$fac$SalProfVys)

kruskal.test(pcSide$x[,1], pcSide$fac$CultDat)
kruskal.test(pcTop$x[,1], pcTop$fac$CultDat)
kruskal.test(pcProf$x[,1], pcProf$fac$CultDat)
kruskal.test(pcProf$x[,2], pcProf$fac$CultDat)

kruskal.test(pcSide$x[,1], pcSide$fac$Orig)
kruskal.test(pcTop$x[,1], pcTop$fac$Orig)
kruskal.test(pcProf$x[,1], pcProf$fac$Orig)
kruskal.test(pcProf$x[,2], pcProf$fac$Orig)

# END ==========================================================================
graphics.off()
rm(list = ls())
gc()
.rs.restartR()
q("no")
