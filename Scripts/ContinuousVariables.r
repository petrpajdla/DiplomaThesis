 #  - Basic data analysis and plots of continuous numerical values

# # LIBRARIES ---------------------------
library(tidyverse)
library(gridExtra)
library(xtable)

# citation("tidyverse")
# citation("gridExtra")

getwd()

# INPUT ---------------------------
#  - Reading data from db and extracting metrics data
prime <- read_csv("../Db.PRIME.csv")
# problems(prime)

# met contains Morph types Ad, Ax, We only
met <- select(prime, Include:Ident, Orig:Mas)
met <- filter(met, Morph == "Ad" | Morph == "Ax" | Morph == "We")

# # (this was to copy somwhere? no idea anymore)
# sumStatLok <- prime %>% 
#   group_by(Lokalita) %>% 
#   summarise(n = n(), sum(Mas))

# DATA MANIPULATION ---------------------------
# # Subsetting only ADZES & Wedzes
# met <- met %>% 
#   filter(Morph == "Ad" | Morph == "We")  %>% 
#   filter(RM == "JzH")

# CONTINUE EITHER HERE OR JUMP TO PLOTS!

# Removing rows with NA data in metrics
metLen.Hei <- filter(met, !is.na(Len) | !is.na(Wid) | !is.na(Hei))
metMas <- filter(met, !is.na(Mas))

met <- met %>% 
  mutate(CultDat = paste(Cult, Dat, sep = " "))
met$CultDat <- str_replace(met$CultDat, pattern = "(.*) NA", replacement = "\\1")

# Counting relative width (rWid) and relative thickness (rHei), rounding
metLen.Hei$Len <- as.numeric(metLen.Hei$Len)
metLen.Hei$Wid <- as.numeric(metLen.Hei$Wid)
metLen.Hei$Hei <- as.numeric(metLen.Hei$Hei)
metMas$Mas <- as.numeric(metMas$Mas)

metLen.Hei <- metLen.Hei %>%
  mutate(rWid = (Wid / Len) * 100, rHei = (Hei / Wid) * 100)

metLen.Hei$rWid <- round(metLen.Hei$rWid, 2)
metLen.Hei$rHei <- round(metLen.Hei$rHei, 2)

# DESCRIPTIVE STATS ---------------------------
# Komplet
stVse <- metLen.Hei %>% 
  summarise(n=n(),
            mean(Len), median(Len), sd(Len), min(Len), max(Len),
            mean(Wid), median(Wid), sd(Wid), min(Wid), max(Wid),
            mean(Hei), median(Hei), sd(Hei), min(Hei), max(Hei)) %>% 
  as.data.frame() %>% 
  t()

stMas <- metMas %>% 
  summarise(n=n(), mean(Mas), median(Mas), sd(Mas), min(Mas), max(Mas)) %>% 
  as.data.frame() %>% 
  t()
  
stVse <- rbind(stVse, stMas)
colnames(stVse) <- c("BKI")
stVse <- round(stVse, 2)
stVse <- as.data.frame(stVse) %>% 
  xtable(caption = "Popisná statistika, celý soubor")

# print(stVse, file = "../../Text/9Priloha/Tab/stats.txt", booktabs = T, caption.placement = "top")

# Lokality
stLok <- metLen.Hei %>% 
  group_by(Lok) %>% 
  summarise(n=n(),
            mean(Len), median(Len), sd(Len), min(Len), max(Len),
            mean(Wid), median(Wid), sd(Wid), min(Wid), max(Wid),
            mean(Hei), median(Hei), sd(Hei), min(Hei), max(Hei))
stLok <- cbind(stLok$Lok, round(stLok[,2:17], 2)) %>% 
  as.data.frame() %>% 
  t()

stLokMas <- metMas %>% 
  group_by(Lok) %>% 
  summarise(n=n(), mean(Mas), median(Mas), sd(Mas), min(Mas), max(Mas))
stLokMas <- cbind(stLokMas$Lok, round(stLokMas[,2:7], 2)) %>% 
  as.data.frame() %>% 
  t()

colnames(stLok) <- as.character(stLok[1, ])
stLok <- as.data.frame(stLok[-1, ]) %>% 
  xtable(caption = "Popisná statistika -- délka, šířka, výška, seskupeno dle lokalit")
# print(stLok, file = "../../Text/9Priloha/Tab/stLok.txt", booktabs = T, caption.placement = "top")

colnames(stLokMas) <- as.character(stLokMas[1, ])
stLokMas <- as.data.frame(stLokMas[-1, ]) %>% 
  xtable(caption = "Popisná statistika -- hmotnost, seskupeno dle lokalit")
# print(stLokMas, file = "../../Text/9Priloha/Tab/stLokMas.txt", booktabs = T, caption.placement = "top")

# Origin
stOrig <- metLen.Hei %>% 
  group_by(Orig) %>% 
  summarise(n=n(),
            mean(Len), median(Len), sd(Len), min(Len), max(Len),
            mean(Wid), median(Wid), sd(Wid), min(Wid), max(Wid),
            mean(Hei), median(Hei), sd(Hei), min(Hei), max(Hei))
stOrig <- cbind(stOrig$Orig, round(stOrig[,2:17], 2)) %>% 
  as.data.frame() %>% 
  t()

stOrigMas <- metMas %>% 
  group_by(Orig) %>% 
  summarise(n=n(), mean(Mas), median(Mas), sd(Mas), min(Mas), max(Mas))
stOrigMas <- cbind(stOrigMas$Orig, round(stOrigMas[,2:7], 2)) %>% 
  as.data.frame() %>% 
  t()

colnames(stOrig) <- as.character(stOrig[1, ])
stOrig <- as.data.frame(stOrig[-1, ]) %>% 
  xtable(caption = "Popisná statistika -- délka, šířka, výška, seskupeno dle kontextu")
# print(stOrig, file = "../../Text/9Priloha/Tab/stOrig.txt", booktabs = T, caption.placement = "top")

colnames(stOrigMas) <- as.character(stOrigMas[1, ])
stOrigMas <- as.data.frame(stOrigMas[-1, ]) %>% 
  xtable(caption = "Popisná statistika -- hmotnost, seskupeno dle kontextu")
# print(stOrigMas, file = "../../Text/9Priloha/Tab/stOrigMas.txt", booktabs = T, caption.placement = "top")

# Morph
stMorph <- metLen.Hei %>% 
  group_by(Morph) %>% 
  summarise(n=n(),
            mean(Len), median(Len), sd(Len), min(Len), max(Len),
            mean(Wid), median(Wid), sd(Wid), min(Wid), max(Wid),
            mean(Hei), median(Hei), sd(Hei), min(Hei), max(Hei))
stMorph <- cbind(stMorph$Morph, round(stMorph[,2:17], 2)) %>% 
  as.data.frame() %>% 
  t()

stMorphMas <- metMas %>% 
  group_by(Morph) %>% 
  summarise(n=n(), mean(Mas), median(Mas), sd(Mas), min(Mas), max(Mas))
stMorphMas <- cbind(stMorphMas$Morph, round(stMorphMas[,2:7], 2)) %>% 
  as.data.frame() %>% 
  t()

colnames(stMorph) <- as.character(stMorph[1, ])
stMorph <- as.data.frame(stMorph[-1, ]) %>% 
  xtable(caption = "Popisná statistika -- délka, šířka, výška, seskupeno dle typu")
# print(stMorph, file = "../../Text/9Priloha/Tab/stMorph.txt", booktabs = T, caption.placement = "top")

colnames(stMorphMas) <- as.character(stMorphMas[1, ])
stMorphMas <- as.data.frame(stMorphMas[-1, ]) %>% 
  xtable(caption = "Popisná statistika -- hmotnost, seskupeno dle typu")
# print(stMorphMas, file = "../../Text/9Priloha/Tab/stMorphMas.txt", booktabs = T, caption.placement = "top")

# PLOTS ---------------------------
## First some manipulation
met <- as.tibble(met)
met <- met %>%
  mutate(rWid = (Wid / Len) * 100, rHei = (Hei / Wid) * 100)
met$rWid <- round(met$rWid, 2)
met$rHei <- round(met$rHei, 2)

met1 <- filter(met, Orig == "Dep") %>% 
  mutate(Gro = Orig)
met2 <- filter(met, Orig == "Bur" | Orig == "Set") %>% 
  mutate(Gro = Lok )
met <- rbind(met1, met2)
met <- filter(met, !Lok == "Nit")

rm(list = "met1", "met2")

# Violin plots for L/W/H/M/rW/rH - descriptive ========================
# BW
ViolL <- met %>% 
  ggplot(aes(x = Gro, y = Len)) +
  geom_violin(alpha = 0.8, color = "gray30") + 
  geom_jitter(position = position_jitter(width = 0.04), pch = 21, col = "gray30", bg = "gray90") +
  theme(legend.position = "none") +
  ylab("Len (mm)") + 
  xlab("Lokalita")

ViolW <- met %>% 
  ggplot(aes(x = Gro, y = Wid)) +
  geom_violin(alpha = 0.8, color = "gray30") + 
  geom_jitter(position = position_jitter(width = 0.04), pch = 21, col = "gray30", bg = "gray90") +
  theme(legend.position = "none") +
  ylab("Wid (mm)") + 
  xlab("Lokalita")

ViolH <- met %>% 
  ggplot(aes(x = Gro, y = Hei)) +
  geom_violin(alpha = 0.8, color = "gray30") + 
  geom_jitter(position = position_jitter(width = 0.04), pch = 21, col = "gray30", bg = "gray90") +
  theme(legend.position = "none") +
  ylab("Hei (mm)") + 
  xlab("Lokalita")

ViolM <- met %>% 
  ggplot(aes(x = Gro, y = Mas)) +
  geom_violin(alpha = 0.8, color = "gray30") + 
  geom_jitter(position = position_jitter(width = 0.04), pch = 21, col = "gray30", bg = "gray90") + 
  coord_cartesian(ylim=c(0, 1100)) +
  theme(legend.position = "none") +
  ylab("Mas (g)") + 
  xlab("Lokalita")

ViolrW <- met %>% 
  ggplot(aes(x = Gro, y = rWid)) +
  geom_violin(alpha = 0.8, color = "gray30") + 
  geom_jitter(position = position_jitter(width = 0.04), pch = 21, col = "gray30", bg = "gray90") +
  coord_cartesian(ylim=c(0, 80)) +
  theme(legend.position = "none") +
  ylab("rel. Wid") + 
  xlab("Lokalita")

ViolrH <- met %>% 
  ggplot(aes(x = Gro, y = rHei)) +
  geom_violin(alpha = 0.8, color = "gray30") + 
  geom_jitter(position = position_jitter(width = 0.04), pch = 21, col = "gray30", bg = "gray90") +
  coord_cartesian(ylim=c(0, 200)) +
  theme(legend.position = "none") +
  ylab("rel. Hei") + 
  xlab("Lokalita") + 
  geom_hline(yintercept = 80, linetype = "dotted") + 
  geom_hline(yintercept = 115, linetype = "dotted")
    # Dotted line - border between low, middle and tall shoe-last adzes 
    # (acc. to Velímský 1967, 27; Štelcl & Malina ?, 124)

met <- met[-135, ]

pdf(file = "./Obr/nope/viol_dat.pdf")
met %>% 
  ggplot(aes(x = Gro, y = rHei)) +
  geom_violin(alpha = 0.8, color = "gray30") + 
  geom_jitter(aes(color = met$Dat), position = position_jitter(width = 0.04), pch = 20, bg = "gray90") +
  coord_cartesian(ylim=c(0, 200)) +
  ylab("rel. Hei") + 
  xlab("Lokalita") + 
  geom_hline(yintercept = 80, linetype = "dotted") + 
  geom_hline(yintercept = 115, linetype = "dotted")
dev.off()

pdf(file = "./Obr/nope/viol_CultDat.pdf")
met %>% 
  ggplot(aes(x = Gro, y = rHei)) +
  geom_violin(alpha = 0.8, color = "gray30") + 
  geom_jitter(aes(color = met$CultDat), position = position_jitter(width = 0.04), pch = 20, bg = "gray90") +
  coord_cartesian(ylim=c(0, 200)) +
  ylab("rel. Hei") + 
  xlab("Lokalita") + 
  geom_hline(yintercept = 80, linetype = "dotted") + 
  geom_hline(yintercept = 115, linetype = "dotted")
dev.off()

# # Export Violin plots
pdf(file = "ViolinPlots.pdf", height = 10, width = 8, pointsize = 18)
  grid.arrange(ViolL, ViolW, ViolH, ViolM, ViolrW, ViolrH)
dev.off()

# rm(list = "ViolL", "ViolW", "ViolH", "ViolM", "ViolrH", "ViolrW")

# # COL
# # Asi zbytečné
# met %>% 
#   ggplot(aes(x = Orig, y = Len)) +
#   geom_violin(alpha = 0.5, color = "gray") + 
#   geom_jitter(alpha = 0.5, position = position_jitter(width = 0.04), 
#               aes(color = Lok)) + 
#   theme(legend.position = "none") +
#   ylab("Délka (mm)") + 
#   xlab("Lokalita")

# Scatterplots ================================================
# # create separate datasets for Ad a Ax
metAd <- filter(met, met$Morph == "Ad")
metAx <- filter(met, met$Morph == "Ax")
metWed <- filter(met, met$Morph == "We")

metBl <- filter(met, met$OpSeq == "Bl")
metFT <- filter(met, met$OpSeq == "FT")
metWa <- filter(met, met$OpSeq == "Wa")

# # Scatterplot Délka vs Šířka, asi k ničemu
# ggplot() +
#   geom_point(data = metAd, aes(x = Len, y = Wid, color = OpSeq), pch = 16) +
#   geom_point(data = metAx, aes(x = Len, y = Wid, color = OpSeq), pch = 17) +
#   geom_point(data = metWed, aes(x = Len, y = Wid, color = OpSeq), pch = 15) +
#   geom_density2d(data = metFT, aes(x = Len, y = Wid), alpha = 0.3, color = "forestgreen") +
#   geom_density2d(data = metBl, aes(x = Len, y = Wid), alpha = 0.3, color = "red2")
#   coord_cartesian(xlim = c(0, 100), ylim=c(0, 200)) +
#   labs(x = "Len", y = "Wid") +
#   theme(legend.position = "right")

# Scatterplot rel. W vs rel. H - k čemu je to dobré?
# ggplot() +
#   geom_point(data = metAd, aes(x = rWid, y = rHei), pch = 21, show.legend = TRUE) +
#   geom_point(data = metAx, aes(x = rWid, y = rHei), pch = 24, show.legend = TRUE) +
#   geom_point(data = metWed, aes(x = rWid, y = rHei), pch = 22, show.legend = TRUE) +
#   geom_density2d(data = metAd, aes(x = rWid, y = rHei), alpha = 0.6, color = "gray60") +
#   geom_density2d(data = metAx, aes(x = rWid, y = rHei), alpha = 0.4, color = "gray60") + 
#   coord_cartesian(xlim = c(0, 100), ylim=c(0, 200)) +
#   labs(x = "rel. Wid", y = "rel. Hei") +
#   theme(legend.position = "right")

# # See Pavlu & Rulf 1991, 314-315
# # - Kopytovité sekery mají apriori nižší hodnoty relativní výšky, protože jsou ploché
# # - rHei < 50 cca, viz graf Pavlů & Rulf 1991
pdf(file = "Scat_Wid_rHei.pdf")
Wid.rHei <- met %>%
  ggplot(aes(x = Wid, y = rHei)) +
  geom_point(aes(shape = Morph, color = OpSeq)) +
  geom_density2d(data = metBl, aes(x = Wid, y = rHei), alpha = 0.2, color = "red2") +
  geom_density2d(data = metFT, aes(x = Wid, y = rHei), alpha = 0.2, color = "forestgreen") +
  geom_hline(yintercept = 50, linetype = "dotted") +
  coord_cartesian(ylim=c(0, 200)) +
  labs(x = "Wid (mm)", y = "rel. Hei") +
  theme(legend.position = c(0.95, 0.95), legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6))
dev.off()

pdf(file = "Scat_Wid_rWid.pdf")
Wid.rWid <- met %>%
  ggplot(aes(x = Wid, y = rWid)) +
  geom_point(aes(shape = Morph, color = OpSeq)) +
  geom_density2d(data = metBl, aes(x = Wid, y = rWid), alpha = 0.2, color = "red2") +
  geom_density2d(data = metFT, aes(x = Wid, y = rWid), alpha = 0.2, color = "forestgreen") +
  coord_cartesian(ylim = c(0, 75)) +
  labs(x = "Wid (mm)", y = "rel. Wid") +
  theme(legend.position = c(0.05, 0.95), legend.justification = c("left", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6))
dev.off()

pdf(file = "Scat_Wid.pdf", width = 13, height = 7)
  grid.arrange(Wid.rHei, Wid.rWid, ncol = 2)
dev.off()

# Only Adzes --------
# # metAd
AdBl <- filter(metAd, metAd$OpSeq == "Bl")
AdFT <- filter(metAd, metAd$OpSeq == "FT")
AdWa <- filter(metAd, metAd$OpSeq == "Wa")

# metAd %>% 
#   ggplot(aes(x = Len, y = Wid)) +
#   geom_point(aes(shape = OpSeq), fill = NA) +
#   scale_shape(solid = FALSE) +
#   stat_smooth(method = "lm")

# metAd %>%
#   ggplot(aes(x = Wid, y = rWid)) +
#   geom_point(aes(shape = OpSeq)) +
#   scale_shape_manual(values=c(23, 4, 9)) +
#   coord_cartesian(ylim = c(0, 55)) +
#   geom_density2d(data = AdBl, aes(x = Wid, y = rWid), alpha = 0.4, color = "red2") +
#   geom_density2d(data = AdFT, aes(x = Wid, y = rWid), alpha = 0.4, color = "forestgreen")

pdf(file = "Scat_Ad_Len_rHei.pdf")
metAd %>% 
  ggplot(aes(x = Len, y = rHei)) +
  geom_point(aes(shape = OpSeq, color = OpSeq)) +
  scale_shape_manual(values=c(23, 4, 9)) +
  geom_density2d(data = AdBl, aes(x = Len, y = rHei), alpha = 0.4, color = "red2") +
  geom_density2d(data = AdFT, aes(x = Len, y = rHei), alpha = 0.4, color = "forestgreen") +
  theme(legend.position = c(0.95, 0.05), legend.justification = c("right", "bottom"),
        legend.box.just = "right", legend.margin = margin(6, 6, 6, 6)) +
  labs(caption = "(Ad only)", x = "Len (mm)", y = "rel. Hei")
dev.off()
  
# # Histogram četnosti rHei (viz Pavlů 1991) ================
# # - asi celkem k ničemu
# metAd %>%
#   ggplot() +
#   geom_histogram(aes(x = Wid), binwidth = 5, alpha = 0.5, col = "black") 
#   geom_vline(xintercept = 95, linetype = "dotted") +
#   geom_vline(xintercept = 135, linetype = "dotted") +
#   labs(x = "Relativní výška", y = "Četnost")

# # Violin plots for Adzes only?
ViolAdL <- metAd %>% 
  ggplot(aes(x = Gro, y = Len)) +
  geom_violin(alpha = 0.8, color = "gray30") + 
  geom_jitter(position = position_jitter(width = 0.04), pch = 21, col = "gray30", bg = "gray90") +
  theme(legend.position = "none") +
  ylab("Len (mm)") + 
  xlab("Lokalita")

ViolAdW <- metAd %>% 
  ggplot(aes(x = Gro, y = Wid)) +
  geom_violin(alpha = 0.8, color = "gray30") + 
  geom_jitter(position = position_jitter(width = 0.04), pch = 21, col = "gray30", bg = "gray90") +
  theme(legend.position = "none") +
  ylab("Wid (mm)") + 
  xlab("Lokalita")

ViolAdH <- metAd %>% 
  ggplot(aes(x = Gro, y = Hei)) +
  geom_violin(alpha = 0.8, color = "gray30") + 
  geom_jitter(position = position_jitter(width = 0.04), pch = 21, col = "gray30", bg = "gray90") +
  coord_cartesian(ylim = c(15, 60)) +
  theme(legend.position = "none") +
  ylab("Hei (mm)") + 
  xlab("Lokalita")

ViolAdM <- metAd %>% 
  ggplot(aes(x = Gro, y = Mas)) +
  geom_violin(alpha = 0.8, color = "gray30") + 
  geom_jitter(position = position_jitter(width = 0.04), pch = 21, col = "gray30", bg = "gray90") +
  coord_cartesian(ylim=c(0, 1100)) +
  theme(legend.position = "none") +
  ylab("Mas (g)") + 
  xlab("Lokalita")

ViolAdrW <- metAd %>% 
  ggplot(aes(x = Gro, y = rWid)) +
  geom_violin(alpha = 0.8, color = "gray30") + 
  geom_jitter(position = position_jitter(width = 0.04), pch = 21, col = "gray30", bg = "gray90") +
  coord_cartesian(ylim=c(8, 50)) +
  theme(legend.position = "none") +
  ylab("rel. Wid") + 
  xlab("Lokalita")

ViolAdrH <- metAd %>% 
  ggplot(aes(x = Gro, y = rHei)) +
  geom_violin(alpha = 0.8, color = "gray30") + 
  geom_jitter(position = position_jitter(width = 0.04), pch = 21, col = "gray30", bg = "gray90") +
  coord_cartesian(ylim=c(40, 160)) +
  theme(legend.position = "none") +
  ylab("rel. Hei") + 
  xlab("Lokalita") + 
  geom_hline(yintercept = 80, linetype = "dotted") + 
  geom_hline(yintercept = 115, linetype = "dotted")
# Dotted line - border between low, middle and tall shoe-last adzes 
# (acc. to Velímský 1967, 27; Štelcl & Malina ?, 124)

# # Export Violin plots
pdf(file = "ViolinPlotsAdzes.pdf", height = 10, width = 8, pointsize = 18)
grid.arrange(ViolAdL, ViolAdW, ViolAdH, ViolAdM, ViolAdrW, ViolAdrH)
dev.off()

rm(list = ls())
graphics.off()
