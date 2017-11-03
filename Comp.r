# Comparing pictures from literature with those made directly

# libs ----
library("Momocs")

# input ----
inNProf <- list.files("./Obr/Porovnani/Prof/", full.names = T)
inNSide <- list.files("./Obr/Porovnani/Side/", full.names = T)
inNTop <- list.files("./Obr/Porovnani/Top/", full.names = T)

inProf <- import_jpg(jpg.path = inNProf, auto.notcentered = TRUE, 
                     threshold = 0.5, verbose = TRUE)

inSide <- import_jpg(jpg.path = inNSide, auto.notcentered = TRUE,
                     threshold = 0.5,verbose = TRUE)

inTop <- import_jpg(jpg.path = inNTop, auto.notcentered = TRUE,
                    threshold = 0.5, verbose = TRUE)

# fac ----
facSlot <- data.frame(factor(c(rep("Lit", 10), rep("Orig", 10))))
names(facSlot) <- "Puv"
facSlot$Nr <- factor(rep(0:9, 2))

# outlines ----
outProf <- Out(inProf, fac = facSlot)
outSide <- Out(inSide, fac = facSlot)
outTop <- Out(inTop, fac = facSlot)

panel(outProf, fac = "Puv", names = "Nr")
panel(outSide, fac = "Puv", names = "Nr")
panel(outTop, fac = "Puv", names = "Nr")

# manipulate outlines ----
manProf <- outProf %>%
  coo_slidedirection("S") %>% coo_center() %>% coo_scale() %>% 
  coo_smooth(50) %>% coo_sample(200)

manSide <- outSide %>%
  coo_alignxax() %>% coo_slidedirection("W") %>% coo_center() %>%
  coo_smooth(200) %>% coo_sample(200)

manTop <- outTop %>%
  coo_alignxax() %>% coo_slidedirection("W") %>% coo_center() %>%
  coo_smooth(200) %>% coo_sample(400)

panel(manProf, fac = "Puv", names = "Nr")
panel(manTop, fac = "Puv", names = "Nr")
panel(manSide, fac = "Puv", names = "Nr")


# stack(filter(manProf, Nr == 5))
# 
# stack(filter(manTop, Nr == 1))
# stack(filter(manTop, Nr == 2))
# stack(filter(manTop, Nr == 3))

# efourier ----
efProf <- efourier(manProf, nb.h = 10, norm = FALSE, smooth.it = 0,
                   start = FALSE, verbose = TRUE)

efSide <- efourier(manSide, nb.h = 11, norm = TRUE, smooth.it = 0,
                   start = FALSE, verbose = TRUE)

efTop <- efourier(manTop, nb.h = 12, norm = TRUE, smooth.it = 0,
                  start = FALSE, verbose = TRUE)

# centroid size ----
# coo_centsize(manProf)
# coo_centsize(manSide)
# coo_centsize(manTop)

# mean shapes ----
Prof <- mshapes(efProf, 1)
Side <- mshapes(efSide, 1)
Top <- mshapes(efTop, 1)

# stack plots of meanshapes ----
# pdf(file = "./Obr/prof.pdf")
# LitProf <- Prof$shp$Lit    %T>% coo_plot(border="blue")
# KreProf <- Prof$shp$Orig  %T>% coo_draw(border="red")
# dev.off()
# 
# pdf(file = "./Obr/side.pdf")
# LitSide <- Side$shp$Lit    %T>% coo_plot(border="blue")
# KreSide <- Side$shp$Orig  %T>% coo_draw(border="red")
# dev.off()
# 
# pdf(file = "./Obr/top.pdf")
# LitTop <- Top$shp$Lit    %T>% coo_plot(border="blue")
# KreTop <- Top$shp$Orig  %T>% coo_draw(border="red")
# dev.off()

# difference plots of meanshapes ----
pdf(file = "./Obr/panProf.pdf")
efProf %>% mshapes("Puv") %>% plot_mshapes(col2="#0000FF")
dev.off()

pdf(file = "./Obr/panSide.pdf")
efSide %>% mshapes("Puv") %>% plot_mshapes(col2="#0000FF")
dev.off()

pdf(file = "./Obr/panTop.pdf")
efTop %>% mshapes("Puv") %>% plot_mshapes(col2="#0000FF")
dev.off()

# tps plots ----
pdf(file = "./Obr/isoProf.pdf", width = 10, height = 10)
tps_iso(LitProf, KreProf, cont.col = "gray40", grid = F, 
        shp.border = c("black", "blue"), shp.lwd = c(4, 3), shp.lty = c(3, 3),
        legend.text = c("Lit", "Orig"))
dev.off()

pdf(file = "./Obr/isoSide.pdf", width = 9, height = 3)
tps_iso(LitSide, KreSide, cont.col = "gray40", grid = F, 
        shp.border = c("black", "blue"), shp.lwd = c(4, 3), shp.lty = c(3, 3),
        legend.text = c("Lit", "Orig"))
dev.off()

pdf(file = "./Obr/isoTop.pdf", width = 11, height = 4)
tps_iso(LitTop, KreTop, cont.col = "gray40", grid = F, 
        shp.border = c("black", "blue"), shp.lwd = c(4, 3), shp.lty = c(3, 3),
        legend.text = c("Lit", "Orig"))
dev.off()

# indiv.
tps_iso(manProf$coo$LitProf1, manProf$coo$OrigProf1)

coo_lolli(LitProf, KreProf)
coo_lolli(LitSide, KreSide)
coo_lolli(LitTop, KreTop)
# or?
coo_arrows(LitProf, KreProf)
coo_arrows(LitSide, KreSide)
coo_arrows(LitTop, KreTop)

# coo_plot(LitProf)
# coo_ruban(LitProf, edm(LitProf, KreProf), lwd=8)
# coo_ruban(LitTop, edm(LitTop, KreTop), lwd=8)

# a <- efourier(manTop)
# ms.Top <- mshapes(a, fac = "Puv", nb.pts = 80)$shp
# fr <- ms.Top$Lit
# to <- ms.Top$Orig
# tps_iso(fr, to)

# end
rm(list = ls())
graphics.off()
