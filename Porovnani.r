library("Momocs")

inNProf <- list.files("./Morpho/Porovnani/Prof/", full.names = T)
inNSide <- list.files("./Morpho/Porovnani/Side/", full.names = T)
inNTop <- list.files("./Morpho/Porovnani/Top/", full.names = T)

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

facSLot <- data.frame(factor(c("Lit", "Kre")))
names(facSLot) <- "Puv"

outProf <- Out(inProf, fac = facSLot)
outSide <- Out(inSide, fac = facSLot)
outTop <- Out(inTop, fac = facSLot)

stack(manProf)

manProf <- outProf %>%
  coo_slidedirection("N") %>%
  coo_center() %>%
  coo_scale() %>% 
  coo_smooth(50) %>%
  coo_sample(200)
?coo_scale
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

stack(manProf)
panel(manProf)
stack(manTop)
panel(manTop)
stack(manSide)
panel(manSide)

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
  nb.h = 11,
  norm = F,
  smooth.it = 0,
  start = FALSE,
  verbose = TRUE
)
efTop <- efourier(
  manTop,
  nb.h = 12,
  norm = F,
  smooth.it = 0,
  start = FALSE,
  verbose = TRUE
)

panel(efProf)
panel(manTop)
panel(efTop)

stack(manTop)

coo_centsize(manProf)
coo_centsize(manSide)
coo_centsize(manTop)

Prof <- mshapes(efProf, 1)
Side <- mshapes(efSide, 1)
Top <- mshapes(efTop, 1)

LitProf <- Prof$shp$Kre    %T>% coo_plot(border="blue")
KreProf <- Prof$shp$Lit  %T>% coo_draw(border="red")

LitSide <- Side$shp$Kre    %T>% coo_plot(border="blue")
KreSide <- Side$shp$Lit  %T>% coo_draw(border="red")

LitTop <- Top$shp$Kre    %T>% coo_plot(border="blue")
KreTop <- Top$shp$Lit  %T>% coo_draw(border="red")

efProf %>% mshapes("Puv") %>% plot_mshapes
efSide %>% mshapes("Puv") %>% plot_mshapes
efTop %>% mshapes("Puv") %>% plot_mshapes

coo_arrows(LitSide, KreSide)
tps_iso(LitProf, KreProf)

coo_lolli(LitProf, KreProf)
coo_lolli(LitSide, KreSide)

coo_plot(LitTop)

