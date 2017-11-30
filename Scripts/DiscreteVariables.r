# Discrete vars
# - Basic plots of discrete variables

# LIBRARIES =======================
library(tidyverse)
library(xtable)
library(gridExtra)
library(stringr)

# DATA ============================
prime <- read_csv("../Db.PRIME2.csv")

# MANIPULATION ====================
secon <- select(prime, ID:Lok, Orig:Pouzitelnost)

# Mutate Cult + Dat
secon <- secon %>% 
  mutate(CultDat = paste(Cult, Dat, sep = " "))

# String to edit CultDat
secon$CultDat <- str_replace(secon$CultDat, pattern = "(.*) NA", replacement = "\\1")

# Factors
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
secon$CultDat <- as.factor(secon$CultDat)

# Filter
# - Adzes == Adzes + Wedzes
Adzes <- filter(secon, Morph == "Ad" | Morph == "Wed")

# Tables 
CultDat <- Adzes %>% count(CultDat)
Orig <- Adzes %>% count(Orig)
OpSeq <- Adzes %>% count(OpSeq)
RM <- Adzes %>% count(RM)
StUpev <- Adzes %>% count(StUpev)
StPrac <- Adzes %>% count(StPrac)
Pouz <- Adzes %>% count(Pouzitelnost)
Half <- Adzes %>% count(Half)
SalProfVys <- Adzes %>% count(SalProfVys)
SalProfTvar <- Adzes %>% count(SalProfTvar)
SalTylBok <- Adzes %>% count(SalTylBok)

# PLOTTING ========================
# # Define colors
paletka <- c("#ffffb2", "#fecc5c", "#fd8d3c", "#e31a1c", "#6baed6", "#c2e699", "#78c679")

# pdf(file = "../../Text/Obr/Datace.pdf", width = 5, height = 3)
#   ggplot() +
#     geom_bar(data = Adzes, aes(x = Cult, fill = CultDat), stat = "count") +
#     scale_fill_manual(values = paletka, guide_legend(title = "Datace")) +
#     labs(title = "Rel. datace") +
#     # ,
#     #       legend.position = c(0.95, 0.95), legend.justification = c("right", "top")) +
#     theme(axis.title.x=element_blank(), axis.title.y = element_blank()) +
#     coord_flip() +
#     annotate("text", x = 1, y = 8, label = "LbK II", size = 2.4) +
#     annotate("text", x = 1, y = 22, label = "LbK I", size = 2.4)
# dev.off()

perc.OpSeq <- Adzes %>% 
  group_by(Orig, OpSeq) %>% 
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))

# pdf(file = "../../Text/Obr/OpSeq.pdf", width = 5, height = 3)
# ggplot(data = perc.OpSeq, aes(x = Orig, y = perc, fill = OpSeq)) +
#   geom_bar(stat="identity", width = 0.5) +
#   scale_y_continuous(labels= scales::percent) +
#   scale_fill_manual(values = paletka, guide_legend(title = "OpSeq")) +
#   labs(title = "Oper. sekvence") +
#   theme(axis.title.x=element_blank(), axis.title.y = element_blank()) +
#   coord_flip()
# dev.off() 

perc.Half <- Adzes %>% 
  group_by(Orig, Half) %>% 
  summarise(count = n()) %>% 
  mutate(perc=count/sum(count))

# ggplot(data = perc.Half, aes(x = Orig, y = count, fill = Half)) +
#     geom_bar(stat="identity", width = 0.5)

pdf(file = "../../Text/Obr/Dat_OpSeq.pdf", width = 10, height = 3)
grid.arrange(
ggplot() +
  geom_bar(data = Adzes, aes(x = Cult, fill = CultDat), stat = "count") +
  scale_fill_manual(values = paletka, guide_legend(title = "Datace")) +
  labs(title = "Rel. datace") +
  # ,
  #       legend.position = c(0.95, 0.95), legend.justification = c("right", "top")) +
  theme(axis.title.x=element_blank(), axis.title.y = element_blank()) +
  coord_flip() +
  annotate("text", x = 1, y = 8, label = "LbK II", size = 2.4) +
  annotate("text", x = 1, y = 22, label = "LbK I", size = 2.4),
ggplot(data = perc.OpSeq, aes(x = Orig, y = perc, fill = OpSeq)) +
  geom_bar(stat="identity", width = 0.5) +
  scale_y_continuous(labels= scales::percent) +
  scale_fill_manual(values = paletka, guide_legend(title = "OpSeq")) +
  labs(title = "Oper. sekvence") +
  theme(axis.title.x=element_blank(), axis.title.y = element_blank()) +
  coord_flip(),
nrow = 1)
dev.off()

# ggplot() +
#   geom_bar(data = Adzes, aes(x = Orig), stat = "count")
# 
# ggplot() +
#   geom_bar(data = Adzes, aes(x = OpSeq), stat = "count")
# 
# ggplot() +
#   geom_bar(data = Adzes, aes(x = RM), stat = "count")
# 
# ggplot() +
#   geom_bar(data = Adzes, aes(x = StUpev), stat = "count")
# 
# ggplot() +
#   geom_bar(data = Adzes, aes(x = StPrac), stat = "count")
# 
# ggplot() +
#   geom_bar(data = Adzes, aes(x = Pouzitelnost), stat = "count")
# 
# ggplot() +
#   geom_bar(data = Adzes, aes(x = Half), stat = "count")
# 
# ggplot() +
#   geom_bar(data = Adzes, aes(x = SalProfVys), stat = "count")
# 
# ggplot() +
#   geom_bar(data = Adzes, aes(x = SalProfTvar), stat = "count")
# 
# ggplot() +
#   geom_bar(data = Adzes, aes(x = SalTylBok), stat = "count")

# ggplot(CultDat, aes(x = CultDat, y = n, label = n)) + 
#   geom_point(size = 6) +
#   geom_text(color = "white", size =3) +
#   geom_segment(aes(x=CultDat, xend=CultDat, 
#                    y=0, yend= n - 0.5)) + 
#   coord_flip()

# ggplot(Orig, aes(x = Orig, y = n, label = n)) + 
#   geom_point(size = 6) +
#   geom_text(color = "white", size =3) +
#   geom_segment(aes(x=Orig, xend=Orig, 
#                    y=0, yend= n - 0.5)) +
#   xlab("orig") +
#   coord_flip()

GrafLizatka <- function(inputDataFrame, inputColumn, osax) {
  ggplot(inputDataFrame, aes(x = inputColumn, y = n, label = n)) + 
    geom_point(size = 6) +
    geom_text(color = "white", size =3) +
    geom_segment(aes(x=inputColumn, xend=inputColumn, 
                     y=0, yend= n - 1.2)) + 
    labs(title = osax) + 
    theme(axis.title.x=element_blank(), axis.title.y = element_blank()) +
    coord_flip()
}

# GrafLizatka(CultDat, CultDat$CultDat, "datace")
# GrafLizatka(Orig, Orig$Orig, "orig")
# GrafLizatka(OpSeq, OpSeq$OpSeq, "OpSeq")
GrRM <- GrafLizatka(RM, RM$RM, "1. surovina")
GrSt1 <- GrafLizatka(StUpev, StUpev$StUpev, "2. stopy upev.")
GrSt2 <- GrafLizatka(StPrac, StPrac$StPrac, "3. prac. stopy")
# GrafLizatka(Pouz, Pouz$Pouzitelnost, "Pouz.")
# GrafLizatka(Half, Half$Half, "Half")
GrSal1 <- GrafLizatka(SalProfVys, SalProfVys$SalProfVys, "1. var. profilu (ProfVys)")
GrSal2 <- GrafLizatka(SalProfTvar, SalProfTvar$SalProfTvar, "2. var. tvaru profilu (ProfTvar)")
GrSal3 <- GrafLizatka(SalTylBok, SalTylBok$SalTylBok, "3. var. tyla (TylBok)")

# Arrange grid to print out
pdf(file = "../../Text/Obr/RM_StopyKK.pdf", width = 10, height = 2.5)
  grid.arrange(GrRM, GrSt1, GrSt2, nrow = 1)
dev.off()

pdf(file = "../../Text/Obr/VariantyKK.pdf", width = 10, height = 2.5)
  grid.arrange(GrSal1, GrSal2, GrSal3, nrow = 1)
dev.off()

# Pouzitelnost
# AdzesOnly <- filter(secon, Morph == "Ad")
# AdzesOnly[89, 18] <- as.factor("F")
# AdzesOnly[89, 18]
# 
# pdf(file = "./Adzes/pouz.pdf", width = 6, height = 3)
# ggplot(AdzesOnly, aes(Orig)) +
#   geom_bar(aes(fill = Pouzitelnost), width = 0.6, stat = "count") +
#   scale_fill_brewer(palette = "Spectral", guide_legend(title = "Pouz.")) +
#   theme(axis.title.x=element_blank(), axis.title.y = element_blank(),
#         legend.position = c(0.95, 0.95), legend.justification = c("right", "top")) +
#   coord_flip()
# dev.off()
  
rm(list = ls())
graphics.off()
