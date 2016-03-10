###################################################################################
###################################################################################
##
##
##
##
##
##   Compara Focos de calor entre 2016 e 2015, para avaliar em que estados houve
## o maior crescimento
##
##
##
##
##
###################################################################################
## 
##   Data de elaboração do script: 2016-03-10
##
###################################################################################
###################################################################################












###################################################################################
#  Salva a figura que está ativa (dev.cur()) em vários formatos
###################################################################################
#
# width e height em cm (1 in = 2.54 cm)
# pointsize: relação entre o tamanho do texto e os pontos que compõe o gráfico
#
###################################################################################
saveg <- function(nome.arq, width = 20, height = 10, pointsize=8, ppi=300) {
  
  dev.copy(png, file = paste(nome.arq, ".png", sep=""),
           width = width, height = height, pointsize = pointsize, units="cm", res=ppi) #
  dev.off()
  
  dev.copy(pdf, file = paste(nome.arq, ".pdf", sep=""), 
           width = width/2.54, height = height/2.54, pointsize = pointsize) # o padrão é em "in"; 
  dev.off()                                                                 # 1 in = 2.54 cm
    
  dev.copy(win.metafile, file = paste(nome.arq, ".wmf", sep=""),
           width = width/2.54, height = height/2.54, pointsize = pointsize) #
  dev.off()
  
  dev.copy(postscript, file = paste(nome.arq, ".ps", sep=""),
           width = width/2.54, height = height/2.54, pointsize = pointsize) #
  dev.off()
}
###################################################################################
#   FIM DA FUNÇÃO
###################################################################################















###################################################################################
###################################################################################
##
##     INÍCIO DO SCRIPT
##
###################################################################################
###################################################################################

setwd("e:/Base de dados/Focos_de_calor/zzz_scriptsR_focos/Focos2015X2016_Kernel")


# período de análise dos focos: este ano Vs ano anterior
periodo.analise <- "01/01 a 09/03"


# ---------- rgdal ---------- #

library(rgdal)
library(KernSmooth)
library(ggplot2)
library(GISTools) #densidade Kernel
library(spatstat) # as.ppp,density, contour

br.rg <- readOGR("shp", "BR_ESTADOS_IBGE")

f.2015 <- readOGR("./shp/focos", "Focos20150101a20150309_Brasil_925")
f.2016 <- readOGR("./shp/focos", "Focos20160101a20160309_Brasil_62")

brif2015 <- readOGR("./shp/brigadas", "BRIF2015")

# note that readOGR will read the .prj file if it exists
proj4string(f.2015)
proj4string(br.rg)
proj4string(brif2015)

# altera a projeção
#br.rg    <- spTransform(br.rg,    CRS(proj4string(f.2015)))
#brif2015 <- spTransform(brif2015, CRS(proj4string(f.2015)))

# altera a projeção para "South_America_Equidistant_Conic", copiada do QGIS
# spTransform(f.2015, "+proj=eqdc +lat_0=-32 +lon_0=-60 +lat_1=-5 +lat_2=-42 +x_0=0 +y_0=0 +ellps=aust_SA +units=m +no_defs")

# limpa focos para só ter focos do Brasil
f.2015 <- f.2015[which(f.2015$Pais == "Brasil"), ]
f.2016 <- f.2016[which(f.2016$Pais == "Brasil"), ]


#----------------------------------------------------------------------
# mapa: comparação 2014 X 2015
#----------------------------------------------------------------------
dev.new(noRStudioGD = T)
par(mfrow=c(1, 2), mar=c(2, 2.2, 3.5, 0.25), cex.main=1.1)

plot(br.rg, axes=TRUE, border="gray", main = paste(nrow(f.2015), " focos de calor\n(" ,periodo.analise, "/2015)", sep=""))
points(f.2015, pch=".", col=2)
plot(br.rg, axes=TRUE, border="black", add=T)

plot(br.rg, axes=TRUE, border="gray", main = paste(nrow(f.2016), " focos de calor\n(" ,periodo.analise, "/2016)", sep=""))
points(f.2016, pch=".", col=2)
plot(br.rg, axes=TRUE, border="black", add=T)

saveg("./graficos/focos2016X2015")


# focos e Brifs 2015
dev.new(noRStudioGD = T)
plot(br.rg, axes=TRUE, border="gray", main ="BRIFs X Focos (de 18 a 24/08/2015)")
points(f.2016, pch=3, col=2)
plot(brif2015, axes=F, bor=as.numeric(brif2015$tipo_brif) ,add=T)

#--------------------------------------------------------------
# Kernel comparativo
#--------------------------------------------------------------
dens.2016 <- kde.points(f.2016, lims = br.rg)
dens.2015 <- kde.points(f.2015, lims = br.rg)

#----------------------------------------------------------------------
# mapa kernel: comparação 2014 X 2015
#----------------------------------------------------------------------
dev.new(noRStudioGD = T)
par(mfrow=c(1, 2), mar=c(2, 2.2, 3.5, 0.25), cex.main=1.1)

# 2015
plot(br.rg, axes=TRUE, border="gray", main = paste("Densidade de focos de calor\n(" ,periodo.analise, "/2015)", sep=""))
level.plot(dens.2015, add=T)
plot(br.rg, axes=TRUE, border="black", add=T)

# 2016
plot(br.rg, axes=TRUE, border="gray", main = paste("Densidade de focos de calor\n(" ,periodo.analise, "/2016)", sep=""))
level.plot(dens.2016, add=T)
plot(br.rg, axes=TRUE, border="black", add=T)

saveg("./graficos/densidadefocos2016X2015")

#----------------------------------------------------------------------
# mapa kernel IMAGEM: comparação 2014 X 2015
#----------------------------------------------------------------------
dev.new(noRStudioGD = T)
par(mfrow=c(1, 2), mar=c(2, 2.2, 3, 0.25), cex.main=1.1)

# 2015
plot(br.rg, axes=TRUE, border="gray", main = paste("Densidade de focos de calor\n(" ,periodo.analise, "/2015)", sep=""))
image(dens.2015, add=T)
plot(br.rg, axes=TRUE, border="black", add=T)

# 2016
plot(br.rg, axes=TRUE, border="gray", main = paste("Densidade de focos de calor\n(" ,periodo.analise, "/2016)", sep=""))
image(dens.2016, add=T)
plot(br.rg, axes=TRUE, border="black", add=T)


#----------------------------------------------------------------------
# mapa kernel do spatstat: comparação 2014 X 2015
#----------------------------------------------------------------------
dev.new(noRStudioGD = T)
par(mfrow=c(1, 2), mar=c(2, 1, 3, 2), cex.main=1)

dens2.2015 <- density(as.ppp(f.2015))
dens2.2016 <- density(as.ppp(f.2016))

plot(dens2.2015, main = paste("Densidade de focos de calor\n\n(" ,periodo.analise, "/2015)", sep=""))
contour(dens2.2015, add=T)
plot(br.rg, add=T)
plot(dens2.2016, main = paste("Densidade de focos de calor\n\n(" ,periodo.analise, "/2016)", sep=""))
contour(dens2.2016, add=T)
plot(br.rg, add=T)

saveg("./graficos/dens_2016X2015_statspat")



#--------------------------------------------------------------
# Gráfico: FOCOS x Uf / ano
#--------------------------------------------------------------

# cria o dataframe
uf2015e2016 <- data.frame(Uf = unique(c(as.character(f.2015$Uf), as.character(f.2016$Uf))),
                          'f2015' = NA,
                          'f2016' = NA)
uf2015e2016$Uf <- as.character(uf2015e2016$Uf)
# retira indeterminado 
uf2015e2016 <- uf2015e2016[uf2015e2016$Uf != "Indeterminado", ]

# calcula qtos focos por estado para cada ano
uf2015e2016$f2015 <- sapply(uf2015e2016$Uf, function(x) {sum(as.character(f.2015$Uf)==x)})
uf2015e2016$f2016 <- sapply(uf2015e2016$Uf, function(x) {sum(as.character(f.2016$Uf)==x)})

# reorganiza o dataframe
(uf2015e2016.reshape <- data.frame(Uf = c(uf2015e2016$Uf, uf2015e2016$Uf),
                                   ano = c(rep(2015, nrow(uf2015e2016)), rep(2016, nrow(uf2015e2016))),
                                   focos = c(uf2015e2016$f2015, uf2015e2016$f2016)))

uf2015e2016.reshape <- uf2015e2016.reshape[order(uf2015e2016.reshape$ano, uf2015e2016.reshape$Uf), ]
# focos 2015
(tot.2015 <- sum(uf2015e2016.reshape$focos[uf2015e2016.reshape$ano == 2015]))
# focos 2015
(tot.2016 <- sum(uf2015e2016.reshape$focos[uf2015e2016.reshape$ano == 2016]))
#calcula porcentagem
uf2015e2016.reshape$porcent.ano <- NA
uf2015e2016.reshape$porcent.ano[uf2015e2016.reshape$ano == 2015] <- 
  100*uf2015e2016.reshape$focos[uf2015e2016.reshape$ano == 2015]/tot.2015
uf2015e2016.reshape$porcent.ano[uf2015e2016.reshape$ano == 2016] <- 
  100*uf2015e2016.reshape$focos[uf2015e2016.reshape$ano == 2016]/tot.2016

# GRAFICO: QTDE FOCOS
dev.new(noRStudioGD = T)
graf.focos <- ggplot(uf2015e2016.reshape, aes(x = Uf, y = focos, fill=as.factor(ano))) + 
              geom_bar(stat = "identity", position=position_dodge(), colour="black")

graf.focos + xlab("Estado") + ylab("Qtde dos focos de calor por UF  (sat. referência)") + 
  scale_fill_hue(name="Ano") + 
  ggtitle(paste("Comparação anual dos focos e calor detectados entre", periodo.analise, sep=" ")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

ggsave("./graficos/hist_UF_tot.pdf", units="cm", width=18, height=12)
ggsave("./graficos/hist_UF_tot.png", units="cm", width=18, height=12, dpi=300)


# GRAFICO: % ANUAL FOCOS
dev.new(noRStudioGD = T)
graf.focos <- ggplot(uf2015e2016.reshape, aes(x = Uf, y = porcent.ano, fill=as.factor(ano))) + 
   geom_bar(stat = "identity", position=position_dodge(), colour="black")

graf.focos + xlab("Estado") + ylab("Porcentagem dos focos de calor por UF (sat. referência)") +
  scale_fill_hue(name="Ano") + 
  ggtitle(paste("Comparação anual dos focos e calor detectados entre", periodo.analise, sep=" ")) +      # legenda title
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

ggsave("./graficos/hist_UF_porc.pdf", units="cm", width=18, height=12)
ggsave("./graficos/hist_UF_porc.png", units="cm", width=18, height=12, dpi=300)



###################################################################################
###################################################################################
##
##     FIM DO SCRIPT
##
###################################################################################
###################################################################################

###################################################################################
#   JAN                                 
f.2015 <- f.2015back[f.2015back$Data <= 20150131, ]

f.2016 <- f.2016back[f.2016back$Data <= 20160131, ]
periodo.analise <- "01/01 a 31/01"

min(f.2015$Data); max(f.2015$Data)
min(f.2016$Data); max(f.2016$Data)

###################################################################################
#   FEV
f.2015 <- f.2015back[f.2015back$Data > 20150131 &
                       f.2015back$Data < 20150301, ]

f.2016 <- f.2016back[f.2016back$Data > 20160131 &
                       f.2016back$Data < 20160301, ]
periodo.analise <- "01/02 a 28-29/02"

min(f.2015$Data); max(f.2015$Data)
min(f.2016$Data); max(f.2016$Data)

###################################################################################
#   MAR
f.2015 <- f.2015back[f.2015back$Data >= 20150301, ]

f.2016 <- f.2016back[f.2016back$Data >= 20160301, ]
periodo.analise <- "01/03 a 09/03"

min(f.2015$Data); max(f.2015$Data)
min(f.2016$Data); max(f.2016$Data)