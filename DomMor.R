library(readr)
library(tidyverse)

votacao_secao_2016_SP <- read_delim("C:/Users/lucas/Google Drive/Acadêmicos/Sciences Po/Master/Research/Data/TSE/votacao_secao_2016_SP.txt", 
                                    ";", escape_double = FALSE, col_names = FALSE, 
                                    locale = locale(encoding = "latin1"), 
                                    trim_ws = TRUE)

votacao_candidato_munzona_2016_SP <- read_delim("C:/Users/lucas/Google Drive/Acadêmicos/Sciences Po/Master/Research/Data/TSE/votacao_candidato_munzona_2016_SP.csv", 
                                                ";", escape_double = FALSE, locale = locale(encoding = "latin1"), 
                                                trim_ws = TRUE)

votacao_secao_2016_SP_sp_ver <- subset(votacao_secao_2016_SP, X9=="SÃO PAULO" & X13=="VEREADOR", select = c(X10, X11, X14, X15))
votacao_candidato_munzona_2016_SP_sp_ver <- unique(subset(votacao_candidato_munzona_2016_SP, NM_MUNICIPIO=="SÃO PAULO" & DS_CARGO=="Vereador", select = c(NM_URNA_CANDIDATO, NR_CANDIDATO, SG_PARTIDO, DS_COMPOSICAO_COLIGACAO, DS_SIT_TOT_TURNO)))

votacao_secao_2016_SP_sp_ver$zs <- paste(votacao_secao_2016_SP_sp_ver$X10,"-",votacao_secao_2016_SP_sp_ver$X11)
votacao_secao_2016_SP_sp_ver$zs <- str_replace(votacao_secao_2016_SP_sp_ver$zs, " ", "")
votacao_secao_2016_SP_sp_ver$zs <- str_replace(votacao_secao_2016_SP_sp_ver$zs, " ", "")

votacao_secao_2016_SP_sp_ver_nom <- subset(votacao_secao_2016_SP_sp_ver, X14>99)

ZONA_SECAO <- read_delim("C:/Users/lucas/Google Drive/Acadêmicos/Sciences Po/Master/Research/Data/csv/ZONA_SECAO.csv", 
                         ";", escape_double = FALSE, locale = locale(encoding = "UTF-8"), 
                         trim_ws = TRUE)

colnames(ZONA_SECAO)[5] <- "zs"
sp_ver_2016 <- select(merge(votacao_secao_2016_SP_sp_ver_nom, ZONA_SECAO, by = "zs", all.x=T), "zs","DISTRITO", "ds_codigo", "X14", "X15")
colnames(sp_ver_2016)[4] <- "NR_CANDIDATO"
colnames(sp_ver_2016)[5] <- "VOTOS"
sp_ver_2016 <- select(merge(sp_ver_2016, votacao_candidato_munzona_2016_SP_sp_ver, by = "NR_CANDIDATO", all.x=T), "NM_URNA_CANDIDATO", "NR_CANDIDATO", "SG_PARTIDO", "DS_COMPOSICAO_COLIGACAO", "zs","DISTRITO", "ds_codigo", "VOTOS", "DS_SIT_TOT_TURNO")
sum(is.na(sp_ver_2016$DISTRITO))
sp_ver_2016_g <- group_by(sp_ver_2016, NM_URNA_CANDIDATO, NR_CANDIDATO, SG_PARTIDO, DS_COMPOSICAO_COLIGACAO, DISTRITO, ds_codigo, DS_SIT_TOT_TURNO)
sp_ver_2016_t <- summarise(sp_ver_2016_g, VOTOS = sum(VOTOS))
sum(is.na(sp_ver_2016_t$DISTRITO))

sum(votacao_secao_2016_SP_sp_ver_nom$X15)
sum(sp_ver_2016_t$VOTOS)

sp_ver_2016_tNA <- na.omit(sp_ver_2016_t)
sp_ver_2016_tNA$A <- NA
sp_ver_2016_tNA$B <- NA

for (i in 1:79564) {
  sp_ver_2016_tNA[i,9] <- (as.double(sp_ver_2016_tNA[i,8])/sum(sp_ver_2016_tNA$VOTOS[sp_ver_2016_tNA$DISTRITO==as.character(sp_ver_2016_tNA[i,5])])) * (as.double(sp_ver_2016_tNA[i,8])/sum(sp_ver_2016_tNA$VOTOS[sp_ver_2016_tNA$NM_URNA_CANDIDATO==as.character(sp_ver_2016_tNA[i,1])]))
  sp_ver_2016_tNA[i,10] <- as.double(sp_ver_2016_tNA[i,8])/sum(sp_ver_2016_tNA$VOTOS[sp_ver_2016_tNA$NM_URNA_CANDIDATO==as.character(sp_ver_2016_tNA[i,1])])
}

sp_ver_2016_tNA_g <- group_by(sp_ver_2016_tNA, NM_URNA_CANDIDATO, NR_CANDIDATO, SG_PARTIDO, DS_COMPOSICAO_COLIGACAO, DS_SIT_TOT_TURNO)
sp_ver_2016_dom <- summarise(sp_ver_2016_tNA_g, VOTOS = sum(VOTOS), A = sum(A), B = sum(B))
sp_ver_2016_dom$dom <- sp_ver_2016_dom$A/sp_ver_2016_dom$B
sp_ver_2016_dom$A <- NULL
sp_ver_2016_dom$B <- NULL
               
library(spdep)
library(ape)

distr <- read_sf("C:/Users/lucas/Google Drive/Acadêmicos/Sciences Po/Master/Research/Data/Shapefiles/SIRGAS_SHP_distrito_polygon.shp")
votes_distr_2012 <- sp_ver_2016_t[,c(1,6,8)]
votes_distr_2016 <- spread(votes_distr_2016, NM_URNA_CANDIDATO,  VOTOS)
votes_distr_2016 <- votes_distr_2016[1:96,]
votes_distr_2016[is.na(votes_distr_2016)] <- 0

m_2016 <- merge(distr, votes_distr_2016, by = "ds_codigo", all.x=T)

m_nbq_2016 <- poly2nb(m_2016)
m_nbq_w_2016 <- nb2listw(m_nbq_2016)

m1_2016 <- as.data.frame(merge(distr, votes_distr_2016, by = "ds_codigo", all.x=T))

sp_ver_2016_mor <- as.data.frame(colnames(m1_2016[3:1276]))
sp_ver_2016_mor$mor <- NA
colnames(sp_ver_2016_mor)[1] <- "NM_URNA_CANDIDATO"

for (i in 1:1274) {

  sp_ver_2016_mor[i,2] <- moran(m1_2016[,i+2], m_nbq_w_2016, 96, 96)
  
}

sp_ver_2016_dom_mor <- select(merge(sp_ver_2016_dom, sp_ver_2016_mor, by = "NM_URNA_CANDIDATO", all.x=T), "NM_URNA_CANDIDATO","dom", "mor", "DS_SIT_TOT_TURNO")

sp_ver_2016_dom_mor_el <- subset(sp_ver_2016_dom_mor, DS_SIT_TOT_TURNO=="ELEITO POR QP" | DS_SIT_TOT_TURNO=="ELEITO POR MÉDIA", select = c("NM_URNA_CANDIDATO","dom", "mor"))
write.csv(sp_ver_2016_dom_mor_el, "DOM_MOR_ELEITOS_2016.csv", fileEncoding = "UTF-8")

sp_ver_2016_dom_mor_su <- subset(sp_ver_2016_dom_mor, DS_SIT_TOT_TURNO=="SUPLENTE", select = c("NM_URNA_CANDIDATO","dom", "mor"))
write.csv(sp_ver_2016_dom_mor_su, "DOM_MOR_SUPL_2016.csv", fileEncoding = "UTF-8")


votacao_secao_2012_SP <- read_delim("C:/Users/lucas/Google Drive/Acadêmicos/Sciences Po/Master/Research/Data/TSE/votacao_secao_2012_SP.txt", 
                                    ";", escape_double = FALSE, col_names = FALSE, 
                                    locale = locale(encoding = "latin1"), 
                                    trim_ws = TRUE)

votacao_candidato_munzona_2012_SP <- read_delim("C:/Users/lucas/Google Drive/Acadêmicos/Sciences Po/Master/Research/Data/TSE/votacao_candidato_munzona_2012_SP.txt", 
                                                ";", escape_double = FALSE, col_names = FALSE, 
                                                locale = locale(encoding = "latin1"), 
                                                trim_ws = TRUE)

votacao_secao_2012_SP_sp_ver <- subset(votacao_secao_2012_SP, X9=="SÃO PAULO" & X13=="VEREADOR", select = c(X10, X11, X14, X15))
votacao_candidato_munzona_2012_SP_sp_ver <- unique(subset(votacao_candidato_munzona_2012_SP, X9=="SÃO PAULO" & X16=="VEREADOR", select = c(X15, X12, X24, X28, X22)))

votacao_secao_2012_SP_sp_ver$zs <- paste(votacao_secao_2012_SP_sp_ver$X10,"-",votacao_secao_2012_SP_sp_ver$X11)
votacao_secao_2012_SP_sp_ver$zs <- str_replace(votacao_secao_2012_SP_sp_ver$zs, " ", "")
votacao_secao_2012_SP_sp_ver$zs <- str_replace(votacao_secao_2012_SP_sp_ver$zs, " ", "")

votacao_secao_2012_SP_sp_ver_nom <- subset(votacao_secao_2012_SP_sp_ver, X14>99)

ZONA_SECAO <- read_delim("C:/Users/lucas/Google Drive/Acadêmicos/Sciences Po/Master/Research/Data/csv/ZONA_SECAO.csv", 
                         ";", escape_double = FALSE, locale = locale(encoding = "UTF-8"), 
                         trim_ws = TRUE)

colnames(ZONA_SECAO)[5] <- "zs"
sp_ver_2012 <- select(merge(votacao_secao_2012_SP_sp_ver_nom, ZONA_SECAO, by = "zs", all.x=T), "zs","DISTRITO", "ds_codigo", "X14", "X15")
colnames(sp_ver_2012)[4] <- "X12"
colnames(sp_ver_2012)[5] <- "VOTOS"
sp_ver_2012 <- select(merge(sp_ver_2012, votacao_candidato_munzona_2012_SP_sp_ver, by = "X12", all.x=T), "X15", "X12", "X24", "X28", "zs","DISTRITO", "ds_codigo", "VOTOS", "X22")
sum(is.na(sp_ver_2012$DISTRITO))
sp_ver_2012_g <- group_by(sp_ver_2012, X15, X12, X24, X28, DISTRITO, ds_codigo, X22)
sp_ver_2012_t <- summarise(sp_ver_2012_g, VOTOS = sum(VOTOS))
sum(is.na(sp_ver_2012_t$DISTRITO))

sum(votacao_secao_2012_SP_sp_ver_nom$X15)
sum(sp_ver_2012_t$VOTOS)

sp_ver_2012_tNA <- na.omit(sp_ver_2012_t)
sp_ver_2012_tNA$A <- NA
sp_ver_2012_tNA$B <- NA

for (i in 1:72526) {
  sp_ver_2012_tNA[i,9] <- (as.double(sp_ver_2012_tNA[i,8])/sum(sp_ver_2012_tNA$VOTOS[sp_ver_2012_tNA$DISTRITO==as.character(sp_ver_2012_tNA[i,5])])) * (as.double(sp_ver_2012_tNA[i,8])/sum(sp_ver_2012_tNA$VOTOS[sp_ver_2012_tNA$X15==as.character(sp_ver_2012_tNA[i,1])]))
  sp_ver_2012_tNA[i,10] <- as.double(sp_ver_2012_tNA[i,8])/sum(sp_ver_2012_tNA$VOTOS[sp_ver_2012_tNA$X15==as.character(sp_ver_2012_tNA[i,1])])
}

sp_ver_2012_tNA_g <- group_by(sp_ver_2012_tNA, X15, X12, X24, X28, X22)
sp_ver_2012_dom <- summarise(sp_ver_2012_tNA_g, VOTOS = sum(VOTOS), A = sum(A), B = sum(B))
sp_ver_2012_dom$dom <- sp_ver_2012_dom$A/sp_ver_2012_dom$B
sp_ver_2012_dom$A <- NULL
sp_ver_2012_dom$B <- NULL

library(spdep)
library(ape)

distr <- read_sf("C:/Users/lucas/Google Drive/Acadêmicos/Sciences Po/Master/Research/Data/Shapefiles/SIRGAS_SHP_distrito_polygon.shp")
votes_distr_2012 <- sp_ver_2012_t[,c(1,6,8)]
votes_distr_2012 <- spread(votes_distr_2012, X15,  VOTOS)
votes_distr_2012 <- votes_distr_2012[1:96,]
votes_distr_2012[is.na(votes_distr_2012)] <- 0

m_2012 <- merge(distr, votes_distr_2012, by = "ds_codigo", all.x=T)

m_nbq_2012 <- poly2nb(m_2012)
m_nbq_w_2012 <- nb2listw(m_nbq_2012)

m1_2012 <- as.data.frame(merge(distr, votes_distr_2012, by = "ds_codigo", all.x=T))

sp_ver_2012_mor <- as.data.frame(colnames(m1_2012[3:1168]))
sp_ver_2012_mor$mor <- NA
colnames(sp_ver_2012_mor)[1] <- "X15"

for (i in 1:1274) {
  
  sp_ver_2012_mor[i,2] <- moran(m1_2012[,i+2], m_nbq_w_2012, 96, 96)
  
}

sp_ver_2012_dom_mor <- select(merge(sp_ver_2012_dom, sp_ver_2012_mor, by = "X15", all.x=T), "X15","dom", "mor", "X22")

sp_ver_2012_dom_mor_el <- subset(sp_ver_2012_dom_mor, X22=="ELEITO POR QP" | X22=="ELEITO POR MÉDIA", select = c("X15","dom", "mor"))
colnames(sp_ver_2012_dom_mor_el)[1] <- "NM_URNA_CANDIDATO"
write.csv(sp_ver_2012_dom_mor_el, "DOM_MOR_ELEITOS_2012.csv", fileEncoding = "UTF-8")

sp_ver_2012_dom_mor_su <- subset(sp_ver_2012_dom_mor, X22=="SUPLENTE", select = c("X15","dom", "mor"))
colnames(sp_ver_2012_dom_mor_su)[1] <- "NM_URNA_CANDIDATO"
write.csv(sp_ver_2012_dom_mor_su, "DOM_MOR_SUPL_2012.csv", fileEncoding = "UTF-8")




