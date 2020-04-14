library(readr)
library(tidyverse)

votacao_secao_2016_SP <- read_delim("C:/Users/lucas/Google Drive/Acadêmicos/Sciences Po/Master/Research/Data/TSE/votacao_secao_2016_SP.txt", 
                                    ";", escape_double = FALSE, col_names = FALSE, 
                                    locale = locale(encoding = "latin1"), 
                                    trim_ws = TRUE)

votacao_candidato_munzona_2016_SP <- read_delim("C:/Users/lucas/Google Drive/Acadêmicos/Sciences Po/Master/Research/Data/TSE/votacao_candidato_munzona_2016_SP.csv", 
                                                ";", escape_double = FALSE, locale = locale(encoding = "latin1"), 
                                                trim_ws = TRUE)

votacao_secao_2012_SP <- read_delim("C:/Users/lucas/Google Drive/Acadêmicos/Sciences Po/Master/Research/Data/TSE/votacao_secao_2012_SP.txt", 
                                    ";", escape_double = FALSE, col_names = FALSE, 
                                    locale = locale(encoding = "latin1"), 
                                    trim_ws = TRUE)

votacao_candidato_munzona_2012_SP <- read_delim("C:/Users/lucas/Google Drive/Acadêmicos/Sciences Po/Master/Research/Data/TSE/votacao_candidato_munzona_2012_SP.txt", 
                                                ";", escape_double = FALSE, col_names = FALSE, 
                                                locale = locale(encoding = "latin1"), 
                                                trim_ws = TRUE)

ZONA_SECAO <- read_delim("C:/Users/lucas/Google Drive/Acadêmicos/Sciences Po/Master/Research/Data/csv/ZONA_SECAO.csv", 
                         ";", escape_double = FALSE, locale = locale(encoding = "UTF-8"), 
                         trim_ws = TRUE)

votacao_secao_2016_SP_sp_ver <- subset(votacao_secao_2016_SP, X9=="SÃO PAULO" & X13=="VEREADOR", select = c(X10, X11, X14, X15))
votacao_candidato_munzona_2016_SP_sp_ver <- unique(subset(votacao_candidato_munzona_2016_SP, NM_MUNICIPIO=="SÃO PAULO" & DS_CARGO=="Vereador", select = c(NM_URNA_CANDIDATO, NR_CANDIDATO, SG_PARTIDO, DS_COMPOSICAO_COLIGACAO, DS_SIT_TOT_TURNO)))

votacao_secao_2016_SP_sp_ver$zs <- paste(votacao_secao_2016_SP_sp_ver$X10,"-",votacao_secao_2016_SP_sp_ver$X11)
votacao_secao_2016_SP_sp_ver$zs <- str_replace(votacao_secao_2016_SP_sp_ver$zs, " ", "")
votacao_secao_2016_SP_sp_ver$zs <- str_replace(votacao_secao_2016_SP_sp_ver$zs, " ", "")

votacao_secao_2016_SP_sp_ver_nom <- subset(votacao_secao_2016_SP_sp_ver, X14>99)

votacao_secao_2012_SP_sp_ver <- subset(votacao_secao_2012_SP, X9=="SÃO PAULO" & X13=="VEREADOR", select = c(X10, X11, X14, X15))
votacao_candidato_munzona_2012_SP_sp_ver <- unique(subset(votacao_candidato_munzona_2012_SP, X9=="SÃO PAULO" & X16=="VEREADOR", select = c(X15, X12, X24, X28, X22)))

votacao_secao_2012_SP_sp_ver$zs <- paste(votacao_secao_2012_SP_sp_ver$X10,"-",votacao_secao_2012_SP_sp_ver$X11)
votacao_secao_2012_SP_sp_ver$zs <- str_replace(votacao_secao_2012_SP_sp_ver$zs, " ", "")
votacao_secao_2012_SP_sp_ver$zs <- str_replace(votacao_secao_2012_SP_sp_ver$zs, " ", "")

votacao_secao_2012_SP_sp_ver_nom <- subset(votacao_secao_2012_SP_sp_ver, X14>99)

colnames(ZONA_SECAO)[5] <- "zs"
sp_ver_2016 <- select(merge(votacao_secao_2016_SP_sp_ver_nom, ZONA_SECAO, by = "zs", all.x=T), "zs","DISTRITO", "ds_codigo", "X14", "X15")
colnames(sp_ver_2016)[4] <- "NR_CANDIDATO"
colnames(sp_ver_2016)[5] <- "VOTOS"
sp_ver_2016 <- select(merge(sp_ver_2016, votacao_candidato_munzona_2016_SP_sp_ver, by = "NR_CANDIDATO", all.x=T), "NM_URNA_CANDIDATO", "NR_CANDIDATO", "SG_PARTIDO", "DS_COMPOSICAO_COLIGACAO", "zs","DISTRITO", "ds_codigo", "VOTOS", "DS_SIT_TOT_TURNO")
sum(is.na(sp_ver_2016$DISTRITO))
sp_ver_2016_g <- group_by(sp_ver_2016, NM_URNA_CANDIDATO, NR_CANDIDATO, SG_PARTIDO, DS_COMPOSICAO_COLIGACAO, DISTRITO, ds_codigo, DS_SIT_TOT_TURNO)
sp_ver_2016_t <- summarise(sp_ver_2016_g, VOTOS = sum(VOTOS))
sum(is.na(sp_ver_2016_t$DISTRITO))

sp_ver_2016_el <- subset(sp_ver_2016_t, DS_SIT_TOT_TURNO=="ELEITO POR QP" | DS_SIT_TOT_TURNO=="ELEITO POR MÉDIA", select = c("NM_URNA_CANDIDATO", "DISTRITO", "ds_codigo", "VOTOS"))
sp_ver_2016_el <- spread(sp_ver_2016_el, NM_URNA_CANDIDATO,  VOTOS)
sum(sp_ver_2016_el[97,3:57], na.rm = T)/sum(sp_ver_2016_el[1:96,3:57], na.rm = T)
sp_ver_2016_el <- sp_ver_2016_el[1:96,]
sp_ver_2016_el[is.na(sp_ver_2016_el)] <- 0

write.csv(sp_ver_2016_el, "VOTOS_DIST_ELEITOS_2016.csv", fileEncoding = "UTF-8")

sp_ver_2016_su <- subset(sp_ver_2016_t, DS_SIT_TOT_TURNO=="SUPLENTE", select = c("NM_URNA_CANDIDATO", "DISTRITO", "ds_codigo", "VOTOS"))
sp_ver_2016_su <- spread(sp_ver_2016_su, NM_URNA_CANDIDATO,  VOTOS)
sum(sp_ver_2016_su[97,3:679], na.rm = T)/sum(sp_ver_2016_su[1:96,3:679], na.rm = T)
sp_ver_2016_su <- sp_ver_2016_su[1:96,]
sp_ver_2016_su[is.na(sp_ver_2016_su)] <- 0

write.csv(sp_ver_2016_su, "VOTOS_DIST_SUPL_2016.csv", fileEncoding = "UTF-8")

sp_ver_2012 <- select(merge(votacao_secao_2012_SP_sp_ver_nom, ZONA_SECAO, by = "zs", all.x=T), "zs","DISTRITO", "ds_codigo", "X14", "X15")
colnames(sp_ver_2012)[4] <- "X12"
colnames(sp_ver_2012)[5] <- "VOTOS"
sp_ver_2012 <- select(merge(sp_ver_2012, votacao_candidato_munzona_2012_SP_sp_ver, by = "X12", all.x=T), "X15", "X12", "X24", "X28", "zs","DISTRITO", "ds_codigo", "VOTOS", "X22")
sum(is.na(sp_ver_2012$DISTRITO))
sp_ver_2012_g <- group_by(sp_ver_2012, X15, X12, X24, X28, DISTRITO, ds_codigo, X22)
sp_ver_2012_t <- summarise(sp_ver_2012_g, VOTOS = sum(VOTOS))
sum(is.na(sp_ver_2012_t$DISTRITO))

sp_ver_2012_el <- subset(sp_ver_2012_t, X22=="ELEITO POR QP" | X22=="ELEITO POR MÉDIA", select = c("X15", "DISTRITO", "ds_codigo", "VOTOS"))
sp_ver_2012_el <- spread(sp_ver_2012_el, X15,  VOTOS)
sum(sp_ver_2012_el[97,3:57], na.rm = T)/sum(sp_ver_2012_el[1:96,3:57], na.rm = T)
sp_ver_2012_el <- sp_ver_2012_el[1:96,]
sp_ver_2012_el[is.na(sp_ver_2012_el)] <- 0

write.csv(sp_ver_2012_el, "VOTOS_DIST_ELEITOS_2012.csv", fileEncoding = "UTF-8")

sp_ver_2012_su <- subset(sp_ver_2012_t, X22=="SUPLENTE", select = c("X15", "DISTRITO", "ds_codigo", "VOTOS"))
sp_ver_2012_su <- spread(sp_ver_2012_su, X15,  VOTOS)
sum(sp_ver_2012_su[97,3:824], na.rm = T)/sum(sp_ver_2012_su[1:96,3:824], na.rm = T)
sp_ver_2012_su <- sp_ver_2012_su[1:96,]
sp_ver_2012_su[is.na(sp_ver_2012_su)] <- 0

write.csv(sp_ver_2012_su, "VOTOS_DIST_SUPL_2012.csv", fileEncoding = "UTF-8")

sum(sp_ver_2016_el[,3:57])
sum(sp_ver_2012_el[,3:57])
