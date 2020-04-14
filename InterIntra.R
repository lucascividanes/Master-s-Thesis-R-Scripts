library(readr)
library(tidyverse)

votacao_secao_2016_SP <- read_delim("C:/Users/lucas/Google Drive/Acadêmicos/Sciences Po/Master/Research/Data/TSE/votacao_secao_2016_SP.txt", 
                                    ";", escape_double = FALSE, col_names = FALSE, 
                                    locale = locale(encoding = "latin1"), 
                                    trim_ws = TRUE)

votacao_secao_2016_SP_sp_ver <- subset(votacao_secao_2016_SP, X9=="SÃO PAULO" & X13=="VEREADOR", select = c(X10, X11, X14, X15))
votacao_secao_2016_SP_sp_ver$zs <- paste(votacao_secao_2016_SP_sp_ver$X10,"-",votacao_secao_2016_SP_sp_ver$X11)
votacao_secao_2016_SP_sp_ver$zs <- str_replace(votacao_secao_2016_SP_sp_ver$zs, " ", "")
votacao_secao_2016_SP_sp_ver$zs <- str_replace(votacao_secao_2016_SP_sp_ver$zs, " ", "")
votacao_secao_2016_SP_sp_ver_nom <- subset(votacao_secao_2016_SP_sp_ver, X14>99)

votacao_candidato_munzona_2016_SP <- read_delim("C:/Users/lucas/Google Drive/Acadêmicos/Sciences Po/Master/Research/Data/TSE/votacao_candidato_munzona_2016_SP.csv", 
                                                ";", escape_double = FALSE, locale = locale(encoding = "latin1"), 
                                                trim_ws = TRUE)

votacao_candidato_munzona_2016_SP_sp_ver <- unique(subset(votacao_candidato_munzona_2016_SP, NM_MUNICIPIO=="SÃO PAULO" & DS_CARGO=="Vereador", select = c(NM_URNA_CANDIDATO, NR_CANDIDATO, SG_PARTIDO, DS_COMPOSICAO_COLIGACAO, DS_SIT_TOT_TURNO)))

ZONA_SECAO <- read_delim("C:/Users/lucas/Google Drive/Acadêmicos/Sciences Po/Master/Research/Data/csv/ZONA_SECAO.csv", 
                         ";", escape_double = FALSE, locale = locale(encoding = "UTF-8"), 
                         trim_ws = TRUE)
ZONA_SECAO$sp_codigo[ZONA_SECAO$sp_codigo==1] <- "01"
ZONA_SECAO$sp_codigo[ZONA_SECAO$sp_codigo==2] <- "02"
ZONA_SECAO$sp_codigo[ZONA_SECAO$sp_codigo==3] <- "03"
ZONA_SECAO$sp_codigo[ZONA_SECAO$sp_codigo==4] <- "04"
ZONA_SECAO$sp_codigo[ZONA_SECAO$sp_codigo==5] <- "05"
ZONA_SECAO$sp_codigo[ZONA_SECAO$sp_codigo==6] <- "06"
ZONA_SECAO$sp_codigo[ZONA_SECAO$sp_codigo==7] <- "07"
ZONA_SECAO$sp_codigo[ZONA_SECAO$sp_codigo==8] <- "08"
ZONA_SECAO$sp_codigo[ZONA_SECAO$sp_codigo==9] <- "09"

colnames(ZONA_SECAO)[5] <- "zs"
sp_ver_2016 <- select(merge(votacao_secao_2016_SP_sp_ver_nom, ZONA_SECAO, by = "zs", all.x=T), "zs", "SUBPREFEITURA", "sp_codigo", "X14", "X15")
colnames(sp_ver_2016)[4] <- "NR_CANDIDATO"
colnames(sp_ver_2016)[5] <- "VOTOS"
sp_ver_2016 <- select(merge(sp_ver_2016, votacao_candidato_munzona_2016_SP_sp_ver, by = "NR_CANDIDATO", all.x=T), "zs", "SUBPREFEITURA", "sp_codigo", "NR_CANDIDATO", "SG_PARTIDO", "DS_COMPOSICAO_COLIGACAO", "VOTOS")

sp_ver_2016_inter <- group_by(sp_ver_2016, SUBPREFEITURA, sp_codigo, DS_COMPOSICAO_COLIGACAO) %>% summarise(VOTOS = sum(VOTOS))
sp_ver_2016_inter <- na.omit(sp_ver_2016_inter)

aux1 <- group_by(sp_ver_2016_inter, SUBPREFEITURA) %>% summarise(VOTOS = sum(VOTOS))
colnames(aux1)[2] <- "TOTAL"

sp_ver_2016_inter <- select(merge(sp_ver_2016_inter, aux1, by = "SUBPREFEITURA", all.x=T), "SUBPREFEITURA", "sp_codigo", "DS_COMPOSICAO_COLIGACAO", "VOTOS", "TOTAL")
sp_ver_2016_inter$inter <- (sp_ver_2016_inter$VOTOS/sp_ver_2016_inter$TOTAL)^2

resumo_inter_2016 <- group_by(sp_ver_2016_inter, SUBPREFEITURA, sp_codigo) %>% summarise(inter = sum(inter))
resumo_inter_2016$interparty <- 1 - resumo_inter_2016$inter
resumo_inter_2016$inter <- NULL
resumo_inter_2016 <- na.omit(resumo_inter_2016)

write.csv(resumo_inter_2016, "inter.csv", fileEncoding = "UTF-8")

sp_ver_2016_intra <- group_by(sp_ver_2016, SUBPREFEITURA, sp_codigo, NR_CANDIDATO, DS_COMPOSICAO_COLIGACAO) %>% summarise(VOTOS = sum(VOTOS))
sp_ver_2016_intra <- na.omit(sp_ver_2016_intra)

aux2 <- group_by(sp_ver_2016_intra, SUBPREFEITURA, DS_COMPOSICAO_COLIGACAO) %>% summarise(VOTOS = sum(VOTOS))
colnames(aux2)[3] <- "TOTAL"

sp_ver_2016_intra <- select(merge(sp_ver_2016_intra, aux2, by = c("SUBPREFEITURA", "DS_COMPOSICAO_COLIGACAO"), all.x=T), "SUBPREFEITURA", "sp_codigo", "NR_CANDIDATO", "DS_COMPOSICAO_COLIGACAO", "VOTOS", "TOTAL")
sp_ver_2016_intra$intra <- (sp_ver_2016_intra$VOTOS/sp_ver_2016_intra$TOTAL)^2

resumo_intra_2016 <- group_by(sp_ver_2016_intra, SUBPREFEITURA, sp_codigo, DS_COMPOSICAO_COLIGACAO) %>% summarise(intra = sum(intra))
resumo_intra_2016$intraparty <- 1 - resumo_intra_2016$intra
resumo_intra_2016$intra <- NULL

write.csv(resumo_intra_2016, "intra.csv", fileEncoding = "UTF-8")