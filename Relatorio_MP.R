## O OBJETIVO É CLASSIFICAR AS PEÇAS PROCESSUAIS PRODUZIDAS MÊS A MÊS E ELABORAR UM DASHBOARD
library(magrittr)

relat <- function(endereco) {
# Faz um dataframe na pasta do mês
library(tidyverse)
arquivos <- list.files(path = endereco, full.names = FALSE)
relatorio <- tibble(nomes = arquivos, 
             processo = str_extract(nomes, "\\d+.\\d+.\\d+"),
             peca = case_when(
                 str_detect(nomes, "(?i)parec+.") ~ "parecer",
                 str_detect(nomes, "(?i)arq+.") ~ "arquivamento",
                 str_detect(nomes, "(?i)CRR+.|(?i)contrarraz+.") ~ "contrarrazões",
                 str_detect(nomes, "(?i)ato+.|(?i)racionaliza+.") ~ "ato_racionalização",
                 str_detect(nomes, "(?i)previ+.") ~ "previdenciário",
                 str_detect(nomes, "(?i)den+.|(?i)CRDen+.") ~ "denúncia",
                 str_detect(nomes, "(?i)mem+.|(?i)aleg") ~ "memorial",
                 str_detect(nomes, "(?i)proteti+.") ~ "parecer_protetiva",
                 str_detect(nomes, "(?i)repli+.") ~ "réplica",
                 str_detect(nomes, "(?i)rep+.") ~ "representação",
                 str_detect(nomes, "(?i)prorroga..o+.") ~ "prorrogação",
                 str_detect(nomes, "(?i)cota+.") ~ "cota",
                 TRUE ~ "cota"
             ),
             área = case_when(
                 str_detect(processo, "^43+.|^62+.|^14+.") ~ "difusos",
                 str_detect(nomes, "^(?i)Ci+.|(?i)civel+.|(?i)interdi..o+.") ~ "cível",
                 str_detect(nomes, "^(?i)Cr+.|(?i)delpol+.|(?i)retorno+.|(?i)CRR+.|(?i)criminal+.") ~ "criminal",
                 str_detect(peca, "denúncia") ~ "criminal",
                 str_detect(nomes, "^(?i)DC+.") ~ "difusos",
                 str_detect(nomes, "(?i)ACP+.|(?i)popular+.") ~ "difusos_ACP",
                 str_detect(nomes, "^(?i)Ij+.|(?i)inf.ncia+.|(?i)prote..o+.|(?i)acolhimento+.") ~ "infancia",
                 str_detect(nomes, "(?i)jecrim+.|(?i)transacao+.|(?i)supens..o+.") ~ "criminal_JECRIM",
                 str_detect(processo, "^15+.") ~ "criminal",
                 str_detect(processo, "^100+.") ~ "cível",
                 str_detect(peca, "(?i)representação+.|(?i)prorroga..o+.") ~ "difusos",
                 TRUE ~ "desconhecido"
             )
)%>%
    group_by(processo, peca) %>%
    slice(1) %>% 
    ungroup()
relatorio
}

arruma_base <- function(caminho = getwd()){
    # Faz um data frame geral dos arquivos da PJ, com colunas de anos e meses 
    # o path, deve conter em seu interior APENAS pastas individuais com cada ano no formato XXXX, "ex.: 2020", e dentro delas, 
    # pastas mensais, com os nomes dos meses em português ("Janeiro", "Fevereiro") e no interior de cada uma, as peças com
    # nomenclatura no modelo da Corregedoria
     
    relat <- function(endereco) {
        # Faz um dataframe na pasta do mês
        library(tidyverse)
        arquivos <- list.files(path = endereco, full.names = FALSE)
        relatorio <- tibble(nomes = arquivos, 
                            processo = str_extract(nomes, "\\d+.\\d+.\\d+"),
                            peca = case_when(
                                str_detect(nomes, "(?i)parec+.") ~ "parecer",
                                str_detect(nomes, "(?i)arq+.") ~ "arquivamento",
                                str_detect(nomes, "(?i)CRR+.|(?i)contrarraz+.") ~ "contrarrazões",
                                str_detect(nomes, "(?i)ato+.|(?i)racionaliza+.") ~ "ato_racionalização",
                                str_detect(nomes, "(?i)previ+.") ~ "previdenciário",
                                str_detect(nomes, "(?i)den+.|(?i)CRDen+.") ~ "denúncia",
                                str_detect(nomes, "(?i)mem+.|(?i)aleg") ~ "memorial",
                                str_detect(nomes, "(?i)proteti+.") ~ "parecer_protetiva",
                                str_detect(nomes, "(?i)repli+.") ~ "réplica",
                                str_detect(nomes, "(?i)rep+.") ~ "representação",
                                str_detect(nomes, "(?i)prorroga..o+.") ~ "prorrogação",
                                str_detect(nomes, "(?i)cota+.") ~ "cota",
                                TRUE ~ "cota"
                            ),
                            área = case_when(
                                str_detect(processo, "^43+.|^62+.|^14+.") ~ "difusos",
                                str_detect(nomes, "^(?i)Ci+.|(?i)civel+.|(?i)interdi..o+.") ~ "cível",
                                str_detect(nomes, "^(?i)Cr+.|(?i)delpol+.|(?i)retorno+.|(?i)CRR+.|(?i)criminal+.") ~ "criminal",
                                str_detect(peca, "denúncia") ~ "criminal",
                                str_detect(nomes, "^(?i)DC+.") ~ "difusos",
                                str_detect(nomes, "(?i)ACP+.|(?i)popular+.") ~ "difusos_ACP",
                                str_detect(nomes, "^(?i)Ij+.|(?i)inf.ncia+.|(?i)prote..o+.|(?i)acolhimento+.") ~ "infancia",
                                str_detect(nomes, "(?i)jecrim+.|(?i)transacao+.|(?i)supens..o+.") ~ "criminal_JECRIM",
                                str_detect(processo, "^15+.") ~ "criminal",
                                str_detect(processo, "^100+.") ~ "cível",
                                str_detect(peca, "(?i)representação+.|(?i)prorroga..o+.") ~ "difusos",
                                TRUE ~ "desconhecido"
                            )
        )%>%
            group_by(processo, peca) %>%
            slice(1) %>% 
            ungroup()
    }
   
    home <- getwd()
    setwd(caminho)
    anos <- list.dirs(full.names = FALSE, recursive = FALSE)
    if("Rproj" %in% anos){
        anos <-  anos[-1]
    } 
    meses <- list.dirs(anos,recursive = FALSE)
    if("Rproj" %in% meses){
        meses <- meses[-1]
    }
    meses <- purrr::set_names(meses)
    
    
    df <- purrr::map_dfr(meses, relat, .id = "mes")
    df <- df %>% 
        dplyr::mutate(ano = str_extract(mes, "\\d+"), .after = 1)%>% 
        dplyr::mutate( mes = stringr::str_extract(mes, "[:alpha:]{3}"),
                mes = stringr::str_squish(mes),
                mes = stringr::str_to_title(mes))
    df <- df %>% 
       dplyr:: mutate(mes_num = dplyr::case_when(
            mes == "Jan" ~ "01",
            mes == "Fev" ~ "02",
            mes == "Mar" ~ "03",
            mes == "Abr" ~ "04",
            mes == "Mai" ~ "05",
            mes == "Jun" ~ "06",
            mes == "Jul" ~ "07",
            mes == "Ago" ~ "08",
            mes == "Set" ~ "09",
            mes == "Out" ~ "10",
            mes == "Nov" ~ "11",
            mes == "Dez" ~ "12"
            
        )) %>%
        dplyr::mutate(data = paste0(ano, "-", mes_num , "-01")) %>% 
        dplyr::mutate(data = lubridate::as_date(data)) %>% 
        janitor::clean_names()
    setwd(home)
    df
    
}

# Testes bem sucedidos:
df_Conchas <- arruma_base("C:/FCD/Relatorio_MP/2_PJ_Conchas")
df_Sebast <- arruma_base("C:/FCD/Relatorio_MP/4_PJ_Sao_Sebastiao")


df <- df_Conchas %>% 
    bind_rows(df_Sebast, .id = "comarca") %>% 
    mutate(comarca = if_else(comarca=="1","Conchas", "Sebast"))
saveRDS(df, file="df.rds")
# Plot Criminal -----------------------------------------------------------


df_abr_2021 <- df_Conchas %>% 
    filter(mes == "Abr",
           ano == "2021"
)
# Relatorio de abril 2021
df_abr_21_count <- df_abr_2021 %>% 
    group_by(mes, ano, data, peca, area) %>% 
    count()

library(ggplot2)
library(highcharter)
library(hrbrthemes)
df_Conchas %>%
    ggplot() +
    aes(x = data, fill = peca) +
    geom_histogram(bins = 30L) +
    scale_fill_brewer(palette = "Set3", direction = 1) +
    labs(
        title = "Volume de peças por área",
        subtitle = "2ª Promotoria de Conchas"
    ) +
    hrbrthemes::theme_ipsum_rc() +
    facet_wrap(vars(area))

plot_1 <- df |> 
    filter(ano == 2021) |>  
    # filter(peca == "denúncia") |>/
    ggplot() +
    aes(x = data, fill = peca) +
    geom_histogram(bins = 30L) +
    scale_fill_brewer(palette = "Set3", direction = 1) +
    labs(
        title = "Volume de peças por área",
        subtitle = "Conchas vs São Sebatião"
    ) +
    hrbrthemes::theme_ipsum_rc() +
    facet_wrap(vars(comarca))

plot_1

df %>%
    # filter(peca == "denúncia") %>% 
    group_by(comarca,data) %>% 
    count() %>% 
    hchart('column', hcaes(x = 'data', y = 'n', group = "comarca")) %>% 
    hc_title(text = "2ª PJ Conchas x 4ª PJ São Sebastião") %>%
    hc_subtitle(text = "Volume de peças 2020 - abril/2021") %>% 
    hc_yAxis(title = list(text = "Número de peças")) 
    
