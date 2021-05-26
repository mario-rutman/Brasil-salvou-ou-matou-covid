
# 1. Importando a tabela direto do site. -------------------------------------------------

url <- "https://www.worldometers.info/coronavirus/#countries"

mortes_populacao <- ralger::table_scrap(link = url) 


# 2. Limpando e preparando a tabela. ---------------------------------------------------

options(scipen = 999)
library(stringr)
library(magrittr, include.only = "%>%")
mort_pop_tidy <- mortes_populacao %>% 
  janitor::clean_names() %>% #Limpando os nomes das colunas.
  # Selecionando as colunas que interessam.
  dplyr::select(country_other, total_deaths,
                population, continent) %>%
  # Traduzindo os nomes das colunas.
  dplyr::rename(pais = country_other, mortes = total_deaths,
                populacao = population, continente = continent) %>% 
  # Retirando as observações que não compõem a tabela.
  dplyr::slice(c(9:230)) %>%
  # Retirando em mortes e população o que é NA ou "".
  dplyr::filter(!is.na(mortes), mortes != "", populacao != "") %>% 
  # Convertendo mortes e populacao para número.
  dplyr::mutate(mortes = str_remove_all(mortes, pattern = ",") %>% 
                  as.numeric(),
                populacao = str_remove_all(populacao, pattern = ",") %>% 
                  as.numeric()) %>% 
  # Reposicionando a coluna continente.
  dplyr::select(continente, pais:populacao) %>% 
  # Criando a coluna que indica quantos morreram por milhão de pessoas.
  dplyr::mutate(mortes_por_milhao = mortes * 1e6 / populacao) %>% 
  # Calculando a diferença entre a mediana_das 
  # mortes por milhão e as mortes por milhao de um país.
  dplyr::mutate(dif_media_mortes_por_milhao = 
                  mean(mortes_por_milhao) - mortes_por_milhao) %>%
  # Transformando a dif_mediana_mortes_por_milhao numa escala
  # de 0 a 100.
  # dplyr::mutate(escala_0_a_100 = 
  #                 round((dif_mediana_mortes_por_milhao + 2800) * 100 / 3055, 1)) %>% 
  # Classificando se o país salvou ou matou.
  dplyr::mutate(matou_ou_salvou = 
                  dplyr::case_when(
                    dif_media_mortes_por_milhao >= 0 ~ "salvou da Covid",
                    TRUE ~ "matou por Covid"
                  )) %>% 
  # Calculando quantos salvou ou matou.
  dplyr::mutate(resultado = populacao * abs(dif_media_mortes_por_milhao)/1e6)
  
# Salvando em rds.
saveRDS(mort_pop_tidy, "mort_pop_tidy.rds")



# 3. A forma de cálculo e as regiões. -------------------------------------

# Se é a diferença entre a media do conjunto de mortos por milhão e os mortos
# por milhão de um país que determina quantos matou ou salvou, continentes diferentes
# resultarão quantidades diferentes de salvos ou mortos.
# O objetivo da função 'resultado_covid' é mostrar, sobre determinado país
# em cada uma das 6 regiões ("North America", "Asia", "South America", "Europe", 
# "Africa" e "Australia/Oceania") quantos 'salvou da' ou 'matou por' Covid". 


# 4. Criando a função. ----------------------------------------------------


resultado_covid <- function(regiao, lugar) {
  
  # A observação, linha, sobre o Brasil
  pais <- mort_pop_tidy %>% 
    dplyr::filter(pais == lugar)
  
  # População do Brasil.
  pop_pais <- pais %>%
    dplyr::select(populacao) %>% 
    dplyr::pull()
  
  # Mortes por milhão no Brasil.
  m_p_m_pais <- pais %>% 
    dplyr::select(mortes_por_milhao) %>% 
    dplyr::pull()
  
  # A mediana do número de mortes por milhão considerando o Brasil um país de
  # algum dos continentes.
  mediana_regiao <- mort_pop_tidy %>%
    # Filtrando ragião escolhida e retirando o Brasil
    dplyr::filter(continente == regiao, pais != lugar) %>%
    # Colando a linha Brasil à região escolhida.
    dplyr::bind_rows(pais) %>% 
    # Calculando a mediana da região como se o Brasil pertencesse a ela. 
    dplyr::pull(mortes_por_milhao) %>% 
    median()
    
  # Diferença entre a mediana do conjunto de mortos por milhão da região
  # e o número de mortos por milhão do país.
  round((mediana_regiao - m_p_m_pais)/1e6 * pop_pais, 0) %>% 
    abs()
}

# Criando os dois vetores para passar na função.
pais <- rep("Brazil", 6)
cont <- c("Europe", "South America", "North America", "Asia", "Africa",
                 "Australia/Oceania")

# Traduzindo o vetor cont.
cont_traduzido <- c("Europa", "América do Sul", "América do Norte",
                    "Ásia", "África", "Austrália/Oceania")

# Passando os vetores na função.
valores <- purrr::map2_dbl(cont, pais, resultado_covid) 

# Criando o tibble de onde será feita a tabela.
library(glue)
obitos_por_covid_brasil <- tibble::tibble(cont_traduzido, valores) %>% 
  dplyr::mutate(frase = glue('...{cont_traduzido}, diríamos que "matou por Covid" {prettyNum(valores, big.mark = ".", decimal.mark = ",")} habitantes.')) %>% 
  dplyr::select(frase)

saveRDS(obitos_por_covid_brasil, "obitos_por_covid_brasil.rds")


