library(dplyr)
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(egg)

# leitura da base de dados
dados_base <- read.csv("C:/Users/Autaza/R Projects/Respostas formulario.csv")

# Criando um histogramna da auto-avaliacao da saude mental
dados_base %>% 
  ggplot(aes(avaliacao_saude_mental)) +
  geom_histogram(bins = 8, colour = "black", fill = "grey80", aes(y=..density..)) +
  theme_article() +
  labs(title = "Distribuição da saúde mental",
       x = "Saúde mental",
       y = "Quantidade de alunos") +
  stat_function(fun = dnorm, args = list(
    mean = mean(dados_base$avaliacao_saude_mental),
    sd = sd(dados_base$avaliacao_saude_mental)))


# grafico de linha c/ de tendencia: saude mental x qntd. ucs
dados_base %>% 
  ggplot(aes(x = ucs, y = avaliacao_saude_mental)) +
  ylim(1,10) +
  geom_smooth(color="black", se = FALSE, size = 0.5) +
  theme_article() +
  labs(title = "Saúde mental X Quantidade de UCs sendo cursadas ",
       x = "Quantidade de UCs",
       y = "Saúde mental")

# grafico de ponto e linha: perido x saude mental
dados_base %>% 
  ggplot(aes(x = semestre, y = avaliacao_saude_mental)) +
  ylim(1,10) +
  geom_point(shape=21) +
  geom_smooth(color="black", se = FALSE, size = 0.5) +
  theme_article() +
  labs(title = "Saúde mental X Semestre cursado ",
       x = "Semestre",
       y = "Saúde mental")

# box_plot: morar sozinho x saude mental
boxplot(dados_base$avaliacao_saude_mental ~ dados_base$mora_sozinho, 
        main = "Relação entre saúde mental e morar sozinho",
        ylab = "Saúde mental",
        xlab = "Mora sozinho")



# filtrando os dados por acompanhamento pscologico e saude mental
dados_avaliacao <- dados_base %>%
  group_by(avaliacao_saude_mental, acompanhamento_pscologico) %>%
  summarise(quantidade_avaliacao = n()) %>%
  arrange(desc(quantidade_avaliacao))
  # grafico de linha: necessidade acompanhamento e saude mental
dados_avaliacao %>% 
  filter(acompanhamento_pscologico %in% c("não, mas vejo necessidade", 
                                          "não, e não vejo necessidade",
                                          "sim,  e vejo necessidade")) %>%
  ggplot(aes(x =  avaliacao_saude_mental, y = quantidade_avaliacao, color = acompanhamento_pscologico)) +
  geom_smooth(se = FALSE) +
  labs(title = "Relação entre acompanhamento pscológico e saúde mental",
       x = "Saúde mental",
       y = "Quantidade de alunos",
       color = "Faz acompanhamento psicológico") +
  theme_article()
  
# garfico de barra: acompanhamento psicologico("sim, vejo necessidade") x trabalha
dados_trabalha <- dados_base %>%
  group_by(acompanhamento_pscologico , trabalha) %>%
  summarise(quantidade_avaliacao = n()) %>%
  arrange(desc(quantidade_avaliacao))
  
dados_trabalha %>%
  ggplot() +
  geom_col(aes(x = acompanhamento_pscologico, 
               y = quantidade_avaliacao, 
               fill = trabalha), 
           position = "dodge",
           colour = "black") +
  theme_article() +
  scale_fill_brewer(type = "qual", palette = 4) +
  coord_flip() +
  labs(title = "Relação entre acompanhamento pscológico e trabalho",
       x = "Acompanhamento psicológico",
       y = "Quantidade de alunos",
       fill = "Trabalha")
  
# grafico de barra: saude mental x faixa etária
# filtrando os dados por idade
dados_idade <- dados_base %>%
  group_by(faixa_etaria, avaliacao_saude_mental) %>%
  summarise(quantidade = n()) %>%
  arrange(desc(quantidade))
# plotando no ggplot
dados_idade %>%
  ggplot() +
  geom_col(aes(x = avaliacao_saude_mental, 
               y = quantidade, 
               fill = faixa_etaria), 
           position = "dodge",
           colour = "black") +
  theme_article() +
  scale_fill_brewer(type = "qual", palette = 1) +
  labs(title = "Relação entre saúde mental e faixa etária",
       x = "Saúde mental",
       y = "Quantidade de alunos",
       fill = "Faixa etária")


# filtrando os dados por hora de trabalho
dados_horas_trabalho <- dados_base %>%
  group_by(horas_trabalho, avaliacao_saude_mental) %>%
  summarise(quantidade = n()) %>%
  arrange(desc(quantidade))
# plotando no ggplot
dados_horas_trabalho %>%
  ggplot() +
  geom_col(aes(x = avaliacao_saude_mental, 
               y = quantidade, 
               fill = as.character(horas_trabalho)), 
           colour = "black") +
  theme_article() +
  scale_fill_brewer(type = "qual", palette = 5) +
  labs(title = "Relação entre saúde mental e horas de trabalho",
       x = "Saúde mental",
       y = "Quantidade de alunos",
       fill = "Horas de trabalho")



# grafico densidade: atividade extracurricular x saude mental
dados_base %>% 
  ggplot(aes(x = avaliacao_saude_mental, color = atividade_extracurricular)) +
  geom_density() +
  theme_article() +
  labs(title = "Gráfico de densidade da saúde mental e atividade extracurricular",
     x = "Saúde mental",
     y = "Densidade",
     color = "Pratica atividade extracurricular")



