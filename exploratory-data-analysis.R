library(tidyverse)
library(ggplot2)
theme_set(theme_bw())

avaliacoes <- read_csv("data/3-avaliacao-humana/avaliacoes-20181030.csv")
reclamacoes <-  read_csv("data/1-reclamacoes-selecionadas/reclamacoes-avaliadas.csv")

#inserindo colunas comprimento da reclamacao e do titulo
reclamacoes <- reclamacoes %>% mutate(reclamacao.length = str_length(reclamacao),
                                      titulo.length = str_length(titulo))

# insere coluna com número de letras em capslock
reclamacoes$numero.de.capslock.reclamacao <- str_count(reclamacoes$reclamacao, "\\b[A-Z]{2,}\\b")

#insere coluna com número de letras em capslock no titulo
reclamacoes$numero.de.capslock.titulo <- str_count(reclamacoes$titulo, "\\b[A-Z]{2,}\\b")

###############################################################################################################################

# insere coluna com número de exclamações no titulo e na reclamação
reclamacoes$numero.de.exclamacoes <- str_count(reclamacoes$reclamacao, "!")

# cria um data frame para cada orgão
reclamacoes_anac <- filter(reclamacoes, reclamacoes$orgao=="anac-agencia-nacional-de-aviacao-civil")
reclamacoes_inss <- filter(reclamacoes, reclamacoes$orgao=="inss-ministerio-da-previdencia-social")

# Das avaliações realizadas, quantas foram feitas para cada nível de insatisfação?
(ggplot(avaliacoes, aes(x=`Grau de insatisfação`))
    +geom_bar(width=0.5)
    +labs(title="Número de avaliações por grau de insatisfação", x="Grau de insatisfação", y="Quantidade de avaliações")
)

# Ver nota média das medianas da ANAC e do INSS
mean(reclamacoes_anac$mediana)
mean(reclamacoes_inss$mediana)

# Ver proporção do tamanho da reclamação para cada nível de insatisfação em relação a ANAC e INSS
(ggplot(reclamacoes, aes(x=mediana, y=reclamacao.length, colour=orgao))
    +geom_point()
    +labs(title="Tamanho das reclamações por grau de insatisfação", x="Grau de insatisfação geral", y="Quantidade de caracteres da reclamação")
    +facet_grid(orgao ~ .)
)
cor_anac <- cor(reclamacoes_anac$mediana, reclamacoes_anac$reclamacao.length)
cor_inss <- cor(reclamacoes_inss$mediana, reclamacoes_inss$reclamacao.length)
mat <- matrix(c(cor_anac, cor_inss), nrow=1, ncol=2)
colnames(mat) <- c("cor_anac", "cor_inss")
mat

# Ver proporção da quantidade de exclamações pra cada nível de insatisfação em relação a ANAC e INSS
(ggplot(reclamacoes, aes(x=mediana, y=numero.de.exclamacoes, colour=orgao))
    +geom_point()
    +labs(title="Quantidade de exclamações por grau de insatisfação", x="Grau de insatisfação geral", y="Quantidade de exclamações")
    +facet_grid(orgao ~ .)
)
cor_anac <- cor(reclamacoes_anac$mediana, reclamacoes_anac$numero.de.exclamacoes)
cor_inss <- cor(reclamacoes_inss$mediana, reclamacoes_inss$numero.de.exclamacoes)
mat <- matrix(c(cor_anac, cor_inss), nrow=1, ncol=2)
colnames(mat) <- c("cor_anac", "cor_inss")
mat

# Ver proporção da quantidade de letras maiúsculas pra cada nível de insatisfação em relação a ANAC e INSS
(ggplot(reclamacoes, aes(x=mediana, y=numero.de.capslock, colour=orgao))
    +geom_point() 
    +labs(title="Quantidade de letras em caixa alta por grau de insatisfação", x="Grau de insatisfação geral", y="Quantidade de palavras em caixa alta")
    +facet_grid(orgao ~ .)
)
cor_anac <- cor(reclamacoes_anac$mediana, reclamacoes_anac$numero.de.capslock)
cor_inss <- cor(reclamacoes_inss$mediana, reclamacoes_inss$numero.de.capslock)
mat <- matrix(c(cor_anac, cor_inss), nrow=1, ncol=2)
colnames(mat) <- c("cor_anac", "cor_inss")
mat

###############################################################################################################################

#qual o formato das distribuições dos tamanhos dos títulos e das reclamações por órgão
reclamacoes %>% 
  filter(complete.cases(.)) %>% 
  ggplot(aes(fill = orgao, x = titulo.length), na.rm = TRUE) + 
  geom_histogram(binwidth = 2, na.rm = TRUE) + 
  facet_grid(orgao ~ .)

reclamacoes %>% 
  filter(complete.cases(.)) %>% 
  ggplot(aes(fill = orgao, x = reclamacao.length), na.rm = TRUE) + 
  geom_histogram(binwidth = 50, na.rm = TRUE) + 
  facet_grid(orgao ~ .)

#Percebemos que as reclamações do INSS são em média menores do que as reclamações da anac

avaliacoes <- avaliacoes %>% 
              select(avaliador = `Matrícula`, 
                      id = `ID da reclamação`, 
                       insatisfacao = `Grau de insatisfação`)

avaliacoes_por_id <- avaliacoes %>% group_by(id) %>% 
    summarise(range = max(insatisfacao) - min(insatisfacao),
              mediana = median(insatisfacao),media = mean(insatisfacao),
              maximo =  max(insatisfacao), minimo = min(insatisfacao))


reclamacoes %>% inner_join(avaliacoes_por_id, by = "id") %>% View()
## Será que podemos confiar em nossas avaliações humanas?

#alguma avaliação foge dos valores de 1 a 5?
avaliacoes %>% 
  filter((id %in% 1:5 ))

#quantas avaliações foram feitas por reclamação?
avaliacoes %>% 
  group_by(id) %>% 
  count() %>% 
  ggplot(aes("reclamacoes", n)) + 
  geom_jitter(width = .05, alpha = .7)

    
# em média, quantas avaliações por reclamação?
avaliacoes %>% 
  group_by(id) %>% 
  count() %>%
  ungroup() %>% 
  summarise(media = mean(n), 
            mediana = median(n))

#mostra número de revisores por reclamação
avaliacoes %>% group_by(id) %>% 
  summarize(count=n()) %>% 
  ggplot(aes(x=reorder(id, count), y=count)) + geom_bar(stat = "identity")

# Será que há consenso entre as avaliações de cada reclamação?
#  níveis de discordância X id da reclamação
avaliacoes %>% group_by(id) %>% 
  summarise(range = max(insatisfacao) - min(insatisfacao),
            mediana = median(insatisfacao)) %>% 
  ggplot(aes(x=id, y=range, colour = id)) + geom_point() +
  geom_jitter(height = 0.05, alpha = .4)

# vemos que para algums reclamações houve uma discordância de até 3 níveis de insatisfação
# níveis de discordância X nível médio de insatisfação
# não parece haver relação entre essas variáveis
avaliacoes %>% group_by(id) %>% 
  summarise(range = max(insatisfacao) - min(insatisfacao),
            mediana = median(insatisfacao)) %>% 
  ggplot(aes(x=mediana, y=range)) + geom_point() +
  geom_jitter(height = 0.05, alpha = .4)

# a maioria das avaliações tem nível de discordância de 1 e 2
avaliacoes %>% group_by(id) %>% 
  summarise(range = max(insatisfacao) - min(insatisfacao),
            mediana = median(insatisfacao)) %>% 
  group_by(range) %>% count()

# quantas reclamações tem discordância maior que 2?
avaliacoes %>% group_by(id) %>% 
  summarise(range = max(insatisfacao) - min(insatisfacao)) %>% 
  filter(range > 2) %>% count()

# que reclamações são essas?
avaliacoes %>% group_by(id) %>% 
  summarise(range = max(insatisfacao) - min(insatisfacao)) %>% 
  filter(range > 2) %>% inner_join(reclamacoes, by = "id") %>% View()

avaliacoes %>% group_by(id) %>% 
  summarise(range = max(insatisfacao) - min(insatisfacao)) %>% 
  filter(range > 2) %>% inner_join(reclamacoes, by = "id") %>% 
  ggplot(aes(fill = orgao, x = reclamacao.length), na.rm = TRUE) + 
  geom_histogram(binwidth = 60, na.rm = TRUE) + 
  facet_grid(orgao ~ .)
# o que fazer com essas reclamações?
  

# Já vimos que as reclamações da ANAC são maiores. Outra forma de ver é através de boxplots
reclamacoes %>% group_by(orgao) %>% 
  ggplot(aes(x=reorder(orgao, reclamacao.length), y=reclamacao.length)) + geom_boxplot()

## Será que os tamanhos das reclamações ou títulos tem alguma relação com o nível de insatisfação?
reclamacoes %>% ggplot(aes(x=mediana, y=reclamacao.length)) + geom_point()
reclamacoes %>% ggplot(aes(x=mediana, y=numero.de.capslock)) + geom_point()
reclamacoes %>% ggplot(aes(x=mediana, y=titulo.length)) + geom_point()

# Olhando as variáveis não encontramos relações fortes entre elas
library(GGally)
reclamacoes %>% 
  select(orgao, titulo.length, reclamacao.length, numero.de.capslock.reclamacao, mediana) %>% 
  ggpairs()

