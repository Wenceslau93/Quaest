# Avaliação técnica — Data Scientist Jr.


# A atividade consiste em analisar um survey.

# Para começar, vamos definir e conferir o diretório de trabalho.
# O diretório é de acordo com o caminho no seu computador.
setwd("C:/FCD/Quaest")
getwd()

# Instalar o pacote para ler arquivo do formato do excel.
install.packages("readxl")

# Liberar pacotes para que possam ser utilizados.
library(gdata)
library(readxl)
library(gmodels)
library(dplyr)
library(ggplot2)

# Carregando o dataset.
df <- read_excel("bd_surveyquaest.xlsx", sheet=1, col_names=TRUE)

# Verificando o tipo dos dados.
str(df)

# Lendo as primeiras linhas.
View(head(df))

# Calculando a quantidade dos respondente do sexo masculino e feminino.

# Termos mais respondente do sexo feminino.
table(df$sexo)

# Calculando a quantidade de voto que o candidatos receberam e ordenando do maior para o menor.

# Podemos ver que o Canditato 2 teve mais votos.
frequencia <- sort(table(df$voto1), decreasing = TRUE)
frequencia

# Criando a tabela de contingência com candidato e sexo.

# Na maioria dos casos, não tivermos uma grande diferença entre os votos dos homens e mulheres.
CrossTable(df$voto1, df$sexo, 
           prop.r=TRUE, prop.c=FALSE, prop.t=FALSE, prop.chisq=FALSE, 
           digits=2)

# Criando gráfico da quantidade de votos que o candidtato obteve.

# Aqui podemos ver uma enorme diferença de votos que o Candidato 2 recebeu aos demais candidatos.
df %>%
  group_by(voto1) %>%
  summarise(n = n()) %>%
  ggplot(aes(voto1, n, label = n)) +
  geom_col() +
  geom_bar(stat = "identity", fill = "RoyalBlue") +
  geom_text(aes(y = n), vjust = -0.1) +
  labs(title = "Frequencia de votos por Candidato", x = "Candidato", y = "Frequência") +
  theme_grey(base_size = 10)

# Verificando a quantidade de avaliação do governo.

# Tivemos mais quantidade de avaliação Boa.
table(df$aval_gov)

# Criando tabela cruzada entre candidato e avaliação do governo.

# Tendo novamente boa avaliação para o candidato 2
table(df$voto1, df$aval_gov)

# Verificando a quantidade de avaliação Ótima por candidato.

# Tendo o Cadidato 2 com Ótima avaliação.
ava_otima <- filter(df, aval_gov == "Ótima")
df_otima <- data.frame(table(ava_otima$voto1, ava_otima$aval_gov))
df_otima$Var2 = NULL
colnames(df_otima)[1]<-'Candidato'
colnames(df_otima)[2]<-'Qtd_Otima'

df_otima %>% 
  arrange(desc(Qtd_Otima))

# Verificando a quantidade de avaliação Péssima por candidato.

# Aqui vermos que quem opinou por Ninguém/Branco/Nulo, se teve maior quantidade em avaliar em péssimo.
ava_pessima <- filter(df, aval_gov == "Péssima")
df_pessima <- data.frame(table(ava_pessima$voto1, ava_pessima$aval_gov))
df_pessima$Var2 = NULL
colnames(df_pessima)[1]<-'Candidato'
colnames(df_pessima)[2]<-'Qtd_Pessimo'

df_pessima %>% 
  arrange(desc(Qtd_Pessimo))

# Vamos montar um gráfico mostrando relação do voto e avaliação do governo.

# O candidato 2 teve grande quantidade de avaliação Ótima e Boa.
# Ninguém/Branco/Nulo, NS/NR e Candidato 8, teve mais avaliação negativa.
# Os candidatos de 3 a 7, não teve avaliação de Ótima.

ggplot(df, aes(voto1, aval_gov)) + geom_count() + geom_blank()
