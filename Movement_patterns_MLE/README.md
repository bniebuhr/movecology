---
title: "Distinguindo entre padrões de movimento animal usando a abordagem da verossimilhança"
author: "Bernardo B. S. Niebuhr"
fontsize: 12pt
output:
  pdf_document:
    highlight: tango
    toc: no
  html_document:
    theme: united
documentclass: article
---

* Laboratório de Ecologia Espacial e Conservação, Departamento de Ecologia, UNESP - Rio Claro
* bernardo_brandaum@yahoo.com.br

### Introdução

A ecologia descreve os organismos por meio de suas interações - consumo de recursos,
interações sociais, competição entre indivíduos próximos, predação, mutualismos, e
a lista pode continuar indefinidamente. De qualquer maneira, um processo é necessário para que organismos e outros elementos
que compõe a natureza se acoplem no espaço e no tempo, de forma a possibilitar a ocorrência de tais interações: o movimento.

Quando pensamos no contexto da movimentação de animais, podemos imaginar diversos motivos que os levam a se mover: busca por alimento
ou água, busca por parceiros reprodutivos ou por evitar estar no mesmo território que outros indivíduos, dispersão entre manchas de 
habitat ou áreas de vida etc. Ao mensurarmos o movimento dos animais, seja por métodos mais diretos seja pela utilização de complexos
aparatos tecnológicos, os padrões de movimento refletem essa multiplicidade de comportamentos, atividades e motivações. 

Um resultado típico de um processo de quantificação de uma trajetória animal é uma sequência de segmentos que representam os 
deslocamentos do animal, entre os quais há mudanças na direção em que o animal se move. Assim, por meio da mensuração de sua
posição em intervalos de tempo regulares, chega-se a um conjunto de deslocamentos (chamados aqui de "passos", mas podendo se referir
ao qualquer tipo de movimento) e de ângulos de virada entre esses deslocamentos. Esse conjunto de dados pode ser visualizado na
forma de distribuições de frequência^[Veja a seção Distribuições de probabilidade.]. Por sua vez, imbuído nessas distribuições
estão os processos que levam os animais a se movimentarem. Um animal se alimentando em um local com muitos recursos, por exemplo, 
provavelmente evitará se deslocar por grandes distâncias e dará passos predominantemente curtos, com ângulos de virada grandes, 
visitando os mesmos locais várias vezes^[A forma mais simples de representar esse comportamento é por meio de caminhadas aleatórias Brownianas; 
veja mais na seção de Análise de padrões de movimento.]. 
Um animal em um ambiente com menos recursos pode tender a mesclar passos curtos nos locais
de alta densidade de recursos com passos mais longos entre manchas de recursos. Um animal que possui um ninho ou toca, por sua vez, 
na maioria das vezes deve evitar se distanciar demasiadamente de seu abrigo, para evitar ser predado ou ficar demasiadamente cansado.
Muitos são os processos que podem gerar distintos padrões de movimento animal; o papel do ecólogo é justamente observar e compreender
quais mecanismos estão levando os animais a se moverem como se movem.

Dentro desse contexto, o primeiro passo para se compreender a movimentação animal^[Aqui estamos falando em animais, mas essa 
abordagem pode ser ampliada para o contexto de outros organismos.] é identificar o padrão de movimento, isso é, identificar a distribuição de probabilidade dos dados de movimento, para então inferir sobre os processos que podem estar levando a esse padrão. O presente texto busca justamente tratar desse primeiro passo, utilizando a abordagem da verossimilhança para distinguir entre modelos de distribuição concorrentes que sejam mais adequados para explicar os dados de movimento. 

Primeiramente será feita uma breve revisão sobre conceitos de distribuições de probabilidade e de função de verossimilhança; 
então, esse panorama será aplicado para dintinguir entre modelos concorrentes de movimentação animal, utilizando dados de movimentação simulados.
Por fim, alguns comentários serão feitos com relação a outras possíveis análises a serem feitas partindo dessa abordagem.

#### Distribuições de probabilidade


Ao observarmos um processo na natureza e o quantificarmos, a cada vez que o observamos pode ser que o resultado seja distinto. 
Isso pode ocorrer por erros ou incertezas no processo de medição, mas pode também haver uma variação inerente ao processo em questão.
 Por exemplo, ao contabilizarmos todos os pássaros que visitam uma certa árvore em uma manhã, e repetirmos essa obsevação por vários 
 dias seguidos, certamente o número de indivíduos de cada espécie que visita a árvore variará entre os dias. Essa variação não se deve a problemas de identificação da espécie ou tempo de observação, mas ocorre pois a visita de aves a uma árvore é um processo estocástico, variável no tempo e no espaço.
A cada dia de observação, podemos abstrair a quantidade de espécies que ocorre na região e que visitou não visitou a árvore, que 
visitou uma vez, que visitou duas vezes e assim por diante. A quantidade de espécies em cada classe de número de ocorrências é chamada 
de frequência de observação, e
temos então uma distribuição de frequências. A extensão da ideia de uma distribuição de frequências, em que alguns eventos são 
observados mais frequentemente que outros, leva à noção de distribuições de probabilidade.

Distribuições de probabilidade são conjuntos de probabilidades atribuídos a um conjunto de eventos ou possíveis resultados de uma observação ou 
medição. No caso da contagem do número de indivíduos de cada espécie que visita uma árvore, tem-se uma distribuição discreta, isso é, uma processo
em que o que é observado pode ser traduzido em contagens de números inteiros. Outros exemplos de distribuições discretas 
são o número de espécies que ocorre em um fragmento,
o número de animais de uma ninhada que sobrevive até uma certo ano ou o número de plântulas que morrem devido a predação em parcelas de restauração
florestal. Já no caso da observação do movimento de um animal, ao se medir o comprimento do passo e os ângulos de virada tem-se duas variáveis
que seguem distribuições contínuas - elas podem assumir qualquer valor real dentro de um determinado intervalo.

Qualquer que seja o caso, a distribuição de probabilidade é uma função matemática que atribui uma probabilidade de ocorrência para cada evento 
(isso é, cada valor da variável) possível de ser observado. Por ser uma função matemática, distribuições de probabilidade "transformam" um número,
correspondente ao valor observado da variável, em uma probabilidade de que aquele número ocorra. Como esse valor não é conhecido, cabe aos 
pesquisadores observar o processo em questão na natureza e inferir sobre ele a partir dos dados coletados.

No caso de distribuições de probabilidade contínuas, o mais comum é se pensar em distribuições de densidade de probabilidade. Densidades de probabilidade
podem assumir valores maiores que 1, mas o que faz sentido de se perguntar diante de tal distribuição é: qual é a probabilidade de se observar um 
valor da variável dentro de um determinado intervalo (por exemplo, de observar um tamanho de passo entre 1 e 10 metros, para a movimentação de uma
onça-pintada?). Uma função de densidade de probabilidade (*fdp*) é representada por um conjunto de parâmetros $\theta$, que irão atribuir probabilidades a cada
possível observação: 

$$fdp = f(x) = f(X=x | \theta)$$

onde X é a variável observada e x é o conjunto de valores a serem observados para essa variável.
Em outras palavras, se atribuirmos valores aos parâmetros, a *fdp* vai retornar um valor de probabilidade para cada intervalo de valores possíveis de 
serem observados^[Não tratarei nesse texto dos aspectos técnicos da distribuições de probabilidade, mas simplesmente farei uma explicação breve
dos conceitos; para uma visão mais técnica, ver o Capítulo 4 de Bolker (2008).].

O ponto central é que, ao se observar um processo ecológico (por exemplo, a movimentação de um animal), pode-se representar essa informação por
meio de uma distribuição de frequências, e então realizar inferências a respeito da distribuição de probabilidades do processo em questão. Olhar para
as distribuições de probabilidade é o primeiro passo para investigar os mecanismos subjacentes por trás dos padrões de movimento observados^[Cada
distribuição de probabilidade representa um modelo teórico para explicar um padrão observado; aqui, ao utilizar o termo "padrões de movimentação",
estaremos nos referindo às possíveis distribuições ou modelos que as características de movimento seguem.].

#### A função de verossimilhança

Ao falarmos em *fdp*'s, pensamos em atribuir um conjunto de parâmetros a uma função para obter a probabilidade de se observar cada valor em um
determinado contexto. Entretanto, ao se ir realizar uma observação de uma fenômeno, como a trajetória de um animal forrageando, o que tem-se em mãos
é um conjunto de dados (o qual pode ser organizado na forma de uma distribuição), mas não se sabe qual é a distribuição que gerou os dados, nem quais os 
parâmetros dessa distribuição, conjunto que atribui probabilidade às observações feitas.

Uma saída, então, é pensar não em funções de densidade probabilística, mas em funções de verossimilhança. Imagine que, agora, ao invés de fixarmos os
parâmetros de uma *fdp* e atribuir probabilidade a observar cada valor para os dados, vamos fixar as observações das variáveis - no caso, o comprimento
dos passos em um trajetória de movimento - e olhar para uma função dos parâmetros, que atribui uma probabilidade a cada valor deles para ter gerado os 
dados observados. Em outros termos, busca-se, a partir do conjunto de dados observados, construir uma função de densidade que retorne 
a plausibilidade de cada conjunto de valores de parâmetros. Essa é a função de verossimilhança. Na forma mais usual, ela é escrita como:

$$\mathcal{L} (\theta | X=x) = f(\theta | X = x)$$

Essa função não atribui uma probabilidade, no sentido estrito^[A função de verossimilhança não possui as propriedades de uma distribuição de 
probabilidades.], mas uma plausibilidade, verossimilhança ou força de evidência para cada conjunto de parâmetros de uma distribuição, baseado em um
conjunto de dados. A função nada mais é do que a própria *fdp* com os valores observados fixos e tendo-se os parâmetros como variáveis.

Uma vez construída a função de verossimilhança, busca-se o valor que atribui máxima plausibilidade para que os parâmetros tenham gerado os dados observados, 
o estimador de máxima verossimilhança (MLE). Ao redor do MLE se concentram outros valores dos parâmetros que são também muito plausíveis; portanto, é usual
contruir um intervalo de verossimilhança, definido como o conjunto de valores de parâmetro que são igualmente verossímeis, dados os dados. Como a 
verossimilhança é diferente de uma probabilidade, ela não possui interpretação absoluta, somente uma interpretação relativa: só pode-se pensar na 
verossimilhança de um valor de parâmetro em relação a outro, ou de um modelo em relação a outro, e não em termos absolutos, em relação a todos os outros
valores ou modelos possíveis.

#### A função de log-verossimilhança negativa

Na prática, é usual utilizar a função de log-verossimilhança negativa, ao invés da função de verossimilhança. Ela corresponde simplesmente ao negativo
do logaritmo natural^[Logatimo com base e $\sim$ 2,71.] da função de verossimilhança:

$$-L(\theta | X=x) = -ln[\mathcal{L} (\theta | X=x)]$$

Essa transformação facilita os cálculos, uma vez que multiplicações de verossimilhanças (geralmente com valores muito pequenos, com muitas casas decimais)
se transformam em somas de números mais fáceis de se tratar. Nesse caso, o MLE então representa o valor que minimiza a função de log-verossimilhança 
negativa, e razões de evidência de um
conjunto de parâmetros e favor de outro ou de um modelo em favor de outro se tornam diferenças de valores de log-verossimilhança negativa.

#### Seleção de modelos concorrentes

Uma vez que conhece-se um conjunto de distribuições de probabilidade a que as observações realizadas possam estar relacionadas, e que essas observações
foram realizadas, pode-se então buscar as estimativas de parâmetros mais plausíveis para cada modelo. Entretanto, permanece uma pergunta: qual é o modelo
mais apoiado pelas observações? Que tipo de processo estocástico está relacionado ao processo ecológico observado, a partir do qual podemos então buscar
compreender a variabilidade desse processo e quais os mecanismos por trás dele?

Um valor que dá informação de quão bem uma distribuição com um conjunto de parâmetros se ajusta a um conjunto de dados é o valor da verossimilhança 
(log-verossimilhança negativa): quanto mais alto (baixo) esse valor, mais o modelo se ajusta às observações. Entretanto, essa estimativa é enviesada:
quanto mais parâmetros tem uma distribuição, geralmente se obterá um ajuste melhor aos dados, mesmo que esse ajuste possa em muitos casos não se referir
de fato ao processo estocástico subjacente, mas a uma observação (ou conjunto de observações) em particular. Para lidar com esse viés, há várias propostas
de *critérios de informação* que fazem um balanço entre o ajuste do modelo aos dados e o número de parâmetros e observações. Os critérios de informação
expressam quanta informação é perdida relativamente entre modelos, ao expressar um processo (desconhecido) que gerou os dados observados. 
Um dos critérios mais utilizados é o *Critério de Informação de Akaike* (AIC):

$$AIC \ = \ -2\ L( \widehat{\theta}\, | \,y) + 2 K$$

Onde $L( \widehat{\theta}\, | \,y)$ é a log-verossimilhança máxima (avaliada nos MLEs $\widehat{\theta}$), e 
$K$ é o número de parâmetros do modelo. Quanto menor o primeiro termo dessa relação, melhor o ajuste aos dados; entretanto, quanto maior o número de 
parâmetros, mais alto o valor do AIC. Por meio desse balanço, busca-se, entre um conjunto de modelos, aquele mais plausível, isso é, aquele com menor 
valor de AIC, que melhor se ajusta aos dados, seja qual for seu número de parâmetros. Usualmente considera-se quaisquer modelos com uma diferença entre
AIC's menor ou igual a 2 como igualmente plausíveis^[Isso representa modelos com razão de verossimilhança menor ou igual a aproximadamente 7.], mas 
essa é apenas um convenção e não uma regra a ser seguida à risca.

### Analisando padrões de movimento animal

O que buscamos fazer nessa seção é utilizar a abodagem da verossimilhança e da seleção de modelos para distinguir entre padrões de movimentação animal,
utilizando dados simulados de movimento. Os primeiros modelos de movimento animal que surgiram consideravam as trajetórias como uma movimento Browniano,
isso é, um movimento sem direção preferencial (distribuição uniforme dos ângulos de virada) e uma comprimento de passo esperado médio seguindo uma 
distribuição exponencial, com uma escala caracvterística de movimento (ou, alternativamente, uma distribuição Gaussiana; Viswanathan et al. 2011). 
Entretanto, um conjunto crescente de estudos
analisando trajetórias de movimento animal começou a juntar evidências de processos de movimento de Lévy, caracterizados por distribuições de leis de
potência (também chamadas de distribuições de Lévy)^[Para saber mais, ver Newman (2005).]. Esse padrão é caracterizado por conjuntos de passos curtos, 
separados por alguns poucos (mas não tão raros)
deslocamentos surpreendentemente longos. Pode-se pensar ainda em outros modelos que caracterizem bem padrões de movimento, como distribuições Gama, 
que são mais flexíveis e podem representar movimentos em diferentes contextos e com motivações distintas, ou modelos de mistura de distribuições, 
representando, por exemplo, o movimento em duas escalas espaciais distintas.

O método tradicional de se avaliar o padrão de movimentação, que dava suporte para distribuições de Lévy, consistia em (i) plotar alguma forma de histograma
do comprimento de passo do movimento, em escala log-log; (ii) fazer uma regressão linear simples desses dados; e (iii) adotar o coeficiente 
angular dessa regressão
como o coeficiente da lei de potência^[Esse procedimento justifica-se pois, ao transformar uma distribuição de lei de potência logaritmicamente, obtem-se
uma linha reta.]. Entretanto, Edwards et al. (2007, 2008) mostraram que esse método não é eficiente e pode levar a resultados equivocados. O que eles 
propõe é justamente aplicar a abordagem da verossimilhança, para distinguir a plausibilidade de modelos explicativos concorrentes.

#### "Estudo de Caso" 1: uma mistura de dois tipos de movimento

Para exemplificar a aplicação da abordagem da verossimilhança para distinguir entre padrões de movimento animal, vamos simular uma caminhada
aleatória Browniana composta de duas fases: uma fase de movimento intensivo, em escala fina (representando, por exemplo, o movimento dentro de uma
mancha de recursos), e uma fase de movimento extensivo, com passos mais longos (representando o movimento de dispersão entre fragmentos ou manchas
de habitat). A trajetória consiste em 600 pontos, com duas passagens por cada fase de movimento, e foi gerada utilizando o pacote 
[adehabitatLT][adeLT].

[adeLT]: http://cran.r-project.org/web/packages/adehabitatLT/index.html "adehabitatLT package"

```{r}
# Carregando pacotes
library(adehabitatLT)

# Simulando a trajetória
# Combinação de duas trajetórias Brownianas com diferents parâmetros de escala
set.seed(1210)

path1 <- simm.brown(1:150, x0=c(0,0), h=5)
x <- path1[[1]]$x[150]; y <- path1[[1]]$y[150] 
path2 <- simm.brown(151:301, x0=c(x, y), h=30)
x <- path2[[1]]$x[151]; y <- path2[[1]]$y[151]
path3 <- simm.brown(302:452, x0=c(x,y), h=5)
x <- path3[[1]]$x[151]; y <- path3[[1]]$y[151]
path4 <- simm.brown(453:603, x0=c(x,y), h=30)

nlinhas <- 600
path <- as.data.frame(matrix(NA, nrow=nlinhas, ncol=4, dimnames=list(1:nlinhas, 
                      c("name", "date", "X", "Y")))) 
path$name <- "animal1"
path[2:4] <- rbind(path1[[1]], path2[[1]][-1,], 
                   path3[[1]][-1,], path4[[1]][-1,])[c(3,1,2)]
path.comb <- as.ltraj(xy = path[,c("X", "Y")], date=path$date, id=path$name)
```

Para visualizar essa trajetória, com início no ponto (0,0) do plano, bem como a variação do comprimento de passo ao longo do tempo, 
pode-se utilizar os seguintes comandos^[Ou, altenativamente, pode-se utilizar a função *trajdyn* do pacote [adehabitatLT][adeLT] para uma visualização dinâmica do processo de movimento.]:

```{r fig.width=10, fig.height=5, R.options=par(mfrow=c(1,2))}
par(mfrow = c(1,2))
plot(path.comb, xlab="x", ylab="y")        	# plotando a trajetória
abline(h=0, lty=2, col=4); abline(v=0, lty=2, col=4)
data = path.comb[[1]]$dist[-600]
# plotando o comprimento de passo vs. tempo
plot(data, type="l", xlab="Tempo (h)", ylab="Comprimento de passo (m)")
abline(v=c(150, 300, 450), lty=2, col=4)    # mudanças no modo de movimento
```

As figuras geradas estão mostradas acima. Pode-se observar claramente duas fases de movimento, uma mais restrita, outra com deslocamentos maiores.

O passo seguinte consiste em construir as funções de log-verossimilhança negativa. Faremos isso para o modelo de exponencial, Gaussiano, Gama, Lévy e para uma 
mistura de duas distribuições gaussianas. As funções de verossimilhança negativa para os três primeiros modelos estão definidas abaixo:

```{r}
# Dados de comprimento de passo
data = path.comb[[1]]$dist[-600]

# Modelo 1
# Exponencial
LLexp <- function(lambda){
  -sum(dexp(data, rate=lambda, log=T))
}

# Modelo 2
# Gaussiana
LLgauss <- function(mu, sigma){
  -sum(dnorm(data, mean=mu, sd=sigma, log=T))  
}

# Model 3
# Gama
LLgama <- function(forma, escala) {
  -sum(dgamma(data, shape=forma, scale=escala, log=T))
}
```

E, a seguir, são definidas as funções de para a distribuição de Lévy e para o modelo de mistura de Gaussianas:

```{r fig.show='hide'}
# Modelo 4
# Lei de potência
dpowlaw <- function(x, alfa, xmin, log=FALSE){
  c <- (alfa-1)*xmin^(alfa-1)
  if(log) ifelse(x < xmin, 0, log(c*x^(-alfa)))
  else ifelse(x < xmin, 0, c*x^(-alfa))
}

# Testando
curve(dpowlaw(x, alfa=2.5, xmin=1), from=0, to=100, log="")
curve(dpowlaw(x, alfa=2.5, xmin=1), from=1, to=100, log="xy")
#integrate(dpowlaw, -Inf, Inf,  alfa=2, xmin=1)

# Levy - função de log-verossimilhança negativa
LLlevy <- function(mu, xmin){
  #if(mu <= 0) mu <- runif(1, 0, 5)
  -sum(dpowlaw(data, alfa=mu, xmin=xmin, log=T))  
}

# Modelo 5
# Modelo de mistura entre duas distribuições Gaussianas
mix <- function(x, mu1, mu2, sig1, sig2, a, log=FALSE){
  z <- a*dnorm(x, mean=mu1, sd=sig1) + 
  (1-a)*dnorm(x, mean=mu2, sd=sig2)
  if(log) log(z)
  else z
}

# Testando a função
curve(mix(x, mu1=1, mu2=10, sig1=2, sig2=3, a=0.5), -5, 15)
#integrate(mix, -Inf, Inf,  mu1=1, mu2=10, sig1=2, sig2=3, a=0.5)

# Função de log-verossimilhança negativa
LLmist <- function(mu1, mu2, s1, s2, a){
  -sum(mix(data, mu1, mu2, s1, s2, a, log=TRUE))
}
```

Por fim, utilizamos a função *mle2* do pacote [*bbmle*][bbmle] para encontrar os valores de máxima verossimilhança de cada modelo, e a função 
AICtab para calcular os valores de AIC entre cada e comparar os modelos:

[bbmle]: http://cran.r-project.org/web/packages/bbmle/bbmle.pdf "bbmle package"

```{r warning=FALSE}
library(bbmle)
mexp <- mle2(LLexp, start=list(lambda=1/mean(data)))
mgauss <- mle2(LLgauss, start=list(mu=mean(data), sigma=sd(data)))
mgama <- mle2(LLgama, start=list(forma=mean(data)**2/var(data), 
                                 escala=var(data)/mean(data)))
mlevy <- mle2(LLlevy, start=list(mu=2), fixed=list(xmin=min(data)))
mmist <- mle2(LLmist, start=list(mu1=mean(data)+1, mu2=mean(data), 
                                 s1=sd(data), s2=sd(data), a=0.5))

AICtab(mexp, mgauss, mgama, mlevy, mmist, base=T, weights=T)
```

A comparação de modelos mostra uma vantagem clara do modelo de mistura de Gaussianas como modelo mais plausível para explicar os dados; isso
é, o padrão gerado inicialmente foi detectado. Essa abordagem parece ser muito eficiente e útil na identificação de padrões de movimentação, 
ainda que, para dados reais, seja difícil distinguir entre alguns modelos. Abaixo, por fim, é mostrada a figura do histograma dos dados de 
comprimento de passo e o ajuste de cada modelo. É possível ver claramente que o modelo que melhor se ajusta é o modelo de mistura.


```{r echo=FALSE}
# Ploting
hist(data, breaks=50, prob=T, xlab="Step length", ylab="Probability density", main="")
curve(dexp(x, rate=coef(mexp)), col=2, add=T)
curve(mix(x, mu1=coef(mmist)[1], mu2=coef(mmist)[2], sig1=coef(mmist)[3], sig2=coef(mmist)[4], 
              a=coef(mmist)[5]), col=4, add=T)
curve(dgamma(x, shape=coef(mgama)[1], scale=coef(mgama)[2]), col=1, add=T)
curve(dpowlaw(x, alfa=coef(mlevy)[1], xmin=coef(mlevy)[2]), col=6, add=T)
curve(dnorm(x, mean=coef(mgauss)[1], sd=coef(mgauss)[2]), col=3, add=T)
legend("topright", legend=c("M1: exponencial", "M2: Gaussiana", "M3: Gama",
                            "M4: Lévy", "M5: mistura"), col=c(2,3,1,6,4), lwd=2)
```

### Indo além: outras análises e possibilidades

Aqui ajustamos modelos de distribuição com parâmetros fixos a dados simulados de movimentação. Entretanto, ao fazermos isso, estamos simplesmente
fazendo um ajuste fenomenológico e ignorando os mecanismos e fatores que influenciam na formação desse padrão. O passo seguinte, então, para além
dessa identificação de padrões, é desenvolver modelos lineares (ou não lineares) para investigar como variáveis diversas (como variáveis ambientais,
da paisagem, ou da presença de recursos ou outros animais, por exemplo) influenciam nos parâmetros das distribuições de tamanho de passo (ou de
outras características relacionadas a movimento). Além disso, outras possibilidades de abordagem incluem modelos de detecção imperfeita, 
levando em conta a incerteza na medida dos pontos de deslocamento, abordagem que se aproxima do que é chamado na literatura de *state-space models*
(veja Patterson et al. 2008).

### Referências bibliográficas

Batista, J.L.F. 2009. Verossimilhança e Máxima Verossimilhança.

Bolker, B.M. 2008 Ecological Models and Data in R Princeton: Princeton University Press.

Burnham, K.P., Anderson, D.R. 2002. Model Selection and Multimodel Inference: 
A Practical-Theoretic Approach, 2nd ed. New York, Springer-Verlag.

Edwards, A.M. et al. 2007. Revisiting Lévy flight search patterns of wandering
albatrosses, bumblebees and deer. Nature, 449, 1044-1049.

Edwards, A.M. 2008. Using likelihood to test for Lévt flight search patterns and for general power-law distributions in nature.
Journal of Animal Ecology, 77, 1212-1222.

Patterson, T.A. et al. 2008. State-space models of individual animal movement. Trends in Ecology and Evolution,
23(2), 87-94.

Newman, M.E. 2005. Power laws, Pareto distributions and Zipf's law. Contemporary physics, 46(5), 323-351.

Viswanathan, G.M. et al. 2011. The Physics of Foraging: an Introduction to Random Searches and 
Biological Encounters. Cambridge University Press, Cambridge.