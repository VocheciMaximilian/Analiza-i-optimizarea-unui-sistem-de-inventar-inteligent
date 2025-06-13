# cerinta 1
library(tidyverse)
library(truncnorm)
library(shiny)

nr_zile <- 90 
magazine <- c("magA", "magB")
produse <-c("prod1", "prod2", "prod3")

dateStoc <- expand.grid(zi=1:nr_zile, Magazin=magazine, Produs=produse)

dateStoc <- dateStoc %>% group_by(Produs, Magazin) %>% mutate(
  Cerere = case_when(
    Produs=="prod1" & Magazin=="magA" ~ rpois(n(), lambda=20),
    Produs=="prod1" & Magazin=="magB" ~ rpois(n(), lambda=15),
    
    Produs=="prod2" ~ round(rtruncnorm(n(), a=0, b=Inf, mean=10, sd=5)),
    
    Produs=="prod3" ~ round(rexp(n(), rate = 0.1)),
    
    TRUE ~ 0
  ), 
  Timp_Livrare = case_when(
    Produs=="prod1" ~ rgamma(n(), shape=2, rate=0.5),
    Produs=="prod2" ~ rgamma(n(), shape=3, rate=0.5),
    Produs=="prod3" ~ rgamma(n(), shape=4, rate=0.4),
    TRUE ~ 0
  ),
  
  Produse_defecte = rbinom(n(), size=100, prob=0.03)
)

# cerinta 2
magazinA <- dateStoc %>% filter(Magazin=="magA")
pivotA <- magazinA %>% select(zi, Produs, Cerere) %>% pivot_wider(names_from = Produs, values_from = Cerere, names_prefix = "Cerere_")

#corelatia dintre 2 produse la magazin A
corelatieProd <- cor(pivotA$Cerere_prod1, pivotA$Cerere_prod2)
ggplot(pivotA, aes(x=Cerere_prod1, y= Cerere_prod2)) +
  geom_point(alpha=0.6, color="darkblue") +
  geom_smooth(method="lm", se=FALSE, color="red") +
  labs(
    title="Corelatia prod1 si prod2 (de la magazin A)",
    subtitle="Linia rosie indica tendinta generala",
    x="Cerere zilnica prod1",
    y="Cerere zilnica prod2"
    
  ) + theme_minimal()

#corelatia intre cerere si timpul de livrare
produs2_date <- dateStoc %>% filter(Produs == "prod2")
corelatie_cerere_livrare <- cor(produs2_date$Cerere, produs2_date$Timp_Livrare)
ggplot(produs2_date, aes(x = Timp_Livrare, y = Cerere)) +
  geom_point(alpha = 0.5, color = "coral") +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(
    title = "Corelatia intre cerere si timpul de livrare",
    subtitle = paste("Coeficient de corelatie:", round(corelatie_cerere_livrare, 3)),
    x = "Timp de Livrare (zile)",
    y = "Cerere"
  ) +
  theme_minimal()

# frecventa se refera la numarul de zile in care s-a inregistrat un nr x de cereri

# repartitia conditionata pentru a vedea diferenta dintre cereri pentru produsul 1 in magazinul A si B
ggplot(dateStoc %>% filter(Produs=="prod1"), aes(x=Cerere, fill=Magazin)) + 
  geom_histogram(fill= "coral", color="black", binwidth = 1) +
  facet_wrap(~ Magazin, ncol=1) +
  labs(
    title="Repartitii conditionate: Cererea pentru produsul 1",
    x = "cerere zilnica", 
    y= "frecventa"
  )

# repartitia conditionata a timpului de livrare in functie de produs
ggplot(dateStoc, aes(x=Timp_Livrare)) + 
  geom_histogram(fill = "coral", color= "black", bins=20) + 
  facet_wrap(~ Produs, ncol=1) + 
  labs(
    title = "Repartitii conditionate: timpul de livreare per produs" ,
    subtitle = "Compararea timpilor de livare intre produse", 
    x= "Timp de livrare (zile)",
    y= "Frecventa"
  ) + theme_bw()

# repartitia conditionata a cereri in functie de timpul de livrare
# timp livrare este o variabila continua deci nu putem folosii facet_wrap pentru fiecare valoare posibila
dateStoc_categorizat <- dateStoc %>%
  mutate(Categorie_Livrare = cut(
    Timp_Livrare,
    breaks = c(0, 4, 8, Inf),
    labels = c("Rapid (<4 zile)", "Mediu (4-8 zile)", "Lent (>8 zile)"),
    right = FALSE 
  ))
ggplot(dateStoc_categorizat, aes(x = Cerere)) +
  geom_histogram(fill = "coral", color = "black", binwidth = 2) +
  facet_wrap(~ Categorie_Livrare, ncol = 1) +
  labs(
    title = "Repartitia conditionata a cereri in functie de timpul de livrarei",
    subtitle = "Comparam daca timpul de livrare afecteaza cererea",
    x = "Cerere Zilnica",
    y = "Frecventa"
  )

#repartitia marginala a cereri
media_cererii <- mean(dateStoc$Cerere)
ggplot(dateStoc, aes(x = Cerere)) +
  geom_histogram(binwidth = 1, fill = "coral", color = "black", alpha = 0.9) +
  geom_vline(aes(xintercept = media_cererii), color = "red", linetype = "dashed", size = 1) +
  annotate("text", x = media_cererii + 7, y = 35, label = paste("Media =", round(media_cererii, 1)), color = "red") +
  labs(
    title = "Repartitia marginala a cereri",
    subtitle = "Ajuta la estimarea volumului total de munca al sistemului de logistica.",
    x = "Cerere Zilnica (per produs/magazin)",
    y = "Frecventa"
  ) +
  theme_minimal()

#repartitia marginala a timpului de livrare
media_timp_livrare <- mean(dateStoc$Timp_Livrare)

ggplot(dateStoc, aes(x = Timp_Livrare)) +
  geom_histogram(aes(y = ..density..), fill = "coral", color = "black", alpha = 0.9, bins = 20) +
  geom_density(color = "black", size = 1) + # Suprapunem curba de densitate
  geom_vline(aes(xintercept = media_timp_livrare), color = "red", linetype = "dashed", size = 1) +
  annotate("text", x = media_timp_livrare + 3, y = 0.1, label = paste("Media =", round(media_timp_livrare, 1), "zile"), color = "red") +
  labs(
    title = "repartitia marginala a timpului de livrare",
    subtitle = "Indica eficienta lantului de aprovizionare.",
    x = "Timp de Livrare (zile)",
    y = "Densitate"
  ) +
  theme_minimal()

#repartitia marginala a produselor defecte
prag_qc <- quantile(dateStoc$Produse_defecte, 0.95)

ggplot(dateStoc, aes(x = Produse_defecte)) +
  geom_histogram(binwidth = 1, fill = "coral", color = "black", alpha = 0.9) +
  geom_vline(aes(xintercept = prag_qc), color = "darkred", linetype = "dashed", size = 1) +
  annotate("text", x = prag_qc + 1.5, y = 40, label = paste("Prag QC =", prag_qc), color = "darkred") +
  labs(
    title = "Repartitia marginala a produselor defecte",
    subtitle = "Permite setarea unui prag pentru QC.",
    x = "Numar de Produse Defecte per Lot",
    y = "Frecventa"
  ) +
  theme_minimal()

#testul χ² 
dateCategori <- dateStoc %>% mutate(NivelCerere=case_when(
  Cerere <10 ~ "Mic", 
  Cerere >=10 & Cerere <20 ~ "Mediu",
  Cerere >=20 ~ "Mare",
  TRUE ~ "Necunoscut"
))
tabelContingenta <- table(dateCategori$Produs, dateCategori$NivelCerere)
print("Tabel contingenta:")
print(tabelContingenta)
test_chi <-chisq.test(tabelContingenta)
print(test_chi)

# cerinta 3
#suma cererilor pentru prod 1
cerereTotalaP1 <- dateStoc %>% 
  filter(Produs=="prod1") %>% 
  group_by(zi) %>% 
  summarise(CerereTotala=sum(Cerere))

ggplot(cerereTotalaP1, aes(x = CerereTotala)) +
  geom_histogram(aes(y=..density..), binwidth=1, fill="coral", color="black", alpha=0.8) +
  stat_function(
    fun=function(x) dpois(x, lambda=35),
    geom="point",
    color="red",
    size=2
  ) + 
  labs(
    title="Suma cererilor pentru prod 1",
    subtitle="Histograma vs distribvutie poisson 35",
    x= "Cerere totala zilnica", 
    y= "desitate"
  ) + 
  theme_minimal()

#suma produselor defecte intr-o zi
defecteTotaleZi <- dateStoc %>%
  group_by(zi) %>%
  summarise(DefecteTotale=sum(Produse_defecte))
ggplot(defecteTotaleZi, aes(x=DefecteTotale))+
  geom_histogram(aes(y=..density..), binwidth = 1, fill="coral", color="black", alpha=0.8) + 
  stat_function(
    fun = function(x) dbinom(x, size = 600, prob = 0.03),
    geom = "point",
    color = "red",
    size = 1.2
  ) +
  labs(
    title = "Suma produselor defecte intr-o zi",
    subtitle = "Histograma simulata vs distributie teoretica Binomial(n=600, p=0.03)",
    x = "Numar Total Produse Defecte",
    y = "Densitate"
  ) +
  theme_minimal()

# suma timpilor de livrare pentru 2 produse
timpLivrarePivot <- dateStoc %>% 
  filter(Produs %in% c("prod1", "prod2")) %>%
  select(zi, Magazin, Produs, Timp_Livrare) %>%
  pivot_wider(names_from = Produs, values_from = Timp_Livrare, names_prefix = "Timp_Livrare_")
timpLivrareTotal <- timpLivrarePivot %>% mutate(TimpTotal=Timp_Livrare_prod1 + Timp_Livrare_prod2)

ggplot(timpLivrareTotal, aes(x=TimpTotal)) + 
  geom_histogram(aes(y=..density..), bins=30, fill="coral", color="black", alpha=0.8) +
  stat_function(
    fun = function(x) dgamma(x, shape = 5, rate = 0.5),
    color = "red",
    size = 1.2
  ) +
  labs(
    title="suma timpilor de livrare pentru 2 produse",
    subtitle="Histograma simulata vs distributia teoretica Gamma(α=5, β=0.5)",
    x="Timp total livrare",
    y="Densitate"
  )

#cerinta 4
#subprob 1 - definirea pragului sub care dorim să estimam probabilitatea
prag <- 10 

#calcularea mediei și a variantei cererii
media_cererii <- mean(dateStoc$Cerere)
varianța_cererii <- var(dateStoc$Cerere)

#aplicarea inegalității lui Cebisev
#P(X < prag) <= Var(X) / (Var(X) + (media - prag)^2)

#probabilitatea estimata
probabilitate_estimata <- varianța_cererii / (varianța_cererii + (media_cererii - prag)^2)
cat("Probabilitatea ca stocul sa scadă sub pragul de", prag, "este estimata la:", probabilitate_estimata, "\n")

#subprob 2
#definirea coeficientilor pentru costul de depozitare
c1 <- 5  # cost fix
c2 <- 0.2 # cost variabil (pe unitate)

#definirea unei funcții pentru costul de depozitare ca functie de stoc
cost_depozitare <- function(Q) {
  return(c1 * Q + c2 * Q^2)
}

#calcularea mediei stocului
media_stoc <- mean(dateStoc$Cerere)

#calcularea costului de depozitare la nivelul mediu al stocului
cost_mediu <- cost_depozitare(media_stoc)

#calcularea mediei costului de depozitare pe baza stocului fluctuant
costuri_stoc_fluctuant <- cost_depozitare(dateStoc$Cerere)
media_costurilor <- mean(costuri_stoc_fluctuant)

#aplicarea inegalitatii lui Jensen
cat("Inegalitatea lui Jensen ne arata ca:", cost_mediu, "<=", media_costurilor, "\n")

#subproblema 3, inegalitate Chernoff-Poisson pentru comanda minima necesara cu risc mai mic de 1% de a ramane fara stoc
# Calcularea mediei cererii (lambda) pe baza datelor existente
lambda <- mean(dateStoc$Cerere) # media cererii zilnice

# Definirea timpului de livrare in zile
L <- 5 

# Funcția inegalitatii Chernoff-Poisson
chernoff_poisson <- function(lambda, L, alpha = 0.01) {
  Lambda <- lambda * L
  f <- function(delta) Lambda * delta^2 / (2 + delta) - log(1/alpha)
  delta_star <- uniroot(f, c(0, 10))$root
  ceiling((1 + delta_star) * Lambda)
}

# Calcularea punctului de comanda minim pentru riscul de 1%
S <- chernoff_poisson(lambda = lambda, L = L, alpha = 0.01)
cat("Punct de comanda minim pentru α=1%:", S, "buc\n")