###
### IMPORTATION DE DONNÉES
###

## On utilise la fonction 'scan' pour importer des données
## sous forme de vecteur. Les fichiers 'deaths.dat' et
## 'strikes.dat' comptent chacun trois lignes de commentaires
## en début de fichier. On spécifie le caractère délimitant
## les commentaires avec l'argument 'comment.char'. De plus,
## on peut lire les fichiers directement depuis Internet.
deaths <- scan(
  "http://vgoulet.act.ulaval.ca/pub/donnees/deaths.dat",
  comment.char = "#")
strikes <- scan(
  "http://vgoulet.act.ulaval.ca/pub/donnees/strikes.dat",
  comment.char = "#")

###
### CRÉATION ET MANIPULATION DE SÉRIES
###

## Le fichier deaths.dat contient le nombre mensuel de morts
## accidentelles, 1973-1978. On transforme l'objet 'deaths'
## en une série chronologique aux propriétés correspondantes
## avec la fonction 'ts'.
( deaths <- ts(deaths, start = 1973, frequency = 12) )

## Le résultat est une série chronologique.
mode(deaths)               # un vecteur...
class(deaths)              # ... de classe "ts"

## Même chose avec l'objet 'strikes', qui contient le nombre
## de grèves aux États-Unis entre 1951-1980. L'argument
## 'frequency' n'est pas nécessaire: les séries sont
## annuelles par défaut.
( strikes <- ts(strikes, start = 1951) )

## La fonction 'window' est la façon élégante d'extraire des
## observations d'une série. Ici, on extrait les données
## 'deaths' du mois de février 1974 au mois d'octobre 1974,
## inclusivement.
window(deaths, start = c(1974, 2), end = c(1974, 10))

###
### IDENTIFICATION
###

## Graphiques des séries 'deaths' et 'strikes'.
plot(deaths)
plot(strikes)

## Corrélogramme de la série 'deaths'. Par défaut, 'acf'
## trace le corrélogramme.
acf(deaths)

## Pour obtenir les valeurs numériques de la fonction
## d'autocorrélation empirique, utiliser l'argument
## 'plot = FALSE'.
acf(deaths, plot = FALSE)

###
### MODÉLISATION
###

## On ajuste d'abord un modèle autorégressif pur aux données
## 'strikes' avec la fonction 'ar'.
( modele <- ar(strikes) )  # modèle AR(2) choisi

## On peut comparer les statistiques AIC des divers modèles.
## La statistique AIC du modèle AR(2) ne vaut pas vraiment 0;
## les statistiques sont simplement mise à l'échelle avec
## cette valeur comme référence.
modele$aic

## Ajustement d'un modèle ARIMA(1, 2, 1) aux données
## 'strikes'.
( fit.strikes <- arima(strikes, order = c(1, 2, 1)) )

## Ajustement d'un modèle SARIMA(0, 1, 1) x (0, 1, 1)_{12}
## aux données 'deaths'. Par défaut, la fréquence de la série
## (s = 12) est supposée identique à celle spécifiée dans
## l'objet. Il n'est donc pas nécessaire de préciser la
## valeur de s dans l'appel de 'arima', ici, puisque la série
## a été correctement définie dès le départ.
( fit.deaths <- arima(deaths, order = c(0, 1, 1),
                      seasonal = c(0, 1, 1)) )

## Cinq premières valeurs de la fonction d'autocorrélation
## théorique d'un processus ARMA(1, 1) avec phi = 0,6 et
## theta = -0,4.
ARMAacf(ar = 0.6, ma = -0.4, lag.max = 5)

## Cinq premiers coefficients de la représentation MA(infini)
## d'un processus AR(1) avec phi = 0,8.
ARMAtoMA(ar = 0.8, lag.max = 3)

###
### DIAGNOSTICS
###

## Vérification graphique de la qualité de l'ajustement du
## modèle ARIMA(1, 2, 1) aux données 'strikes' à l'aide de la
## fonction 'tsdiag'.
tsdiag(fit.strikes)

## Idem pour le modèle des données 'deaths'.
tsdiag(fit.deaths)

###
### PRÉVISIONS
###

## Prévision des six prochaines valeurs de la série 'deaths'
## à partir du modèle SARIMA.
( pred <- predict(fit.deaths, n.ahead = 6) )

## Graphique présentant la série originale, les prévisions
## des six prochaines années et les intervalles de prévision.
ts.plot(deaths,
        pred$pred,
        pred$pred + 1.96 * pred$se,
        pred$pred - 1.96 * pred$se,
        col = c(1, 2, 4, 4), lty = c(1, 3, 2, 2))

###
### SIMULATION
###

## Simulation de 10 observations d'un modèle ARMA(1, 1) avec
## phi = 0,8, theta = 0,5 et sigma^2 = 1.
arima.sim(10, model = list(ar = 0.8, ma = -0.5))

## Simulation de 10 observations d'un modèle ARIMA(2, 1, 1)
## avec phi_1 = 0,6, phi_2 = 0,3, theta = -0,2 et
## sigma^2 = 25.
arima.sim(10, model = list(ar = c(0.6, 0.3), ma = 0.2,
              order = c(2, 1, 1), sd = 5))
