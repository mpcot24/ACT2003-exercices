###
### IMPORTATION DE DONNÉES
###

## On importe les données du fichier anscombe.dat. On peut
## lire le fichier directement depuis Internet. De plus, les
## lignes débutant par # sont automatiquement reconnues comme
## des lignes de commentaires.
anscombe <- read.table(
  "http://vgoulet.act.ulaval.ca/pub/donnees/anscombe.dat")

## Ce jeu de données se trouve en fait déjà dans R et il est
## chargé en mémoire avec 'data'.
data(anscombe)

## Le résultat est un data frame, soit
mode(anscombe)             # ... une liste...
class(anscombe)            # ... de classe "data.frame"

## Extraction des étiquettes des colonnes et des lignes.
names(anscombe)            # étiquettes des colonnes
row.names(anscombe)        # étiquettes des lignes

###
### MODÉLISATION DES DONNÉES
###

## Relation graphique entre les variables y1 et x1 des données
## anscombe.
plot(y1 ~ x1, data = anscombe)

## On peut aussi rendre les colonnes du data frame visibles
## dans l'espace de travail et référer ensuite à celles-ci
## directement.
attach(anscombe)
plot(y1 ~ x1)

## Estimation des coefficients de la régression. Il est
## recommandé de sauvegarder les résultats dans un objet (de
## classe "lm") puisqu'il existe de multiples méthodes pour de
## tels objets.
( fit <- lm(y1 ~ x1, data = anscombe) )
class(fit)

###
### ANALYSE DES RÉSULTATS
###

## Le sommaire de la régression contient, outre le modèle
## utilisé, les résultats des tests t, la valeur des
## coefficients de détermination et de détermination ajusté,
## ainsi que le résultat du test F global.
summary(fit)

## Calcul du coefficient de détermination à la main.
attach(anscombe)
1 - sum(residuals(fit)^2)/sum((y1 - mean(y1))^2)
1 - deviance(fit)/sum((y1 - mean(y1))^2)
detach(anscombe)

## Intervalles de confiance pour les paramètres de la
## régression.
confint(fit)

## Le tableau d'analyse de variance (séquentiel, en régression
## multiple) est calculé avec la fonction générique 'anova'.
anova(fit)

## Pour ajouter la droite de régression au graphique créé
## précédemment, utiliser la fonction générique
## 'abline'. L'ordonnée à l'origine et la pente sont extraites
## de l'objet 'fit'.
abline(fit)

###
### MISE À JOUR DES RÉSULTATS ET PRÉVISION
###

## La fonction 'update' est utilisé pour modifier une ou
## plusieurs données dans le modèle ou pour enlever ou ajouter
## une ou plusieurs variables dans le modèle.
anscombe$x1[11] <- 6       # modification d'une donnée
update(fit)                # modèle mis à jour
update(fit, . ~ . + x4)    # ajout de la variable "x4"

## Retour au modèle d'origine
fit <- lm(y1 ~ x1, data = anscombe)

## Prévisions du modèle pour des valeurs de la variables "x1"
## de 3 et 15:
predict(fit, newdata = data.frame(x1 = c(3, 15)))

## Calcul des intervalles de confiance et de prévision pour
## les prévisions ci-dessus avec un niveau de confiance de
## 90%.
predict(fit, newdata = data.frame(x1 = c(3, 15)),
        interval = "confidence", level = 0.90)
predict(fit, newdata = data.frame(x1 = c(3, 15)),
        interval = "prediction", level = 0.90)

## Ajout des limites supérieures et inférieures des
## intervalles de confiance au graphique des données. On
## utilise la fonction 'matplot' qui prend en argument deux
## matrices 'x' et 'y' et produit un graphique des coordonnées
## de la première colonne de 'x' avec la première colonne de
## 'y', la seconde de 'x' avec la seconde de 'y', etc.
##
## Afin d'obtenir un beau graphique, il faut s'assurer de
## mettre les valeurs de 'x' en ordre croissant et de classer
## celles de 'y' en conséquence.
##
## En fait, on utilise la fonction 'matlines' qui ajoute à un
## graphique existant. La fonction 'matplot' créerait un
## nouveau graphique. (Note: il est possible de combiner les
## deux commandes matlines() ci-dessous en une seule.)
##
## Rendre les colonnes visibles.
attach(anscombe)

## Calcul des prévisions et des intervalles pour toutes les
## valeurs de "x1".
pred.ci <- predict(fit, interval = "confidence")
pred.pi <- predict(fit, interval = "prediction")
matlines(sort(x1), pred.ci[order(x1), -1],
         lty = 2, col = "red")
matlines(sort(x1), pred.pi[order(x1), -1],
         lty = 2, col = "green")

## Pour éviter que des lignes ne dépassent à extérieur du
## graphique, il faut trouver, avant de faire le graphique,
## les limites inférieure et supérieure des ordonnées. La
## fonction 'matplot' peut combiner des lignes et des points,
## ce qui permet de faire tout le graphique avec une seule
## commande.
y <- cbind(y1, pred.ci, pred.pi[, -1])
matplot(sort(x1), y[order(x1),],
     pch = 19, type = c("p", rep("l", 5)),
     lty = c(0, 1, rep(2, 4)),
     col = c("black", "blue", "red", "red", "green", "green"))
