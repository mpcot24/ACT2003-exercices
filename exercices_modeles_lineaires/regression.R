###
### IMPORTATION DE DONN�ES
###

## On importe les donn�es du fichier anscombe.dat. On peut
## lire le fichier directement depuis Internet. De plus, les
## lignes d�butant par # sont automatiquement reconnues comme
## des lignes de commentaires.
anscombe <- read.table(
  "http://vgoulet.act.ulaval.ca/pub/donnees/anscombe.dat")

## Ce jeu de donn�es se trouve en fait d�j� dans R et il est
## charg� en m�moire avec 'data'.
data(anscombe)

## Le r�sultat est un data frame, soit
mode(anscombe)             # ... une liste...
class(anscombe)            # ... de classe "data.frame"

## Extraction des �tiquettes des colonnes et des lignes.
names(anscombe)            # �tiquettes des colonnes
row.names(anscombe)        # �tiquettes des lignes

###
### MOD�LISATION DES DONN�ES
###

## Relation graphique entre les variables y1 et x1 des donn�es
## anscombe.
plot(y1 ~ x1, data = anscombe)

## On peut aussi rendre les colonnes du data frame visibles
## dans l'espace de travail et r�f�rer ensuite � celles-ci
## directement.
attach(anscombe)
plot(y1 ~ x1)

## Estimation des coefficients de la r�gression. Il est
## recommand� de sauvegarder les r�sultats dans un objet (de
## classe "lm") puisqu'il existe de multiples m�thodes pour de
## tels objets.
( fit <- lm(y1 ~ x1, data = anscombe) )
class(fit)

###
### ANALYSE DES R�SULTATS
###

## Le sommaire de la r�gression contient, outre le mod�le
## utilis�, les r�sultats des tests t, la valeur des
## coefficients de d�termination et de d�termination ajust�,
## ainsi que le r�sultat du test F global.
summary(fit)

## Calcul du coefficient de d�termination � la main.
attach(anscombe)
1 - sum(residuals(fit)^2)/sum((y1 - mean(y1))^2)
1 - deviance(fit)/sum((y1 - mean(y1))^2)
detach(anscombe)

## Intervalles de confiance pour les param�tres de la
## r�gression.
confint(fit)

## Le tableau d'analyse de variance (s�quentiel, en r�gression
## multiple) est calcul� avec la fonction g�n�rique 'anova'.
anova(fit)

## Pour ajouter la droite de r�gression au graphique cr��
## pr�c�demment, utiliser la fonction g�n�rique
## 'abline'. L'ordonn�e � l'origine et la pente sont extraites
## de l'objet 'fit'.
abline(fit)

###
### MISE � JOUR DES R�SULTATS ET PR�VISION
###

## La fonction 'update' est utilis� pour modifier une ou
## plusieurs donn�es dans le mod�le ou pour enlever ou ajouter
## une ou plusieurs variables dans le mod�le.
anscombe$x1[11] <- 6       # modification d'une donn�e
update(fit)                # mod�le mis � jour
update(fit, . ~ . + x4)    # ajout de la variable "x4"

## Retour au mod�le d'origine
fit <- lm(y1 ~ x1, data = anscombe)

## Pr�visions du mod�le pour des valeurs de la variables "x1"
## de 3 et 15:
predict(fit, newdata = data.frame(x1 = c(3, 15)))

## Calcul des intervalles de confiance et de pr�vision pour
## les pr�visions ci-dessus avec un niveau de confiance de
## 90%.
predict(fit, newdata = data.frame(x1 = c(3, 15)),
        interval = "confidence", level = 0.90)
predict(fit, newdata = data.frame(x1 = c(3, 15)),
        interval = "prediction", level = 0.90)

## Ajout des limites sup�rieures et inf�rieures des
## intervalles de confiance au graphique des donn�es. On
## utilise la fonction 'matplot' qui prend en argument deux
## matrices 'x' et 'y' et produit un graphique des coordonn�es
## de la premi�re colonne de 'x' avec la premi�re colonne de
## 'y', la seconde de 'x' avec la seconde de 'y', etc.
##
## Afin d'obtenir un beau graphique, il faut s'assurer de
## mettre les valeurs de 'x' en ordre croissant et de classer
## celles de 'y' en cons�quence.
##
## En fait, on utilise la fonction 'matlines' qui ajoute � un
## graphique existant. La fonction 'matplot' cr�erait un
## nouveau graphique. (Note: il est possible de combiner les
## deux commandes matlines() ci-dessous en une seule.)
##
## Rendre les colonnes visibles.
attach(anscombe)

## Calcul des pr�visions et des intervalles pour toutes les
## valeurs de "x1".
pred.ci <- predict(fit, interval = "confidence")
pred.pi <- predict(fit, interval = "prediction")
matlines(sort(x1), pred.ci[order(x1), -1],
         lty = 2, col = "red")
matlines(sort(x1), pred.pi[order(x1), -1],
         lty = 2, col = "green")

## Pour �viter que des lignes ne d�passent � ext�rieur du
## graphique, il faut trouver, avant de faire le graphique,
## les limites inf�rieure et sup�rieure des ordonn�es. La
## fonction 'matplot' peut combiner des lignes et des points,
## ce qui permet de faire tout le graphique avec une seule
## commande.
y <- cbind(y1, pred.ci, pred.pi[, -1])
matplot(sort(x1), y[order(x1),],
     pch = 19, type = c("p", rep("l", 5)),
     lty = c(0, 1, rep(2, 4)),
     col = c("black", "blue", "red", "red", "green", "green"))
