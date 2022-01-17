# Infrastucture_Taiwan_linear_regression
Regresszió elemzés:
  Melyik az a függvény amelyiknek a segítségével az egyik változó értékét ismerve egy másikra becslést tudunk adni.
  Összefüggésben lévő tendencit vizsgálja
 A lineáris regresszióanalízis az a statisztikai eljárás, amellyel megtalálhatjuk a két változóra együttesen felvett értékekhez (a pontdiagram pontjaira) legjobban illeszkedő egyenest
library(tidyverse)
setwd('C:/Users/Felhasználó/Desktop/sokváltozós statisztikai elemzés Bozsonyi/lineráis_regresszió')
dataset <- read.csv("real_estate.csv")
str(dataset)
'data.frame':   414 obs. of  8 variables:
 $ No                                    : int  1 2 3 4 5 6 7 8 9 10 ... #ID, egész számok
 $ X1.transaction.date                   : num  2013 2013 2014 2014 2013 ... #vásárlás dátuma, szám
 $ X2.house.age                          : num  32 19.5 13.3 13.3 5 7.1 34.5 20.3 31.7 17.9 ...#ház kora, szám 
 $ X3.distance.to.the.nearest.MRT.station: num  84.9 306.6 562 562 390.6 ...#távolsága a legközelebbi tömegközlekedéstől, szám (vasúthálózat)
 $ X4.number.of.convenience.stores       : int  10 9 5 5 5 3 7 6 1 3 ... #boltok száma a környéken, egész számok
 $ X5.latitude                           : num  25 25 25 25 25 ... #szélességi fok, szám 
 $ X6.longitude                          : num  122 122 122 122 122 ...#hosszúsági fok, szám
 $ Y.house.price.of.unit.area            : num  37.9 42.2 47.3 54.8 43.1 32.1 40.3 46.7 18.8 22.1 ...#egységnyi területre jutó lakás, szám 
#
#független: x1,x2,x3,x4,x5,x6 =infrastruktúra
#függő: Y.egységnyi.területre.jutó.lakás (Taiwan)
#független: hatást gyakorol a függőre 
#függő: befolyásolja a függő

################################################################## ADAT TISZTÍTÁS #########################################################################
#sorok átnevezése
names(dataset)[2]<-paste("X1.vásárlás.dátuma")
names(dataset)[3]<-paste("X2.ház.kora")
names(dataset)[4]<-paste("X3.távolság.tömegközlekedéstől")
names(dataset)[5]<-paste("X4.üzletek.száma")
names(dataset)[6]<-paste("X5.szélességi.fok")
names(dataset)[7]<-paste("X6.hosszúsági.fok")
names(dataset)[8]<-paste("Y.egységnyi.területre.jutó.lakás") #LAKÁSÁR

################################################################## ADATok BEMUTATÁSA #########################################################################
sample_n(dataset, 5) #random
 No X1.vásárlás.dátuma X2.ház.kora X3.távolság.tömegközlekedéstől X4.üzletek.száma X5.szélességi.fok X6.hosszúsági.fok Y.egységnyi.területre.jutó.lakás
1  90           2013.500        23.0                      3947.9450                0          24.94783          121.5024                             25.3
2 192           2013.167        13.2                       750.0704                2          24.97371          121.5495                             37.8
3 342           2013.000        13.0                       750.0704                2          24.97371          121.5495                             37.0
4 273           2012.750        13.0                       492.2313                5          24.96515          121.5374                             40.5
5  60           2013.083        13.3                       336.0532                5          24.95776          121.5344                             42.4


head(dataset, n=5) #első 5 sor
No X1.vásárlás.dátuma X2.ház.kora X3.távolság.tömegközlekedéstől X4.üzletek.száma X5.szélességi.fok X6.hosszúsági.fok Y.egységnyi.területre.jutó.lakás
1  1           2012.917        32.0                       84.87882               10          24.98298          121.5402                             37.9
2  2           2012.917        19.5                      306.59470                9          24.98034          121.5395                             42.2
3  3           2013.583        13.3                      561.98450                5          24.98746          121.5439                             47.3
4  4           2013.500        13.3                      561.98450                5          24.98746          121.5439                             54.8
5  5           2012.833         5.0                      390.56840                5          24.97937          121.5425                             43.1

################################################################## LEÍRÓ STATISZTIKA A VÁLTOZÓKRA #########################################################################
 summary(dataset)
        No        X1.vásárlás.dátuma  X2.ház.kora     X3.távolság.tömegközlekedéstől X4.üzletek.száma X5.szélességi.fok X6.hosszúsági.fok Y.egységnyi.területre.jutó.lakás
 Min.   :  1.0   Min.   :2013       Min.   : 0.000   Min.   :  23.38                Min.   : 0.000   Min.   :24.93     Min.   :121.5     Min.   :  7.60                  
 1st Qu.:104.2   1st Qu.:2013       1st Qu.: 9.025   1st Qu.: 289.32                1st Qu.: 1.000   1st Qu.:24.96     1st Qu.:121.5     1st Qu.: 27.70                  
 Median :207.5   Median :2013       Median :16.100   Median : 492.23                Median : 4.000   Median :24.97     Median :121.5     Median : 38.45                  
 Mean   :207.5   Mean   :2013       Mean   :17.713   Mean   :1083.89                Mean   : 4.094   Mean   :24.97     Mean   :121.5     Mean   : 37.98                  
 3rd Qu.:310.8   3rd Qu.:2013       3rd Qu.:28.150   3rd Qu.:1454.28                3rd Qu.: 6.000   3rd Qu.:24.98     3rd Qu.:121.5     3rd Qu.: 46.60                  
 Max.   :414.0   Max.   :2014       Max.   :43.800   Max.   :6488.02                Max.   :10.000   Max.   :25.01     Max.   :121.6     Max.   :117.50                  

#függő változó:Y.egységnyi.területre.jutó.lakás
#független változó/magyarázó változók: x1,x2,x3,x4,x5,x6 =infrastruktúra
#H0: nincs összefüggés a lakások árának növekedése és az infrastruktúra fejlődése között Taiwanban.
#H1: a lakások árának növekedése hatással van az infrastruktúra fejlődésére Taiwanban.


################################################################## ALKALMASSÁG VIZSGÁLATA #########################################################################
############### FÜGGŐ VÁLTOZÓ NORMÁLIS ELOSZLÁS VIZSGÁLAT ###############
png(file="1_függő_normalis_eloszlas.png", width=500, height=540)
hist(dataset$Y.egységnyi.területre.jutó.lakás)

png(file="2_függő_normalis_eloszlas_Q_Q_plot.png", width=500, height=540)
qqnorm(dataset$Y.egységnyi.területre.jutó.lakás)

############### KORRELÁCIÓ VIZSGÁLAT AZ ÖSSZES VÁLTOZÓ KÖZÖTT ###############
#lineáris kapcsolat erősségének és irányának vizsgálata
#1: tökéletes kapcsolat
#0: nincs kapcsolat, függetlenek
#-1: tökéletes, fordítottan arányos kapcsolat van.

#########dataset=subset(dataset,select=-c(X6.hosszúsági.fok)) #x6 változó kihagyása a modellből##########

dataset=subset(dataset,select=-c(No)) #ID kihagyása az adatbázisból

library(corrplot)
corrplot(cor(dataset))
png(file="3_lineáris_regresszio_korrelacio_vizsgalat_összes_változó_corrplot.png", width=500, height=540)
corlnMtrx <- cor(dataset[,-8])
corrplot.mixed(corlnMtrx,
               lower = "number", upper = "pie", 
               tl.col = "black",tl.pos = "lt")
			   
corlnMtrx <- cor(dataset[,-8])
corlnMtrx
                                   X1.vásárlás.dátuma X2.ház.kora X3.távolság.tömegközlekedéstől X4.üzletek.száma X5.szélességi.fok X6.hosszúsági.fok Y.egységnyi.területre.jutó.lakásár
X1.vásárlás.dátuma                        1.000000000  0.01754877                     0.06087995      0.009635445        0.03505776       -0.04108178                         0.08749061
X2.ház.kora                               0.017548767  1.00000000                     0.02562205      0.049592513        0.05441990       -0.04852005                        -0.21056705
X3.távolság.tömegközlekedéstől            0.060879953  0.02562205                     1.00000000     -0.602519145       -0.59106657       -0.80631677                        -0.67361286
X4.üzletek.száma                          0.009635445  0.04959251                    -0.60251914      1.000000000        0.44414331        0.44909901                         0.57100491
X5.szélességi.fok                         0.035057756  0.05441990                    -0.59106657      0.444143306        1.00000000        0.41292394                         0.54630665
X6.hosszúsági.fok                        -0.041081778 -0.04852005                    -0.80631677      0.449099007        0.41292394        1.00000000                         0.52328651
Y.egységnyi.területre.jutó.lakásár        0.087490606 -0.21056705                    -0.67361286      0.571004911        0.54630665        0.52328651                         1.00000000
 

################################################################## MODELL FUTTATÁSA #########################################################################
############### LINERÁIS REGRESSZIÓ AZ ÖSSZES VÁLTOZÓRA ###############
#1. modell létrehozása
Regression=lm(dataset$Y.egységnyi.területre.jutó.lakás~dataset$X1.vásárlás.dátuma+dataset$X2.ház.kora+dataset$X3.távolság.tömegközlekedéstől+dataset$X4.üzletek.száma+dataset$X5.szélességi.fok+dataset$X6.hosszúsági.fok)

#2. magyarázó erő
summary(Regression)[c('r.squared', 'adj.r.squared')]
$r.squared
[1] 0.5823704 #egységnyi területre jutó lakásár szórásából az infrastruktúa 58%-ot magyaráz

$adj.r.squared
[1] 0.5762137 #egységnyi területre jutó lakásár nagyságát 57%-ban magyarázza az infrastruktúra fejlettsége
#Modellilleszkedés: R négyzet, 0-1 között. minél nagyobb annál jobb. 
#függő változó szórásából a modell hány százalékot magyaráz

# 3. feltételek teljesülése
png(file="4_linear_regression__összes_változo_Q_Q_plot.png", width=500, height=540)
res<-resid(Regression)
qqnorm(res)
qqline(res) 
#a maradékok/két vége ha 45 fokos szöget zár be akkor normális eloszlást követ
#homoszkedasztikusak az eredmények és követik a 45 fokos szöget tehát a lineáris regresszió jó modell lesz

#4. Regresszió
summary(Regression)

Call:
lm(formula = dataset$Y.egységnyi.területre.jutó.lakás ~ dataset$X1.vásárlás.dátuma + 
    dataset$X2.ház.kora + dataset$X3.távolság.tömegközlekedéstől + 
    dataset$X4.üzletek.száma + dataset$X5.szélességi.fok + dataset$X6.hosszúsági.fok)

Residuals:
    Min      1Q  Median      3Q     Max 
-35.664  -5.410  -0.966   4.217  75.193 

Coefficients:
                                         Estimate Std. Error t value Pr(>|t|)    
(Intercept)                            -1.444e+04  6.776e+03  -2.131  0.03371 *  #szignifikáns
dataset$X1.vásárlás.dátuma              5.146e+00  1.557e+00   3.305  0.00103 ** #szignifikáns
dataset$X2.ház.kora                    -2.697e-01  3.853e-02  -7.000 1.06e-11 ***#szignifikáns
dataset$X3.távolság.tömegközlekedéstől -4.488e-03  7.180e-04  -6.250 1.04e-09 ***#szignifikáns
dataset$X4.üzletek.száma                1.133e+00  1.882e-01   6.023 3.84e-09 ***#szignifikáns
dataset$X5.szélességi.fok               2.255e+02  4.457e+01   5.059 6.38e-07 ***#szignifikáns
dataset$X6.hosszúsági.fok              -1.242e+01  4.858e+01  -0.256  0.79829    #nem szignifikáns, nagy ezért kikell hagyni
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#szignifikáns:  kicsi az esélye, hogy a véletlen szülte az eredményeket, tehát a lakások árának növekedésével (függő) nő az infrasturktúra (független)
#H0:elvetjük
#Std. Error: minatbeli hiba: nem nagy
#Pr(>|t|) : szignifikancia: ha 0,05 nél nagyobb nem szignifikáns, ezért dobom x6-ot
#X1,X2,X3,X4,X5 : szignifikáns kapcsolat 
#X6: nem szignifikáns a kapcsolat

Residual standard error: 8.858 on 407 degrees of freedom
Multiple R-squared:  0.5824,    Adjusted R-squared:  0.5762 
F-statistic: 94.59 on 6 and 407 DF,  p-value: < 2.2e-16


#Residual standard error: 8.858: regressziós egyenestől lévő átlagos távolság (minél kissebb annál jobb)
#F-statistic: 94.59: általános szignifikancia, megmondja, hogy a magyarázó változóink hasznosak-e
# p-value: < 2.2e-16: mivel kissebb mint 0,05 hasznosnak tekinthető modellünk, van lineáris kapcsolat

#5. vizualizáció
png(file="5_linear_regression_model.png", width=500, height=540)
plot(Regression, data = dataset, cex.lab = 1.5)
abline(Regression, col = "red", lwd = 2.5)
legend('bottomright', legend = 'lakásár~inrastruktúra', lty = 1, col = 'red', lwd = 2.5, title = 'Regression line')


H0: elvetése

################################################################## MODELL JAVÍTÁSA (X6 KIHAGYÁSA) #########################################################################
############### LINERÁIS REGRESSZIÓ x6 VÁLTOZÓ KIHAGYÁSA ###############
#1. modell létrehozása
Regression_x6=lm(dataset$Y.egységnyi.területre.jutó.lakás~dataset$X1.vásárlás.dátuma+dataset$X2.ház.kora+dataset$X3.távolság.tömegközlekedéstől+dataset$X4.üzletek.száma+dataset$X5.szélességi.fok)

#2. magyarázó erő
summary(Regression_x6)[c('r.squared', 'adj.r.squared')]
$r.squared
[1] 0.5823033 #Egységnyi területre jutó lakásár szórásából az infrastruktúra 58%-ot magyaráz -> kicsit rosszabb lett (0.5823704)

$adj.r.squared
[1] 0.5771845 #Egységnyi területre jutó lakásár növekedését 57%-ban magyarázza az infrastruktúra ->kicsit jobb lett (0.5762137)

#3. modellek összehasonlítása
anova(Regression)
Analysis of Variance Table

Response: dataset$Y.egységnyi.területre.jutó.lakás
                                        Df Sum Sq Mean Sq  F value    Pr(>F)    
dataset$X1.vásárlás.dátuma               1    585     585   7.4598  0.006584 ** 
dataset$X2.ház.kora                      1   3441    3441  43.8559  1.12e-10 ***
dataset$X3.távolság.tömegközlekedéstől   1  34857   34857 444.2734 < 2.2e-16 ***
dataset$X4.üzletek.száma                 1   3576    3576  45.5748  5.08e-11 ***
dataset$X5.szélességi.fok                1   2065    2065  26.3187  4.49e-07 ***
dataset$X6.hosszúsági.fok                1      5       5   0.0654  0.798293    
Residuals                              407  31933      78                       
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

############### MODEL1 - MODEL2 - ÖSSZEHASONLÍTÁSA ###############
 anova(Regression_x6)
Analysis of Variance Table

Response: dataset$Y.egységnyi.területre.jutó.lakás
                                        Df Sum Sq Mean Sq  F value    Pr(>F)    
dataset$X1.vásárlás.dátuma               1    585     585   7.4769  0.006522 ** 
dataset$X2.ház.kora                      1   3441    3441  43.9566 1.067e-10 ***
dataset$X3.távolság.tömegközlekedéstől   1  34857   34857 445.2934 < 2.2e-16 ***
dataset$X4.üzletek.száma                 1   3576    3576  45.6794 4.828e-11 ***
dataset$X5.szélességi.fok                1   2065    2065  26.3791 4.355e-07 ***
Residuals                              408  31938      78                       
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
anova(Regression, Regression_x6)
  Res.Df   RSS Df Sum of Sq      F Pr(>F)
1    407 31933                           
2    408 31938 -1   -5.1308 0.0654 0.7983  #kettes modell jobb lesz nekem

#4. feltételek ellenőrzése
png(file="6_linear_regression_x6_Q_Q_plot.png", width=500, height=540)
res_x6<-resid(Regression_x6)
qqnorm(res_x6)
qqline(res_x6)

#5. Regresszió
summary(Regression_x6)

#függő változó:Y.egységnyi.területre.jutó.lakás
#független vátlozó/magyarázó változók: x1,x2,x3,x4,x5 =infrastruktúra
#H0: nincs összefüggés a vizsgált változók között
#H1: a lakások árának növekedése hatással van az infrastuktúra fejlődésére

Call:
lm(formula = dataset$Y.egységnyi.területre.jutó.lakás ~ dataset$X1.vásárlás.dátuma + 
    dataset$X2.ház.kora + dataset$X3.távolság.tömegközlekedéstől + 
    dataset$X4.üzletek.száma + dataset$X5.szélességi.fok)

Residuals:
    Min      1Q  Median      3Q     Max 
-35.623  -5.371  -1.020   4.244  75.346 

Coefficients:
                                         Estimate Std. Error t value Pr(>|t|)    
(Intercept)                            -1.596e+04  3.233e+03  -4.936 1.17e-06 *** #szignifikáns
dataset$X1.vásárlás.dátuma              5.135e+00  1.555e+00   3.303  0.00104 ** #szignifikáns
dataset$X2.ház.kora                    -2.694e-01  3.847e-02  -7.003 1.04e-11 ***#szignifikáns
dataset$X3.távolság.tömegközlekedéstől -4.353e-03  4.899e-04  -8.887  < 2e-16 ***#szignifikáns
dataset$X4.üzletek.száma                1.136e+00  1.876e-01   6.056 3.17e-09 ***#szignifikáns
dataset$X5.szélességi.fok               2.269e+02  4.417e+01   5.136 4.36e-07 ***#szignifikáns
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 8.848 on 408 degrees of freedom
Multiple R-squared:  0.5823,    Adjusted R-squared:  0.5772 
F-statistic: 113.8 on 5 and 408 DF,  p-value: < 2.2e-16

#X1,X2,X3,X4,X5 : szignifikáns kapcsolat 
#szignifikáns:  kicsi az esélye, hogy a véletlen szülte az eredményeket, tehát a lakások árának növekedésével (függő) nő az infrastruktúra (független)
#H0:elvetjük
#Std. Error: minatbeli hiba: nem nagy

#Multiple R-squared:  0.5824: magyarázó erő. Egységnyi területre jutó lakások árának növekedését  58%-ban magyarázza az infrastruktúra növekedése.
#Residual standard error: 8.848: regressziós egyenestől lévő átlagos távolság (minél kissebb annál jobb) ->jobb lett (0,1)
#F-statistic: 113.8: általános szignifikancia, megmondja, hogy a magyarázó változóink hasznosak-e.
#Adjusted R-squared:  0.5772 magyarázó erő. :  Egységnyi területre jutó lakások árának növekedését 57%-ban magyarázza az infrastruktúra növekedése.
# p-value: < 2.2e-16: mivel kissebb mint 0,05 hasznosnak tekinthető modellünk

#5.vizualizáció
png(file="7_linear_regression_x6.png", width=500, height=540)
plot(Regression_x6, data = dataset, cex.lab = 1.5)
legend('bottomright', legend = 'lakásár~infrastruktúra', lty = 1, col = 'red', lwd = 2.5, title = 'Regression line')




################################################################## MODELL JAVÍTÁSA (X6 KIHAGYÁSA, LOG TRANSZFROMÁCIÓ) #########################################################################
#1. modell létrehozása
Regression_log=lm(log(dataset$Y.egységnyi.területre.jutó.lakás)~dataset$X1.vásárlás.dátuma+dataset$X2.ház.kora+dataset$X3.távolság.tömegközlekedéstől+dataset$X4.üzletek.száma+dataset$X5.szélességi.fok)

#2. magyarázó erő
summary(Regression_log)[c('r.squared', 'adj.r.squared')]
$r.squared
[1] 0.6856785 : Egységnyi területre jutó lakásár szórásából az infrastruktúra növekedése 68%-ot magyaráz -> jobb lett (0.5823033)


$adj.r.squared
[1] 0.6818265 : Egységnyi területre jutó lakásár növekedését 68%-ban magyarázza az infrastruktúra fejlettsége ->kicsit jobb lett (0.5771845)

#3. feltételek ellenőrzése
png(file="8_linear_regression_Q_Q_plot_log_model.png", width=500, height=540)
res2<-resid(Regression_log)
qqnorm(res2)
qqline(res2) 
#a maradékok/két vége ha 45 fokos szöget zár be akkor normális eloszlást követ
#homoszkedasztikusak az eredmények és követik a 45 fokos szöget tehát a lineáris regresszió jó modell lesz

#4. modellek összehasonlítása
anova(Regression_x6)
anova(Regression_log)
 anova(Regression_x6)
Analysis of Variance Table

Response: dataset$Y.egységnyi.területre.jutó.lakás
                                        Df Sum Sq Mean Sq  F value    Pr(>F)    
dataset$X1.vásárlás.dátuma               1    585     585   7.4769  0.006522 ** 
dataset$X2.ház.kora                      1   3441    3441  43.9566 1.067e-10 ***
dataset$X3.távolság.tömegközlekedéstől   1  34857   34857 445.2934 < 2.2e-16 ***
dataset$X4.üzletek.száma                 1   3576    3576  45.6794 4.828e-11 ***
dataset$X5.szélességi.fok                1   2065    2065  26.3791 4.355e-07 ***
Residuals                              408  31938      78                       
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
> anova(Regression_log)
Analysis of Variance Table

Response: log(dataset$Y.egységnyi.területre.jutó.lakás)
                                        Df Sum Sq Mean Sq  F value    Pr(>F)    
dataset$X1.vásárlás.dátuma               1  0.363   0.363   7.3976   0.00681 ** 
dataset$X2.ház.kora                      1  2.311   2.311  47.1596 2.450e-11 ***
dataset$X3.távolság.tömegközlekedéstől   1 36.155  36.155 737.7156 < 2.2e-16 ***
dataset$X4.üzletek.száma                 1  2.298   2.298  46.8982 2.761e-11 ***
dataset$X5.szélességi.fok                1  2.493   2.493  50.8630 4.541e-12 ***
Residuals                              408 19.996   0.049                       
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1



#5. Regresszió
summary(Regression_log)

Call:
lm(formula = log(dataset$Y.egységnyi.területre.jutó.lakás) ~ 
    dataset$X1.vásárlás.dátuma + dataset$X2.ház.kora + dataset$X3.távolság.tömegközlekedéstől + 
        dataset$X4.üzletek.száma + dataset$X5.szélességi.fok)

Residuals:
     Min       1Q   Median       3Q      Max 
-1.68218 -0.11505  0.00055  0.11262  1.04395 

Coefficients:
                                         Estimate Std. Error t value Pr(>|t|)    
(Intercept)                            -4.665e+02  8.091e+01  -5.766 1.61e-08 *** #szignifikáns
dataset$X1.vásárlás.dátuma              1.358e-01  3.890e-02   3.491 0.000533 *** #szignifikáns
dataset$X2.ház.kora                    -6.977e-03  9.625e-04  -7.248 2.13e-12 *** #szignifikáns
dataset$X3.távolság.tömegközlekedéstől -1.495e-04  1.226e-05 -12.194  < 2e-16 *** #szignifikáns
dataset$X4.üzletek.száma                2.766e-02  4.694e-03   5.892 7.97e-09 *** #szignifikáns
dataset$X5.szélességi.fok               7.883e+00  1.105e+00   7.132 4.54e-12 *** #szignifikáns
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.2214 on 408 degrees of freedom
Multiple R-squared:  0.6857,    Adjusted R-squared:  0.6818 
F-statistic:   178 on 5 and 408 DF,  p-value: < 2.2e-16

#függő változó:Y.egységnyi.területre.jutó.lakás
#független vátlozó/magyarázó változók: x1,x2,x3,x4,x5 =infrastruktúra
#H0: nincs összefüggés a vizsgált változók között
#H1: a lakások árának növekedése hatással van az infrastuktúra fejlődésére
#X1,X2,X3,X4,X5 : szignifikáns kapcsolat 

#szignifikáns:  kicsi az esélye, hogy a véletlen szülte az eredményeket, tehát a lakások árának növekedésével (függő) nő az infrasturktúra fejlettsége (független)
#H0:elvetjük
#Std. Error: minatbeli hiba: nem nagy

#Multiple R-squared:  0.6857: magyarázó erő. 
#Egységnyi területre jutó lakások árának növekedését 68%-ban magyarázza az infrastruktúra fejlettsége. (10%-javulás)
#Residual standard error: 0.2214: regressziós egyenestől lévő átlagos távolság.  Jelentősen jobb. (8.848)
#F-statistic: 178: általános szignifikancia, megmondja, hogy a magyarázó változóink hasznosak-e. Jobb lett.
#Adjusted R-squared:  0.6818 magyarázó erő. Egységnyi területre jutó lakások árának növekedése 68%-ban magyarázza az infrastruktúra fejlettsége. (11%-os javulás)
# p-value: < 2.2e-16: mivel kissebb mint 0,05 hasznosnak tekinthető modellünk

png(file="9_linear_regression_log_model.png", width=500, height=540)
plot(Regression_log, data = dataset, cex.lab = 1.5)
legend('bottomright', legend = 'lakásár~infrastruktúra', lty = 1, col = 'red', lwd = 2.5, title = 'Regression line')


################################################################## MODELL JAVÍTÁSA (X1,X2,X6 KIHAGYÁSA, LOG TRANSZFROMÁCIÓ) #########################################################################
#1.korreláció vizsgálat
dataset=subset(dataset,select=-c(X1.vásárlás.dátuma,X2.ház.kora))
png(file="10_lineáris_regresszio_korrelacio_vizsgalat_log_model_2.png", width=500, height=540)

corlnMtrx2 <- cor(dataset[,-4])
corrplot.mixed(corlnMtrx2,
               lower = "number", upper = "pie", 
               tl.col = "black",tl.pos = "lt")
			   
corlnMtrx2 <- cor(dataset[,-4])
corlnMtrx2
                               X3.távolság.tömegközlekedéstől X4.üzletek.száma X5.szélességi.fok 
X3.távolság.tömegközlekedéstől                      1.0000000       -0.6025191        -0.5910666       
X4.üzletek.száma                                   -0.6025191        1.0000000         0.4441433         
X5.szélességi.fok                                  -0.5910666        0.4441433         1.0000000        


#Korreláció vizsgálat:
#Kapcsolat erősségét és irányát vizsgálja
#Értékei:
#1: tökéletes kapcsolat
#0: nincs kapcsolat, függetlenek
#-1: tökéletes fordítottan arányos kapcsolat 
	van

#2. modell létrehozása
Regression_log_2=lm(log(dataset$Y.egységnyi.területre.jutó.lakás)~dataset$X3.távolság.tömegközlekedéstől+dataset$X4.üzletek.száma+dataset$X5.szélességi.fok

#3. modellek összehasonlítása
anova(Regression_log, Regression_log_2)
  Res.Df     RSS Df Sum of Sq F Pr(>F)
1    408 19.9959                      
2    410  2.0039 -2    17.992         #2. modell jobb nekem

#4. magyarázó erő: egységnyi területre jutó lakás szórásából a modell mennyit magyaráz
summary(Regression_log_2)[c('r.squared', 'adj.r.squared')]

$r.squared
[1] 0.6432135 #Egységnyi területre jutó lakásár szórásából az infrastruktúra növekedése 68%-ot magyaráz -> rosszabb lett (0.6856785 )

$adj.r.squared
[1] 0.6406029 #Egységnyi területre jutó lakásár növekedését 68%-ban magyarázza az infrastruktúra fejlettsége ->kicsit jobb lett (0.6818265)


#5. feltételek ellenőrzése
png(file="11_linear_regression_Q_Q_plot_log_model_x1_x2.png", width=500, height=540)
 res3<-resid(Regression_log_2)
qqnorm(res3)
qqline(res3) 
#a maradékok/két vége ha 45 fokos szöget zár be akkor normális eloszlást követ
#homoszkedasztikusak az eredmények és követik a 45 fokos szöget tehát a lineáris regresszió jó modell lesz

#5. Regresszió
summary(Regression_log_2)

Call:
lm(formula = log(dataset$Y.egységnyi.területre.jutó.lakás) ~ 
    dataset$X3.távolság.tömegközlekedéstől + dataset$X4.üzletek.száma + 
        dataset$X5.szélességi.fok)

Residuals:
     Min       1Q   Median       3Q      Max 
-0.58648 -0.02980  0.00172  0.03795  0.31107 

Coefficients:
                                         Estimate Std. Error t value Pr(>|t|)    
(Intercept)                            -5.696e+01  8.663e+00  -6.575 1.49e-10 ***
dataset$X3.távolság.tömegközlekedéstől -4.687e-05  3.829e-06 -12.239  < 2e-16 ***
dataset$X4.üzletek.száma                6.904e-03  1.477e-03   4.673 4.03e-06 ***
dataset$X5.szélességi.fok               2.333e+00  3.469e-01   6.724 5.95e-11 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.06991 on 410 degrees of freedom
Multiple R-squared:  0.6432,    Adjusted R-squared:  0.6406 
F-statistic: 246.4 on 3 and 410 DF,  p-value: < 2.2e-16

#függő változó:Y.egységnyi.területre.jutó.lakás 
#független vátlozó/magyarázó változók: x3,x4,x5 =infrastruktúra
#H0: nincs összefüggés a vizsgált változók között
#H1: a lakások árának növekedésével nő az infrastuktúra fejlettsége
#X3,X4,X5 : szignifikáns kapcsolat az összes változóval
#szignifikáns:  kicsi az esélye, hogy a véletlen szülte az eredményeket, tehát a lakások árának növekedésével (függő) nő az infrasturktúra fejlettsége (független)
#H0:elvetjük
#Std. Error: minatbeli hiba: nem nagy

#Multiple R-squared:  0.6432: magyarázó erő. Lakások sűrüségét mennyire magyarázza az infrastruktúra 
#Egységnyi területre jutó lakások árának növekedését 64%-ban magyarázza az infrastruktúra fejlettsége.

#Residual standard error: 0.06991: regressziós egyenestől lévő átlagos távolság.  Jelentősen javult.
#F-statistic: 246.4: általános szignifikancia, megmondja, hogy a magyarázó változóink hasznosak-e. 
#Adjusted R-squared:  0.6406 magyarázó erő. Egységnyi területre jutó lakások árának növekedése 64%-ban magyarázza az infrastruktúra növekedése. 
# p-value: < 2.2e-16: mivel kissebb mint 0,05 hasznosnak tekinthető modellünk

plot(Regression_log_2)
png(file="12_linear_regression_log_model_2.png", width=500, height=540)
plot(Regression_log_2, data = dataset, cex.lab = 1.5)
legend('bottomright', legend = 'lakásár~infrastruktúra', lty = 1, col = 'red', lwd = 2.5, title = 'Regression line')


