ANALISIS FACTORIAL
================
Jasbeidy Marcela Ortiz Sanchez
1950133

# IMPORTAR LA BASE DE DATOS EN FORMA EXCEL

``` r
library(readxl)
datos<- read_excel("C:/Users/Jasbeidy Ortiz/Downloads/tabla excel (5).xlsx")
```

# TIPIFICACION O ESTANDARIZACION DE LAS VARIABLES

la tipificacion permite que todas las varfiables metricas gocen de una
misma unidad de medida estdistica.

``` r
datost<- datos #crear una nueva base de datos o data frame
datost<- scale(datost, center= T , scale= T)
datost<- as.data.frame(datost)
```

\#NORMALIDAD MULTIVARIANTE  
HO: normalidad multivariante H1: no normalidad multivariante confianza=
95% Alfa= 5% = 0,05 P value &gt; alfa: no se rechaza a la HO
(Normalidad) P Value &lt; alfa: se rechaza a la HO (No normalidad)

``` r
library(MVN)
```

    ## Registered S3 method overwritten by 'GGally':
    ##   method from   
    ##   +.gg   ggplot2

    ## sROC 0.1-2 loaded

``` r
mvn(datost[2:7])
```

    ## $multivariateNormality
    ##              Test         Statistic            p value Result
    ## 1 Mardia Skewness  46.0587091513111  0.825748646699118    YES
    ## 2 Mardia Kurtosis -1.66364390197536 0.0961836130276497    YES
    ## 3             MVN              <NA>               <NA>    YES
    ## 
    ## $univariateNormality
    ##           Test        Variable Statistic   p value Normality
    ## 1 Shapiro-Wilk   Minutos (%)      0.8879  0.0043      NO    
    ## 2 Shapiro-Wilk Minutos jugados    0.9529  0.2023      YES   
    ## 3 Shapiro-Wilk     vueltas        0.8772  0.0024      NO    
    ## 4 Shapiro-Wilk   Altura (cm)      0.8763  0.0023      NO    
    ## 5 Shapiro-Wilk      Horas         0.7238  <0.001      NO    
    ## 6 Shapiro-Wilk Masa grasa (%)     0.6366  <0.001      NO    
    ## 
    ## $Descriptives
    ##                  n          Mean Std.Dev      Median        Min      Max
    ## Minutos (%)     30  3.537246e-16       1  0.26741370 -2.2851716 1.118275
    ## Minutos jugados 30 -1.272112e-16       1  0.02952382 -1.7419056 1.800953
    ## vueltas         30  1.332304e-16       1 -0.03874320 -1.7821870 1.704701
    ## Altura (cm)     30  1.357698e-15       1 -0.26263997 -1.2176944 1.647469
    ## Horas           30 -1.896595e-16       1 -0.36514837 -0.9128709 1.278019
    ## Masa grasa (%)  30  2.072272e-16       1 -0.91969198 -0.9196920 1.051077
    ##                       25th      75th        Skew   Kurtosis
    ## Minutos (%)     -0.5530602 0.7232325 -0.84719219 -0.4280534
    ## Minutos jugados -0.8561909 0.8045242  0.14933801 -1.0437017
    ## vueltas         -0.6198911 0.5424047  0.05768966 -0.7743993
    ## Altura (cm)     -0.7401672 1.1699417  0.31050440 -1.5257576
    ## Horas           -0.9128709 1.2780193  0.31646192 -1.7633333
    ## Masa grasa (%)  -0.9196920 1.0510765  0.12700508 -2.0488690

# MATRIZ DE CORRELACIONES

HO: Correlacion entre las variables=0 (no hay correacion) H1:
Correlacion diferente de 0 (si hay correlacion)

cuando no se rechaza la HO, no se aplica AFE. Se rechace HO, si para
aplicar AFE.

``` r
library(psych)
corr.test(datost[,2:7])
```

    ## Call:corr.test(x = datost[, 2:7])
    ## Correlation matrix 
    ##                 Minutos (%) Minutos jugados vueltas Altura (cm) Horas
    ## Minutos (%)            1.00            0.35    0.23        0.01 -0.37
    ## Minutos jugados        0.35            1.00    0.21       -0.24 -0.44
    ## vueltas                0.23            0.21    1.00       -0.10  0.16
    ## Altura (cm)            0.01           -0.24   -0.10        1.00  0.28
    ## Horas                 -0.37           -0.44    0.16        0.28  1.00
    ## Masa grasa (%)         0.08            0.18   -0.04       -0.28 -0.27
    ##                 Masa grasa (%)
    ## Minutos (%)               0.08
    ## Minutos jugados           0.18
    ## vueltas                  -0.04
    ## Altura (cm)              -0.28
    ## Horas                    -0.27
    ## Masa grasa (%)            1.00
    ## Sample Size 
    ## [1] 30
    ## Probability values (Entries above the diagonal are adjusted for multiple tests.) 
    ##                 Minutos (%) Minutos jugados vueltas Altura (cm) Horas
    ## Minutos (%)            0.00            0.77    1.00        1.00  0.60
    ## Minutos jugados        0.06            0.00    1.00        1.00  0.22
    ## vueltas                0.21            0.26    0.00        1.00  1.00
    ## Altura (cm)            0.94            0.21    0.59        0.00  1.00
    ## Horas                  0.04            0.01    0.40        0.13  0.00
    ## Masa grasa (%)         0.67            0.35    0.85        0.13  0.14
    ##                 Masa grasa (%)
    ## Minutos (%)                  1
    ## Minutos jugados              1
    ## vueltas                      1
    ## Altura (cm)                  1
    ## Horas                        1
    ## Masa grasa (%)               0
    ## 
    ##  To see confidence intervals of the correlations, print with the short=FALSE option

``` r
correlaciones<- corr.test(datost[,2:7]) #se crea la matriz de correlaciones
correlaciones$r #matriz de correlaciones
```

    ##                 Minutos (%) Minutos jugados     vueltas Altura (cm)      Horas
    ## Minutos (%)      1.00000000       0.3482248  0.23481547  0.01361037 -0.3719107
    ## Minutos jugados  0.34822478       1.0000000  0.21417554 -0.23627023 -0.4405166
    ## vueltas          0.23481547       0.2141755  1.00000000 -0.10143613  0.1609833
    ## Altura (cm)      0.01361037      -0.2362702 -0.10143613  1.00000000  0.2841000
    ## Horas           -0.37191065      -0.4405166  0.16098327  0.28410005  1.0000000
    ## Masa grasa (%)   0.08095146       0.1785666 -0.03686049 -0.28232868 -0.2729604
    ##                 Masa grasa (%)
    ## Minutos (%)         0.08095146
    ## Minutos jugados     0.17856660
    ## vueltas            -0.03686049
    ## Altura (cm)        -0.28232868
    ## Horas              -0.27296042
    ## Masa grasa (%)      1.00000000

``` r
r <-as.matrix(correlaciones$r)
```

Alfa=0.05 P value &gt; alfa: no se rechaza HO estamos en esta situacion
,por lo tanto no se aplica Analisis Factorial Exploratorio P value &lt;
alfa: se rechaza HO

# INDICADORES DE APLICABILIDAD DEL AFE(BONDAD DEL AJUSTE)

## CONTRASTE DE ESFERICIDAD DE BARTLETT

HO: las correlaciones teoricas entre cada par de variables es nulo H1:
Las correlaciones teoricas entre cada par de variables no es nula

P value &gt; alfa: no s e aplica el AFE ( no se rechaza HO) P value &lt;
alfa: si se aplica el AFE (se rechaza HO)

``` r
dim(datost) #tamaño de la muestra=30 personas 
```

    ## [1] 30  7

``` r
cortest.bartlett(r, n=30)
```

    ## $chisq
    ## [1] 24.05747
    ## 
    ## $p.value
    ## [1] 0.06412394
    ## 
    ## $df
    ## [1] 15

como el P value es menor a alfa,se rechaza la HO,por lo tanto la
correlaciones teoricas entre cada par de variables es nulo,es decir,si
es aplicable el analisis factorial exploratorio (AFE).

## MEDIDA DE ADECUACION MUESTRAL DE KAISER, MEYER Y OKLIN( KMO)

Estudia variable por variables,si son o no aceptadas en el modelo para
hacer AFE. (que variables elimino o mantengo) Se mantiene una variable
en el modelo,si el KNO es igual o mayor a 0,7. se elimina una variable
del modelo,si el KNO es menor a 0,7.

``` r
KMO(r)
```

    ## Kaiser-Meyer-Olkin factor adequacy
    ## Call: KMO(r = r)
    ## Overall MSA =  0.54
    ## MSA for each item = 
    ##     Minutos (%) Minutos jugados         vueltas     Altura (cm)           Horas 
    ##            0.53            0.67            0.30            0.54            0.52 
    ##  Masa grasa (%) 
    ##            0.73

KMO= 0.54 El modelo es miserable KMO Minutos =0.53 (miserable) Minutos
jugados=0.67 (Unnaceptable) Vueltas=0.30 (Unnaceptable) Altura=0.54
(Miserable) Horas=0.52 (Miserable) Masa Grama=0.73 (regular)

# DETERMINACION DEL NUMERO DE FACTORES A EXTRAER

## metodos de las componentes principales iteradas (o Ejes principales)

Este metodo de las ejes principales es de naturaliza no parametrica,es
decir, que se ocupa,cuando no hay normalidad multivariante;pero,tambien
es valido para modelos parametricos (normalidad multivariante)

``` r
fa.parallel(r, fm="pa", n.obs=30, ylabel = "Eigenvalues") 
```

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate, : An
    ## ultra-Heywood case was detected. Examine the results carefully

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate, : An
    ## ultra-Heywood case was detected. Examine the results carefully

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate, : An
    ## ultra-Heywood case was detected. Examine the results carefully

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate, : An
    ## ultra-Heywood case was detected. Examine the results carefully

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate, : An
    ## ultra-Heywood case was detected. Examine the results carefully

![](EXAMENFINALDISEÑOSPROYECTO_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

    ## Parallel analysis suggests that the number of factors =  0  and the number of components =  1

con el metodo de los ejes principales es 1

## Metodo de las componentes principales

metodos parametrico,sirve solo para modelos con normalidad
multivariante.

``` r
fa.parallel(r, fm="pc", n.obs=30, ylabel = "Eigenvalues")
```

    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate, : An
    ## ultra-Heywood case was detected. Examine the results carefully

    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## An ultra-Heywood case was detected. Examine the results carefully

    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## An ultra-Heywood case was detected. Examine the results carefully

    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## An ultra-Heywood case was detected. Examine the results carefully

    ## factor method not specified correctly, minimum residual (unweighted least squares  used

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## An ultra-Heywood case was detected. Examine the results carefully

    ## factor method not specified correctly, minimum residual (unweighted least squares  used

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## An ultra-Heywood case was detected. Examine the results carefully

    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## An ultra-Heywood case was detected. Examine the results carefully

    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## An ultra-Heywood case was detected. Examine the results carefully

    ## factor method not specified correctly, minimum residual (unweighted least squares  used

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## An ultra-Heywood case was detected. Examine the results carefully

![](EXAMENFINALDISEÑOSPROYECTO_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

    ## Parallel analysis suggests that the number of factors =  0  and the number of components =  1

con el metodo de las componentes principales 1 factor

## Metodo de la maxima verosimilitud

con el metodo parametrico,sirve solo para modelos con normalidad
multivariante.

``` r
fa.parallel(r, fm="ml", n.obs=30, ylabel = "Eigenvalues")
```

![](EXAMENFINALDISEÑOSPROYECTO_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

    ## Parallel analysis suggests that the number of factors =  0  and the number of components =  1

con el metodo de la maxima verosimilitud 1 factor

## Metodo paralelo con interaciones

metodo parametrico,sirve solo para modelos con normalidad multivariante.

``` r
library(paran)
```

    ## Loading required package: MASS

``` r
paran(r, iterations= 100,graph= F)
```

    ## 
    ## Using eigendecomposition of correlation matrix.
    ## Computing: 10%  20%  30%  40%  50%  60%  70%  80%  90%  100%
    ## 
    ## 
    ## Results of Horn's Parallel Analysis for component retention
    ## 100 iterations, using the mean estimate
    ## 
    ## -------------------------------------------------- 
    ## Component   Adjusted    Unadjusted    Estimated 
    ##             Eigenvalue  Eigenvalue    Bias 
    ## -------------------------------------------------- 
    ## 1           1.494174    3.276493      1.782318
    ## -------------------------------------------------- 
    ## 
    ## Adjusted eigenvalues > 1 indicate dimensions to retain.
    ## (1 components retained)

con el metodo de Horn s se recomienda extraer 1 factor.

# METODOS DE EXTRACCION DE FACTORES

## METODO DE ANALISIS DE LOS COMPONENTES PRINCIPALES(ACP)

``` r
acp<- principal(r, nfactors=1, rotate= "none")
acp
```

    ## Principal Components Analysis
    ## Call: principal(r = r, nfactors = 1, rotate = "none")
    ## Standardized loadings (pattern matrix) based upon correlation matrix
    ##                   PC1   h2   u2 com
    ## Minutos (%)      0.59 0.35 0.65   1
    ## Minutos jugados  0.75 0.57 0.43   1
    ## vueltas          0.20 0.04 0.96   1
    ## Altura (cm)     -0.52 0.27 0.73   1
    ## Horas           -0.76 0.58 0.42   1
    ## Masa grasa (%)   0.50 0.25 0.75   1
    ## 
    ##                 PC1
    ## SS loadings    2.06
    ## Proportion Var 0.34
    ## 
    ## Mean item complexity =  1
    ## Test of the hypothesis that 1 component is sufficient.
    ## 
    ## The root mean square of the residuals (RMSR) is  0.16 
    ## 
    ## Fit based upon off diagonal values = 0.56

PC1: cargas factoriales de cada variable. h2: Comunalidad(varianza comun
explicado).

## 
