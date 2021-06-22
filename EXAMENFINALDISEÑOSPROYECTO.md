ANALISIS FACTORIAL
================
Jasbeidy Marcela Ortiz Sanchez
1950133

# IMPORTAR LA BASE DE DATOS EN FORMA EXCEL

``` r
library(readxl)
datos<- read_excel("C:/Users/Jasbeidy Ortiz/Downloads/tabla excel (3).xlsx")
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
    ##              Test         Statistic           p value Result
    ## 1 Mardia Skewness  43.7553756645497 0.882933727539232    YES
    ## 2 Mardia Kurtosis -1.53971234877027 0.123630485401952    YES
    ## 3             MVN              <NA>              <NA>    YES
    ## 
    ## $univariateNormality
    ##           Test        Variable Statistic   p value Normality
    ## 1 Shapiro-Wilk Minutos jugados    0.9530  0.2027      YES   
    ## 2 Shapiro-Wilk   Minutos (%)      0.8879  0.0043      NO    
    ## 3 Shapiro-Wilk      Edad          0.9647  0.4056      YES   
    ## 4 Shapiro-Wilk   Altura (cm)      0.8763  0.0023      NO    
    ## 5 Shapiro-Wilk    Peso (Kg)       0.9197  0.0264      NO    
    ## 6 Shapiro-Wilk Masa grasa (%)     0.5774  <0.001      NO    
    ## 
    ## $Descriptives
    ##                  n          Mean Std.Dev      Median        Min      Max
    ## Minutos jugados 30  2.671474e-16       1  0.06243548 -1.8722977 1.844564
    ## Minutos (%)     30  3.537246e-16       1  0.26741370 -2.2851716 1.118275
    ## Edad            30  3.682339e-16       1  0.08725821 -1.7825606 1.957077
    ## Altura (cm)     30  1.357698e-15       1 -0.26263997 -1.2176944 1.647469
    ## Peso (Kg)       30  7.012547e-16       1  0.04956527 -1.4993495 1.722393
    ## Masa grasa (%)  30 -1.554312e-15       1 -0.64365030 -0.6436503 1.501851
    ##                       25th      75th        Skew   Kurtosis
    ## Minutos jugados -0.7058135 0.3284482  0.22284717 -0.8818769
    ## Minutos (%)     -0.5530602 0.7232325 -0.84719219 -0.4280534
    ## Edad            -0.6606693 0.4612220  0.08461497 -0.9304853
    ## Altura (cm)     -0.7401672 1.1699417  0.31050440 -1.5257576
    ## Peso (Kg)       -0.9417402 0.8240227  0.13967529 -1.4587371
    ## Masa grasa (%)  -0.6436503 1.5018507  0.82959373 -1.3535979

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
    ##                 Minutos jugados Minutos (%)  Edad Altura (cm) Peso (Kg)
    ## Minutos jugados            1.00       -0.04  0.07        0.17      0.06
    ## Minutos (%)               -0.04        1.00  0.50        0.01      0.24
    ## Edad                       0.07        0.50  1.00       -0.03     -0.01
    ## Altura (cm)                0.17        0.01 -0.03        1.00      0.15
    ## Peso (Kg)                  0.06        0.24 -0.01        0.15      1.00
    ## Masa grasa (%)            -0.27       -0.05  0.03       -0.17     -0.33
    ##                 Masa grasa (%)
    ## Minutos jugados          -0.27
    ## Minutos (%)              -0.05
    ## Edad                      0.03
    ## Altura (cm)              -0.17
    ## Peso (Kg)                -0.33
    ## Masa grasa (%)            1.00
    ## Sample Size 
    ## [1] 30
    ## Probability values (Entries above the diagonal are adjusted for multiple tests.) 
    ##                 Minutos jugados Minutos (%) Edad Altura (cm) Peso (Kg)
    ## Minutos jugados            0.00        1.00 1.00        1.00      1.00
    ## Minutos (%)                0.85        0.00 0.08        1.00      1.00
    ## Edad                       0.73        0.01 0.00        1.00      1.00
    ## Altura (cm)                0.37        0.94 0.87        0.00      1.00
    ## Peso (Kg)                  0.73        0.21 0.95        0.44      0.00
    ## Masa grasa (%)             0.15        0.81 0.87        0.36      0.08
    ##                 Masa grasa (%)
    ## Minutos jugados              1
    ## Minutos (%)                  1
    ## Edad                         1
    ## Altura (cm)                  1
    ## Peso (Kg)                    1
    ## Masa grasa (%)               0
    ## 
    ##  To see confidence intervals of the correlations, print with the short=FALSE option

``` r
correlaciones<- corr.test(datost[,2:7]) #se crea la matriz de correlaciones
correlaciones$r #matriz de correlaciones
```

    ##                 Minutos jugados Minutos (%)        Edad Altura (cm)   Peso (Kg)
    ## Minutos jugados      1.00000000 -0.03522535  0.06514049  0.16803608  0.06492098
    ## Minutos (%)         -0.03522535  1.00000000  0.49625331  0.01361037  0.23610757
    ## Edad                 0.06514049  0.49625331  1.00000000 -0.03171297 -0.01166465
    ## Altura (cm)          0.16803608  0.01361037 -0.03171297  1.00000000  0.14762373
    ## Peso (Kg)            0.06492098  0.23610757 -0.01166465  0.14762373  1.00000000
    ## Masa grasa (%)      -0.26844175 -0.04676221  0.03043357 -0.17487755 -0.32911122
    ##                 Masa grasa (%)
    ## Minutos jugados    -0.26844175
    ## Minutos (%)        -0.04676221
    ## Edad                0.03043357
    ## Altura (cm)        -0.17487755
    ## Peso (Kg)          -0.32911122
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
    ## [1] 16.47855
    ## 
    ## $p.value
    ## [1] 0.3509787
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
    ## Overall MSA =  0.5
    ## MSA for each item = 
    ## Minutos jugados     Minutos (%)            Edad     Altura (cm)       Peso (Kg) 
    ##            0.52            0.47            0.45            0.69            0.50 
    ##  Masa grasa (%) 
    ##            0.57

KMO= 0.5 El modelo es miserable KMO Minutos jugados=0.52 (miserable)
Minutos=0.47 (Unnaceptable) Edad=0.45 (Unnaceptable) Altura=0.69
(Middling) Peso=0.50 (Miserable) Masa Grama=0.57 (Miserable)

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

![](EXAMENFINALDISEÑOSPROYECTO_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

    ## Parallel analysis suggests that the number of factors =  0  and the number of components =  0

con el metodo de los ejes principales no se extraeria ningun factor

## Metodo de las componentes principales

metodos parametrico,sirve solo para modelos con normalidad
multivariante.

``` r
fa.parallel(r, fm="pc", n.obs=30, ylabel = "Eigenvalues")
```

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

    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used
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
    ## factor method not specified correctly, minimum residual (unweighted least squares  used

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

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

    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used

![](EXAMENFINALDISEÑOSPROYECTO_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

    ## Parallel analysis suggests that the number of factors =  0  and the number of components =  0

con el metodo de las componentes principales no se extraeria ningun
factor

## Metodo de la maxima verosimilitud

con el metodo parametrico,sirve solo para modelos con normalidad
multivariante.

``` r
fa.parallel(r, fm="ml", n.obs=30, ylabel = "Eigenvalues")
```

![](EXAMENFINALDISEÑOSPROYECTO_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

    ## Parallel analysis suggests that the number of factors =  0  and the number of components =  0

con el metodo de la maxima verosimilitud no se recomienda ningun factor

## Metodo paralelo con interaciones

metodo parametrico,sirve solo para modelos con normalidad multivariante.
