Teste - Introdução ao R
================
2023-05-19

# Medidas de posição - Carregando os dados:

``` r
dados <- trees
dados
```

    ##    Girth Height Volume
    ## 1    8.3     70   10.3
    ## 2    8.6     65   10.3
    ## 3    8.8     63   10.2
    ## 4   10.5     72   16.4
    ## 5   10.7     81   18.8
    ## 6   10.8     83   19.7
    ## 7   11.0     66   15.6
    ## 8   11.0     75   18.2
    ## 9   11.1     80   22.6
    ## 10  11.2     75   19.9
    ## 11  11.3     79   24.2
    ## 12  11.4     76   21.0
    ## 13  11.4     76   21.4
    ## 14  11.7     69   21.3
    ## 15  12.0     75   19.1
    ## 16  12.9     74   22.2
    ## 17  12.9     85   33.8
    ## 18  13.3     86   27.4
    ## 19  13.7     71   25.7
    ## 20  13.8     64   24.9
    ## 21  14.0     78   34.5
    ## 22  14.2     80   31.7
    ## 23  14.5     74   36.3
    ## 24  16.0     72   38.3
    ## 25  16.3     77   42.6
    ## 26  17.3     81   55.4
    ## 27  17.5     82   55.7
    ## 28  17.9     80   58.3
    ## 29  18.0     80   51.5
    ## 30  18.0     80   51.0
    ## 31  20.6     87   77.0

# Mínimo

``` r
apply(dados, MARGIN = 2, min)
```

    ##  Girth Height Volume 
    ##    8.3   63.0   10.2

# Máximo

``` r
apply(dados, MARGIN = 2, max)
```

    ##  Girth Height Volume 
    ##   20.6   87.0   77.0

# Média

``` r
apply(dados, MARGIN = 2, mean) 
```

    ##    Girth   Height   Volume 
    ## 13.24839 76.00000 30.17097

# Apenas para demonstrar a função que faz somatório.

``` r
apply(dados, MARGIN = 2, sum) / 31 
```

    ##    Girth   Height   Volume 
    ## 13.24839 76.00000 30.17097

# Mediana

``` r
apply(dados, MARGIN = 2, median)
```

    ##  Girth Height Volume 
    ##   12.9   76.0   24.2

# Moda // OBS: Não temos uma função para a moda no R, mas podemos criar

``` r
table(dados$Height) # A função table conta a quantidade de vezes que cada valor aparece no vetor. Pode ser utilizada # para fazer tabelas de contingência para dados qualitativos. Por mera observação do resultado já conseguimos # identificar a moda. O que não seria tão fácil para uma base de dados grande.
```

    ## 
    ## 63 64 65 66 69 70 71 72 74 75 76 77 78 79 80 81 82 83 85 86 87 
    ##  1  1  1  1  1  1  1  2  2  3  2  1  1  1  5  2  1  1  1  1  1

# Podemos criar uma função para esse fim:

``` r
moda <- function(x) { tab <- table(x) # Criamos um vetor para receber a frequência dos valores da variável. 
names(tab)[tab == max(tab)] # Aqui usamos um teste lógico para identificar o valor com a maior frequência. 
}
```

``` r
# Veja os comandos de maneira separada para entender: 
names(table(dados$Height)) # Retorna uma lista dos valores observados.
```

    ##  [1] "63" "64" "65" "66" "69" "70" "71" "72" "74" "75" "76" "77" "78" "79" "80"
    ## [16] "81" "82" "83" "85" "86" "87"

``` r
tab <- table(dados$Height); tab == max(tab) # Testa se cada valor é o que apresenta a maior frequência.
```

    ## 
    ##    63    64    65    66    69    70    71    72    74    75    76    77    78 
    ## FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE 
    ##    79    80    81    82    83    85    86    87 
    ## FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE

``` r
moda(dados$Height) # Testamos a função e ela funciona!
```

    ## [1] "80"

# Apenas para ilustrar, podemos ordernar os dados, o que nos permitiria encontrar a mediana e a moda:

``` r
sort(dados$Height) # O default é em ordem crescente.
```

    ##  [1] 63 64 65 66 69 70 71 72 72 74 74 75 75 75 76 76 77 78 79 80 80 80 80 80 81
    ## [26] 81 82 83 85 86 87

``` r
sort(dados$Height, decreasing = TRUE) # Existe um argumento na função que permite colocar em ordem decrescente.
```

    ##  [1] 87 86 85 83 82 81 81 80 80 80 80 80 79 78 77 76 76 75 75 75 74 74 72 72 71
    ## [26] 70 69 66 65 64 63

``` r
apply(dados, MARGIN = 2, sort) # Loop.
```

    ##       Girth Height Volume
    ##  [1,]   8.3     63   10.2
    ##  [2,]   8.6     64   10.3
    ##  [3,]   8.8     65   10.3
    ##  [4,]  10.5     66   15.6
    ##  [5,]  10.7     69   16.4
    ##  [6,]  10.8     70   18.2
    ##  [7,]  11.0     71   18.8
    ##  [8,]  11.0     72   19.1
    ##  [9,]  11.1     72   19.7
    ## [10,]  11.2     74   19.9
    ## [11,]  11.3     74   21.0
    ## [12,]  11.4     75   21.3
    ## [13,]  11.4     75   21.4
    ## [14,]  11.7     75   22.2
    ## [15,]  12.0     76   22.6
    ## [16,]  12.9     76   24.2
    ## [17,]  12.9     77   24.9
    ## [18,]  13.3     78   25.7
    ## [19,]  13.7     79   27.4
    ## [20,]  13.8     80   31.7
    ## [21,]  14.0     80   33.8
    ## [22,]  14.2     80   34.5
    ## [23,]  14.5     80   36.3
    ## [24,]  16.0     80   38.3
    ## [25,]  16.3     81   42.6
    ## [26,]  17.3     81   51.0
    ## [27,]  17.5     82   51.5
    ## [28,]  17.9     83   55.4
    ## [29,]  18.0     85   55.7
    ## [30,]  18.0     86   58.3
    ## [31,]  20.6     87   77.0
