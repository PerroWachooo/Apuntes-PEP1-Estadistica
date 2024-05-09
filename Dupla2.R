

# Pregunta 1
El enólogo cree que pudo lograr niveles de acidez similares en las cepas Cabernet Sauvignion, Carmenére y Merlot Para confirmar esta sospecha se les pide realizar un análisis inferencial, con 95% de confianza explicando y justificando paso a paso el proceidimiento seguido



### Formulación de Hipótesis:
Ho: Los niveles de acidez en las cepas Cabernet Sauvignion, Carmenére y Merlot son iguales
Ha: Los niveles de acidez en las cepas Cabernet Sauvignion, Carmenére y Merlot son presentan diferencias



En esta pregunta se pide inferir acerca de las medias de una variable numérica (PH) medidas en grupos independientes formados por un factor con tres niveles (cepas). Luego se requiere usar un procedimiento ANOVA para muestras independeintes.


Ahora verifiquemos las condiciones para asegurar que podemos aplicar el procedimiento con validez.

- La variable dependiente corresponde a Ph, que sabemos tiene escala de razón, y por lo tanto una escala continua de intervalos iguales, por ser una medida física.

- Por otro lado, el enunciado indica que las observaciones son independientes entre sí, pues provienen de mostos diferentes para cada cepa.

Revisemos ahora la condición de normalidad por medio del la prueba de Shapiro-Wilk.

- Revisaremos la homogeneadad de las varianzas con el test de Levene


```{r}
# Importamos las librerías
library(dplyr)
library(ez)

# Lectura de archivo
# Se abre el CSV
nombre_archivo <- "EI-2024-1-PE1-Datos.csv"
carpeta <- "C:\\Users\\BastiánBritoGarrido\\Desktop\\PEP1"
ruta <- file.path(carpeta, nombre_archivo)
datos <- read.csv(ruta, sep = ",")
datos
```

```{r}
#Fijamos nuestro nivel de significancia
alfa <- 0.05

#Filtramos por los vinos que queremos
datos_filtrados <- datos %>%
  select(-Pinot, -Syrah)
```


```{r}
# Comprobamos la normalidad de los datos con el test de Shapiro
shapiro_cab <- shapiro.test(datos_filtrados$Cabernet)
shapiro_car <- shapiro.test(datos_filtrados$Carmenere)
shapiro_mer <- shapiro.test(datos_filtrados$Merlot)

shapiro_cab
shapiro_car
shapiro_mer 
```

Con el test de Shapiro-Wilk podemos concluir que todos los datos siguen una distribución normal, ya que el p.value de cada vino es mayor a nuestro nivel de significancia (0,05)



```{r}
#Dejamos como datos largos
datos_largos <- datos_filtrados %>% pivot_longer(c("Cabernet", "Carmenere", "Merlot"),
  names_to = "Cepas", values_to = "Ph")
```

```{r}
# Realizamos la prueba de Levene para comprobar la homogeneadad de las varianzas
levene_resultado <- leveneTest(Ph ~ Cepas, data = datos_largos)
print(levene_resultado)
```
Se comprueba la homogeneidad de las varianzas, ya que p > 0.05


Ya que todo se cumple, procedemos a usar ANOVA
```{r}
#Se usa la prueba de anova para muestras independiente de una sola via
  #tiempo : variable dependiente
  #pais : variable independiente

prueba <- aov(Ph ~ Cepas, data = datos_largos)

print(summary(prueba))
```
Ya que p < 0.05 hay fuerte evidencia para rechazar la hipotesis nula, insinuando que hay al menos una diferencia en la acidez entre las cepas.

Puesto que el procedimiento ómnibus encuentra diferencias estadísticamente significativas, es necesario realizar un procedimiento post-hoc. Puesto que no requerimos hacer contrastes adicionales, usaremos la prueba HSD de Tukey, más poderosa que los factores de corrección no paramétricos (como Bonferroni y Holm), ya que no se ha descartado que los datos siguen distribuciones normales y con igual varianza.

```{r}
#Se realiza prueba post-hoc a la prueba anova ya hecha
post_hoc <- TukeyHSD(prueba, "Cepas", ordered = TRUE, conf.level = 1 - alfa)
post_hoc
```
### Respuesta 
Como se puede notar, la diferencia que posee la cepa Cabernet con las cepas Carmenere y Merlot posee un valor mayor a 0.05 (nuestro nivel de significancia), siendo esta cepa la unica que genera la diferencia. Por ende, se concluye que la cepa que difiere de las medias es Cabernet, y como existe dicha diferencia, se puede concluir que existe suficiente información para rechazar la hipótesis nula y apoyar la alternativa con un 95% de confianza. En otras palabras, Si existe una diferencia entre las medias de acidez de las cepas, siendo Cabernet la cepa que difiere.



```{r}

#Filtramos por los vinos que queremos
datos_filtrados2 <- datos %>%
  select(-Pinot, -Syrah, -Merlot)
```
Pregunta 2
# Enunciado
En la prueba

# Hipotesis:
H0: Hay igualdad entre las frecuencias de acidez(pH) menor a 3 para
las cepas Cabernet Sauvignon y Carmenere
HA: No hay igualdad entre las frecuencias de acidez(pH) menor a 3 para
las cepas Cabernet Sauvignon y Carmenere


#Selección de la prueba a utilizar
Usamos chi-cuadrado para compara las frecuencias de las muestras


# Condiciones de la prueba
Los datos presentan normalidad por lo que se puede seguir y realizar la prueba.



# Llenar la tabla de frecuencias
Para cada cepa contar las acorrencias de menor y mayor que 3 pH

```{r}
library(kableExtra)

pH_menor_3 <- c(24, 16)
pH_mayor_3  <- c(12, 20)

tabla1_obs <- rbind(pH_menor_3, pH_mayor_3)
colnames(pH_mayor_3) <- c("Cabernet", "Carmenere")

Total1_obs <- pH_menor_3 + pH_mayor_3 
tabla1_obs_total <-rbind(tabla1_obs, Total1_obs) 
tabla1_obs_total %>%  
  kable(booktabs = TRUE, caption = "Frecuencias observadas") %>%
  kable_styling(full_width = FALSE) %>%  
  kable_styling(bootstrap_options = c("striped")) %>% 
  row_spec(3, bold = TRUE) %>% 
  add_header_above(c("Acidez", "cepa" = 2))

```



```{r}
alfa1 <- 0.05
prueba1 <- chisq.test(x = tabla1_obs)
cat("Resultado de la prueba chi-cuadrado de homogeneidad:\n")
print(prueba1)


```

Análisis y conclusión:

El p-valor es mayor a 0.05, no rechazamos la hipótesis nula,
indicando que las frecuencias son similares.

La tabla no se pudo ver pero el resultado de la prueba indica que son frecuencias similares
