# Emparejamiento para reducción de la confusión en un estudio con brazo control observacional


[Este repositorio](https://github.com/crivilla/emparejamiento_datos_obsevacionales.git) contiene el código correspondiente a la implementación de diferentes **métodos de emparejamiento** aplicados con el fin de **reducir la confusión** en un estudio que utiliza **datos observacionales** procedentes de historias clínicas electrónicas.

Contiene también el código utilizado en el preprocesamiento de los datos y una sección adicional que aborda la generación de datos sintéticos para la realización de pruebas de carga y funcionamiento de los algoritmos utilizados.


En este proyecto se ponen en práctica los conceptos teóricos de técnicas de emparejamiento en el contexto de estudios de investigación epidemiológica. 
Así, se desarrolla el proceso por el cual a partir de **dos grupos de sujetos** (un grupo control y un grupo intervención) y tras evaluar la situación de equilibrio con respecto a los valores de ciertas covariables de interés, se obtienen dos nuevos **grupos emparejados** y equilibrados en cuanto a sus covariables sobre los cuales podrían inferirse efectos causales y medir el efecto de la intervención ya que a través de este proceso de emparejamiento se habría logrado controlar los posibles efectos de las **variables de confusión**.


En el notebook principal se aborda en primer lugar la exploración inicial y el preprocesamiento de los conjuntos de datos. Este primera fase tiene como objetivo estudiar el contenido de los datasets y realizar una valoración de la calidad de los datos de cara a su utilización en los algoritmos de emparejamiento. A través del preprocesamiento se lleva a cabo además de la limpieza correspondiente de los datos y selección de variables de interés, la unificación de las variables recogidas en ambos conjuntos de datos.

<br>

## Librería ***MatchIt*** para algoritmos de emparejamiento

Para la implementación de los algoritmos de emparejamiento se hace uso del [paquete ***MatchIt***](https://CRAN.R-project.org/package=MatchIt). 

Este paquete aporta funciones que permiten seleccionar muestras emparejadas de los grupos de intervención y control originales con distribuciones de covariables similares.

Puede utilizarse para aplicar múltiples técnicas de **emparejamiento** o ***matching***, como el emparejamiento por covariables de forma exacta o el emparejamiento por puntajes de propensión (*Propensity Score Matching*) entre otras.

Existe mucha documentación disponible relativa a la utilización de este paquete. Parte de esta documentación puede encontrarse a través de los siguientes enlaces:


* Página principal en CRAN.R

    [*MatchIt: Nonparametric Preprocessing for Parametric Causal Inference*](https://cran.r-project.org/web/packages/MatchIt/index.html)

* Manual de referencia
  
    [Package 'MatchIt'](https://cran.r-project.org/web/packages/MatchIt/MatchIt.pdf)

* RDocumentation sobre la función matchit en concreto - MatchIt (version 3.0.2): 

     [matchit: MatchIt: Matching Software for Causal Inference](https://www.rdocumentation.org/packages/MatchIt/versions/3.0.2/topics/matchit) 
    

<br>

---

Los datos utilizados en el desarrollo del proyecto han sido recogidos (o extraídos en el caso de los datos observacionales) en el marco de un estudio principal sobre prevención de enfermedades cardiovasculares (ECV) y están pendientes de publicación.

---

<br>

## Referencias
Ho D, Imai K, King G, Stuart E (2011). “MatchIt: Nonparametric Preprocessing for Parametric Causal Inference.” *Journal of Statistical Software*, **42**(8), 1–28. [doi:10.18637/jss.v042.i08](https://www.jstatsoft.org/article/view/v042i08).
