# Survey2Network Shiny App  
**Transformador interactivo de encuestas en listas de nodos y aristas + taller guiado de Análisis de Redes Sociales**

> Convierte datos de encuestas de relaciones en **node lists** y **edge lists** listos para `igraph`, explora tu red con visualizaciones básicas y sigue un itinerario pedagógico paso a paso para dominar el Social Network Analysis (SNA).

---

## 📑 Tabla de contenidos
1. [Motivación](#motivación)  
2. [Características principales](#características-principales)  
3. [Demo online](#demo-online)  
4. [Requisitos](#requisitos)  
5. [Instalación local](#instalación-local)  
6. [Uso de la app](#uso-de-la-app)  
7. [Estructura del repositorio](#estructura-del-repositorio)  
8. [Contribuir](#contribuir)  
9. [Licencia](#licencia)  

---

## Motivación
Las **encuestas de redes sociales** son una herramienta poderosa para entender la dinámica de grupos (clases, equipos de trabajo, comunidades …).  
Sin embargo, el *formato en crudo* rara vez coincide con lo que las librerías de análisis —`igraph`, `statnet`, `tidygraph`— necesitan.  
Esta aplicación Shiny elimina esa fricción:

* **Carga** tu archivo CSV/Excel.  
* **Mapea** columnas con clics.  
* **Descarga** listas de nodos y aristas, un script R listo para correr y un *workshop interactivo* para profundizar.  

Ideal para **estudiantes, docentes y analistas** que quieren ir de la recolección de datos a los insights en minutos.

## Características principales
| Módulo | Descripción | Tecnologías |
|--------|-------------|-------------|
| **Importación flexible** | Soporta CSV y Excel; elige separador, encoding y si hay encabezado. | `readr`, `readxl` |
| **Mapeo de columnas** | Interfaz dinámica para asignar nombre, géneros, relaciones (almuerzo, trabajos, vida social…), clases, edad, semestre. | `shiny` |
| **Procesamiento automático** | Normaliza nombres, crea atributos, genera `node_list` y `edge_list` con pesos y espacios compartidos, calcula materias comunes. | `dplyr`, funciones propias |
| **Descargas 1‑click** | `node_list.csv`, `edge_list.csv`, *script_analisis_red.R* reproducible. | `downloadHandler` |
| **Visualización básica** | Grafo dirigido con colores por género/participación, tamaño por grado y leyenda automática. | `igraph` |
| **Taller guiado** | 6 pasos con instrucciones, código y tareas: exploración, centralidades, comunidades, homofilia, visualización avanzada y mini‑proyecto final. | `shiny::navlistPanel` |
| **Tema moderno** | Estilo *Cosmo* de **Bootswatch** + `shinythemes`. | `shinythemes` |

## Demo online
¿Quieres probar antes de clonar?  
👉 **[Demo en shinyapps.io](https://owxbz2-daniel-otero.shinyapps.io/DataTranformer/)** <br>
*(puedes cambiar la URL si despliegas tu propia instancia)*

## Requisitos
- **R ≥ 4.1**  
- Paquetes CRAN:  
  `shiny`, `shinythemes`, `tidyverse`, `readxl`, `DT`, `igraph`, `ggraph` (opcional).  

Instalación express:
```r
install.packages(c(
  "shiny", "shinythemes", "tidyverse", "readxl",
  "DT", "igraph", "ggraph"
))
```

## Instalación local
```bash
git clone https://github.com/tuusuario/survey2network.git
cd survey2network
R -e "shiny::runApp('app.R')"
```

## Uso de la app
1. **Carga tu archivo** (`.csv` o `.xlsx`).  
2. **Define opciones** de separador y encoding si es necesario.  
3. **Asigna columnas** en “Mapeo de columnas”.  
4. Haz clic en **Procesar datos**.  
5. Revisa las pestañas **Node List**, **Edge List** y **Visualización**.  
6. Descarga archivos o sigue el **Taller guiado** para análisis profundo en R.

## Estructura del repositorio
```
├── app.R                   # Código completo de la app Shiny
├── RED 2025-1.csv          # Ejemplo de datos de encuesta (anónimo)
├── README.md               # (este archivo)
└── LICENCE
```

## Contribuir
¡Pull requests y *issues* son bienvenidos!

1. Haz un fork, crea una rama (`feature/tu-mejora`).  
2. Asegúrate de que la app corre (`shiny::runApp()`) y que `lintr` pasa.  
3. Explica el cambio y su motivación en el PR.

## Licencia
Distribuido bajo **MIT License** — consulta `LICENCE`.

---

> **Survey2Network Shiny App** reduce la distancia entre los datos de tu encuesta y los *insights* que esconden tus redes. ¡Convierte, explora y aprende en un solo lugar!
