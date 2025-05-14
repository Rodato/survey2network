# app.R
# Aplicación Shiny para transformar datos de encuesta de redes sociales
# en edge_list y node_list para análisis de redes

library(shiny)
library(tidyverse)
library(readxl)
library(DT)
library(shinythemes)

# UI de la aplicación
ui <- fluidPage(
  theme = shinytheme("cosmo"),
  titlePanel("Transformador de Datos para Análisis de Redes Sociales"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      # Cargar archivo
      fileInput("upload", "Cargar archivo CSV o Excel",
                multiple = FALSE,
                accept = c("text/csv", 
                           "application/vnd.ms-excel", 
                           "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")),
      
      # Opciones para CSV
      conditionalPanel(
        condition = "output.fileUploaded == true",
        checkboxInput("header", "¿El archivo tiene encabezados?", TRUE),
        selectInput("encoding", "Codificación:", 
                    choices = c("UTF-8", "Latin-1", "Windows-1252"),
                    selected = "UTF-8"),
        selectInput("separator", "Separador:", 
                    choices = c(Coma = ",", PuntoYComa = ";", Tab = "\t", Espacio = " "),
                    selected = ",")
      ),
      
      # Mapeo de columnas
      conditionalPanel(
        condition = "output.fileProcessed == true",
        h4("Mapeo de columnas"),
        selectInput("colNombres", "Columna de nombres:", choices = NULL),
        selectInput("colAlmuerzo", "Columna de relaciones de almuerzo:", choices = NULL),
        selectInput("colTrabajos", "Columna de relaciones de trabajos:", choices = NULL),
        selectInput("colSocial", "Columna de relaciones sociales:", choices = NULL),
        selectInput("colClases", "Columna de clases:", choices = NULL),
        selectInput("colGenero", "Columna de género:", choices = NULL),
        selectInput("colEdad", "Columna de edad:", choices = NULL),
        selectInput("colSemestre", "Columna de semestre:", choices = NULL),
        actionButton("procesarBtn", "Procesar datos", class = "btn-primary")
      ),
      
      # Botones de descarga
      conditionalPanel(
        condition = "output.dataProcessed == true",
        br(),
        h4("Descargar resultados"),
        downloadButton("downloadNodes", "Descargar Node List"),
        br(), br(),
        downloadButton("downloadEdges", "Descargar Edge List"),
        br(), br(),
        downloadButton("downloadScript", "Descargar Script de R")
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Datos originales", 
                 br(),
                 tableOutput("contenido")),
        tabPanel("Node List", 
                 br(),
                 DTOutput("nodeTable")),
        tabPanel("Edge List", 
                 br(),
                 DTOutput("edgeTable")),
        tabPanel("Visualización",
                 br(),
                 plotOutput("redPlot", height = "600px")),
        tabPanel("Taller Guiado",
                 br(),
                 navlistPanel(
                   id = "tallerNav",
                   widths = c(3, 9),
                   
                   # Sección 1: Introducción
                   tabPanel("Introducción",
                            h2("Análisis de Redes Sociales - Taller Guiado"),
                            p("Bienvenido a este taller interactivo de análisis de redes sociales. A través de una serie de ejercicios prácticos, aprenderás a analizar la estructura social de tu clase utilizando herramientas y conceptos de la ciencia de redes."),
                            p("Este taller te guiará paso a paso desde la exploración básica de los datos hasta análisis más sofisticados de la estructura de la red."),
                            br(),
                            h4("Objetivos del taller:"),
                            tags$ul(
                              tags$li("Comprender los conceptos básicos del análisis de redes"),
                              tags$li("Identificar patrones estructurales en tu red social"),
                              tags$li("Calcular e interpretar métricas importantes de la red"),
                              tags$li("Visualizar y comunicar hallazgos sobre la estructura social"),
                              tags$li("Relacionar los conceptos teóricos con datos reales")
                            ),
                            br(),
                            p("Comienza subiendo tus datos en la sección izquierda, y luego sigue cada paso del taller. Puedes descargar los archivos procesados cuando los necesites para completar los análisis más avanzados en R."),
                            br(),
                            div(class = "alert alert-info",
                                icon("info-circle"), 
                                strong("Consejo:"), 
                                " Puedes alternar entre la pestaña de visualización y el taller para ver los resultados de cada paso."
                            )
                   ),
                   
                   # Sección 2: Exploración básica
                   tabPanel("Paso 1: Exploración",
                            h3("Explorando los datos de la red"),
                            p("Empecemos por comprender qué datos tenemos y su significado en términos de red social."),
                            br(),
                            
                            h4("Actividad 1.1: Análisis de nodos y conexiones"),
                            p("Examina las tablas de node_list y edge_list y responde:"),
                            tags$ol(
                              tags$li("¿Cuántos nodos (personas) hay en total en la red?"),
                              tags$li("¿Cuántas personas completaron la encuesta vs. cuántas fueron solo mencionadas?"),
                              tags$li("¿Cuál es la distribución por género entre quienes completaron la encuesta?"),
                              tags$li("¿Cuántas conexiones (enlaces) existen en total?")
                            ),
                            br(),
                            tags$div(
                              tags$strong("Para resolver esto:"),
                              tags$ul(
                                tags$li("Revisa la pestaña 'Node List' y cuenta el número total de filas"),
                                tags$li("Filtra por la columna 'en_encuesta' para contar cuántos son TRUE vs FALSE"),
                                tags$li("Revisa la pestaña 'Edge List' para contar el número total de conexiones")
                              )
                            ),
                            br(),
                            
                            h4("Actividad 1.2: Tipos de relaciones"),
                            p("Analiza la columna 'shared_spaces' en edge_list para entender los diferentes tipos de relaciones:"),
                            tags$ol(
                              tags$li("¿Cuántas relaciones son exclusivamente de almuerzo?"),
                              tags$li("¿Cuántas son exclusivamente de trabajos grupales?"),
                              tags$li("¿Cuántas son exclusivamente de vida social?"),
                              tags$li("¿Cuántas combinan dos o tres espacios?")
                            ),
                            br(),
                            div(class = "well well-sm",
                                h5("Código R para análisis de relaciones:"),
                                pre(
                                  code(
                                    '# Cargar los datos (después de descargarlos de la app)
edge_list <- read.csv("edge_list.csv")

# Analizar tipos de relaciones
table(edge_list$shared_spaces)

# Ver distribución de pesos (número de espacios compartidos)
table(edge_list$weight)'
                                  )
                                )
                            )
                   ),
                   
                   # Sección 3: Métricas básicas
                   tabPanel("Paso 2: Métricas",
                            h3("Calculando métricas de centralidad"),
                            p("Ahora vamos a calcular métricas básicas para identificar nodos importantes en la red."),
                            br(),
                            
                            h4("Actividad 2.1: Grados de centralidad"),
                            p("El grado de un nodo indica cuántas conexiones tiene. En una red dirigida, distinguimos entre:"),
                            tags$ul(
                              tags$li(strong("Grado de entrada (indegree):"), " Número de conexiones que llegan al nodo. En nuestra red, representa cuántas personas mencionaron a un estudiante."),
                              tags$li(strong("Grado de salida (outdegree):"), " Número de conexiones que salen del nodo. Representa a cuántas personas mencionó un estudiante.")
                            ),
                            br(),
                            div(class = "well well-sm",
                                h5("Código R para calcular grados:"),
                                pre(
                                  code(
                                    '# Cargar librerías
library(igraph)

# Crear la red
node_list <- read.csv("node_list.csv")
edge_list <- read.csv("edge_list.csv")

red <- graph_from_data_frame(
  d = edge_list,
  vertices = node_list,
  directed = TRUE
)

# Calcular grados
node_list$indegree <- degree(red, mode = "in")
node_list$outdegree <- degree(red, mode = "out")
node_list$total_degree <- degree(red, mode = "total")

# Ver los 5 nodos con mayor grado de entrada
head(node_list[order(node_list$indegree, decreasing = TRUE), c("id", "indegree")], 5)

# Ver los 5 nodos con mayor grado de salida
head(node_list[order(node_list$outdegree, decreasing = TRUE), c("id", "outdegree")], 5)'
                                  )
                                )
                            ),
                   br(),
                   h4("Actividad 2.2: Interpretación"),
                   p("Una vez calculadas estas métricas, responde:"),
                   tags$ol(
                     tags$li("¿Quiénes son las personas más mencionadas (mayor indegree)?"),
                     tags$li("¿Quiénes mencionan a más personas (mayor outdegree)?"),
                     tags$li("¿Existe alguna relación entre el semestre y la centralidad?"),
                     tags$li("¿Qué significado tiene ser 'central' en esta red en términos sociales?")
                   )
                   ),
                 
                 # Sección 4: Métricas avanzadas
                 tabPanel("Paso 3: Métricas avanzadas",
                          h3("Intermediación y otras métricas"),
                          p("Más allá de los grados, existen otras métricas que revelan diferentes aspectos de la centralidad."),
                          br(),
                          
                          h4("Actividad 3.1: Intermediación (Betweenness)"),
                          p("La intermediación mide cuántas veces un nodo actúa como puente en los caminos más cortos entre otros pares de nodos. Nodos con alta intermediación pueden controlar el flujo de información."),
                          br(),
                          
                          h4("Actividad 3.2: Cercanía (Closeness)"),
                          p("La cercanía mide qué tan cerca está un nodo de todos los demás. Nodos con alta cercanía pueden diseminar información eficientemente."),
                          br(),
                          
                          div(class = "well well-sm",
                              h5("Código R para métricas avanzadas:"),
                              pre(
                                code(
                                  '# Calcular intermediación
node_list$betweenness <- betweenness(red, normalized = TRUE)

# Calcular cercanía
node_list$closeness <- closeness(red, normalized = TRUE)

# Calcular centralidad de autovector
node_list$eigen <- eigen_centrality(red)$vector

# Ver los 5 nodos con mayor intermediación
head(node_list[order(node_list$betweenness, decreasing = TRUE), c("id", "betweenness")], 5)

# Ver los 5 nodos con mayor cercanía
head(node_list[order(node_list$closeness, decreasing = TRUE), c("id", "closeness")], 5)'
                                )
                              )
                          ),
                          br(),
                          h4("Actividad 3.3: Comparación de métricas"),
                          p("Compara las diferentes métricas de centralidad y responde:"),
                          tags$ol(
                            tags$li("¿Las mismas personas son centrales según todas las métricas?"),
                            tags$li("¿Qué métrica crees que representa mejor la 'influencia' en el contexto de tu clase?"),
                            tags$li("¿Hay personas que son puentes entre grupos (alta intermediación) pero no tan populares (bajo grado)?")
                          )
                 ),
                 
                 # Sección 5: Visualización avanzada
                 tabPanel("Paso 4: Visualización",
                          h3("Visualización avanzada de la red"),
                          p("Una buena visualización puede revelar patrones estructurales importantes en la red."),
                          br(),
                          
                          h4("Actividad 4.1: Visualización por atributos"),
                          p("Crea visualizaciones que representen diferentes atributos de los nodos:"),
                          div(class = "well well-sm",
                              h5("Código R para visualización avanzada:"),
                              pre(
                                code(
                                  '# Colorear por género
V(red)$color <- ifelse(V(red)$en_encuesta,
                      ifelse(V(red)$genero == "MASCULINO", "skyblue",
                            ifelse(V(red)$genero == "FEMENINO", "pink", "lightgreen")),
                      "gray80")

# Tamaño por grado
V(red)$size <- 5 + degree(red, mode = "total")/2

# Grosor por peso de la relación
E(red)$width <- E(red)$weight/2

# Crear visualización
plot(red,
     layout = layout_with_fr(red),
     vertex.label.cex = 0.7,
     edge.curved = 0.2,
     main = "Red Social del Curso")'
                                )
                              )
                          ),
                          br(),
                          
                          h4("Actividad 4.2: Visualización con ggraph"),
                          p("Para visualizaciones más avanzadas, puedes usar ggraph:"),
                          div(class = "well well-sm",
                              h5("Código R para ggraph:"),
                              pre(
                                code(
                                  '# Instalar si es necesario
# install.packages("ggraph")
library(ggraph)
library(tidygraph)

# Convertir a formato tidygraph
red_tidy <- as_tbl_graph(red)

# Crear visualización
ggraph(red_tidy, layout = "fr") +
  geom_edge_link(aes(width = weight, alpha = weight), 
                 arrow = arrow(length = unit(2, "mm")), 
                 end_cap = circle(3, "mm")) +
  geom_node_point(aes(size = total_degree, 
                      color = genero,
                      shape = en_encuesta)) +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  scale_color_manual(values = c("MASCULINO" = "skyblue", 
                               "FEMENINO" = "pink", 
                               "OTRO" = "lightgreen",
                               "Desconocido" = "gray")) +
  scale_shape_manual(values = c("TRUE" = 19, "FALSE" = 15)) +
  scale_size(range = c(2, 8)) +
  scale_edge_width(range = c(0.5, 2)) +
  scale_edge_alpha(range = c(0.3, 1)) +
  theme_graph() +
  labs(title = "Red Social del Curso",
       subtitle = "Tamaño = Grado total, Color = Género, Forma = En encuesta")'
                                )
                              )
                          )
                 ),
                 
                 # Sección 6: Comunidades
                 tabPanel("Paso 5: Comunidades",
                          h3("Detección de comunidades"),
                          p("Las comunidades son grupos de nodos densamente conectados entre sí y con pocas conexiones hacia afuera. Su detección puede revelar subgrupos cohesivos."),
                          br(),
                          
                          h4("Actividad 5.1: Algoritmos de detección"),
                          p("Hay varios algoritmos para detectar comunidades. Probemos algunos:"),
                          div(class = "well well-sm",
                              h5("Código R para detección de comunidades:"),
                              pre(
                                code(
                                  '# Primero, crear una versión no dirigida de la red
red_undir <- as.undirected(red, mode = "collapse", 
                          edge.attr.comb = list(weight = "sum", "ignore"))

# Algoritmo de Louvain
comm_louvain <- cluster_louvain(red_undir)
node_list$community_louvain <- membership(comm_louvain)[match(node_list$id, V(red_undir)$name)]

# Algoritmo Fast Greedy
comm_fg <- cluster_fast_greedy(red_undir)
node_list$community_fg <- membership(comm_fg)[match(node_list$id, V(red_undir)$name)]

# Ver el número de comunidades detectadas
cat("Comunidades (Louvain):", length(unique(membership(comm_louvain))), "\\n")
cat("Comunidades (Fast Greedy):", length(unique(membership(comm_fg))), "\\n")

# Visualizar comunidades
plot(comm_louvain, red_undir,
     vertex.label.cex = 0.7,
     vertex.size = 5,
     main = "Comunidades (Algoritmo de Louvain)")

# Modularidad (qué tan bien definidas están las comunidades)
cat("Modularidad (Louvain):", modularity(comm_louvain), "\\n")
cat("Modularidad (Fast Greedy):", modularity(comm_fg), "\\n")'
                                )
                              )
                          ),
                          br(),
                          
                          h4("Actividad 5.2: Análisis de comunidades"),
                          p("Una vez detectadas las comunidades, responde:"),
                          tags$ol(
                            tags$li("¿Cuántas comunidades se identificaron con cada algoritmo?"),
                            tags$li("¿Tienen sentido estas comunidades en el contexto de tu clase?"),
                            tags$li("¿Hay alguna relación entre las comunidades y atributos como género o semestre?"),
                            tags$li("¿Hay personas que actúan como 'puentes' entre comunidades?")
                          )
                 ),
                 
                 # Sección 7: Análisis avanzado
                 tabPanel("Paso 6: Análisis avanzado",
                          h3("Análisis avanzados"),
                          p("Podemos profundizar nuestro análisis relacionando la estructura de la red con variables académicas y demográficas."),
                          br(),
                          
                          h4("Actividad 6.1: Homofilia"),
                          p("La homofilia es la tendencia a conectarse con personas similares. Analicemos si existe en nuestra red:"),
                          div(class = "well well-sm",
                              h5("Código R para análisis de homofilia:"),
                              pre(
                                code(
                                  '# Análisis de homofilia por género
# Crear un dataframe de edges con atributos de los nodos
edges_attr <- edge_list
edges_attr$from_gender <- node_list$genero[match(edges_attr$from, node_list$id)]
edges_attr$to_gender <- node_list$genero[match(edges_attr$to, node_list$id)]

# Calcular porcentaje de conexiones entre mismo género
same_gender <- sum(edges_attr$from_gender == edges_attr$to_gender, na.rm = TRUE)
total_edges <- nrow(edges_attr)
pct_same_gender <- (same_gender / total_edges) * 100

cat("Porcentaje de conexiones entre mismo género:", round(pct_same_gender, 2), "%\\n")

# Crear matriz de adyacencia por género
gender_matrix <- table(edges_attr$from_gender, edges_attr$to_gender)
gender_matrix'
                                )
                              )
                          ),
                          br(),
                          
                          h4("Actividad 6.2: Clases compartidas"),
                          p("Analiza si compartir clases está relacionado con la fuerza de las conexiones:"),
                          div(class = "well well-sm",
                              h5("Código R para análisis de clases:"),
                              pre(
                                code(
                                  '# Relación entre materias compartidas y peso de la relación
library(ggplot2)

ggplot(edge_list, aes(x = materias_comunes, y = weight)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Relación entre materias compartidas y fuerza de conexión",
       x = "Materias en común",
       y = "Espacios compartidos (peso)")

# Calcular correlación
cor(edge_list$materias_comunes, edge_list$weight, use = "complete.obs")'
                                )
                              )
                          )
                 ),
                 
                 # Sección 8: Proyecto final
                 tabPanel("Proyecto final",
                          h3("Proyecto de investigación"),
                          p("Como proyecto final, formula y responde tu propia pregunta de investigación sobre la red social de tu clase."),
                          br(),
                          
                          h4("Instrucciones:"),
                          tags$ol(
                            tags$li("Formula una pregunta de investigación clara y específica"),
                            tags$li("Identifica las métricas y técnicas necesarias para responderla"),
                            tags$li("Realiza el análisis y crea visualizaciones apropiadas"),
                            tags$li("Interpreta los resultados en el contexto de tu clase"),
                            tags$li("Presenta tus hallazgos de manera clara y convincente")
                          ),
                          br(),
                          
                          h4("Ejemplos de preguntas:"),
                          tags$ul(
                            tags$li("¿Existe segregación por género en la red social del curso?"),
                            tags$li("¿Cómo influye el semestre de ingreso en la posición estructural de los estudiantes?"),
                            tags$li("¿Las personas con mayor centralidad tienden a compartir más materias con otros?"),
                            tags$li("¿Cómo se comparan las redes de almuerzo, trabajos y vida social? ¿Son diferentes?"),
                            tags$li("¿Qué pasaría con la conectividad de la red si se removieran los nodos más centrales?")
                          ),
                          br(),
                          
                          div(class = "alert alert-success",
                              tags$b("Formato de entrega:"), 
                              p("Un informe de 3-5 páginas que incluya:"),
                              tags$ul(
                                tags$li("Pregunta de investigación y justificación"),
                                tags$li("Metodología (métricas y técnicas utilizadas)"),
                                tags$li("Visualizaciones principales"),
                                tags$li("Resultados y análisis"),
                                tags$li("Conclusiones e implicaciones"),
                                tags$li("Código R en un apéndice")
                              )
                          )
                 ),
                 
                 # Sección 9: Recursos
                 tabPanel("Recursos adicionales",
                          h3("Recursos adicionales"),
                          p("Para profundizar tus conocimientos sobre análisis de redes sociales:"),
                          br(),
                          
                          h4("Libros:"),
                          tags$ul(
                            tags$li(HTML("<b>Networks: An Introduction</b> por M.E.J. Newman")),
                            tags$li(HTML("<b>Social Network Analysis: Methods and Applications</b> por Wasserman & Faust")),
                            tags$li(HTML("<b>Analyzing Social Networks</b> por Borgatti, Everett & Johnson"))
                          ),
                          br(),
                          
                          h4("Recursos en línea:"),
                          tags$ul(
                            tags$li(tags$a(href="https://igraph.org/r/", "Documentación de igraph para R")),
                            tags$li(tags$a(href="https://kateto.net/network-visualization", "Visualización de redes con R por Katya Ognyanova")),
                            tags$li(tags$a(href="https://www.datacamp.com/courses/network-analysis-in-r", "Curso de DataCamp: Network Analysis in R"))
                          ),
                          br(),
                          
                          h4("Tutoriales:"),
                          tags$ul(
                            tags$li(tags$a(href="https://sna.stanford.edu/rlabs.php", "Stanford SNA R Labs")),
                            tags$li(tags$a(href="http://www.stat.cmu.edu/~brian/780/lectures/", "Statistical Analysis of Network Data (CMU)")),
                            tags$li(tags$a(href="https://github.com/briatte/awesome-network-analysis", "Awesome Network Analysis (GitHub)"))
                          )
                 )
                 )
        ),
      tabPanel("Ayuda",
               br(),
               h4("Instrucciones de uso:"),
               p("1. Carga un archivo CSV o Excel con los datos de la encuesta."),
               p("2. Verifica que las columnas se detecten correctamente."),
               p("3. Asigna cada columna de tu archivo a la categoría correspondiente."),
               p("4. Haz clic en 'Procesar datos' para generar las tablas."),
               p("5. Descarga los archivos resultantes para usarlos en tu análisis."),
               br(),
               h4("Formato esperado:"),
               p("El archivo debe contener columnas para:"),
               tags$ul(
                 tags$li("Nombres de los estudiantes"),
                 tags$li("Relaciones de almuerzo (con quién almuerzan)"),
                 tags$li("Relaciones de trabajos grupales"),
                 tags$li("Relaciones de vida social"),
                 tags$li("Clases que toman"),
                 tags$li("Información demográfica (género, edad, semestre)")
               ),
               p("Las relaciones deben estar separadas por comas, por ejemplo: 'Juan Pérez, María García'")
      )
      )
    )
  )
)

# Servidor de la aplicación
server <- function(input, output, session) {
  # Variables reactivas
  data_raw <- reactiveVal(NULL)
  node_list <- reactiveVal(NULL)
  edge_list <- reactiveVal(NULL)
  
  # Indicadores de estado
  output$fileUploaded <- reactive({ !is.null(data_raw()) })
  outputOptions(output, "fileUploaded", suspendWhenHidden = FALSE)
  
  output$fileProcessed <- reactive({ !is.null(data_raw()) })
  outputOptions(output, "fileProcessed", suspendWhenHidden = FALSE)
  
  output$dataProcessed <- reactive({ !is.null(node_list()) && !is.null(edge_list()) })
  outputOptions(output, "dataProcessed", suspendWhenHidden = FALSE)
  
  # Cargar el archivo
  observeEvent(input$upload, {
    req(input$upload)
    file <- input$upload
    
    # Determinar el tipo de archivo y cargarlo
    if (grepl("\\.csv$", file$name, ignore.case = TRUE)) {
      # Cargar CSV
      df <- read.csv(file$datapath, 
                     header = input$header,
                     sep = input$separator,
                     encoding = input$encoding,
                     stringsAsFactors = FALSE)
    } else {
      # Cargar Excel
      df <- read_excel(file$datapath)
    }
    
    data_raw(df)
    
    # Actualizar las opciones de selección de columnas
    updateSelectInput(session, "colNombres", choices = colnames(df))
    updateSelectInput(session, "colAlmuerzo", choices = colnames(df))
    updateSelectInput(session, "colTrabajos", choices = colnames(df))
    updateSelectInput(session, "colSocial", choices = colnames(df))
    updateSelectInput(session, "colClases", choices = colnames(df))
    updateSelectInput(session, "colGenero", choices = colnames(df))
    updateSelectInput(session, "colEdad", choices = colnames(df))
    updateSelectInput(session, "colSemestre", choices = colnames(df))
  })
  
  # Mostrar los datos cargados
  output$contenido <- renderTable({
    req(data_raw())
    head(data_raw(), 10)
  })
  
  # Procesar datos cuando se presiona el botón
  observeEvent(input$procesarBtn, {
    req(data_raw())
    
    # Obtener datos
    datos <- data_raw()
    
    # Renombrar columnas según el mapeo seleccionado
    datos_mapeados <- datos
    colnames(datos_mapeados)[colnames(datos_mapeados) == input$colNombres] <- "nombre"
    colnames(datos_mapeados)[colnames(datos_mapeados) == input$colAlmuerzo] <- "almuerzo"
    colnames(datos_mapeados)[colnames(datos_mapeados) == input$colTrabajos] <- "trabajos_grupales"
    colnames(datos_mapeados)[colnames(datos_mapeados) == input$colSocial] <- "vida_social"
    colnames(datos_mapeados)[colnames(datos_mapeados) == input$colClases] <- "clases"
    colnames(datos_mapeados)[colnames(datos_mapeados) == input$colGenero] <- "genero"
    colnames(datos_mapeados)[colnames(datos_mapeados) == input$colEdad] <- "edad"
    colnames(datos_mapeados)[colnames(datos_mapeados) == input$colSemestre] <- "semestre"
    
    # Preprocesamiento
    datos_procesados <- datos_mapeados
    
    # Convertir todo a mayúsculas para normalizar
    datos_procesados$nombre <- toupper(datos_procesados$nombre)
    datos_procesados$almuerzo <- toupper(datos_procesados$almuerzo)
    datos_procesados$trabajos_grupales <- toupper(datos_procesados$trabajos_grupales)
    datos_procesados$vida_social <- toupper(datos_procesados$vida_social)
    datos_procesados$clases <- toupper(datos_procesados$clases)
    
    # Reemplazar "NA" como texto por NA real
    datos_procesados$almuerzo[datos_procesados$almuerzo == "NA"] <- NA
    datos_procesados$trabajos_grupales[datos_procesados$trabajos_grupales == "NA"] <- NA
    datos_procesados$vida_social[datos_procesados$vida_social == "NA"] <- NA
    
    # Calcular el número de materias por estudiante
    datos_procesados$clases_lista <- strsplit(datos_procesados$clases, ",\\s*|;\\s*")
    datos_procesados$num_materias <- sapply(datos_procesados$clases_lista, length)
    
    # Extraer todos los nombres mencionados
    extract_all_names <- function(data, column_name) {
      names_list <- c()
      for(i in 1:nrow(data)) {
        if(!is.na(data[[column_name]][i])) {
          names <- strsplit(data[[column_name]][i], ",\\s*|;\\s*")[[1]]
          names_list <- c(names_list, trimws(names))
        }
      }
      return(unique(names_list))
    }
    
    almuerzo_names <- extract_all_names(datos_procesados, "almuerzo")
    trabajo_names <- extract_all_names(datos_procesados, "trabajos_grupales")
    social_names <- extract_all_names(datos_procesados, "vida_social")
    
    # Combinar todos los nombres únicos
    all_mentioned_names <- unique(c(almuerzo_names, trabajo_names, social_names))
    all_names <- unique(c(datos_procesados$nombre, all_mentioned_names))
    
    # Crear node_list
    node_list_df <- data.frame(
      id = all_names,
      en_encuesta = FALSE,  # Inicializar todos como no encuestados
      genero = "Desconocido",  # Inicializar género como desconocido para todos
      stringsAsFactors = FALSE
    )
    
    # Añadir atributos SOLO para los estudiantes que completaron la encuesta
    for(i in 1:nrow(datos_procesados)) {
      idx <- which(node_list_df$id == datos_procesados$nombre[i])
      if(length(idx) > 0) {
        node_list_df$semestre[idx] <- datos_procesados$semestre[i]
        node_list_df$edad[idx] <- datos_procesados$edad[i]
        node_list_df$genero[idx] <- datos_procesados$genero[i]
        node_list_df$num_materias[idx] <- datos_procesados$num_materias[i]
        node_list_df$en_encuesta[idx] <- TRUE
      }
    }
    
    # Rellenar valores NA para otros campos, PERO NO PARA GÉNERO
    node_list_df$semestre[is.na(node_list_df$semestre)] <- "Desconocido"
    node_list_df$edad[is.na(node_list_df$edad)] <- NA
    node_list_df$num_materias[is.na(node_list_df$num_materias)] <- 0
    
    # Función para procesar relaciones
    create_connections <- function(data, relationship_col, relationship_name) {
      connections <- data.frame(from = character(), 
                                to = character(), 
                                relationship = character(),
                                stringsAsFactors = FALSE)
      
      for(i in 1:nrow(data)) {
        person <- data$nombre[i]
        rel_text <- data[[relationship_col]][i]
        
        # Si no hay relaciones, continuar con el siguiente estudiante
        if(is.na(rel_text)) next
        
        # Dividir la lista de nombres
        relations <- strsplit(rel_text, ",\\s*|;\\s*")[[1]]
        
        # Crear una conexión para cada persona mencionada
        for(relation in relations) {
          relation <- trimws(relation) # Eliminar espacios en blanco
          if(relation != "" && !is.na(relation)) {
            # Agregar la conexión a nuestro dataframe
            new_connection <- data.frame(
              from = person,
              to = relation,
              relationship = relationship_name,
              stringsAsFactors = FALSE
            )
            connections <- rbind(connections, new_connection)
          }
        }
      }
      
      return(connections)
    }
    
    # Generar conexiones para cada tipo de relación
    almuerzo_connections <- create_connections(datos_procesados, "almuerzo", "almuerzo")
    trabajo_connections <- create_connections(datos_procesados, "trabajos_grupales", "trabajos")
    social_connections <- create_connections(datos_procesados, "vida_social", "social")
    
    # Combinar todas las conexiones
    all_connections <- rbind(
      almuerzo_connections,
      trabajo_connections,
      social_connections
    )
    
    # Calcular pesos y espacios compartidos
    edge_list_df <- all_connections %>%
      group_by(from, to) %>%
      summarise(
        weight = n(),
        shared_spaces = paste(sort(unique(relationship)), collapse = ", "),
        .groups = 'drop'
      )
    
    # Crear función para contar materias compartidas
    materias_por_estudiante <- setNames(
      datos_procesados$clases_lista,
      datos_procesados$nombre
    )
    
    count_shared_classes <- function(person1, person2, class_dict) {
      if(!person1 %in% names(class_dict) || !person2 %in% names(class_dict)) {
        return(0)
      }
      
      classes1 <- class_dict[[person1]]
      classes2 <- class_dict[[person2]]
      
      shared <- intersect(classes1, classes2)
      return(length(shared))
    }
    
    # Aplicar la función a cada par de estudiantes
    edge_list_df$materias_comunes <- mapply(
      count_shared_classes,
      edge_list_df$from,
      edge_list_df$to,
      MoreArgs = list(class_dict = materias_por_estudiante)
    )
    
    # Guardar los dataframes
    node_list(node_list_df)
    edge_list(edge_list_df)
    
    # Crear visualización básica de la red
    if(requireNamespace("igraph", quietly = TRUE)) {
      library(igraph)
      
      # Crear una red simple para visualización
      g <- graph_from_data_frame(
        d = edge_list_df,
        vertices = node_list_df,
        directed = TRUE
      )
      
      # Guardar el objeto para usarlo en la visualización
      red(g)
    }
  })
  
  # Variable reactiva para la red
  red <- reactiveVal(NULL)
  
  # Mostrar tabla de nodos
  output$nodeTable <- renderDT({
    req(node_list())
    node_list() %>%
      DT::datatable(options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Mostrar tabla de enlaces
  output$edgeTable <- renderDT({
    req(edge_list())
    edge_list() %>%
      DT::datatable(options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Visualización básica de la red
  output$redPlot <- renderPlot({
    req(red())
    
    g <- red()
    
    # Configurar el layout
    layout <- layout_with_fr(g)
    
    # Verificar que los vectores lógicos se manejan correctamente
    `%||%` <- function(x, y) if (is.null(x)) y else x
    
    # Crear la visualización
    vertex_en_encuesta <- V(g)$en_encuesta
    vertex_en_encuesta <- ifelse(is.na(vertex_en_encuesta), FALSE, vertex_en_encuesta)
    
    vertex_colors <- ifelse(vertex_en_encuesta == TRUE,
                            ifelse(V(g)$genero == "MASCULINO", "lightblue",
                                   ifelse(V(g)$genero == "FEMENINO", "pink", "lightgreen")),
                            "gray80")
    
    vertex_label_colors <- ifelse(vertex_en_encuesta == TRUE, "black", "gray50")
    
    vertex_sizes <- ifelse(vertex_en_encuesta == TRUE, 
                           6 + degree(g, mode = "total")/3, 
                           4)  # Nodos no encuestados más pequeños
    
    plot(g,
         layout = layout,
         vertex.size = vertex_sizes,
         vertex.label.cex = 0.7,
         vertex.color = vertex_colors,
         vertex.label.color = vertex_label_colors,
         edge.width = E(g)$weight/2,
         edge.arrow.size = 0.3,
         edge.curved = 0.2,
         main = "Red Social del Curso")
    
    # Añadir leyenda
    legend("topright", 
           legend = c("Masculino", "Femenino", "Otro", "No encuestado"), 
           pt.bg = c("lightblue", "pink", "lightgreen", "gray80"),
           pch = 21,
           pt.cex = 2,
           cex = 0.8,
           bty = "n")
  })
  
  # Descargar Node List
  output$downloadNodes <- downloadHandler(
    filename = function() {
      paste("node_list_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(node_list(), file, row.names = FALSE)
    }
  )
  
  # Descargar Edge List
  output$downloadEdges <- downloadHandler(
    filename = function() {
      paste("edge_list_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(edge_list(), file, row.names = FALSE)
    }
  )
  
  # Descargar Script R
  output$downloadScript <- downloadHandler(
    filename = function() {
      paste("script_analisis_red_", Sys.Date(), ".R", sep = "")
    },
    content = function(file) {
      script <- '
# Script para análisis de la red social
# Generado automáticamente por la aplicación Transformador de Datos

# Cargar librerías necesarias
library(tidyverse)
library(igraph)
library(ggraph)  # Opcional para visualizaciones avanzadas

# Cargar datos
node_list <- read.csv("node_list.csv", stringsAsFactors = FALSE)
edge_list <- read.csv("edge_list.csv", stringsAsFactors = FALSE)

# Crear la red
red <- graph_from_data_frame(
  d = edge_list,
  vertices = node_list,
  directed = TRUE
)

# Explorar características básicas de la red
cat("Número de nodos:", vcount(red), "\\n")
cat("Número de conexiones:", ecount(red), "\\n")
cat("¿Es dirigida?", is_directed(red), "\\n")
cat("¿Es ponderada?", is_weighted(red), "\\n")

# Calcular métricas de centralidad
node_list$indegree <- degree(red, mode = "in")
node_list$outdegree <- degree(red, mode = "out")
node_list$total_degree <- degree(red, mode = "total")
node_list$betweenness <- betweenness(red, normalized = TRUE)
node_list$closeness <- closeness(red, normalized = TRUE)

# Visualización básica
plot(red,
     vertex.size = 5 + degree(red, mode="total")/3,
     vertex.label.cex = 0.7,
     vertex.color = ifelse(V(red)$en_encuesta, 
                          ifelse(V(red)$genero == "MASCULINO", "lightblue", 
                                ifelse(V(red)$genero == "FEMENINO", "pink", "lightgreen")),
                          "gray80"),
     vertex.label.color = ifelse(V(red)$en_encuesta, "black", "gray50"),
     edge.width = E(red)$weight / 2,
     edge.arrow.size = 0.3,
     edge.curved = 0.2,
     main = "Red Social del Curso")

# Detección de comunidades (versión no dirigida de la red)
red_undir <- as.undirected(red, mode = "collapse", edge.attr.comb = list(weight = "sum", "ignore"))
comm_louvain <- cluster_louvain(red_undir)
node_list$community <- membership(comm_louvain)[match(node_list$id, V(red_undir)$name)]

# Visualizar comunidades
plot(comm_louvain, red_undir, 
     vertex.label.cex = 0.7,
     vertex.size = 8,
     main = "Comunidades en la Red Social (Algoritmo de Louvain)")

# Guardar resultados actualizados
write.csv(node_list, "node_list_con_metricas.csv", row.names = FALSE)

# Puedes continuar con más análisis según tus necesidades
'
  writeLines(script, file)
    }
  )
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)