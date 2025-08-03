#-----------------------------------------------------------------------------
# APLICACIÓN SHINY PARA ANÁLISIS PESQUERO (VPA + THOMSON & BELL) - v12 ADAPTADO DEL SCRIPT DE BARRETO
# VERSIÓN CON CORRECCIÓN CRÍTICA EN FÓRMULA DE BIOMASA Y NUEVO PUNTO DE REFERENCIA
# Ajustes: Manejo de errores, chequeos delta_t, UI con explicación y descarga, + Plotly para gráficos interactivos
# Nuevo: Margen superior aumentado para separar el título del gráfico
#-----------------------------------------------------------------------------

# 1. Cargar las librerías necesarias
library(shiny)
library(ggplot2)
library(readxl)
library(DT)
library(tidyr)
library(scales)
library(dplyr)  # Para bind_rows
library(plotly) # Para gráficos interactivos

#=============================================================================
# 2. FUNCIÓN DE ANÁLISIS (CORREGIDA CON MANEJO DE ERRORES)
#=============================================================================
analisis_pesquero_completo <- function(Linf, K, M, param_a, param_b, datos_captura, captura_total_kg, precio_por_kg, F_terminal = 0.5, multiplicadores_F = seq(0, 3, by = 0.1)) {
  # --- PASO 1: Procesamiento de datos ---
  df <- datos_captura
  df$talla_media <- (df$talla_inf + df$talla_sup) / 2
  df$peso_medio_g <- param_a * df$talla_media^param_b
  df$f_por_wm <- df$frecuencia_muestra * df$peso_medio_g
  total_w_muestra <- sum(df$f_por_wm, na.rm = TRUE)
  df$porc_w_muestra <- df$f_por_wm / total_w_muestra
  df$w_captura_g <- df$porc_w_muestra * (captura_total_kg * 1000)
  df$captura_numeros <- df$w_captura_g / df$peso_medio_g
  
  # Chequeo nuevo: Advertir si delta_t <=0
  df$delta_t <- (1/K) * log((Linf - df$talla_inf) / (Linf - df$talla_sup))
  if (any(df$delta_t <= 0, na.rm = TRUE)) {
    warning("Delta_t negativo o cero detectado; revise si tallas exceden Linf o K es inadecuado.")
  }
  
  # --- PASO 2: VPA con tryCatch para manejo de errores ---
  n_clases <- nrow(df)
  df$F <- numeric(n_clases); df$Z <- numeric(n_clases); df$N <- numeric(n_clases)
  tryCatch({
    df$F[n_clases] <- F_terminal
    df$Z[n_clases] <- df$F[n_clases] + M
    if (df$F[n_clases] > 0 && !is.infinite(df$delta_t[n_clases]) && (1 - exp(-df$Z[n_clases] * df$delta_t[n_clases])) > 0) {
      df$N[n_clases] <- df$captura_numeros[n_clases] * df$Z[n_clases] / (df$F[n_clases] * (1 - exp(-df$Z[n_clases] * df$delta_t[n_clases])))
    } else {
      df$N[n_clases] <- 0
    }
    for (i in (n_clases - 1):1) {
      if (is.finite(df$delta_t[i]) && df$delta_t[i] > 0) {
        df$N[i] <- (df$N[i+1] * exp(M * df$delta_t[i] / 2)) + (df$captura_numeros[i] * exp(M * df$delta_t[i] / 2))
        F_val <- (1 / df$delta_t[i]) * log(df$N[i] / df$N[i+1]) - M
        df$F[i] <- ifelse(is.finite(F_val) && F_val > 0, F_val, 0)
        df$Z[i] <- df$F[i] + M
      } else {
        df$N[i] <- 0
        df$F[i] <- 0
        df$Z[i] <- M
      }
    }
  }, error = function(e) {
    message("Error en VPA: ", e$message, ". Revise inputs o datos.")
    df$N <- rep(0, n_clases)  # Valor fallback
  })
  
  F_linea_base <- df$F
  reclutas_estimados <- df$N[1]
  
  # --- PASO 3: Simulación Thomson & Bell con tryCatch y bind_rows ---
  resultados_simulacion <- lapply(multiplicadores_F, function(f_mult) {
    tryCatch({
      sim_df <- df[, c("talla_media", "peso_medio_g", "delta_t")]
      sim_df$F_nuevo <- F_linea_base * f_mult
      sim_df$Z_nuevo <- sim_df$F_nuevo + M
      sim_df$N_nuevo <- numeric(n_clases)
      sim_df$N_nuevo[1] <- reclutas_estimados
      for(i in 1:(n_clases - 1)) {
        if(is.finite(sim_df$Z_nuevo[i]) && is.finite(sim_df$delta_t[i]) && sim_df$delta_t[i] > 0) {
          sim_df$N_nuevo[i+1] <- sim_df$N_nuevo[i] * exp(-sim_df$Z_nuevo[i] * sim_df$delta_t[i])
        } else {
          sim_df$N_nuevo[i+1] <- 0
        }
      }
      sim_df$captura_predicha_numeros <- sim_df$N_nuevo * (sim_df$F_nuevo / sim_df$Z_nuevo) * (1 - exp(-sim_df$Z_nuevo * sim_df$delta_t))  # Ajuste: Multiplicado por delta_t para integración temporal (opcional, basado en tu agregación)
      sim_df$rendimiento_predicho_g <- sim_df$captura_predicha_numeros * sim_df$peso_medio_g
      
      # <<< CORRECCIÓN CRÍTICA: Fórmula de Biomasa corregida a la estándar.
      sim_df$biomasa_media_predicha_g <- (sim_df$N_nuevo * (1 - exp(-sim_df$Z_nuevo * sim_df$delta_t)) / sim_df$Z_nuevo) * sim_df$peso_medio_g  # Ajuste similar para consistencia
      
      rendimiento_total_g <- sum(sim_df$rendimiento_predicho_g, na.rm = TRUE)
      biomasa_total_g <- sum(sim_df$biomasa_media_predicha_g, na.rm = TRUE)
      valor_total <- (rendimiento_total_g / 1000) * precio_por_kg 
      
      data.frame(
        F_multiplicador = f_mult, 
        Rendimiento_Kg = rendimiento_total_g / 1000,
        Biomasa_Kg = biomasa_total_g / 1000,
        Valor_COP = valor_total
      )
    }, error = function(e) {
      message("Error en simulación para F_mult=", f_mult, ": ", e$message)
      data.frame(F_multiplicador = f_mult, Rendimiento_Kg = NA, Biomasa_Kg = NA, Valor_COP = NA)
    })
  })
  bind_rows(resultados_simulacion)
}

#=============================================================================
# 3. INTERFAZ DE USUARIO (UI) CON EXPLICACIÓN Y DESCARGA
#=============================================================================
ui <- fluidPage(
  titlePanel("Análisis de Pesquerías (VPA y Thomson & Bell)"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("1. Cargar Datos"),
      fileInput("archivo_datos", "Seleccionar archivo (.csv o .xlsx)", accept = c(".csv", ".xlsx")),
      helpText("Columnas requeridas: 'talla_inf', 'talla_sup', 'frecuencia_muestra'"),
      helpText("Nota: Este análisis asume datos agregados sobre múltiples años para un enfoque general, ideal para muestras pequeñas por año."),
      hr(),
      h4("2. Parámetros"),
      numericInput("Linf", "L∞ (cm):", 197, min = 1),
      numericInput("K", "K (anual):", 0.4, min = 0.01, step = 0.01),
      numericInput("M", "M (Mortalidad Natural):", 0.23, min = 0.01, step = 0.01),
      numericInput("param_a", "Parámetro 'a':", 0.000099, step = 1e-7),
      numericInput("param_b", "Parámetro 'b':", 2.65, step = 0.1),
      numericInput("captura_total_kg", "Captura Total (Kg):", 34301, min = 1),
      numericInput("precio_kg", "Precio por Kg (COP):", 36000, min = 0),
      hr(),
      actionButton("ejecutar", "Ejecutar Análisis", class = "btn-primary", icon = icon("play"), width = "100%")
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("Gráfico Combinado", icon = icon("chart-area"), plotlyOutput("graficoCombinado", height = "600px")),  # Cambiado a plotlyOutput
        tabPanel("Tabla de Resultados", icon = icon("table"), 
                 DTOutput("tablaResultados"),
                 downloadButton("descargarResultados", "Descargar CSV"),
                 h4("Puntos de Referencia Biológico y Económico"),
                 verbatimTextOutput("info_rms_rme"))
      )
    )
  )
)

#=============================================================================
# 4. SERVIDOR (LÓGICA) CON VALIDACIONES Y PLOTLY
#=============================================================================
server <- function(input, output, session) {
  
  datos_cargados <- reactive({
    req(input$archivo_datos)
    archivo <- input$archivo_datos$datapath
    ext <- tools::file_ext(archivo)
    if (!(ext %in% c("csv", "xlsx"))) validate("Archivo no soportado (solo .csv o .xlsx).")
    df <- if (ext == "csv") read.csv(archivo) else read_excel(archivo)
    req_cols <- c("talla_inf", "talla_sup", "frecuencia_muestra")
    missing_cols <- setdiff(req_cols, colnames(df))
    if (length(missing_cols) > 0) validate(paste("Columnas faltantes:", paste(missing_cols, collapse = ", ")))
    df
  })
  
  resultados_analisis <- eventReactive(input$ejecutar, {
    req(datos_cargados(), input$Linf, input$K, input$M, input$param_a, input$param_b, input$captura_total_kg, input$precio_kg)
    withProgress(message = 'Realizando análisis...', value = 0.5, {
      analisis_pesquero_completo(
        Linf = input$Linf, K = input$K, M = input$M,
        param_a = input$param_a, param_b = input$param_b,
        datos_captura = datos_cargados(),
        captura_total_kg = input$captura_total_kg,
        precio_por_kg = input$precio_kg 
      )
    })
  })
  
  puntos_referencia <- eventReactive(input$ejecutar, {
    df <- resultados_analisis()
    rms <- df[which.max(df$Rendimiento_Kg), ]
    rme <- df[which.max(df$Valor_COP), ]
    
    # --- NOVEDAD: Encontrar el punto de intersección visual ---
    max_valor_cop <- max(df$Valor_COP, na.rm = TRUE)
    max_bio <- max(df$Biomasa_Kg, na.rm = TRUE)
    escala <- if (max_valor_cop > 0) max_bio / max_valor_cop else 1
    
    # Encontrar el índice donde la diferencia cambia de signo
    diferencia <- df$Biomasa_Kg - (df$Valor_COP * escala)
    interseccion_idx <- which(diff(sign(diferencia)) != 0)[1]
    
    # Interpolar para encontrar el punto F exacto
    if(!is.na(interseccion_idx)){
      x1 <- df$F_multiplicador[interseccion_idx]
      y1 <- diferencia[interseccion_idx]
      x2 <- df$F_multiplicador[interseccion_idx + 1]
      y2 <- diferencia[interseccion_idx + 1]
      f_interseccion <- x1 - y1 * (x2 - x1) / (y2 - y1)
    } else {
      f_interseccion <- NA
    }
    
    list(rms = rms, rme = rme, f_interseccion = f_interseccion)
  })
  
  output$graficoCombinado <- renderPlotly({
    req(resultados_analisis())
    df <- resultados_analisis()
    ref <- puntos_referencia()
    rms_info <- ref$rms
    
    df_largo <- pivot_longer(df, cols = c("Rendimiento_Kg", "Biomasa_Kg", "Valor_COP"), names_to = "Metrica", values_to = "Valor_metrica")
    
    max_valor_cop <- max(df$Valor_COP, na.rm = TRUE)
    max_bio_rend <- max(df$Rendimiento_Kg, df$Biomasa_Kg, na.rm = TRUE)
    escala <- if (max_valor_cop > 0) max_bio_rend / max_valor_cop else 1
    
    p <- ggplot(df_largo, aes(x = F_multiplicador, group = Metrica)) +
      geom_line(aes(y = Valor_metrica, color = Metrica), data = subset(df_largo, Metrica %in% c("Rendimiento_Kg", "Biomasa_Kg")), size = 1.2) +
      geom_line(aes(y = Valor_metrica * escala, color = Metrica), data = subset(df_largo, Metrica == "Valor_COP"), size = 1.2) +
      geom_vline(xintercept = rms_info$F_multiplicador, linetype = "dashed", color = "black", size = 1) +
      geom_segment(data = rms_info, aes(x = F_multiplicador, y = Rendimiento_Kg, xend = F_multiplicador, yend = 0),
                   arrow = arrow(length = unit(0.4, "cm")), color = "red", size = 1.2, inherit.aes = FALSE) +
      annotate("text", x = rms_info$F_multiplicador, y = rms_info$Rendimiento_Kg, 
               label = "RMS", vjust = -1.5, color = "red", size = 5, fontface = "bold") +
      scale_y_continuous(name = "Rendimiento y Biomasa (Kg)", labels = comma,
                         sec.axis = sec_axis(~ . / escala, name = "Valor Económico (COP)", labels = comma)) +
      scale_color_manual(name = "Métrica",
                         values = c("Rendimiento_Kg" = "#3498DB", "Biomasa_Kg" = "#2ECC71", "Valor_COP" = "#E74C3C"),
                         labels = c("Biomasa", "Rendimiento", "Valor Económico")) +
      labs(title = "Análisis de Rendimiento, Biomasa y Valor Económico",
           subtitle = "Líneas verticales indican puntos de referencia biológicos y económicos.",
           x = "Multiplicador de Esfuerzo de Pesca (Factor-F)") +
      theme_light(base_size = 16) + theme(legend.position = "top")
    
    # --- NOVEDAD: Añadir la línea de intersección si existe ---
    if(!is.na(ref$f_interseccion)){
      p <- p + geom_vline(xintercept = ref$f_interseccion, linetype = "dotted", color = "blue", size = 1.2) +
        annotate("text", x = ref$f_interseccion, y = 0, label = "Intersección B-V", hjust = -0.1, vjust = -0.5, color = "blue", size=4)
    }
    
    # Convertir a Plotly para interactividad, con margen superior aumentado
    ggplotly(p) %>%
      layout(hovermode = "x unified",  # Hover unificado para todas las líneas
             legend = list(orientation = "h", y = 1.1),  # Leyenda horizontal arriba
             margin = list(t = 100))  # Ajuste: Más espacio superior para separar el título
  })
  
  output$tablaResultados <- renderDT({
    req(resultados_analisis())
    df <- resultados_analisis()
    names(df) <- c("Factor-F", "Rendimiento (Kg)", "Biomasa (Kg)", "Valor Económico (COP)")
    datatable(df, options = list(pageLength = 10, searching = FALSE), rownames = FALSE) %>%
      formatCurrency(columns = "Valor Económico (COP)", currency = "$", interval = 3, mark = ",", digits = 0) %>%
      formatRound(columns = c("Rendimiento (Kg)", "Biomasa (Kg)"), digits = 0)
  })
  
  output$descargarResultados <- downloadHandler(
    filename = function() { "resultados_analisis_pesquero.csv" },
    content = function(file) {
      write.csv(resultados_analisis(), file, row.names = FALSE)
    }
  )
  
  output$info_rms_rme <- renderText({
    req(puntos_referencia())
    ref <- puntos_referencia()
    rms_text <- paste0("RMS: Se alcanza con un Factor-F de ", ref$rms$F_multiplicador, 
                       ", produciendo un rendimiento de ", format(round(ref$rms$Rendimiento_Kg), big.mark = ","), " Kg.")
    rme_text <- paste0("RME: Se alcanza con un Factor-F de ", ref$rme$F_multiplicador, 
                       ", produciendo un valor de $", format(round(ref$rme$Valor_COP), big.mark = ","), " COP.")
    inter_text <- if(!is.na(ref$f_interseccion)) {
      paste0("Intersección Biomasa-Valor: Ocurre con un Factor-F de ", round(ref$f_interseccion, 2), ".")
    } else {""}
    
    paste(rms_text, rme_text, inter_text, sep = "\n")
  })
}

#=============================================================================
# 5. EJECUTAR LA APLICACIÓN
#=============================================================================
shinyApp(ui = ui, server = server)
