# Análisis Pesquero (VPA + Thomson & Bell)

Esta es una aplicación Shiny en desarrollo para realizar análisis pesquero utilizando los métodos de Análisis de Poblaciones Virtuales (VPA) y el modelo de Thomson & Bell. Está basada en un código proporcionado por el Dr. Carlos Barreto de la AUNAP, con adaptaciones que incluyen correcciones críticas en la fórmula de biomasa, manejo de errores, gráficos interactivos con Plotly, y nuevos puntos de referencia.

## Estado del proyecto
Este proyecto está en desarrollo. La versión actual incluye una interfaz para cargar datos, configurar parámetros, visualizar resultados en gráficos interactivos y descargar resultados en formato CSV.

## Requisitos
- R (versión 4.0 o superior)
- Paquetes de R necesarios:
  ```R
  install.packages(c("shiny", "ggplot2", "readxl", "DT", "tidyr", "scales", "dplyr", "plotly"))
  ```

## Instalación
1. Clona el repositorio:
   ```bash
   git clone https://github.com/tu-usuario/Thompson.git
   ```
2. Abre RStudio y carga el proyecto (`Thompson.Rproj`).
3. Instala las dependencias listadas arriba.
4. Ejecuta la aplicación:
   ```R
   shiny::runApp("app.R")
   ```

## Uso
1. Carga un archivo de datos en formato `.csv` o `.xlsx` con las columnas requeridas: `talla_inf`, `talla_sup`, `frecuencia_muestra`.
2. Configura los parámetros (L∞, K, M, etc.) en la interfaz.
3. Haz clic en "Ejecutar Análisis" para generar gráficos y tablas de resultados.
4. Descarga los resultados en formato CSV si es necesario.

## Estructura del proyecto
- `app.R`: Código principal de la aplicación Shiny, incluyendo la interfaz de usuario y la lógica del servidor.
- (Opcional) Carpetas futuras: `data/` para datos de entrada, `www/` para recursos estáticos.

## Notas
- La aplicación asume datos agregados sobre múltiples años, ideal para muestras pequeñas.
- Se han implementado chequeos de errores para `delta_t` y una corrección en la fórmula de biomasa.

## Contacto
Creado por Anthony Rojas. Contacto: antroojasa@gmail.com  
Basado en el código original del Dr. Carlos Barreto de la AUNAP.
