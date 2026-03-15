# app.R
library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)
library(dplyr)
library(plotly)
library(rmarkdown)

# ---------------- Helpers ----------------
is_num <- function(x) is.numeric(x) || is.integer(x)
num_vars <- function(df) names(df)[vapply(df, is_num, logical(1))]
pretty_name <- function(x) gsub("_", " ", x)

safe_cor <- function(df) {
  df_num <- df[, num_vars(df), drop = FALSE]
  if (ncol(df_num) < 2) return(NULL)
  suppressWarnings(stats::cor(df_num, use = "pairwise.complete.obs"))
}

sig_label <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.001) return("***")
  if (p < 0.01)  return("**")
  if (p < 0.05)  return("*")
  if (p < 0.1)   return(".")
  ""
}

rmse_of <- function(m) sqrt(mean(residuals(m)^2))

# Interpretación "vida real" (ajústala a tu gusto)
coef_story <- list(
  temperatura_media = "Factor climático: temperaturas más altas suelen reducir demanda asociada a calefacción; en calor extremo podría aumentar por refrigeración.",
  gasto_consumidor  = "Indicador de actividad económica: mayor gasto suele asociarse a mayor consumo energético agregado.",
  inflacion         = "Entorno macroeconómico: efecto directo suele ser débil; puede capturar presión de costes y cambios de hábitos.",
  desempleo         = "Actividad económica: mayor desempleo suele asociarse a menor consumo/producción y menor demanda energética.",
  consumo_historico = "Inercia operativa: la demanda futura suele seguir patrones del consumo pasado."
)

interpret_beta <- function(var, beta) {
  dir <- if (beta >= 0) "aumenta" else "reduce"
  paste0(
    "- ", var, ": +1 unidad ", dir, " la demanda en ~",
    round(abs(beta), 2), " kWh. ",
    if (!is.null(coef_story[[var]])) coef_story[[var]] else ""
  )
}

# ---------------- Data (fixed) ----------------
df0 <- read.csv("data/datos.csv", stringsAsFactors = FALSE)

# Variables del enunciado
predictors_5 <- c("temperatura_media", "gasto_consumidor", "inflacion", "desempleo", "consumo_historico")
y_var <- "demanda_energia"

# ---------------- PDF template (auditoría preliminar) ----------------
report_template <- c(
  "---",
  "title: \"Auditoría preliminar del modelo\"",
  "output: pdf_document",
  "params:",
  "  model_name: NA",
  "  formula_txt: NA",
  "  metrics: NA",
  "  coefs: NA",
  "  interpret: NA",
  "  has_single: NA",
  "  single_var: NA",
  "  data: NA",
  "---",
  "",
  "```{r setup, include=FALSE}",
  "knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)",
  "```",
  "",
  "# Identificación",
  "",
  "**Modelo:** `r params$model_name`  ",
  "**Fórmula:** `r params$formula_txt`",
  "",
  "# Métricas clave",
  "",
  "```{r}",
  "knitr::kable(params$metrics)",
  "```",
  "",
  "# Coeficientes",
  "",
  "```{r}",
  "knitr::kable(params$coefs)",
  "```",
  "",
  "# Interpretación (didáctica)",
  "",
  "```{r}",
  "cat(params$interpret)",
  "```",
  "",
  "# Visualización",
  "",
  "```{r, eval = params$has_single}",
  "d <- params$data",
  "xv <- params$single_var",
  "plot(d[[xv]], d$demanda_energia,",
  "     xlab = xv, ylab = 'demanda_energia (kWh)',",
  "     main = paste('Demanda vs', xv))",
  "m <- lm(as.formula(paste('demanda_energia ~', xv)), data = d)",
  "abline(m, lwd = 2)",
  "```"
)

# ---------------- UI ----------------
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = "",
    titleWidth = 200
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Portada", tabName = "home", icon = icon("house")),
      menuItem("Datos", tabName = "data", icon = icon("table")),
      menuItem("Análisis univariante", tabName = "uni", icon = icon("chart-column")),
      menuItem("Análisis bivariante", tabName = "bivar", icon = icon("link")),
      menuItem("Ajuste del modelo", tabName = "fit", icon = icon("sliders")),
      menuItem("Predicción", tabName = "pred", icon = icon("calculator"))
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML("
      .small-box { border-radius: 16px; }
      .box { border-radius: 16px; }
      .content-wrapper, .right-side { background: #f7f7f7; }

      .hero { padding: 20px 22px 14px 22px; }
      .hero h2 { margin: 0; font-weight: 900; letter-spacing: .2px; }
      .hero p { margin: 8px 0 0 0; opacity: .95; font-size: 14px; line-height: 1.35; }
      .hero ul { margin: 10px 0 8px 18px; }
      .hero li { margin: 4px 0; }

      .pill { display:inline-block; padding:7px 11px; border-radius:999px; background:#eef2ff; margin:6px 8px 0 0; font-size:12px; }
      .cor-caption { font-weight: 700; margin-bottom: 8px; }

      .mini-note { font-size: 12px; opacity: .9; }
    "))),
    tabItems(
      
      # ---------------- HOME ----------------
      tabItem(
        tabName = "home",
        
        fluidRow(
          box(
            width = 12, status = "primary", solidHeader = TRUE,
            div(class = "hero",
                tags$h2("Pronosticar con inteligencia"),
                tags$p("Diagnóstico inicial para un modelo de predicción de demanda energética en una empresa de servicios energéticos."),
                tags$ul(
                  tags$li("Integra señales climáticas y macroeconómicas para contextualizar la demanda."),
                  tags$li("Permite identificar patrones, outliers y relaciones relevantes antes de modelizar."),
                  tags$li("Base cuantitativa para una futura implementación en IBM Planning Analytics.")
                ),
                tags$p(tags$b("Objetivo:"), " extraer evidencia exploratoria para apoyar selección de variables y evaluación posterior de modelos.")
            )
          )
        ),
        
        fluidRow(
          valueBoxOutput("vb_rows_home"),
          valueBoxOutput("vb_cols_home"),
          valueBoxOutput("vb_na_home")
        ),
        
        fluidRow(
          box(
            width = 7, status = "info", solidHeader = TRUE, title = "Resumen del conjunto de datos",
            p(
              strong("Tamaño: "),
              textOutput("txt_rows", inline = TRUE),
              " observaciones · ",
              textOutput("txt_cols", inline = TRUE),
              " variables."
            ),
            p(strong("Variable objetivo:"), " demanda_energia (kWh).")
          ),
          box(
            width = 5, status = "warning", solidHeader = TRUE, title = "Variables disponibles",
            uiOutput("chips_vars_home")
          )
        )
      ),
      
      # ---------------- DATA ----------------
      tabItem(
        tabName = "data",
        fluidRow(
          valueBoxOutput("vb_rows"),
          valueBoxOutput("vb_cols"),
          valueBoxOutput("vb_na")
        ),
        fluidRow(
          box(width = 6, status = "primary", solidHeader = TRUE, title = "Resumen estadístico",
              verbatimTextOutput("summary_txt")),
          box(width = 6, status = "primary", solidHeader = TRUE, title = "Estructura",
              verbatimTextOutput("str_txt"))
        ),
        fluidRow(
          box(width = 12, status = "info", solidHeader = TRUE, title = "Tabla de datos",
              DTOutput("dt_data"))
        )
      ),
      
      # ---------------- UNIVARIANTE ----------------
      tabItem(
        tabName = "uni",
        fluidRow(
          box(
            width = 4, status = "primary", solidHeader = TRUE, title = "Controles",
            uiOutput("uni_var_ui"),
            sliderInput("bins", "Bins (histograma)", min = 5, max = 80, value = 30),
            checkboxInput("show_density", "Mostrar curva de densidad", value = TRUE)
          ),
          box(
            width = 8, status = "info", solidHeader = TRUE, title = "Estadísticos",
            tableOutput("uni_stats")
          )
        ),
        fluidRow(
          box(width = 6, status = "primary", solidHeader = TRUE, title = "Histograma (interactivo)",
              plotlyOutput("hist_plotly", height = 340)),
          box(width = 6, status = "primary", solidHeader = TRUE, title = "Boxplot (interactivo, con etiquetas)",
              plotlyOutput("box_plotly", height = 340))
        )
      ),
      
      # ---------------- BIVARIANTE (2D + COR) ----------------
      tabItem(
        tabName = "bivar",
        fluidRow(
          box(
            width = 4, status = "primary", solidHeader = TRUE, title = "Selección de variables",
            uiOutput("x2_ui"),
            uiOutput("y2_ui"),
            checkboxInput("lm_line2", "Recta de regresión", value = TRUE)
          ),
          box(
            width = 8, status = "info", solidHeader = TRUE, title = "Matriz de correlación (formal)",
            div(class = "cor-caption", "Correlación de Pearson (pairwise complete observations)"),
            DTOutput("dt_cor_formal")
          )
        ),
        fluidRow(
          box(width = 6, status = "primary", solidHeader = TRUE, title = "Heatmap de correlación",
              plotlyOutput("cor_heat_formal", height = 380)),
          box(width = 6, status = "primary", solidHeader = TRUE, title = "Dispersión (2D)",
              plotlyOutput("scatter2d_bivar", height = 380))
        )
      ),
      
      # ---------------- AJUSTE DEL MODELO ----------------
      tabItem(
        tabName = "fit",
        fluidRow(
          box(
            width = 4, status = "primary", solidHeader = TRUE, title = "Selección de modelo",
            textInput("model_name", "Nombre del modelo", value = "modelo_lineal_01"),
            checkboxGroupInput(
              "fit_vars",
              "Variables explicativas (elige entre las 5)",
              choices = predictors_5,
              selected = c("temperatura_media", "gasto_consumidor", "desempleo", "consumo_historico")
            ),
            actionButton("fit_btn", "Ajustar modelo", icon = icon("play"), class = "btn btn-primary"),
            br(), br(),
            downloadButton("download_audit_pdf", "Descargar auditoría (PDF)")
          ),
          box(
            width = 8, status = "info", solidHeader = TRUE, title = "Informe didáctico del ajuste",
            uiOutput("fit_report_ui")
          )
        ),
        
        fluidRow(
          box(width = 6, status = "primary", solidHeader = TRUE, title = "Métricas del modelo",
              DTOutput("dt_fit_metrics")),
          box(width = 6, status = "primary", solidHeader = TRUE, title = "Coeficientes (tabla)",
              DTOutput("dt_fit_coefs"))
        ),
        
        fluidRow(
          box(width = 12, status = "warning", solidHeader = TRUE, title = "Gráfico 2D (solo si hay 1 predictor)",
              uiOutput("fit_plot_2d_ui"))
        )
      ),
      
      # ---------------- PREDICCIÓN ----------------
      tabItem(
        tabName = "pred",
        fluidRow(
          box(
            width = 4, status = "primary", solidHeader = TRUE, title = "Inputs de predicción",
            tags$p(class = "mini-note", "Usa el modelo ajustado en la pestaña anterior."),
            actionButton("use_fit_model", "Cargar modelo", icon = icon("download")),
            br(), br(),
            uiOutput("pred_inputs_ui"),
            actionButton("pred_btn", "Realizar predicción", icon = icon("calculator"), class = "btn btn-success")
          ),
          box(
            width = 8, status = "info", solidHeader = TRUE, title = "Resultado (predicción + intervalos + error)",
            uiOutput("pred_result_ui")
          )
        ),
        fluidRow(
          box(width = 12, status = "warning", solidHeader = TRUE, title = "Visualización predicción 2D",
              uiOutput("pred_plot_2d_ui"))
        )
      )
      
    )
  )
)

# ---------------- Server ----------------
server <- function(input, output, session) {
  
  df <- reactive(df0)
  
  # HOME boxes
  output$vb_rows_home <- renderValueBox({
    valueBox(nrow(df()), "Observaciones", icon = icon("list"), color = "aqua")
  })
  output$vb_cols_home <- renderValueBox({
    valueBox(ncol(df()), "Variables", icon = icon("table-columns"), color = "purple")
  })
  output$vb_na_home <- renderValueBox({
    valueBox(sum(is.na(df())), "Valores perdidos (NA)", icon = icon("triangle-exclamation"), color = "yellow")
  })
  output$txt_rows <- renderText(nrow(df()))
  output$txt_cols <- renderText(ncol(df()))
  output$chips_vars_home <- renderUI({
    vars <- names(df())
    tags$div(lapply(vars, function(v) tags$span(class = "pill", v)))
  })
  
  # DATA tab
  output$vb_rows <- renderValueBox({
    valueBox(nrow(df()), "Observaciones", icon = icon("list"), color = "aqua")
  })
  output$vb_cols <- renderValueBox({
    valueBox(ncol(df()), "Variables", icon = icon("table-columns"), color = "purple")
  })
  output$vb_na <- renderValueBox({
    valueBox(sum(is.na(df())), "Valores perdidos (NA)", icon = icon("triangle-exclamation"), color = "yellow")
  })
  output$summary_txt <- renderPrint(summary(df()))
  output$str_txt <- renderPrint(str(df()))
  output$dt_data <- renderDT({
    datatable(df(), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # UNIVARIANTE
  output$uni_var_ui <- renderUI({
    nv <- num_vars(df())
    selectInput("uni_var", "Variable numérica", choices = nv, selected = "demanda_energia")
  })
  
  output$uni_stats <- renderTable({
    req(input$uni_var)
    x <- df()[[input$uni_var]]
    x <- x[!is.na(x)]
    data.frame(
      Estadístico = c("N", "Media", "Mediana", "Desv. estándar", "Mínimo", "Q1", "Q3", "Máximo"),
      Valor = c(
        length(x), mean(x), median(x), sd(x),
        min(x), quantile(x, 0.25), quantile(x, 0.75), max(x)
      ),
      check.names = FALSE
    )
  })
  
  output$hist_plotly <- renderPlotly({
    req(input$uni_var)
    v <- input$uni_var
    p <- ggplot(df(), aes(x = .data[[v]])) +
      geom_histogram(aes(y = after_stat(density)), bins = input$bins) +
      labs(x = pretty_name(v), y = "Densidad")
    if (isTRUE(input$show_density)) p <- p + geom_density(linewidth = 0.9)
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  output$box_plotly <- renderPlotly({
    req(input$uni_var)
    v <- input$uni_var
    x <- df()[[v]]
    x <- x[!is.na(x)]
    qs <- quantile(x, probs = c(0, .25, .5, .75, 1))
    lab <- paste0(
      "min: ", round(qs[1], 2), "<br>",
      "Q1: ", round(qs[2], 2), "<br>",
      "med: ", round(qs[3], 2), "<br>",
      "Q3: ", round(qs[4], 2), "<br>",
      "max: ", round(qs[5], 2)
    )
    plot_ly() |>
      add_boxplot(y = x, name = pretty_name(v), boxpoints = "outliers") |>
      layout(
        yaxis = list(title = pretty_name(v)),
        annotations = list(list(x = 0.5, y = qs[5], text = lab, showarrow = FALSE, yanchor = "top"))
      )
  })
  
  # BIVARIANTE CONTROLES
  output$x2_ui <- renderUI({
    nv <- num_vars(df())
    selectInput("x2", "Eje X", choices = nv, selected = "consumo_historico")
  })
  output$y2_ui <- renderUI({
    nv <- num_vars(df())
    selectInput("y2", "Eje Y", choices = nv, selected = "demanda_energia")
  })
  
  # Correlación formal: tabla + heatmap
  output$dt_cor_formal <- renderDT({
    cm <- safe_cor(df())
    validate(need(!is.null(cm), "No hay suficientes variables numéricas para correlación."))
    
    datatable(
      round(cm, 3),
      rownames = TRUE,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = "t",
        ordering = FALSE
      )
    ) %>%
      formatStyle(
        columns = colnames(cm),
        backgroundColor = styleInterval(
          c(-0.8, -0.5, -0.2, 0.2, 0.5, 0.8),
          c("#67000d", "#cb181d", "#fcbba1", "#ffffff", "#c7e9c0", "#41ab5d", "#00441b")
        )
      )
  })
  
  output$cor_heat_formal <- renderPlotly({
    cm <- safe_cor(df())
    validate(need(!is.null(cm), "No hay suficientes variables numéricas para correlación."))
    plot_ly(
      x = colnames(cm), y = rownames(cm), z = cm,
      type = "heatmap",
      hovertemplate = "%{y} vs %{x}<br>r=%{z:.3f}<extra></extra>"
    ) %>%
      layout(
        xaxis = list(title = "", tickangle = -35),
        yaxis = list(title = ""),
        margin = list(l = 60, r = 20, b = 90, t = 10)
      )
  })
  
  output$scatter2d_bivar <- renderPlotly({
    req(input$x2, input$y2)
    validate(need(input$x2 != input$y2, "Selecciona variables distintas para X e Y."))
    p <- ggplot(df(), aes(x = .data[[input$x2]], y = .data[[input$y2]])) +
      geom_point(alpha = 0.85) +
      labs(x = pretty_name(input$x2), y = pretty_name(input$y2))
    if (isTRUE(input$lm_line2)) p <- p + geom_smooth(method = "lm", se = FALSE)
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  # ---------------- Ajuste del modelo ----------------
  rv <- reactiveValues(model = NULL, model_name = NULL, fit_vars = NULL)
  
  observeEvent(input$fit_btn, {
    req(input$fit_vars)
    validate(need(length(input$fit_vars) >= 1, "Selecciona al menos 1 variable explicativa."))
    
    f <- as.formula(paste0(y_var, " ~ ", paste(input$fit_vars, collapse = " + ")))
    m <- lm(f, data = df())
    
    rv$model <- m
    rv$model_name <- input$model_name
    rv$fit_vars <- input$fit_vars
  })
  
  output$dt_fit_metrics <- renderDT({
    req(rv$model)
    m <- rv$model
    s <- summary(m)
    met <- data.frame(
      Metrica = c("N", "Predictores", "R2", "R2 ajustado", "Sigma (SE residual)", "RMSE", "AIC", "BIC", "p-valor F global"),
      Valor = c(
        length(m$fitted.values),
        length(attr(terms(m), "term.labels")),
        round(s$r.squared, 4),
        round(s$adj.r.squared, 4),
        round(s$sigma, 2),
        round(rmse_of(m), 2),
        round(AIC(m), 2),
        round(BIC(m), 2),
        signif(pf(s$fstatistic[1], s$fstatistic[2], s$fstatistic[3], lower.tail = FALSE), 3)
      ),
      check.names = FALSE
    )
    datatable(met, options = list(dom = "t", paging = FALSE), rownames = FALSE)
  })
  
  output$dt_fit_coefs <- renderDT({
    req(rv$model)
    m <- rv$model
    co <- summary(m)$coefficients
    tab <- data.frame(
      Variable = rownames(co),
      Estimate = round(co[, 1], 6),
      `Std. Error` = round(co[, 2], 6),
      `t value` = round(co[, 3], 3),
      `Pr(>|t|)` = signif(co[, 4], 3),
      Signif = vapply(co[, 4], sig_label, character(1)),
      check.names = FALSE
    )
    datatable(tab, options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
  })
  
  output$fit_report_ui <- renderUI({
    req(rv$model)
    m <- rv$model
    s <- summary(m)
    vars <- attr(terms(m), "term.labels")
    co <- coef(m)
    coef_tab <- summary(m)$coefficients
    
    # Construir interpretación didáctica
    interp_lines <- c()
    for (v in vars) {
      beta <- unname(co[[v]])
      p <- coef_tab[v, 4]
      sig_txt <- if (p < 0.05) "significativo (α=0.05)" else "no significativo (α=0.05)"
      interp_lines <- c(interp_lines, paste0(interpret_beta(v, beta), " → ", sig_txt))
    }
    
    tags$div(
      tags$p(tags$b("Modelo ajustado:"), paste0(rv$model_name, " (OLS)")),
      tags$p(tags$b("Fórmula:"), tags$code(paste0(y_var, " ~ ", paste(vars, collapse = " + ")))),
      tags$hr(),
      tags$h4("Puntos clave"),
      tags$ul(
        tags$li(paste0("Explica aproximadamente ", round(100*s$r.squared, 1), "% de la variabilidad (R²=", round(s$r.squared, 3), ").")),
        tags$li(paste0("R² ajustado=", round(s$adj.r.squared, 3), " (penaliza complejidad).")),
        tags$li(paste0("Error típico (RMSE) ≈ ", round(rmse_of(m), 2), " kWh.")),
        tags$li(paste0("AIC=", round(AIC(m),2), " | BIC=", round(BIC(m),2), " (menor es mejor).")),
        tags$li(paste0("Test global F: p-valor=", signif(pf(s$fstatistic[1], s$fstatistic[2], s$fstatistic[3], lower.tail = FALSE), 3), " (modelo globalmente significativo si es pequeño)."))
      ),
      tags$h4("Interpretación (mercado / vida real)"),
      tags$pre(paste(interp_lines, collapse = "\n"))
    )
  })
  
  output$fit_plot_2d_ui <- renderUI({
    req(rv$model)
    vars <- attr(terms(rv$model), "term.labels")
    if (length(vars) != 1) {
      return(tags$p(class = "mini-note", "Este gráfico solo aparece si ajustas el modelo con 1 única variable explicativa."))
    }
    plotlyOutput("fit_plot_2d", height = 420)
  })
  
  output$fit_plot_2d <- renderPlotly({
    req(rv$model)
    m <- rv$model
    vars <- attr(terms(m), "term.labels")
    req(length(vars) == 1)
    xvar <- vars[1]
    
    d <- df()
    
    p <- ggplot(d, aes(x = .data[[xvar]], y = .data[[y_var]])) +
      geom_point(alpha = 0.85) +
      geom_smooth(method = "lm", se = FALSE) +
      labs(x = pretty_name(xvar), y = pretty_name(y_var))
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  # PDF auditoría
  output$download_audit_pdf <- downloadHandler(
    filename = function() {
      nm <- if (!is.null(rv$model_name) && nzchar(rv$model_name)) rv$model_name else "modelo"
      paste0(gsub("[^A-Za-z0-9_\\-]", "_", nm), "_auditoria.pdf")
    },
    content = function(file) {
      req(rv$model)
      d <- df()
      
      rmd_path <- file.path(tempdir(), "auditoria_preliminar.Rmd")
      writeLines(report_template, rmd_path)
      
      vars <- attr(terms(rv$model), "term.labels")
      s <- summary(rv$model)
      
      # Métricas + coeficientes
      metrics <- data.frame(
        Metrica = c("N", "Predictores", "R2", "R2 ajustado", "Sigma", "RMSE", "AIC", "BIC"),
        Valor = c(
          length(rv$model$fitted.values),
          length(vars),
          round(s$r.squared, 4),
          round(s$adj.r.squared, 4),
          round(s$sigma, 2),
          round(rmse_of(rv$model), 2),
          round(AIC(rv$model), 2),
          round(BIC(rv$model), 2)
        )
      )
      
      co <- summary(rv$model)$coefficients
      coefs <- data.frame(
        Variable = rownames(co),
        Estimate = round(co[, 1], 6),
        `Std. Error` = round(co[, 2], 6),
        `t value` = round(co[, 3], 3),
        `Pr(>|t|)` = signif(co[, 4], 3),
        Signif = vapply(co[, 4], sig_label, character(1)),
        row.names = NULL,
        check.names = FALSE
      )
      
      # Interpretación
      interp <- c()
      for (v in vars) {
        beta <- unname(coef(rv$model)[[v]])
        interp <- c(interp, interpret_beta(v, beta))
      }
      interp_txt <- paste(interp, collapse = "\n")
      
      params <- list(
        model_name = rv$model_name,
        formula_txt = paste0(y_var, " ~ ", paste(vars, collapse = " + ")),
        metrics = metrics,
        coefs = coefs,
        interpret = interp_txt,
        has_single = length(vars) == 1,
        single_var = if (length(vars) == 1) vars[1] else NA_character_,
        data = d
      )
      
      out <- render(
        input = rmd_path,
        output_format = "pdf_document",
        params = params,
        envir = new.env(parent = globalenv()),
        quiet = TRUE
      )
      
      file.copy(out, file, overwrite = TRUE)
    }
  )
  
  # ---------------- Predicción ----------------
  observeEvent(input$use_fit_model, {
    # Solo fuerza re-render de inputs si ya existe el modelo
    req(rv$model)
  })
  
  output$pred_inputs_ui <- renderUI({
    req(rv$model)
    vars <- attr(terms(rv$model), "term.labels")
    validate(need(length(vars) >= 1, "Ajusta primero un modelo con al menos 1 variable."))
    
    d <- df()
    
    tagList(
      tags$p(class = "mini-note", paste0("Modelo cargado: ", rv$model_name)),
      lapply(vars, function(v) {
        numericInput(
          inputId = paste0("pred_", v),
          label = v,
          value = median(d[[v]], na.rm = TRUE)
        )
      })
    )
  })
  
  pred_res <- eventReactive(input$pred_btn, {
    req(rv$model)
    vars <- attr(terms(rv$model), "term.labels")
    validate(need(length(vars) >= 1, "El modelo no tiene predictores. Ajusta un modelo con al menos 1 variable."))
    
    # Crear newdata con 1 fila (IMPORTANTE)
    newdata <- as.data.frame(
      setNames(rep(NA_real_, length(vars)), vars)
    )
    
    # Rellenar valores desde inputs
    for (v in vars) {
      val <- input[[paste0("pred_", v)]]
      validate(need(!is.null(val) && is.finite(val), paste("Falta valor para:", v)))
      newdata[1, v] <- val
    }
    
    ic <- predict(rv$model, newdata = newdata, interval = "confidence", level = 0.95)
    pi <- predict(rv$model, newdata = newdata, interval = "prediction", level = 0.95)
    
    list(
      newdata = newdata,
      ic = ic[1, ],
      pi = pi[1, ],
      rmse = rmse_of(rv$model)
    )
  })
  
  output$pred_result_ui <- renderUI({
    req(pred_res())
    pr <- pred_res()
    
    tags$div(
      tags$p(
        tags$b(pr$ic["fit"]), " kWh",
        " (predicción puntual) bajo las condiciones introducidas."
      ),
      tags$ul(
        tags$li(paste0("IC 95% (media esperada): [", round(pr$ic["lwr"], 2), ", ", round(pr$ic["upr"], 2), "] kWh")),
        tags$li(paste0("PI 95% (observación nueva): [", round(pr$pi["lwr"], 2), ", ", round(pr$pi["upr"], 2), "] kWh"))
      ),
      tags$p(tags$b("Error típico del modelo (RMSE): "), paste0(round(pr$rmse, 2), " kWh")),
      tags$h4("Entrada usada"),
      tags$pre(paste(capture.output(print(pr$newdata)), collapse = "\n"))
    )
  })
  
  output$pred_plot_2d_ui <- renderUI({
    req(rv$model)
    vars <- attr(terms(rv$model), "term.labels")
    if (length(vars) != 1) {
      return(tags$p(class = "mini-note", "Este gráfico solo aparece si el modelo tiene 1 predictor (predicción en 2D con punto nuevo)."))
    }
    plotlyOutput("pred_plot_2d", height = 420)
  })
  
  output$pred_plot_2d <- renderPlotly({
    req(rv$model)
    req(pred_res())
    m <- rv$model
    vars <- attr(terms(m), "term.labels")
    req(length(vars) == 1)
    xvar <- vars[1]
    
    d <- df()
    newx <- pred_res()$newdata[[xvar]]
    newy <- unname(pred_res()$ic["fit"])
    
    p <- ggplot(d, aes(x = .data[[xvar]], y = .data[[y_var]])) +
      geom_point(alpha = 0.75) +
      geom_smooth(method = "lm", se = FALSE) +
      geom_point(aes(x = newx, y = newy), color = "red", size = 3) +
      labs(
        x = pretty_name(xvar),
        y = pretty_name(y_var)
      )
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
}

shinyApp(ui, server)




