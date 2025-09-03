
library(shiny)
library(shinydashboard)
library(scales)
library(plotly)
library(ggplot2)
library(dplyr)
library(bslib)
library(bsicons)

# Función para ordenar líneas
ordenar_lineas <- function(lineas) {
  numeros <- as.numeric(gsub("Linea ", "", lineas[grepl("Linea \\d+", lineas)]))
  letras <- lineas[grepl("Linea [A-Z]", lineas)]
  lineas_ordenadas <- c(paste("Linea", sort(numeros)), letras)
  return(lineas_ordenadas)
}

ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "darkly"),
  tags$head(
    tags$link(rel = "icon", href = "logo-metro.png"),
    tags$title("Afluencia Metro"),
    tags$style(HTML("
  html, body {
    height: 100%;
    margin: 0;
    padding: 0;
    overflow-x: hidden;
  }

  .container-fluid {
    height: 100%;
  }

  .shiny-input-container {
    width: 100% !important;
  }

  .plot-container {
    height: 100%;
  }

  #plot {
    height: 70vh !important;
  }

  .value-box {
    min-height: 100px;
  }
"))

  ),

  # 🔼 Panel superior con el selector
  fluidRow(
    column(12,
           selectInput("linea_metro", "Selecciona una línea", 
                       choices = ordenar_lineas(c(paste("Linea", 1:9), "Linea 12", "Linea A", "Linea B")),
                       width = "100%")
    )
  ),
  
  # 📊 Value boxes
  fluidRow(
    column(6, uiOutput("usuarios_totales")),
    column(6, uiOutput("ing_tot"))
  ),
  fluidRow(
    column(6, uiOutput("usu_pago")),
    column(6, uiOutput("usu_grat"))
  ),
  
  # 📈 Gráfica
  fluidRow(
    column(12, plotlyOutput("plot", height = "70vh"))
  )
)


# Server
server <- function(input, output, session) {
  
  df_metro <- read.csv("datos_metro.csv")
  df_agg <- aggregate(afluencia ~ anio + linea + estacion + tipo_pago, df_metro, FUN = sum)
  df_agg <- df_agg[-c(490),]  # Ajuste puntual
  
  df_agg_int <- reactive({
    df_agg %>% filter(linea == input$linea_metro)
  })
  
  output$plot <- renderPlotly({
    p <- ggplot(data = df_agg_int(),
                aes(x = estacion, y = afluencia, fill = tipo_pago)) +
      geom_col(position = "dodge") +
      labs(
        x = "Estación",
        y = "Afluencia",
        fill = "Tipo de Pago",
        title = input$linea_metro
      ) +
      theme(
        plot.title = element_text(face = "bold", size = 20, color = "white"),
        legend.background = element_rect(fill = "#63605f"),
        legend.text = element_text(size = 13, color = "white"),
        legend.title = element_text(size = 16, colour = "white"),
        panel.background = element_rect(fill = "transparent"), 
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1, size = 13, color = "white"),
        axis.text.y = element_text(size = 13, color = "white"),
        axis.title = element_text(size = 17, color = "white", face = "bold"),
        panel.grid.major = element_blank(),
        ppanel.grid.minor = element_blank()
      ) +
      scale_y_continuous(labels = label_number(big.mark = ",")) +
      scale_x_discrete(labels = label_wrap(5)) +
      scale_fill_manual("Tipo de pago", values = c(
        "Boleto" = "#075213",
        "Gratuidad" = "#192039",
        "Prepago" = "#993455"
      ))
    
    ggplotly(p)
  })

  get_linea_theme <- function(linea, color_fg = "white", color_bg = NULL) {
    colores <- list(
      "Linea 1" = "#f04e98",
      "Linea 2" = "#005e98",
      "Linea 3" = "#af9800",
      "Linea 4" = "#6bbbae",
      "Linea 5" = "#e7d417",
      "Linea 6" = "#da291c",
      "Linea 7" = "#e87722",
      "Linea 8" = "#009a44",
      "Linea 9" = "#512f2e",
      "Linea 12" = "#b9975b",
      "Linea A" = "#981e97",
      "Linea B" = "#b1b3b3"
    )
    col <- colores[[linea]]
    if (is.null(col)) col <- "#b1b3b3"
    if (is.null(color_bg)) color_bg <- col
    value_box_theme(bg = color_bg, fg = color_fg)
  }

  output$usuarios_totales <- renderUI({
    value_box(
      title = "Usuarios totales",
      value = formatC(sum(df_agg_int()$afluencia), digits = 0, big.mark = ",", format = "d"),
      showcase = bsicons::bs_icon("person"),
      theme = get_linea_theme(input$linea_metro)
    )
  })

  output$usu_pago <- renderUI({
    value_box(
      title = "Usuarios de Pago",
      value = formatC(sum(df_agg_int()$afluencia[df_agg_int()$tipo_pago != "Gratuidad"]), digits = 0, big.mark = ",", format = "d"),
      showcase = bsicons::bs_icon("person-check"),
      theme = get_linea_theme(input$linea_metro, color_fg = get_linea_theme(input$linea_metro)$bg, color_bg = "white")
    )
  })

  output$ing_tot <- renderUI({
    value_box(
      title = "Ingresos totales",
      value = formatC(sum(df_agg_int()$afluencia[df_agg_int()$tipo_pago != "Gratuidad"]) * 5, digits = 0, big.mark = ",", format = "d"),
      showcase = bsicons::bs_icon("currency-dollar"),
      theme = get_linea_theme(input$linea_metro)
    )
  })

  output$usu_grat <- renderUI({
    value_box(
      title = "Usuarios Gratuidad",
      value = formatC(sum(df_agg_int()$afluencia[df_agg_int()$tipo_pago == "Gratuidad"]), digits = 0, big.mark = ",", format = "d"),
      showcase = bsicons::bs_icon("person-exclamation"),
      theme = get_linea_theme(input$linea_metro, color_fg = get_linea_theme(input$linea_metro)$bg, color_bg = "white")
    )
  })
}

# Run app
shinyApp(ui, server)






