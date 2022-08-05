library(shiny)
library(tidymodels)
library(ggplot2)

##### SERVER #####

alfa1_model <- readRDS("data/alfa1_model.rds")
alfa2_model  <- readRDS("data/alfa2_model.rds")
n1_model  <- readRDS("data/n1_model.rds")
n2_model  <- readRDS("data/n2_model.rds")
ths_model  <- readRDS("data/ths_model.rds")
thr_model  <- readRDS("data/thr_model.rds")
w2_model  <- readRDS("data/w2_model.rds")


calcularValores <- function(input) {
  # Leemos valores
  arena <- input$sd
  arcilla <- input$cl
  om <- input$om
  bd <- input$bd
  limo <- 100 - arena - arcilla
  
  # Computamos valores
  data.frame(
    thr = calcularThr(arena, arcilla, om, limo, bd),
    ths = calcularThs(arena, arcilla, om, limo, bd),
    alfa = calcularAlfa(arena, arcilla, om, limo, bd),
    alfa2 = calcularAlfa2(arena, arcilla, om, limo, bd),
    n = calcularN(arena, arcilla, om, limo, bd),
    n2 = calcularN2(arena, arcilla, om, limo, bd),
    w2 = calcularW2(arena, arcilla, om, limo, bd)
  )
}

calcularThr <- function(arena, arcilla, om, limo, bd) {
  return(as.numeric(predict(
    thr_model,
    new_data = data.frame(
      BD = bd,
      Sand = arena,
      Silt = limo,
      Clay = arcilla,
      OM = om
    )
  )))
}

calcularThs <-  function(arena, arcilla, om, limo, bd) {
  return(as.numeric(predict(
    ths_model,
    new_data = data.frame(
      BD = bd,
      Sand = arena,
      Silt = limo,
      Clay = arcilla,
      OM = om
    )
  )))
}


calcularAlfa <-  function(arena, arcilla, om, limo, bd) {
  return(as.numeric(predict(
    alfa1_model,
    new_data = data.frame(
      BD = bd,
      Sand = arena,
      Silt = limo,
      Clay = arcilla,
      OM = om
    )
  )))
}

calcularAlfa2 <- function(arena, arcilla, om, limo, bd) {
  return(as.numeric(predict(
    alfa2_model,
    new_data = data.frame(
      BD = bd,
      Sand = arena,
      Silt = limo,
      Clay = arcilla,
      OM = om
    )
  )))
}

calcularN <- function(arena, arcilla, om, limo, bd) {
  return(as.numeric(predict(
    n1_model,
    new_data = data.frame(
      BD = bd,
      Sand = arena,
      Silt = limo,
      Clay = arcilla,
      OM = om
    )
  )))
}


calcularN2 <- function(arena, arcilla, om, limo, bd) {
  return(as.numeric(predict(
    n2_model,
    new_data = data.frame(
      BD = bd,
      Sand = arena,
      Silt = limo,
      Clay = arcilla,
      OM = om
    )
  )))
}


calcularW2 <- function(arena, arcilla, om, limo, bd) {
  return(as.numeric(predict(
    w2_model,
    new_data = data.frame(
      BD = bd,
      Sand = arena,
      Silt = limo,
      Clay = arcilla,
      OM = om
    )
  )))
}


calcularVg <- function(x, thr, ths, alfa, alfa2, n, n2, w2) {
  return(((1 - w2) * (1 + (alfa * x) ^ n) ^ (-(1 - (
    1 / n
  )))) +
    (w2 * (1 + (alfa2 * x) ^ n2) ^ (-(1 - (
      1 / n2
    ))))) *
    (ths - thr) + thr
}

obtenerDataFrame <- function(thr, ths, alfa, alfa2, n, n2, w2) {
  x <- c()
  y <- c()
  xValues = c(1:10 %o% 10 ^ (0:6))
  met <- rep("Lineal Model", 70)
  for (i in xValues) {
    currentX <- i
    currentY <-
      calcularVg(currentX, thr, ths, alfa, alfa2, n, n2, w2)
    x <- c(x, currentX)
    y <- c(y, currentY)
  }
  frame_ml = data.frame(x, y, met)
}

#Función
server <- function(input, output) {
  ### Hago que el slider de arcilla sea condicional al de arena
  output$cl <- renderUI({
    sliderInput(
      "cl",
      "Clay (%)",
      min = 0,
      max = 100 - input$sd,
      value = 33
    )
  })
  
  output$capacidad_campo <- renderText({
    valores = calcularValores(input)
    paste(
      "Field capacity:",
      round(
        calcularVg(
          33,
          valores$thr,
          valores$ths,
          valores$alfa,
          valores$alfa2,
          valores$n,
          valores$n2,
          valores$w2
        ),
        digits = 2
      ))
  })
  output$punto_marchitez <- renderText({
    valores = calcularValores(input)
    paste(
      "Permanent wilting point:",
      round(
        calcularVg(
          1500,
          valores$thr,
          valores$ths,
          valores$alfa,
          valores$alfa2,
          valores$n,
          valores$n2,
          valores$w2
        ),
        digits = 2
      ))
  })
  
  output$table <- renderTable({
    calcularValores(input)
  })
  
  output$mapa <- renderPlot({
    valores = calcularValores(input)
    
    # Creamos data frame
    frame = obtenerDataFrame(
      valores$thr,
      valores$ths,
      valores$alfa,
      valores$alfa2,
      valores$n,
      valores$n2,
      valores$w2
    )
    # Ploteamos
    ggplot(data = frame, aes(x = x, y = y)) +
      geom_line(aes(colour = as.factor(met)), size = 1, show.legend = FALSE) +
      scale_x_log10() +
      ylab(expression(theta)) +
      xlab("log(cm)") +
      ylim(0, 1) +
      geom_vline(xintercept = 33) +
      geom_vline(xintercept = 1500) +
      geom_text(
        aes(x = 33, label = "\nField capacity", y = 0.75),
        colour = "blue",
        angle = 90,
        text = element_text(size = 8)
      ) +
      geom_text(
        aes(x = 1500, label = "\nPermantent wilting point", y = 0.75),
        colour = "blue",
        angle = 90,
        text = element_text(size = 8) +
      theme(legend.position="none")
      )
  })
  
}


##### UI #####

ui <- shinyUI(fluidPage(
  titlePanel("Soil water retention curve - bimodal parametric PTF"),
  
  options(shiny.sanitize.errors = TRUE),
  
  #tags$style(type="text/css",
  #  ".shiny-output-error { visibility: hidden; }",
  # ".shiny-output-error:before { visibility: hidden; }"
  #),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        "sd",
        "Sand (%): ",
        min = 20,
        max = 100,
        value = 33,
        step = 1
      ),
      #arena
      uiOutput("cl"),
      #arcilla
      sliderInput(
        'om',
        'Organic Matter (%)',
        min = 0,
        max = 12,
        value = 2,
        step = 0.1
      ),
      sliderInput(
        'bd',
        'Bulk Density (g/cm3)',
        min = 0.9,
        max = 1.75,
        value = 1.2,
        step = 0.1
      ),
      hr(),
      p(
        'Work-in-progress, Sara Acevedo - thesis',
        a("email",
          href = "mailto:seaceved@uc.cl")
      ),
      hr()
    ),
    
    mainPanel(
      p('Field capacity'),
      textOutput("capacidad_campo"),
      hr(),
      p('Permantent wilting point'),
      textOutput("punto_marchitez"),
      hr(),
      plotOutput("mapa", width = 400, height = 400),
      hr(),
      tableOutput("table")
    )
  )
))

##### Run #####
shinyApp(ui = ui, server = server)
