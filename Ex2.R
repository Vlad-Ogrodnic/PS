library(shiny)

ui <- fluidPage(
  titlePanel("Analiza Variabilelor Aleatoare și Transformărilor lor"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("scenariu", "1. Alege Scenariul:",
                  choices = list("Scenariul 1: N(0,1)" = 1,
                                 "Scenariul 2: N(mu, sigma^2)" = 2,
                                 "Scenariul 3: Exp(lambda)" = 3,
                                 "Scenariul 4: Pois(lambda)" = 4,
                                 "Scenariul 5: Binom(r, p)" = 5)),
      
      # Parametri dinamici în funcție de scenariu
      conditionalPanel(condition = "input.scenariu == 2",
                       numericInput("mu", "mu:", 0),
                       numericInput("sigma", "sigma:", 1, min = 0.1)),
      
      conditionalPanel(condition = "input.scenariu == 3 || input.scenariu == 4",
                       numericInput("lambda", "lambda:", 1, min = 0.1)),
      
      conditionalPanel(condition = "input.scenariu == 5",
                       numericInput("r", "r (n-ul binomial):", 10, min = 1),
                       sliderInput("p", "p (probabilitate):", 0, 1, 0.5)),
      
      numericInput("n_fixat", "n (număr de variabile Xi):", 5, min = 1),
      
      hr(),
      
      radioButtons("transf", "2. Alege funcția reprezentată:",
                   choices = list("X" = "x", 
                                  "Transformare Liniară" = "lin", 
                                  "X la putere (X^2 sau X^3)" = "pow", 
                                  "Suma (Sum Xi)" = "sum",
                                  "Suma pătratelor (Sum Xi^2) - doar Sc. 1&2" = "sum_sq"))
    ),
    
    mainPanel(
      plotOutput("distPlot"),
      wellPanel(
        helpText("Graficul reprezintă Funcția de Repartiție (CDF)."),
        textOutput("descriere")
      )
    )
  )
)

server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    set.seed(123)
    N_SIM <- 10000
    n <- input$n_fixat
    
    # Generare date în funcție de scenariu
    data <- switch(input$scenariu,
                   "1" = matrix(rnorm(N_SIM * n, 0, 1), ncol = n),
                   "2" = matrix(rnorm(N_SIM * n, input$mu, input$sigma), ncol = n),
                   "3" = matrix(rexp(N_SIM * n, input$lambda), ncol = n),
                   "4" = matrix(rpois(N_SIM * n, input$lambda), ncol = n),
                   "5" = matrix(rbinom(N_SIM * n, input$r, input$p), ncol = n)
    )
    
    # Aplicare transformare
    valori <- if(input$transf == "x") {
      data[, 1]
    } else if(input$transf == "lin") {
      switch(input$scenariu,
             "1" = 3 + 2 * data[, 1], "2" = 3 + 2 * data[, 1],
             "3" = 2 - 5 * data[, 1], "4" = 3 * data[, 1] + 2,
             "5" = 5 * data[, 1] + 4)
    } else if(input$transf == "pow") {
      if(input$scenariu == 5) data[, 1]^3 else data[, 1]^2
    } else if(input$transf == "sum") {
      rowSums(data)
    } else { # sum_sq
      rowSums(data^2)
    }
    
    # Plotting
    plot(ecdf(valori), main = "Funcția de Repartiție (Empirică)", 
         col = "firebrick", lwd = 2, verticals = TRUE, do.points = FALSE)
    grid()
  })
  
  output$descriere <- renderText({
    paste("Afișezi transformarea pentru scenariul", input$scenariu, 
          "folosind un eșantion de", input$n_fixat, "variabile.")
  })
}

shinyApp(ui = ui, server = server)