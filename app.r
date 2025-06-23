library(shiny)
library(ggplot2)
library(truncnorm)

# --- Interfata Utilizator (UI) ---
ui <- fluidPage(
  titlePanel("Simulator Interactiv de Stocuri si Riscuri"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Parametri Simulare"),
      
      selectInput("produs_selectat", "Alege Produsul de Simulat:",
                  choices = c("Produs cu vanzare rapida (Poisson)" = "poisson",
                              "Produs cu cerere variabila (Normal Trunchiat)" = "truncnorm")),
      
      # Slidere care apar conditionat
      conditionalPanel(
        condition = "input.produs_selectat == 'poisson'",
        sliderInput("lambda", "Cererea Medie (λ):", min = 5, max = 50, value = 20, step = 1)
      ),
      
      conditionalPanel(
        condition = "input.produs_selectat == 'truncnorm'",
        sliderInput("medie_norm", "Cererea Medie (μ):", min = 5, max = 50, value = 15),
        sliderInput("sd_norm", "Variabilitatea Cererii (σ):", min = 1, max = 20, value = 5)
      ),
      
      hr(), # Linie de separare
      
      sliderInput("prag_risc", "Prag de Risc (Nivel Stoc):", min = 0, max = 100, value = 25),
      
      numericInput("num_sim", "Numar de Zile Simulate:", value = 1000, min = 100, max = 10000)
    ),
    
    mainPanel(
      plotOutput("distPlot"),
      hr(),
      h4("Analiza Riscului"),
      verbatimTextOutput("riskAnalysis")
    )
  )
)

# --- Logica Server ---
server <- function(input, output) {
  
  # Functie reactiva pentru a genera datele pe baza input-urilor
  simulated_data <- reactive({
    if (input$produs_selectat == "poisson") {
      rpois(input$num_sim, lambda = input$lambda)
    } else {
      round(rtruncnorm(input$num_sim, a = 0, b = Inf, mean = input$medie_norm, sd = input$sd_norm))
    }
  })
  
  # Genereaza graficul
  output$distPlot <- renderPlot({
    data <- simulated_data()
    
    ggplot(data.frame(Cerere = data), aes(x = Cerere)) +
      geom_histogram(aes(y = ..density..), binwidth = 1, fill = "#0072B2", color = "white", alpha = 0.7) +
      geom_density(color = "red", size = 1) +
      geom_vline(xintercept = input$prag_risc, color = "orange", linetype = "dashed", size = 1.5) +
      annotate("text", x = input$prag_risc, y = 0, label = " Prag Risc", hjust = -0.1, color = "orange", size = 5) +
      labs(title = "Distributia Simulata a Cererii Zilnice",
           x = "Cerere Zilnica",
           y = "Densitate") +
      theme_minimal(base_size = 15)
  })
  
  # Genereaza textul de analiza a riscului
  output$riskAnalysis <- renderText({
    data <- simulated_data()
    prob_depasire <- mean(data > input$prag_risc)
    
    paste(
      "Sumar pentru parametrii selectati:\n",
      "-------------------------------------\n",
      "Cerere medie simulata:", round(mean(data), 2), "\n",
      "Deviatie standard simulata:", round(sd(data), 2), "\n",
      "Cerere maxima simulata:", max(data), "\n\n",
      "Analiza Pragului de Risc (Stoc =", input$prag_risc, "):\n",
      "-------------------------------------\n",
      "Probabilitatea ca cererea sa depaseasca stocul este:", scales::percent(prob_depasire, accuracy = 0.1), "\n",
      "(Acesta este riscul de a epuiza stocul intr-o zi oarecare)"
    )
  })
}

# Ruleaza aplicatia
shinyApp(ui = ui, server = server) # Comenteaza aceasta linie cand nu rulezi direct fisierul