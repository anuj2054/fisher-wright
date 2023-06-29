# Load required libraries
library(shiny)
library(ggplot2)

# Fisher-Wright model simulation function
fisher_wright_simulation <- function(population_size, mutation_rate, selection_coefficient, simulation_duration) {
  # Initialize population with a single allele
  population <- c(rep(0, population_size - 1), 1)
  allele_frequencies <- c(1 / population_size)
  
  # Perform simulation
  for (generation in 1:simulation_duration) {
    # Calculate fitness values
    fitness <- 1 + selection_coefficient * population
    
    # Perform selection
    selected_alleles <- sample(population, size = population_size, replace = TRUE, prob = fitness)
    
    # Perform mutation
    mutated_alleles <- ifelse(runif(population_size) < mutation_rate, 1 - selected_alleles, selected_alleles)
    
    # Update population and allele frequencies
    population <- mutated_alleles
    allele_frequencies <- c(allele_frequencies, sum(population) / population_size)
  }
  
  return(allele_frequencies)
}

# Define UI for Shiny app
ui <- fluidPage(
  titlePanel("Fisher-Wright Model Simulation"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("population_size", "Population Size", value = 1000, min = 1),
      numericInput("mutation_rate", "Mutation Rate", value = 0.001, min = 0, max = 1),
      numericInput("selection_coefficient", "Selection Coefficient", value = 0.1, min = 0),
      numericInput("simulation_duration", "Simulation Duration", value = 100, min = 1),
      actionButton("run_simulation", "Run Simulation")
    ),
    
    mainPanel(
      plotOutput("simulation_plot")
    )
  )
)

# Define server logic for Shiny app
server <- function(input, output) {
  observeEvent(input$run_simulation, {
    # Perform Fisher-Wright model simulation
    allele_frequencies <- fisher_wright_simulation(
      population_size = input$population_size,
      mutation_rate = input$mutation_rate,
      selection_coefficient = input$selection_coefficient,
      simulation_duration = input$simulation_duration
    )
    
    # Plot simulation results
    output$simulation_plot <- renderPlot({
      generation <- seq_along(allele_frequencies)
      data <- data.frame(generation, allele_frequencies)
      
      ggplot(data, aes(x = generation, y = allele_frequencies)) +
        geom_line() +
        xlab("Generation") +
        ylab("Allele Frequency") +
        ggtitle("Fisher-Wright Model Simulation")
    })
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
