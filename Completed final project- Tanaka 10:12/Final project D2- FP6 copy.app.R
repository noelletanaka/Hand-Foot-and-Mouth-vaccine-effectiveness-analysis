library(shiny)
library(deSolve)
library(ggplot2)
library(tidyverse)

#define ui 
ui<- shinyUI(fluidPage(

#application title 
titlePanel("The Effectiveness and Rate of Hand, Foot, and Mouth Disease Vaccine in China"),
("Noelle Tanaka and Kiana Habibi- University of San Diego, Biology 444: Evolution and Ecology of Infecious Diseases"),

# UI Definition
ui <- navbarPage("SWVEIR Model and Analysis of Hand, Foot, Mouth Disease",
                 tabPanel("Welcome!",
                          fluidPage(
                            p(strong("WELCOME"), 
                              align="center", 
                              style = "font-family: 'Times', serif; 
               font-weight: 500; 
               font-size: 100px;  # Increased font size
               text-shadow: 3px 3px 3px 
               line-height: 1; 
               color: #404040;"
                            ),
                            fluidRow(
                              column(12, 
                              p("This research application employs a SWVEIR (Susceptible-Waning-Vaccinated-Exposed-Infectious-Recovered) epidemiological model to comparatively evaluate Hand, Foot, and Mouth Disease (HFMD) vaccine effectiveness in China. Developed for Biology 444: Ecology and Evolution of Infectious Diseases at the University of San Diego, the project—conducted by Noelle Tanaka and Kiana Habibi—utilizes statistical modeling to analyze transmission dynamics, vaccination impact, and potential population-level immunity strategies across different regional contexts across China.", 
                                align="center", 
                                style = "font-family: 'times'; font-size: 16pt"),
                            )
                          )
                 )
                 ),
                 navbarMenu("Introduction",
                            tabPanel("Introduction",
                                     fluidRow(
                                       column(7,
                                              titlePanel("Introduction for the SWEVIR Plot of HFMD"),
                                              mainPanel(
                                                p("Hand, Foot, and Mouth Disease (HFMD) is a widespread public health issue, especially in the Asia-Pacific region, where it primarily affects children under five. While HFMD is often caused by Coxsackievirus A16 and Enterovirus 71 (EV71), infections linked to EV71 are of particular concern due to their potential to cause severe complications like encephalitis and paralysis (Lei et al., 2015). Transmission occurs mainly through direct contact with bodily fluids such as saliva, feces, or respiratory droplets (Centers for Disease Control and Prevention [CDC], n.d.). For most children, HFMD presents as a mild illness with fever and rash, but EV71 can escalate to life-threatening neurological conditions, making it a critical target for prevention and treatment efforts (González et al., 2019).", 
                                                  style = "font-family: 'times'; font-size: 15px; font-s20pt"),
                                                p("In response to the severe outbreaks caused by EV71, China introduced the world’s first EV71 vaccine in 2016, marking a significant step in combating HFMD (Yee & Poh, 2015). Studies have shown that the vaccine has been effective in reducing the number of HFMD cases and complications. However, its relatively short immune response and limited cellular immunity remain challenges to its broader success (Yee & Poh, 2015). This highlights the need to evaluate the vaccine’s performance further and explore improvements that could make it more effective in reducing severe cases of HFMD.", 
                                                  style = "font-family: 'times'; font-size: 15px; font-s20pt"),
                                                p("Understanding EV71's biology and its interaction with the immune system is crucial for developing better prevention strategies. Research has revealed that EV71 evolves rapidly and poses a growing threat to public health (Chang et al., 2018). In China alone, studies have reported a mortality rate of 0.03% and a complication rate of 1.1% among HFMD cases, highlighting the need for effective vaccinations (González et al., 2019).", 
                                                  style = "font-family: 'times'; font-size: 15px; font-s20pt"),
                                                p("This study will focus on evaluating the effectiveness and rate of the EV71 vaccine in reducing severe HFMD cases in China. By understanding how well the vaccine prevents the most dangerous outcomes of the disease, we can provide insights into improving current vaccination programs and guide similar efforts worldwide", 
                                                  style = "font-family: 'times'; font-size: 15px; font-s20pt")
                                              )
                                       ),
                                              tags$img(
                                                class = "img-polaroid",
                                                src = "https://www.cdc.gov/hand-foot-mouth/images/hfmd-childs-foot.jpg",
                                                alt = "Foot, HFMD",
                                                height = 300, width = 500, style="display: block; margin-right: auto;"
                                              ),
                                              tags$small(
                                                p("Figure 1: Photo of a baby with Hand, Foot, Mouth Disease symptoms."),
                                                p("Source: Centers for Disease Control and Prevention. (n.d.). About hand, foot, and mouth disease."), 
                                                p("Centers for Disease Control and Prevention. https://www.cdc.gov/hand-foot-mouth/about/index.html"),
                                              )
                                     
                                     )
                            ),
                            tabPanel("Research Question",
                                     fluidPage(
                                       p(strong("RESEARCH QUESTION"), 
                                         align="center", 
                                         style = "font-family: 'Times', serif; 
               font-weight: 500; 
               font-size: 100px;  # Increased font size
               text-shadow: 3px 3px 3px 
               line-height: 1; 
               color: #404040;"
                                       ),
                                       fluidRow(
                                         column(12, 
                                                p("How does the effectiveness and rate of the EV71 vaccination compare in reducing severe HFMD cases in China, who were the first to create a legalized vaccine?", 
                                                  align="center", 
                                                  style = "font-family: 'times'; font-size: 16pt"),
                                         )
                                       )
                                     )
                            ),
                            tabPanel("Model",
                                     fluidRow(
                                       column(6,
                                              titlePanel("Model and Equations for the SWEVIR Plot of HFMD"),
                                              HTML('<center><img src = "SWVEIRequations.png" height = 300, width = 640></center>'),
                                              tags$small(
                                                "Figure 2: All of the Equations made from the SWVEIR Model.",
                                               p("Source: Equations created by Tanaka, October 2024"),
                                              )
                                       ),
                                       column(3,
                                              HTML('<center><img src = "SWVEIRscreenshot.png" height = 426, width = 640></center>'),
                                              tags$small(
                                                "Figure 3: SWVEIR Flowchart for Hand, Foot, Mouth Disease.",
                                                p("Source: Flowchart created by Tanaka, October 2024"),
                                              )
                                       ),
                                       column(5,
                                              HTML('<center><img src = "equationexplination.png" height = 240, width = 600></center>'),
                                       )
                                     )
                            ),
                            tabPanel("Methods",
                                              fluidPage(
                                                column(6,
                                                       titlePanel("Methods for the SWEVIR Plot of HFMD"),
                                                       HTML('<center><img src = "HFMDmethoddescription.png" height = 500, width = 640></center>'),
                                                ),
                                     column(6,
                                            titlePanel("Table 1: Parameters for the SWEVIR Plot of HFMD"),
                                            HTML('<center><img src = "SWVEIRparameters.png" height = 600, width = 640></center>'),
                                     )
                            )
                            )
                 ),
                
                 tabPanel("SWVEIR Model",
                          fluidPage(
                            titlePanel("HFMD SWVEIR Model"),
                            # Sidebar with slider input for model parameters and initial values
                            sidebarLayout(
                              sidebarPanel(
                                h4("Parameters"),
                                sliderInput("theta_a", "Vaccination Rate (theta_a):", min = 0, max = 0.5, value = 0.017),
                                sliderInput("omega", "Vaccination Effectiveness (omega):", min = 0, max = 0.97, value = 0.9),
                                
                                h4("Display options"),
                                checkboxInput("show_S", "Show Susceptible (S)", TRUE),
                                checkboxInput("show_W", "Show Environmental Factors  (W)", TRUE),
                                checkboxInput("show_V", "Show Vaccinated  (V)", TRUE),
                                checkboxInput("show_E", "Show Exposed (E)", TRUE),
                                checkboxInput("show_I", "Show Infected (I)", TRUE),
                                checkboxInput("show_R", "Show Recovered (R)", TRUE)
                              ),
                              
                              # Show plots 
                              mainPanel(
                                fluidRow(
                                  column(6, plotOutput("SWVEIR_Plot")),
                                  HTML('<center><img src = "SWVEIRresults.png" height = 250, width = 960></center>'),
                                ),
                               
                              )
                            ),
                           
                          )
                 ),
                            tabPanel("Conclusions",
                                     fluidPage(
                                       column(8,
                                              titlePanel("Conclusion for the SWEVIR Plot of HFMD"),
                                              mainPanel(
                                                p("Our analysis highlights the critical role of vaccination rates in controlling the spread and severity of Hand, Foot, and Mouth Disease (HFMD) caused by Enterovirus 71 (EV71). Through our shiny app, we found that vaccination rate had a significantly greater impact on reducing severe HFMD cases than vaccine effectiveness alone.", 
                                                  style = "font-family: 'times'; font-size: 16px; font-s20pt"),
                                                p(strong("Key findings:"), 
                                                  style = "font-family: 'times'; font-size: 16px; font-s20pt"),
                                                p(strong("-"), 
                                                  strong("Higher vaccination rates significantly reduced HFMD exposure, highlighting the importance of widespread vaccine coverage. Scaling up vaccination efforts can greatly control disease spread, even if vaccine effectiveness is moderate."),
                                                  style = "font-family: 'times'; font-size: 16px; font-s20pt"),
                                                p(strong("-"), 
                                                  strong("While vaccine effectiveness played a role in reducing exposure, its impact was less significant compared to vaccination rates. This suggests that expanding access to the vaccine should be a primary public health focus."),
                                                  style = "font-family: 'times'; font-size: 16px; font-s20pt"),
                                                p("These results align with prior studies, such as those by Yee and Poh (2015), which demonstrated the EV71 vaccine's effectiveness but noted challenges like limited immunity duration. Similarly, González et al. (2019) found that regions with robust vaccination programs saw fewer severe cases and deaths during EV71 outbreaks.", 
                                                  style = "font-family: 'times'; font-size: 16px; font-s20pt"),
                                                p("Our findings support the concept of herd immunity, where higher vaccination rates break transmission chains and protect unvaccinated individuals, reducing the overall disease burden. This is particularly crucial for EV71, which can cause severe neurological complications.", 
                                                  style = "font-family: 'times'; font-size: 16px; font-s20pt"),
                                                p("In conclusion, increasing vaccination coverage in China is essential to minimize severe HFMD cases. While improving vaccine effectiveness is valuable, prioritizing access and higher immunization rates will yield the greatest public health benefits", 
                                                  style = "font-family: 'times'; font-size: 16px; font-s20pt")
                                              )
                                       ),
                                       column(4,
                                              tags$img(
                                                class = "img-polaroid",
                                                src = "https://upload.wikimedia.org/wikipedia/commons/thumb/d/d9/Journal.pntd.0003044.g005_cropped.jpg/440px-Journal.pntd.0003044.g005_cropped.jpg",
                                                alt = "Enterovirus A71 capsid"
                                              ),
                                              tags$small(
                                                "Figure 4: Photo of Enterovirus 71.",
                                                p("Source: projects, Contributors to Wikimedia. Wikimedia Commons, Wikimedia Foundation, Inc., 30 Nov. 2024, commons.m.wikimedia.org/wiki/Main_Page.")
                                              )
                                       )
                                     )
                 ),
                 navbarMenu("References and Contributions",
                            tabPanel("References",
                                     column(7,
                                            titlePanel("References for all Information Found in this App"),
                                            HTML('<center><img src = "References.png" height = 600, width = 1400></center>'),
                                     )
                            ),
                            tabPanel("Contributions",
                                     fluidPage(
                                       titlePanel("Contributions for Each Part of the Final Project"),
                                       mainPanel(
                                         p(strong("Final Project 1"),
                                           "Habibi researched and answered questions on one disease. Tanaka researched and answered questions on two diseases.", 
                                           style = "font-family: 'times'; font-s16pt"),
                                         p(strong("Final Project 2"),
                                           "Tanaka initially researched and chose the disease. Tanaka wrote the first, fifth, sixth, and seventh paragraphs. Tanaka also researched and found the primary literature for the proposal. Habibi wrote the second, third, and fourth paragraphs and also found some of the sources for the proposal.", 
                                           style = "font-family: 'times'; font-si16pt"),
                                         p(strong("Final Project 3"),
                                           "Tanaka did questions 1 and 2. Habibi and Tanaka did question 3. Tanaka wrote the last paragraph of question 3.", 
                                           style = "font-family: 'times'; font-si16pt"),
                                         p(strong("Final Project 4"),
                                           "Tanaka did part 1 (R coding). Habibi did part 2 (Table of parameter values from the literature).", 
                                           style = "font-family: 'times'; font-si16pt"),
                                         p(strong("Final Project 5"),
                                           "Tanaka wrote and completed the entire initial R code file for our inital SWEVIR model.", 
                                           style = "font-family: 'times'; font-si16pt"),
                                         p(strong("Final Project 6"),
                                           "Tanaka wrote and completed the shiny app. In addition, Tanaka wrote all of the figure captions and sourced all of the photos. Tanaka also accumulated and condensed the all of the model text and some of the text sections. Habibi accumulated and condensed the rest of the text sections.", 
                                           style = "font-family: 'times'; font-si16pt")
                                       )
                                     )
                            )
                 )
)
)
)

# Server Logic
# Define server logic for SWVEIR
server <- function(input, output) {
  # Define the SWVEIR model function outside of the renderPlot
  SWVEIR <- function(time, states, parameters) {
    with(as.list(c(states, parameters)), {
      dS = -S*(beta_a*W + beta_b*I) - theta_a*S + theta_b*V 
      dV = theta_a*S - theta_b*V - (1-omega)*V*(beta_a*W + beta_b*I)  
      dW = phi*I - kappa*W 
      dE = S*(beta_a*W + beta_b*I) + (1-omega)*V*(beta_a*W + beta_b*I) - mu*E 
      dI = mu*E - gamma*I 
      dR = gamma*I 
      
      return(list(c(dS, dV, dW, dE, dI, dR)))
    })
  }
  
  
  
  # Render plot
  output$SWVEIR_Plot <- renderPlot({
    # Define time sequence for simulation
    h <- 0.01
    times <- seq(0, 20, by = h)
    
    # Human and mosquito initial values
    N <- 149.11  # Total human population density per km in china 
    E0 <- N * 0.11/1000  # Starting with one exposed human per thousand people
    I0 <- 0  # No initial infections
    R0 <- 0  # No initial recoveries
    W0 <- 0  # Initial environment 
    V0 <- 0  # No initial vaccination
    S0 <- N - E0 - I0 - R0 - W0 - V0  # Remaining population is susceptible
    
    initial.values <- c(S = S0, V = V0, W = W0, E = E0, I = I0, R = R0)       
    
    parameters<-c(
      beta_a = 2.92e-12,  # Transmission from environment rate
      beta_b = 1.5714,    # Transmission from direct contact rate
      theta_a = input$theta_a,  # Vaccination rate
      omega = input$omega,          #vaccine effectiveness
      theta_b = 0.0143,   # Waning immunity rate
      phi = 1,            # Infected individuals infecting the environment rate
      kappa = 0.9,        # Environmental decay rate of HFMD
      mu = 0.2,          # Incubation rate
      gamma = 0.07143    # Recovery rate      
    )
    
    # Solve the ODE system
    out <- as.data.frame(ode(
      func = SWVEIR, 
      y = initial.values, 
      parms = parameters, 
      times = times
    ))
    
    # Convert output to long format for ggplot
    out_long <- pivot_longer(out, cols = c(S, W, V, E, I, R), names_to = "State", values_to = "Fraction")
    
    # Filter by checkbox selections
    out_long <- out_long %>% 
      filter((State == "S" & input$show_S) |
               (State == "W" & input$show_W) |
               (State == "V" & input$show_V) |
               (State == "E" & input$show_E) |
               (State == "I" & input$show_I) |
               (State == "R" & input$show_R))
    
    # Plot the population
    ggplot(out_long, aes(x = time, y = Fraction, color = State)) +
      geom_line(size = 1) +
      scale_color_manual(values = c(
        "S" = "orchid", 
        "V" = "aquamarine2", 
        "W" = "purple4", 
        "E" = "forestgreen", 
        "I" = "orange", 
        "R" = "blue"
      )) +
      theme_classic() +
      labs(x = "Time (Days)", 
           y = "Population Density (people per km²)", 
           color = "State")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)