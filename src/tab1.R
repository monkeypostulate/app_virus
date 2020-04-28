tab1<-function(){
  tabPanel(
  p("Build World"), # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      h3("Build World"),
      sliderInput (inputId="n_size", label = "Number of people", 
                    value = 473, min=50,max=500),
      uiOutput("n_g_input"),
      uiOutput("pn_input"),
      uiOutput("p_comm_input"),
      sliderInput (inputId="gamma", label = "Superhubs parameters",
                    value = 0.00, min=0,max=1,step=.01),
      actionButton(inputId="go", "Generate world")
    ),
    # Show a plot of the generated distribution
    mainPanel(
      conditionalPanel("input.go==0",
      htmlOutput("message")),
      plotOutput("simul_graph",
                 width = "100%", height = "500px"
      )
    )
  )
)
  
}