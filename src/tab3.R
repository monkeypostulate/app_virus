tab3<-function(){
  tabPanel(
    p("Results"), # Sidebar with a slider input for the number of bins
 
      
      # Show a plot of the generated distribution
      mainPanel(    
        conditionalPanel("input.go==0",
                         htmlOutput("message3")),
        conditionalPanel(
        condition = "input.go2!=0", 
        plotlyOutput("virus_evolution",
                     width = "100%", height = "500px"
        ),
        br(),
        
        plotlyOutput("when_it_occured",
                     width = "100%", height = "500px"
        )
      )
      
      )
    )
  
  
}