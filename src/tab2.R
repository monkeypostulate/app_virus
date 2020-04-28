tab2<-function(){
  tabPanel(
    p("Simulate outbreak"), # Sidebar with a slider input for the number of bins
    sidebarLayout(
      sidebarPanel(

        h3("Activity and virus"),
        sliderInput (inputId="perc",
                      label = "Active people parameter", value = 0.7, min=0,max=1),
        sliderInput (inputId="prob",
                      label = "Transmission parameter", value = 0.4, min=0,max=.9,step=0.1),
        
        h3("Lockdown"),
        radioButtons("lockdown",
                     label = "Lockdown",
                     choices = list("Yes" ='Yes', "No" = 'No'), selected = 'Yes'),
      conditionalPanel(  condition = "input.lockdown == 'Yes'" ,
        numericInput (inputId="begin_lockdown",
                      label = "Start", value =15, min=0,max=30,step=1),
        uiOutput("end_lockdown_input")
        ),
        
        conditionalPanel(
          condition = "input.go!=0",                 
          actionButton(inputId="go2", "Simulate")
        )
      ),
      
      # Show a plot of the generated distribution
      mainPanel(     
        conditionalPanel("input.go==0",
                         htmlOutput("message2")),
        
        conditionalPanel(
        condition = "input.go2!=0", 
        sliderInput("myslider", "Steps", 
                    min = 1, max = 100, value = 5,
                    animate = animationOptions(),width ="80%"),
        plotOutput("final_outcome",
                   width = "100%", height = "500px"
        )
     
        
      
        )
       
      )
    )
  )
  
}