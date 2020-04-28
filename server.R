

shinyServer(function(input, output) {
#  callModule(tab1, "inner")
  
  
  output$n_g_input<-renderUI({
    possible_n_com<-divisors(input$n_size)[-1]
  selectInput(inputId="n_g", label = "Number of communities",
            choices=possible_n_com, selected=possible_n_com[1] )
    })
 
   
  output$pn_input<-renderUI({
  numericInput (inputId="pn", label = "Random encounter parameter",
                value = 0.5, min=0,max=input$n_size,step = 0.1)
  })
 
  output$end_lockdown_input<-renderUI({
    begin_lockdown<-input$begin_lockdown
  numericInput (inputId="end_lockdown",
                label = "End", value =begin_lockdown+13, min=begin_lockdown+1,max=80,step=1)
  })
  
  output$p_comm_input<-renderUI({
    n_size<-as.numeric(input$n_size)
    n_g<-as.numeric(input$n_g)
    max_val<-n_size/n_g
    current_value<-min(3,round(n_size/n_g))
    numericInput (inputId="p_comm", label = "Community cohesion parameter",
                  value = as.numeric(current_value), min=3,max=max_val)
  })
   
  
  observeEvent(input$go, {
    n_size<-as.numeric(input$n_size)
    n_g<-as.numeric(input$n_g)
    pn<-as.numeric(input$pn)
    p_comm<-as.numeric(input$p_comm)
   gamma_param<<-ifelse(input$gamma==0,NA,1.3*as.numeric(input$gamma))
    prob<-as.numeric(input$prob)
    perc<-as.numeric(input$perc)
 model_parameters<<-list(n_size=n_size,n_g=n_g,pn=pn,
                         p_comm=p_comm,prob=prob,perc=perc,
                         gamma_param=gamma_param)   
 simulated_network<<-generate_network(n_size=n_size,n_g=n_g,
                                      pn=pn,p_comm=p_comm,
                                      gamma_param=gamma_param) 

 

  output$simul_graph <- renderPlot( {

    node_size2<-10*degree(simulated_network$g)/
      max(degree(simulated_network$g))

    plot.igraph(simulated_network$g,
                vertex.label='',
                vertex.size=node_size2,
                layout=simulated_network$ly,
                vertex.color='#97CAEF',
                edge.color='gray80')
  })
  
  })

  
  observeEvent(input$go2 ,   {

    n_size<-as.numeric(input$n_size)
    n_g<-as.numeric(input$n_g)
    comm_size=n_size/n_g
    lockdown<-list()
    lockdown$perc_lockdown<-input$perc*0.01
    lockdown$perc<-input$perc
    prob<-input$prob
    if(input$lockdown=='Yes'){
    lockdown$start<-input$begin_lockdown
    lockdown$end<-input$end_lockdown
    g<-simulate_virus_spread(g=simplify(simulated_network$g),
                            t_time=100,
                            lockdown=lockdown,
                            duration=14,
                            prob=prob)
    }else{
      lockdown$start<-500
      lockdown$end<-500
      lockdown$perc<-  lockdown$perc
      g<-simulate_virus_spread(g=simplify(simulated_network$g),
                              t_time=100,
                              lockdown=lockdown,
                              duration=14,
                              prob=prob)
    }



      output$final_outcome <- renderPlot({
        
        node_size2<-10*degree(g)/
        max(degree(g))
      
      i<-input$myslider
      colors_v<-rep('#65CCB8',length(V(g)))

      col1<-((V(g)$period<=i) & (V(g)$period!=0))
      col3<-(V(g)$period<i-14  & (V(g)$period!=0))
      colors_v[col1]<-'#B23850'
      colors_v[col3]<-'#5086A5'
      plot.igraph(g,
                  vertex.label='',
                  vertex.size=node_size2,
                  layout=simulated_network$ly,
                  edge.color='gray80',
                  vertex.color=colors_v)
    })
    
      
      
      
      
      colfunc <- colorRampPalette(c("#FC4445", "#3FEEE6","#97CAEF"))
      col_groups<-colfunc(n_g)
      
      
      col_vertex<-unlist(lapply(col_groups,rep,comm_size))
      community<-unlist(lapply(1:n_g,rep,comm_size))
      nodes_info <- data.frame(state = V(g)$state,
                               period = V(g)$period,
                               degree=degree(g),community=community)
      
      
      community_info<-nodes_info%>%
        mutate(period=ifelse(period==0,999,period))%>%
        group_by(period, community)%>%
        summarise(total=n())
      community_info<-expand.grid(community=1:n_g, period=c(1:100,999))%>%
        left_join(community_info, by=c('community','period'))
      
      
      community_info<-community_info%>%
        mutate(total=replace_na(total,0))                                
      
      
      community_info<-community_info%>%
        group_by(community)%>%
        arrange(period)%>%
        mutate(ctotal=cumsum(total))
      
      
      
      
      output$virus_evolution <- renderPlotly({
      
      plot_evol<-  community_info%>%
        mutate(Period=period,
               Percentage=round(100*ctotal/comm_size,1),
               Community=factor(community,levels = 1:n_g))%>%
          ggplot()+
          geom_line(aes(x=Period,y=Percentage,col=Community),lwd=1.2)+
          scale_color_manual(values=col_groups,name="Community")+
          theme_bw()+
          xlab('Time')+
          ylab('% of infected people')+
          ylim(0,101)+
          xlim(0,100)
        
      
      ggplotly(plot_evol)
      })
  
      
      
  cols_type<-c('Second Wave'='#E13C20','Lock Down'='#C5CBE3',
               'First Wave'='#4056A4','Healthy'='#65CCB8')
  
  output$when_it_occured <- renderPlotly({
    
    if(input$lockdown=='Yes'){
    start_lockdown<-input$begin_lockdown
    end_lockdown<-input$end_lockdown
 
    community_info<-community_info%>%
      mutate(when_inf=ifelse(
        period<start_lockdown,
        'First Wave',ifelse(period>end_lockdown & period!=999 ,
                            'Second Wave',
                            ifelse(period==999,'Healthy','Lock Down'))
      ))
    }else{
      community_info<-community_info%>%
        mutate(when_inf=ifelse(
          period!=999,
          'First Wave','Healthy'))
    }

        community_info%>%
      group_by(community,when_inf)%>%
      summarise(total=sum(total))%>%
          mutate(Community=factor(community,levels=1:n_g),
                 Percentage=round(100*total/comm_size,1),#Total=round(100*total/c,1),
                 Status=factor(when_inf,
                               levels=c('Healthy',
                                        'Second Wave',
                                        'Lock Down','First Wave')))%>%
    ggplot()+
    geom_bar(aes(x=Community,y=Percentage,
                 fill=Status),
             stat='identity',position=position_dodge())+
          coord_flip()+
          facet_wrap(~ Status)+
    scale_fill_manual(values=cols_type, name='')+
      xlab('Community')+
      ylab('Percentage by Community')+
      ylim(0,100)+
          theme_bw()
  })
 
  }) 
   
  output$message <- renderUI({
    HTML("<h4 style='color:white'>Simulate your first world!! </h4><br/>")
  })
  
  output$message2 <- renderUI({
    HTML("<h4 style='color:white'> Click the button simulate!!</h4><br/>
         <p style='color:white'> You don't see it?  Simulate your world before...</p>")
  })
  output$message3 <- renderUI({
    HTML("<h4 style='color:white'> Click the button simulate!!</h4><br/>
         <p style='color:white'> You don't see it?  Simulate your world before...</p>")
  })
  
  
  
    
    
  })
  

  
