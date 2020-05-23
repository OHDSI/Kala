
server = function(input, output, session){
    
    
    output$plot = renderPlotly({
        get_fct = 
            function(x){
                if(x <= 30){
                    ymd
                }else if(x > 30){
                    yearmonth
                }
            }
        
        
        collapsing_function = get_fct(x = as.numeric(input$dr[2] - input$dr[1]))
        
        collapsing_function = get_fct(x = as.numeric(input$dr[2] - input$dr[1]))
        
        df = tibble(date = seq(input$dr[1], input$dr[2], by = "days"), value = rnorm(as.numeric(input$dr[2] - input$dr[1]) + 1), key = 1:(as.numeric(input$dr[2] - input$dr[1]) + 1)) #dateRangeInput generates data - think of it as if filtering SQL database using input$dr
        df %>% 
            as_tsibble(index = date, key = key) %>% 
            index_by(Period = collapsing_function(date)) %>% 
            summarise(value = sum(value)) %>% 
            plot_ly(x = ~Period, y = ~value, type = "bar")
    })
}

