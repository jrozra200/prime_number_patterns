library(plotly)

plotting_periodic_curves <- function(number_of_curves = 10, max_x = 10){
        dat <- data.frame(x = seq(0, max_x, 0.001))
        
        for(i in 1:number_of_curves){
                col_nam <- paste("y", i, sep = "")
                
                dat[col_nam] <- 0
                
                counter <- 1
                
                endline <- if((max_x %% i) == 0){
                        max_x / i
                } else {
                        as.integer(max_x / i) + 1
                }
                
                for(j in 1:endline){
                        ending_here <- if((j * i) > max_x){
                                max_x - ((j - 1) * i)
                        } else {
                                i
                        }
                        
                        for(k in seq(0, ending_here, 0.001)) {
                                if((j %% 2) != 0) {
                                        dat[counter, col_nam] <- sqrt(-k^2 + (i * k))
                                } else {
                                        dat[counter, col_nam] <- -sqrt(-k^2 + (i * k))
                                }
                                counter <- counter + 1
                        }
                        counter <- counter - 1
                }
        }
        
        m <- list(l = 50, r = 50, b = 100, t = 100, pad = 4)
        
        form <- paste('plot_ly(dat, x = ~x, y = ~y1, type = "scatter", mode = ',
                      '"lines", color = I("black"), name = "n = 1")', sep = '')
        
        for(z in 2:number_of_curves){
                tmp <- paste(' %>% add_trace(y = ~y', z, ', mode = "lines", co',
                             'lor = I("black"), name = "n = ', z, '")', 
                             sep = '')
                
                form <- paste(form, tmp, sep = "")
        }
        
        form <- paste(form, ' %>% layout(title = "Prime Number Patterns", yaxi',
                      's = list(title = "", range = c(-', max_x, ', ', max_x, 
                      ')), xaxis = list(title = ""), showlegend = FALSE, autos',
                      'ize = F, width = 500, height = 500, margin = m)', 
                      sep = "")
        
        p <- parse(text = form)
        
        eval(p)
}
