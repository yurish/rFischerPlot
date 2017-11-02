require(splines)
require(plotly)
graph.step = 1

fischer_plot<-function(data = c(1, 0.5, 1.5, 1)){
    points<-data.frame(x=double(), y=double(), type=character(), stringsAsFactors = F)
    points<-rbind(data.frame(x = 0, y = 0, type = "sub"))
    str(points)
    mean_depth<-mean(data)
    x<-0
    y<-0
    for(depth in data){
        x <- x + graph.step
        y <- y - mean_depth
        points<-rbind(points, data.frame(x = x, y = y, type = "main"))
        y <- y + depth
        points<-rbind(points, data.frame(x = x, y = y, type = "sub"))
    }
    print(points)
    main_points<- points[points$type == "main",]
    
    sp_data<-points[points$type == "sub",]
    sp_line<-predict(interpSpline(sp_data$x, sp_data$y))
    str(sp_line)
    x<-list(title = "Cikliti")
    y<-list(title = "Ciklitu biezums, m")
    p<-plot_ly(points, x = ~x, y = ~y, type = 'scatter', mode = 'lines', name = "Ciklitu likne") %>%
        add_trace(x = sp_line$x, y = sp_line$y, mode = 'lines', name = "Cikliskuma izmainu likne") %>%
        add_trace(x = main_points$x, y = main_points$y, mode='markers', name = "Cikliti")%>%
        layout(xaxis = x, yaxis = y)
    p
}