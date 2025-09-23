df_linreg
if (!require(corrplot)) {install.packages("corrplot"); library(corrplot)}


df_linreg[names(df_linreg)]  |> lapply( typeof)


a<-df_linreg |>
  select(altitude,slope_weighted,id)|>
  distinct() |> 
  group_by(altitude = round(altitude, -1))|> 
  reframe(slope_weighted = mean(slope_weighted))

a <- a|>add_column(id = seq(1,length(a$altitude)))


mean_slope_w<-mean(df_linreg$slope_weighted)


#Visualize slope for one station
ggplotly(df_linreg|>select(id,time,var,altitude, slope)|>filter(id == 39)|> mutate(time =as.Date(time))|>
           ggplot(aes(x=time, y = var)) + 
           geom_line()+
           geom_point(col = "cyan", size = 0.5) + 
           geom_smooth(method = "lm", se = FALSE, colour = "red", linewidth = 1, na.rm = TRUE)+
           scale_x_date(date_breaks = "10 years", date_labels = "%b %Y") + theme_classic()
         )

typeof(df_linreg$date[1])



#change df_linreg to a to see rounded altitudes (tried to minimize the error of many low altitude stations)
ggplotly( df_linreg |>
            select(altitude,slope_weighted,id,nonNacount)|>
            filter( nonNacount >= 800 ) |> distinct() |>
            group_by(altitude = round(altitude, -1))|> 
            reframe(slope_weighted = mean(slope_weighted), id = paste0(id," "))|> 
            
            ggplot() + 
            geom_point(aes(x = altitude , y = slope_weighted, col = id))+
            theme_dark()+
            geom_smooth(aes(x = altitude , y = slope_weighted, col = id),method = "lm",se = F , na.rm = T, col = "red")+ 
            geom_line(aes(altitude,mean_slope_w), col = "green")

            ) 


mean_slope<-mean(df_linreg$slope)

ggplotly( df_linreg |>
            select(altitude,slope,id,nonNacount)|>
            filter( nonNacount >= 800 ) |> distinct() |>
            group_by(altitude = round(altitude, -1))|> 
            reframe(slope = mean(slope), id = paste0(id," "))|> 
            
            ggplot() + 
            geom_point(aes(x = altitude , y = slope, col = id))+
            theme_dark()+
            geom_smooth(aes(x = altitude , y = slope, col = id),method = "lm",se = F , na.rm = T, col = "red")+ 
            geom_line(aes(altitude,mean_slope), col = "green")+
            geom_smooth(data = df_linreg|>select(altitude,slope,time_range_years)|>filter(time_range_years>= 50) , aes(x = altitude , y = slope),method = "lm",se = F , na.rm = T, col = "blue")
          
          
          
) 



# slider for noNacount so we can check if the noNacounts make a difference



#########################

ui <- fluidPage(
  sliderInput("nonNacount_slider", "Min nonNacount", 
              min = min(df_linreg$nonNacount, na.rm = TRUE),
              max = max(df_linreg$nonNacount, na.rm = TRUE),
              value = 800, step = 50),
  plotlyOutput("myplot")
)

server <- function(input, output, session) {
  output$myplot <- renderPlotly({
    ggplotly(df_linreg %>%
      select(altitude, slope_weighted, id, nonNacount) %>%
      filter(nonNacount >= input$nonNacount_slider) %>%
      distinct() %>%
      
      ggplot(aes(x = altitude, y = slope_weighted, col = id)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, col = "red") +
      theme_classic() )
      
  })
}

shinyApp(ui = ui, server = server)

#########################




non_na_count <- df_heatwave |> group_by(station) |> reframe(nonNacount = sum(!is.na(var))) |> as.data.frame()

parameters
location_server <- paste0("station_ids=", metadata$stations$id|>head(30) , collapse = "&")
link <- paste0("https://dataset.api.hub.geosphere.at/v1/station/historical/synop-v1-1h?parameters=T&start=1972-01-01&end=2025-09-17&",location_server,"&output_format=json")

df <- fromJSON(link)

#NonNacount Graphs sorted by altitude and the nonNacount
ggplotly(df |>distinct()|> mutate(name = reorder(name, nonNacount)) |> ggplot()+ 
           geom_tile(aes(x= time, y= name , fill = var))+
           scale_fill_gradientn(colors = hcl.colors(20, "ag_Sunset")))

ggplotly(df_linreg |>distinct()|> mutate(name = reorder(name, altitude))|> filter( nonNacount >= 800 )|> ggplot()+ 
           geom_tile(aes(x= time, y= name , fill = var))+
           scale_fill_gradientn(colors = hcl.colors(20, "purple-blue")))



# Abhängigkeit nonNacount vs Altitude
ggplotly(df_linreg |>select(altitude,nonNacount,id)|>distinct()|> ggplot()+ 
           geom_point(aes( x= altitude, y= nonNacount , fill = factor(id))) + geom_smooth(aes( x= altitude, y= nonNacount), method = "lm", se = F, col = "red"))





