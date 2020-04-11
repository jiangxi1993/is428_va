#Package
packages = c('sf', 'tmap', 'tidyverse','ggplot2','pastecs','data.table','devtools','reshape2','viridis','shiny','shinydashboard','plotly','GGally','lubridate','dplyr','readr', 'gganimate','scales', 'animation','stringr','gapminder','png','gifski')





for (p in packages){
    if(!require(p, character.only = T)){
        install.packages(p)
    }
    library(p,character.only = T)
}


detach("package:dplyr")
library(dplyr)


#Preparation for data map box 1



SG_2014_planningarea <- st_read(dsn = "./../Data/GEO", layer = "MP14_PLNG_AREA_WEB_PL")


population_data_10_19<-read_csv("./../Data/Residential_Planning/respopagesextod2011to2019.csv")


young=c("0_to_4","10_to_14","15_to_19","20_to_24")
active=c("25_to_29","30_to_34","35_to_39","40_to_44","45_to_49","5_to_9","50_to_54","55_to_59","60_to_64")
aged=c("65_to_69","70_to_74","75_to_79","80_to_84","85_and_over")

agg <- with(population_data_10_19, aggregate(Pop, by = list(Time = Time, AG  = AG ,Sex= Sex ), FUN = sum))

out <- reshape(agg, timevar = "AG", idvar = c("Time","Sex"), direction = "wide")


col_order <- c("Time" ,"Sex" ,"x.0_to_4" , "x.5_to_9", "x.10_to_14","x.15_to_19","x.20_to_24","x.25_to_29","x.30_to_34",    "x.35_to_39" , "x.40_to_44" ,"x.45_to_49","x.50_to_54" ,"x.55_to_59" ,"x.60_to_64", "x.65_to_69","x.70_to_74" , "x.75_to_79"    ,"x.80_to_84","x.85_to_89","x.90_and_over")

pop_reordered_10_19<- out[, col_order]


popdf_age=mutate(population_data_10_19 ,AgeGroup=ifelse(population_data_10_19$AG %in% young,"Young",ifelse(population_data_10_19$AG%in% active,"Economy Active","Aged"))) %>%select(-AG)%>%arrange(Time) %>% mutate(AgeGroup=factor(AgeGroup))

agg_pop <- with(popdf_age ,aggregate(Pop, by = list(Time = Time,PA=PA,AgeGroup=AgeGroup ), FUN = sum))

out_pop <- reshape(agg_pop, timevar = "AgeGroup", idvar = c("Time","PA"), direction = "wide")



out_pop <-out_pop %>% mutate(Total_pop = select(., 3:5) %>% rowSums()) 
out_pop <-out_pop %>% mutate(Young_ratio=x.Young/Total_pop)
out_pop <-out_pop %>% mutate(Old_ratio = x.Aged/Total_pop) 
out_pop_10_19 <-out_pop %>% mutate(Active_ratio=1-Old_ratio-Young_ratio)

out_pop_10_19[[2]] <- toupper(out_pop_10_19[[2]])

out_pop_10_19[[1]] <- year(mdy((out_pop_10_19[[1]])))
view(out_pop_10_19)

SG_2014_planningarea_pop <- left_join(SG_2014_planningarea, out_pop_10_19, by = c("PLN_AREA_N" = "PA"))

view(SG_2014_planningarea_pop )


# Preparation for bar chart top 5 BOX 2
df_box_2 <- out_pop_10_19

setDT(df_box_2)
df_box_2 <- df_box_2[order(-Total_pop), .SD[1:5], Time]

view(df_box_2)

# Preparation for bar chart tail 5 BOX 3
df_box_3 <- out_pop_10_19[out_pop_10_19$Total_pop>1,]

setDT(df_box_2)
df_box_3 <- df_box_3[order(Total_pop), .SD[1:5], Time]

view(df_box_3)



# Preparation for bar chart tail 5 BOX 4

agg_year_total <- out_pop_10_19[, sum(Total_pop), by = Time]
agg_year_total=rep("Total",nrow(agg_year_total))
names(agg_year_total)[2] <- "Population"

view(agg_year_total)

#df_region=SG_2014_planningarea_pop %>% st_drop_geometry()

#view(df_region)
#class(df_region)


#df_by_region_pop_agg <- with(df_region, aggregate(Total_pop, by = list(Time = Time, REGION_N  = REGION_N  ), FUN = sum))

#view(df_by_region_pop_agg)

#names(df_by_region_pop_agg)[3] <- "Population"



#view(Line_df5)

#UI Section


tmap_mode("plot")
ui <- dashboardPage(skin = "green",
                    
                    #---------------------------- dashboard header ----------------------------                                    
                    header <- dashboardHeader(title = "Precision Policy And Planning",
                                              titleWidth = 300),
                    
                    
                    #--------------------------- dashboard sidebar ----------------------------
                    sidebar <-dashboardSidebar(
                        sidebarMenu(
                            menuItem(text = "Population",
                                     tabName = "Population",
                                     icon = icon("home")
                            ),
                            menuItem(text = "Dashboard 1",
                                     tabName = "Dashboard 1",
                                     icon = icon("dice-one")
                            ),
                            menuItem(text = "Dashboard 2",
                                     tabName = "Dashboard 2",
                                     icon = icon("dice-two")
                            )
                        )
                    ),
                    
                    #--------------------------- dashboard sidebar ----------------------------
                    body <- dashboardBody(
                        tabItems(
                            tabItem(tabName = "Population",
                                    column(7,height=550,box(width=NULL,title="Singapore population distribution by Planning Area",
                                
                                        tmapOutput("pop_pa_map",height = 550))
                                        
                                        ,
                                        box(width=NULL,title="Total Singapore Population by Region"
                                           , plotOutput("pop_pa_top_reg", height = 300)
                                             
                                        )
                                        
                                        )
                                       ,
                                    
                                    column(5,
                                        
                                        box(width=NULL,title="Contro Panel", sliderInput("slider", "Year :", min = 2011, max=2019,8)
                                             
                                        )
                                        ,box(width=NULL,title="Planning Area Population Top 5",plotOutput("pop_pa_top", height = 300)
                                             
                                             
                                        )
                                        ,box(width=NULL,title="Planning Area Population Bottom 5",plotOutput("pop_pa_top2", height = 300)
                                             
                                             
                                        
                                        
                                    )
                                        
                                        
                                        
                                        )
                                    
                                    
                                    
                            ),
                            
                            
                            tabItem(tabName = "Dashboard 1",
                                    h2("Widgets tab content")
                            )
                        
                        
                        
                        
                    )
                    )
                    
)


server <- function(input, output, session) {
     
 
    
    output$pop_pa_map <- renderTmap({
        tmap_mode("plot")
        tm_shape(SG_2014_planningarea_pop[SG_2014_planningarea_pop$Time==2011,])+tm_fill('Total_pop',style = "equal",palette="Greens", 
                     legend.hist = TRUE, 
                     legend.is.portrait = TRUE,
                     legend.hist.z = 0.2)+ 
            tm_compass(type="arrow", size = 2) +
            tm_scale_bar(width = 0.45) +
            tm_grid() +tm_text("PLN_AREA_C", size =0.7)+ tm_borders(alpha = 0.6)+ tm_layout(legend.outside = TRUE,
                                                                                            legend.height = 0.45, 
                                                                                            legend.width = 4.0,
                                                                                            legend.position = c("right", "bottom"),
                                                                                            frame = FALSE)
    })
    
    output$reg <- renderPlot({
        ggplot(data=df_box_2[df_box_2$Time==input$slider,], aes(x = PA, y=Total_pop ,fill=PA)) +
            geom_bar(stat="identity") + theme(legend.position="bottom")+
            geom_text(aes(label=Total_pop), hjust=1.6, color="white", size=4.5) + scale_y_continuous(labels = function(y) format(y, scientific = FALSE))+ coord_flip()
        
        
    })
    
    
    
    output$pop_pa_top <- renderPlot({
        ggplot(data=df_box_2[df_box_2$Time==input$slider,], aes(x = PA, y=Total_pop ,fill=PA)) +
            geom_bar(stat="identity") + theme(legend.position="bottom")+
            geom_text(aes(label=Total_pop), hjust=1.6, color="white", size=4.5) + scale_y_continuous(labels = function(y) format(y, scientific = FALSE))+ coord_flip()
        
        
    })
    
    
    output$pop_pa_top2 <- renderPlot({
        ggplot(data=df_box_3[df_box_3$Time==input$slider,], aes(x = PA, y=Total_pop ,fill=PA)) +
            geom_bar(stat="identity") + theme(legend.position="bottom")+
            geom_text(aes(label=Total_pop), hjust=1.6, color="white", size=4.5) + scale_y_continuous(labels = function(y) format(y, scientific = FALSE))+ coord_flip()
        
        
    })
    
    
    
    
    
    
    observe({
        var_year <- input$slider
        
        output$pop_pa_map <- renderTmap({
            tmap_mode("plot")
            tm_shape(SG_2014_planningarea_pop[SG_2014_planningarea_pop$Time==input$slider,])+tm_fill('Total_pop',style = "equal",palette="Greens", 
                                                                                             legend.hist = TRUE, 
                                                                                             legend.is.portrait = TRUE,
                                                                                             legend.hist.z = 0.2)+ 
                tm_compass(type="arrow", size = 2) +
                tm_scale_bar(width = 0.45) +
                tm_grid() +tm_text("PLN_AREA_C", size =0.7)+ tm_borders(alpha = 0.6)+ tm_layout(legend.outside = TRUE,
                                                                                                legend.height = 0.45, 
                                                                                                legend.width = 4.0,
                                                                                                legend.position = c("right", "bottom"),
                                                                                                frame = FALSE)
        })
        
        
        
        
       
    })
    
    
}	




shinyApp(ui, server)
