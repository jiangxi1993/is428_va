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

names(agg_year_total)[2] <- "Total_population"

view(agg_year_total)




df_region=SG_2014_planningarea_pop %>% st_drop_geometry()

View(df_region)
str(df_region)


df_year_central <- df_region[df_region$REGION_C=="CR",]

str(df_year_central)

df_year_central_agg <- aggregate(df_year_central$Total_pop, by=list(Time=df_year_central$Time), FUN=sum)

names(df_year_central_agg )[2] <- "Central"

str(df_year_central_agg)

View(df_year_central_agg)



df_year_west <- df_region[df_region$REGION_C=="WR",]

str(df_year_west )

df_year_west_agg <- aggregate(df_year_west$Total_pop, by=list(Time=df_year_west$Time), FUN=sum)

names(df_year_west_agg )[2] <- "West"

str(df_year_west_agg)

View(df_year_west_agg)



df_year_east <- df_region[df_region$REGION_C=="ER",]

str(df_year_east )

df_year_east_agg <- aggregate(df_year_east$Total_pop, by=list(Time=df_year_east$Time), FUN=sum)

names(df_year_east_agg )[2] <- "East"

str(df_year_east_agg)

View(df_year_east_agg)



df_year_North <- df_region[df_region$REGION_C=="NR",]

str(df_year_North )

df_year_North_agg <- aggregate(df_year_North$Total_pop, by=list(Time=df_year_North$Time), FUN=sum)

names(df_year_North_agg )[2] <- "North"

str(df_year_North_agg)

View(df_year_North_agg)


df_year_ner <- df_region[df_region$REGION_C=="NER",]

str(df_year_ner )

df_year_ner_agg <- aggregate(df_year_ner$Total_pop, by=list(Time=df_year_ner$Time), FUN=sum)

names(df_year_ner_agg )[2] <- "North_East"

str(df_year_ner_agg)

View(df_year_ner_agg)


df_line_1=merge(agg_year_total , df_year_central_agg, by = "Time")
df_line_2=merge(df_line_1 , df_year_east_agg, by = "Time")
df_line_3=merge(df_line_2 , df_year_west_agg, by = "Time")
df_line_4=merge(df_line_3 , df_year_North_agg, by = "Time")
df_line_5=merge(df_line_4 , df_year_ner_agg, by = "Time")


str(df_line_5)

View(df_line_5)














































#UI Section


tmap_mode("plot")
ui <- dashboardPage(skin = "green",
                    
                    #---------------------------- dashboard header ----------------------------                                    
                    header <- dashboardHeader(title = "Precision Policy And Planning",
                                              titleWidth = 300),
                    
                    
                    #--------------------------- dashboard sidebar ----------------------------
                    sidebar <-dashboardSidebar(
                        sidebarMenu(
                            menuItem(text = "Introduction",
                                     tabName = "Introduction",
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
                            tabItem(tabName = "Introduction",
                                    column(7,height=550,box(width=NULL,title="Singapore population distribution by Planning Area",
                                
                                        tmapOutput("pop_pa_map",height = 550))
                                        
                                        ,
                                        box(width=NULL,title="Total Singapore Population by Region"
                                           , plotOutput("pop_pa_top_reg", height = 280)
                                             
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
    
    output$pop_pa_top_reg<- renderPlot({
      
        ggplot(df_line_5, aes(x=Time,label = rownames(df_line_5))) + 
           # geom_line(aes(y =  Total_population ), color = "darkred") + 
            geom_line(aes(y = Central ), color="steelblue",linetype="dotted",size=2)  + 
            geom_line(aes(y = West ), color="green",linetype="dotted",size=2) +
            geom_line(aes(y = East ), color="darkred",linetype="dotted",size=2) +
            geom_line(aes(y = North ), color="orange",linetype="dotted",size=2)+
            geom_line(aes(y = North_East ), color="#CC79A7",linetype="dotted",size=2)+
            theme(legend.position="top") + scale_y_continuous(labels = function(y) format(y, scientific = FALSE))+
              labs(y="Popuplation", x = "Year ")+ scale_x_continuous( breaks=function(x) pretty(x, n=9))+
            geom_text(aes(x = 2011,y = 936710 , label = "Central", color = "steelblue")) + 
            geom_text(aes(x = 2012,y = 900120 	, label = "West", color = "green")) + 
            geom_text(aes(x = 2013,y = 	694140, label = "East", color = "darkred")) + 
            geom_text(aes(x = 2014,y = 521480, label = "North", color = "orange"))+ 
            geom_text(aes(x = 2014,y = 836120 , label = "North East", color = "#CC79A7"))+ theme(legend.position = "none") 
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
