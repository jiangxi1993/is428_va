#Package
packages = c('sf', 'tmap', 'tidyverse','ggplot2','pastecs','data.table','devtools','reshape2','viridis','shiny','shinydashboard','plotly','ggalluvial','GGally','lubridate','RColorBrewer','dplyr','readr', 'gganimate','scales', 'animation','stringr','gapminder','png','gifski')#,'ggtern')





for (p in packages){
    if(!require(p, character.only = T)){
        install.packages(p)
    }
    library(p,character.only = T)
}


#detach("package:dplyr")
library(dplyr)

#install_version("ggplot2", version = "3.2.1", repos = "http://cran.us.r-project.org")
#library(ggplot2)



#Preparation for data map box 1
options(scipen=10000)


SG_2014_planningarea <- st_read(dsn = "./../Data/GEO", layer = "MP14_PLNG_AREA_WEB_PL")


population_data_10_19<-read_csv("./../Data/Residential_Planning/respopagesextod2011to2019.csv")


young=c("0_to_4","10_to_14","15_to_19","20_to_24")
active=c("25_to_29","30_to_34","35_to_39","40_to_44","45_to_49","5_to_9","50_to_54","55_to_59","60_to_64")
aged=c("65_to_69","70_to_74","75_to_79","80_to_84","85_and_over")

agg <- with(population_data_10_19, aggregate(Pop, by = list(Time = Time, AG  = AG ,Sex= Sex ), FUN = sum))

out <- reshape(agg, timevar = "AG", idvar = c("Time","Sex"), direction = "wide")


col_order <- c("Time" ,"Sex" ,"x.0_to_4" , "x.5_to_9", "x.10_to_14","x.15_to_19","x.20_to_24","x.25_to_29","x.30_to_34",    "x.35_to_39" , "x.40_to_44" ,"x.45_to_49","x.50_to_54" ,"x.55_to_59" ,"x.60_to_64", "x.65_to_69","x.70_to_74" , "x.75_to_79"    ,"x.80_to_84","x.85_to_89","x.90_and_over")

pop_reordered_10_19<- out[, col_order]


popdf_age=mutate(population_data_10_19 ,AgeGroup=ifelse(population_data_10_19$AG %in% young,"Young",ifelse(population_data_10_19$AG%in% active,"Economy_Active","Aged"))) %>%select(-AG)%>%arrange(Time) %>% mutate(AgeGroup=factor(AgeGroup))

agg_pop <- with(popdf_age ,aggregate(Pop, by = list(Time = Time,PA=PA,AgeGroup=AgeGroup ), FUN = sum))

out_pop <- reshape(agg_pop, timevar = "AgeGroup", idvar = c("Time","PA"), direction = "wide")



out_pop <-out_pop %>% mutate(Total_pop = select(., 3:5) %>% rowSums()) 
out_pop <-out_pop %>% mutate(Young_ratio=x.Young/Total_pop)
out_pop <-out_pop %>% mutate(Old_ratio = x.Aged/Total_pop) 
out_pop_10_19 <-out_pop %>% mutate(Active_ratio=1-Old_ratio-Young_ratio)

out_pop_10_19[[2]] <- toupper(out_pop_10_19[[2]])

out_pop_10_19[[1]] <- year(mdy((out_pop_10_19[[1]])))
#view(out_pop_10_19)

SG_2014_planningarea_pop <- left_join(SG_2014_planningarea, out_pop_10_19, by = c("PLN_AREA_N" = "PA"))

view(SG_2014_planningarea_pop )
view(SG_2014_planningarea_pop )

# Preparation for bar chart tail 5 BOX 4

agg_year_total <- aggregate(out_pop_10_19$Total_pop, by=list(Time=out_pop_10_19$Time), FUN=sum) 

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






# Preparation for bar chart top 5 BOX 2
df_box_2 <- out_pop_10_19

setDT(df_box_2)
df_box_2 <- df_box_2[order(-Total_pop), .SD[1:5], Time]

#view(df_box_2)

# Preparation for bar chart tail 5 BOX 3
df_box_3 <- out_pop_10_19[out_pop_10_19$Total_pop>1,]

setDT(df_box_2)
df_box_3 <- df_box_3[order(Total_pop), .SD[1:5], Time]

#view(df_box_3)


view(out_pop_10_19)

# view 3




census_data <- read_csv("../Data/Census/census.csv")


save_data <- census_data %>% 
  # pivot education
  pivot_longer(
    cols = starts_with("Education: "), 
    names_to = "Education", 
    values_to = "Education_count",
    values_drop_na = TRUE
  )%>%
  
  # pivot Housing
  pivot_longer(
    cols = starts_with("Housing: "), 
    names_to = "Housing", 
    values_to = "Housing_count",
    values_drop_na = TRUE
  )%>%
  
  # pivot Income
  pivot_longer(
    cols = starts_with("Income: "), 
    names_to = "Income", 
    values_to = "Income_count",
    values_drop_na = TRUE
  )

write.csv(save_data, "../Data/Census/census_data_pivoted.csv")


census_data_pivoted <- read_csv("../Data/Census/census_data_pivoted.csv") 



list_for_drop_down =unique(census_data_pivoted[,"Planning_Area"])

#UI Section

library(plotly)

tmap_mode("plot")
ui <- dashboardPage(skin = "green",
                    
                    #---------------------------- dashboard header ----------------------------                                    
                    header <- dashboardHeader(title = "Our Homeland x Facts",
                                              titleWidth = 300),
                    
                    
                    #--------------------------- dashboard sidebar ----------------------------
                    sidebar <-dashboardSidebar(
                        sidebarMenu(
                            menuItem(text = "Population",
                                     tabName = "Population",
                                     icon = icon("home")
                            ),
                            menuItem(text = "Age",
                                     tabName = "Age",
                                     icon = icon("dice-one")
                            ),
                            menuItem(text = "Education | Income",
                                     tabName = "Education",
                                     icon = icon("dice-two")
                            )
                        )
                    ),
                    
                    #--------------------------- dashboard sidebar ----------------------------
                    body <- dashboardBody(
                        tabItems(
                            tabItem(tabName = "Population",
                                    column(7,height=550,box(width=NULL,title="Singapore Population (Summary view)",
                                
                                        tmapOutput("pop_pa_map",height = 550))
                                        
                                        ,
                                        box(width=NULL,title="Total Singapore Population (Regional Trend Finder)"
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
                            
                            
                            tabItem(tabName = "Age",
                                    column(7,height=550,box(width=NULL,title="Singapore Age structure by Planning Area" ,
                                                            
                                                           tmapOutput("pop_pa_age_map",height = 550)
                                                          )
                                           
                                           ,
                                           box(width=NULL,title="Control Panel",
                                              # , plotOutput("pop_pa_top_reg", height = 280)
                                              
                                              box(width=NULL, sliderInput("slider_age", "Year :", min = 2011, max=2019,8)),
                                              box(width=NULL, radioButtons("rb_age", "Resident Age Group:",
                                                                            c("Young" = "Young_ratio",
                                                                              "Economy_Active" = "Active_ratio",
                                                                              "Aged" = "Old_ratio"))
                                           
                                    )
                            )
                        
                        
                            ),
                            column(5,
                                   
                                   tabBox(width=NULL,
                                          tabPanel("Young population group by PA",plotOutput("lolipop_ani_young", height = 500)),
                                          tabPanel("Economy Active population group by PA",plotOutput("lolipop_ani_active", height = 500)),
                                          tabPanel("Aging population group by PA",plotOutput("lolipop_ani", height = 500))
                                                              
                                                              
                                                              
                                                              
                                          
                                          
                                          
                                          
                                       
                                   )
                                   ,box(width=NULL,title="Age group penetration by planning area", plotlyOutput('tern_age')  )
                                   
                                   
                                   
                                   
                            )
                            
                            
                        
                    ), #view 3
                    tabItem(tabName = "Education",
                            column(12,height=550,box(width=NULL,title="Social mobility: Income, Housing and Qualification" ,
                                                    
                                                    plotOutput("sankey_view3",height = 600)
                            ),
                            box(width=NULL, 
                                
                                column(4,box(width=200, radioButtons("rb_year_income", "Year:",
                                                                     c("2010" = "2010",
                                                                       "2015" = "2015"))
      
                                      ), box(width=200,
                                      
                                      selectInput(
                                        "select_pa",
                                        label =  "Select Planning Area" ,
                                        choices = list_for_drop_down
                                        )
                                      )
                                
                                )
                                
                                
                            )
                            
    
                            
                            
                            )
                            
                    ))
                    
))






server <- function(input, output, session) {
     
 
    library(plotly)
    output$pop_pa_map <- renderTmap({
     
        tm_shape(SG_2014_planningarea_pop[SG_2014_planningarea_pop$Time==2011,])+tm_fill('Total_pop',style = "equal",palette="Greens", 
                     legend.hist = TRUE, 
                     legend.is.portrait = TRUE,
                     legend.hist.z = 0.2)+ 
            tm_compass(type="arrow", size = 2) +
            tm_scale_bar(width = 0.45) +
            tm_grid() +tm_text("PLN_AREA_C", size =0.7)+ tm_borders(alpha = 0.6)+ tm_layout(frame = FALSE)
    })
    
    output$pop_pa_top_reg<- renderPlot({
            df_line_1<-merge(agg_year_total , df_year_central_agg, by = "Time")
            df_line_2<-merge(df_line_1 , df_year_east_agg, by = "Time")
            df_line_3<-merge(df_line_2 , df_year_west_agg, by = "Time")
            df_line_4<-merge(df_line_3 , df_year_North_agg, by = "Time")
            df_line_6<-merge(df_line_4 , df_year_ner_agg, by = "Time")
            
            
        
        ggplot(df_line_6, aes(x=Time)) + geom_line(aes(y = Central ), color="steelblue",linetype="dotted",size=2)  + 
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
    
    
 
    
    
    
    #tab 2

        
    output$pop_pa_age_map <- renderTmap({
            
            tm_shape(SG_2014_planningarea_pop[SG_2014_planningarea_pop$Time==2011,])+tm_fill('Young_ratio',style = "equal",palette="Blues", 
                                                                                             legend.hist = TRUE, 
                                                                                             legend.is.portrait = TRUE,
                                                                                             legend.hist.z = 0.2)+ 
                tm_compass(type="arrow", size = 2) +
                tm_scale_bar(width = 0.45) +
                tm_grid() +tm_text("PLN_AREA_C", size =0.7)+ tm_borders(alpha = 0.6)+ tm_layout(frame = FALSE)
        
        })    
        
    
    

    

    
    df_lolipop = out_pop_10_19[out_pop_10_19 $x.Aged>0,]
    
    p<- ggplot( df_lolipop,
               aes(x = reorder(PA , x.Aged), y = x.Aged, color = PA)) +
        geom_point(stat = 'identity', size = 5) +
        geom_segment(aes(
            y=100,
            x = PA ,
            yend = x.Aged,
            xend = PA )
        )+
        coord_flip() 
    
    z<- p +transition_states(Time,transition_length = 1,state_length = 2)+transition_time(Time)+labs(title = "Year: {as.integer(frame_time)}",x="Popuplation", y = "PA") +
        theme(legend.position = "none") + ease_aes("linear")+ shadow_mark(alpha = 0.3, size = 0.5)+theme(axis.text.x=element_text(angle=90,size = rel(0.9), margin = margin(0.3, unit = "cm"),vjust =1))
    
    #ani_pop<animate(z)
    anim_save("lolipop_outfile.gif", animate(z)) 
    
    
    
    
    df_lolipop_young = out_pop_10_19[out_pop_10_19 $x.Young>0,]
    
    p1<- ggplot( df_lolipop_young,
                aes(x = reorder(PA , x.Young), y = x.Young, color = PA)) +
      geom_point(stat = 'identity', size = 5) +
      geom_segment(aes(
        y=100,
        x = PA ,
        yend = x.Young,
        xend = PA )
      )+
      coord_flip() 
    
    z1<- p1 +transition_states(Time,transition_length = 1,state_length = 2)+transition_time(Time)+labs(title = "Year: {as.integer(frame_time)}",x="Popuplation", y = "PA") +
      theme(legend.position = "none") + ease_aes("linear")+ shadow_mark(alpha = 0.3, size = 0.5)+theme(axis.text.x=element_text(angle=90,size = rel(0.9), margin = margin(0.3, unit = "cm"),vjust =1))
    
    #ani_pop<animate(z)
    anim_save("lolipop_young_outfile.gif", animate(z1)) 
    
    
    
    df_lolipop_active = out_pop_10_19[out_pop_10_19 $x.Economy_Active>0,]
    
    p2<- ggplot( df_lolipop_active,
                 aes(x = reorder(PA , x.Economy_Active), y = x.Economy_Active, color = PA)) +
      geom_point(stat = 'identity', size = 5) +
      geom_segment(aes(
        y=100,
        x = PA ,
        yend = x.Economy_Active,
        xend = PA )
      )+
      coord_flip() 
    
    z2<- p2 +transition_states(Time,transition_length = 1,state_length = 2)+transition_time(Time)+labs(title = "Year: {as.integer(frame_time)}",x="Popuplation", y = "PA") +
      theme(legend.position = "none") + ease_aes("linear")+ shadow_mark(alpha = 0.3, size = 0.5)+theme(axis.text.x=element_text(angle=90,size = rel(0.9), margin = margin(0.3, unit = "cm"),vjust =1))
    
    
    #ani_pop<animate(z)
    anim_save("lolipop_active_outfile.gif", animate(z2)) 
    
    
    
    
  

        
    output$lolipop_ani <- renderImage({
      
        list(src = "lolipop_outfile.gif", align = "center",
             contentType = 'image/gif'
        )}, deleteFile = TRUE)
    
    
    
    output$lolipop_ani_young <- renderImage({
      
      list(src = "lolipop_young_outfile.gif", align = "center",
           contentType = 'image/gif'
      )}, deleteFile = TRUE)
    
    
    
    output$lolipop_ani_active <- renderImage({
      
      list(src = "lolipop_active_outfile.gif", align = "center",
           contentType = 'image/gif'
      )}, deleteFile = TRUE)
    
        
     
    
    output$pop_pa_top2 <- renderPlot({
        ggplot(data=df_box_3[df_box_3$Time==input$slider,], aes(x = PA, y=Total_pop ,fill=PA)) +
            geom_bar(stat="identity") + theme(legend.position="bottom")+
            geom_text(aes(label=Total_pop), hjust=1.6, color="white", size=4.5) + scale_y_continuous(labels = function(y) format(y, scientific = FALSE))+ coord_flip()
    
        
    })
    
    
    label = function(txt) {
      list(
        text = txt,
        x = 0.1, y = 1,
        ax = 0.1, ay = 0,
        xref = "paper", yref = "paper",
        align = "bottom",
        font = list(family = "calibri", size = 15, color = "#3f3f3f"),
        bgcolor = "white", bordercolor = "Black", borderwidth = 1
        
      )
    }
    
    
    # reusable function for axis formatting
    axis = function(txt) {
      list(
        title = txt, tickformat = ".0%", tickfont = list(size = 10)
      )
    }
    
    ternaryAxes = list(
      aaxis = axis("Ageing"),
      baxis = axis("Young"),
      caxis = axis("Economy Active")
      
      
    )
 
    data_input=out_pop_10_19[out_pop_10_19$Time==2011,]#input$slider_age
    data_input= data_input[data_input$Total_pop>0,]
    view( data_input)
    str(data_input)
    
    output$tern_age <- renderPlotly({
      plot_ly(data_input, a = ~x.Aged, b = ~x.Young, c = ~`x.Economy_Active`, color = "Blues", type = "scatterternary",text=~paste('Aging Ratio:',Old_ratio,"<br>Planning Area:",data_input$PA),size=2 ,mode="markers",marker=list( opacity=1),colors =data_input$PA #"Spectral"
      ) %>%layout(title=2011, annotations = label("Marker size:Old Age Ratio" ), ternary = ternaryAxes,margin = 0.05,showlegend = TRUE ,autosize=TRUE                  #data_input$Old_ratio*10
        ) 
      
      
    })
    
    
    
   #Tab 3
                                
#    census_data_pivoted[[4]] <- year(mdy((census_data_pivoted[[4]])))
#    str(census_data_pivoted)
#    
#    census_data_pivoted_input<-census_data_pivoted[census_data_pivoted$Year==2010,]
 
        
 #   output$sankey_view3 <- renderPlot({

 #     input_data <- census_data_pivoted_input %>%filter(Planning_Area == "Total")
      
#      ggplot(data = input_data,
 #            aes(axis1 = Income, axis2 = Education, axis3 = Housing, y = Education_count)) +
#        xlab("Demographic") +
#        geom_alluvium(aes(fill = Education)) +
 #       geom_stratum() + geom_text(stat = "stratum", infer.label = TRUE) +
#        theme_minimal() +
 #       ggtitle("Total 2010")+ theme(legend.position="top")
      
      
#    })    
        
            
    
    
    
    
    
    
    
    observe({
        var_year <- input$slider
        
        
        output$pop_pa_map <- renderTmap({
            tmap_mode("plot")
            tm_shape(SG_2014_planningarea_pop[SG_2014_planningarea_pop$Time==input$slider,])+tm_fill('Total_pop',style = "equal",palette="Greens", 
                                                                                             legend.hist = TRUE, 
                                                                                             legend.is.portrait = TRUE,
                                                                                             legend.hist.z = 0.2
                                                                                             
                                                                                             )+ 
                tm_compass(type="arrow", size = 2) +
                tm_scale_bar(width = 0.45) +
                tm_grid() +tm_text("PLN_AREA_C", size =0.7)+ tm_borders(alpha = 0.6)+ tm_layout(legend.outside = TRUE,
                                                                                                legend.height = 0.45, 
                                                                                                legend.width = 4.0,
                                                                                                legend.position = c("right", "bottom"),
                                                                                                frame = FALSE
                                                                                                
                                                                                                )
        })
        
        
        var_year_tab2 <- input$slider_age
        var_rb<-input$rb_age
        var_rb_fill<-"Blues"

      
        if(var_rb == "Young_ratio"){
            var_rb_fill<-"Blues"
          
        }
        else if (var_rb == "Active_ratio"){
            var_rb_fill<-"Oranges"
          
        }
        else if (var_rb == "Old_ratio"){
            var_rb_fill<-"Reds"
            
        }
        
        
        
       
        
        output$pop_pa_age_map <- renderTmap({
            
            tm_shape(SG_2014_planningarea_pop[SG_2014_planningarea_pop$Time==input$slider_age,])+tm_fill(var_rb,style = "equal",palette=var_rb_fill, 
                                                                                             legend.hist = TRUE, 
                                                                                             legend.is.portrait = TRUE,
                                                                                             legend.hist.z = 0.2 )+ 
                tm_compass(type="arrow", size = 2) +
                tm_scale_bar(width = 0.45) +
                tm_grid() +tm_text("PLN_AREA_C", size =0.7)+ tm_borders(alpha = 0.6)+ tm_layout(frame = FALSE)
            
        })    
        
        if(input$rb_year_income == "2010"){
          var_year_income<-2010
          
        }
        else if (input$rb_year_income == "2015"){
          var_year_income<-2015
          
        
        }
        
        census_data_pivoted[[4]] <- year(mdy((census_data_pivoted[[4]])))
        census_data_pivoted_input<-census_data_pivoted[census_data_pivoted$Year== var_year_income ,]
        print(as.numeric(as.character(input$rb_year_income)))
        
         output$sankey_view3 <- renderPlot({
           
           input_data <- census_data_pivoted_input %>%
             filter(Planning_Area == input$select_pa)
           
           ggplot(data = input_data,
                  aes(axis1 = Income, axis2 = Education, axis3 = Housing, y = Education_count)) +
            xlab("Demographic") +
            geom_alluvium(aes(fill = Education)) +
            geom_stratum() + geom_text(stat = "stratum", infer.label = TRUE) +
             theme_minimal() +
             ggtitle(input$select_pa)+ theme(legend.position="top")
           
         })
        
        
        data_input=out_pop_10_19[out_pop_10_19$Time==input$slider_age,]#input$slider_age
        data_input= data_input[data_input$Total_pop>0,]
        view( data_input)
        str(data_input)
        
        output$tern_age <- renderPlotly({
          plot_ly(
            #   data_input, a = ~x.Aged, b = ~x.Young, c = ~`x.Economy_Active`, color = ~PA, type = "scatterternary",text=~paste('Aging Ratio:',Old_ratio,"<br>Planning Area:",data_input$PA),size=data_input$Old_ratio*10 ,mode="markers",marker=list( opacity=1),colors = (colorRampPalette(brewer.pal(name="Spectral", n = 8))(14)) 
            
            data_input, a = ~Old_ratio, b = ~Young_ratio , c = ~`Active_ratio`, color = ~PA, type = "scatterternary",text=~paste('Aging Ratio:',Old_ratio,"<br>Planning Area:",data_input$PA),size=data_input$Old_ratio*10 ,mode="markers",marker=list( opacity=1),colors = (colorRampPalette(brewer.pal(name="Spectral", n = 8))(14)) 
          ) %>%layout(title=input$slider_age, annotations = label("Marker size:Old Age Ratio" ), ternary = ternaryAxes,margin = 0.05,showlegend = TRUE
          ) 
          
        })
        
        
        
        
       
    })
    
    
}	




shinyApp(ui, server)
