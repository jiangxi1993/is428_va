#Contents:
#    1. Package Imports
#    2. Loading of data
#    3. Visualisation Functions
#    4. Application UI

#Package
packages = c('ggbeeswarm','sf', 'tmap', 'tidyverse','ggplot2','pastecs','data.table','devtools','reshape2','viridis','shiny','shinydashboard','plotly','GGally')


for (p in packages){
    if(!require(p, character.only = T)){
        install.packages(p)
    }
    library(p,character.only = T)
}


detach("package:dplyr")
library(dplyr)


#Preparation for data



SG_2014_planningarea <- st_read(dsn = "../Data/GEO", layer = "MP14_PLNG_AREA_WEB_PL")

population_data_10_19<-read_csv("../Data/Residential_Planning/respopagesextod2011to2019.csv")



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
view(out_pop_10_19)

SG_2014_planningarea_pop <- left_join(SG_2014_planningarea, out_pop_10_19, by = c("PLN_AREA_N" = "PA"))


# Beeswarm plot
ggplot(population_data_10_19,aes(PA, Pop)) + 
    geom_jitter()





#UI Section

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
    body <- dashboardBody()
    
)


server <- function(input, output) {
 
}



shinyApp(ui, server)
