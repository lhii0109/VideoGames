library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)


vg <- readRDS("./vg.rds")


###design
ui<- fluidPage(

  navbarPage("Video Games",
##### 
#demographics page
    tabPanel("Platforms",
            	   sliderInput(
            	       "demo_yr", "Games Release Year", min = 1970, max = 2017, value = c(1990,2017),
            	    width = 1500)
            	 ,hr(),
            	  tabsetPanel(
            	    tabPanel("Global",
            	             plotlyOutput("region_global",height=650)
            	    ),
            	    tabPanel("North America",
            	             plotlyOutput("region_na",height=650)
            	    ),
            	    tabPanel("Europe",
            	             plotlyOutput("region_eu",height=650)
            	    ),
            	    tabPanel("Japan",
            	             plotlyOutput("region_jpn",height=650)
            	    )
          	  )
    ),
#####
#Demographics page
  tabPanel("Demographics",
               sliderInput(
                 "cons_yr", "Games Release Year", min = 1970, max = 2017, value = c(1970,2017),width=1500
               ),hr(),
               tabsetPanel(
                 tabPanel("All Platforms",
                          plotlyOutput("plat",height=650)
                 ),
                 tabPanel("PS4",
                          plotlyOutput("plat_ps4",height=650)
                 ),
                 tabPanel("Xbox One",
                          plotlyOutput("plat_xone",height=650)
                 ),
                 tabPanel("3DS",
                          plotlyOutput("plat_3ds",height=650)
                 )
               )
             ),

#####
#company
  tabPanel("Publishers",
                 sliderInput(
                   "yr_sales", "Games Release Year", min = 2000, max = 2017, value = c(2010,2017),width=1500
                 ),
           selectizeInput(
                   "pub_select", "Publisher", choices = unique(vg$Publisher),multiple = TRUE,width = 500
                 ),hr(),
                 tabsetPanel(
                   tabPanel("Sales",
                      sliderInput(
                              "min_total_sales", "Min. Total Sales", min=0,max=200,value=20,width = 1000
                            ),
                      plotlyOutput("pub_all",height = 500)
                   ),
                   tabPanel("Platform",
                      fluidRow(
                        column(6,
                      plotlyOutput("pub_pub",height=700,width=750)),
                        column(width=6,
                      tableOutput("pub_pub_tab"))
                   )),
                   tabPanel("Genre",
                      fluidRow(
                        column(6,
                      plotOutput("pub_genre",height=700,width=750)),
                        column(width=6,
                      tableOutput("pub_genre_tab"))
                    ))
                  )
                ),
#####
#games
  tabPanel("Games",
           selectInput("games_cust","Games",choices = unique(vg$Game),multiple=TRUE,width=1500),
            hr(),
            plotlyOutput("gam",height = 750)
  )
)
)

#####
server<-function(input, output) {
  
#####
#Platforms output
  output$region_global <- renderPlotly({
    p<-vg %>% filter(Year >= input$demo_yr[1], Year <= input$demo_yr[2]) %>% gather("Region","Sales",7:11)%>%
      select(Platform,Year,Region,Sales) %>% group_by(Platform,Year,Region) %>% summarise(total.sales =sum(Sales)) %>%
      filter(Region == "Global")%>%
      ggplot(aes(x=Year,y=total.sales,color=Platform))+geom_line()+
      labs(y="Total Sales (in millions)")
    ggplotly(p)
    
  })
  output$region_na <- renderPlotly({
    p<-vg %>% filter(Year >= input$demo_yr[1], Year <= input$demo_yr[2]) %>% gather("Region","Sales",7:11)%>%
      select(Platform,Year,Region,Sales) %>% group_by(Platform,Year,Region) %>% summarise(total.sales =sum(Sales)) %>%
      filter(Region == "North America")%>%
      ggplot(aes(x=Year,y=total.sales,color=Platform))+geom_line()+
      labs(y="Total Sales (in millions)")
    ggplotly(p)
  })
  output$region_eu <- renderPlotly({
    p<-vg %>% filter(Year >= input$demo_yr[1], Year <= input$demo_yr[2]) %>% gather("Region","Sales",7:11)%>%
      select(Platform,Year,Region,Sales) %>% group_by(Platform,Year,Region) %>% summarise(total.sales =sum(Sales)) %>%
      filter(Region == "Europe")%>%
      ggplot(aes(x=Year,y=total.sales,color=Platform))+geom_line()+
      labs(y="Total Sales (in millions)")
    ggplotly(p)
  })
  output$region_jpn <- renderPlotly({
    p<-vg %>% filter(Year >= input$demo_yr[1], Year <= input$demo_yr[2]) %>% gather("Region","Sales",7:11)%>%
      select(Platform,Year,Region,Sales) %>% group_by(Platform,Year,Region) %>% summarise(total.sales =sum(Sales)) %>%
      filter(Region == "Japan")%>%
      ggplot(aes(x=Year,y=total.sales,color=Platform))+geom_line()+
      labs(y="Total Sales (in millions)")
    ggplotly(p)
  })
  
  
#####
#Demographics output
  output$plat <- renderPlotly({
    p<-vg %>% filter(Year >= input$cons_yr[1], Year <= input$cons_yr[2]) %>% gather("Region","Sales",7:11)%>%
      select(Year,Region,Sales) %>% group_by(Year,Region) %>% summarise(total.sales =sum(Sales)) %>%
      ggplot(aes(x=Year,y=total.sales,color=Region))+geom_line()+
      labs(y="Total Sales (in millions)")
    ggplotly(p)
  })
  output$plat_ps4 <- renderPlotly({
    p<-vg %>% filter(Year >=input$cons_yr[1], Year <= input$cons_yr[2]) %>% gather("Region","Sales",7:11)%>%
      select(Platform,Year,Region,Sales) %>% group_by(Platform,Year,Region) %>% summarise(total.sales =sum(Sales)) %>%
      filter(Platform == "PS4") %>%
      ggplot(aes(x=Year,y=total.sales,color=Region))+geom_line()+
      labs(y="Total Sales (in millions)")
    ggplotly(p)
  })
  output$plat_xone <- renderPlotly({
    p<-vg %>% filter(Year >= input$cons_yr[1], Year <= input$cons_yr[2]) %>% gather("Region","Sales",7:11)%>%
      select(Platform,Year,Region,Sales) %>% group_by(Platform,Year,Region) %>% summarise(total.sales =sum(Sales)) %>%
      filter(Platform == "XOne") %>%
      ggplot(aes(x=Year,y=total.sales,color=Region))+geom_line()+
      labs(y="Total Sales (in millions)")
    ggplotly(p)
  })
  output$plat_3ds <- renderPlotly({
    p<-vg %>% filter(Year >= input$cons_yr[1], Year <= input$cons_yr[2]) %>% gather("Region","Sales",7:11)%>%
      select(Platform,Year,Region,Sales) %>% group_by(Platform,Year,Region) %>% summarise(total.sales =sum(Sales)) %>%
      filter(Platform == "3DS") %>%
      ggplot(aes(x=Year,y=total.sales,color=Region))+geom_line()+
      labs(y="Total Sales (in millions)")
    ggplotly(p)
  })

#####
#company output
  output$pub_all<- renderPlotly({
    vg  %>% filter(Year >= input$yr_sales[1], Year <= input$yr_sales[2])%>% group_by(Publisher) %>% 
      summarise(total.sales = sum(Global)) %>% filter(total.sales >=input$min_total_sales) %>%
      ggplot(aes(x=Publisher,y=total.sales,fill=Publisher))+geom_bar(stat="identity")+
      theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+
      labs(y="Total Sales (in millions)")
  })
  output$pub_pub<- renderPlotly({
    vg  %>% filter(Year >= input$yr_sales[1], Year <= input$yr_sales[2])%>% filter(Publisher %in% c(input$pub_select))%>% group_by(Publisher,Platform) %>% 
      summarise(total.sales = sum(Global)) %>%
      ggplot(aes(x=Platform,y=total.sales,fill=Publisher))+
      geom_bar(stat="identity",position=position_dodge())+coord_flip()+
      labs(y="Total Sales (in millions)")
  })
  output$pub_pub_tab <- renderTable({
    vg  %>% filter(Year >= input$yr_sales[1], Year <= input$yr_sales[2])%>% filter(Publisher %in% c(input$pub_select))%>% group_by(Publisher,Platform) %>% 
      summarise(total.sales =sum(Global)) %>% spread(key = Publisher,value = total.sales)
  })
  output$pub_genre<- renderPlot({
    vg  %>% filter(Year >= input$yr_sales[1], Year <= input$yr_sales[2])%>% filter(Publisher %in% c(input$pub_select))%>% group_by(Publisher,Genre) %>% 
      summarise(total.sales =sum(Global)) %>%
      ggplot(aes(x=Genre,y=total.sales,group=Publisher,fill=Publisher))+
      geom_polygon(alpha = .3)+coord_polar()+
      labs(y="Total Sales (in millions)")+
      theme(text = element_text(size=20))
  })
  output$pub_genre_tab <- renderTable({
    vg  %>% filter(Year >= input$yr_sales[1], Year <= input$yr_sales[2])%>% filter(Publisher %in% c(input$pub_select))%>% group_by(Publisher,Genre) %>% 
      summarise(total.sales =sum(Global)) %>% spread(key = Publisher,value = total.sales)
  })
  
  
  
  
#####
#games
  output$gam<- renderPlotly({
    vg  %>%  filter(Game %in% c(input$games_cust))%>% group_by(Game,Platform) %>% 
      summarise(total.sales =sum(Global)) %>%
      ggplot(aes(x=Platform,y=total.sales,fill=Game))+
      geom_bar(stat="identity",position=position_dodge())+
      labs(y="Total Sales (in millions)")
  })
}

shinyApp(ui,server)



