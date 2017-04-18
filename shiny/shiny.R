library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)


vg <- readRDS("./vg.rds")


###design
ui<- fluidPage(

  navbarPage("Video Games",
    
    tabPanel("Region",
            sidebarLayout(
            	sidebarPanel(
            	   sliderInput(
            	       "yr_plat", "Games' Release Year", min = 2000, max = 2017, value = c(2010,2017)
            	    )
            	 ),
            
          	mainPanel(
          	  tabsetPanel(
          	    tabPanel("Global",
              	          plotlyOutput("sales_global")
          	    ),
          	    tabPanel("North America",
          	             plotlyOutput("sales_na")
          	    ),
          	    tabPanel("Europe",
          	             plotlyOutput("sales_eu")
          	    ),
          	    tabPanel("Japan",
          	           plotlyOutput("sales_jpn")
          	    )
          	  )
            )
            )
    ),
  tabPanel("Platform",
           sidebarLayout(
             sidebarPanel(
               sliderInput(
                 "yr_reg", "Games' Release Year", min = 1970, max = 2017, value = c(2010,2017)
               )
             ),
             
             mainPanel(
               tabsetPanel(
                 tabPanel("All Platforms",
                          plotlyOutput("sales_plat")
                  ),
                   tabPanel("PS4",
                            plotlyOutput("sales_ps4")
                   ),
                   tabPanel("Xbox One",
                            plotlyOutput("sales_xone")
                   ),
                   tabPanel("Wii U",
                            plotlyOutput("sales_wiiu")
                   )
                
               )
             )
           )
           ),
  tabPanel("Publisher",
           mainPanel(
             tabsetPanel(
               tabPanel("Nintendo",
                        plotlyOutput("sales_nin")
               )
             )
           ))
)
)

### code
server<-function(input, output) {

  output$sales_global <- renderPlotly({
    p<-vg %>% filter(Year >= input$yr_plat[1], Year <= input$yr_plat[2]) %>% gather("Region","Sales",7:11)%>%
      select(Platform,Year,Region,Sales) %>% group_by(Platform,Year,Region) %>% summarise(total.sales =sum(Sales)) %>%
      filter(Region == "Global")%>%
      ggplot(aes(x=Year,y=total.sales,color=Platform))+geom_line()+
               labs(y="Total Sales (in millions)")+theme(axis.text.x=element_text(angle=-25,hjust=.1))
    ggplotly(p)

  })
  output$sales_na <- renderPlotly({
    p<-vg %>% filter(Year >= input$yr_plat[1], Year <= input$yr_plat[2]) %>% gather("Region","Sales",7:11)%>%
      select(Platform,Year,Region,Sales) %>% group_by(Platform,Year,Region) %>% summarise(total.sales =sum(Sales)) %>%
      filter(Region == "North America")%>%
      ggplot(aes(x=Year,y=total.sales,color=Platform))+geom_line()+
      labs(y="Total Sales (in millions)")+theme(axis.text.x=element_text(angle=-25,hjust=.1))
    ggplotly(p)
  })
  output$sales_eu <- renderPlotly({
    p<-vg %>% filter(Year >= input$yr_plat[1], Year <= input$yr_plat[2]) %>% gather("Region","Sales",7:11)%>%
      select(Platform,Year,Region,Sales) %>% group_by(Platform,Year,Region) %>% summarise(total.sales =sum(Sales)) %>%
      filter(Region == "Europe")%>%
      ggplot(aes(x=Year,y=total.sales,color=Platform))+geom_line()+
      labs(y="Total Sales (in millions)")+theme(axis.text.x=element_text(angle=-25,hjust=.1))
    ggplotly(p)
  })
  output$sales_jpn <- renderPlotly({
    p<-vg %>% filter(Year >= input$yr_plat[1], Year <= input$yr_plat[2]) %>% gather("Region","Sales",7:11)%>%
      select(Platform,Year,Region,Sales) %>% group_by(Platform,Year,Region) %>% summarise(total.sales =sum(Sales)) %>%
      filter(Region == "Japan")%>%
      ggplot(aes(x=Year,y=total.sales,color=Platform))+geom_line()+
      labs(y="Total Sales (in millions)")+theme(axis.text.x=element_text(angle=-25,hjust=.1))
    ggplotly(p)
  })
  

  output$sales_plat <- renderPlotly({
    p<-vg %>% filter(Year >= input$yr_reg[1], Year <= input$yr_reg[2]) %>% gather("Region","Sales",7:11)%>%
      select(Year,Region,Sales) %>% group_by(Year,Region) %>% summarise(total.sales =sum(Sales)) %>%
      ggplot(aes(x=Year,y=total.sales,color=Region))+geom_line()+
      labs(y="Total Sales (in millions)")+theme(axis.text.x=element_text(angle=-25,hjust=.1))
    ggplotly(p)
  })
  output$sales_ps4 <- renderPlotly({
    p<-vg %>% filter(Year >= input$yr_reg[1], Year <= input$yr_reg[2]) %>% gather("Region","Sales",7:11)%>%
      select(Platform,Year,Region,Sales) %>% group_by(Platform,Year,Region) %>% summarise(total.sales =sum(Sales)) %>%
      filter(Platform == "PS4") %>%
      ggplot(aes(x=Year,y=total.sales,color=Region))+geom_line()+
      labs(y="Total Sales (in millions)")+theme(axis.text.x=element_text(angle=-25,hjust=.1))
    ggplotly(p)
  })
  output$sales_xone <- renderPlotly({
    p<-vg %>% filter(Year >= input$yr_reg[1], Year <= input$yr_reg[2]) %>% gather("Region","Sales",7:11)%>%
      select(Platform,Year,Region,Sales) %>% group_by(Platform,Year,Region) %>% summarise(total.sales =sum(Sales)) %>%
      filter(Platform == "XOne") %>%
      ggplot(aes(x=Year,y=total.sales,color=Region))+geom_line()+
      labs(y="Total Sales (in millions)")+theme(axis.text.x=element_text(angle=-25,hjust=.1))
    ggplotly(p)
  })
  output$sales_wiiu <- renderPlotly({
    p<-vg %>% filter(Year >= input$yr_reg[1], Year <= input$yr_reg[2]) %>% gather("Region","Sales",7:11)%>%
      select(Platform,Year,Region,Sales) %>% group_by(Platform,Year,Region) %>% summarise(total.sales =sum(Sales)) %>%
      filter(Platform == "WiiU") %>%
      ggplot(aes(x=Year,y=total.sales,color=Region))+geom_line()+
      labs(y="Total Sales (in millions)")+theme(axis.text.x=element_text(angle=-25,hjust=.1))
    ggplotly(p)
  })
  
  output$sales_nin<- renderPlotly({
    p<-vg %>% filter(Publisher == "Nintendo") %>% 
      group_by(Platform) %>% summarise(total.sales =sum(Global)) %>%
      ggplot(aes(x=Platform,y=total.sales))+geom_line()
    ggplotly(p+coord_polar())
  })
  
  
  
}

shinyApp(ui,server)



