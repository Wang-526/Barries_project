library(shiny)
library(tidyverse)
library(reshape2)
library(data.table)
Sys.setlocale("LC_TIME", "English")

dt <- read.csv(file="/Users/mac/Desktop/berries.csv",header=T)
berry_raw <- dt %>%
    select(Year,Period,State,Commodity,Data.Item,Domain,Domain.Category,Value)

berry_raw$Value <- as.numeric(berry_raw$Value)
# Replace (D),(NA),(X) and (Z) with NA
berry_raw[berry_raw =="(D)"] <- NA
berry_raw[berry_raw =="(NA)"] <- NA
berry_raw[berry_raw =="(X)"] <- NA
berry_raw[berry_raw =="(Z)"] <- NA
strawberry_raw <- berry_raw %>% filter(Commodity=="STRAWBERRIES")
strawberry_raw2 <- strawberry_raw %>% drop_na()
item_pre <- strawberry_raw2$Data.Item
# Replace "-" with "," for the convenience of spliting
item <- gsub(" - ",",",item_pre)
# Type of the strawberry
type_stberry <- str_extract_all(item,"(BEARING){1}")
strawberry_raw2$unit <- str_extract_all(item,"MEASURED IN.*[^, /AVG]|ACRES.*")
# Also, we have to delate the comma and space
strawberry_raw2$unit <- str_replace(strawberry_raw2$unit,",","")
strawberry_raw2$unit <- trimws(strawberry_raw2$unit)
strawberry_raw2$unit <- as.character(strawberry_raw2$unit)
strawberry_sum <- strawberry_raw2 %>%
    group_by(unit)%>%
    summarize(
        count=n(),
        value=sum(Value)
    )
strawberry_unit <- strawberry_raw2 %>%
    group_by(unit)%>%
    summarize(
        state=State,
        year= Year,
        count=n(),
        value=Value
    )
strawberry_unit_CWT <- filter(strawberry_unit,unit=="MEASURED IN $ / CWT" )
strawberry_unit_CWT$value <- as.numeric(strawberry_unit_CWT$value)
strawberry_unit_CWT$value[strawberry_unit_CWT$value ==0] <- NA # Replace 0 with NA
strawberry_unit_CWT_new <- group_by(strawberry_unit_CWT,year,state)
strawberry_CWT <- summarize(strawberry_unit_CWT_new, value = mean(value, na.rm = TRUE))
strawberry_unit_APPLICATION <- filter(strawberry_unit,unit=="MEASURED IN LB / ACRE / APPLICATION" )
strawberry_unit_APPLICATION$value <- as.numeric(strawberry_unit_APPLICATION$value)
strawberry_unit_APPLICATION$value[strawberry_unit_APPLICATION$value ==0] <- NA # Replace 0 with NA
strawberry_unit_APPLICATION_new <- group_by(strawberry_unit_APPLICATION,year,state)
strawberry_APPLICATION <- summarize(strawberry_unit_APPLICATION_new, value = mean(value, na.rm = TRUE))


# Define UI ----
ui <- fluidPage(
    titlePanel("Analysis of Berries Dataset"),
    verticalLayout(
        mainPanel(
            tabsetPanel(
                id = 'dataset',
                tabPanel("strawberry data",
                         
                         # Create a new Row in the UI for selectInputs
                         fluidRow(
                             column(4,
                                    selectInput("measurement",
                                                "kinds of measurement:",
                                                c("All",
                                                  unique(as.character(strawberry_unit$unit))))
                             ),
                             column(4,
                                    selectInput("year",
                                                "Year:",
                                                c("All",
                                                  unique(strawberry_unit$year)))
                             ),
                             column(4,
                                    selectInput("state",
                                                "State:",
                                                c("All",
                                                  unique(strawberry_unit$state)))
                             ),
                         ),
                         # Create a new row for the table.
                         DT::dataTableOutput("table1"))),
            h3("1. Overview of the classification of the measurement."),
            h4("1.1 two point plot of the measurement"),
            h5("Having a roughly knowledge of the value of each measurement after grouping the measurement"),
            plotOutput("p1"),
            plotOutput("p2"),
            h5("The first plot shows that the number of each measurement. While the second plot show that the value of each measurement. this has a little meaning, but I think we have to have a roughly knowledge of the value of each measurement. The huge difference can show why I need to separate the 'item' variable."),
            
            h4("1.2 Box plot of each knid of measurement"),
            h5("A box plot."),
            plotOutput("p3"),
            h5("This plot is just let us have a knowledge that which measurement can be use as a variable in the futher exploring analysis. Since the data that are have much outliers or the range of the data are not great can make a misunderstanding of the model."),
           
            h3("2. EDA for the measurement is  $/CWT"),
            h4("2.1 Bar plot of Year"),
            plotOutput("p4"),
            h5("Making a plot that the x-label is year, the y-label is value and different color means difference states."),

            h4("2.2  Bar plot of state"),
            plotOutput("p5"),
            h5("Making a plot that the x-label is each state, the y-label is value and different color means difference years."),
           
            h4("2.3 box plot of state"),
            plotOutput("p6"),
            h5("By looking at this plot, there are servel states that the data of tem are not very great. However, since I have not separate the year of the variable.")
        )  
    )
)

# Define server logic required to draw a plot
server <- function(input, output) {
    strawberry_unit$unit <- as.character(strawberry_unit$unit)
    strawberry_unit$year <- as.numeric(strawberry_unit$year)
    strawberry_unit$state <- as.character(strawberry_unit$state)
    # Filter data based on selections
    output$table1 <- DT::renderDataTable(
        DT::datatable({
            data <- strawberry_unit
            
            if (input$year != "All") {
                data <- data[data$year == input$year,]
            }

            
            data
        }))
    output$p1 <- renderPlot({
        ggplot(data = strawberry_sum, mapping = aes(x = unit, y = count))+
            geom_point(shape=21,fill="black", alpha = 1/3)
    })
    
    output$p2 <- renderPlot({
        ggplot(data = strawberry_sum, mapping = aes(x = unit, y = value, size = count)) +
            geom_point(shape=21,fill="cornsilk", alpha = 1/3) 
    })
    output$p3 <- renderPlot({
        ggplot(strawberry_unit, aes(x = unit, y = value, fill=unit))+ 
            geom_boxplot(outlier.colour = NA,notch = TRUE) +
            theme(axis.text.x = element_text(angle = 60, hjust = 1),
                  axis.text = element_text(size = 7),
                  axis.title = element_text(size = 13, face = "bold")) +
            coord_cartesian(ylim = c(0, 1000)) +
            guides(fill=FALSE)
    })

    output$p4 <- renderPlot({
        ggplot(strawberry_CWT, aes(x = year, y = value, fill=state)) +
            geom_bar(stat = "identity")
    })
    output$p5 <- renderPlot({
        ggplot(strawberry_CWT, aes(x =state , y = value, fill=year)) +
            geom_bar(stat = "identity")+
            theme(axis.text.x = element_text(angle = 60, hjust = 1),
                  axis.text = element_text(size = 11),
                  axis.title = element_text(size = 13, face = "bold"))
    })
    output$p6 <- renderPlot({
        ggplot(strawberry_CWT, aes(x =state, y = value,fill=state))+ 
            geom_boxplot(width=0.5, outlier.colour = NA) +
            theme(axis.text.x = element_text(angle = 60, hjust = 1),
                  axis.text = element_text(size = 11),
                  axis.title = element_text(size = 13, face = "bold")) +
            coord_cartesian(ylim = c(0, 250)) +
            labs(x = "strawberry_CWT of year")+
            guides(fill= FALSE)
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
