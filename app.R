library(tidyverse)
library(shiny)
library(plotly)

# read the disaster data and the map data
disaster = read.csv('data/disaster.csv') %>%
    select(-X)
mapdata = read.csv('data/Location.csv') %>%
    select(-X)
GDP = read.csv('data/GDP.csv') %>%
    select(-X) %>%
    pivot_longer(cols = X1960:X2020, values_to = 'gdp', names_to = 'year') %>%
    mutate(year = as.numeric(substr(year, 2, 5)))

# generate a certain disaster matrix
disaster_raw = disaster %>%
    group_by(Year,Country) %>%
    summarize(deaths = sum(Total.Deaths, na.rm = T),
              damage = sum(Total.Damages, na.rm = T),
              damageadj = sum(Total.Damages.Adjusted, na.rm = T)) %>%
    unique()

# generate the plot data
country = sort(unique(mapdata$region))
data = data.frame(Year = rep(seq(1900,2022), each = length(country)), Country = rep(country, 123))
data1 = data %>% left_join(disaster_raw)
data1[is.na(data1)] = 0
data2 = data1 %>% left_join(GDP, by = c('Year'='year', 'Country'='Country.Name'))

# join the region and country
rawdata = left_join(mapdata,data2,by=c("region" = "Country"))
rawdata$Year = as.numeric(rawdata$Year)

# Define UI for application that draws a histogram
ui <- fluidPage('The Display of Geographical disaster data',

    # Application title
    titlePanel("Data Map"),
    helpText('A map for basic display for different variable'),
    
    sidebarLayout(
        sidebarPanel(h3('user input'),
            radioButtons(inputId = 'Variable',
                     label = 'Variable', 
                     choices = c('GDP','Damage','Adjusted Damage','Deaths','Covid'),
                     inline = T),
            sliderInput("Year",
                    "Year:",
                    min = 1900,
                    max = 2020,
                    value = 2020)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotlyOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    df = reactive(subset(rawdata, Year == input$Year))

    output$distPlot <- renderPlotly({
        # generate the ggplot
        if(input$Variable == 'GDP'){
            g = ggplot(df(), aes(x = long, y = lat, group = group, text = region))+ 
                # take longitude and latitude as x and y, a certain region as a group
                geom_polygon(data = df(), aes(fill = log10(gdp)), color = 'black')+
                # draw a map filled by log10(deaths), and separate each country by black lines
                scale_fill_gradient(low = '#FFF68F',high = '#FC4902') +
                # use a common used heat map color setting
                labs(title = 'Number of log GDP for Every Country')+
                # rename the plot
                ggdark::dark_theme_bw() 
        } else if(input$Variable == 'Damage'){
            g = ggplot(df(), aes(x = long, y = lat, group = group, text = region))+ 
                # take longitude and latitude as x and y, a certain region as a group
                geom_polygon(data = df(), aes(fill = log10(damage)), color = 'black')+
                # draw a map filled by log10(deaths), and separate each country by black lines
                scale_fill_gradient(low = '#FFF68F',high = '#FC4902') +
                # use a common used heat map color setting
                labs(title = 'Number of log Damage for Every Country')+
                # rename the plot
                ggdark::dark_theme_bw()
        } else if(input$Variable == 'Adjusted Damage'){
            g = ggplot(df(), aes(x = long, y = lat, group = group, text = region))+ 
                # take longitude and latitude as x and y, a certain region as a group
                geom_polygon(data = df(), aes(fill = log10(damageadj)), color = 'black')+
                # draw a map filled by log10(deaths), and separate each country by black lines
                scale_fill_gradient(low = '#FFF68F',high = '#FC4902') +
                # use a common used heat map color setting
                labs(title = 'Number of log adjusted Damage for Every Country')+
                # rename the plot
                ggdark::dark_theme_bw() 
        } else if(input$Variable == 'Deaths'){
            g = ggplot(df(), aes(x = long, y = lat, group = group, text = region))+ 
                # take longitude and latitude as x and y, a certain region as a group
                geom_polygon(data = df(), aes(fill = log10(deaths)), color = 'black')+
                # draw a map filled by log10(deaths), and separate each country by black lines
                scale_fill_gradient(low = '#FFF68F',high = '#FC4902') +
                # use a common used heat map color setting
                labs(title = 'Number of log Deaths for Every Country')+
                # rename the plot
                ggdark::dark_theme_bw() 
        }
        
        ggplotly(g, tooltip = c("text",'fill'))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
