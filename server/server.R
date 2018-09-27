# data visualization final project shiny sever

require(ggplot2)
require(dplyr)
require(shiny)
require(shinydashboard)
require(data.world)
require(readr)
require(DT)
require(plotly)
require(hexbin)


connection <- data.world(token = "eyJhbGciOiJIUzUxMiJ9.eyJzdWIiOiJwcm9kLXVzZXItY2xpZW50OmV4c29yZXN0IiwiaXNzIjoiYWdlbnQ6ZXhzb3Jlc3Q6OmY5ODk0YTlhLWZkNjAtNDI2NC04YTk3LTlhYjUwOWYzODZiZSIsImlhdCI6MTQ4NDY5NzMzNiwicm9sZSI6WyJ1c2VyX2FwaV93cml0ZSIsInVzZXJfYXBpX3JlYWQiXSwiZ2VuZXJhbC1wdXJwb3NlIjp0cnVlfQ.WLINQ3z7bGjvYCmpDR3Fvl3LZ4fFDLBDCngivFE3nfoF1EgGQQ0WCxZElC2bxC3YUoUiYEJ6hz8rxVW3yHoecg")

states <- query(connection,dataset="robin-stewart/s-17-dv-project-6", type="sql",
                query="SELECT distinct `fatal-police-shootings-cleaned`.state
                FROM `fatal-police-shootings-cleaned.csv/fatal-police-shootings-cleaned`
                order by 1"
)

stateSelectList <- as.list(states$state)

income <- query(connection,
                dataset="uscensusbureau/acs-2015-5-e-income", type="sql",
                query="select State, B19083_001 as GINI, B19301_001 as Per_Capita_Income, B19113_001 as Median_Family_Income, B19202_001 as Median_Non_Family_Income, B19019_001 as Median_Income
                from `USA_All_States` 
                order by Median_Income 
                limit 1000")

fatalPoliceShootings <- query(connection,
                              dataset="robin-stewart/s-17-dv-final-project", type="sql",
                              query="SELECT * FROM `fatal-police-shootings-cleaned.csv/fatal-police-shootings-cleaned` LIMIT 1000"
)

incomeOfTheFatallyShot <- dplyr::inner_join(income,fatalPoliceShootings, by = c("State" = "state"))

shinyServer(function(input, output) {
  
  #------------------------------------------------------- Begin Histogram Tab -------------------------------------------------------
  
  histogramData <- eventReactive(input$clickHis, {
    
    histo <- incomeOfTheFatallyShot
  })
  output$dataHis <- renderDataTable({DT::datatable(histogramData(), rownames = FALSE,
                                                   extensions = list(Responsive = TRUE, FixedHeader = TRUE)
  )
  })
  output$Histogram <- renderPlot({
    countTotal <- as.data.frame(histogramData())
    
    ggplot(countTotal) + 
      geom_histogram(aes(Per_Capita_Income, fill = Per_Capita_Income), binwidth = 700) + 
      ggtitle("Count of the per capita of Fatal Police Shooting Individuals")
    
  })
  #------------------------------------------------------- End Histogram Tab -------------------------------------------------------
  
  
  
  
  #------------------------------------------------------- Begin Box Plots Tab -------------------------------------------------------
  
  boxData <- eventReactive(input$clickBox, {
    
    boxDataSet <- incomeOfTheFatallyShot %>% dplyr::select(flee, Median_Family_Income)
    
  })
  output$dataBox <- renderDataTable({DT::datatable(boxData(), rownames = FALSE,
                                                   extensions = list(Responsive = TRUE, FixedHeader = TRUE)
  )
  })
  output$plotBox <- renderPlot({
    countTotal <- as.data.frame(boxData())
    
    ggplot(countTotal) + geom_boxplot(aes(x = flee, y = Median_Family_Income, fill = flee)  ) + ggtitle("Median Family Income Boxplot of Individuals who suffered from \nFatal Police Shootings in 2015. \n\nThe x axis is the fleeing type, i.e. if anyone was fleeing and if so how.")
    
  })
  #------------------------------------------------------- End Box Plots Tab -------------------------------------------------------
  

  
  
  #------------------------------------------------------- Begin Scatter Plots Tab -------------------------------------------------------
  
  dataScatter <- eventReactive(input$clickScatter, {
    
   scatters <- incomeOfTheFatallyShot %>% dplyr::select(GINI, Median_Family_Income, armed)
    
  })
  output$dataScatter <- renderDataTable({DT::datatable(dataScatter(), rownames = FALSE,
                                                       extensions = list(Responsive = TRUE, FixedHeader = TRUE)
  )
  })

  output$scatterPlot1 <- renderPlot({
    df <- as.data.frame(dataScatter())
    ggplot(df) + geom_point(aes(x = GINI, y = Median_Family_Income, color = armed))  + ggtitle("This plot shows the median family income vs. the gini index for individuals \nfrom fatal police shootings in 2015. \n\nThe color is the weapon said individuals were armed with.")
  })
  output$scatterPlot2 <- renderPlot({
    df <- as.data.frame(dataScatter())
    brush = brushOpts(id="plot_brush", delayType = "throttle", delay = 30)
    bdf=brushedPoints(df, input$plot_brush)
    
    if( !is.null(input$plot_brush) ) {
      df %>% dplyr::filter(df$GINI %in% bdf[, "GINI"]) %>% ggplot() + geom_point(aes(x = GINI, y = Median_Family_Income, color = armed)) 
    } 
  })
  #------------------------------------------------------- End Scatter Plots Tab -------------------------------------------------------
  
  

  
  
  #------------------------------------------------------- Begin Crosstabs, KPIs, Parameters Tab -------------------------------------------------------
  
  dataCross <- eventReactive(input$clickCross, {
    
    tdf = query(connection,
                dataset="uscensusbureau/acs-2015-5-e-income", type="sql",
                query="select State, B19083_001 as GINI, B19301_001 as Per_Capita_Income, B19113_001 as Median_Family_Income, B19202_001 as Median_Non_Family_Income, B19019_001 as Median_Income
                from `USA_All_States` 
                order by Median_Income 
                limit 1000")
    
  })
  output$dataCross <- renderDataTable({DT::datatable(dataCross(), rownames = FALSE,
                                                     extensions = list(Responsive = TRUE, FixedHeader = TRUE)
  )
  })
  
  
  dataKPIs <- eventReactive(input$clickKPIs, {
    genderMentalIll <- dplyr::select(incomeOfTheFatallyShot, Per_Capita_Income, gender, signs_of_mental_illness)
    countTotal <- genderMentalIll %>% mutate(Per_Capita_Range = ifelse(Per_Capita_Income < 26500, "low", ifelse(Per_Capita_Income < 31000 & Per_Capita_Income > 26500, "medium","high"))) %>% dplyr::count(Per_Capita_Range,gender, signs_of_mental_illness)
  })
  output$dataKPIs <- renderDataTable({DT::datatable(dataKPIs(), rownames = FALSE,
                                                    extensions = list(Responsive = TRUE, FixedHeader = TRUE)
  )
  })
  output$KPIPlot <- renderPlot({
    countTotal <- as.data.frame(dataKPIs())
    
    lowCapitaRange <- countTotal %>% filter(Per_Capita_Range == "low")
    mediumCapitaRange <- countTotal %>% filter(Per_Capita_Range == "medium")
    highCapitaRange <- countTotal %>% filter(Per_Capita_Range == "high")
    ggplot() + 
      geom_text(data = lowCapitaRange, colour="#CC0000", aes(x=gender, y=signs_of_mental_illness, label = n),nudge_x = -0.2, size=10) + 
      geom_text(data = mediumCapitaRange, colour="000099", aes(x=gender, y=signs_of_mental_illness, label = n),nudge_x = 0, size=10) + 
      geom_text(data = highCapitaRange, colour="blue", aes(x=gender, y=signs_of_mental_illness, label = n),nudge_x = 0.2, size=10)  + ggtitle("This plot shows the signs of mental illness vs. gender for individuals \nfrom fatal police shootings in 2015. \n\nThe text is ranges of count of per capita income for the individuals in \neach category. Red is the low per capita income, green is the middle \nand blue is the high.") 
    
  })
  
  dataSets <- eventReactive(input$clickSets, {
    incomeOfTheFatallyShot %>% 
      dplyr::group_by(gender, race) %>% 
      dplyr::summarize(avg_median_income = mean(Median_Income))
    
  })
  output$dataSets <- renderDataTable({DT::datatable(dataSets(), rownames = FALSE,
                                                    extensions = list(Responsive = TRUE, FixedHeader = TRUE)
  )
  })
  output$setPlot <- renderPlot({
    escapePlot <- as.data.frame(dataSets())
    subset <- dplyr::inner_join(income,fatalPoliceShootings, by = c("State" = "state")) %>% dplyr::filter(Median_Income >= 46000 & Median_Income <= 62000) %>%
      dplyr::group_by(gender, race) %>% 
      dplyr::summarize(avg_median_income = mean(Median_Income)) 
    ggplot() + 
      geom_text(data = escapePlot, aes(x= gender, y=race, label = avg_median_income), size=8) + 
      geom_text(data = subset, aes(x=gender, y=race, label = avg_median_income), nudge_y = -.5, size=4)   + ggtitle("This plot shows the race vs. gender for individuals \nfrom fatal police shootings in 2015. \n\nThe large text Represents being Part of the mean of the top 25% \nHighest Median Incomes. Smaller text is the average of all \nremaining median incomes.")
    
  })
  dataPara <- eventReactive(input$clickPara, {
    paraData <- incomeOfTheFatallyShot %>% dplyr::group_by(race,flee) %>% dplyr::summarise(income = median(Median_Income), MedianFamilyIncomePerCapitaIncomeRatio = median(Median_Family_Income/Per_Capita_Income))

    
  })
  output$dataPara <- renderDataTable({DT::datatable(dataPara(), rownames = FALSE,
                                                    extensions = list(Responsive = TRUE, FixedHeader = TRUE)
  )
  })
  output$paraPlot <- renderPlot({
    
    escapePlot <- as.data.frame(dataPara())
    
    ggplot(escapePlot) + 
      theme(axis.text.x=element_text(angle=90, size=16, vjust=0.5)) + 
      theme(axis.text.y=element_text(size=16, hjust=0.5)) +
      geom_text(aes(x=race, y=flee, label = income), size=6)+
      geom_tile(aes(x=race, y=flee, fill=MedianFamilyIncomePerCapitaIncomeRatio), alpha=0.50)  + ggtitle("This R visualization was created using the calculated fields of \nMedian(MedianFamilyIncome/PerCapitaIncome) and \nplotting based on how the individual from the fatal police \nshooting fled against the race of the individual shot.")
    
  })
  
  #------------------------------------------------------- End Crosstabs, KPIs, Sets, Parameters Tab -------------------------------------------------------
  

  
  
  
  #------------------------------------------------------- Begin Bar Charts and Table Calculations Tab -------------------------------------------------------
  
  dataBar <- eventReactive(input$clickBar, {
    
    tdf = query(connection,
                dataset="uscensusbureau/acs-2015-5-e-income", type="sql",
                query="select State, B19083_001 as GINI, B19301_001 as Per_Capita_Income, B19113_001 as Median_Family_Income, B19202_001 as Median_Non_Family_Income, B19019_001 as Median_Income
                from `USA_All_States` 
                order by Median_Income 
                limit 1000")
    
  })
  output$dataBar <- renderDataTable({DT::datatable(dataBar(), rownames = FALSE,
                                                   extensions = list(Responsive = TRUE, FixedHeader = TRUE)
  )
  })
  
  dataTabCal <- eventReactive(input$clickTabCal, {
    
    incomeByRace <- incomeOfTheFatallyShot %>% dplyr::group_by(race, gender) %>% dplyr::summarize(avg_median_income = mean(Median_Income), sum_income = sum(Median_Income)) %>% dplyr::group_by(race, gender, avg_median_income) %>% dplyr::summarize(window_avg_income = mean(sum_income))
    
  })
  
  output$dataTabCal <- renderDataTable({DT::datatable(dataTabCal(), rownames = FALSE,
                                                      extensions = list(Responsive = TRUE, FixedHeader = TRUE)
  )
  })
  
  output$tabCalPlot <- renderPlot({
    
    escapePlot <- as.data.frame(dataTabCal())
    
    ggplot(escapePlot, aes(x = gender, y = avg_median_income)) + 
      geom_bar(stat = "identity") +
      scale_y_continuous(labels = scales::comma) + 
      facet_wrap(~race, ncol=1) +
      coord_flip() +
      geom_text(mapping=aes(x=gender, y=avg_median_income, label=round(avg_median_income - window_avg_income)),colour="blue", hjust=-.5) + ggtitle("This plot shows the race vs. gender for individuals \nfrom fatal police shootings in 2015 the bars are the \naverage median income. \n\nThe blue numbers are a table calculation (the sum of the median \nincome - the window average of the median income).")
    
  })
  
  dataRefLine <- eventReactive(input$clickRefLine, {
    
    fleeMentalIncome <- incomeOfTheFatallyShot %>% dplyr::select(flee,signs_of_mental_illness,Median_Income) %>% group_by(signs_of_mental_illness,flee) %>% dplyr::filter(flee %in% c('Car','Foot','Not fleeing')) %>% summarise(Median_income = median(Median_Income))
    
    
    
  })
  output$dataRefLine <- renderDataTable({DT::datatable(dataRefLine(), rownames = FALSE,
                                                       extensions = list(Responsive = TRUE, FixedHeader = TRUE)
  )
  })
  
  output$refLinePlot <- renderPlot({
    
    escapePlot <- as.data.frame(dataRefLine())
    
    ggplot(escapePlot, aes(x=signs_of_mental_illness, y=Median_income, fill=signs_of_mental_illness)) +
      theme(axis.text.x=element_text(angle=0, size=12, vjust=0.5)) + 
      theme(axis.text.y=element_text(size=12, hjust=0.5)) + 
      geom_bar(stat = "identity") + 
      facet_wrap(~flee, ncol=1) + 
      coord_flip() + 
      geom_hline(aes(yintercept = median(Median_income)), color="purple")  + ggtitle("Income of Individuals from a Fatal Police Shooting. \nBroken up by if they were feeling and/or had signs of mental illness.")
    
  })
  
  dataIdSet <- eventReactive(input$clickIdSet, {
    
    inequalityIndexforHighIncome <- incomeOfTheFatallyShot %>% dplyr::select(id,GINI,Median_Income) %>% mutate(Median_Income_Range = ifelse(Median_Income < 50000, "low", ifelse(Median_Income < 60000 & Median_Income > 50000, "medium","high"))) %>% dplyr::filter(Median_Income_Range == 'high',id > 1000) 
    
  })
  output$dataIdSet <- renderDataTable({DT::datatable(dataIdSet(), rownames = FALSE,
                                                     extensions = list(Responsive = TRUE, FixedHeader = TRUE)
  )
  })
  
  output$idSetPlot <- renderPlot({
    
    escapePlot <- as.data.frame(dataIdSet())
    
    ggplot(escapePlot, aes(x=id, y=GINI, fill=Median_Income)) +
      theme(axis.text.x=element_text(angle=0, size=12, vjust=0.5)) + 
      theme(axis.text.y=element_text(size=12, hjust=0.5)) +
      geom_bar(stat = "identity")  + ggtitle("Inequality Index For High Income Individuals from 2015 Fatal Police \nShootings. Each ID represents a person.")
    plot(inequalityPlot)
    
  })
  
  #------------------------------------------------------- End Bar Charts and Table Calculations Tab -------------------------------------------------------
  
  
})