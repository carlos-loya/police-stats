require(data.world)
require(dplyr)

connection <- data.world(token = "eyJhbGciOiJIUzUxMiJ9.eyJzdWIiOiJwcm9kLXVzZXItY2xpZW50OmV4c29yZXN0IiwiaXNzIjoiYWdlbnQ6ZXhzb3Jlc3Q6OmY5ODk0YTlhLWZkNjAtNDI2NC04YTk3LTlhYjUwOWYzODZiZSIsImlhdCI6MTQ4NDY5NzMzNiwicm9sZSI6WyJ1c2VyX2FwaV93cml0ZSIsInVzZXJfYXBpX3JlYWQiXSwiZ2VuZXJhbC1wdXJwb3NlIjp0cnVlfQ.WLINQ3z7bGjvYCmpDR3Fvl3LZ4fFDLBDCngivFE3nfoF1EgGQQ0WCxZElC2bxC3YUoUiYEJ6hz8rxVW3yHoecg")

income <- query(connection, dataset="uscensusbureau/acs-2015-5-e-income",
                query="select State, B19083_001 as GINI, B19301_001 as Per_Capita_Income, B19113_001 as Median_Family_Income, B19202_001 as Median_Non_Family_Income, B19019_001 as Median_Income
                from `USA_All_States` 
                order by Median_Income 
                limit 1000")

fatalPoliceShootings <- query(connection,
                              dataset="robin-stewart/s-17-dv-project-5", type="sql",
                              query="SELECT * FROM `fatal-police-shootings-cleaned.csv/fatal-police-shootings-cleaned` LIMIT 100"
)

incomeOfTheFatallyShot <- dplyr::inner_join(income,fatalPoliceShootings, by = c("State" = "state"))

write.csv(income, file = "income.csv")
write.csv(fatalPoliceShootings, file = "fatalPoliceShootings.csv")
write.csv(incomeOfTheFatallyShot, file = "incomeOfTheFatallyShot.csv")
