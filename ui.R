
library(shinyBS)

# Many inputs are strings with percentages for readability.
# These two functions allow to easily go to and fro between numbers and percentages (strings).
to_percent <- function(x) paste0(as.character(x*100), '%')
perc_to_numeric <- function(x) as.numeric(gsub('%', '', x))/100

shinyUI(fluidPage(
  titlePanel(h1('Buy vs. Rent - Demystified!', align = 'center')),
  
  fluidRow(
    column(8,
      fluidRow(
        column(6, 
          wellPanel(
            h4('About the property'),
            strong('House price'),
            numericInput('house_price', label = NULL, value = 250000),
            numericInput('live_in_time', label = 'How long do you plan to stay?',
                         value = 9),
            bsCollapsePanel('How do these factors affect the result?', 
                            plotOutput('house_price_plot', height = '300px'),
                            sliderInput('house_price_slider', 'House price', 
                                        min = 0, step = 1000, max = 3e6, value = 250000),
                            hr(),
                            plotOutput('live_in_years_plot', height = '300px'),
                            sliderInput('live_in_years_slider', 'Live in years', 
                                        min = 0, step = 1, max = 25, value = 9)
            )
          )
        ),
        column(6, 
          wellPanel(
            h4('About your mortgage'),
            textInput('mortgage_rate', 'What is your mortgage rate?',
                      value = to_percent(0.0367)),
            numericInput('mortgage_duration', 'And your mortgage duration?',
                         value = 30),
            textInput('down_payment', 'What is the size of your down payment?',
                      value = to_percent(0.2))
          )
        )
      ),
      fluidRow(
        column(6,
          wellPanel(
            h4('Details about what the future might bring'),
            textInput('investment_return_rate', 'Investment return rate',
               value = to_percent(0.04)),
            textInput('home_price_growth_rate', 'What is the expected
               home price growth rate?', value = to_percent(0.03)),
            textInput('rent_growth_rate', 'What is the rent growth rate?',
               value = to_percent(0.025)),
            textInput('inflation_rate', 'Inflation Rate:', value = to_percent(0.02))
          )
        ),
        column(6,
          wellPanel(
            h4('Costs of buying and selling a home'),
            textInput('buying_cost', 'Cost of buying',
                      value = to_percent(0.04)),
            textInput('selling_cost', 'Cost of selling',
                      value = to_percent(0.06))
          )
        )
      ),
      fluidRow(
        column(12,
               wellPanel(
                 h4('Property maintenance and insurance'),
                 textInput('yearly_maintenance_rate', 'Maintenance/Renovation',
                           value = to_percent(0.01)),
                 textInput('home_insurannce_rate', 'Home Insurance',
                           value = to_percent(0.0045))
               )
        )
      )
    ),
    column(4, wellPanel(htmlOutput('maximum_rent', align = 'center',
                                   style = 'color: #2E8B57;'))
           )
  ),
  theme = 'cerulean.css'
  )
)



