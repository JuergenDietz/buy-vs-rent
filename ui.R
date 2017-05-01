
library(shinyBS)

# Many inputs are strings with percentages for readability.
# These two functions allow to easily go to and fro between numbers and percentages (strings).
to_percent <- function(x)
  paste0(as.character(x * 100), '%')
perc_to_numeric <- function(x)
  as.numeric(gsub('%', '', x)) / 100

shinyUI(fluidPage(
  titlePanel(
    h1('Buy vs. Rent - Demystified!', align = 'center'),
    tags$head(tags$title("Buy vs. Rent - Demystified"))
  ),
  
  tabsetPanel(
    tabPanel('Calculate Maximum Rent',
      fluidRow(column(12, wellPanel('Press "Calculate!" after changing
        any numbers!', align = 'center', style = 'color: #7F0000;'))),
      fluidRow(
        column(4,
          wellPanel(
            htmlOutput('maximum_rent', align = 'center',
              style = 'color: #2E8B57;'),
            actionButton("btn", "Calculate!", align = 'center')
          ),
          conditionalPanel(condition = 'true == false',
            textInput('trigger', '', width = 0))
        ),
        column(8,
          fluidRow(
            column(6,
              wellPanel(
                h4('About the property'),
                numericInput('house_price', label = 'House Price', value = 250000, step = 1000),
                textInput('live_in_time', label = 'How long do you plan to stay?',
                  value = '9 years')
              )
            ),
            column(6,
              wellPanel(
                h4('About your mortgage'),
                textInput('mortgage_rate', 'What is your mortgage rate?',
                  value = to_percent(0.0367)),
                textInput('mortgage_duration', 'And your mortgage duration?',
                  value = '30 years'),
                textInput(
                  'down_payment',
                  'What is the size of your down payment?',
                  value = to_percent(0.2)
                )
              )
            )
          ),
          fluidRow(column(6,
            wellPanel(
              h4('Details about what the future might bring'),
              textInput(
                'investment_return_rate',
                'Investment return rate',
                value = to_percent(0.04)
              ),
              textInput(
                'home_price_growth_rate',
                'What is the expected
                home price growth rate? For the UK, 
                get last year\'s estimate by entering a postcode.',
                value = to_percent(0.03)
              ),
              textInput(
                'rent_growth_rate',
                'What is the rent growth rate?',
                value = to_percent(0.025)
              ),
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
              ))),
          fluidRow(column(6,
            wellPanel(
              h4('Property maintenance and insurance'),
              textInput(
                'yearly_maintenance_rate',
                'Maintenance/Renovation',
                value = to_percent(0.01)
              ),
              textInput('home_insurance_rate', 'Home Insurance',
                value = to_percent(0.0045))
            )),
            column(6)
          )
        )
      )
    ),
    tabPanel('Play with the numbers!',
      bsCollapsePanel('Strongest influencers',
        fluidRow(
          column(4,
            wellPanel('House Price:',
              plotOutput('house_price_plot', height = '300px'),
              sliderInput(
                'house_price_slider',
                'House price',
                min = 0,
                step = 1000,
                max = 3e6,
                value = 250000
              )
            )
          ),
          column(4,
            wellPanel('How many years do you plan to stay?',
              plotOutput('live_in_years_plot', height = '300px'),
              sliderInput(
                'live_in_years_slider',
                'Live in years',
                min = 0,
                step = 1,
                max = 25,
                value = 9
              )
            )
          ),
          column(4,
            wellPanel('What is your mortgage rate?',
              plotOutput('mortgage_rate_plot', height = '300px'),
              sliderInput(
                'mortgage_rate_slider',
                'Mortgage Rate (%)',
                min = 0,
                step = 0.25,
                max = 10,
                value = 3.67
              )
            )
          )
        ),
        fluidRow(
          column(4,
            wellPanel('House Price Growth Rate:',
              plotOutput('house_price_growth_plot', height = '300px'),
              sliderInput(
                'house_price_growth_slider',
                'House Price Growth Rate (%)',
                min = -5,
                step = 0.1,
                max = 20,
                value = 3
              )
            )
          ),
          column(4,
            wellPanel('Maintenance/Renovation',
              plotOutput('maintenance_plot', height = '300px'),
              sliderInput(
                'maintenance_slider',
                'Maintenance Costs (%)',
                min = 0,
                step = 0.5,
                max = 8,
                value = 1
              )
            )
          ),
          column(4,
            wellPanel('Home Insurance',
              plotOutput('home_insurance_plot', height = '300px'),
              sliderInput(
                'home_insurance_slider',
                'Home Insurance (%)',
                min = 0,
                step = 0.05,
                max = 2,
                value = 0.45
              )
            )
          )
        )
      ),
      bsCollapsePanel('Some of the secondary influencers',
        fluidRow(
          column(4,
            wellPanel('Rent Growth Rate:',
              plotOutput('rent_growth_plot', height = '300px'),
              sliderInput(
                'rent_growth_slider',
                'Rent Growth Rate (%)',
                min = -2,
                step = 0.5,
                max = 5,
                value = 2.5
              )
            )
          ),
          column(4,
            wellPanel('Investment Return Rate:',
              plotOutput('investment_return_plot', height = '300px'),
              sliderInput(
                'investment_return_slider',
                'Investment Return Rate (%)',
                min = 0,
                step = 1,
                max = 30,
                value = 4
              )
            )
          )
        ),
        fluidRow(
          column(4,
            wellPanel('What is your mortgage duration?',
              plotOutput('mortgage_duration_plot', height = '300px'),
              sliderInput(
                'mortgage_duration_slider',
                'Mortgage Duration (Years)',
                min = 5,
                step = 1,
                max = 40,
                value = 30
              )
            )
          ),
          column(4,
            wellPanel('What is the size of your down payment?',
              plotOutput('down_payment_plot', height = '300px'),
              sliderInput(
                'down_payment_slider',
                'Down Payment (%)',
                min = 5,
                step = 1,
                max = 100,
                value = 20
              )
            )
          )
        )
      )
    )
  ),
  theme = 'cerulean.css'))
