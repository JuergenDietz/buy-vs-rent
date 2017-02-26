
# Many inputs are strings with percentages for readability.
# These two functions allow to easily go to and fro between numbers and percentages (strings).
to_percent <- function(x) paste0(as.character(x*100), '%')
perc_to_numeric <- function(x) as.numeric(gsub('%', '', x))/100

years_to_int <- function(x) as.numeric(gsub(' years', '', x))
int_to_years <- function(x) paste0(x, ' years')

get_mortgage_data <- function(rate_yearly, duration_years, principal) {
  # Takes in mortgage parameters and outputs a dataframe with useful 
  # quantities over the whole duration of the mortgage.
  # Columns of output data frame:
  # month: Number of months into the mortgage. The remaining colums show
  #        the state of the mortgage at the end of this month.
  # monthly_payment: the mortgage payment made in this month.
  # remaining_principal: outstanding value of the mortgage which, together with
  #                     interest on it, still has to be paid off.
  # interest: amount payed in interest in this month.
  # payment_towards_principal: amount payed towards the remaining value of the mortgage.
  # total_payments: sum of all monthly payments (interest + principal) made to date
  # total_interest: sum of all payments towards interest to date
  # total_payment_towards_principal: sum of all payments towards principal to date
  r <- rate_yearly/12
  N <- duration_years*12
  # Principal is the house price minus down payment
  P <- principal
  
  monthly_payment <- r/(1 - (1 + r)^(-N))*P
  remaining_principal <- (1 - (1 + r)^(1:N - N))/(1 - (1 + r)^(-N))*P
  
  interest <- r*(1 - (1 + r)^(0:(N - 1) - N))/(1 - (1 + r)^(-N))*P
  payment_towards_principal <- monthly_payment - interest
  total_interest <- sapply(1:N, FUN = function(k) sum(interest[1:k]))
  total_payment_towards_principal <- 
    sapply(1:N, FUN = function(k) sum(payment_towards_principal[1:k]))
  return(
    data.frame(month = 1:N, 
               monthly_payment = rep(monthly_payment, N),
               remaining_principal = remaining_principal,
               interest = interest,
               payment_towards_principal = payment_towards_principal,
               total_payments = 1:N*monthly_payment,
               total_interest = total_interest,
               total_payment_towards_principal = total_payment_towards_principal
               )
  )
}

get_tot_balance_buy <- function(mortgage_data, home_price_growth_rate,
                                cost_selling, house_price, duration_years)
{
  remaining_principal <- mortgage_data$remaining_principal[(1:duration_years)*12]
  # money flowing in for the buying scenario comes from selling the home
  buy_in <- house_price*(1 + home_price_growth_rate)^(1:duration_years)
  # money flowing out that cannot be re-invested in the alternative renting scenario (one-off out)
  buy_out_one_off <- buy_in*cost_selling + remaining_principal

  balance_buy <- buy_in - buy_out_one_off
  return(balance_buy)
}


get_buy_out_running <- function(house_price, duration_years, down_payment, rate_yearly,
                                 maintenance_rate, inflation_rate, home_insurance_rate) {
  r <- rate_yearly/12
  N <- duration_years*12
  P <- house_price*(1 - down_payment)
  
  monthly_payment <- r/(1 - (1 + r)^(-N))*P
  yearly_mortgage_payments <- rep(monthly_payment*12, duration_years)
  # The amount for maintenance of the first year grows with inflation
  maintenance_cost <- house_price*maintenance_rate*(1 + inflation_rate)^(1:duration_years)
  home_insurance_cost <- house_price*home_insurance_rate*(1 + inflation_rate)^(1:duration_years)
  
  # the running costs of the buying scenario can be invested in the renting scenario
  buy_out_running <- yearly_mortgage_payments + maintenance_cost + home_insurance_cost
  return(buy_out_running)
}


get_balance_rent <- function(rent, buy_out_running, house_price, down_payment, cost_buying,
                             inv_rate_yearly, rent_growth_rate, duration_years) {
  # Given a monthly rent this computes a vector with the total balance at the end of each year
  # in the renting scenario
  
  # To start out with we have the deposit and the one-off buying costs available
  balance_rent <- house_price*down_payment + house_price*cost_buying
  for (i in 1:duration_years) {
    balance_rent <- c(balance_rent, 
                      tail(balance_rent, 1)*(1 + inv_rate_yearly) + 
                        buy_out_running[i] - rent*12*(1 + rent_growth_rate)^(i - 1))
  }
  balance_rent
}


final_rent <- function(rate_yearly, duration_years, house_price, down_payment,
                       home_price_growth_rate, inflation_rate, live_in_years,
                       inv_rate_yearly, rent_growth_rate, cost_buying,
                       cost_selling, maintenance_rate, home_insurance_rate) {
  buy_out_running <- get_buy_out_running(house_price, duration_years, 
                                         down_payment, rate_yearly, maintenance_rate,
                                         inflation_rate, home_insurance_rate)
  
  principal <- house_price*(1 - down_payment)
  mortgage_data <- get_mortgage_data(rate_yearly, duration_years, principal)
  balance_buy <- get_tot_balance_buy(mortgage_data, 
                                     home_price_growth_rate, cost_selling, 
                                     house_price, duration_years)
  
  uniroot(function(rent) {
    get_balance_rent(rent, buy_out_running, house_price, down_payment, cost_buying,
                     inv_rate_yearly, rent_growth_rate, duration_years)[live_in_years + 1] -
            balance_buy[live_in_years]
    }, c(-10000, 100000))$root
}


get_house_price_plot <- function(rate_yearly, duration_years, house_price, down_payment,
                                 home_price_growth_rate, inflation_rate, live_in_years,
                                 inv_rate_yearly, rent_growth_rate, cost_buying,
                                 cost_selling, maintenance_rate, home_insurance_rate) {
  # Plots the change in maximum rent for values around the current house price.
  
  house_prices <- as.integer(seq(from = floor(house_price*0.8/1000)*1000,
                      to = floor(house_price*1.2/1000)*1000, length.out = 10))
  
  rents <- sapply(house_prices, FUN = function(hp) {
    final_rent(rate_yearly, duration_years, house_price = hp, down_payment,
               home_price_growth_rate, inflation_rate, live_in_years,
               inv_rate_yearly, rent_growth_rate, cost_buying,
               cost_selling, maintenance_rate, home_insurance_rate)
    })

  barplot(rents, names.arg = paste0(as.integer(house_prices/1000), 'k'),
          border = NA, col = 'skyblue3', space = 0.3, xlab = 'Home Price', ylab = 'Rent', 
          ylim = c(0, max(rents)*1.2))
}

get_live_in_years_plot <- function(rate_yearly, duration_years, house_price, down_payment,
                                   home_price_growth_rate, inflation_rate, live_in_years,
                                   inv_rate_yearly, rent_growth_rate, cost_buying,
                                   cost_selling, maintenance_rate, home_insurance_rate) {
  # Plots the change in maximum rent for values around the current number of live-in years.
  
  live_in_years_vec <- as.integer(seq(from = max(live_in_years - 2, 1),
                                 to = min(live_in_years + 2, duration_years), by = 1))
  
  rents <- sapply(live_in_years_vec, FUN = function(liy) {
    final_rent(rate_yearly, duration_years, house_price, down_payment,
               home_price_growth_rate, inflation_rate, live_in_years = liy,
               inv_rate_yearly, rent_growth_rate, cost_buying,
               cost_selling, maintenance_rate, home_insurance_rate)
  })

  barplot(rents, names.arg = live_in_years_vec,
          border = NA, col = 'skyblue3', space = 0.3, xlab = 'Live in years', ylab = 'Rent', 
          ylim = c(0, max(rents)*1.2))
}

shinyServer(function(input, output, session) {
  
  input_vals <- 
    reactive(list(rate_yearly = perc_to_numeric(input$mortgage_rate),
              duration_years = input$mortgage_duration,
              house_price = input$house_price,
              down_payment = perc_to_numeric(input$down_payment),
              home_price_growth_rate = perc_to_numeric(input$home_price_growth_rate),
              inflation_rate = perc_to_numeric(input$inflation_rate),
              live_in_years = years_to_int(input$live_in_time),
              inv_rate_yearly = perc_to_numeric(input$investment_return_rate),
              rent_growth_rate = perc_to_numeric(input$rent_growth_rate),
              cost_buying = perc_to_numeric(input$buying_cost),
              cost_selling = perc_to_numeric(input$selling_cost),
              maintenance_rate = perc_to_numeric(input$yearly_maintenance_rate),
              home_insurance_rate = perc_to_numeric(input$home_insurannce_rate)))
  
  observeEvent(input$house_price_slider,
               updateNumericInput(session, 'house_price', value = input$house_price_slider)
  )
  
  observeEvent(input$live_in_years_slider,
               updateNumericInput(session, 'live_in_time', 
                                  value = int_to_years(input$live_in_years_slider))
  )
  
  final_rent_text <- 
    eventReactive(list(input$btn, input$trigger), {
      max_rent <- do.call(final_rent, input_vals())
      
      paste0("<font size = 6>",'If you pay less than ',
             '<br>', '<font size = 10><b>', floor(max_rent), '</b><br>', '<font size = 6>',
             ' in rent it would be better to rent')
      }
    )
  
  output$house_price_plot = renderPlot({
    do.call(get_house_price_plot, input_vals())
    })
  
  output$live_in_years_plot = renderPlot({
    do.call(get_live_in_years_plot, input_vals())
  })
  
  output$maximum_rent = renderText(final_rent_text())
  
})