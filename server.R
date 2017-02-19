
# Many inputs are strings with percentages for readability.
# These two functions allow to easily go to and fro between numbers and percentages (strings).
to_percent <- function(x) paste0(as.character(x*100), '%')
perc_to_numeric <- function(x) as.numeric(gsub('%', '', x))/100

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
                                inflation_rate, cost_buying, cost_selling,
                                maintenance_rate, live_in_years, home_insurance_rate,
                                house_price)
  # Determines the total value in the buying scenario over the whole mortgae duration.
  # Inputs: * mortgage_data: mortgage data set as output by get_mortgage_data
  #         * home_price_growth_rate and inflation_rate as a decimal
  # Output (numeric): total financial position after live_in_years
  {
  # duration of mortgage in years to keep a yearly tally of finances
  N <- nrow(mortgage_data)/12 
  # The price at which the home can be sold after each year has passed
  price_sold <- house_price*(1 + home_price_growth_rate)^(1:N)
  # From this we need to subtract the cost of selling
  cost_of_selling <- price_sold*cost_selling
  cost_of_buying <- house_price*cost_buying
  # From this the outstanding principal of the mortgage has to be paid off
  remaining_principal <- mortgage_data$remaining_principal[(1:N)*12]
  # The amount for maintenance of the first year grows with inflation
  maintenance_vec <- house_price*maintenance_rate*(1 + inflation_rate)^(1:N)
  maintenance_cost <- sapply(1:N, FUN = function(k) sum(maintenance_vec[1:k])) 
  # Similar to maintenance, home insurance grows with inflation
  home_insurance_vec <- house_price*home_insurance_rate*(1 + inflation_rate)^(1:N)
  home_insurance <- sapply(1:N, FUN = function(k) sum(home_insurance_vec[1:k])) 
  # total interest
  total_interest <- mortgage_data$total_interest[(1:N)*12]
  
  
  tot_mortgage_payment <- mortgage_data$total_payments[(1:N)*12]
  
  house_equity_before_inflation <- price_sold - cost_of_selling -
    cost_of_buying - remaining_principal - maintenance_cost -
    home_insurance - tot_mortgage_payment
  
  return(data.frame(year = 1:N, price_sold = price_sold,
                    total_interest = total_interest,
             cost_of_selling = cost_of_selling, cost_of_buying = cost_of_buying,
             remaining_principal = remaining_principal,
             maintenance_cost = maintenance_cost, home_insurance = home_insurance,
             house_equity_before_inflation = house_equity_before_inflation,
             house_equity = house_equity_before_inflation/(1 + inflation_rate)^(1:N)
             ))
}

get_tot_balance_rent <- function(inv_rate_yearly, down_payment, duration_years, house_price,
                                 inflation_rate) {
  # Determines the total value in the renting scenario over the whole mortgae duration.
  # Inputs: * inv_rate_yearly as a decimal (rate of growth for investing the down payment)
  #         * duration_years: number of years the mortgage runs for
  #         * down_payment and inflation_rate as decimals
  # Output: Dataframe with first column year and second column balance giving
  # the total financial balance in the renting scenario at the end of the year.
  N <- duration_years
  down_payment_investment <- house_price*down_payment*
    ((1 + inv_rate_yearly)/(1 + inflation_rate))^(1:N)
  data.frame(year = 1:N, down_payment_investment = down_payment_investment,
             balance = down_payment_investment)
}

get_maximum_rent <- function(to_spend_on_rent, rent_growth_rate, live_in_years) {
  # Determines the maximum rent that can be paid before buying is financially better.
  # Inputs: * to_spend_on_rent: amount that can be spent on rent over the live_in_years period
  #         * rent_growth_rate as a decimal: yearly growth rate of rent
  #         * live_in_years: number of years the owner lives in the property
  # Output: * maximum amount of rent per month before buying is better
  
  r <- rent_growth_rate
  to_spend_on_rent*r/(12*((1 + r)^live_in_years - 1))
}

main_calc <- function(rate_yearly, duration_years, house_price,
                      home_price_growth_rate, inflation_rate, live_in_years,
                      down_payment, inv_rate_yearly, 
                      rent_growth_rate, cost_buying, cost_selling,
                      maintenance_rate, home_insurance_rate) {
  # Takes inputs from UI and carries out individual calculations to get the overall result
  # of maximum rent that can be paid before buying is better.
  # Inputs: * rate_yearly: yearly mortgage rate as a decimal
  #         * duration_years: mortgage duration in years
  #         * home_price_growth_rate, inflation_rate, down_payment, rent_growth rate as decimals
  #         * inv_rate_yearly: rate of growth of down payment investment as a decimal
  #         * live_in_years: number of years owner plans to live in property before selling
  # Output: * maximum amount of rent per month before buying is better
  
  principal <- house_price*(1 - down_payment)
  mortgage_data <- get_mortgage_data(rate_yearly, duration_years, principal)
  tot_balance_buy <- get_tot_balance_buy(mortgage_data, home_price_growth_rate,
                                         inflation_rate, cost_buying, cost_selling,
                                         maintenance_rate, live_in_years, home_insurance_rate,
                                         house_price)
  tot_balance_rent <- get_tot_balance_rent(inv_rate_yearly, down_payment,
                                           duration_years, house_price,
                                           inflation_rate)
  
  to_spend_on_rent <- tot_balance_rent$balance[live_in_years] - 
    tot_balance_buy$house_equity[live_in_years]
  max_rent <- get_maximum_rent(to_spend_on_rent, rent_growth_rate, live_in_years)
  return(max_rent)
}

rate_yearly <- 0.04
duration_years <- 25
house_price <- 200000
home_price_growth_rate <- 0.02
inflation_rate <- 0.02
live_in_years <- 5
down_payment <- 0.2
inv_rate_yearly <- 0.04
rent_growth_rate <- 0.02
cost_selling <- 0.05
cost_buying <- 0.04
maintenance_rate <- 0.005
home_insurance_rate <- 0.0045

principal <- house_price*(1 - down_payment)
mortgage_data <- get_mortgage_data(rate_yearly, duration_years, principal)
plot(mortgage_data$total_interest, pch = 20, ylim = c(0, principal))
points(mortgage_data$total_payment_towards_principal, pch = 10, col = 'green')

tot_balance_buy <- get_tot_balance_buy(mortgage_data, home_price_growth_rate,
                                       inflation_rate, cost_buying, cost_selling,
                                       maintenance_rate, live_in_years, home_insurance_rate,
                                       house_price)
head(mortgage_data)
head(tot_balance_buy, 20)

tot_balance_rent <- get_tot_balance_rent(inv_rate_yearly, 
                     down_payment, duration_years,
                     house_price, inflation_rate)

head(tot_balance_rent)

tot_balance_rent$balance - tot_balance_buy$house_equity



sapply(c(0.05, 0.1, 0.15, 0.2, 0.25), FUN = function(dp) get_tot_balance_rent(inv_rate_yearly, 
                                  down_payment = dp, duration_years,
                                  house_price, inflation_rate)[])

# main_calc(0.04, 25, 200000, 0.04, 0.02, 5, 0.2, 0.04, 0.02)

get_house_price_plot <- function(rate_yearly, duration_years, house_price, down_payment,
                                 home_price_growth_rate, inflation_rate, live_in_years,
                                 inv_rate_yearly, rent_growth_rate, cost_buying,
                                 cost_selling, maintenance_rate, home_insurance_rate) {
  # Plots the change in maximum rent for values around the current house price.
  
  house_prices <- as.integer(seq(from = floor(house_price*0.8/1000)*1000,
                      to = floor(house_price*1.2/1000)*1000, length.out = 10))
  rents <- sapply(house_prices, FUN = function(hp) {
    main_calc(rate_yearly, duration_years, house_price = hp,
              home_price_growth_rate, inflation_rate, live_in_years,
              down_payment, inv_rate_yearly, 
              rent_growth_rate, cost_buying, cost_selling, maintenance_rate, home_insurance_rate)
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
  
  live_in_years_vec <- as.integer(seq(from = live_in_years - 2,
                                 to = live_in_years + 2, by = 1))
  rents <- sapply(live_in_years_vec, FUN = function(liy) {
    main_calc(rate_yearly, duration_years, house_price,
              home_price_growth_rate, inflation_rate, live_in_years = liy,
              down_payment, inv_rate_yearly, 
              rent_growth_rate, cost_buying, cost_selling, maintenance_rate, home_insurance_rate)
  })

  barplot(rents, names.arg = live_in_years_vec,
          border = NA, col = 'skyblue3', space = 0.3, xlab = 'Live in years', ylab = 'Rent', 
          ylim = c(0, max(rents)*1.2))
}

shinyServer(function(input, output, session) {
  
  get_params <- function() {
    return(list(rate_yearly = perc_to_numeric(input$mortgage_rate),
                duration_years = input$mortgage_duration,
                house_price = input$house_price,
                down_payment = perc_to_numeric(input$down_payment),
                home_price_growth_rate = perc_to_numeric(input$home_price_growth_rate),
                inflation_rate = perc_to_numeric(input$inflation_rate),
                live_in_years = input$live_in_time,
                inv_rate_yearly = perc_to_numeric(input$investment_return_rate),
                rent_growth_rate = perc_to_numeric(input$rent_growth_rate),
                cost_buying = perc_to_numeric(input$buying_cost),
                cost_selling = perc_to_numeric(input$selling_cost),
                maintenance_rate = perc_to_numeric(input$yearly_maintenance_rate),
                home_insurance_rate = perc_to_numeric(input$home_insurannce_rate)))
  }
  
  observeEvent(input$house_price_slider,
               updateNumericInput(session, 'house_price', value = input$house_price_slider)
  )
  
  observeEvent(input$live_in_years_slider,
               updateNumericInput(session, 'live_in_time', value = input$live_in_years_slider)
  )
  
  output$house_price_plot = renderPlot({
    do.call(get_house_price_plot, get_params())
    })
  
  output$live_in_years_plot = renderPlot({
    do.call(get_live_in_years_plot, get_params())
  })
  
  output$maximum_rent = renderText(
    {
      max_rent <- do.call(main_calc, get_params())
      
      paste0("<font size = 6>",'If you pay less than ',
             '<br>', '<font size = 10><b>', floor(max_rent), '</b><br>', '<font size = 6>',
             ' in rent it would be better to rent')
    }
  )
  
})