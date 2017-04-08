
library(httr)

api_key <- readLines('zoopla-api-key.txt')

# Many inputs are strings with percentages for readability.
# These two functions allow to easily go to and fro between numbers and percentages (strings).
to_percent <- function(x) paste0(as.character(x*100), '%')
perc_to_numeric <- function(x) as.numeric(gsub('%', '', x))/100

years_to_int <- function(x) as.numeric(gsub(' years', '', x))
int_to_years <- function(x) paste0(x, ' years')

get_mortgage_data <- function(rate_yearly, duration_years, principal) {
  # Takes in mortgage parameters and outputs a dataframe with useful 
  # quantities over the whole duration of the mortgage at monthly granularity.
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


get_buy_out_running <- function(p) {
  r <- p$rate_yearly/12
  N <- p$duration_years*12
  P <- p$house_price*(1 - p$down_payment)
  
  monthly_payment <- r/(1 - (1 + r)^(-N))*P
  yearly_mortgage_payments <- rep(monthly_payment*12, p$duration_years)
  # The amount for maintenance of the first year grows with inflation
  maintenance_cost <- p$house_price*p$maintenance_rate*
    (1 + p$inflation_rate)^(1:p$duration_years - 1)
  home_insurance_cost <- p$house_price*p$home_insurance_rate*
    (1 + p$inflation_rate)^(1:p$duration_years - 1)
  
  # the running costs of the buying scenario can be invested in the renting scenario
  buy_out_running <- yearly_mortgage_payments + maintenance_cost + home_insurance_cost
  return(buy_out_running)
}


get_balance_rent <- function(rent, p) {
  # Given a monthly rent this computes a vector with the total balance at the end of each year
  # in the renting scenario.
  
  buy_out_running <- get_buy_out_running(p)
  
  # To start out with we have the deposit and the one-off buying costs available
  balance_rent <- p$house_price*p$down_payment + p$house_price*p$cost_buying
  for (i in 1:p$duration_years) {
    balance_rent <- c(balance_rent, 
                      tail(balance_rent, 1)*(1 + p$inv_rate_yearly) + 
                        buy_out_running[i] - rent*12*(1 + p$rent_growth_rate)^(i - 1))
  }
  balance_rent
}


final_rent <- function(p) {
  # In: p is a list of parameters from the interface
  # Out: rent_max is the maximum rent before buying is better

  principal <- p$house_price*(1 - p$down_payment)
  mortgage_data <- get_mortgage_data(p$rate_yearly, p$duration_years, principal)
  balance_buy <- get_tot_balance_buy(mortgage_data,
                  p$home_price_growth_rate, p$cost_selling,
                  p$house_price, p$duration_years)

  rent_max <- uniroot(function(rent) {
    get_balance_rent(rent, p)[p$live_in_years + 1] - balance_buy[p$live_in_years]
    }, c(-10000, 100000))$root
  return(rent_max)
}


get_house_price_plot <- function(p) {
  # Plots the change in maximum rent for values around the current house price.
  
  house_prices <- as.integer(seq(from = floor(p$house_price*0.8/1000)*1000,
                      to = floor(p$house_price*1.2/1000)*1000, length.out = 10))
  
  rents <- sapply(house_prices, FUN = function(hp) {
    p_mod <- p
    p_mod$house_price <- hp
    final_rent(p_mod)
    })

  barplot(rents, names.arg = paste0(as.integer(house_prices/1000), 'k'),
          border = NA, col = 'skyblue3', space = 0.3, xlab = 'Home Price', ylab = 'Rent', 
          ylim = c(0, max(rents)*1.2))
}

get_live_in_years_plot <- function(p) {
  # Plots the change in maximum rent for values around the current number of live-in years.
  
  live_in_years_vec <- as.integer(seq(from = max(p$live_in_years - 2, 1),
                                 to = min(p$live_in_years + 2, p$duration_years), by = 1))
  
  rents <- sapply(live_in_years_vec, FUN = function(liy) {
    p_mod <- p
    p_mod$live_in_years <- liy
    final_rent(p_mod)
  })

  barplot(rents, names.arg = live_in_years_vec,
          border = NA, col = 'skyblue3', space = 0.3, xlab = 'Live in years', ylab = 'Rent', 
          ylim = c(0, max(rents)*1.2))
}

get_zed_index <- function(zoopla_content) {
  zvals <- data.frame(months_ago = 0, zed_value = as.numeric(zoopla_content$zed_index))
  zvals <- rbind(zvals, data.frame(months_ago = -3, 
    zed_value = as.numeric(zoopla_content$zed_index_3month)))
  zvals <- rbind(zvals, data.frame(months_ago = -6, 
    zed_value = as.numeric(zoopla_content$zed_index_6month)))
  for (i in 1:5) {
    col_name <- paste0('zed_index_', i, 'year')
    zvals <- rbind(zvals, data.frame(months_ago = -i*12,
      zed_value = as.numeric(zoopla_content[[col_name]])))
  }
  return(zvals)
}

get_hp_growth_rate <- function(hp_input) {
  # A postcode is here identified by having at least three letters and
  # two numbers
  hp_input <- gsub(pattern = '\\s', replacement = '', tolower(hp_input))
  n_letters <- sum(sapply(strsplit(hp_input, ''), FUN = function(l) l %in% letters))
  n_digits <- sum(sapply(strsplit(hp_input, ''), FUN = function(l) l %in% 0:9))
  if (n_letters > 2 & n_digits > 1) {
    # get estimate from Zoopla
    zoopla_out <- GET(url = paste0('http://api.zoopla.co.uk/api/v1/zed_index.js?',
      'postcode=', hp_input, '&output_type=outcode&api_key=', api_key))
    zoopla_cont <- content(zoopla_out, as = 'parsed')
    zvals <- get_zed_index(zoopla_cont)
    # Only use the data for the last year to estimate the growth rate
    # to focus on short term buyers for now
    lm_zval <- lm(zed_value ~ months_ago, data = zvals[1:4, ])
    growth_rate <- lm_zval$coefficients[2]/zvals$zed_value[4]
    return(growth_rate)
  } else {
    return(perc_to_numeric(hp_input))
  }
}

shinyServer(function(input, output, session) {
  
  input_vals <- 
    reactive(list(rate_yearly = perc_to_numeric(input$mortgage_rate),
              duration_years = years_to_int(input$mortgage_duration),
              house_price = input$house_price,
              down_payment = perc_to_numeric(input$down_payment),
              home_price_growth_rate = get_hp_growth_rate(input$home_price_growth_rate),
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
      max_rent <- final_rent(input_vals())
      
      paste0("<font size = 6>",'If you pay less than ',
             '<br>', '<font size = 10><b>', floor(max_rent), '</b><br>', '<font size = 6>',
             ' in rent it would be better to rent')
      }
    )
  
  current_home_price_growth_text <- 
    eventReactive(list(input$btn, input$trigger), {
      paste('Growth rate used in calculation:',
        to_percent(round(input_vals()$home_price_growth_rate, 4)))
    }
    )
  
  output$house_price_plot = renderPlot({
    get_house_price_plot(input_vals())
    })
  
  output$live_in_years_plot = renderPlot({
    get_live_in_years_plot(input_vals())
  })
  
  output$maximum_rent = renderText(final_rent_text())
  
  output$home_price_growth_from_postcode <- renderText(current_home_price_growth_text())
  
})