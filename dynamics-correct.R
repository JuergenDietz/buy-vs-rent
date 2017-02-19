

rate_yearly <- 0.0364
duration_years <- 30
house_price <- 250000
home_price_growth_rate <- 0.03
inflation_rate <- 0.02
live_in_years <- 9
down_payment <- 0.2
inv_rate_yearly <- 0.04
rent_growth_rate <- 0.025
cost_selling <- 0.06
cost_buying <- 0.04
maintenance_rate <- 0.01
home_insurance_rate <- 0.0046


r <- rate_yearly/12
N <- duration_years*12
# Principal is the house price minus down payment
P <- house_price*(1 - down_payment)

monthly_payment <- r/(1 - (1 + r)^(-N))*P

## RENTING
year <- 0:N
yearly_mortgage_payments <- rep(monthly_payment*12, duration_years)
# The amount for maintenance of the first year grows with inflation
maintenance_cost <- house_price*maintenance_rate*(1 + inflation_rate)^(1:duration_years)
home_insurance_cost <- house_price*home_insurance_rate*(1 + inflation_rate)^(1:duration_years)

buy_out_running <- yearly_mortgage_payments + maintenance_cost + home_insurance_cost

get_balance_rent <- function(rent) {
  balance_rent <- house_price*down_payment + house_price*cost_buying
  for (i in 1:duration_years) {
    balance_rent <- c(balance_rent, 
                              tail(balance_rent, 1)*(1 + inv_rate_yearly) + 
                                buy_out_running[i] - rent*12*(1 + rent_growth_rate)^(i - 1))
  }
  balance_rent
}

get_balance_rent(500)

## Buying
remaining_principal <- (1 - (1 + r)^(1:N - N))/(1 - (1 + r)^(-N))*P
remaining_principal <- remaining_principal[(1:duration_years)*12]

buy_in <- house_price*(1 + home_price_growth_rate)^(1:duration_years)
buy_out_one_off <- buy_in*cost_selling + remaining_principal

balance_buy <- buy_in - buy_out_one_off

## Putting both together:
rent_minus_buy <- function(rent, live_in_years){
  get_balance_rent(rent)[live_in_years + 1] - balance_buy[live_in_years]
}

rent_minus_buy(800, live_in_years)

final_rent <- function(live_in_years) {
  uniroot(function(rent) rent_minus_buy(rent, live_in_years), c(0, 10000))$root
}

final_rent(10)


