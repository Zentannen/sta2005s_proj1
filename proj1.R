#| results: hide
#| warning: false
#| message: false
#| error: false
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}
remotes::install_github("MiguelRodo/DataTidyRodoSTA2005S")
data("data_tidy_energy_use", package = "DataTidyRodoSTA2005S")

#hello my firend


#set out all variables so we can use them without the dollar
consumption_kwh <- data_tidy_energy_use$consumption_kwh
outside_temperature <- data_tidy_energy_use$outside_temperature
humidity <- data_tidy_energy_use$humidity
wind_speed <- data_tidy_energy_use$wind_speed
household_size <- data_tidy_energy_use$household_size
appliance_index <- data_tidy_energy_use$appliance_index
energy_efficiency <- data_tidy_energy_use$energy_efficiency
solar_installed <- data_tidy_energy_use$solar_installed
day_of_week <- data_tidy_energy_use$day_of_week
holiday <- data_tidy_energy_use$holiday



# Section 2
# 1.
hist(consumption_kwh, freq = FALSE)
x_vec <- seq( min(consumption_kwh), max(consumption_kwh),length.out = 1e3)
y_vec <- dnorm(x_vec, mean = mean(consumption_kwh), sd = sd(consumption_kwh))
lines(x_vec, y_vec, col = "red", size = 2) 


dens_yes <- density(consumption_kwh[solar_installed == "yes"], na.rm = TRUE)
dens_no  <- density(df$consumption_kwh[df$solar_installed == "no"], na.rm = TRUE)

# Plot first group
plot(dens_yes, col = "blue", lwd = 2, main = "Consumption (kWh) by Solar Installation",
     xlab = "Consumption (kWh)", ylab = "Density")


# 2. here we have to exclued data that is non numerical otherwise it does not let us plot (we can force data to be numeric but i will ask miguel)
numerical_data <- data_tidy_energy_use[sapply(data_tidy_energy_use, is.numeric)]
pairs(numerical_data)



lm1 = lm(consumption_kwh~outside_temperature,data=data_tidy_energy_use)
summary(lm1)

