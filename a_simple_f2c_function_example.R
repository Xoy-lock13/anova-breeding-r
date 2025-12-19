#### Example of a simple Fahrenheit to Celsius function

f2c <- function(F) {
  Celsius <- 5 / 9 * (F - 32)
  return(Celsius)
}

f2c(2)
f2c(-40)



gforce <- function(m1, m2, d) {
  G <- 6.02 * 10^(-11)
  F <- G * (m1 * m2) / d^2
  return(F)
}

gforce(2,3, 100)


