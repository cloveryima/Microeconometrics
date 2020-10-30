# ...-2...-1...0...1...2...

#nested if else statement
x <- rnorm(1)
if(x > 1){
  answer <- "Greater than 1"
} else {
  
    if(x >= -1){
    answer <- "Between -1 and 1"
    } else {
    answer <- "Smaller than -1"
  }
}

#chaining statement
x <- rnorm(1)
if (x > 1){
  answer <- "Greater than 1"
} else if (x >= -1){
  answer <- "Between -1 and 1"
} else {
  answer <- "Smaller than 1"
}
