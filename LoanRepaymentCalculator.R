#### Set up required packages ####
library(shiny)
library(bslib)
library(shinydashboard)
library(DT)
library(tidyverse)

#### Generate internal functions ####
# Compound growth calculator - incomplete functionality
compound = function(base, rate_percent, cap_freq = NA, time_mo, 
                    additional_mo = NULL, additional_int = 0, plot = T) {
  rate = (rate_percent * 0.01) / 12
  temp = base
  yr_bal = rep(NA, time_mo + 1)
  yr_bal[1] = temp
  i = 1
  
  # Growth rate without contributions. I.e. growth on starting value
  if (is.null(additional_mo)){
    while(i <= time_mo) {
      temp = temp + (temp * rate)
      try((yr_bal[i + 1] = temp))
      i = 1 + i
    }
  } else  # Growth rate with monthly contributions
    { while(i <= time_mo) {
      temp = temp + (temp * rate) + (additional_mo)
      try((yr_bal[i + 1] = temp))
      i = 1 + i
    }
    }
  
  # Generate a plot if desired and return annual values regardless of plot inclusion
  if (plot == T){
    p.plot = plot(y = yr_bal, x = 0:(i - 1), type = 'l', xlab = "Months", ylab = 'Dollars')
    return(list(FinalTotal = temp, YearlyBalance = yr_bal, Month = 0:(i - 1), p.plot))
  } else{
    return(list(FinalTotal = temp, YearlyBalance = yr_bal, Month = 0:(i - 1)))
  }
}

#### UI generation ####
ui = dashboardPage(
     dashboardHeader(title = "Financial calculator"),
     dashboardSidebar(

      # Inputs:
      sliderInput(
      inputId = 'Repay',
      label = "Monthly repayment",
      min = 0,
      max = 1000,
      value = 100,
      ticks = F,
      step = 10),
    
      sliderInput(
      inputId = "Adjust",
      label = "Monthly repayment adjustment increment:",
      min = 0,
      max = 1000,
      value = 10,
      ticks = F, 
      step = 10), 
    
      sliderInput(
      inputId = "N_est",
      label = "Number of increments:",
      min = 1,
      max = 15,
      value = 10,
      ticks = F),
    
      selectInput(
      inputId = 'Type',
      label = "Type of input data:",
      choices = c('Loans', 'Investment'),
      selected = 'Loans')
  ),
  
  dashboardBody(fluidPage(
  # Some information about the functionality of the calculator
  card("Information: Plots the change in cumulative balance over time under 
       different contribution values. Treats interest capitalization on a 
       monthly basis which may be conservative if capitalization is more frequent.
       Interest is assumed to be added to the principal at the end of the month
       as are contributions. Contributions are assumed to be equally distributed
       across each individual line item."),
  
  # Some instructions
  card(HTML("Instructions for use: <br>
    1. Upload a .csv file with your loan/investment information: <br>
    2. Select the amount you plan to contribute monthly. <br>
    3. Select the magnitude of the adjustment to monthly contributions <br>
    4. Select the number of adjustments you would like to display. <br>")),
  
  # The preamble to the file upload
  "File must contain a row for each loan/investment with different interest 
  rates. Columns with current principal value (titled 'Principal'), expected 
  annual interest rates as a percent (titled 'Rates'),and months to project 
  out to (titled 'Time_remaining')",
  
  # Give option to upload a file
  fileInput("file", "Select a CSV"),
  
  # Set to include the plot 
  plotOutput(outputId = "Plot" ),
  
  "Find the change in value of a dollar for a given interest rate and time period.",
  
  div(style = "width: 100%; height: 1vh"),
  
  tags$div(
  numericInput(
    inputId = 'Rate', 
    label = "Enter the annual interest rate as percent", 
    value = 0, 
    min = 0, max = 100), style="display:inline-block" ), 
  
  tags$div(
    numericInput(
      inputId = 'Time', 
      label = "Enter the length of time in years", 
      value = 0, 
      min = 0, 
      max = 60), style="display:inline-block"
  ),
  
  # Return the expected value of the dollar 
  textOutput(outputId = 'ExpectedValue'),
  
  # Little bit of info about the value of paying early
  "Why does this matter? Paying an extra dollar on the principal of a loan will 
  decrease the total amount paid in interest over the life of the loan. 
  For example, at a 10% interest rate over 10 years, $1 in principal will grow 
  to $2.59. So by paying $1 at year 0, you will save $1.59 over paying it at the
  end. This scales as well; $100 at 10% for 10 years will grow to $259. By 
  paying $100 extra on the principal early in the life of the loan, you save 
  yourself $159. In contrast, paying an extra dollar with only a year left on 
  the 10 year loan will only save you 10 cents.",
  
  # Add some spacing at the bottom
  div(style = "width: 100%; height: 10vh")
  )))

#### Server generation ####
server = function(input, output) {
  
  input_file = reactive({
    # Make sure file is input before trying to generate plots
    if (is.null(input$file)) {
      return("")
    }
    # Read the file
    read.csv(file = input$file$datapath)
  })
  
  # Generating the plot
  output$Plot = renderPlot({
    
    # Require the input file
    req(input_file())
    
    # Set the input file to a dataframe
    loans = input_file()
    
    # Find average interest rate
    avg_rate = weighted.mean(loans$Rates, w = loans$Principal)
    
    # Generate the contribution values
    amount = input$Repay
    amts = amount + round((input$Adjust * 0:(input$N_est - 1)))
    
    # Set some colors
    mypal <- colorRampPalette(c("red", "green", "blue" ))(input$N_est)
    
    # Generate empty plot with correct sizes for the different calculator types (loan v. investment). 
    plot(NA, ylim = c(0, ifelse(input$Type == 'Loans', 
                                sum(loans$Principal, na.rm = T)*2, 
                                sum(loans$Principal, na.rm = T)*40)),
             xlim = c(0, max(loans$Time_remaining) + 5),
             xlab = "Time to payoff (Months)", 
             ylab = "Cumulative balance ($)")
    
    # Set some scales to make the plot more readable
    abline(v = seq(0, max(loans$Time_remaining), 12), col = alpha('black', 0.2))
    text(x = seq(0, max(loans$Time_remaining), 12), 
         y = ifelse(input$Type == 'Loans', 
                    rep(sum(loans$Principal, na.rm = T) * 1.95), 
                    rep(sum(loans$Principal, na.rm = T) * 39.5)), 
         labels = paste("Year\n", 0:round(max(loans$Time_remaining) / 12)), 
         col = alpha('black', 0.3), 
         adj = 0)
    
    # Generate the data using the compound growth function
    for (i in 1:length(amts)){
      temp = compound(base = sum(loans$Principal, na.rm = T),
                      rate_percent = avg_rate, 
                      time_mo = max(loans$Time_remaining) + 3, 
                      additional_mo = ifelse(input$Type =='Loans',
                                             -amts[i],
                                             amts[i]), 
                      plot = F)
      
      # Add the repayment line to the plot
      lines(y = c(sum(loans$Principal, na.rm = T), temp$YearlyBalance), 
            x = c(0, temp$Month), col = mypal[i], lwd = 2)
    }
   
   # Add a legend to the plot
   legend(x = ifelse(input$Type == 'Loans', max(loans$Time_remaining) * 0.85, 0),
          y = ifelse(input$Type == 'Loans', 
                     sum(loans$Principal, na.rm = T) * 1.8, 
                     sum(loans$Principal, na.rm = T) * 37),
          legend = paste("$",amts), 
          lwd = 3, 
          col = mypal, 
          cex = 1, 
          title = "Monthly payment ($)")
    abline(h = 0)
  })
  
  output$ExpectedValue = renderText({
    ev = round(((1+input$Rate/100)**input$Time), 4)
    paste('The expected value of a dollar at an interest of', input$Rate, '% for a total of', input$Time, 'years is: $', ev)
  })
  
}

#### Launch the App ####
shinyApp(ui, server)
