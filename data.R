library(tidyverse)
library(data.table)
library(readxl)
library(plotly)
library(leaflet)
library(maps)

data <- fread("data/basetable.csv")
cont <- fread("data/continents_custom.csv")
file <- "data/appendices.xlsx"
product <- read_excel(file, sheet = 1, col_names = c("pid", "Product"), skip = 1)

# Fixing country names and joining with the contient dataset
data <- data %>%
    mutate(
        CountryName = str_replace_all(CountryName, "Tunesia", "Tunisia"),
        CountryName = str_replace_all(CountryName, "Moldavia", "Moldova"),
        CountryName = str_replace_all(CountryName, "Holland", "Netherlands"),
        CountryName = str_replace_all(CountryName, "Russian Federation", "Russia"),
    ) %>% 
    left_join(cont, by = "CountryName")

data(world.cities)
data <- world.cities %>%
    filter(capital == 1) %>%
    select(CountryName = country.etc, lat, long) %>%
    mutate(
        CountryName = str_replace_all(CountryName, "UK", "United Kingdom"),
        CountryName = str_replace_all(CountryName, "US Virgin Islands", "United States Virgin Islands"),
        CountryName = str_replace_all(CountryName, "North Macedonia", "FYR Macedonia"),
        CountryName = str_replace_all(CountryName, "Serbia", "Serbia and Montenegro"), 
        # Wikipedia says the capital of Serbia and Montenegro was Belgrade https://en.wikipedia.org/wiki/Serbia_and_Montenegro
    ) %>%
    rbind(list("Diego Garcia", -7.31, 72.41)) %>% 
    # https://geohack.toolforge.org/geohack.php?pagename=Diego_Garcia&params=7_18_48_S_72_24_40_E_type:isle
    right_join(data, by = "CountryName")

# A function to get the avg LOR under the "Know Thy Customer" tab
avg_lor <- function() {
data %>%
    transmute(LOR = as.numeric(FirstActiveDate %--% LastActiveDate, "days")) %>%
    colMeans(na.rm = TRUE) %>% 
    floor()
}

# A function to render the churn rat plot under the "Know Thy Customer" tab 
render_home_plot <- function(gender, region) {
  temp <- data %>%
    filter(
      Gender %in% gender,
      Region %in% region
    ) %>%
    select(UserID, FirstActiveDate, LastActiveDate)

  for (i in min(month(temp$FirstActiveDate), na.rm = T):max(month(temp$LastActiveDate), na.rm = T)) {
    temp <- temp %>%
      mutate(
        "{ i }" := ifelse(ymd(paste0("2005-",i,"-01")) %within% (FirstActiveDate %--% LastActiveDate), 1, 0)
        )
  }

  temp %>%
    select(-c(FirstActiveDate, LastActiveDate)) %>%
    pivot_longer(cols = -1, names_to = "Month", values_to = "BOM") %>%
    mutate(Month = as.numeric(Month)) %>%
    group_by(Month) %>%
    summarize(BOM = sum(BOM, na.rm = T)) %>%
    transmute(
        ChurnMonth = factor(month.name[Month - 1], levels = month.name),
        ChurnRate = 1 - BOM / lag(BOM, default = first(BOM))
    ) %>%
    filter(
        !(ChurnMonth %in% c("January", "February", "September"))
    ) %>%
    plot_ly(x = ~ChurnMonth, y = ~ChurnRate, type = "scatter", mode = "lines+text",
      line = list(color = "#EE6F7E", width = 4),
      text = ~paste0(round(ChurnRate * 100, 2),"%"), textposition = 'top'
    ) %>%
    layout(
      xaxis = list(title = "", showgrid = FALSE),
      yaxis = list(title = "Churn Rate", gridcolor = "#605A52", tickformat = ".00%"),
      paper_bgcolor = "rgba(1,1,1,0)",
      plot_bgcolor = "rgba(1,1,1,0)",
      margin = list(t = 0, b = 50, pad = 3)) %>%
    config(displayModeBar = FALSE)
}

# A helper function to get the categorical range for sliderTextInput
get_spender_range <- function() {
  quants <- ceiling(quantile(data$TotalSpending, 0:4/4))
  ranges <- c()

  for (i in 1:4) {
    ranges <- append(ranges, paste(quants[i], quants[i + 1], sep = "-"))
  }

  labels <- paste(c("Small Spender", "Medium Spender", "Big Spender", "Whale"), ranges, sep = "<br>")

  return(labels)
}

# A function to get the charts related to poker
get_poker_chart <- function(gender, region, country, wealth_slider) {
  temp <- data %>%
    filter(
      Gender %in% gender,
      Region %in% region,
      CountryName %in% country,
      Segment %in% str_extract(wealth_slider, "[a-z A-Z]+(?=<br>)")
    ) %>%
    select(UserID, starts_with("Poker"))

  # A helper function to get the time-series data
  get_transactions <- function(data, var) {
    data %>%
      select(UserID, ends_with(paste0("_", var))) %>%
      pivot_longer(cols = ends_with(paste0("_", var)), names_to = "Month", values_to = var) %>%
      mutate(
        "{ var }" := ifelse(is.na(.data[[var]]), 0, .data[[var]]),
        Month = factor(str_extract(Month, "(?<=_)[a-zA-Z]+(?=_)"), levels = month.abb)
      ) %>%
      group_by(Month) %>%
      summarize("{ var }" := sum(.data[[var]])) %>%
      head(-1)
  }

  trans_data <- get_transactions(temp, "Buying") %>%
    left_join(get_transactions(temp, "Selling"), by = "Month") %>%
    left_join(get_transactions(temp, "BuyingAmount"), by = "Month") %>%
    left_join(get_transactions(temp, "SellingAmount"), by = "Month")

  fig1 <- trans_data %>%
    plot_ly(x = ~Month, y = ~Buying, name = "Buying", type = "scatter", mode = "lines",
      line = list(color = "#40B2EC", width = 4)
    ) %>%
    add_trace(y = ~Selling, name = "Selling", type = "scatter", mode = "lines",
      line = list(color = "#6A76DF", width = 4)) %>%
    layout(
      xaxis = list(title = "", showgrid = FALSE),
      yaxis = list(title = "Number of times buying or selling poker chips", gridcolor = "#605A52"),
      paper_bgcolor = "rgba(1,1,1,0)",
      plot_bgcolor = "rgba(1,1,1,0)",
      margin = list(t = 25, b = 25, pad = 3),
      legend = list(orientation = 'h')) %>%
    config(displayModeBar = FALSE)

  fig2 <- trans_data %>%
    plot_ly(x = ~Month, y = ~BuyingAmount, name = "Buying", type = "scatter", mode = "lines",
      line = list(color = "#40B2EC", width = 4)
    ) %>%
    add_trace(y = ~SellingAmount, name = "Selling", type = "scatter", mode = "lines",
      line = list(color = "#6A76DF", width = 4)) %>%
    layout(
      xaxis = list(title = "", showgrid = FALSE),
      yaxis = list(title = "Amount from buying or selling poker chips", gridcolor = "#605A52"),
      paper_bgcolor = "rgba(1,1,1,0)",
      plot_bgcolor = "rgba(1,1,1,0)",
      margin = list(t = 25, b = 25, pad = 3),
      legend = list(orientation = 'h')) %>%
    config(displayModeBar = FALSE)

  fig3 <- temp %>%
    filter(!is.na(Poker_IsProfiting)) %>%
    group_by(Poker_IsProfiting) %>%
    summarize(Percentage = n()) %>%
    mutate(Label = ifelse(Poker_IsProfiting == 1, "Earning Money from Games", "Losing Money from Games")) %>%
    plot_ly(labels = ~Label, values = ~Percentage, type = 'pie',
      textposition = 'inside',
      textinfo = 'label+value+percent',
      insidetextfont = list(color = '#F8F8F8'),
      marker = list(colors = c("#6A76DF", "#40B2EC"), line = list(color = '#54514E', width = 4)),
      showlegend = FALSE) %>% 
    layout(
      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
      paper_bgcolor = "rgba(1,1,1,0)",
      plot_bgcolor = "rgba(1,1,1,0)",
      margin = list(t = 25, b = 25, r = 25, l = 25, pad = 3)) %>%
    config(displayModeBar = FALSE)

  fig4 <- temp %>%
    filter(!is.na(Poker_LifespanInMonths)) %>%
    group_by(Poker_LifespanInMonths) %>%
    summarize(Customers = n()) %>%
    plot_ly(x = ~Poker_LifespanInMonths, y = ~Customers, type = "bar", marker = list(color = "#40B2EC")) %>%
    layout(
      xaxis = list(title = "", showgrid = FALSE),
      yaxis = list(title = "Customers and Length of Engagement", gridcolor = "#605A52"),
      paper_bgcolor = "rgba(1,1,1,0)",
      plot_bgcolor = "rgba(1,1,1,0)",
      margin = list(t = 25, b = 25, pad = 3)) %>%
    config(displayModeBar = FALSE)

  fig5 <- data %>%
    filter(!is.na(Poker_LifespanInMonths)) %>% {nrow(.) / nrow(data) * 100} %>% round(2) %>% paste0("%")

  return(list(fig1, fig2, fig3, fig4, fig5))
}

# A function to get the charts related to other betting products
get_bett_chart <- function(pid, gender, region, country, wealth_slider) {
  temp <- data %>%
    filter(
      Gender %in% gender,
      Region %in% region,
      CountryName %in% country,
      Segment %in% str_extract(wealth_slider, "[a-z A-Z]+(?=<br>)")
    ) %>%
    select(UserID, starts_with(paste0("BettingProd" ,pid, "_")))

  colnames(temp)[-1] <- str_replace(colnames(temp)[-1], paste0("BettingProd", pid, "_"), "")

  # A helper function to get the time-series data
  get_transactions <- function(data, var) {
  data %>%
    select(UserID, ends_with(paste0("_", var))) %>%
    pivot_longer(cols = ends_with(paste0("_", var)), names_to = "Month", values_to = var) %>%
    mutate(
      "{ var }" := ifelse(is.na(.data[[var]]), 0, .data[[var]]),
      Month = factor(str_extract(Month, "[a-zA-Z]+(?=_)"), levels = month.abb)
    ) %>%
    group_by(Month) %>%
    summarize("{ var }" := sum(.data[[var]]))
  }

  trans_data <- get_transactions(temp, "Stakes") %>%
    left_join(get_transactions(temp, "Winnings"), by = "Month") %>%
    left_join(get_transactions(temp, "Bets"), by = "Month")

  fig1 <- trans_data %>%
    plot_ly(x = ~Month, y = ~Stakes, name = "Stakes", type = "scatter", mode = "lines",
      line = list(color = "#40B2EC", width = 4)
    ) %>%
    add_trace(y = ~Winnings, name = "Winnings", type = "scatter", mode = "lines",
      line = list(color = "#6A76DF", width = 4)) %>%
    layout(
      xaxis = list(title = "", showgrid = FALSE),
      yaxis = list(title = "Amount staked and won", gridcolor = "#605A52"),
      paper_bgcolor = "rgba(1,1,1,0)",
      plot_bgcolor = "rgba(1,1,1,0)",
      margin = list(t = 25, b = 25, pad = 3),
      legend = list(orientation = 'h')) %>%
    config(displayModeBar = FALSE)

  fig2 <- trans_data %>%
    plot_ly(x = ~Month, y = ~Bets, name = "Bets", type = "scatter", mode = "lines",
      line = list(color = "#40B2EC", width = 4)
    ) %>%
    layout(
      xaxis = list(title = "", showgrid = FALSE),
      yaxis = list(title = "Number of times staking money", gridcolor = "#605A52"),
      paper_bgcolor = "rgba(1,1,1,0)",
      plot_bgcolor = "rgba(1,1,1,0)",
      margin = list(t = 25, b = 25, pad = 3),
      legend = list(orientation = 'h')) %>%
    config(displayModeBar = FALSE)

  fig3 <- temp %>%
    filter(!is.na(IsProfiting)) %>%
      group_by(IsProfiting) %>%
      summarize(Percentage = n()) %>%
      mutate(Label = ifelse(IsProfiting == 1, "Earning Money from Games", "Losing Money from Games")) %>%
      plot_ly(labels = ~Label, values = ~Percentage, type = 'pie',
        textposition = 'inside',
        textinfo = 'label+value+percent',
        insidetextfont = list(color = '#F8F8F8'),
        marker = list(colors = c("#6A76DF", "#40B2EC"), line = list(color = '#54514E', width = 4)),
        showlegend = FALSE) %>% 
      layout(
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        paper_bgcolor = "rgba(1,1,1,0)",
        plot_bgcolor = "rgba(1,1,1,0)",
        margin = list(t = 25, b = 25, r = 25, l = 25, pad = 3)) %>%
      config(displayModeBar = FALSE)

  fig4 <- temp %>%
    filter(!is.na(LifespanInMonths)) %>%
      group_by(LifespanInMonths) %>%
      summarize(Customers = n()) %>%
      plot_ly(x = ~LifespanInMonths, y = ~Customers, type = "bar", marker = list(color = "#40B2EC")) %>%
      layout(
        xaxis = list(title = "", showgrid = FALSE),
        yaxis = list(title = "Customers and Length of Engagement", gridcolor = "#605A52"),
        paper_bgcolor = "rgba(1,1,1,0)",
        plot_bgcolor = "rgba(1,1,1,0)",
        margin = list(t = 25, b = 25, pad = 3)) %>%
      config(displayModeBar = FALSE)

  fig5 <- data %>%
    filter(!is.na(.data[[paste0("BettingProd", pid, "_FirstPlayDate")]])) %>% 
    {nrow(.) / nrow(data) * 100} %>% 
    round(2) %>% 
    paste0("%")

  return(list(fig1, fig2, fig3, fig4, fig5))
}

# A function to render charts under the "Question Thy Products" tab
render_prod_chart <- function(pid, gender, region, country, wealth_slider) {
  if (pid == 3) {
    return(get_poker_chart(gender, region, country, wealth_slider))
  } else {
    return(get_bett_chart(pid, gender, region, country, wealth_slider))
  }
}

# A function to render the map under the "Jet Around the Globe" tab
# Ref: https://stackoverflow.com/questions/60317293/add-markers-by-country-name-leaflet-r
# Ref: https://stackoverflow.com/questions/54978367/custom-markers-with-different-shapes-and-colors-in-leaflet-r
render_map <- function() {
  # This is done to give the markers varying sizes based on the number of customers.
  # The different svgs are to make sure the border weight stays the same for all marker sizes.
  icon_mapping <- expand.grid(region = c("Asia", "Americas", "Europe", "Africa", "Oceania"), size = c("16", "24", "36", "48"))
  icon_mapping <- icon_mapping %>%
    mutate(
      url = case_when(
        region == "Europe" ~ "images/Club",
        region == "Asia" ~ "images/Heart",
        region == "Americas" ~ "images/Diamond",
        region == "Africa" ~ "images/Spade",
        region == "Oceania" ~ "images/Heart",
      ),
      url = paste0(url, "_", size, ".png")
      )

  data %>%
    group_by(CountryName) %>%
    summarize(
      region = max(Region),
      lat = max(lat),
      long = max(long),
      n = n()) %>%
    mutate(
      label = paste0(CountryName, " ~ Number of customers in country: ", n),
      size = as.character(cut(
        n, 
        breaks = quantile(n, 0:4/4), 
        labels = c("16", "24", "36", "48"),
        include.lowest = TRUE
      )),
    ) %>%
    left_join(icon_mapping, by = c("region", "size")) %>%
    leaflet(options = leafletOptions(minZoom = 2)) %>%
    addTiles() %>%
    addMarkers(
      lat = ~lat, 
      lng = ~long, 
      label = ~label, 
      icon = ~makeIcon(iconUrl = url, iconWidth = 48, iconHeight = 48),
      ) %>%
    setView(lng = 9.81, lat = 48.31, zoom = 4) # Middle of Europe
}

# A function to renders a table when you click on a marker on the map
render_map_table <- function(lat, lng) {
  filtered <- data %>%
    filter(
      lat == lat,
      long == lng
    )

  isActive <- apply(!is.na(select(filtered, Poker_FirstTransactionDate, ends_with("_FirstPlayDate"))), 2, sum)
  spentAmount <- apply(select(filtered, Poker_TotalBuyingAmount, ends_with("_TotalStakes")), 2, sum, na.rm = T)
  lor <- apply(select(filtered, Poker_LifespanInMonths, ends_with("_LifespanInMonths")), 2, mean, na.rm = T)
  lor[is.na(lor)] <- 0

  tibble(pid = names(lor), isActive, spentAmount, lor) %>%
    filter(isActive != 0) %>%
    mutate(
      pid = str_extract(pid, "\\d+"),
      pid = ifelse(is.na(pid), 3, as.numeric(pid))) %>%
    left_join(product, by = "pid") %>%
    transmute(
      Product,
      "Active Players" := format(isActive, big.mark = ","),
      "Percent Active Players" := sprintf("%.2f%%", round(isActive / nrow(filtered) * 100, 2)),
      "Spent Amount" := format(round(spentAmount, 2), big.mark = ","),
      "Average Playtime" := paste(round(lor, 1), "month(s)")
    ) %>%
    arrange(desc(`Active Players`)) %>%
    head(5)
}

# A function to renders a chart when you click on a marker on the map
render_map_chart <- function(lat, lng) {
  filtered <- data %>%
    filter(
      lat == lat,
      long == lng
    )

  Spendings <- apply(select(filtered, Poker_TotalBuyingAmount, ends_with("_TotalStakes")), 2, sum, na.rm = T)
  Earnings <- apply(select(filtered, Poker_TotalSellingAmount, ends_with("_TotalWinnings")), 2, sum, na.rm = T)

  tibble(pid = names(Spendings), Spendings, Earnings) %>%
    filter(Spendings != 0) %>%
    mutate(
      pid = str_extract(pid, "\\d+"),
      pid = ifelse(is.na(pid), 3, as.numeric(pid))) %>%
    left_join(product, by = "pid") %>%
    plot_ly(x = ~Spendings, y = ~Product, type = 'bar', name = 'Spent', marker = list(color = "#4BC2B0")) %>%
    add_trace(x = ~Earnings, name = 'Earned', marker = list(color = "#B8DE68")) %>%
    layout(
      barmode = 'group',
      xaxis = list(title = "", gridcolor = "#605A52"),
      yaxis = list(title = "", showgrid = FALSE, categoryorder = "total ascending"),
      paper_bgcolor = "rgba(1,1,1,0)",
      plot_bgcolor = "rgba(1,1,1,0)",
      margin = list(t = 25, b = 25, pad = 3)) %>%
    config(displayModeBar = FALSE)
}

# A function to renders the text that displays the country name when you click on a marker on the map
render_country_name <- function(lat, lng) {
  name <- data %>%
    filter(
      lat == lat,
      long == lng
    ) %>%
    select(CountryName)

  name <- unique(name)[[1]]
  paste0("Welcome to ", name, "!")
}

# A function to update the choices in the "region" picker input when you change the product
get_regions <- function(prod) {
  pid <- product[product$Product == prod, "pid"][[1]]
  if (pid == 3) {
    return(unique(filter(data, is.na(Poker_FirstTransactionDate))$Region))
  }
  return(unique(filter(data, is.na(.data[[paste0('BettingProd', pid, '_FirstPlayDate')]]))$Region))
}

# A function to update the choices in the "country" picker input when you change the product and the region
get_countries <- function(prod, reg) {
  pid <- product[product$Product == prod, "pid"][[1]]
  if (pid == 3) {
    return(unique(filter(data, is.na(Poker_FirstTransactionDate), Region %in% reg)$CountryName))
  }
  return(unique(filter(data, is.na(.data[[paste0('BettingProd', pid, '_FirstPlayDate')]]), Region %in% reg)$CountryName))
}

# A function to apply all the filters selected under the fourth tab to the data and return the treated data ready for plotting and tabling
get_chart_table_data <- function(prod, region, country) {
  pid <- product[product$Product == prod, "pid"][[1]]
  if (pid == 3) {
    filtered <- data %>%
      filter(
        Region %in% region,
        CountryName %in% country
      ) %>%
      select(UserID, Region, CountryName, starts_with("Poker_")) %>%
      select(-matches("(?<=_)[a-zA-Z]+(?=_)", perl = TRUE))

    names(filtered)[-c(1,2,3)] <- str_extract(names(filtered)[-c(1,2,3)], "(?<=_)([a-zA-Z]+)")
    names(filtered)[-c(1,2,3)] <- str_replace_all(names(filtered)[-c(1,2,3)], "([a-z0-9])(?=[A-Z])", "\\1 ")

    filtered <- filtered %>% filter(!is.na(`First Transaction Date`))

    return(filtered)
  }

  filtered <- data %>%
    filter(
      Region %in% region,
      CountryName %in% country
    ) %>%
    select(UserID, Region, CountryName, starts_with(paste0("BettingProd", pid, "_"))) %>%
    select(-matches("(?<=_)[a-zA-Z]+(?=_)", perl = TRUE))

  names(filtered)[-c(1,2,3)] <- str_extract(names(filtered)[-c(1,2,3)], "(?<=_)([a-zA-Z]+)")
  names(filtered)[-c(1,2,3)] <- str_replace_all(names(filtered)[-c(1,2,3)], "([a-z0-9])(?=[A-Z])", "\\1 ")

  filtered <- filtered %>% filter(!is.na(`First Play Date`))

  return(filtered)
}

# A function to get the all the plottable variables from the treated data from get_chart_table_data()
get_variables <- function(prod, region, country) {
  get_chart_table_data(prod, region, country) %>%
    select(-contains(c("Date", "Profiting"))) %>%
    names()
}

# A function to get the summary of the variable selected from the treated data from get_chart_table_data()
get_summary_data <- function(prod, region, country, variable) {
  chart_data <- get_chart_table_data(prod, region, country)
  summ <- summary(chart_data[[variable]])

  return(list(
    nrow(chart_data),
    summ["Min."],
    summ["Median"],
    summ["Mean"],
    summ["Max."]
    ))
}

# A function to plot the variable selected from the treated data from get_chart_table_data()
render_var_chart <- function(prod, region, country, variable) {
  chart_data <- get_chart_table_data(prod, region, country)
  plot_ly(x = ~chart_data[[variable]], type = "histogram", marker = list(color = "#EDAF4F"), nbinsx = 30) %>%
  layout(
    xaxis = list(title = "Distribution of selected variable", showgrid = FALSE),
    yaxis = list(title = "", gridcolor = "#605A52"),
    paper_bgcolor = "rgba(1,1,1,0)",
    plot_bgcolor = "rgba(1,1,1,0)",
    margin = list(t = 25, b = 25, pad = 3)) %>%
  config(displayModeBar = FALSE)
}