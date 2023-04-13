# First page
header <- function() {
  div(class = "top-bar",
      div(class = "top-bar-icons",
        div(
          img(src = "images/Heart.svg", class = "suits", onclick="$('li:eq(1) a').tab('show');"),
          p("Know Thy Customers", class = "top-bar-text"),
        ),
        div(
          img(src = "images/Diamond.svg", class = "suits", onclick="$('li:eq(2) a').tab('show');"),
          p("Question Thy Products", class = "top-bar-text"),
        ),
        div(
          img(src = "images/Club.svg", class = "suits", onclick="$('li:eq(3) a').tab('show');"),
          p("Jet Around The Globe", class = "top-bar-text"),
        ),
        div(
          img(src = "images/Spade.svg", class = "suits", onclick="$('li:eq(4) a').tab('show');"),
          p("Analyze in Depth", class = "top-bar-text"),
        ),
      )
  )
}

footer <- function() {
  tagList(
    div(class = "footer",
      "Created by Nam Nguyen."
    )
  )
}

knowThyCustomers <- function() {
  tagList(
    header(),
    div(class = "content",
      div(class = "left-wrapper",
        div(class = "rank-suit",
          p("K", class = "card-rank"),
          img(src = "images/Heart.svg", class = "suits")
        ),
        div(class = "borders",
          div(p(HTML("&nbsp;"))),
          div(class = "left-border heart", p(HTML("&nbsp;")))
        )
      ),
      div(class = "main-panel",
        div(class = "welcome",
          div(h1("Know Thy Customers"), class = "left"),
          div(class = "left", p("As marketers, it is your job to improve the business's bottom line. 
          The following data mart was built to serve that exact purpose. The datasets used to build this 
          application come from the collaborative Internet gambling research project between the Division on Addictions 
          (DOA) and bwin Interactive Entertainment, AG (bwin), an Internet betting service provider headquartered in Vienna, 
          Austria. These datasets provide evidence from the first eight months of the first prospective longitudinal, 
          real-time, Internet sports betting behavior study that took place from February 1, 2005 through September 30, 2005."))
        ),
        h2("Glimpse at important statistics"),
        div(class = "big-numbers",
          div(
            div(textOutput("nbr_cust")),
            div(textOutput("ttl_revenue")),
            div(textOutput("avg_lor"))
          ),
          div(
            div(p("Customers recorded throughout the study")),
            div(p("Total revenue collected over 8 months")),
            div(p("Average length of relationship in days"))
          )
        ),
        div(class = "page-end",
          div(class = "home-graph",
            h2("Churn rate throughout the months"),
            div(class = "input-box",
              pickerInput("gender", "Select Gender", 
                unique(data$Gender),
                unique(data$Gender),
                TRUE, 
                list(`actions-box` = TRUE)),
              pickerInput("region", "Select Region", 
                unique(data$Region),
                unique(data$Region),
                TRUE,
                list(`actions-box` = TRUE))
            ),
            plotlyOutput("drop_off")
          )
        )
      ),
      div(class = "right-wrapper",
        div(class = "borders",
          div(class = "right-border heart", p(HTML("&nbsp;"))),
          div(p(HTML("&nbsp;")))
        ),
        div(class = "rank-suit flipped",
          p("K", class = "card-rank"),
          img(src = "images/Heart.svg", class = "suits")
        )
      )
    ),
    footer()
  )
}

# Second page
questionThyProducts <- function() {
  tagList(
    header(),
    div(class = "content",
      div(class = "left-wrapper",
        div(class = "rank-suit",
          p("Q", class = "card-rank"),
          img(src = "images/Diamond.svg", class = "suits")
        ),
        div(class = "borders",
          div(p(HTML("&nbsp;"))),
          div(class = "left-border diamond", p(HTML("&nbsp;")))
        )
      ),
      div(class = "main-panel",
        div(class = "welcome",
          div(class = "centered",
            h1("Question Thy Products"),
            p(HTML("Are our products captivating customers? Do they spend enough money on gambling? 
            Let's dive in!<br><br>Find out how many people engage with a particular product. 
            How long they stick with the product and their spending and earning behaviors through the month.<br><br>
            You might also be interested in only a portion of the customers, maybe the high-spending ones?"))
          )
        ),
        div(class = "chart-selector",
          div(class = "filters",
            pickerInput("gender2", "Select Gender", 
              unique(data$Gender),
              unique(data$Gender),
              TRUE, 
              list(`actions-box` = TRUE)),
            pickerInput("region2", "Select Region", 
              unique(data$Region),
              unique(data$Region),
              TRUE,
              list(`actions-box` = TRUE)),
            pickerInput("country2", "Select Country", 
              unique(data$CountryName),
              unique(data$CountryName),
              TRUE,
              list(`actions-box` = TRUE)),
          ),
          sliderTextInput(
            "wealth_slider",
            "Specify Range of Customer Segment",
            choices = get_spender_range(),
            selected = c(get_spender_range()[1], get_spender_range()[4]),
            width = "100%"
          )
        ),
        div(class = "product-selector",
          actionButton("prod1", "Sports book fixed-odd"),
          actionButton("prod2", "Sports book live-action"),
          actionButton("prod3", "Poker BossMedia"),
          actionButton("prod4", "Casino BossMedia"),
          actionButton("prod5", "Supertoto"),
          actionButton("prod6", "Games VS"),
          actionButton("prod7", "Games bwin"),
          actionButton("prod8", "Casino Chartwell"),
        ),
        div(uiOutput("chart_section"), class = "chart-section")
      ),
      div(class = "right-wrapper",
        div(class = "borders",
          div(class = "right-border diamond", p(HTML("&nbsp;"))),
          div(p(HTML("&nbsp;")))
        ),
        div(class = "rank-suit flipped",
          p("Q", class = "card-rank"),
          img(src = "images/Diamond.svg", class = "suits")
        )
      )
    ),
    footer()
  )
}

render_poker_chart_UI <- function() {
  tagList(
    div(class = "chart-row-1",
      div(
        div(class = "pct-player",
          p("Percent of customers playing this game"),
          textOutput("pct_player"),
        ),
        plotlyOutput("pie_chart_tl")
      ),
      div(
        h2("Number of Months Active in Game"),
        plotlyOutput("bar_chart_tr")
      ),
    ),
    div(class = "chart-row-2",
      div(
        h2("Number of Transactions"),
        plotlyOutput("line_chart_bl")
      ),
      div(
        h2("Transaction Amount"),
        plotlyOutput("line_chart_br")
      )
    )
  )
}

render_bett_chart_UI <- function() {
  tagList(
    div(class = "chart-row-1",
      div(
        div(class = "pct-player",
          p("Percent of customers playing this game"),
          textOutput("pct_player"),
        ),
        plotlyOutput("pie_chart_tl")
      ),
      div(
        h2("Number of Months Active in Game"),
        plotlyOutput("bar_chart_tr")
      ),
    ),
    div(class = "chart-row-2",
      div(
        h2("Amount staked and won"),
        plotlyOutput("line_chart_bl")
      ),
      div(
        h2("Number of Bets Made"),
        plotlyOutput("line_chart_br")
      )
    )
  )
}

# Third page
jetAround <- function() {
  tagList(
    header(),
    div(class = "content",
      div(class = "left-wrapper",
        div(class = "rank-suit",
          p("J", class = "card-rank"),
          img(src = "images/Club.svg", class = "suits")
        ),
        div(class = "borders",
          div(p(HTML("&nbsp;"))),
          div(class = "left-border club", p(HTML("&nbsp;")))
        )
      ),
      div(class = "main-panel",
        div(class = "welcome",
          div(class = "centered",
            h1("Jet Around The Globe"),
            p(HTML(
              "Explore statistics for all countries recorded in our database.<br><br>
              Click on one of the markers in the map below to start exploring data for that country."
            )),
            leafletOutput("map", height = 400),
          )
        ),
        uiOutput("map_chart")
      ),
      div(class = "right-wrapper",
        div(class = "borders",
          div(class = "right-border club", p(HTML("&nbsp;"))),
          div(p(HTML("&nbsp;")))
        ),
        div(class = "rank-suit flipped",
          p("J", class = "card-rank"),
          img(src = "images/Club.svg", class = "suits")
        )
      )
    ),
    footer()
  )
}

render_map_chart_UI <- function() {
  tagList(
    textOutput("country_name"),
    div(
      div(
        h2("Top 5 most popular products in the country"),
        tableOutput("table_top")
      ),
      div(
        h2("Spending and earning amount for all products"),
        plotlyOutput("bar_chart_btm", height = "350px")
      )
    )
  )
}

# Fourth page
analyzeInDepth <- function() {
  tagList(
    header(),
    div(class = "content",
      div(class = "left-wrapper",
        div(class = "rank-suit",
          p("A", class = "card-rank"),
          img(src = "images/Spade.svg", class = "suits")
        ),
        div(class = "borders",
          div(p(HTML("&nbsp;"))),
          div(class = "left-border spade", p(HTML("&nbsp;")))
        )
      ),
      div(class = "main-panel",
        div(class = "welcome",
          div(class = "centered",
            h1("Analyze in Depth"),
            p(HTML(
              "Explore every variable individually.<br><br>
              Start by changing some of the filters then choose a variable you want to explore in depth."
              ))
          ),
        ),
        div(class = "interactive-area",
          div(class = "controls",
            div(class = "var_filters",
              div(
                h2("Filter"),
                pickerInput("var_prod", "Select Product",
                  product$Product,
                  NULL,
                  FALSE, 
                  list(`actions-box` = TRUE)),
                pickerInput("var_reg", "Select Region", 
                  c(),
                  c(),
                  TRUE, 
                  list(`actions-box` = TRUE)),
                pickerInput("var_ctry", "Select Country", 
                  c(),
                  c(),
                  TRUE, 
                  list(`actions-box` = TRUE)),
                h2("Select Variable"),
                pickerInput("var_var", "Select one",
                  # get_var_list(),
                  # get_var_list(),
                  c(),
                  c(),
                  FALSE, 
                  list(`actions-box` = TRUE)),
              )
            ),
            actionButton("gen_chart", "Generate Chart"),
            actionButton("gen_table", "Generate Table")
          ),
          uiOutput("gen_chart_table")
        ),
      ),
      div(class = "right-wrapper",
        div(class = "borders",
          div(class = "right-border spade", p(HTML("&nbsp;"))),
          div(p(HTML("&nbsp;")))
        ),
        div(class = "rank-suit flipped",
          p("A", class = "card-rank"),
          img(src = "images/Spade.svg", class = "suits")
        )
      )
    ),
    footer()
  )
}

render_table_UI <- function() {
  DT::dataTableOutput("var_table")
} 

render_chart_UI <- function() {
  tagList(
    div(class = "summary",
      div(
        h4("N"),
        textOutput("sum_n")
      ),
      div(
        h4("Min"),
        textOutput("sum_min")
      ),
      div(
        h4("Median"),
        textOutput("sum_med")
      ),
      div(
        h4("Mean"),
        textOutput("sum_avg")
      ),
      div(
        h4("Max"),
        textOutput("sum_max")
      )
    ),
    plotlyOutput("var_chart")
  )
} 