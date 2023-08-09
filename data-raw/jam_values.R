## code to prepare `jam_values` dataset goes here

# https://docs.google.com/spreadsheets/d/1YX3NBDkkoDXHs88KDfPS_QoS9-1j_C_q8UAyjPznfzA/edit?usp=sharing

jam_values <- tibble::tribble(
    ~value,            ~meaning,                                                                      ~use,     ~type,  ~units, ~year,
        0L,         "1 or less",                                               "Age, Duration of Marriage", "minimum", "years", 2021L,
        9L,       "9.0 or more",                                                                   "Rooms", "maximum", "rooms", 2021L,
       10L,      "10.0 or less", "Gross Rent as Percentage of Income, Owner Costs as Percentage of Income", "minimum",      NA, 2021L,
       50L,      "50.0 or more", "Gross Rent as Percentage of Income, Owner Costs as Percentage of Income", "maximum",      NA, 2021L,
       99L,       "100 or less",   "Rent, Gross Rent, Selected Monthly Owner Costs, Monthly Housing Costs", "minimum",   "USD", 2021L,
      101L,       "101 or more",                                                    "Duration of Marriage", "maximum", "years", 2021L,
      116L,       "115 or more",                                                                     "Age", "maximum", "years", 2021L,
      199L,       "200 or less",                                                                     "Tax", "minimum",   "USD", 2021L,
     1001L,     "1,000 or more",                                            "Selected Monthly Owner Costs", "maximum",   "USD", 2021L,
     1939L,   "1939 or earlier",                                                              "Year Built", "minimum",  "year", 2021L,
     1969L,   "1969 or earlier",                                                           "Year Moved In", "minimum",  "year", 2021L,
     2001L,     "2,000 or more",                                                        "Rent, Gross Rent", "maximum",   "USD", 2021L,
     2010L,     "2010 or later",                                               "Year Built, Year Moved In", "maximum",  "year", 2021L,
     2499L,     "2,500 or less",                                                        "Income, Earnings", "minimum",   "USD", 2021L,
     4001L,     "4,000 or more",                     "Selected Monthly Owner Costs, Monthly Housing Costs", "maximum",   "USD", 2021L,
     9999L,    "10,000 or less",                                                                   "Value", "minimum",   "USD", 2021L,
    10001L,    "10,000 or more",                                                                     "Tax", "maximum",   "USD", 2021L,
   200001L,   "200,000 or more",                                                                  "Income", "maximum",   "USD", 2021L,
   250001L,   "250,000 or more",                                                        "Income, Earnings", "maximum",   "USD", 2021L,
  1000001L, "1,000,000 or more",                                                                   "Value", "maximum",   "USD", 2021L
  )


usethis::use_data(jam_values, overwrite = TRUE)
