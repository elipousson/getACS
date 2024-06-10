# multiplication works

    Code
      edu_collapse_data
    Output
      # A tibble: 5 x 26
        NAME      label variable estimate   moe perc_estimate perc_moe GEOID column_id
        <chr>     <fct> <list>      <dbl> <dbl>         <dbl>    <dbl> <lis> <list>   
      1 Baltimor~ Total <chr>      410221   57           1       NA    <chr> <chr [1]>
      2 Baltimor~ 5th ~ <chr>        1775  392.          0       NA    <chr> <chr [5]>
      3 Baltimor~ 6th ~ <chr>        7588  770.          0.01    NA    <chr> <chr [3]>
      4 Baltimor~ 9th ~ <chr>       27937 1504.          0.08    NA    <chr> <chr [3]>
      5 Baltimor~ Other <chr>      372921 5328.          0.91     0.02 <chr> <chr>    
      # i 17 more variables: table_id <list>, geography <list>, county <list>,
      #   state <list>, table_title <list>, simple_table_title <list>,
      #   subject_area <list>, universe <list>, denominator_column_id <list>,
      #   topics <list>, line_number <list>, column_title <list>, indent <list>,
      #   parent_column_id <list>, denominator_estimate <list>,
      #   denominator_moe <list>, denominator_column_title <list>

