# gt_acs works

    Code
      tbl1[["_options"]]
    Output
      # A tibble: 194 x 5
         parameter              scss  category type   value     
         <chr>                  <lgl> <chr>    <chr>  <list>    
       1 table_id               FALSE table    value  <chr [1]> 
       2 table_caption          FALSE table    value  <chr [1]> 
       3 table_width            TRUE  table    px     <chr [1]> 
       4 table_layout           TRUE  table    value  <chr [1]> 
       5 table_margin_left      TRUE  table    px     <chr [1]> 
       6 table_margin_right     TRUE  table    px     <chr [1]> 
       7 table_background_color TRUE  table    value  <chr [1]> 
       8 table_additional_css   FALSE table    values <chr [0]> 
       9 table_font_names       FALSE table    values <chr [10]>
      10 table_font_size        TRUE  table    px     <chr [1]> 
      # i 184 more rows

---

    Code
      tbl2[["_options"]]
    Output
      # A tibble: 194 x 5
         parameter              scss  category type   value     
         <chr>                  <lgl> <chr>    <chr>  <list>    
       1 table_id               FALSE table    value  <chr [1]> 
       2 table_caption          FALSE table    value  <chr [1]> 
       3 table_width            TRUE  table    px     <chr [1]> 
       4 table_layout           TRUE  table    value  <chr [1]> 
       5 table_margin_left      TRUE  table    px     <chr [1]> 
       6 table_margin_right     TRUE  table    px     <chr [1]> 
       7 table_background_color TRUE  table    value  <chr [1]> 
       8 table_additional_css   FALSE table    values <chr [0]> 
       9 table_font_names       FALSE table    values <chr [10]>
      10 table_font_size        TRUE  table    px     <chr [1]> 
      # i 184 more rows

# gt_acs_compare works

    Code
      tbl3[["_boxhead"]]
    Output
                                           var    type column_label column_units
      1                           column_title default column_title         <NA>
      2      estimate_Baltimore city, Maryland default         Est.         <NA>
      3           moe_Baltimore city, Maryland  hidden          MOE         <NA>
      4 perc_estimate_Baltimore city, Maryland  hidden      % share         <NA>
      5      perc_moe_Baltimore city, Maryland  hidden        % MOE         <NA>
      6                      estimate_Maryland default         Est.         <NA>
      7                           moe_Maryland  hidden          MOE         <NA>
      8                 perc_estimate_Maryland  hidden      % share         <NA>
      9                      perc_moe_Maryland  hidden        % MOE         <NA>
        column_pattern column_align column_width hidden_px
      1           <NA>         left         NULL      NULL
      2           <NA>        right         NULL      NULL
      3           <NA>        right         NULL      NULL
      4           <NA>        right         NULL      NULL
      5           <NA>        right         NULL      NULL
      6           <NA>        right         NULL      NULL
      7           <NA>        right         NULL      NULL
      8           <NA>        right         NULL      NULL
      9           <NA>        right         NULL      NULL

