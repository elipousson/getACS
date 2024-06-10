# gt_acs works

    Code
      tbl1[["_options"]]
    Output
      # A tibble: 190 x 5
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
      # i 180 more rows

---

    Code
      tbl2[["_options"]]
    Output
      # A tibble: 190 x 5
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
      # i 180 more rows

# gt_acs_compare works

    Code
      tbl3[["_boxhead"]]
    Output
      # A tibble: 9 x 8
        var   type  column_label column_units column_pattern column_align column_width
        <chr> <chr> <list>       <chr>        <chr>          <chr>        <list>      
      1 colu~ defa~ <chr [1]>    <NA>         <NA>           left         <NULL>      
      2 esti~ defa~ <chr [1]>    <NA>         <NA>           right        <NULL>      
      3 moe_~ hidd~ <chr [1]>    <NA>         <NA>           right        <NULL>      
      4 perc~ hidd~ <chr [1]>    <NA>         <NA>           right        <NULL>      
      5 perc~ hidd~ <chr [1]>    <NA>         <NA>           right        <NULL>      
      6 esti~ defa~ <chr [1]>    <NA>         <NA>           right        <NULL>      
      7 moe_~ hidd~ <chr [1]>    <NA>         <NA>           right        <NULL>      
      8 perc~ hidd~ <chr [1]>    <NA>         <NA>           right        <NULL>      
      9 perc~ hidd~ <chr [1]>    <NA>         <NA>           right        <NULL>      
      # i 1 more variable: hidden_px <list>

