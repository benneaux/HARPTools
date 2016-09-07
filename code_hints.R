## Code


  #Select Odd-Numbered Rows
    
    df.odd <- df[ c(TRUE,FALSE), ]

  #Select Even-Numbered Rows
    
    df.even <- df[ !c(TRUE,FALSE), ]