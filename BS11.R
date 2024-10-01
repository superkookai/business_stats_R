library(tidyverse)
options(sci=999)

### Find the value x for which P(chi_square_10<x) = 0.05
qchisq(0.05,10,lower.tail = TRUE) # 3.940299
## This one is equivalent to P(chi_square_10>x) = 1-0.05 = 0.95
qchisq(0.95,10,lower.tail = FALSE) # 3.940299

### Find the value x for which P(chi_square_10>x) = 0.05
qchisq(0.05,10,lower.tail = FALSE) # 18.30704

### Remark when find value of x in Table -> There is only a right-tailed P(chi_square_df>x) = alpha
