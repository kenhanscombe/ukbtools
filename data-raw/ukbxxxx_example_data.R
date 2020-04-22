
# Generate an example UKB dataset

library(tidyverse)

df <- data_frame(
  f.eid = 1:100,
  f.31.0.0 = rbinom(100, 1, .50),
  f.54.0.0 = sample(seq(11001, 11027), size = 100, replace = TRUE),
  f.54.1.0 = NA,
  f.54.2.0 = NA,
  f.189.0.0 = round(rpois(100, lambda = 5) + runif(100, -1, 1), 2),
  f.6142.0.0 = sample(c(-3, -7, 1:7), size = 100, replace = TRUE),
  f.6142.0.1 = NA,
  f.6142.0.2 = NA,
  f.6142.0.3 = NA,
  f.6142.0.4 = NA,
  f.6142.0.5 = NA,
  f.6142.0.6 = NA,
  f.6142.1.0 = NA,
  f.6142.1.1 = NA,
  f.6142.1.2 = NA,
  f.6142.1.3 = NA,
  f.6142.1.4 = NA,
  f.6142.1.5 = NA,
  f.6142.1.6 = NA,
  f.6142.2.0 = NA,
  f.6142.2.1 = NA,
  f.6142.2.2 = NA,
  f.6142.2.3 = NA,
  f.6142.2.4 = NA,
  f.6142.2.5 = NA,
  f.6142.2.6 = NA,
  f.21000.0.0 = sample(c(-3, -1, 1, 2, 3, 4, 5, 6, 1001, 1002, 1003, 2001, 2002,
                         2003, 2004, 3001, 3002, 3003, 3004, 4001, 4002, 4003),
                       size = 100, replace = TRUE),
  f.21000.1.0 = NA,
  f.21000.2.0 = NA,
  f.21003.0.0 = round(runif(100, min = 40, max = 70)),
  f.21003.1.0 = NA,
  f.21003.2.0 = NA)


write.table(df, file = "ukbxxxx.tab", col.names = TRUE, row.names = FALSE,
            quote = FALSE, append = FALSE, sep = "\t")
