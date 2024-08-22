base=as.data.frame(LATINOBAROMETRO_CR)
base <- as.data.frame(lapply(LATINOBAROMETRO_CR, as_factor))

save(base,file = "Latinobarometro.Rdata")
library(openxlsx)
library(writexl)
write.xlsx(base, file = "Latinobarometro.xlsx")
