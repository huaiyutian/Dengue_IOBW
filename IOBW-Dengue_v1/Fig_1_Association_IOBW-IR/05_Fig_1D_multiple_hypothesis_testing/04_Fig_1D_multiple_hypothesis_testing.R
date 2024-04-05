# Take the northern hemisphere as an example

data <- read.csv("01_Index_IR-N.csv")

ir <- data.matrix(data[1])

# Calculate the P-value
p_values_all <- c()
r_all <- c()

for (i in 2:31) {
  index <- data.matrix(data[i])
  r <- cor(ir, index)
  p_values <- cor.test(ir, index)$p.value

  r_all <- cbind(r_all, r)
  p_values_all <- cbind(p_values_all, p_values)
}

# BH-ajusted P-value
p_adjusted_FDR_BH <- p.adjust(p_values_all, method = "BH")
p_adjusted_FDR_BH

# Bonferroni-ajusted P-value
p_adjusted_Bonferroni <- p.adjust(p_values_all, method = "bonferroni")
p_adjusted_Bonferroni
