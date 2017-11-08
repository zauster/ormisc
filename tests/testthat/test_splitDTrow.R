
dt <- data.table(a = 1:2, b = c("a, b, c", "d, e"), stringsAsFactors = FALSE)
res <- splitDTRow(dt, ", ", "b")

expect_true(nrow(res) == 5)
