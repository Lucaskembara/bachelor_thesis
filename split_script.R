dat <- read.csv("labelled_full_nested.csv", sep = ",")

sample_size <- floor(0.75 * nrow(dat))

set.seed(123)
train_index <- sample(seq_len(nrow(dat)), size = sample_size)

train <- dat[train_index, ]
test <- dat[-train_index, ]

write.csv(train, "train_data.csv", row.names = FALSE)
write.csv(test, "test_data.csv", row.names = FALSE)