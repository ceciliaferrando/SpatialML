library(tidyverse)
library(readr)
library(GGally)


data_df = readr::read_csv('C:/Users/Cecilia/Documents/USA/CMU/CMU_2nd_year/Pre-Thesis Seminar/room_privacy/data/measures.csv')
data_df = dplyr::select(data_df, -one_of(c('X15', 'X16', 'X17', 'X18')))
View(data_df)

new_names = c()
for (name in names(data_df)){
  new_names <- c(new_names, gsub(" ", "_", tolower(name), fixed = TRUE))
}
names(data_df) = new_names


data_df = dplyr::mutate(data_df, 
                       privacy = as.integer(privacy*2))

print(names(data_df))
View(data_df)


# Pair Plot
plot_df = dplyr::select(data_df, -one_of(c('privacy', 'function', 'room', 'sample')))
plot_df = dplyr::mutate(plot_df, 
                        privacy_binary = as.factor(privacy_binary))
names(plot_df)
ggpairs(plot_df, aes(color = privacy_binary, alpha = 0.4))


# Pair Plot 2
plot_df2 = dplyr::select(data_df, one_of(c('centered_isovist', 'number_of_visual_neighbors', 
                                           'degree_centrality', 'betweenness_centrality', 'privacy_binary')))
plot_df2 = dplyr::mutate(plot_df2, 
                        privacy_binary = as.factor(privacy_binary))
names(plot_df2)
ggpairs(plot_df2, aes(color = privacy_binary, alpha = 0.4))

plot_df = dplyr::select(data_df, -one_of(c('privacy_binary', 'function', 'room', 'sample')))
plot_df = dplyr::mutate(plot_df, 
                        privacy = as.factor(privacy))
ggpairs(plot_df, aes(colour = privacy, alpha = 0.4))


# Training and testing
model_df <- dplyr::select(data_df, -one_of(c('function', 'room', 'sample')))
model_df <- dplyr::mutate(model_df,
                          privacy = as.factor(privacy),
                          privacy_binary = as.factor(privacy_binary))

set.seed(7)
sample_row <- sample.int(n=nrow(model_df), size = floor(.7*nrow(model_df)), replace=F)
training_data <- model_df[sample_row, ]
test_data <- model_df[-sample_row, ]


# Binary Privacy
training_data_binary <- dplyr::select(training_data, -one_of('privacy'))
test_data_binary <- dplyr::select(test_data, -one_of('privacy'))

binary_log_reg = glm(formula = privacy_binary ~., family = binomial(link='logit'), data = training_data_binary)
summary(binary_log_reg)

predictions = as.integer(predict(binary_log_reg, newdata = test_data_binary, type='response') >=0.5)
table(predictions, test_data_binary$privacy_binary)
accuracy = sum(predictions == test_data_binary$privacy_binary)/nrow(test_data_binary)
accuracy
sum(test_data_binary$privacy_binary==1)/nrow(test_data_binary)

write.csv(training_data_binary, file = 'data/training_data_binary.csv')
write.csv(test_data_binary, file = 'data/test_data_binary.csv')



# Multinomial Privacy
library(splitstackshape)
library(nnet)
set.seed(7)

model_df_mult <- dplyr::mutate(model_df, id = seq(0, nrow(model_df)-1))

training_data_mult <- stratified(model_df_mult, c('privacy'), .7)
test_data_mult <- as.data.frame(model_df_mult[-which(model_df_mult$id %in% training_data_mult$id), ])

training_data_mult <- training_data_mult[, -c('id', 'privacy_binary')]
test_data_mult <- dplyr::select(test_data_mult, -one_of('id', 'privacy_binary'))


mult_log_reg = multinom(formula = privacy ~., data = training_data_mult)
summary(mult_log_reg)
z <- summary(mult_log_reg)$coefficients/summary(mult_log_reg)$standard.errors
z
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

pred_mult_df <- predict(mult_log_reg, type = "probs", newdata = data.frame(test_data_mult))
pred_class <- apply(X = pred_mult_df, MARGIN = 1, FUN = function(x) {return(which.max(x) -1)})

table(pred_class, test_data_mult$privacy)
table(test_data_mult$privacy)

write.csv(training_data_mult, file = 'data/training_data.csv')
write.csv(test_data_mult, file = 'data/test_data.csv')

