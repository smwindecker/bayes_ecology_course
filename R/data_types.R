my_vector <- c(0, 1, 2, 3)

my_character_vector <- c('horse', 'pig', 'sheep', 'cow')

my_farm_data <- data.frame(no_animals = my_vector,
                           animals = my_character_vector)

my_farm_matrix <- as.matrix(my_farm_data)

X <- model.matrix(~ no_animals + animals, data = my_farm_data)
