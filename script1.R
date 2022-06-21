### Pacotes
pacman::p_load(dplyr, tidyr, stringr)


### Arquivo dicionario
dici <- read.csv("./dados\\CXR8\\Data_Entry_2017_v2020.csv")
glimpse(dici)

title <- c("Atelectasis",
           "Cardiomegaly", 
           "Consolidation",
           "Effusion",
           "Edema",
           "Emphysema",
           "Fibrosis",
           "Hernia",
           "Infiltration", 
           "Mass",
           "Nodule",
           "Pneumonia",
           "Pneumothorax", 
           "Pleural_Thickening")

title_vars <- title %>% 
  purrr::map(~ expr(ifelse(data.table::like(Finding.Labels, !!.x), 1, 0))) %>%
  purrr::set_names(paste0("title_", gsub("\\s|/", "", title)))

dici <- mutate(dici,!!!title_vars)

#### analise descritiva

library(ComplexUpset)
upset(dici, paste0("title_", gsub("\\s|/", "", title)),
      name='Patologia', width_ratio=0.1, min_size = 10, set_sizes = FALSE)


dici %>% select(starts_with("title")) %>% 
  summarise_all(~sum(as.integer(.))) 

options(scipen = 999)
dici  %>% select(starts_with("title")) %>%
  transmute(sumVar = rowSums(.)) %>% bind_cols(dici, .) %>% 
  pull(sumVar) %>% table() %>% prop.table() %>% round(2)


# amostragem estratificada proporcional
dici %>% select(starts_with("title")) %>%
  group_by_all() 
## cmr   
library(mldr)
mldrGUI()
print(emotions)
mymldr <- mldr_from_dataframe(dici, labelIndices = c(13:25), name = "testMLDR")
write_arff(mymldr, "my_new_mldr")

### Particionando em bases de treinamento e teste:
local_dataset <- "./dados/CXR8/images"

base_dir <- "base"
dir.create(base_dir)

# local de todas imagens
list.files(local_dataset, full.names = T, recursive = T)


train_dir <- file.path(base_dir, "train")
dir.create(train_dir)
validation_dir <- file.path(base_dir, "validation")
dir.create(validation_dir)
test_dir <- file.path(base_dir, "test")

file


dir.create(test_dir)
train_cats_dir <- file.path(train_dir, "cats")
dir.create(train_cats_dir)
train_dogs_dir <- file.path(train_dir, "dogs")

dir.create(train_dogs_dir)
validation_cats_dir <- file.path(validation_dir, "cats")
dir.create(validation_cats_dir)
validation_dogs_dir <- file.path(validation_dir, "dogs")

dir.create(validation_dogs_dir)
test_cats_dir <- file.path(test_dir, "cats")

dir.create(test_cats_dir)
test_dogs_dir <- file.path(test_dir, "dogs")
dir.create(test_dogs_dir)

fnames <- paste0("cat.", 1:1000, ".jpg")
file.copy(file.path(original_dataset_dir, fnames),
          file.path(train_cats_dir))

fnames <- paste0("cat.", 1001:1500, ".jpg")
file.copy(file.path(original_dataset_dir, fnames),
          file.path(validation_cats_dir))

fnames <- paste0("cat.", 1501:2000, ".jpg")
file.copy(file.path(original_dataset_dir, fnames),
          file.path(test_cats_dir))
fnames <- paste0("dog.", 1:1000, ".jpg")
file.copy(file.path(original_dataset_dir, fnames),
          file.path(train_dogs_dir))
fnames <- paste0("dog.", 1001:1500, ".jpg")
file.copy(file.path(original_dataset_dir, fnames),
          file.path(validation_dogs_dir))

fnames <- paste0("dog.", 1501:2000, ".jpg")
file.copy(file.path(original_dataset_dir, fnames),
          file.path(test_dogs_dir))
