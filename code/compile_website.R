# install packages ---------------------------------------------------------
packages <- c("tidyverse", "testthat", "lingglosses", "lingtypology", "bib2df", 
              "DT", "knitr", "ymlthis", "rmarkdown", "RefManageR", "stringi",
              "readxl", "jsTreeR", "rrapply", "spelling")

to_install <- packages[!(packages %in% installed.packages())]

if(length(to_install) > 0){
  install.packages(to_install, dependencies = TRUE)
}

suppressPackageStartupMessages(library(tidyverse))

# convert .bib.tsv to .bib -------------------------------------------------
library(bib2df)

walk(list.files("data/orig_bib_tsv", full.names = TRUE), function(bib_tsv){
  bib_tsv_df <- read_tsv(bib_tsv, progress = FALSE, show_col_types = FALSE)
  
  bib_tsv |> 
    str_remove_all("[_\\.]tsv") |> 
    str_replace("_bib$", "\\.bib") ->
    result_file
  
  if(nrow(bib_tsv_df) == 0) {
    write_lines("", result_file)
  } else {
    bib_tsv_df |>  
      mutate(TITLE = ifelse(is.na(TITLE_TRANSLATION), TITLE, str_c(TITLE, " [", TITLE_TRANSLATION, "]")),
             BOOKTITLE = ifelse(is.na(BOOKTITLE_TRANSLATION), BOOKTITLE, str_c(BOOKTITLE, " [", BOOKTITLE_TRANSLATION, "]"))) |>
      df2bib(result_file)
  }
})

readxl::read_xlsx("data/biblib.xlsx", 
                  col_types = c(rep("text", 5),
                                "numeric", # YEAR 
                                rep("text", 10),
                                "numeric", # VOLUME
                                rep("text", 7))) |> 
  mutate(TITLE = ifelse(is.na(TITLE_TRANSLATION), 
                        TITLE, 
                        str_c(TITLE, " [", TITLE_TRANSLATION, "]")),
         BOOKTITLE = ifelse(is.na(BOOKTITLE_TRANSLATION), 
                            BOOKTITLE, 
                            str_c(BOOKTITLE, " [", BOOKTITLE_TRANSLATION, "]"))) |>
  df2bib("data/bibliography.bib")

# convert cyrillic to latin -----------------------------------------------
library(stringi)

cyr_latin_coresp <- "
    щ > šč;
    ю > ju;
    я > ja;
    х > x;
    Э > È;
    э > è;
    Щ > Šč;
    Ю > Ju;
    Я > Ja;
    Х > X;
    :: cyrillic-latin;
"

# dataset with some transcription fixes like: Lander, Jurij -> Lander, Yury
tr_patches <- read_csv("data/transliteration_patches.csv", 
                       show_col_types = FALSE)

vector_of_patches <- tr_patches$to
names(vector_of_patches) <- tr_patches$from

walk(c(list.files("data/orig_bib", full.names = TRUE), "data/bibliography.bib"), function(i){
  read_lines(i, progress = FALSE) |> 
    stri_trans_general(cyr_latin_coresp, rules=TRUE) |> 
    str_replace_all(vector_of_patches) |> 
    write_lines(i)
})

# embrace uppercased letters with curly braces ----------------------------
regular_expression <- str_c("((?<=[ \\[\\-\\(\\</])[", str_c(c(LETTERS, "Ž", "Č", "Š", "Ë", "É"), collapse = ""), "])")

c(list.files("data/orig_bib", full.names = TRUE), 
  "data/bibliography.bib") |> 
  walk(function(i){
  if(file.info(i)$size > 7){
    suppressMessages(suppressWarnings(bib2df(file = i))) |> 
      mutate(TITLE = ifelse(!is.na(TITLE), 
                            str_replace_all(TITLE, regular_expression, "\\{\\1\\}"),
                            NA),
             BOOKTITLE = ifelse(!is.na(BOOKTITLE),
                                str_replace_all(BOOKTITLE, regular_expression, "\\{\\1\\}"),
                                NA)) |> 
      df2bib(file = i)
  } 
})

# GENERATION OF THE RMD ----------------------------------------------------

# remove everything that starts with number and ends with Rmd --------------
file.remove(grep("\\d{1,}_.*.Rmd", list.files(), value = TRUE))

# read our fetures data ----------------------------------------------------
readxl::read_xlsx("data/contributors.xlsx") |> 
  filter(render == 1) |> 
  mutate(created_date = as.integer(created_date)) ->
  chapters

# deal with major topics --------------------------------------------------
chapters |> 
  filter(!is.na(major_topic_text))  |> 
  pull(filename) ->
  major_topics

chapters |> 
  filter(is.na(major_topic_text)) ->
  chapters 

file.copy(str_c("data/orig_rmd/", major_topics, ".Rmd"), 
          str_c(major_topics, ".Rmd"),
          overwrite = TRUE)

# create variable with leading 0 -------------------------------------------
# remove +1 when we will have more then 100 topics
chapters$id_0 <- sprintf(paste0("%0", nchar(max(chapters$id))+1, "d_"), 
                         chapters$id)

chapters |> 
  mutate(filename = str_c(filename, "_map")) |> 
  bind_rows(chapters) ->
  chapters

# create Rmd names ---------------------------------------------------------
rmd_filenames <- c(str_c(chapters$id_0, chapters$filename, ".Rmd"))

# create key for bibtex ----------------------------------------------------
first_authors <- tolower(str_remove(map(str_split(chapters$author, " "), 2), ","))

# create orig_rmd/..._map.Rmd files ----------------------------------------------------

str_subset(rmd_filenames, "_map.Rmd") |> 
  walk(function(i){
    
    str_c("data/orig_table/", 
          str_remove(str_remove(i, "_map.Rmd"), "\\d{1,}_"),
          ".tsv") |> 
      read_tsv(progress = FALSE, show_col_types = FALSE) |> 
      select(matches("^feature\\d{1,}")) |> 
      distinct() |> 
      pivot_longer(names_to = "features", values_to = "titles", everything()) |> 
      mutate(features = as.double(str_extract(features, "\\d{1,}")),
             first_letter = str_extract(titles, ".") |> str_to_upper(),
             titles = str_remove(titles, "."),
             titles = str_c(first_letter, titles)) |> 
      select(-first_letter) ->
      multiple_values
    
  write_lines(
    c("
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning=FALSE, fig.width = 9.5)
library(tidyverse)
library(lingtypology)
library(RCaucTile)
```

## {-}

Visualization 1 (**General datapoints**) shows no more than one dot per language. Visualization 2 (**Extrapolated data**) represents each language as a cluster of dots, which correspond to villages where a certain language is spoken. Visualization 3 (**Data granularity**) takes into account dialect levels. Visualization 4 (**Tile map**) is analogous to Visualization 1 and displays languages as rectangles that are distributed according to their geographic location. On the **Data granularity** maps you can see the village and the data type (village data, general language data, etc.) when you click on a dot. Hover over or click on a dot to see the language. By unticking the box “show languages” you can remove the inner dots and visualize the distribution of different values in the area without the distraction of genealogical information.

```{r}
",
str_c('read_tsv("../orig_table/', 
      str_remove(str_remove(i, "_map.Rmd"), "\\d{1,}_"),
      '.tsv", show_col_types = FALSE, guess_max = 2000) |>'),
"  mutate(type = case_when(type == 'language' ~ 'language',
                          type == 'dialect_toplevel' ~ 'top level dialect',
                          type == 'dialect_nt1' ~ 'non top level 1 dialect',
                          type == 'dialect_nt2' ~ 'non top level 2 dialect',
                          type == 'dialect_nt3' ~ 'non top level 3 dialect',
                          type == 'village' ~ 'village dialect')) |> 
  filter(map != 'no') ->
  feature_dataset

'../feature_types.csv' |> 
  read_csv(progress = FALSE, show_col_types = FALSE) |> ",
  str_c("  filter(filename == '",
        str_remove(i, "\\d{1,}_"),
        "') |> ", collapse = ""),
"  select(-filename) ->
  feature_types

colnames(feature_dataset) |> 
  str_detect('value\\\\d') |> 
  which() |> 
  tibble(column_ids = _) |> 
  mutate(feature_id = 1:n()) |> 
  left_join(feature_types) |>
  na.omit() ->
  for_changing_feature_types

if(nrow(for_changing_feature_types) > 0){
for_changing_feature_types$column_ids |> 
  seq_along() |> 
  walk(function(i){
    if(for_changing_feature_types$feature_type[i] == 'numeric'){
      lapply(feature_dataset[,for_changing_feature_types$column_ids[i]], as.numeric) ->
        feature_dataset[,for_changing_feature_types$column_ids[i]]
    } else {
      lapply(feature_dataset[,for_changing_feature_types$column_ids[i]], function(j){
        factor(j, levels = c(for_changing_feature_types$feature_type[i] |> str_split('; ') |> unlist()))
      }) ->>
        feature_dataset[,for_changing_feature_types$column_ids[i]]
    }
  })}

read_tsv('../tald_villages.csv', show_col_types = FALSE, guess_max = 2000)  |>
  select(village, rus_village, lat, lon, gltc_lang, aff, family, standard, default_level, dialect_toplevel, dialect_nt1, dialect_nt2, dialect_nt3, village_dialect, lang_col, aff_col) |> 
  mutate(language = default_level) |>
  pivot_longer(names_to = 'type', values_to = 'idiom', standard:village_dialect) |> 
  filter(!is.na(idiom)) |> 
  mutate(type = case_when(type == 'standard' ~ 'language',
                          type == 'default_level' ~ 'language',
                          type == 'dialect_toplevel' ~ 'top level dialect',
                          type == 'dialect_nt1' ~ 'non top level 1 dialect',
                          type == 'dialect_nt2' ~ 'non top level 2 dialect',
                          type == 'dialect_nt3' ~ 'non top level 3 dialect',
                          type == 'village_dialect' ~ 'village dialect')) ->
  coordinates

coordinates |> 
  distinct(gltc_lang) |> 
  mutate(lang4map = lang.gltc(gltc_lang)) ->
  lang4map

coordinates |> 
  group_by(idiom, type, gltc_lang, lang_col) |> 
  reframe(lat = mean(lat), 
          lon = mean(lon)) |> 
  left_join(lang4map)->
  coordinates_averaged

feature_dataset |> 
  mutate(idiom = str_remove(idiom, 'Standard ')) |> 
  inner_join(coordinates, by = c('type', 'idiom', 'language'), relationship = 'many-to-many') |> 
  left_join(lang4map) |> 
  mutate(display = 'show languages') ->
  all_data
```
",

map(multiple_values$features, function(i){
  str_c(
    "## ", multiple_values$titles[i], " {.tabset .tabset-fade .tabset-pills #m", multiple_values$features[i], "} 
    
### General datapoints {-}

```{r}
if(is.numeric(feature_dataset$value",
    multiple_values$features[i],
    ")){
  feature_dataset |> 
    filter(map == 'yes',
           genlang_point == 'yes') |>
    inner_join(coordinates_averaged) |> 
    mutate(popup = ifelse((lang4map == idiom | str_detect(idiom, 'Standard')), 
                          str_c('data level: ', type),
                          str_c(idiom, '<br> data level: ', type)),
           display = 'show languages')  |> 
    filter(!is.na(value",
    multiple_values$features[i],
    "),
           !is.na(lang4map)) ->
    general_datapoints_map
  } else if(is.factor(feature_dataset$value",
    multiple_values$features[i],
    ")){
    feature_dataset |> 
      filter(map == 'yes',
             genlang_point == 'yes') |>
      inner_join(coordinates_averaged) |> 
      mutate(popup = ifelse((lang4map == idiom | str_detect(idiom, 'Standard')), 
                            str_c('data level: ', type),
                            str_c(idiom, '<br> data level: ', type)),
             value",
    multiple_values$features[i],
    " = droplevels(value",
    multiple_values$features[i],
    "),
             display = 'show languages')  |> 
      filter(!is.na(value",
    multiple_values$features[i],
    "),
             !is.na(lang4map)) ->
      general_datapoints_map
    
    general_datapoints_map |> 
      count(value",
    multiple_values$features[i],
    ") |> 
      arrange(value",
    multiple_values$features[i],
    ") |> 
      mutate(new_levels = str_c(value",
    multiple_values$features[i],
    ", ' (', n, ')')) |> 
      pull(new_levels) ->
      levels(general_datapoints_map$value",
    multiple_values$features[i],
    ")
    
  } else{
    feature_dataset |> 
      filter(map == 'yes',
             genlang_point == 'yes') |>
      add_count(value",
    multiple_values$features[i],
    ") |> 
      inner_join(coordinates_averaged) |> 
      mutate(popup = ifelse((lang4map == idiom | str_detect(idiom, 'Standard')), 
                            str_c('data level: ', type),
                            str_c(idiom, '<br> data level: ', type)),
             value",
    multiple_values$features[i],
    " = str_c(value",
    multiple_values$features[i],
    ", ' (', n, ')'),
             display = 'show languages')  |> 
      filter(!is.na(value1),
             !is.na(lang4map)) ->
      general_datapoints_map
  }

map.feature(general_datapoints_map$lang4map,
            latitude = general_datapoints_map$lat,
            longitude = general_datapoints_map$lon,
            label = general_datapoints_map$language,
            features = general_datapoints_map$value",
    multiple_values$features[i],
    ",
            color = 'viridis',
            stroke.features = 'a',
            stroke.color = 'black',
            stroke.radius = 1,
            stroke.legend = FALSE,
            tile = 'Esri.WorldGrayCanvas',
            legend = TRUE,
            legend.position = 'bottomleft', 
            zoom.control = TRUE,
            width = 8) %>% 
  map.feature(general_datapoints_map$lang4map,
              latitude = general_datapoints_map$lat,
              longitude = general_datapoints_map$lon,
              features = general_datapoints_map$language,
              label = general_datapoints_map$language,
              color = general_datapoints_map$lang_col,
              tile = 'Esri.WorldGrayCanvas',
              legend = FALSE,
              width = 5,
              pipe.data = .,
              control = general_datapoints_map$display)
```

### Extrapolated data {-}

```{r}
all_data |> 
  filter(!is.na(value",
    multiple_values$features[i],
    ")) ->
  all_data_filtered

map.feature(all_data_filtered$lang4map,
            latitude = all_data_filtered$lat, 
            longitude = all_data_filtered$lon,
            features = all_data_filtered$value",
    multiple_values$features[i],
    ",
            color = 'viridis',
            stroke.features = 'a',
            stroke.color = 'black',
            stroke.radius = 1,
            stroke.legend = FALSE,
            tile = 'Esri.WorldGrayCanvas',
            legend = TRUE,
            legend.position = 'bottomleft', 
            label = all_data_filtered$language,
            zoom.control = TRUE,
            width = 8,
            popup = paste(all_data_filtered$village, '|',
                          all_data_filtered$rus_village, '<br>',
                          'data:', all_data_filtered$type)) %>% 
  map.feature(all_data_filtered$lang4map,
              latitude = all_data_filtered$lat,
              longitude = all_data_filtered$lon,
              features = all_data_filtered$language,
              label = all_data_filtered$language,
              color = all_data_filtered$lang_col,
              tile = 'Esri.WorldGrayCanvas',
              legend = FALSE,
              width = 5,
              pipe.data = .,
              control = all_data_filtered$display,
              popup = paste(all_data_filtered$village, '|',
                            all_data_filtered$rus_village, '<br>',
                            'data:', all_data_filtered$type))
```

### Data granularity {-}

```{r}
feature_dataset |> 
  filter(map == 'yes') |>
  inner_join(coordinates_averaged) |> 
  mutate(popup = ifelse((lang4map == idiom | str_detect(idiom, 'Standard')), 
                        str_c('data level: ', type),
                        str_c(idiom, '<br> data level: ', type)),
         type = factor(type, levels = c('language', 
                                        'top level dialect',
                                        'non top level 1 dialect',
                                        'non top level 2 dialect',
                                        'non top level 3 dialect',
                                        'village dialect')))  |> 
  filter(!is.na(value",
    multiple_values$features[i],
    "),
         !is.na(lang4map)) |> 
  arrange(type) ->
  data_granularity_map
  
map.feature(data_granularity_map$lang4map,
            latitude = data_granularity_map$lat, 
            longitude = data_granularity_map$lon,
            features = data_granularity_map$value",
    multiple_values$features[i],
    ",
            color = 'viridis',
            stroke.features = 'a',
            stroke.color = 'black',
            stroke.radius = 1,
            stroke.legend = FALSE,
            tile = 'Esri.WorldGrayCanvas',
            legend = TRUE,
            legend.position = 'bottomleft', 
            label = data_granularity_map$language,
            zoom.control = TRUE,
            control = data_granularity_map$type,
            popup = data_granularity_map$popup)
```

### Tile map {-}

```{r, fig.width=12, fig.height=8}
general_datapoints_map |> 
  select(language, value",
    multiple_values$features[i],
    ") |> 
  ec_tile_map(feature_column = 'value",
    multiple_values$features[i],
    "',
              tile_colors = 'viridis') +
  theme(text = element_text(size = 15))
```

    ")
}),
"

## Datatable
",
str_c('[Download](https://raw.githubusercontent.com/LingConLab/TALD/master/data/orig_table/', 
      str_remove(str_remove(i, "_map.Rmd"), "\\d{1,}_"),
      '.tsv) the whole dataset.'),
"
```{r}
bib <- RefManageR::ReadBib(file = '../bibliography.bib')

feature_dataset |> 
  select(str_which(colnames(feature_dataset), 'feature\\\\d{1,}$')) |> 
  pivot_longer(cols = everything()) |> 
  distinct() |>  
  mutate(name = str_replace(name, 'feature', 'value')) ->
  columns_rename

feature_dataset |> 
  select(language, idiom, source, page, matches('value\\\\d{1,}$')) |> 
  filter(!if_all(matches('value\\\\d{1,}$'), is.na)) |> 
  rename_with(function(x){columns_rename$value[match(x, columns_rename$name)]}, matches('value\\\\d{1,}$'))  |> 
  rename(Language=language, 
         Idiom = idiom,
         Source = source) |> 
  mutate(page = str_replace_all(page, '--', '–'),
         Source = str_split(Source, '; '),
         page = str_split(page, '; ')) |> 
  unnest_longer(col = c(Source, page)) |> 
  rowwise() |> 
  mutate(page = ifelse(page == 'NA', NA_character_, page),
         Source = case_when(str_detect(Source, '[Ff]ield [Dd]ata') ~ Source,
                            str_detect(Source, 'p.\\\\s?c.$') ~ Source,
                            TRUE ~ RefManageR::Cite(bib, Source, 
                                        after = ifelse(!is.na(page),  str_c(': ', page), '')))) |> 
  select(-page) |> 
  DT::datatable(class = 'cell-border stripe', 
    rownames = FALSE, 
    filter = 'top', 
    extensions = 'Buttons',
    options = list(pageLength = 100, 
                   autoWidth = TRUE, 
                   info = FALSE,
                   dom = 'fBltp',
                   buttons = list(list(extend = 'collection',
                                       buttons = c('csv', 'excel', 'pdf'),
                                       text = '<i class=\"fas fa-download\"></i>')),
                   paginate = TRUE))
```

## References {-}

```{r, results='asis'}
RefManageR::PrintBibliography(bib)
```

"), 
    file = str_c("data/orig_rmd/", str_remove(i, "\\d{1,}_"))
  )
})

# create Rmd files ---------------------------------------------------------
options(ymlthis.rmd_body = "
```{r, include = FALSE}
knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE, comment = '')
library(lingglosses)
```
")

walk(seq_along(rmd_filenames), function(i){
  ymlthis::yml_empty() |> 
    ymlthis::yml_title(ifelse(str_detect(rmd_filenames[i], "_map.Rmd"), 
                              str_c(chapters$title[i], " (Maps & Data)"), 
                              chapters$title[i])) |> 
    ymlthis::yml_author(chapters$author[i]) |> 
    ymlthis::yml_date(paste0('Last update: ', 
                             ifelse(str_detect(rmd_filenames[i], "_map.Rmd"), 
                                    chapters$updated_map[i], 
                                    chapters$updated_text[i]))) |> 
    ymlthis::yml_citations(bibliography = ifelse(str_detect(rmd_filenames[i], "_map.Rmd"), 
                                                 FALSE, 
                                                 paste0("./data/orig_bib/", 
                                                        str_remove(chapters$filename[i], "_map"), 
                                                        ".bib")),
                           csl = "apa.csl",
                           link_citations = TRUE) |> 
    ymlthis::yml_output(html_document(number_sections = TRUE,
                                      anchor_sections = TRUE,
                                      pandoc_args = "--shift-heading-level-by=-1")) |> 
    ymlthis::use_rmarkdown(path = rmd_filenames[i], 
                           open_doc = FALSE, 
                           quiet = TRUE,
                           include_body = FALSE,
                           body = NULL) 
  write_lines(c(
    # add link to map/chapter page
    paste0("See [",
           ifelse(str_detect(rmd_filenames[i], "_map.Rmd"), 
                  "chapter", 
                  "data and maps"),
           "](", 
           str_remove(rmd_filenames[i], "(_map)?.Rmd"), 
           ifelse(str_detect(rmd_filenames[i], "_map.Rmd"), 
                  ".html)",
                  "_map.html).")),
    "",
    # create and add citation
    "```{r}",
    "library(RefManageR)",
    "BibOptions(check.entries = FALSE, style = 'text', first.inits = FALSE, bib.style = 'authoryear')",
    "article_citation <- BibEntry(bibtype = 'Incollection', ",
    paste0(" key='", first_authors[i], chapters$created_date[i], "',"),
    paste0(" title='", 
           ifelse(str_detect(rmd_filenames[i], "_map.Rmd"), 
                  str_c(chapters$title[i], " (Maps & Data)"), 
                  chapters$title[i]), 
           "',"),
    paste0(" author='", str_replace(chapters$author[i], ",", " and"), "',"),
    paste0(" year='", chapters$created_date[i], "',"),
    " editor= 'George Moroz and Michael Daniel and Konstantin Filatov and Timur Maisak and Timofey Mukhin and Irina Politova and Elena Shvedova and Samira Verhees and Chiara Naccarato',",
    " publisher='Linguistic Convergence Laboratory, HSE University',",
    " address='Moscow',",
    " booktitle= 'Typological Atlas of the Languages of Daghestan (TALD), v 2.0.1',",
    " url='https://lingconlab.ru/tald',",
    " doi='10.5281/zenodo.6807070')",
    "```",
    "",
    "## {.tabset .tabset-fade .tabset-pills -} ",
    "",
    "### Plain text {-}",
    "```{r, results = 'asis'}",
    "print(article_citation, .opts = list(style = 'text'))",
    "```",
    "",
    "### BibTeX {-}",
    "",
    "```{r}",
    "print(article_citation, .opts = list(style = 'Bibtex'))",
    "```",
    # add text of the Rmd
    "",
    str_c("```{r, child='data/orig_rmd/", chapters$filename[i], ".Rmd'}"),
    "```",
    ""),
    rmd_filenames[i], append = TRUE)
})

# RENDER AND CLEAN ---------------------------------------------------------
rmarkdown::render_site()

beepr::beep()
