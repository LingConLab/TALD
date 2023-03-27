# install packages ---------------------------------------------------------
packages <- c("tidyverse", "testthat", "lingglosses", "lingtypology", "bib2df", 
              "DT", "knitr", "ymlthis", "rmarkdown", "RefManageR", "stringi",
              "readxl")

to_install <- packages[!(packages %in% installed.packages())]

if(length(to_install) > 0){
  install.packages(to_install, dependencies = TRUE)
}

library(tidyverse)

# CREATE VILLAGE DATASET --------------------------------------------------
# Moroz, George, & Verhees, Samira. (2020). East Caucasian villages dataset (Version v2.0) [Data set]. Zenodo. https://doi.org/10.5281/zenodo.5588473

read_tsv("https://raw.githubusercontent.com/sverhees/master_villages/master/data/TALD/tald_villages.tsv",
         progress = FALSE, show_col_types = FALSE) %>% 
  write_tsv("data/tald_villages.csv")

read_tsv("https://raw.githubusercontent.com/sverhees/master_villages/master/data/villages.tsv",
         progress = FALSE, show_col_types = FALSE) %>% 
  select(village, rus_village, lat, lon, gltc_lang, gltc_dialect, version) %>% 
  rename(village_dataset_version = version) %>% 
  write_csv("data/villages.csv")

# RUN TESTS ----------------------------------------------------------------
# file.remove("test_logs.txt")
# testthat::test_dir("tests")
# test_logs <- read_lines("test_logs.txt") 
# write_lines(test_logs[test_logs != "everything is ok"], "test_logs.txt")

# convert .bib.tsv to .bib -------------------------------------------------
library(bib2df)

map(list.files("data/orig_bib_tsv", full.names = TRUE), function(bib_tsv){
  read_tsv(bib_tsv, progress = FALSE, show_col_types = FALSE) %>%  
    mutate(TITLE = ifelse(is.na(TITLE_TRANSLATION), TITLE, str_c(TITLE, " [", TITLE_TRANSLATION, "]")),
           BOOKTITLE = ifelse(is.na(BOOKTITLE_TRANSLATION), BOOKTITLE, str_c(BOOKTITLE, " [", BOOKTITLE_TRANSLATION, "]"))) %>%
    df2bib(bib_tsv %>% 
                     str_remove_all("[_\\.]tsv") %>% 
                     str_replace("_bib$", "\\.bib"))
})

readxl::read_xlsx("data/biblib.xlsx", 
                  col_types = c(rep("text", 4),
                                "numeric", # YEAR 
                                rep("text", 8),
                                "numeric", # VOLUME
                                rep("text", 8))) %>% 
  mutate(TITLE = ifelse(is.na(TITLE_TRANSLATION), TITLE, str_c(TITLE, " [", TITLE_TRANSLATION, "]")),
         BOOKTITLE = ifelse(is.na(BOOKTITLE_TRANSLATION), BOOKTITLE, str_c(BOOKTITLE, " [", BOOKTITLE_TRANSLATION, "]"))) %>%
  df2bib("data/bibliography.bib")

# convert cyrillic to latin -----------------------------------------------
library(stringi)

cyr_latin_coresp <- "
    щ > šč;
    ю > ju;
    я > ja;
    х > x;
    Щ > Šč;
    Ю > Ju;
    Я > Ja;
    Х > X;
    :: cyrillic-latin;
"

map(c(list.files("data/orig_bib", full.names = TRUE), "data/bibliography.bib"), function(i){
  read_lines(i) %>% 
    stri_trans_general(cyr_latin_coresp, rules=TRUE) %>% 
    write_lines(i)
})


# embrace uppercased letters with curly braces ----------------------------
regular_expression <- str_c("((?<=[ \\[\\-\\(\\</])[", str_c(c(LETTERS, "Ž", "Č", "Š", "Ë", "É"), collapse = ""), "])")

map(c(list.files("data/orig_bib", full.names = TRUE), "data/bibliography.bib"), function(i){
  if(file.info(i)$size > 7){
  bib2df(i) %>% 
    mutate(TITLE = ifelse(!is.na(TITLE), 
                          str_replace_all(TITLE, regular_expression, "\\{\\1\\}"),
                          NA),
           BOOKTITLE = ifelse(!is.na(BOOKTITLE),
                              str_replace_all(BOOKTITLE, regular_expression, "\\{\\1\\}"),
                              NA)) %>% 
    df2bib(i)
  } 
})

# e <- map(c(list.files("data/orig_bib", full.names = TRUE), "data/bibliography.bib"),
#          function(i){
#            print(i)
#            RefManageR::ReadBib(i)})

# GENERATION OF THE RMD ----------------------------------------------------
library(tidyverse)

# remove everything that starts with number and ends with Rmd --------------
file.remove(grep("\\d{1,}_.*.Rmd", list.files(), value = TRUE))

# read our fetures data ----------------------------------------------------
readxl::read_xlsx("data/contributors.xlsx") %>% 
  filter(render == 1) %>% 
  mutate(created_date = as.integer(created_date)) ->
  features

# deal with major topics --------------------------------------------------
features %>% 
  filter(major_topic_text == TRUE) %>% 
  pull(filename) ->
  major_topics

features %>% 
  filter(is.na(major_topic_text)) ->
  features 

file.copy(str_c("data/orig_rmd/", major_topics, ".Rmd"), 
          str_c(major_topics, ".Rmd"),
          overwrite = TRUE)

# create variable with leading 0 -------------------------------------------
# remove +1 when we will have more then 100 topics
features$id_0 <- sprintf(paste0("%0", nchar(max(features$id))+1, "d_"), 
                         features$id)

features %>% 
  mutate(filename = str_c(filename, "_map")) %>% 
  bind_rows(features) ->
  features

# create Rmd names ---------------------------------------------------------
rmd_filenames <- c(str_c(features$id_0, features$filename, ".Rmd"))

# create key for bibtex ----------------------------------------------------
first_authors <- tolower(str_remove(map(str_split(features$author, " "), 2), ","))

# create orig_rmd/..._map.Rmd files ----------------------------------------------------

map(rmd_filenames[str_detect(rmd_filenames, "_map.Rmd")], function(i){
  
  read_tsv(str_c("data/orig_table/", 
                 str_remove(str_remove(i, "_map.Rmd"), "\\d{1,}_"),
                 ".tsv"),
           progress = FALSE, show_col_types = FALSE)  %>% 
    select(matches("^value\\d{1,}_")) %>% 
    distinct() %>% 
    pivot_longer(names_to = "values", values_to = "titles", everything()) %>% 
    mutate(values = as.double(str_extract(values, "\\d{1,}")))  ->
    multiple_values
  
  write_lines(
    c("

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning=FALSE, fig.width = 9.5)
library(tidyverse)
library(lingtypology)
```

## {-}

Visualization 1 (**General datapoints**) and 3 (**Extrapolated data**) are both based on the principle 'one value – one language'. Visualization 2 (**Data granularity**) takes into account dialect levels. On the **Data granularity** maps you can see the village and the data type (village data, general language data, etc.) when you click on a dot. Hover over or click on a dot to see the language. By unticking the box “show languages” you can remove the inner dots and visualize the distribution of different values in the area without the distraction of genealogical information.

```{r}
",
str_c('feature_dataset <- read_tsv("../orig_table/', 
      str_remove(str_remove(i, "_map.Rmd"), "\\d{1,}_"),
      '.tsv")'),
"
feature_dataset %>% 
  filter(map == 'yes') ->
  feature_dataset_4map

villages <- read_tsv('../tald_villages.csv') # village coordinates
genlang <- read_tsv('../genlangpoints.csv') # general language points

feature_dataset_4map %>% 
  filter(type == 'language') ->
  feature_dataset_4map_languages

feature_dataset_4map %>% 
  filter(type != 'language') ->
  feature_dataset_4map_rest

genlang %>% 
  filter(aff == 'Dargwa',
         lang != 'Kaitag',
         lang != 'Standard Dargwa') ->
  change

if(nrow(change) > 0) {
  villages$gltc_lang[which(villages$village_dialect %in% change$gltc_lang
)] <- change$gltc_lang

  villages$gltc_lang[which(villages$dialect_toplevel == 'Kaitag')] <- 'kajt1238'
}

villages %>% 
  select(village, rus_village, lat, lon, gltc_lang, lang, aff, family, standard, dialect_toplevel, dialect_nt1, dialect_nt2, dialect_nt3, village_dialect, lang_col, aff_col) %>% 
  pivot_longer(names_to = 'type', values_to = 'idiom', standard:village_dialect) %>% 
  filter(!is.na(idiom)) %>% 
  mutate(type = case_when(type == 'village_dialect' ~ 'village',
                          TRUE ~ type)) %>%
  inner_join(feature_dataset_4map_rest, by = c('type', 'idiom', 'lang')) %>% 
  distinct() ->
  all_data_without_languages

villages %>% 
  select(village, rus_village, lat, lon, gltc_lang, lang, aff, family, lang_col, aff_col) %>%
  filter(lang %in% feature_dataset_4map_languages$lang) %>%
  anti_join(all_data_without_languages %>%  select(village)) %>% 
  inner_join(feature_dataset_4map_languages) %>% 
  distinct() %>% 
  bind_rows(all_data_without_languages)  ->
  alldata_clean  

alldata_clean  %>% 
  distinct(gltc_lang) %>% 
  mutate(lang4map = lang.gltc(gltc_lang),
         display = 'show languages') %>% 
  right_join(alldata_clean) %>% 
  filter(!is.na(contributor)) %>% 
  mutate(type = factor(type, levels = c('language', 'dialect_toplevel', 'dialect_nt1', 'dialect_nt2', 'dialect_nt3', 'village'))) ->
  alldata_clean

#make short

feature_dataset_4map %>% 
  filter(genlang_point == 'yes') %>% 
  mutate(lang = ifelse(idiom == 'Standard Dargwa', 'Standard Dargwa', lang)) %>% 
  left_join(genlang, by = join_by(lang)) %>% 
  distinct() %>% 
  mutate(lang4map = lang.gltc(gltc_lang),
         display = 'show languages') %>% 
  filter(!is.na(contributor)) ->
  all_genpoints
  
rm(all_data_without_languages, change, feature_dataset_4map, feature_dataset_4map_languages, feature_dataset_4map_rest, villages, genlang)
```
",

map(multiple_values$values, function(i){
  str_c(
    "## ", multiple_values$titles[i], "{.tabset .tabset-fade .tabset-pills #m", multiple_values$values[i], "} 
    
### General datapoints {-}

```{r}
all_genpoints %>% 
  filter(!is.na(all_genpoints$value",
            multiple_values$values[i],
            ")) %>% 
  add_count(value",
            multiple_values$values[i],
          ") %>% 
  mutate(value",
    multiple_values$values[i],
    " = str_c(value",
    multiple_values$values[i],
    ", ' (', n, ')')) ->
  all_genpoints_filtered

map.feature(all_genpoints_filtered$lang4map,
            latitude = all_genpoints_filtered$lat,
            longitude = all_genpoints_filtered$lon,
            features = all_genpoints_filtered$value",
    multiple_values$values[i],
    ",
            title = all_genpoints_filtered$value",
    multiple_values$values[i],
    "_name[1],
            color = 'magma',
            stroke.features = 'a',
            stroke.color = 'black',
            stroke.radius = 1,
            stroke.legend = FALSE,
            tile = 'Stamen.TonerLite',
            legend = TRUE,
            legend.position = 'bottomleft', 
            zoom.control = TRUE,
            width = 8) %>% 
  map.feature(all_genpoints_filtered$lang4map,
              latitude = all_genpoints_filtered$lat,
              longitude = all_genpoints_filtered$lon,
              features = all_genpoints_filtered$lang,
              color = all_genpoints_filtered$lang_col,
              tile = 'Stamen.TonerLite',
              legend = FALSE,
              width = 5,
              pipe.data = .,
              control = all_genpoints_filtered$display)
```

### Extrapolated data {-}

```{r}
alldata_clean %>% 
  filter(!is.na(alldata_clean$value",
    multiple_values$values[i],
    ")) ->
  alldata_clean_filtered

map.feature(alldata_clean_filtered$lang4map,
            latitude = alldata_clean_filtered$lat, 
            longitude = alldata_clean_filtered$lon,
            features = alldata_clean_filtered$value",
    multiple_values$values[i],
    ",
            color = 'magma',
            stroke.features = 'a',
            stroke.color = 'black',
            stroke.radius = 1,
            stroke.legend = FALSE,
            tile = 'Stamen.TonerLite',
            title = alldata_clean_filtered$value",
    multiple_values$values[i],
    "_name[1],
            legend = TRUE,
            legend.position = 'bottomleft', 
            label = alldata_clean_filtered$lang,
            zoom.control = TRUE,
            width = 8,
            popup = paste(alldata_clean_filtered$village, '|',
                          alldata_clean_filtered$rus_village, '<br>',
                          'data:', alldata_clean_filtered$type)) %>% 
  map.feature(alldata_clean_filtered$lang4map,
              latitude = alldata_clean_filtered$lat,
              longitude = alldata_clean_filtered$lon,
              features = alldata_clean_filtered$lang,
              color = alldata_clean_filtered$lang_col,
              tile = 'Stamen.TonerLite',
              legend = FALSE,
              width = 5,
              pipe.data = .,
              control = alldata_clean_filtered$display,
              popup = paste(alldata_clean_filtered$village, '|',
                            alldata_clean_filtered$rus_village, '<br>',
                            'data:', alldata_clean_filtered$type))
```

### Data granularity {-}

```{r}
map.feature(alldata_clean_filtered$lang4map,
            latitude = alldata_clean_filtered$lat, 
            longitude = alldata_clean_filtered$lon,
            features = alldata_clean_filtered$value",
    multiple_values$values[i],    
    ",
            color = 'magma',
            stroke.features = 'a',
            stroke.color = 'black',
            stroke.radius = 1,
            stroke.legend = FALSE,
            tile = 'Stamen.TonerLite',
            title = alldata_clean_filtered$value",
    multiple_values$values[i],
    "_name[1],
            legend = TRUE,
            legend.position = 'bottomleft', 
            label = alldata_clean_filtered$lang,
            zoom.control = TRUE,
            control = alldata_clean_filtered$type,
            popup = paste(alldata_clean_filtered$village, '|',
                          alldata_clean_filtered$rus_village, '<br>',
                          'data:', alldata_clean_filtered$type))
```


    ")
}),
"

## Datatable
",
str_c('[Download](https://raw.githubusercontent.com/LingConLab/TALD/master/data/orig_table/', 
      str_remove(str_remove(i, "_map.Rmd"), "\\d{1,}_"),
      '.tsv) the whole dataset and [its description](https://raw.githubusercontent.com/LingConLab/TALD/master/data/md_description/',
      str_remove(str_remove(i, "_map.Rmd"), "\\d{1,}_"),
      '.md).'),
"
```{r}
bib <- RefManageR::ReadBib(file = '../bibliography.bib')

tibble(column = str_subset(colnames(feature_dataset), 'value\\\\d{1,}$')) %>%
  mutate(new_name = feature_dataset %>% 
           select(str_c(column, '_name')) %>% 
           unlist() %>% 
           unique()) ->
  columns_rename

feature_dataset %>% 
  select(lang, idiom, source, page, matches('value\\\\d{1,}$')) %>% 
  rename_with(function(x){columns_rename[columns_rename$column == x, ]$new_name}, matches('value\\\\d{1,}$'))  %>% 
  rename(Language=lang, 
         Idiom = idiom,
         Source = source) %>% 
  mutate(page = str_replace_all(page, '--', '–'),
         Source = str_split(Source, '; '),
         page = str_split(page, '; ')) %>% 
  unnest_longer(col = c(Source, page)) %>% 
  rowwise() %>% 
  mutate(page = ifelse(page == 'NA', NA_character_, page),
         Source = ifelse(str_detect(Source, 'p.c.$'),
                         Source,
                         Cite(bib, Source, 
                       after = ifelse(!is.na(page), 
                                      str_c(': ', page),
                                      '')))) %>%
  select(-page) %>% 
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

map(seq_along(rmd_filenames), function(i){
  ymlthis::yml_empty() %>% 
    ymlthis::yml_title(ifelse(str_detect(rmd_filenames[i], "_map.Rmd"), 
                              str_c(features$title[i], " (Maps & Data)"), 
                              features$title[i])) %>% 
    ymlthis::yml_author(features$author[i]) %>% 
    ymlthis::yml_date(paste0('Last update: ', 
                             ifelse(str_detect(rmd_filenames[i], "_map.Rmd"), 
                                    features$updated_map[i], 
                                    features$updated_text[i]))) %>% 
    ymlthis::yml_citations(bibliography = paste0("./data/orig_bib/", 
                                                 str_remove(features$filename[i], "_map"), 
                                                 ".bib"), 
                           link_citations = TRUE, 
                           csl = "apa.csl") %>% 
    ymlthis::yml_output(html_document(number_sections = TRUE,
                                      anchor_sections = TRUE,
                                      pandoc_args = "--shift-heading-level-by=-1")) %>% 
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
    "BibOptions(check.entries = FALSE, style = 'text', bib.style = 'authoryear')",
    "article_citation <- BibEntry(bibtype = 'Incollection', ",
    paste0(" key='", first_authors[i], features$created_date[i], "',"),
    paste0(" title='", 
           ifelse(str_detect(rmd_filenames[i], "_map.Rmd"), 
                  str_c(features$title[i], " (Maps & Data)"), 
                  features$title[i]), 
           "',"),
    paste0(" author='", str_replace(features$author[i], ",", " and"), "',"),
    paste0(" year='", features$created_date[i], "',"),
    " editor= 'Daniel, Michael  and Filatov, Konstantin and Maisak, Timur and Moroz, George and Mukhin, Timofey and Naccarato, Chiara and Verhees, Samira',",
    " publisher='Linguistic Convergence Laboratory, NRU HSE',",
    " address='Moscow',",
    " booktitle= 'Typological Atlas of the Languages of Daghestan (TALD)',",
    " url='http://lingconlab.ru/dagatlas',",
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
    str_c("```{r, child='data/orig_rmd/", features$filename[i], ".Rmd'}"),
    "```",
    "",
    "```{r, results='asis'}",
    ifelse(str_detect(rmd_filenames[i], "_map.Rmd"), 
           "PrintBibliography(bib)",
           ""),
    "```",
    ""),
    rmd_filenames[i], append = TRUE)
})

# RENDER AND CLEAN ---------------------------------------------------------
rmarkdown::render_site()

beepr::beep()
