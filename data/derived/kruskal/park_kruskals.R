asis_kruskal <- veg_cleaned$site_perc_cover %>% 
  ungroup() %>%
  filter(UnitCode == "ASIS") %>%
  select(UniqueID, UnitCode, Year_chr, SciName_cor, "cover" = mean_site_cover) %>%
  group_by(UniqueID, UnitCode, Year_chr, SciName_cor) %>%
  pivot_wider(., names_from = "SciName_cor", values_from = "cover", values_fill = 0) %>%
  pivot_longer(., cols = -c(UniqueID, UnitCode, Year_chr), names_to = "SciName_cor", values_to = "cover") %>%
  group_by(UnitCode, Year_chr, SciName_cor) %>%
  nest() %>%
  pivot_wider(., names_from = "Year_chr", values_from = "data") %>%
  mutate(cover_2008_2010 = map2(`2008`, `2010`, ~bind_rows(.x,.y) %>% select(cover)),
         year_2008_2010 = map2(`2008`, `2010`, ~data.frame("years" = c(rep("2008", length(.x$cover)), rep("2010", length(.y$cover))))),
         df_2008_2010 = map2(cover_2008_2010, year_2008_2010, ~bind_cols(.x, .y)),
         mod_2008_2010 = map(df_2008_2010, ~kruskal.test(.x$cover ~ .x$years)),
         mod_p_2008_2010 = map(mod_2008_2010, ~p.adjust(.x$p.value, "fdr")),
         
         cover_2008_2012 = map2(`2008`, `2012`, ~bind_rows(.x,.y) %>% select(cover)),
         year_2008_2012 = map2(`2008`, `2012`, ~data.frame("years" = c(rep("2008", length(.x$cover)), rep("2012", length(.y$cover))))),
         df_2008_2012 = map2(cover_2008_2012, year_2008_2012, ~bind_cols(.x, .y)),
         mod_2008_2012 = map(df_2008_2012, ~kruskal.test(.x$cover ~ .x$years)),
         mod_p_2008_2012 = map(mod_2008_2012, ~p.adjust(.x$p.value, "fdr")),
         
         cover_2008_2014 = map2(`2008`, `2014`, ~bind_rows(.x,.y) %>% select(cover)),
         year_2008_2014 = map2(`2008`, `2014`, ~data.frame("years" = c(rep("2008", length(.x$cover)), rep("2014", length(.y$cover))))),
         df_2008_2014 = map2(cover_2008_2014, year_2008_2014, ~bind_cols(.x, .y)),
         mod_2008_2014 = map(df_2008_2014, ~kruskal.test(.x$cover ~ .x$years)),
         mod_p_2008_2014 = map(mod_2008_2014, ~p.adjust(.x$p.value, "fdr")),
         
         cover_2008_2016 = map2(`2008`, `2016`, ~bind_rows(.x,.y) %>% select(cover)),
         year_2008_2016 = map2(`2008`, `2016`, ~data.frame("years" = c(rep("2008", length(.x$cover)), rep("2016", length(.y$cover))))),
         df_2008_2016 = map2(cover_2008_2016, year_2008_2016, ~bind_cols(.x, .y)),
         mod_2008_2016 = map(df_2008_2016, ~kruskal.test(.x$cover ~ .x$years)),
         mod_p_2008_2016 = map(mod_2008_2016, ~p.adjust(.x$p.value, "fdr")),
         
         cover_2008_2018 = map2(`2008`, `2018`, ~bind_rows(.x,.y) %>% select(cover)),
         year_2008_2018 = map2(`2008`, `2018`, ~data.frame("years" = c(rep("2008", length(.x$cover)), rep("2018", length(.y$cover))))),
         df_2008_2018 = map2(cover_2008_2018, year_2008_2018, ~bind_cols(.x, .y)),
         mod_2008_2018 = map(df_2008_2018, ~kruskal.test(.x$cover ~ .x$years)),
         mod_p_2008_2018 = map(mod_2008_2018, ~p.adjust(.x$p.value, "fdr"))
  ) %>%
  select(-c(cover_2008_2010, cover_2008_2012, cover_2008_2014, cover_2008_2016, cover_2008_2018,
            year_2008_2010, year_2008_2012, year_2008_2014, year_2008_2016, year_2008_2018,
            df_2008_2010, df_2008_2012, df_2008_2014, df_2008_2016, df_2008_2018)) %>%
  
  mutate(cover_2010_2012 = map2(`2010`, `2012`, ~bind_rows(.x,.y) %>% select(cover)),
         year_2010_2012 = map2(`2010`, `2012`, ~data.frame("years" = c(rep("2010", length(.x$cover)), rep("2012", length(.y$cover))))),
         df_2010_2012 = map2(cover_2010_2012, year_2010_2012, ~bind_cols(.x, .y)),
         mod_2010_2012 = map(df_2010_2012, ~kruskal.test(.x$cover ~ .x$years)),
         mod_p_2010_2012 = map(mod_2010_2012, ~p.adjust(.x$p.value, "fdr")),
         
         cover_2010_2014 = map2(`2010`, `2014`, ~bind_rows(.x,.y) %>% select(cover)),
         year_2010_2014 = map2(`2010`, `2014`, ~data.frame("years" = c(rep("2010", length(.x$cover)), rep("2014", length(.y$cover))))),
         df_2010_2014 = map2(cover_2010_2014, year_2010_2014, ~bind_cols(.x, .y)),
         mod_2010_2014 = map(df_2010_2014, ~kruskal.test(.x$cover ~ .x$years)),
         mod_p_2010_2014 = map(mod_2010_2014, ~p.adjust(.x$p.value, "fdr")),
         
         cover_2010_2016 = map2(`2010`, `2016`, ~bind_rows(.x,.y) %>% select(cover)),
         year_2010_2016 = map2(`2010`, `2016`, ~data.frame("years" = c(rep("2010", length(.x$cover)), rep("2016", length(.y$cover))))),
         df_2010_2016 = map2(cover_2010_2016, year_2010_2016, ~bind_cols(.x, .y)),
         mod_2010_2016 = map(df_2010_2016, ~kruskal.test(.x$cover ~ .x$years)),
         mod_p_2010_2016 = map(mod_2010_2016, ~p.adjust(.x$p.value, "fdr")),
         
         cover_2010_2018 = map2(`2010`, `2018`, ~bind_rows(.x,.y) %>% select(cover)),
         year_2010_2018 = map2(`2010`, `2018`, ~data.frame("years" = c(rep("2010", length(.x$cover)), rep("2018", length(.y$cover))))),
         df_2010_2018 = map2(cover_2010_2018, year_2010_2018, ~bind_cols(.x, .y)),
         mod_2010_2018 = map(df_2010_2018, ~kruskal.test(.x$cover ~ .x$years)),
         mod_p_2010_2018 = map(mod_2010_2018, ~p.adjust(.x$p.value, "fdr"))
  ) %>%
  select(-c(cover_2010_2012, cover_2010_2014, cover_2010_2016, cover_2010_2018,
            year_2010_2012, year_2010_2014, year_2010_2016, year_2010_2018,
            df_2010_2012, df_2010_2014, df_2010_2016, df_2010_2018)) %>%
  
  mutate(cover_2012_2014 = map2(`2012`, `2014`, ~bind_rows(.x,.y) %>% select(cover)),
         year_2012_2014 = map2(`2012`, `2014`, ~data.frame("years" = c(rep("2012", length(.x$cover)), rep("2014", length(.y$cover))))),
         df_2012_2014 = map2(cover_2012_2014, year_2012_2014, ~bind_cols(.x, .y)),
         mod_2012_2014 = map(df_2012_2014, ~kruskal.test(.x$cover ~ .x$years)),
         mod_p_2012_2014 = map(mod_2012_2014, ~p.adjust(.x$p.value, "fdr")),
         
         cover_2012_2016 = map2(`2012`, `2016`, ~bind_rows(.x,.y) %>% select(cover)),
         year_2012_2016 = map2(`2012`, `2016`, ~data.frame("years" = c(rep("2012", length(.x$cover)), rep("2016", length(.y$cover))))),
         df_2012_2016 = map2(cover_2012_2016, year_2012_2016, ~bind_cols(.x, .y)),
         mod_2012_2016 = map(df_2012_2016, ~kruskal.test(.x$cover ~ .x$years)),
         mod_p_2012_2016 = map(mod_2012_2016, ~p.adjust(.x$p.value, "fdr")),
         
         cover_2012_2018 = map2(`2012`, `2018`, ~bind_rows(.x,.y) %>% select(cover)),
         year_2012_2018 = map2(`2012`, `2018`, ~data.frame("years" = c(rep("2012", length(.x$cover)), rep("2018", length(.y$cover))))),
         df_2012_2018 = map2(cover_2012_2018, year_2012_2018, ~bind_cols(.x, .y)),
         mod_2012_2018 = map(df_2012_2018, ~kruskal.test(.x$cover ~ .x$years)),
         mod_p_2012_2018 = map(mod_2012_2018, ~p.adjust(.x$p.value, "fdr"))) %>%
  select(-c(cover_2012_2014, cover_2012_2016, cover_2012_2018,
            year_2012_2014, year_2012_2016, year_2012_2018,
            df_2012_2014, df_2012_2016, df_2012_2018)) %>%
  
  mutate(cover_2014_2016 = map2(`2014`, `2016`, ~bind_rows(.x,.y) %>% select(cover)),
         year_2014_2016 = map2(`2014`, `2016`, ~data.frame("years" = c(rep("2014", length(.x$cover)), rep("2016", length(.y$cover))))),
         df_2014_2016 = map2(cover_2014_2016, year_2014_2016, ~bind_cols(.x, .y)),
         mod_2014_2016 = map(df_2014_2016, ~kruskal.test(.x$cover ~ .x$years)),
         mod_p_2014_2016 = map(mod_2014_2016, ~p.adjust(.x$p.value, "fdr")),
         
         cover_2014_2018 = map2(`2014`, `2018`, ~bind_rows(.x,.y) %>% select(cover)),
         year_2014_2018 = map2(`2014`, `2018`, ~data.frame("years" = c(rep("2014", length(.x$cover)), rep("2018", length(.y$cover))))),
         df_2014_2018 = map2(cover_2014_2018, year_2014_2018, ~bind_cols(.x, .y)),
         mod_2014_2018 = map(df_2014_2018, ~kruskal.test(.x$cover ~ .x$years)),
         mod_p_2014_2018 = map(mod_2014_2018, ~p.adjust(.x$p.value, "fdr"))) %>%
  select(-c(cover_2014_2016, cover_2014_2018,
            year_2014_2016, year_2014_2018,
            df_2014_2016, df_2014_2018)) %>%
  
  mutate(cover_2016_2018 = map2(`2016`, `2018`, ~bind_rows(.x,.y) %>% select(cover)),
         year_2016_2018 = map2(`2016`, `2018`, ~data.frame("years" = c(rep("2016", length(.x$cover)), rep("2018", length(.y$cover))))),
         df_2016_2018 = map2(cover_2016_2018, year_2016_2018, ~bind_cols(.x, .y)),
         mod_2016_2018 = map(df_2016_2018, ~kruskal.test(.x$cover ~ .x$years)),
         mod_p_2016_2018 = map(mod_2016_2018, ~p.adjust(.x$p.value, "fdr"))) %>%
  select(-c(cover_2016_2018,
            year_2016_2018,
            df_2016_2018))  %>%
  filter(!if_all(starts_with("mod_p_"), is.na)) %>%
  filter(if_any(starts_with("mod_p"), ~. < 0.05))

#save(asis_kruskal, file = "./data/derived/kruskal/asis_kruskal.rda")

colo_kruskal <- veg_cleaned$site_perc_cover %>%
  ungroup() %>%
  filter(UnitCode == "COLO") %>%
  select(UniqueID, UnitCode, Year_chr, SciName_cor, "cover" = mean_site_cover) %>%
  group_by(UniqueID, UnitCode, Year_chr, SciName_cor) %>%
  pivot_wider(., names_from = "SciName_cor", values_from = "cover", values_fill = 0) %>%
  pivot_longer(., cols = -c(UniqueID, UnitCode, Year_chr), names_to = "SciName_cor", values_to = "cover") %>%
  group_by(UnitCode, Year_chr, SciName_cor) %>%
  nest() %>%
  pivot_wider(., names_from = "Year_chr", values_from = "data") %>%
  mutate(cover_2008_2010 = map2(`2008`, `2010`, ~bind_rows(.x,.y) %>% select(cover)),
         year_2008_2010 = map2(`2008`, `2010`, ~data.frame("years" = c(rep("2008", length(.x$cover)), rep("2010", length(.y$cover))))),
         df_2008_2010 = map2(cover_2008_2010, year_2008_2010, ~bind_cols(.x, .y)),
         mod_2008_2010 = map(df_2008_2010, ~kruskal.test(.x$cover ~ .x$years)),
         mod_p_2008_2010 = map(mod_2008_2010, ~p.adjust(.x$p.value, "fdr")),
         
         cover_2008_2012 = map2(`2008`, `2012`, ~bind_rows(.x,.y) %>% select(cover)),
         year_2008_2012 = map2(`2008`, `2012`, ~data.frame("years" = c(rep("2008", length(.x$cover)), rep("2012", length(.y$cover))))),
         df_2008_2012 = map2(cover_2008_2012, year_2008_2012, ~bind_cols(.x, .y)),
         mod_2008_2012 = map(df_2008_2012, ~kruskal.test(.x$cover ~ .x$years)),
         mod_p_2008_2012 = map(mod_2008_2012, ~p.adjust(.x$p.value, "fdr")),
         
         cover_2008_2014 = map2(`2008`, `2014`, ~bind_rows(.x,.y) %>% select(cover)),
         year_2008_2014 = map2(`2008`, `2014`, ~data.frame("years" = c(rep("2008", length(.x$cover)), rep("2014", length(.y$cover))))),
         df_2008_2014 = map2(cover_2008_2014, year_2008_2014, ~bind_cols(.x, .y)),
         mod_2008_2014 = map(df_2008_2014, ~kruskal.test(.x$cover ~ .x$years)),
         mod_p_2008_2014 = map(mod_2008_2014, ~p.adjust(.x$p.value, "fdr")),
         
         cover_2008_2016 = map2(`2008`, `2016`, ~bind_rows(.x,.y) %>% select(cover)),
         year_2008_2016 = map2(`2008`, `2016`, ~data.frame("years" = c(rep("2008", length(.x$cover)), rep("2016", length(.y$cover))))),
         df_2008_2016 = map2(cover_2008_2016, year_2008_2016, ~bind_cols(.x, .y)),
         mod_2008_2016 = map(df_2008_2016, ~kruskal.test(.x$cover ~ .x$years)),
         mod_p_2008_2016 = map(mod_2008_2016, ~p.adjust(.x$p.value, "fdr"))) %>%
  select(-c(cover_2008_2010, cover_2008_2012, cover_2008_2014, cover_2008_2016,
            year_2008_2010, year_2008_2012, year_2008_2014, year_2008_2016,
            df_2008_2010, df_2008_2012, df_2008_2014, df_2008_2016)) %>%
  
  mutate(cover_2010_2012 = map2(`2010`, `2012`, ~bind_rows(.x,.y) %>% select(cover)),
         year_2010_2012 = map2(`2010`, `2012`, ~data.frame("years" = c(rep("2010", length(.x$cover)), rep("2012", length(.y$cover))))),
         df_2010_2012 = map2(cover_2010_2012, year_2010_2012, ~bind_cols(.x, .y)),
         mod_2010_2012 = map(df_2010_2012, ~kruskal.test(.x$cover ~ .x$years)),
         mod_p_2010_2012 = map(mod_2010_2012, ~p.adjust(.x$p.value, "fdr")),
         
         cover_2010_2014 = map2(`2010`, `2014`, ~bind_rows(.x,.y) %>% select(cover)),
         year_2010_2014 = map2(`2010`, `2014`, ~data.frame("years" = c(rep("2010", length(.x$cover)), rep("2014", length(.y$cover))))),
         df_2010_2014 = map2(cover_2010_2014, year_2010_2014, ~bind_cols(.x, .y)),
         mod_2010_2014 = map(df_2010_2014, ~kruskal.test(.x$cover ~ .x$years)),
         mod_p_2010_2014 = map(mod_2010_2014, ~p.adjust(.x$p.value, "fdr")),
         
         cover_2010_2016 = map2(`2010`, `2016`, ~bind_rows(.x,.y) %>% select(cover)),
         year_2010_2016 = map2(`2010`, `2016`, ~data.frame("years" = c(rep("2010", length(.x$cover)), rep("2016", length(.y$cover))))),
         df_2010_2016 = map2(cover_2010_2016, year_2010_2016, ~bind_cols(.x, .y)),
         mod_2010_2016 = map(df_2010_2016, ~kruskal.test(.x$cover ~ .x$years)),
         mod_p_2010_2016 = map(mod_2010_2016, ~p.adjust(.x$p.value, "fdr"))) %>%
  select(-c(cover_2010_2012, cover_2010_2014, cover_2010_2016,
            year_2010_2012, year_2010_2014, year_2010_2016, 
            df_2010_2012, df_2010_2014, df_2010_2016)) %>%
  
  mutate(cover_2012_2014 = map2(`2012`, `2014`, ~bind_rows(.x,.y) %>% select(cover)),
         year_2012_2014 = map2(`2012`, `2014`, ~data.frame("years" = c(rep("2012", length(.x$cover)), rep("2014", length(.y$cover))))),
         df_2012_2014 = map2(cover_2012_2014, year_2012_2014, ~bind_cols(.x, .y)),
         mod_2012_2014 = map(df_2012_2014, ~kruskal.test(.x$cover ~ .x$years)),
         mod_p_2012_2014 = map(mod_2012_2014, ~p.adjust(.x$p.value, "fdr")),
         
         cover_2012_2016 = map2(`2012`, `2016`, ~bind_rows(.x,.y) %>% select(cover)),
         year_2012_2016 = map2(`2012`, `2016`, ~data.frame("years" = c(rep("2012", length(.x$cover)), rep("2016", length(.y$cover))))),
         df_2012_2016 = map2(cover_2012_2016, year_2012_2016, ~bind_cols(.x, .y)),
         mod_2012_2016 = map(df_2012_2016, ~kruskal.test(.x$cover ~ .x$years)),
         mod_p_2012_2016 = map(mod_2012_2016, ~p.adjust(.x$p.value, "fdr"))) %>%
  select(-c(cover_2012_2014, cover_2012_2016,
            year_2012_2014, year_2012_2016,
            df_2012_2014, df_2012_2016)) %>%
  
  mutate(cover_2014_2016 = map2(`2014`, `2016`, ~bind_rows(.x,.y) %>% select(cover)),
         year_2014_2016 = map2(`2014`, `2016`, ~data.frame("years" = c(rep("2014", length(.x$cover)), rep("2016", length(.y$cover))))),
         df_2014_2016 = map2(cover_2014_2016, year_2014_2016, ~bind_cols(.x, .y)),
         mod_2014_2016 = map(df_2014_2016, ~kruskal.test(.x$cover ~ .x$years)),
         mod_p_2014_2016 = map(mod_2014_2016, ~p.adjust(.x$p.value, "fdr"))) %>%
  select(-c(cover_2014_2016,
            year_2014_2016, 
            df_2014_2016)) %>%
  filter(!if_all(starts_with("mod_p_"), is.na)) %>%
  filter(if_any(starts_with("mod_p"), ~. < 0.05))

#save(colo_kruskal, file = "./data/derived/kruskal/colo_kruskal.rda")

fiis_kruskal <- veg_cleaned$site_perc_cover %>% 
  ungroup() %>%
  filter(UnitCode == "FIIS") %>%
  select(UniqueID, UnitCode, Year_chr, SciName_cor, "cover" = mean_site_cover) %>%
  group_by(UniqueID, UnitCode, Year_chr, SciName_cor) %>%
  pivot_wider(., names_from = "SciName_cor", values_from = "cover", values_fill = 0) %>%
  pivot_longer(., cols = -c(UniqueID, UnitCode, Year_chr), names_to = "SciName_cor", values_to = "cover") %>%
  group_by(UnitCode, Year_chr, SciName_cor) %>%
  nest() %>%
  pivot_wider(., names_from = "Year_chr", values_from = "data") %>%
  mutate(cover_2009_2011 = map2(`2009`, `2011`, ~bind_rows(.x,.y) %>% select(cover)),
         year_2009_2011 = map2(`2009`, `2011`, ~data.frame("years" = c(rep("2009", length(.x$cover)), rep("2011", length(.y$cover))))),
         df_2009_2011 = map2(cover_2009_2011, year_2009_2011, ~bind_cols(.x, .y)),
         mod_2009_2011 = map(df_2009_2011, ~kruskal.test(.x$cover ~ .x$years)),
         mod_p_2009_2011 = map(mod_2009_2011, ~p.adjust(.x$p.value, "fdr")),
         
         cover_2009_2013 = map2(`2009`, `2013`, ~bind_rows(.x,.y) %>% select(cover)),
         year_2009_2013 = map2(`2009`, `2013`, ~data.frame("years" = c(rep("2009", length(.x$cover)), rep("2013", length(.y$cover))))),
         df_2009_2013 = map2(cover_2009_2013, year_2009_2013, ~bind_cols(.x, .y)),
         mod_2009_2013 = map(df_2009_2013, ~kruskal.test(.x$cover ~ .x$years)),
         mod_p_2009_2013 = map(mod_2009_2013, ~p.adjust(.x$p.value, "fdr")),
         
         cover_2009_2015 = map2(`2009`, `2015`, ~bind_rows(.x,.y) %>% select(cover)),
         year_2009_2015 = map2(`2009`, `2015`, ~data.frame("years" = c(rep("2009", length(.x$cover)), rep("2015", length(.y$cover))))),
         df_2009_2015 = map2(cover_2009_2015, year_2009_2015, ~bind_cols(.x, .y)),
         mod_2009_2015 = map(df_2009_2015, ~kruskal.test(.x$cover ~ .x$years)),
         mod_p_2009_2015 = map(mod_2009_2015, ~p.adjust(.x$p.value, "fdr")),
         
         cover_2009_2017 = map2(`2009`, `2017`, ~bind_rows(.x,.y) %>% select(cover)),
         year_2009_2017 = map2(`2009`, `2017`, ~data.frame("years" = c(rep("2009", length(.x$cover)), rep("2017", length(.y$cover))))),
         df_2009_2017 = map2(cover_2009_2017, year_2009_2017, ~bind_cols(.x, .y)),
         mod_2009_2017 = map(df_2009_2017, ~kruskal.test(.x$cover ~ .x$years)),
         mod_p_2009_2017 = map(mod_2009_2017, ~p.adjust(.x$p.value, "fdr"))
  ) %>%
  select(-c(cover_2009_2011, cover_2009_2013, cover_2009_2015, cover_2009_2017, 
            year_2009_2011, year_2009_2013, year_2009_2015, year_2009_2017,
            df_2009_2011, df_2009_2013, df_2009_2015, df_2009_2017)) %>%
  
  mutate(cover_2011_2013 = map2(`2011`, `2013`, ~bind_rows(.x,.y) %>% select(cover)),
         year_2011_2013 = map2(`2011`, `2013`, ~data.frame("years" = c(rep("2011", length(.x$cover)), rep("2013", length(.y$cover))))),
         df_2011_2013 = map2(cover_2011_2013, year_2011_2013, ~bind_cols(.x, .y)),
         mod_2011_2013 = map(df_2011_2013, ~kruskal.test(.x$cover ~ .x$years)),
         mod_p_2011_2013 = map(mod_2011_2013, ~p.adjust(.x$p.value, "fdr")),
         
         cover_2011_2015 = map2(`2011`, `2015`, ~bind_rows(.x,.y) %>% select(cover)),
         year_2011_2015 = map2(`2011`, `2015`, ~data.frame("years" = c(rep("2011", length(.x$cover)), rep("2015", length(.y$cover))))),
         df_2011_2015 = map2(cover_2011_2015, year_2011_2015, ~bind_cols(.x, .y)),
         mod_2011_2015 = map(df_2011_2015, ~kruskal.test(.x$cover ~ .x$years)),
         mod_p_2011_2015 = map(mod_2011_2015, ~p.adjust(.x$p.value, "fdr")),
         
         cover_2011_2017 = map2(`2011`, `2017`, ~bind_rows(.x,.y) %>% select(cover)),
         year_2011_2017 = map2(`2011`, `2017`, ~data.frame("years" = c(rep("2011", length(.x$cover)), rep("2017", length(.y$cover))))),
         df_2011_2017 = map2(cover_2011_2017, year_2011_2017, ~bind_cols(.x, .y)),
         mod_2011_2017 = map(df_2011_2017, ~kruskal.test(.x$cover ~ .x$years)),
         mod_p_2011_2017 = map(mod_2011_2017, ~p.adjust(.x$p.value, "fdr"))
  ) %>%
  select(-c(cover_2011_2013, cover_2011_2015, cover_2011_2017, 
            year_2011_2013, year_2011_2015, year_2011_2017,
            df_2011_2013, df_2011_2015, df_2011_2017)) %>%
  
  mutate(cover_2013_2015 = map2(`2013`, `2015`, ~bind_rows(.x,.y) %>% select(cover)),
         year_2013_2015 = map2(`2013`, `2015`, ~data.frame("years" = c(rep("2013", length(.x$cover)), rep("2015", length(.y$cover))))),
         df_2013_2015 = map2(cover_2013_2015, year_2013_2015, ~bind_cols(.x, .y)),
         mod_2013_2015 = map(df_2013_2015, ~kruskal.test(.x$cover ~ .x$years)),
         mod_p_2013_2015 = map(mod_2013_2015, ~p.adjust(.x$p.value, "fdr")),
         
         cover_2013_2017 = map2(`2013`, `2017`, ~bind_rows(.x,.y) %>% select(cover)),
         year_2013_2017 = map2(`2013`, `2017`, ~data.frame("years" = c(rep("2013", length(.x$cover)), rep("2017", length(.y$cover))))),
         df_2013_2017 = map2(cover_2013_2017, year_2013_2017, ~bind_cols(.x, .y)),
         mod_2013_2017 = map(df_2013_2017, ~kruskal.test(.x$cover ~ .x$years)),
         mod_p_2013_2017 = map(mod_2013_2017, ~p.adjust(.x$p.value, "fdr"))) %>%
  select(-c(cover_2013_2015, cover_2013_2017, 
            year_2013_2015, year_2013_2017,
            df_2013_2015, df_2013_2017)) %>%
  mutate(cover_2015_2017 = map2(`2015`, `2017`, ~bind_rows(.x,.y) %>% select(cover)),
         year_2015_2017 = map2(`2015`, `2017`, ~data.frame("years" = c(rep("2015", length(.x$cover)), rep("2017", length(.y$cover))))),
         df_2015_2017 = map2(cover_2015_2017, year_2015_2017, ~bind_cols(.x, .y)),
         mod_2015_2017 = map(df_2015_2017, ~kruskal.test(.x$cover ~ .x$years)),
         mod_p_2015_2017 = map(mod_2015_2017, ~p.adjust(.x$p.value, "fdr"))) %>%
  select(-c(cover_2015_2017, 
            year_2015_2017,
            df_2015_2017)) %>%
  filter(!if_all(starts_with("mod_p_"), is.na)) %>%
  filter(if_any(starts_with("mod_p"), ~. < 0.05))

#save(fiis_kruskal, file = "./data/derived/kruskal/fiis_kruskal.rda")

gate_kruskal <- veg_cleaned$site_perc_cover %>% 
  ungroup() %>%
  filter(UnitCode == "GATE") %>%
  select(UniqueID, UnitCode, Year_chr, SciName_cor, "cover" = mean_site_cover) %>%
  group_by(UniqueID, UnitCode, Year_chr, SciName_cor) %>%
  pivot_wider(., names_from = "SciName_cor", values_from = "cover", values_fill = 0) %>%
  pivot_longer(., cols = -c(UniqueID, UnitCode, Year_chr), names_to = "SciName_cor", values_to = "cover") %>%
  group_by(UnitCode, Year_chr, SciName_cor) %>%
  nest() %>%
  pivot_wider(., names_from = "Year_chr", values_from = "data") %>%
  mutate(cover_2010_2014 = map2(`2010`, `2014`, ~bind_rows(.x,.y) %>% select(cover)),
         year_2010_2014 = map2(`2010`, `2014`, ~data.frame("years" = c(rep("2010", length(.x$cover)), rep("2014", length(.y$cover))))),
         df_2010_2014 = map2(cover_2010_2014, year_2010_2014, ~bind_cols(.x, .y)),
         mod_2010_2014 = map(df_2010_2014, ~kruskal.test(.x$cover ~ .x$years)),
         mod_p_2010_2014 = map(mod_2010_2014, ~p.adjust(.x$p.value, "fdr")),
         
         cover_2010_2016 = map2(`2010`, `2016`, ~bind_rows(.x,.y) %>% select(cover)),
         year_2010_2016 = map2(`2010`, `2016`, ~data.frame("years" = c(rep("2010", length(.x$cover)), rep("2016", length(.y$cover))))),
         df_2010_2016 = map2(cover_2010_2016, year_2010_2016, ~bind_cols(.x, .y)),
         mod_2010_2016 = map(df_2010_2016, ~kruskal.test(.x$cover ~ .x$years)),
         mod_p_2010_2016 = map(mod_2010_2016, ~p.adjust(.x$p.value, "fdr"))
  ) %>%
  select(-c(cover_2010_2014, cover_2010_2016,
            year_2010_2014, year_2010_2016,
            df_2010_2014, df_2010_2016)) %>%
  
  mutate(cover_2014_2016 = map2(`2014`, `2016`, ~bind_rows(.x,.y) %>% select(cover)),
         year_2014_2016 = map2(`2014`, `2016`, ~data.frame("years" = c(rep("2014", length(.x$cover)), rep("2016", length(.y$cover))))),
         df_2014_2016 = map2(cover_2014_2016, year_2014_2016, ~bind_cols(.x, .y)),
         mod_2014_2016 = map(df_2014_2016, ~kruskal.test(.x$cover ~ .x$years)),
         mod_p_2014_2016 = map(mod_2014_2016, ~p.adjust(.x$p.value, "fdr")),
         
         cover_2014_2018 = map2(`2014`, `2018`, ~bind_rows(.x,.y) %>% select(cover)),
         year_2014_2018 = map2(`2014`, `2018`, ~data.frame("years" = c(rep("2014", length(.x$cover)), rep("2018", length(.y$cover))))),
         df_2014_2018 = map2(cover_2014_2018, year_2014_2018, ~bind_cols(.x, .y)),
         mod_2014_2018 = map(df_2014_2018, ~kruskal.test(.x$cover ~ .x$years)),
         mod_p_2014_2018 = map(mod_2014_2018, ~p.adjust(.x$p.value, "fdr"))) %>%
  select(-c(cover_2014_2016, cover_2014_2018,
            year_2014_2016, year_2014_2018,
            df_2014_2016, df_2014_2018)) %>%
  
  mutate(cover_2016_2018 = map2(`2016`, `2018`, ~bind_rows(.x,.y) %>% select(cover)),
         year_2016_2018 = map2(`2016`, `2018`, ~data.frame("years" = c(rep("2016", length(.x$cover)), rep("2018", length(.y$cover))))),
         df_2016_2018 = map2(cover_2016_2018, year_2016_2018, ~bind_cols(.x, .y)),
         mod_2016_2018 = map(df_2016_2018, ~kruskal.test(.x$cover ~ .x$years)),
         mod_p_2016_2018 = map(mod_2016_2018, ~p.adjust(.x$p.value, "fdr"))) %>%
  select(-c(cover_2016_2018,
            year_2016_2018,
            df_2016_2018))  %>%
  filter(!if_all(starts_with("mod_p_"), is.na)) %>%
  filter(if_any(starts_with("mod_p"), ~. < 0.05))

#save(gate_kruskal, file = "./data/derived/kruskal/gate_kruskal.rda")

gewa_kruskal <- veg_cleaned$site_perc_cover %>% 
  ungroup() %>%
  filter(UnitCode == "GEWA") %>%
  select(UniqueID, UnitCode, Year_chr, SciName_cor, "cover" = mean_site_cover) %>%
  group_by(UniqueID, UnitCode, Year_chr, SciName_cor) %>%
  pivot_wider(., names_from = "SciName_cor", values_from = "cover", values_fill = 0) %>%
  pivot_longer(., cols = -c(UniqueID, UnitCode, Year_chr), names_to = "SciName_cor", values_to = "cover") %>%
  group_by(UnitCode, Year_chr, SciName_cor) %>%
  nest() %>%
  pivot_wider(., names_from = "Year_chr", values_from = "data") %>%
  mutate(cover_2008_2010 = map2(`2008`, `2010`, ~bind_rows(.x,.y) %>% select(cover)),
         year_2008_2010 = map2(`2008`, `2010`, ~data.frame("years" = c(rep("2008", length(.x$cover)), rep("2010", length(.y$cover))))),
         df_2008_2010 = map2(cover_2008_2010, year_2008_2010, ~bind_cols(.x, .y)),
         mod_2008_2010 = map(df_2008_2010, ~kruskal.test(.x$cover ~ .x$years)),
         mod_p_2008_2010 = map(mod_2008_2010, ~p.adjust(.x$p.value, "fdr")),
         
         cover_2008_2012 = map2(`2008`, `2012`, ~bind_rows(.x,.y) %>% select(cover)),
         year_2008_2012 = map2(`2008`, `2012`, ~data.frame("years" = c(rep("2008", length(.x$cover)), rep("2012", length(.y$cover))))),
         df_2008_2012 = map2(cover_2008_2012, year_2008_2012, ~bind_cols(.x, .y)),
         mod_2008_2012 = map(df_2008_2012, ~kruskal.test(.x$cover ~ .x$years)),
         mod_p_2008_2012 = map(mod_2008_2012, ~p.adjust(.x$p.value, "fdr"))) %>%
  select(-c(cover_2008_2010, cover_2008_2012, 
            year_2008_2010, year_2008_2012,
            df_2008_2010, df_2008_2012)) %>%
  
  mutate(cover_2010_2012 = map2(`2010`, `2012`, ~bind_rows(.x,.y) %>% select(cover)),
         year_2010_2012 = map2(`2010`, `2012`, ~data.frame("years" = c(rep("2010", length(.x$cover)), rep("2012", length(.y$cover))))),
         df_2010_2012 = map2(cover_2010_2012, year_2010_2012, ~bind_cols(.x, .y)),
         mod_2010_2012 = map(df_2010_2012, ~kruskal.test(.x$cover ~ .x$years)),
         mod_p_2010_2012 = map(mod_2010_2012, ~p.adjust(.x$p.value, "fdr")) ) %>%
  select(-c(cover_2010_2012,
            year_2010_2012,
            df_2010_2012)) %>%
  filter(!if_all(starts_with("mod_p_"), is.na)) %>%
  filter(if_any(starts_with("mod_p"), ~. < 0.05))

#save(gewa_kruskal, file = "./data/derived/kruskal/gewa_kruskal.rda")

sahi_kruskal <- veg_cleaned$site_perc_cover %>% 
  ungroup() %>%
  filter(UnitCode == "SAHI") %>%
  select(UniqueID, UnitCode, Year_chr, SciName_cor, "cover" = mean_site_cover) %>%
  group_by(UniqueID, UnitCode, Year_chr, SciName_cor) %>%
  pivot_wider(., names_from = "SciName_cor", values_from = "cover", values_fill = 0) %>%
  pivot_longer(., cols = -c(UniqueID, UnitCode, Year_chr), names_to = "SciName_cor", values_to = "cover") %>%
  group_by(UnitCode, Year_chr, SciName_cor) %>%
  nest() %>%
  pivot_wider(., names_from = "Year_chr", values_from = "data") %>%
  mutate(cover_2009_2011 = map2(`2009`, `2011`, ~bind_rows(.x,.y) %>% select(cover)),
         year_2009_2011 = map2(`2009`, `2011`, ~data.frame("years" = c(rep("2009", length(.x$cover)), rep("2011", length(.y$cover))))),
         df_2009_2011 = map2(cover_2009_2011, year_2009_2011, ~bind_cols(.x, .y)),
         mod_2009_2011 = map(df_2009_2011, ~kruskal.test(.x$cover ~ .x$years)),
         mod_p_2009_2011 = map(mod_2009_2011, ~p.adjust(.x$p.value, "fdr")),
         
         cover_2009_2013 = map2(`2009`, `2013`, ~bind_rows(.x,.y) %>% select(cover)),
         year_2009_2013 = map2(`2009`, `2013`, ~data.frame("years" = c(rep("2009", length(.x$cover)), rep("2013", length(.y$cover))))),
         df_2009_2013 = map2(cover_2009_2013, year_2009_2013, ~bind_cols(.x, .y)),
         mod_2009_2013 = map(df_2009_2013, ~kruskal.test(.x$cover ~ .x$years)),
         mod_p_2009_2013 = map(mod_2009_2013, ~p.adjust(.x$p.value, "fdr")),
         
         cover_2009_2015 = map2(`2009`, `2015`, ~bind_rows(.x,.y) %>% select(cover)),
         year_2009_2015 = map2(`2009`, `2015`, ~data.frame("years" = c(rep("2009", length(.x$cover)), rep("2015", length(.y$cover))))),
         df_2009_2015 = map2(cover_2009_2015, year_2009_2015, ~bind_cols(.x, .y)),
         mod_2009_2015 = map(df_2009_2015, ~kruskal.test(.x$cover ~ .x$years)),
         mod_p_2009_2015 = map(mod_2009_2015, ~p.adjust(.x$p.value, "fdr")),
         
         cover_2009_2017 = map2(`2009`, `2017`, ~bind_rows(.x,.y) %>% select(cover)),
         year_2009_2017 = map2(`2009`, `2017`, ~data.frame("years" = c(rep("2009", length(.x$cover)), rep("2017", length(.y$cover))))),
         df_2009_2017 = map2(cover_2009_2017, year_2009_2017, ~bind_cols(.x, .y)),
         mod_2009_2017 = map(df_2009_2017, ~kruskal.test(.x$cover ~ .x$years)),
         mod_p_2009_2017 = map(mod_2009_2017, ~p.adjust(.x$p.value, "fdr"))
  ) %>%
  select(-c(cover_2009_2011, cover_2009_2013, cover_2009_2015, cover_2009_2017, 
            year_2009_2011, year_2009_2013, year_2009_2015, year_2009_2017,
            df_2009_2011, df_2009_2013, df_2009_2015, df_2009_2017)) %>%
  
  mutate(cover_2011_2013 = map2(`2011`, `2013`, ~bind_rows(.x,.y) %>% select(cover)),
         year_2011_2013 = map2(`2011`, `2013`, ~data.frame("years" = c(rep("2011", length(.x$cover)), rep("2013", length(.y$cover))))),
         df_2011_2013 = map2(cover_2011_2013, year_2011_2013, ~bind_cols(.x, .y)),
         mod_2011_2013 = map(df_2011_2013, ~kruskal.test(.x$cover ~ .x$years)),
         mod_p_2011_2013 = map(mod_2011_2013, ~p.adjust(.x$p.value, "fdr")),
         
         cover_2011_2015 = map2(`2011`, `2015`, ~bind_rows(.x,.y) %>% select(cover)),
         year_2011_2015 = map2(`2011`, `2015`, ~data.frame("years" = c(rep("2011", length(.x$cover)), rep("2015", length(.y$cover))))),
         df_2011_2015 = map2(cover_2011_2015, year_2011_2015, ~bind_cols(.x, .y)),
         mod_2011_2015 = map(df_2011_2015, ~kruskal.test(.x$cover ~ .x$years)),
         mod_p_2011_2015 = map(mod_2011_2015, ~p.adjust(.x$p.value, "fdr")),
         
         cover_2011_2017 = map2(`2011`, `2017`, ~bind_rows(.x,.y) %>% select(cover)),
         year_2011_2017 = map2(`2011`, `2017`, ~data.frame("years" = c(rep("2011", length(.x$cover)), rep("2017", length(.y$cover))))),
         df_2011_2017 = map2(cover_2011_2017, year_2011_2017, ~bind_cols(.x, .y)),
         mod_2011_2017 = map(df_2011_2017, ~kruskal.test(.x$cover ~ .x$years)),
         mod_p_2011_2017 = map(mod_2011_2017, ~p.adjust(.x$p.value, "fdr"))
  ) %>%
  select(-c(cover_2011_2013, cover_2011_2015, cover_2011_2017, 
            year_2011_2013, year_2011_2015, year_2011_2017,
            df_2011_2013, df_2011_2015, df_2011_2017)) %>%
  
  mutate(cover_2013_2015 = map2(`2013`, `2015`, ~bind_rows(.x,.y) %>% select(cover)),
         year_2013_2015 = map2(`2013`, `2015`, ~data.frame("years" = c(rep("2013", length(.x$cover)), rep("2015", length(.y$cover))))),
         df_2013_2015 = map2(cover_2013_2015, year_2013_2015, ~bind_cols(.x, .y)),
         mod_2013_2015 = map(df_2013_2015, ~kruskal.test(.x$cover ~ .x$years)),
         mod_p_2013_2015 = map(mod_2013_2015, ~p.adjust(.x$p.value, "fdr")),
         
         cover_2013_2017 = map2(`2013`, `2017`, ~bind_rows(.x,.y) %>% select(cover)),
         year_2013_2017 = map2(`2013`, `2017`, ~data.frame("years" = c(rep("2013", length(.x$cover)), rep("2017", length(.y$cover))))),
         df_2013_2017 = map2(cover_2013_2017, year_2013_2017, ~bind_cols(.x, .y)),
         mod_2013_2017 = map(df_2013_2017, ~kruskal.test(.x$cover ~ .x$years)),
         mod_p_2013_2017 = map(mod_2013_2017, ~p.adjust(.x$p.value, "fdr"))) %>%
  select(-c(cover_2013_2015, cover_2013_2017, 
            year_2013_2015, year_2013_2017,
            df_2013_2015, df_2013_2017)) %>%
  
  mutate(cover_2015_2017 = map2(`2015`, `2017`, ~bind_rows(.x,.y) %>% select(cover)),
         year_2015_2017 = map2(`2015`, `2017`, ~data.frame("years" = c(rep("2015", length(.x$cover)), rep("2017", length(.y$cover))))),
         df_2015_2017 = map2(cover_2015_2017, year_2015_2017, ~bind_cols(.x, .y)),
         mod_2015_2017 = map(df_2015_2017, ~kruskal.test(.x$cover ~ .x$years)),
         mod_p_2015_2017 = map(mod_2015_2017, ~p.adjust(.x$p.value, "fdr"))) %>%
  select(-c(cover_2015_2017, 
            year_2015_2017,
            df_2015_2017)) %>%
  filter(!if_all(starts_with("mod_p_"), is.na)) %>%
  filter(if_any(starts_with("mod_p"), ~. < 0.05))

#save(sahi_kruskal, file = "./data/derived/kruskal/sahi_kruskal.rda")
