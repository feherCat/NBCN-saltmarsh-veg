appd2_fun <- function(...){
  
   if(params$states == "Maryland/Virginia") {
    state = c("MD", "VA")
    rare_source = c("https://dnr.maryland.gov/wildlife/documents/\nrte_plant_list.pdf",
                    "https://www.dcr.virginia.gov/natural-\nheritage/document/plantlist-current.pdf")
    invasive_source = c("https://mda.maryland.gov/plants-pests/\nDocuments/Invasive-Plant-List-March-2020.pdf",
                        "https://www.dcr.virginia.gov/natural-heritage/\ninvsppdflist")
    } else if(params$states == "New Jersey/New York") {
    state = c("NJ/NY")
    rare_source = c("https://www.nj.gov/dep/parksandforests/\nnatural/docs/njplantlist.pdf",
                    "https://www.nynhp.org/documents/5/rare-\nplant-status-lists-2023.pdf")
    invasive_source = c("https://dep.nj.gov/invasive-species/plants/",
                        "https://govt.westlaw.com/nycrr/Browse/Home/\nNewYork/NewYorkCodesRulesandRegulations?\nguid=Ie8d3e7b0339611e4baa20000845b8d3e&\noriginationContext=documenttoc&transitionType=\nDefault&contextData=(sc.Default)")
  } else if(params$states == "Massachusetts") {
    state_id = "MA"
    rare_source = "https://www.mass.gov/info-details/list-of-\nendangered-threatened-and-special-concern-\nspecies"
    invasive_source = "https://massnrc.org/mipag/species\nreviewed_category.htm"
  } else if(params$states == "New York") {
    state_id = "NY"
    rare_source = "https://www.nynhp.org/documents/5/rare-\nplant-status-lists-2023.pdf"
    invasive_source = "https://dep.nj.gov/invasive-species/plants/"
  } else if(params$states == "Virginia") {
    state_id = "VA"
    rare_source = "https://www.dcr.virginia.gov/natural-\nheritage/document/plantlist-current.pdf"
    invasive_source = "https://www.dcr.virginia.gov/natural-heritage/\ninvsppdflist"
  }
  
  data.frame(state, rare_source, invasive_source) %>%
    flextable(data = .) %>%
    set_header_labels(., state = "State", rare_source = "Rare species list source", invasive_source = "Invasive species list source") %>%
    align(., align = "center", part = "all") %>%
    align(., j = c(2,3), part = "body", align = "left") %>%
    padding(., part = "body", padding.top = 3, padding.bottom = 3, padding.left = 3, padding.right = 3) %>%
    padding(., i = 1, j = 1, part = "header", padding.left = 5, padding.right = 5) %>%
    padding(., j = 1, part = "body", padding.left = 5, padding.right = 5) %>%
    autofit() %>%
    set_table_properties(layout = "autofit") %>%
    add_header_lines(., values = c("Appendix 2. Data sources for rare and invasive species lists for each state.")) %>%
    align(., i = 1, align = "left", part = "header") %>%
    padding(., i = 1, part = "header", padding.left = 0)
}