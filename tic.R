get_stage("before_deploy") %>%
  add_step(step_setup_ssh()) %>%
  add_step(step_setup_push_deploy())

get_stage("deploy") %>%
  add_step(step_install_github("trendecon/trendecon")) %>%
  add_step(step_install_cran("prophet")) %>%
  add_step(step_install_cran("tibble")) %>%
  add_step(step_install_cran("httr")) %>%
  add_code_step(withr::with_package("httr", set_config(config(http_version = 0)))) %>%
  add_code_step(library(prophet)) %>%
  add_code_step(withr::with_package("trendecon", proc_trendecon_at())) %>%
  add_code_step(withr::with_package("trendecon", proc_index(c("mango","zara","H&M","blue tomato","schuhe kaufen", "deichmann"), "AT", "clothing"))) %>%
  add_code_step(geo <- "AT") %>%
  add_code_step(food_delivery <- c("take away", "takeaway", "pizza bestellen")) %>%
  add_code_step(withr::with_package("trendecon", proc_index(food_delivery, geo, "food_delivery"))) %>%
  add_code_step(home_office <- c("headset", "monitor","maus","hdmi")) %>%
  add_code_step(withr::with_package("trendecon", proc_index(home_office, geo, "home_office"))) %>%
  add_code_step(gardening <- c("Heim+Hobby","Bau+Hobby","Bauhaus","hornbach","obi")) %>%
  add_code_step(withr::with_package("trendecon", proc_index(gardening, geo, "gardening"))) %>%
  add_code_step(cultural <- c("kino","theater","cinema","cineplexx","oper","konzert","oeticket")) %>%
  add_code_step(withr::with_package("trendecon", proc_index(cultural, geo, "cultural"))) %>%
  add_code_step(travel <- c("städtetrip","flug buchen","günstige flüge")) %>%
  add_code_step(withr::with_package("trendecon", proc_index(travel, geo, "travel"))) %>%
  add_code_step(mobility <- c("Fahrplan","taxi","sixt","google maps")) %>%
  add_code_step(withr::with_package("trendecon", proc_index(mobility, geo, "mobility"))) %>%
  add_code_step(luxury <- c("juwelier","swarovski","uhr","uhren","christ","feichtinger")) %>%
  add_code_step(withr::with_package("trendecon", proc_index(luxury, geo, "luxury"))) %>%
  add_step(step_do_push_deploy())