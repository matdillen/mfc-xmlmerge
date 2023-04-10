library(xml2)
library(magrittr)

setwd("D:/Mathias/MTG/proxies/mfc-xmlmerge")

datadir = "data"

orders = list.files(datadir)
orders_data = list()

final = read_xml(paste0(datadir,"/",orders[1]))

total_cards = final %>%
  xml_children() %>%
  xml_child(.,1) %>%
  xml_contents() %>%
  xml_double()

for (i in orders[-1]) {
  orders_data[[as.name(i)]] = read_xml(paste0(datadir,"/",i))
  
  card_num = orders_data[[as.name(i)]] %>%
    xml_child(.,2) %>%
    xml_length()
  for (j in 1:card_num) {
    slots = orders_data[[as.name(i)]] %>%
      xml_child(.,2) %>%
      xml_child(.,j) %>%
      xml_child(.,2) %>%
      xml_text() %>%
      strsplit(.,split=",") %>%
      unlist() %>%
      as.numeric() %>%
      add(total_cards) %>%
      paste(.,collapse = ",")
    orders_data[[as.name(i)]] %>%
      xml_child(.,2) %>%
      xml_child(.,j) %>%
      xml_child(.,2) %>%
      xml_set_text(.,slots)
    node = orders_data[[as.name(i)]] %>%
      xml_child(.,2) %>%
      xml_child(.,j) 
    final %>%
      xml_child(.,2) %>%
      xml_add_child(node)
  }
  if (xml_length(orders_data[[as.name(i)]]) > 3) {
    card_num_b = orders_data[[as.name(i)]] %>%
      xml_child(.,3) %>%
      xml_length()
    for (j in 1:card_num_b) {
      backslots = orders_data[[as.name(i)]] %>%
        xml_child(.,3) %>%
        xml_child(.,j) %>%
        xml_child(.,2) %>%
        xml_text() %>%
        strsplit(.,split=",") %>%
        unlist() %>%
        as.numeric() %>%
        add(total_cards) %>%
        paste(.,collapse = ",")
      orders_data[[as.name(i)]] %>%
        xml_child(.,3) %>%
        xml_child(.,j) %>%
        xml_child(.,2) %>%
        xml_set_text(.,backslots)
      if (xml_length(final) > 3) {
        node = orders_data[[as.name(i)]] %>%
          xml_child(.,3) %>%
          xml_child(.,j) 
        final %>%
          xml_child(.,3) %>%
          xml_add_child(node)
      }
    }
    if (xml_length(final) < 4) {
      node = orders_data[[i]] %>%
        xml_child(.,3)
      final %>%
        xml_add_child(node,.where = 2)
    }
  }
  total_cards = orders_data[[as.name(i)]] %>%
    xml_children() %>%
    xml_child(.,1) %>%
    xml_contents() %>%
    xml_double() %>%
    sum(total_cards)
}

final %>%
  xml_children() %>%
  xml_child(.,1) %>%
  xml_contents() %>%
  xml_set_text(toString(total_cards))

filename = paste0("order-for-mpc_",
                  format(Sys.time(), "%Y-%m-%d %I.%M%p"),
                  ".xml")

write_xml(final,paste0("output/",filename))
