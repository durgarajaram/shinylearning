library(dplyr)
library(XML)
library(rvest)
library(ggplot2)

tx = read_xml("~/thresholds.xml")
xn = xml_nodes(tx, "threshold")
xml_find_all(xn, "//description") %>% xml_text()
for (n in 1:length(xn)) {
  nchildren = length(xml_children(xn[n]));
  print(nchildren)
}

for (cn in xn) { print(cn); print(paste0("NC = ",length(xml_children(cn))))}
xml_attrs(cn, "sublevels")

myXML <- xmlParse(tx)
myData = xmlToDataFrame(myXML, stringsAsFactors = FALSE) %>% mutate_all(~type.convert(., as.is = T))
myData['levels']
xml_find_all(xn, "//levels")

inf <- read.csv("~/Desktop/datasc/cc.csv")
inf$Post.Date <- as.Date(inf$Post.Date, format="%m/%d/%Y")
inf$Transaction.Date <- as.Date(inf$Transaction.Date, format="%m/%d/%Y")
inf2 = inf %>% mutate(month = format(Transaction.Date, "%m"), year = format(Transaction.Date, "%Y")) %>% group_by(month, year, Category) %>% summarise(total = sum(Amount), Post.Date)
ggplot(inf2, aes(x=paste(month,year), y=total, color=Category)) + geom_point()