## server_news.R: news stories

## GNU General Public License version 2 or any later version
## (c) 2016 Jeremy Darot
## jeremy@greenerleith.org

# Get Broughton Spurtle data from their RSS feed

try(x <- read_xml("http://broughtonspurtle.org.uk/news/Breaking%20news/rss.xml"))
if (exists("x")) {
  try({  
    spurtle <- NULL
    x %>% xml_find_all("//item/title") %>% xml_text() -> spurtle$title
    x %>% xml_find_all("//item/pubDate") %>% xml_text() -> spurtle$date_and_time
    x %>% xml_find_all("//item/link") %>% xml_text() -> spurtle$link
    x %>% xml_find_all("//georss:point", ns = xml_ns(x)) %>% xml_text() -> spurtle$location
    spurtle <- as.data.frame(spurtle)
    spurtle$title %<>% str_to_title()
    spurtle$link %<>% cleanURL(spurtle$title) %>% str_sub(5)
    spurtle$lat <- as.numeric(str_sub(spurtle$location, 1, 9))
    spurtle$lon <- as.numeric(str_sub(spurtle$location, 11))
    spurtle$date <- as.Date(str_sub(spurtle$date_and_time, 6, 16), format = "%d %B %Y")
    rm(x)
  })}

output$panel_news <- renderUI({
  if(!(input$show_panel_news))
    return()
  if (exists("spurtle") && !is.null(spurtle)) {
    list(
      checkboxGroupInput("show_news", label = NULL, width = "100%", choices = list("The Spurtle" = "spurtle")) 
    )
  } else {"No data currently available"}
})  

observe({
  proxy <- leafletProxy("mymap")
  if(is.null(input$show_panel_news)) {
    proxy %>% clearGroup(c("spurtle"))
    return()
  }
  
  current_group <- "spurtle"
  if (current_group %in% input$show_news) {
    if (exists("spurtle") && !is.null(spurtle)) {
      proxy %>% addMarkers(data = spurtle, 
                           lng = ~lon,
                           lat = ~lat,
                           popup = ~paste(sep = "",
                                          "<b>",link,"</b>",
                                          "<br>", "<i>The Spurtle</i>, ", format.Date(date, format = "%d %b %Y")),
                           group = current_group,
                           icon = news_icon) }
  } else
    proxy %>% clearGroup(current_group)
})
