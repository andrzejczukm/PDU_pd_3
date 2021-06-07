library(dplyr)
library(magrittr)
library(stringr)
library(ggplot2)
library(wesanderson)
library(tidyr)


if ( options()$stringsAsFactors )
  options(stringsAsFactors=FALSE)

# Ramki danych do pytania 2:
Posts <- read.csv("gaming.stackexchange.com/Posts.xml.csv")
PostHistory <- read.csv("gaming.stackexchange.com/PostHistory.xml.csv")
  
# Ramki danych do pytania 3:
PostsBuddhism <- read.csv("buddhism.stackexchange.com/Posts.xml.csv")
PostsIslam <- read.csv("islam.stackexchange.com/Posts.xml.csv")
PostsJudaism <- read.csv("judaism.stackexchange.com/Posts.xml.csv")
PostsHinduism <- read.csv("hinduism.stackexchange.com/Posts.xml.csv")
PostsChristianity <- read.csv("christianity.stackexchange.com/Posts.xml.csv")

#Pytanie 2:
#Wykres przedstawiający jaka liczba postów została napisana o xbox a jaka o ps(1,2,3,4 lub 5) 
#w poszczególnych latach zaczynając na 2017 konczac na 2020 

not_deleted <- PostHistory %>%
  filter(PostHistoryTypeId != 12)%>%
  inner_join(Posts, by = c("PostId" = "Id"))%>%
  distinct(PostId, .keep_all = TRUE)

xbox_questions <- filter(not_deleted, str_detect(Tags, 'xbox'))
  
ps_questions <- filter(not_deleted, str_detect(Tags, 'ps1') | str_detect(Tags, "ps2")| str_detect(Tags, "ps3") | str_detect(Tags, "ps4") | str_detect(Tags, "ps3"))

xbox2017 <- as.integer(
filter(xbox_questions, str_detect(CreationDate.y, "2017"))%>%
  count())

xbox2018 <- as.integer(
  filter(xbox_questions, str_detect(CreationDate.y, "2018"))%>%
    count())

xbox2019 <- as.integer(
  filter(xbox_questions, str_detect(CreationDate.y, "2019"))%>%
    count())

xbox2020 <- as.integer(
  filter(xbox_questions, str_detect(CreationDate.y, "2020"))%>%
    count())

ps2017 <- as.integer(
  filter(ps_questions, str_detect(CreationDate.y, "2017"))%>%
    count())

ps2018 <- as.integer(
  filter(ps_questions, str_detect(CreationDate.y, "2018"))%>%
    count())

ps2019 <- as.integer(
  filter(ps_questions, str_detect(CreationDate.y, "2019"))%>%
    count())

ps2020 <- as.integer(
  filter(ps_questions, str_detect(CreationDate.y, "2020"))%>%
    count())

LiczbaWyszukan <- c(xbox2017, ps2017, xbox2018, ps2018, xbox2019, ps2019, xbox2020, ps2020)
Urzadzenie <- c("Xbox", "PlayStation", "Xbox", "PlayStation", "Xbox", "PlayStation", "Xbox", "PlayStation")
Rok <- c("2017",'2017', "2018","2018", "2019", "2019", "2020", "2020")

data <- data.frame(Rok, Urzadzenie, LiczbaWyszukan)

ggplot(data, aes(fill=Urzadzenie, y=LiczbaWyszukan, x=Rok)) +
  geom_bar(position="stack", stat="identity") + scale_fill_manual("Urzadzenie", values = c("PlayStation" = "lightblue", "Xbox" = "darkblue"))

#Pytanie 3:
# Jak zmieniała sie popularnosc/suma liczby wyswietlen postow dotyczacych wybranych religii 
# na przestrzeni lat 2014-2021

viewsBuddhism <- tidyr::separate(PostsBuddhism, CreationDate, c('Rok', 'Miesiac', "theRest"), sep = "-",remove = FALSE)%>%
  filter(PostTypeId == 1)%>%
  group_by(Rok, Miesiac)%>%  
  summarize(WyswietleniaBuddyzm = sum(ViewCount, na.rm = TRUE), .groups = 'drop')%>%
  unite("Miesiac-Rok",Miesiac, Rok, sep = "-")

viewsIslam <- tidyr::separate(PostsIslam, CreationDate, c('Rok', 'Miesiac', 'theRest'), sep = "-",remove = FALSE)%>%
  filter(PostTypeId == 1)%>%
  group_by(Rok, Miesiac)%>%  
  summarize(WyswietleniaIslam = sum(ViewCount, na.rm = TRUE), .groups = 'drop')%>%
  unite("Miesiac-Rok",Miesiac, Rok, sep = "-")

viewsJudaism <- tidyr::separate(PostsJudaism, CreationDate, c('Rok', 'Miesiac', 'theRest'), sep = "-",remove = FALSE)%>%
  filter(PostTypeId ==1)%>%
  group_by(Rok, Miesiac)%>%  
  summarize(WyswietleniaJudaizm = sum(ViewCount, na.rm = TRUE), .groups = 'drop')%>%
  unite("Miesiac-Rok",Miesiac, Rok, sep = "-")

viewsHinduism <- tidyr::separate(PostsHinduism, CreationDate, c('Rok', 'Miesiac', 'theRest'), sep = "-",remove = FALSE)%>%
  filter(PostTypeId ==1)%>%
  group_by(Rok, Miesiac)%>%  
  summarize(WyswietleniaHinduizm = sum(ViewCount, na.rm = TRUE), .groups = 'drop')%>%
  unite("Miesiac-Rok",Miesiac, Rok, sep = "-")

viewsChristianity <- tidyr::separate(PostsChristianity, CreationDate, c('Rok', 'Miesiac', 'theRest'), sep = "-",remove = FALSE)%>%
  filter(PostTypeId ==1)%>%
  group_by(Rok, Miesiac)%>%  
  summarize(WyswietleniaChrzescjanstwo = sum(ViewCount, na.rm = TRUE), .groups = 'drop')%>%
  unite("Miesiac-Rok",Miesiac, Rok, sep = "-")


# Aplikacja webowa przedstawiająca wyniki:
ui <- fluidPage(
  
  titlePanel("Praca domowa nr 3"),

  sidebarLayout(
    sidebarPanel(
          width = 7,
          height = 5,
          h4("Wykonały: Maja Andrzejczuk i Julia Przybytniowska"),
          h6("20.05.2021 - 5.06.2021")
        ),
    column(3,
           selectInput("selectBox",
                       h3('Pytanie: '),
                       choices = list("Pytanie 1" = 1,
                                      "Pytanie 2" = 2,
                                      "Pytanie 3" = 3),
                       selected = 1)
           )
  ),
  mainPanel(
    width = 15,
    plotOutput("plot")
  )
)


server <- function(input, output) {}

shinyApp(ui = ui, server = server)
