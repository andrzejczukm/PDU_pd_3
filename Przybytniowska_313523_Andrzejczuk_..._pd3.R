library(dplyr)
library(magrittr)
library(stringr)
library(ggplot2)
library(wesanderson)
library(tidyr)
library(plotly)
library(shiny)

if ( options()$stringsAsFactors )
  options(stringsAsFactors=FALSE)

# Ramki danych do pytania 1:
Posts <- read.csv("gaming.stackexchange.com/Posts.xml.csv")
PostHistory <- read.csv("gaming.stackexchange.com/PostHistory.xml.csv")

#Ramki danych do pytania 2:
Users<-read.csv("Mus/Users.xml.csv")
Posts<-read.csv("Mus/Posts.xml.csv")
  
# Ramki danych do pytania 3:
PostsBuddhism <- read.csv("buddhism.stackexchange.com/Posts.xml.csv")
PostsIslam <- read.csv("islam.stackexchange.com/Posts.xml.csv")
PostsJudaism <- read.csv("judaism.stackexchange.com/Posts.xml.csv")
PostsHinduism <- read.csv("hinduism.stackexchange.com/Posts.xml.csv")
PostsChristianity <- read.csv("christianity.stackexchange.com/Posts.xml.csv")

#Pytanie 1:
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

wykres <- ggplot(data, aes(fill=Urzadzenie, y=LiczbaWyszukan, x=Rok)) +
  geom_bar(position="stack", stat="identity") + scale_fill_manual("Urzadzenie", values = c("PlayStation" = "lightblue", "Xbox" = "darkblue"))

#Pytanie 2:
# Wykres przedstawiający ilość postów o konkretnych instrumentach (pianino, skrzypce, gitara, bębny) w Niemczech i Francji

tab1 <- Posts %>% filter(Score > 0)
tab2 <- Users  %>% 
        filter(str_detect(Location, "Germany")) %>% 
        select("Id")
tab3 <- Users  %>% 
  filter(str_detect(Location, "France")) %>% 
  select("Id")

piano <- (tab1 %>%
  filter(str_detect(Tags, "piano")) %>%
  inner_join(tab2, by = c("OwnerUserId" = "Id")) %>%
  tally())[1, 1]
 

violin <- (tab1 %>%
  filter(str_detect(Tags, "violin")) %>%
  inner_join(tab2, by = c("OwnerUserId" = "Id")) %>%
  tally())[1, 1]

guitar <- (tab1 %>%
  filter(str_detect(Tags, "guitar")) %>%
  inner_join(tab2, by = c("OwnerUserId" = "Id")) %>%
  tally())[1, 1]

drums <- (tab1 %>%
  filter(str_detect(Tags, "drums")) %>%
  inner_join(tab2, by = c("OwnerUserId" = "Id")) %>%
  tally())[1, 1]

piano2 <- (tab1 %>%
  filter(str_detect(Tags, "piano")) %>%
  inner_join(tab3, by = c("OwnerUserId" = "Id")) %>%
  tally())[1, 1]


violin2 <- (tab1 %>%
  filter(str_detect(Tags, "violin")) %>%
  inner_join(tab3, by = c("OwnerUserId" = "Id")) %>%
  tally())[1, 1]

guitar2 <- (tab1 %>%
  filter(str_detect(Tags, "guitar")) %>%
  inner_join(tab3, by = c("OwnerUserId" = "Id")) %>%
  tally())[1, 1]

drums2 <- (tab1 %>%
  filter(str_detect(Tags, "drums")) %>%
  inner_join(tab3, by = c("OwnerUserId" = "Id")) %>%
  tally())[1, 1]

liczba_wyszukan <- c(piano, violin, guitar, drums, piano2, violin2, guitar2, drums2)

instrumenty <- c("pianino", "skrzypce", "gitara", "bębny", "pianino", "skrzypce", "gitara", "bębny")

Kraj <- c("Niemcy", "Niemcy", "Niemcy", "Niemcy", "Francja", "Francja", "Francja", "Francja")


tabela <- data_frame(liczba_wyszukan, instrumenty, Kraj)

wykres1 <- ggplot(tabela, aes(x=instrumenty, y=liczba_wyszukan, fill=Kraj)) +
  geom_col(position = "dodge") +labs(y= "Liczba wyszukań") + 
  theme(legend.background = element_rect(fill="white",
   size=0.5, linetype="solid", 
   colour ="black")) +
  ggtitle("Liczba postów na temat instrumentów w Niemczech i Francji")
ggplotly(wykres1)

  


#Pytanie 3:
# Jak zmieniała sie popularnosc/suma liczby wyswietlen postow dotyczacych wybranych religii 
# na przestrzeni lat 2014-2021

vecB <- rep("Buddyzm", 8)
vecI <- rep("Islam", 10)
vecJ <- rep("Judaizm", 13)
vecH <- rep("Hinduizm", 8)
vecC <- rep("Chrzescjaństwo", 11)

viewsBuddhism <- tidyr::separate(PostsBuddhism, CreationDate, c('Rok',"theRest"), sep = "-",remove = FALSE)%>%
  filter(PostTypeId == 1)%>%
  group_by(Rok)%>%  
  summarize(Wyswietlenia = sum(ViewCount, na.rm = TRUE))%>%
  mutate(Religia = vecB)

viewsIslam <- tidyr::separate(PostsIslam, CreationDate, c('Rok', 'theRest'), sep = "-",remove = FALSE)%>%
  filter(PostTypeId == 1)%>%
  group_by(Rok)%>%  
  summarize(Wyswietlenia = sum(ViewCount, na.rm = TRUE))%>%
  mutate(Religia = vecI)

viewsJudaism <- tidyr::separate(PostsJudaism, CreationDate, c('Rok', 'theRest'), sep = "-",remove = FALSE)%>%
  filter(PostTypeId ==1)%>%
  group_by(Rok)%>%  
  summarize(Wyswietlenia = sum(ViewCount, na.rm = TRUE))%>%
  mutate(Religia = vecJ)

viewsHinduism <- tidyr::separate(PostsHinduism, CreationDate, c('Rok','theRest'), sep = "-",remove = FALSE)%>%
  filter(PostTypeId ==1)%>%
  group_by(Rok)%>%  
  summarize(Wyswietlenia = sum(ViewCount, na.rm = TRUE))%>%
  mutate(Religia = vecH)

viewsChristianity <- tidyr::separate(PostsChristianity, CreationDate, c('Rok','theRest'), sep = "-",remove = FALSE)%>%
  filter(PostTypeId ==1)%>%
  group_by(Rok)%>%  
  summarize(Wyswietlenia = sum(ViewCount, na.rm = TRUE))%>%
  mutate(Religia = vecC)

Wyswietlenia <- full_join(viewsBuddhism, viewsIslam) %>% 
  full_join(viewsChristianity) %>% 
  full_join(viewsHinduism) %>% 
  full_join(viewsJudaism)

wykres2 <- ggplot(Wyswietlenia, aes(x=Rok, y=Wyswietlenia, color=Religia, group = Religia)) +
  geom_line(size = 1) +
  ggtitle("Liczba wyświetleń postów na temat poszczególnych religii") +
  scale_y_continuous(labels = scales::comma) 
wykres2 <- ggplotly(wykres2)




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
                       choices = list("Wybierz numer pytania",
                                      "Pytanie 1",
                                      "Pytanie 2",
                                      "Pytanie 3"),
                       selected = "Wybierz numer pytania")
    )
  ),
  mainPanel(
    width = 15,
    plotOutput("plot")
  )
)

server <- function(input, output) {
  dataInput <- reactive({ 
    if ("Pytanie 1" %in% input$selectBox)return(wykres)
    if ("Pytanie 2" %in% input$selectBox)return(wykres1)
    if ("Pytanie 3" %in% input$selectBox)return(wykres2)
  })
  
  output$plot <- renderPlot(print(dataInput()))
}

shinyApp(ui = ui, server = server)

