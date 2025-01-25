#biblioteki
library(shiny)
library(WDI)
library(dplyr)
library(ggplot2)
library(plotly)
library(tidyr)
library(leaflet)
library(scales)
library(vcd)
library(plotrix)
library(maps)
library(countrycode)
library(lattice)
library(cartogram)
library(sf)


#Zdefiniowanie wskaźników i lat - pobieranie danych z World Bank
wskazniki <- c(
  "SP.DYN.LE00.IN",   #Oczekiwana długość życia
  "SH.XPD.CHEX.GD.ZS", #Wydatki na zdrowie (% PKB)
  "SH.MED.BEDS.ZS",    #Liczba łóżek szpitalnych na 1000 osób
  "SH.STA.WASH.P5",    #Dostęp do ulepszonych źródeł wody
  "SH.IMM.MEAS",       #Szczepienia przeciw odrze
  "SP.POP.TOTL",       #Liczba ludności
  "SH.STA.BRTC.ZS",    #Porody z udziałem wykwalifikowanego personelu (%)
  "SH.XPD.CHEX.PC.CD", #Wydatki na zdrowie per capita (USD)
  "SH.MMR.RISK.ZS",    #Wskaźnik śmiertelności matek
  "SP.POP.GROW",       #Wzrost populacji (% rocznie)
  "SH.DYN.MORT",       #Wskaźnik śmiertelności dzieci do lat 5
  "SP.DYN.TFRT.IN",    #Wskaźnik dzietności (urodzenia na kobietę)
  "NY.GDP.PCAP.CD",    #PKB per capita
  "NY.GDP.MKTP.CD",    #PKB całkowity
  "SE.ADT.LITR.ZS"     #Wskaźnik alfabetyzacji dorosłych
)

dane_zdrowotne <- WDI(
  indicator = wskazniki,
  country = "all",
  start = 2002,
  end = 2022,
  extra = TRUE #Pobranie dodatkowych metadanych, w tym regionów i współrzędnych
)


#Sprawdzenie kompletności danych
#Zamiana danych na format długi dla łatwej analizy braków (czyli inaczej unpivot)
dane_dlugie <- dane_zdrowotne %>%
  pivot_longer(
    cols = starts_with("SP") | starts_with("SH") | starts_with("NY") | starts_with("SE"),
    names_to = "wskaznik",
    values_to = "wartosc"
  )

#Obliczenie brakujących danych dla każdego wskaźnika i roku
braki_danych <- dane_dlugie %>%
  group_by(wskaznik, year) %>%
  summarise(
    liczba_obserwacji = n(),
    liczba_brakow = sum(is.na(wartosc)),
    procent_brakow = round(100 * liczba_brakow / liczba_obserwacji, 2),
    .groups = "drop"
  )

View(braki_danych)
#niektóre wskaźniki mają braki danych na poziomie 100% 

#zidentyfikowanie wskaźników z 100% braków w jakimkolwiek roku
wskaźniki_do_usunięcia <- braki_danych %>%
  filter(procent_brakow == 100) %>%
  distinct(wskaznik) %>%  
  pull(wskaznik)

#usunięcie tych wskaźników z danych głównych
dane_zdrowotne <- dane_zdrowotne %>%
  select(-all_of(wskaźniki_do_usunięcia))

#sprawdzamy ponownie braki danych
#Sprawdzenie kompletności danych
#Zamiana danych na format długi dla łatwej analizy braków (czyli inaczej unpivot)
dane_dlugie2 <- dane_zdrowotne %>%
  pivot_longer(
    cols = starts_with("SP") | starts_with("SH") | starts_with("NY") | starts_with("SE"),
    names_to = "wskaznik",
    values_to = "wartosc"
  )


#Obliczenie brakujących danych dla każdego wskaźnika i roku
braki_danych2 <- dane_dlugie2 %>%
  group_by(wskaznik, year) %>%
  summarise(
    liczba_obserwacji = n(),
    liczba_brakow = sum(is.na(wartosc)),
    procent_brakow = round(100 * liczba_brakow / liczba_obserwacji, 2),
    .groups = "drop"
  )

View(braki_danych)
#nadal są wskaźniki o dużym odsetku braków danych - usuwam je ręcznie
wskazniki_do_usuniecia2 <- c("SH.STA.BRTC.ZS", "SH.XPD.CHEX.GD.ZS", "SH.XPD.CHEX.PC.CD", "SE.ADT.LITR.ZS")

dane_zdrowotne <- dane_zdrowotne %>%
  select(-all_of(wskazniki_do_usuniecia2))

#sprawdzamy ponownie braki danych
dane_dlugie3 <- dane_zdrowotne %>%
  pivot_longer(
    cols = starts_with("SP") | starts_with("SH") | starts_with("NY") | starts_with("SE"),
    names_to = "wskaznik",
    values_to = "wartosc"
  )

#Obliczenie brakujących danych dla każdego wskaźnika i roku
braki_danych3 <- dane_dlugie3 %>%
  group_by(wskaznik, year) %>%
  summarise(
    liczba_obserwacji = n(),
    liczba_brakow = sum(is.na(wartosc)),
    procent_brakow = round(100 * liczba_brakow / liczba_obserwacji, 2),
    .groups = "drop"
  )

#na ten moment największy odsetek brak danych to 10.15% - nie usuwam na razie dalej, aby zachować niektóre wskaźniki, ale będziemy kontrolować braki danych
#chcę zobaczyć, gdzie dokładnie występują braki danych - w jakich zmiennych, krajach, latach
#identyfikacja braków danych na poziomie kraju, roku i wskaźnika
braki_danych_szczegoly <- dane_dlugie3 %>%
  filter(is.na(wartosc)) %>% #Filtrujemy tylko brakujące wartości
  group_by(country, year, wskaznik) %>% #Grupujemy po kraju, roku i wskaźniku
  summarise(
    liczba_brakow = n(), #Liczba braków danych dla danego kraju, roku i wskaźnika
    .groups = "drop"
  ) %>%
  arrange(desc(liczba_brakow)) #Sortujemy od największej liczby braków danych
View(braki_danych_szczegoly)

#liczba unikatowych krajów, w których występują braki danych
unikatowe_kraje <- braki_danych_szczegoly %>%
  distinct(country)
View(unikatowe_kraje)
#łączna liczba
unikatowe_kraje2 <- braki_danych_szczegoly %>%
  summarise(unikatowe_kraje2 = n_distinct(country))
print(paste("Liczba unikatowych krajów z brakami danych:", unikatowe_kraje2$unikatowe_kraje2))

#na ogół to są bardzo małe kraje bądź wyspy (jest ich 38) - pozbędę się tych krajów z bazy danych, nie wpłynie to znacząco na ostateczną analizę
#usunięcie wierszy, w których wskaźniki mają braki danych w wybranych wskaźnikach
dane_zdrowotne <- dane_zdrowotne %>%
  filter(!if_any(starts_with("SP") | starts_with("SH") | starts_with("NY") | starts_with("SE"), is.na))

#filtrowanie danych, aby usunąć "Aggregates" i kategorie dochodu, gdzie nie jest określona
dane_zdrowotne <- dane_zdrowotne %>%
  filter(!region %in% c("Aggregates")) %>% #Usunięcie agregatów
  filter(income != "Not classified") #Usunięcie wierszy z "Not classified" w kolumnie income

#sprawdzamy ponownie braki danych
dane_dlugie4 <- dane_zdrowotne %>%
  pivot_longer(
    cols = starts_with("SP") | starts_with("SH") | starts_with("NY") | starts_with("SE"),
    names_to = "wskaznik",
    values_to = "wartosc"
  )


#Obliczenie brakujących danych dla każdego wskaźnika i roku
braki_danych4 <- dane_dlugie4 %>%
  group_by(wskaznik, year) %>%
  summarise(
    liczba_obserwacji = n(),
    liczba_brakow = sum(is.na(wartosc)),
    procent_brakow = round(100 * liczba_brakow / liczba_obserwacji, 2),
    .groups = "drop"
  )
#teraz już nie ma więcej braków danych

#Konwersja latitude i longitude na typ numeryczny
dane_zdrowotne <- dane_zdrowotne %>%
  mutate(
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude)
  )

#Konwersja kolumny income na zmienną kategoryczną do wizualizacji i wykresów panelowych
dane_zdrowotne <- dane_zdrowotne %>%
  mutate(Dochód = factor(income, levels = unique(income), ordered = TRUE))
table(dane_zdrowotne$income)

#Zmiana nazw zmiennych na bardziej czytelne
dane_zdrowotne <- dane_zdrowotne %>%
  rename(
    Kraj = country,
    Rok = year,
    Region = region,
    "PKB per capita [$]" = NY.GDP.PCAP.CD,
    PKB = NY.GDP.MKTP.CD,
    "Oczekiwana długość życia" = SP.DYN.LE00.IN,
    "Szczepienia przeciw odrze [% dzieci w 12-23 mies. życia]" = SH.IMM.MEAS,
    "Liczba ludności" = SP.POP.TOTL,
    "Wzrost populacji (%)" = SP.POP.GROW,
    "Śmiertelność dzieci do lat 5" = SH.DYN.MORT,
    "Wskaźnik dzietności" = SP.DYN.TFRT.IN
  )

#Przekształcenie kolumny "Dochód" na 4 kategorie z polskimi nazwami
dane_zdrowotne <- dane_zdrowotne %>%
  mutate(
    Dochód = case_when(
      Dochód == "Low income" ~ "Niskie dochody",
      Dochód == "Lower middle income" ~ "Niższe średnie dochody",
      Dochód == "Upper middle income" ~ "Wyższe średnie dochody",
      Dochód == "High income" ~ "Wysokie dochody"
    )
  )
table(dane_zdrowotne$Dochód)

#wybieram tylko potrzebne zmienne
dane_zdrowotne <- dane_zdrowotne %>%
  select(Kraj, Rok, Region, Dochód, latitude, longitude, PKB_per_capita, PKB,
         `Oczekiwana długość życia`,
         `Szczepienia przeciw odrze`,
         `Liczba ludności`,
         `Wzrost populacji (%)`,
         `Śmiertelność dzieci do lat 5`,
         `Wskaźnik dzietności`)

print(head(dane_zdrowotne))

#ostateczne sprawdzenie występujących braków danych
#sprawdzenie braków danych w każdej kolumnie
colSums(is.na(dane_zdrowotne))
#całkowita liczba braków w całym zbiorze
sum(is.na(dane_zdrowotne))
#są brakujące wartości dla szerokości i długości geograficznej
#znalezienie krajów z brakującymi wartościami w kolumnach latitude i longitude
kraje_z_brakami <- dane_zdrowotne %>%
  filter(is.na(latitude) | is.na(longitude)) %>%
  distinct(Kraj)
#kraj West Bank and Gaza - usunę go, bo nie będzie widoczny na mapie
dane_zdrowotne <- dane_zdrowotne %>%
  filter(Kraj != "West Bank and Gaza")

#zmiana nazwy regionów świata na polskie
print(unique(dane_zdrowotne$Region))
dane_zdrowotne <- dane_zdrowotne %>%
  mutate(Region = recode(Region,
                         "East Asia & Pacific" = "Azja Wschodnia i Pacyfik",
                         "Europe & Central Asia" = "Europa i Azja Centralna",
                         "Latin America & Caribbean" = "Ameryka Łacińska i Karaiby",
                         "Middle East & North Africa" = "Bliski Wschód i Afryka Północna",
                         "North America" = "Ameryka Północna",
                         "South Asia" = "Azja Południowa",
                         "Sub-Saharan Africa" = "Afryka Subsaharyjska"))

#ostateczna baza danych
print(head(dane_zdrowotne))

print(colnames(dane_zdrowotne))
head(dane_zdrowotne)


