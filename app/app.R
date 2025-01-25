library(shiny)
library(ggplot2)
library(leaflet)
library(dplyr)
library(ggtext)

#definiuję UI

ui <- navbarPage(
    tags$strong("Światowe dane dotyczące zdrowia publicznego i ogólnych wskaźników powiązanych w latach 2000-2022"),
  
  #Zakładka: Trendy wskaźników
  tabPanel(
    "Trendy wskaźników",
    sidebarLayout(
      sidebarPanel(
        h4("Instrukcja:"),
        div(
          p("Wybierz region, kraj i wskaźnik, aby przeanalizować dane na zmiany wartości wybranej zmiennej w czasie."),
          p("1. Użyj filtrowania poniżej: 'Wybierz region(y)' i 'Wybierz kraj(e)', aby określić regiony i kraje."),
          p("2. Można wybrać jedynie regiony lub tylko kraje – nie ma konieczności uzupełnienia obydwu filtrów."),
          p("3. W ostatnim kroku, w polu 'Wybierz wskaźnik', zdecyduj, który wskaźnik chcesz przeanalizować."),
          p("Wyniki analizy zostaną przedstawione w formie wykresów:"),
          tags$ul(
            tags$li("Wykres liniowy służy do porównania zmian wskaźnika w czasie dla wybranych krajów. Uwaga: wybór zbyt wielu krajów lub całego regionu może sprawić, że wykres stanie się nieczytelny."),
            tags$li("Wykres pudełkowy przedstawia rozkład wskaźnika w regionach:"),
            tags$ul(
              tags$li("Jeżeli wybierzesz tylko jeden kraj z regionu, pudełko odzwierciedli dane wyłącznie dla tego kraju."),
              tags$li("Jeżeli nie wybierzesz żadnych krajów, pudełka pokażą dane dla wszystkich krajów z wybranych regionów.")
            )
          ),
          p("Uwaga! Aby zapewnić czytelność wykresu liniowego, zaleca się wybór niewielkiej liczby krajów."),
          p("W przypadku wyboru krajów nienależących do wybranych regionów nie będą one wyświetlone.")
        ),
        selectInput("region_filter", "Wybierz region(y):", 
                    choices = unique(dane_zdrowotne$Region), 
                    selected = NULL, multiple = TRUE),
        selectInput("country_filter", "Wybierz kraj(e):", 
                    choices = unique(dane_zdrowotne$Kraj), 
                    selected = NULL, multiple = TRUE),
        selectInput("variable_filter", "Wybierz wskaźnik:", 
                    choices = c("Oczekiwana długość życia", 
                                "Śmiertelność dzieci do lat 5", 
                                "Szczepienia przeciw odrze [% dzieci w 12-23 mies. życia]",
                                "PKB per capita [$]",
                                "PKB",
                                "Wzrost populacji (%)",
                                "Wskaźnik dzietności"),
                    selected = "Oczekiwana długość życia")
      ),
      mainPanel(
        plotOutput("filteredPlot"),
        plotOutput("boxPlot")
      )
    )
  ),
  tabPanel(
    "Zależności między wskaźnikami",
    sidebarLayout(
      sidebarPanel(
        h4("Instrukcja:"),
        div(
          p("W tej sekcji można analizować korelacje pomiędzy dwoma wybranymi wskaźnikami (X i Y) dla wybranych regionów lub krajów."),
          p("1. Wybierz zmienne X i Y z list rozwijanych, aby określić wskaźniki, których korelację chcesz zbadać."),
          p("2. Następnie skorzystaj z filtrów 'Wybierz region(y)' i 'Wybierz kraj(e)', aby zawęzić analizę do interesujących Cię obszarów:"),
          tags$ul(
            tags$li("Wybór regionu automatycznie ograniczy analizę do krajów należących do wybranego regionu."),
            tags$li("Wybór kraju jest możliwy zarówno bez, jak i z wyborem regionu. Jednak kraje spoza wybranego regionu nie zostaną uwzględnione.")
          ),
          p("3. Zaleca się wybór maksymalnie 8 krajów, aby wykres pozostał czytelny. Kolory kropek odpowiadają poszczególnym regionom zgodnie z legendą po prawej stronie wykresu."),
          p("Uwaga! Wybór dużej liczby krajów lub regionów może spowodować, że korelacja będzie trudna do zinterpretowania.")
        ),
        selectInput("x_var", "Wybierz zmienną X:", 
                    choices = c("PKB per capita [$]", "PKB", "Oczekiwana długość życia", 
                                "Wzrost populacji (%)", "Wskaźnik dzietności", "Śmiertelność dzieci do lat 5",
                                "Szczepienia przeciw odrze [% dzieci w 12-23 mies. życia]"), 
                    selected = "PKB per capita [$]"),
        selectInput("y_var", "Wybierz zmienną Y:", 
                    choices = c("PKB per capita [$]", "PKB", "Oczekiwana długość życia", 
                                "Wzrost populacji (%)", "Wskaźnik dzietności", "Śmiertelność dzieci do lat 5",
                                "Szczepienia przeciw odrze [% dzieci w 12-23 mies. życia]"), 
                    selected = "Oczekiwana długość życia"),
        selectInput("region_filter_corr", "Wybierz region(y):", 
                    choices = unique(dane_zdrowotne$Region), 
                    selected = NULL, multiple = TRUE),
        selectInput("country_filter_corr", "Wybierz kraj(e):", 
                    choices = unique(dane_zdrowotne$Kraj), 
                    selected = NULL, multiple = TRUE)
      ),
      mainPanel(
        plotOutput("correlationPlot")
      )
    )
  ),
  tabPanel(
    "Mapa",
    sidebarLayout(
      sidebarPanel(
        h4("Instrukcja:"),
        div(
          p("W tej sekcji można analizować dane w układzie mapy dla wybranych regionów."),
          p("1. Wybierz region(y) za pomocą listy rozwijanej 'Wybierz region(y)', aby ograniczyć widoczność mapy do wybranych obszarów."),
          p("2. Po wyborze regionu mapa automatycznie przybliży się do wybranego obszaru, ułatwiając analizę."),
          p("3. Kropki na mapie reprezentują kraje, a ich wielkość oraz kolor zawierają dodatkowe informacje:"),
          tags$ul(
            tags$li("Rozmiar kropek: odzwierciedla wartość wskaźnika 'Oczekiwana długość życia' (większa wartość = większy rozmiar)."),
            tags$li("Kolor kropek: zależy od poziomu dochodów danego kraju zgodnie z legedną umieszczoną na mapie.")
            )
          ),
          p("4. Po najechaniu na kropkę wyświetlana jest szczegółowa informacja o kraju, zawierająca:"),
          tags$ul(
            tags$li("Nazwę kraju"),
            tags$li("Oczekiwaną długość życia"),
            tags$li("PKB per capita [$]")
          ),
          p("Mapa jest interaktywna – można przesuwać widok, przybliżać lub oddalać w celu dokładniejszej analizy.")
        ),
        selectInput("region_filter_map", "Wybierz region(y):", 
                    choices = unique(dane_zdrowotne$Region), 
                    selected = NULL, multiple = TRUE)
      ),
      mainPanel(
        leafletOutput("interactiveMap")
      )
    ),
  #Zakładka: Wykres kołowy
  tabPanel(
    "Udziały kategorii dochodu i regionów",
    sidebarLayout(
      sidebarPanel(
        h4("Instrukcja:"),
        div(
          p("W tej sekcji możesz zobaczyć rozkład poziomów dochodów oraz liczbę krajów w regionach w postaci wykresu kołowego."),
          p("Wybór zmiennej: Skorzystaj z listy rozwijanej 'Wybierz zmienną', aby określić zmienną, którą chcesz zobaczyć na wykresie."),
          tags$ul(
            tags$li("Zmienna 'Dochód' określa liczbę krajów, które należą do danej kategorii dochodów: wysokie, wyższe średnie, niższe średnie i niskie dochody."),
            tags$li("Zmienna 'Region' pokazuje liczbę krajów należących do wybranego regionu."),
            tags$li("Każdy sektor wykresu kołowego przedstawia liczbę krajów w danej kategorii dochodów lub regionie, w zależności od wybranej zmiennej.")
          ),
        ),
        selectInput("pie_var", "Wybierz zmienną:", 
                    choices = c("Dochód", "Region"),
                    selected = "Dochód")
      ),
      mainPanel(
        plotOutput("pieChart")
      )
    )
  ),
  #Zakładka Histogram
  tabPanel(
    "Histogram",
    sidebarLayout(
      sidebarPanel(
        h4("Instrukcja:"),
        div(
          p("W tej sekcji możesz analizować rozkład wybranej zmiennej dla wybranych regionów w określonym roku w formie histogramu."),
          p("1. Wybór zmiennej: Skorzystaj z listy rozwijanej 'Wybierz zmienną', aby określić, którą zmienną chcesz zobaczyć na histogramie."),
          p("2. Wybór roku: Wybierz rok z dostępnych danych, aby ograniczyć analizę do wybranego okresu."),
          p("3. Wybór regionu: Możesz wybrać jeden lub więcej regionów za pomocą listy rozwijanej 'Wybierz region(y)'."),
          p("4. Histogram przedstawia liczbę krajów w wybranych regionach, które mieszczą się w określonych przedziałach wartości wybranej zmiennej."),
          tags$ul(
            tags$li("Oś X: przedstawia przedziały wartości wybranej zmiennej."),
            tags$li("Oś Y: pokazuje liczbę krajów, których wartość zmiennej mieści się w danym przedziale.")
          ),
          p("Jeśli nie wybierzesz regionu, histogram pokaże dane dla wszystkich regionów.")
        ),
        #Wybór zmiennej
        selectInput("hist_var", "Wybierz zmienną:", 
                    choices = c("Oczekiwana długość życia", 
                                "Śmiertelność dzieci do lat 5", 
                                "Szczepienia przeciw odrze",
                                "PKB per capita [$]",
                                "PKB",
                                "Wzrost populacji (%)",
                                "Wskaźnik dzietności"),
                    selected = "PKB per capita [$]"),
        
        #Wybór roku
        selectInput("hist_year", "Wybierz rok:", 
                    choices = unique(dane_zdrowotne$Rok), 
                    selected = max(dane_zdrowotne$Rok)), #Domyślnie ostatni rok
        
        #Wybór regionu
        selectInput("hist_region", "Wybierz region:", 
                    choices = unique(dane_zdrowotne$Region), 
                    selected = NULL, 
                    multiple = TRUE) #Możliwość wyboru wielu regionów
      ),
      mainPanel(
        plotOutput("histogramPlot")
      )
    )
  ),
  #Zakładka: Wykres słupkowy
  tabPanel(
    "Wykres słupkowy",
    sidebarLayout(
      sidebarPanel(
        h4("Instrukcja:"),
        div(
          p("W tej sekcji możesz analizować średnią wartość wybranej zmiennej dla każdego regionu w określonym roku w postaci wykresu słupkowego."),
          p("1. Wybór zmiennej: używając listy rozwijanej 'Wybierz zmienną' określ, która zmienna będzie na wykresie."),
          p("2. Wybór roku: wybierz rok z dostępnych danych, aby ograniczyć analizę do wybranego okresu."),
          p("3. Wykres słupkowy przedstawia średnią wartość wybranej zmiennej dla każdego regionu."),
          tags$ul(
            tags$li("Oś X: przedstawia nazwy regionów."),
            tags$li("Oś Y: pokazuje średnią wartość wybranej zmiennej w wybranym roku.")
          )
        ),
        #Wybór zmiennej
        selectInput("bar_var", "Wybierz zmienną:", 
                    choices = c("Oczekiwana długość życia", 
                                "Śmiertelność dzieci do lat 5", 
                                "Szczepienia przeciw odrze [% dzieci w 12-23 mies. życia]",
                                "PKB per capita [$]",
                                "PKB",
                                "Wzrost populacji (%)",
                                "Wskaźnik dzietności"),
                    selected = "PKB per capita [$]"),
        
        #wybór roku
        selectInput("bar_year", "Wybierz rok:", 
                    choices = unique(dane_zdrowotne$Rok), #Dynamiczne wybieranie roku na podstawie danych
                    selected = max(dane_zdrowotne$Rok)) #Domyślnie ostatni dostępny rok
      ),
      mainPanel(
        plotOutput("barPlot")
      )
    )
  )
)

#Server
server <- function(input, output, session) {
  
  #Filtrowanie zakładki z trendami
  filtered_data <- reactive({
    data <- dane_zdrowotne
    if (!is.null(input$region_filter)) {
      data <- data %>% filter(Region %in% input$region_filter)
    }
    if (!is.null(input$country_filter)) {
      data <- data %>% filter(Kraj %in% input$country_filter)
    }
    data
  })
  
  #Filtrowanie wykresu korelacji
  filtered_data_corr <- reactive({
    data <- dane_zdrowotne
    if (!is.null(input$region_filter_corr)) {
      data <- data %>% filter(Region %in% input$region_filter_corr)
    }
    if (!is.null(input$country_filter_corr)) {
      data <- data %>% filter(Kraj %in% input$country_filter_corr)
    }
    data
  })
  
  #Filtrowanie mapy
  filtered_data_map <- reactive({
    data <- dane_zdrowotne
    if (!is.null(input$region_filter_map)) {
      data <- data %>% filter(Region %in% input$region_filter_map)
    }
    data
  })
  
  #Wykres liniowy:
  output$filteredPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = Rok, y = get(input$variable_filter), group = Kraj, color = Kraj)) +
      geom_line(alpha = 0.7, linewidth = 1) +  #Zamiana size na linewidth
      theme_minimal() +
      theme(
        plot.title = element_text(size = 20, face = "bold", margin = margin(b = 20)),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14)
      ) +
      labs(
        title = paste("Trendy dla wskaźnika:", input$variable_filter),
        x = "Rok",
        y = input$variable_filter,
        color = "Kraj"
      )
  })
  
  
  #Wykres pudełkowy:
  output$boxPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = Region, y = .data[[input$variable_filter]], fill = Region)) +
      geom_boxplot() +
      theme_minimal() +
      labs(
        title = paste("Porównanie wskaźnika:", input$variable_filter, "w regionach"),
        x = "Region",
        y = input$variable_filter,
        fill = "Region"
      ) +
      theme(
        plot.title = element_text(size = 20, face = "bold", margin = margin(b = 20)),
        axis.title = element_text(size = 16),  #Spójna czcionka tytułów osi
        axis.text = element_text(size = 14),  #Spójna czcionka wartości osi
        axis.text.x = element_blank(),  #Usunięcie podpisów na osi X
        legend.title = element_text(size = 16, face = "bold"),  #Czcionka tytułu legendy
        legend.text = element_text(size = 14),
        plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
      )
  })
  
  #Korelacje zmiennych
  output$correlationPlot <- renderPlot({
    data <- filtered_data_corr()
    ggplot(data, aes(x = .data[[input$x_var]], y = .data[[input$y_var]], color = Region)) +
      geom_point(size = 3, alpha = 0.7) +
      geom_smooth(method = "lm", se = FALSE, color = "black") +
      theme_minimal() +
      labs(
        title = paste("Korelacja między", input$x_var, "a", input$y_var),
        x = input$x_var,
        y = input$y_var,
        color = "Region"
      ) +
      theme(
        plot.title = element_text(size = 20, face = "bold", margin = margin(b = 20)),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        strip.text = element_text(size = 14)
      ) +
      facet_wrap(~ Kraj, scales = "free", ncol = 2)
  })
  
  
  #Dynamiczna mapa
  output$interactiveMap <- renderLeaflet({
    latest_year_data <- filtered_data_map() %>%
      filter(Rok == max(Rok))
    
    leaflet(latest_year_data) %>%
      addTiles() %>%
      addCircleMarkers(
        ~longitude, ~latitude,
        radius = ~ `Oczekiwana długość życia` / 10, #Rozmiar punktów
        color = ~case_when(
          Dochód == "Wysokie dochody" ~ "blue",
          Dochód == "Wyższe średnie dochody" ~ "green",          
          Dochód == "Niższe średnie dochody" ~ "orange",
          Dochód == "Niskie dochody" ~ "red",
          TRUE ~ "grey"
        ),
        label = ~paste0(
          "Kraj: ", Kraj, " ",
          "Oczekiwana długość życia: ", round(`Oczekiwana długość życia`, 1), " ",
          "PKB per capita [$]: ", round(`PKB per capita [$]`, 0)
        ),
        labelOptions = labelOptions(
          style = list("font-size" = "14px", "font-weight" = "bold"),
          textsize = "15px",
          direction = "auto",
          noHide = FALSE,
          textOnly = FALSE
        ),
        options = pathOptions(interactive = TRUE)
      ) %>%
      addLegend(
        position = "bottomright",
        colors = c("blue", "green", "orange", "red"),
        labels = c("Wysokie dochody", "Wyższe średnie dochody", "Niższe średnie dochody", "Niskie dochody"),
        title = "Poziom dochodów",
        opacity = 1
      )
  })
  
  output$pieChart <- renderPlot({
    #Filtrowanie unikalnych krajów
    unique_data <- dane_zdrowotne %>%
      distinct(Kraj, .data[[input$pie_var]], .keep_all = TRUE)
    
    #Grupowanie i liczenie liczby krajów w każdej kategorii
    data <- unique_data %>%
      group_by(.data[[input$pie_var]]) %>%
      summarise(`Liczba krajów` = n()) %>%
      ungroup()
    
    #Dodanie kolumny procentowej
    data <- data %>%
      mutate(Procent = (`Liczba krajów` / sum(`Liczba krajów`)) * 100,
             angle = 90 - cumsum(Procent) + Procent / 2)  #Obliczanie kąta dla etykiety
    
    #Wykres kołowy z procentami
    ggplot(data, aes(x = "", y = `Liczba krajów`, fill = .data[[input$pie_var]])) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y") +
      geom_text(
        aes(label = paste0(`Liczba krajów`, " (", round(Procent, 1), "%)"), 
            angle = angle),  #Użycie kąta dla orientacji etykiety
        size = 5,
        fontface = "bold", 
        color = "black",
        hjust = 0.5,  #Wyśrodkowanie etykiet w poziomie
        vjust = 0.5,  #Wyśrodkowanie etykiet w pionie
        nudge_x = 1.1  #Przesunięcie etykiety na zewnątrz wykresu (przesunięcie w stronę krawędzi)
      ) +
      labs(
        title = paste("Rozkład liczby krajów według:", input$pie_var),
        fill = input$pie_var
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 20, face = "bold", margin = margin(b = 20)),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        axis.text = element_blank(),  #Usunięcie tekstu na osi
        axis.title = element_blank()
      )
  })
  output$pieChart <- renderPlot({
    #Filtrowanie unikalnych krajów
    unique_data <- dane_zdrowotne %>%
      distinct(Kraj, .data[[input$pie_var]], .keep_all = TRUE)
    
    #Grupowanie i liczenie liczby krajów w każdej kategorii
    data <- unique_data %>%
      group_by(.data[[input$pie_var]]) %>%
      summarise(`Liczba krajów` = n()) %>%
      ungroup()
    
    #Dodanie kolumny procentowej i formatu legendy
    data <- data %>%
      mutate(
        Procent = (`Liczba krajów` / sum(`Liczba krajów`)) * 100,
        Label = paste0("**", .data[[input$pie_var]], "**: ", `Liczba krajów`, " (", round(Procent, 1), "%)")
      )
    
    #Wykres kołowy (donut chart)
    ggplot(data, aes(x = 2, y = `Liczba krajów`, fill = Label)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y") +
      xlim(0.5, 2.5) +  #Przestrzeń w środku (donut)
      labs(
        title = paste("Rozkład liczby krajów według:", input$pie_var),
        fill = "Region (liczba krajów i %)"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 20, face = "bold", margin = margin(b = 20)),
        legend.title = element_text(size = 16),
        legend.text = element_markdown(size = 14),  #Umożliwia markdown
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank()
      )
  })
  
  
  #Server - Histogram
  filtered_data_hist <- reactive({
    data <- dane_zdrowotne %>%
      filter(Rok == input$hist_year)  #Filtrowanie po roku
    
    #Filtrowanie po regionie, jeśli wybrano regiony
    if (!is.null(input$hist_region)) {
      data <- data %>% filter(Region %in% input$hist_region)
    }
    
    data
  })
  
  output$histogramPlot <- renderPlot({
    ggplot(filtered_data_hist(), aes(x = .data[[input$hist_var]])) +
      geom_histogram(bins = 10, fill = "skyblue", color = "black") +
      theme_minimal() +
      labs(
        title = paste("Histogram zmiennej:", input$hist_var, 
                      "w roku:", input$hist_year,
                      ifelse(!is.null(input$hist_region), 
                             paste("w regionach:", paste(input$hist_region, collapse = ", ")), 
                             "")),
        x = input$hist_var,
        y = "Liczba obserwacji"
      ) +
      theme(
        plot.title = element_text(size = 20, face = "bold", margin = margin(b = 20)),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14)
      )
  })
  
  
  
  #Dane dla wykresu słupkowego
  filtered_data_bar <- reactive({
    dane_zdrowotne %>%
      filter(Rok == input$bar_year) %>%
      group_by(Region) %>%
      summarise(Srednia = mean(.data[[input$bar_var]], na.rm = TRUE))
  })
  
  #Wykres słupkowy
  output$barPlot <- renderPlot({
    ggplot(filtered_data_bar(), aes(x = Region, y = Srednia, fill = Region)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(
        title = paste("Średnia zmiennej:", input$bar_var, "dla regionów w roku:", input$bar_year),
        x = "Region",
        y = "Średnia wartość",
        fill = "Region"
      ) +
      theme(
        plot.title = element_text(size = 18, face = "bold", margin = margin(b = 20)),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),  #Dopasowanie osi Y
        axis.text.x = element_text(size = 14, angle = 45, hjust = 1),  #Obrót etykiet na osi X
        axis.text.y = element_text(size = 14),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14)
      )
  })
  
}
#Uruchomienie aplikacji
shinyApp(ui, server)

  
