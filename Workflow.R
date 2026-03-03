############################################################
# Data Science - minimalny workflow (Część II)
# Pliki: kraje_makro_1.csv, kraje_makro_2.csv
# Autor: (uzupełnij)
############################################################

# 0) Ustaw katalog roboczy (ODKOMENTUJ i ustaw swoją ścieżkę)
# setwd("C:/Twoj/folder/z/projektem")

# 1) Pakiety ----
# instalacja jednorazowa (odkomentuj jeśli nie masz)
# install.packages(c("dplyr", "ggplot2", "scales", "writexl"))

library(dplyr)
library(ggplot2)
library(scales)
library(writexl)

# 2) Import danych ----
kraje_1 <- read.table("kraje_makro_1.csv", header = TRUE, sep = ",", dec = ".")
kraje_2 <- read.table("kraje_makro_2.csv", header = TRUE, sep = ",", dec = ".")

# 3) Podgląd danych ----
cat("=== PODGLĄD kraj_1 ===\n")
print(head(kraje_1, 10))
print(summary(kraje_1))
str(kraje_1)

cat("\n=== PODGLĄD kraj_2 ===\n")
print(head(kraje_2, 10))
print(summary(kraje_2))
str(kraje_2)

# 4) Przygotowanie danych ----

# 4.1) Usuń zbędną kolumnę X, jeśli istnieje (czasem powstaje przy zapisie CSV)
if ("X" %in% names(kraje_1)) kraje_1$X <- NULL
if ("X" %in% names(kraje_2)) kraje_2$X <- NULL

# 4.2) Uporządkuj nazwy kolumn w kraje_2 (na polskie jak w instrukcji)
# Oczekiwane kolumny pliku 2:
# Country_Code, Country_Name, World_region, Urban.population.%.of.total., Internet.access.%.of.total.
# Zmieniamy na: Kod_kraju, Nazwa, Region, Urbanizacja_proc., Internet_proc.
colnames(kraje_2) <- c("Kod_kraju", "Nazwa", "Region", "Urbanizacja_proc.", "Internet_proc.")

# 4.3) Typy danych
# Region jako factor
kraje_2$Region <- as.factor(kraje_2$Region)

# 4.4) Braki danych
cat("\n=== BRAKI DANYCH ===\n")
print(colSums(is.na(kraje_1)))
print(colSums(is.na(kraje_2)))

# Pokaż wiersze z brakami internetu (jeśli są)
if (sum(is.na(kraje_2$Internet_proc.)) > 0) {
  cat("\nWiersze z brakami Internet_proc.:\n")
  print(kraje_2[is.na(kraje_2$Internet_proc.), ])
}

# 4.5) Czyszczenie: zamiana & na and w Region
# (żeby było mniej problemów przy dalszym przetwarzaniu)
kraje_2$Region <- gsub("&", "and", kraje_2$Region)
kraje_2$Region <- as.factor(kraje_2$Region)

# 5) Scalanie (merge) ----
# Łączymy po kodzie kraju: kraje_1$Kod == kraje_2$Kod_kraju
kraje <- merge(kraje_1, kraje_2, by.x = "Kod", by.y = "Kod_kraju")

# Po scaleniu można usunąć zbędną kolumnę Nazwa (bo Panstwo już jest w pliku 1)
if ("Nazwa" %in% names(kraje)) kraje$Nazwa <- NULL

cat("\n=== RAMKA PO SCALENIU ===\n")
print(head(kraje, 10))
print(summary(kraje))
str(kraje)

# 6) Podstawowa analiza (dplyr) ----

# 6.1) mutate: nowe zmienne
kraje <- kraje %>%
  mutate(
    Populacja_mln = Populacja / 1e6,
    PKB_per_capita = PKB / Populacja
  )

# 6.2) filter: przykłady
kraje_urban_50 <- kraje %>%
  filter(Urbanizacja_proc. > 50)

# 6.3) select: przykłady
kraje_podglad <- kraje %>%
  select(Panstwo, Region, PKB, Populacja_mln, PKB_per_capita, Urbanizacja_proc., Internet_proc.)

# 6.4) arrange: przykłady
kraje_przyrost_rosn <- kraje %>% arrange(Przyrost_populacji)
kraje_przyrost_malej <- kraje %>% arrange(desc(Przyrost_populacji))

# 6.5) Przykład: kraje z PKB > 1 bilion (1e12)
kraje_powyzej_1T <- kraje %>%
  filter(PKB > 1e12) %>%
  arrange(PKB) %>%
  select(Panstwo, PKB, PKB_per_capita)

cat("\nLiczba krajów z PKB > 1e12:", nrow(kraje_powyzej_1T), "\n")

# 6.6) group_by + summarise: regiony
regiony_podsumowanie <- kraje %>%
  group_by(Region) %>%
  summarise(
    liczba_krajow = n(),
    sredni_internet = mean(Internet_proc., na.rm = TRUE),
    srednia_urbanizacja = mean(Urbanizacja_proc., na.rm = TRUE),
    sredni_pkb_pc = mean(PKB_per_capita, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(sredni_internet))

cat("\n=== PODSUMOWANIE REGIONÓW (posortowane po internecie) ===\n")
print(regiony_podsumowanie)

# 6.7) „Bogatsze niż średnia regionu”
bogate <- kraje %>%
  group_by(Region) %>%
  filter(PKB_per_capita > mean(PKB_per_capita, na.rm = TRUE)) %>%
  ungroup()

cat("\nLiczba krajów bogatszych niż średnia regionu:", nrow(bogate), "\n")

# 7) Wizualizacje (ggplot2) ----
# Zrobimy kilka wykresów i zapiszemy je automatycznie do folderu "wykresy"

if (!dir.exists("wykresy")) dir.create("wykresy")

# 7.1) Urbanizacja vs PKB per capita (prosty)
p1 <- ggplot(kraje, aes(x = Urbanizacja_proc., y = PKB_per_capita)) +
  geom_point() +
  labs(
    title = "Urbanizacja a PKB per capita",
    x = "Urbanizacja (%)",
    y = "PKB per capita (USD)"
  )

# 7.2) Urbanizacja vs PKB per capita (kolor region + log)
p2 <- ggplot(kraje, aes(x = Urbanizacja_proc., y = PKB_per_capita, color = Region)) +
  geom_point(size = 3, alpha = 0.7) +
  scale_y_log10(labels = comma) +
  labs(
    title = "Urbanizacja a PKB per capita",
    subtitle = "Czy bardziej zurbanizowane kraje są bogatsze?",
    x = "Urbanizacja (% ludności miejskiej)",
    y = "PKB per capita (USD, skala log)",
    color = "Region świata"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# 7.3) Populacja vs PKB (oba log, rozmiar = PKB per capita)
p3 <- ggplot(kraje, aes(x = Populacja_mln, y = PKB, size = PKB_per_capita, color = Region)) +
  geom_point(alpha = 0.7) +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    title = "Skala gospodarki i demografia",
    x = "Populacja (mln, log10)",
    y = "PKB (USD, log10)",
    size = "PKB per capita",
    color = "Region"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# 7.4) Liczba krajów w regionach
p4 <- ggplot(kraje, aes(x = Region)) +
  geom_bar() +
  labs(
    title = "Liczba krajów w regionach świata",
    x = "Region",
    y = "Liczba krajów"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 7.5) TOP 15 najbogatszych krajów (PKB per capita)
p5 <- kraje %>%
  arrange(desc(PKB_per_capita)) %>%
  head(15) %>%
  ggplot(aes(x = reorder(Panstwo, PKB_per_capita), y = PKB_per_capita, fill = Region)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(
    title = "TOP 15 najbogatszych krajów świata (2016)",
    subtitle = "PKB per capita w USD",
    x = NULL,
    y = "PKB per capita (USD)",
    fill = "Region"
  ) +
  theme_minimal()

# 7.6) Boxplot: internet wg regionu
p6 <- ggplot(kraje, aes(x = reorder(Region, Internet_proc., FUN = median),
                        y = Internet_proc., fill = Region)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 1) +
  coord_flip() +
  labs(
    title = "Dostęp do internetu według regionów świata",
    subtitle = "Punkty to poszczególne kraje",
    x = NULL,
    y = "Dostęp do internetu (% populacji)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Zapis wykresów
ggsave("wykresy/01_urbanizacja_pkbpc_prosty.png", p1, width = 8, height = 5, dpi = 150)
ggsave("wykresy/02_urbanizacja_pkbpc_region_log.png", p2, width = 9, height = 6, dpi = 150)
ggsave("wykresy/03_populacja_pkb_log.png", p3, width = 9, height = 6, dpi = 150)
ggsave("wykresy/04_liczba_krajow_regiony.png", p4, width = 9, height = 6, dpi = 150)
ggsave("wykresy/05_top15_pkbpc.png", p5, width = 9, height = 7, dpi = 150)
ggsave("wykresy/06_boxplot_internet_region.png", p6, width = 9, height = 6, dpi = 150)

cat("\nWykresy zapisane w folderze: wykresy/\n")

# 8) Eksport wyników ----
# 8.1) Eksport pełnej ramki po obliczeniach
write.csv(kraje, "kraje_analiza.csv", row.names = FALSE)
write_xlsx(kraje, "kraje_wynik.xlsx")

# 8.2) Eksport tabel podsumowań (opcjonalnie)
write.csv(regiony_podsumowanie, "regiony_podsumowanie.csv", row.names = FALSE)
write_xlsx(regiony_podsumowanie, "regiony_podsumowanie.xlsx")

cat("\nGotowe! Zapisano:\n- kraje_analiza.csv\n- kraje_wynik.xlsx\n- regiony_podsumowanie.csv/.xlsx\n- wykresy w folderze wykresy/\n")