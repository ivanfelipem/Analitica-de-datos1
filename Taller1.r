install.packages("readxl")
install.packages("tidyverse")
library(readxl)
library(tidyverse)

datos <- read_excel("Web_Analytics.xls")


View(datos)
head(datos)
str(datos)
summary(datos)
library(readxl)

excel_sheets("Web_Analytics.xls")

datos <- read_excel("Web_Analytics.xls", sheet = 2)

head(datos)
View(datos)
colnames(datos)

excel_sheets("Web_Analytics.xls")

library(tidyverse)

datos <- data.frame(
visits = c(2482,2896,1585,1689,1030,1166,1178,1613,1452,1246,1137,912,909,848,896,789,762,750,828,811,828,737,718,714,709),
pageviews = c(3729,4510,3037,2881,2313,2714,2414,2959,2676,2219,2400,1904,2156,1827,2068,1739,1764,1741,2074,1922,2131,1615,1598,1849,1767),
pages_per_visit = c(1.45,1.5,1.83,1.62,2.13,2.2,1.93,1.77,1.77,1.7,2.02,1.99,2.24,2.07,2.2,2.08,2.2,2.16,2.3,2.23,2.31,2.04,2.05,2.38,2.29),
time_on_site = c(31,35,52,37,63,69,59,49,48,45,59,54,76,50,73,77,73,81,65,75,78,67,69,70,72),
bounce_rate = c(0.8478,0.843,0.7655,0.7886,0.6934,0.7059,0.7316,0.7855,0.7761,0.7757,0.7347,0.7085,0.6833,0.6995,0.6858,0.6635,0.6571,0.6526,0.6456,0.6465,0.6418,0.6818,0.6633,0.6379,0.6192),
new_visits = c(0.9376,0.9311,0.9092,0.9129,0.8849,0.892,0.895,0.9277,0.9055,0.9063,0.8976,0.8945,0.8692,0.8889,0.8843,0.8766,0.8965,0.8462,0.8556,0.8686,0.8193,0.8548,0.8399,0.8415,0.8523)
)
head(datos)
summary(datos)

ggplot(datos, aes(x = 1:nrow(datos), y = visits)) +
  geom_line() +
  labs(
    title = "Website Visits Over Time",
    x = "Week",
    y = "Visits"
  )

ggplot(datos, aes(x = pages_per_visit, y = time_on_site)) +
  geom_point() +
  labs(
    title = "Engagement on Website",
    x = "Pages per Visit",
    y = "Time on Site"
  )
ggplot(datos, aes(x = week, y = visits)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Website Visits Over Time",
    x = "Week",
    y = "Number of Visits"
  )
datos$week <- 1:nrow(datos)
ggplot(datos, aes(x = week, y = visits)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Website Visits Over Time",
    x = "Week",
    y = "Number of Visits"
  )
ggplot(datos, aes(x = week, y = visits)) +
  geom_line() +
  geom_vline(xintercept = 30, linetype = "dashed", color = "red") +
  labs(
    title = "Website Visits Over Time",
    x = "Week",
    y = "Number of Visits"
  )
antes <- datos[datos$week < 30, ]
despues <- datos[datos$week >= 30, ]

mean(antes$visits)
mean(despues$visits)
datos$periodo <- ifelse(datos$week < 30, "Antes", "Despues")

ggplot(datos, aes(x = periodo, y = visits)) +
  geom_boxplot() +
  labs(
    title = "Visits Before vs After Brochure Campaign",
    x = "Period",
    y = "Visits"
  )
library(tidyverse)

# Crear dataframe
datos <- data.frame(
  visits = c(2482,2896,1585,1689,1030,1166,1178,1613,1452,1246,1137,912,909,848,896,789,762,750,828,811,828,737,718,714,709),
  pageviews = c(3729,4510,3037,2881,2313,2714,2414,2959,2676,2219,2400,1904,2156,1827,2068,1739,1764,1741,2074,1922,2131,1615,1598,1849,1767),
  pages_per_visit = c(1.45,1.5,1.83,1.62,2.13,2.2,1.93,1.77,1.77,1.7,2.02,1.99,2.24,2.07,2.2,2.08,2.2,2.16,2.3,2.23,2.31,2.04,2.05,2.38,2.29),
  time_on_site = c(31,35,52,37,63,69,59,49,48,45,59,54,76,50,73,77,73,81,65,75,78,67,69,70,72),
  bounce_rate = c(0.8478,0.843,0.7655,0.7886,0.6934,0.7059,0.7316,0.7855,0.7761,0.7757,0.7347,0.7085,0.6833,0.6995,0.6858,0.6635,0.6571,0.6526,0.6456,0.6465,0.6418,0.6818,0.6633,0.6379,0.6192),
  new_visits = c(0.9376,0.9311,0.9092,0.9129,0.8849,0.892,0.895,0.9277,0.9055,0.9063,0.8976,0.8945,0.8692,0.8889,0.8843,0.8766,0.8965,0.8462,0.8556,0.8686,0.8193,0.8548,0.8399,0.8415,0.8523)
)

# Crear variable semana
datos$week <- 1:nrow(datos)

# Gráfico visitas en el tiempo
ggplot(datos, aes(x = week, y = visits)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Website Visits Over Time",
    x = "Week",
    y = "Visits"
  )
# Marcar campaña
ggplot(datos, aes(x = week, y = visits)) +
  geom_line() +
  geom_vline(xintercept = 30, linetype = "dashed") +
  labs(
    title = "Impact of Marketing Campaign on Website Visits",
    x = "Week",
    y = "Visits"
  )

# Dividir antes y después
antes <- datos[datos$week < 30, ]
despues <- datos[datos$week >= 30, ]

# Promedio de visitas
mean(antes$visits)
mean(despues$visits)

# Comparación visual
datos$periodo <- ifelse(datos$week < 30, "Antes", "Despues")

ggplot(datos, aes(x = periodo, y = visits)) +
  geom_boxplot() +
  labs(
    title = "Visits Before vs After Marketing Campaign",
    x = "Periodo",
    y = "Visitas"
  )
# Librerías
library(tidyverse)

# Crear base de datos
datos <- data.frame(
visits = c(2482,2896,1585,1689,1030,1166,1178,1613,1452,1246,1137,912,909,848,896,789,762,750,828,811,828,737,718,714,709),
pageviews = c(3729,4510,3037,2881,2313,2714,2414,2959,2676,2219,2400,1904,2156,1827,2068,1739,1764,1741,2074,1922,2131,1615,1598,1849,1767),
pages_per_visit = c(1.45,1.5,1.83,1.62,2.13,2.2,1.93,1.77,1.77,1.7,2.02,1.99,2.24,2.07,2.2,2.08,2.2,2.16,2.3,2.23,2.31,2.04,2.05,2.38,2.29),
time_on_site = c(31,35,52,37,63,69,59,49,48,45,59,54,76,50,73,77,73,81,65,75,78,67,69,70,72),
bounce_rate = c(0.8478,0.843,0.7655,0.7886,0.6934,0.7059,0.7316,0.7855,0.7761,0.7757,0.7347,0.7085,0.6833,0.6995,0.6858,0.6635,0.6571,0.6526,0.6456,0.6465,0.6418,0.6818,0.6633,0.6379,0.6192),
new_visits = c(0.9376,0.9311,0.9092,0.9129,0.8849,0.892,0.895,0.9277,0.9055,0.9063,0.8976,0.8945,0.8692,0.8889,0.8843,0.8766,0.8965,0.8462,0.8556,0.8686,0.8193,0.8548,0.8399,0.8415,0.8523)
)

# Crear variable semana
datos$week <- 1:nrow(datos)

# Ver resumen estadístico
summary(datos)

# Histograma de visitas
ggplot(datos, aes(x = visits)) +
  geom_histogram(bins = 10) +
  labs(
    title = "Distribution of Website Visits",
    x = "Visits",
    y = "Frequency"
  )

# Relación visitas y páginas vistas
ggplot(datos, aes(x = visits, y = pageviews)) +
  geom_point() +
  labs(
    title = "Visits vs Pageviews",
    x = "Visits",
    y = "Pageviews"
  )

# Engagement de usuarios
ggplot(datos, aes(x = pages_per_visit, y = time_on_site)) +
  geom_point() +
  labs(
    title = "User Engagement",
    x = "Pages per Visit",
    y = "Time on Site"
  )

# Bounce rate vs visitas
ggplot(datos, aes(x = visits, y = bounce_rate)) +
  geom_point() +
  labs(
    title = "Visits vs Bounce Rate",
    x = "Visits",
    y = "Bounce Rate"
  )

# Evolución de visitas en el tiempo
ggplot(datos, aes(x = week, y = visits)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Website Visits Over Time",
    x = "Week",
    y = "Visits"
  )

png("visitas_web.png", width = 800, height = 600)

print(
  ggplot(datos, aes(x = week, y = visits)) +
    geom_line() +
    geom_point() +
    labs(
      title = "Website Visits Over Time",
      x = "Week",
      y = "Visits"
    )
)

dev.off()

cor(datos)
modelo <- lm(visits ~ pageviews + pages_per_visit + time_on_site + bounce_rate, data = datos)

summary(modelo)
ggplot(datos, aes(x = week, y = visits)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(
    title = "Trend of Website Visits",
    x = "Week",
    y = "Visits"
  )