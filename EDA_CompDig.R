# library ----
library(readxl)
library(tidyverse)
library(BSDA)

# data ---
data <- read_excel("~/Doc_Evelyn/competencia/data_comp_dig.xlsx")

data %>%
  select(-Otros...14) -> data

data <- data %>%
  mutate(P1 = `¿Te gustan las nuevas tecnologías de la información y la comunicación (TIC) y las usas habitualmente?`,
         P2 = `¿Consideras que has trabajado la competencia digital durante la carrera?`,
         P3 = `¿Has trabajado con el ordenador en el aula?`,
         CP1 = `Editores de texto (Word, OpenOffice Writer, LibreOffice Writer,…)...79`,
         CP2 = `Hojas de cálculo (Excel, Google Sheets, LibreOffice Calc,…)...80`,
         CP3 = `Bases de datos (Access, Base, nuBuilder,…)...81`,
         CP4 = `Creadores de presentaciones visuales (PowerPoint, Google Slides,…)...82`,
         CP5 = `Programas de edición de imagen (Paint, Adobe PhotoShop, Gimp,…)...83`,
         CP6 = `Programas de edición de audio (Audacity, Ocenaudio, Reaper, …)...84`,
         CP7 = `Programas de edición de video (Kdenlive, Shotcut, Pinnacle, Sony Vegas,…)...85`,
         CP8 = `Videoconferencia...86`,
         CP9 = `Listas de distribución...87`,
         CP10 = `Foros...88`,
         CP11 = `Mensajería instantánea / Chat...89`,
         CP12 = `Redes sociales (Facebook, twitter, Instagram,…)...90`,
         CP13 = `Herramientas de trabajo colaborativo (blogs, wikis,…)...91`,
         CP14 = `Herramientas de intercambio de archivos (Emule, Torrents,…)...92`,
         CP15 = `Herramientas de búsqueda de información en la red (Google,…)...93`,
         CP16 = `Traductores on-line...94`,
         CP17 = `Herramientas de publicación de imágenes en red (Flickr, Google Fotos, Imgur …)...95`,
         CP18 = `Presentaciones interactivas en red (prezi, SlideShare, Genially,…)...96`,
         CP19 = `Marcadores sociales (Diigo, Pocket,…)...97`,
         CP20 = `Lectores de RSS (Feedly, NewsTab,…)...98`,
         CP21 = `Páginas de inicio personalizadas (Start.me, Eversync, Protopage,…)...99`,
         CP22 = `Live streaming (Twitch, Livestream, …)...100`,
         CP23 = `Editores de páginas web (Wix, Jimdo, WordPress,..)...101`,
         CP24 = `Bibliotecas y enciclopedias virtuales...102`,
         CP25 = `Cartografía digital (google maps, google earth,…)...103`,
         CP26 = `Entornos virtuales de aprendizaje (Moodle, Dokeos, Sakai, Canvas,…)...104`,
         CP27 = `Entornos personales de aprendizaje (Symbaloo, Netvibes,...)...105`,
         CP28 = `Alojamiento de archivos multiplataforma en la nube (Google drive, OneDrive, Dropbox,..)...106`,
         CP29 = `Plataformas educativas (Google classroom, BrainCert, Edmodo,…)...107`,
         CP30 = `Software educativo de autor (Cuadernia, EdiLIM, Ardora, Educaplay, eXeLearning, Malted, Jclic, Hot Potatoes,…)...108`,
         CP31 = `Actividades guiadas de búsqueda en Internet (Webquest,  caza del tesoro,…)...109`,
         CP32 = `Mapas conceptuales (cmaptool, mindomo, bubbl.us, MindMeister,…)...110`,
         CP33 = `Realidad aumentada...111`,
         CP34 = `Códigos QR...112`,
         CP35 = `Gamificación...113`,
         UP1 = `Editores de texto (Word, OpenOffice Writer, LibreOffice Writer,…)...115`,
         UP2 = `Hojas de cálculo (Excel, Google Sheets, LibreOffice Calc,…)...116`,
         UP3 = `Bases de datos (Access, Base, nuBuilder,…)...117`,
         UP4 = `Creadores de presentaciones visuales (PowerPoint, Google Slides,…)...118`,
         UP5 = `Programas de edición de imagen (Paint, Adobe PhotoShop, Gimp,…)...119`,
         UP6 = `Programas de edición de audio (Audacity, Ocenaudio, Reaper, …)...120`,
         UP7 = `Programas de edición de video (Kdenlive, Shotcut, Pinnacle, Sony Vegas,…)...121`,
         UP8 = `Videoconferencia...122`,
         UP9 = `Listas de distribución...123`,
         UP10 = `Foros...124`,
         UP11 = `Mensajería instantánea / Chat...125`,
         UP12 = `Redes sociales (Facebook, twitter, Instagram,…)...126`,
         UP13 = `Herramientas de trabajo colaborativo (blogs, wikis,…)...127`,
         UP14 = `Herramientas de intercambio de archivos (Emule, Torrents,…)...128`,
         UP15 = `Herramientas de búsqueda de información en la red (Google,…)...129`,
         UP16 = `Traductores on-line...130`,
         UP17 = `Herramientas de publicación de imágenes en red (Flickr, Google Fotos, Imgur …)...131`,
         UP18 = `Presentaciones interactivas en red (prezi, SlideShare, Genially,…)...132`,
         UP19 = `Marcadores sociales (Diigo, Pocket,…)...133`,
         UP20 = `Lectores de RSS (Feedly, NewsTab,…)...134`,
         UP21 = `Páginas de inicio personalizadas (Start.me, Eversync, Protopage,…)...135`,
         UP22 = `Live streaming (Twitch, Livestream, …)...136`,
         UP23 = `Editores de páginas web (Wix, Jimdo, WordPress,..)...137`,
         UP24 = `Bibliotecas y enciclopedias virtuales...138`,
         UP25 = `Cartografía digital (google maps, google earth,…)...139`,
         UP26 = `Entornos virtuales de aprendizaje (Moodle, Dokeos, Sakai, Canvas,…)...140`,
         UP27 = `Entornos personales de aprendizaje (Symbaloo, Netvibes,...)...141`,
         UP28 = `Alojamiento de archivos multiplataforma en la nube (Google drive, OneDrive, Dropbox,..)...142`,
         UP29 = `Plataformas educativas (Google classroom, BrainCert, Edmodo,…)...143`,
         UP30 = `Software educativo de autor (Cuadernia, EdiLIM, Ardora, Educaplay, eXeLearning, Malted, Jclic, Hot Potatoes,…)...144`,
         UP31 = `Actividades guiadas de búsqueda en Internet (Webquest,  caza del tesoro,…)...145`,
         UP32 = `Mapas conceptuales (cmaptool, mindomo, bubbl.us, MindMeister,…)...146`,
         UP33 = `Realidad aumentada...147`,
         UP34 = `Códigos QR...148`,
         UP35 = `Gamificación...149`,
         CONP1 = `Editores de texto (Word, OpenOffice Writer, LibreOffice Writer,…)...151`,
         CONP2 = `Hojas de cálculo (Excel, Google Sheets, LibreOffice Calc,…)...152`,
         CONP3 = `Bases de datos (Access, Base, nuBuilder,…)...153`,
         CONP4 = `Creadores de presentaciones visuales (PowerPoint, Google Slides,…)...154`,
         CONP5 = `Programas de edición de imagen (Paint, Adobe PhotoShop, Gimp,…)...155`,
         CONP6 = `Programas de edición de audio (Audacity, Ocenaudio, Reaper, …)...156`,
         CONP7 = `Programas de edición de video (Kdenlive, Shotcut, Pinnacle, Sony Vegas,…)...157`,
         CONP8 = `Videoconferencia...158`,
         CONP9 = `Listas de distribución...159`,
         CONP10 = `Foros...160`,
         CONP11 = `Mensajería instantánea / Chat...161`,
         CONP12 = `Redes sociales (Facebook, twitter, Instagram,…)...162`,
         CONP13 = `Herramientas de trabajo colaborativo (blogs, wikis,…)...163`,
         CONP14 = `Herramientas de intercambio de archivos (Emule, Torrents,…)...164`,
         CONP15 = `Herramientas de búsqueda de información en la red (Google,…)...165`,
         CONP16 = `Traductores on-line...166`,
         CONP17 = `Herramientas de publicación de imágenes en red (Flickr, Google Fotos, Imgur …)...167`,
         CONP18 = `Presentaciones interactivas en red (prezi, SlideShare, Genially,…)...168`,
         CONP19 = `Marcadores sociales (Diigo, Pocket,…)...169`,
         CONP20 = `Lectores de RSS (Feedly, NewsTab,…)...170`,
         CONP21 = `Páginas de inicio personalizadas (Start.me, Eversync, Protopage,…)...171`,
         CONP22 = `Live streaming (Twitch, Livestream, …)...172`,
         CONP23 = `Editores de páginas web (Wix, Jimdo, WordPress,..)...173`,
         CONP24 = `Bibliotecas y enciclopedias virtuales...174`,
         CONP25 = `Cartografía digital (google maps, google earth,…)...175`,
         CONP26 = `Entornos virtuales de aprendizaje (Moodle, Dokeos, Sakai, Canvas,…)...176`,
         CONP27 = `Entornos personales de aprendizaje (Symbaloo, Netvibes,...)...177`,
         CONP28 = `Alojamiento de archivos multiplataforma en la nube (Google drive, OneDrive, Dropbox,..)...178`,
         CONP29 = `Plataformas educativas (Google classroom, BrainCert, Edmodo,…)...179`,
         CONP30 = `Software educativo de autor (Cuadernia, EdiLIM, Ardora, Educaplay, eXeLearning, Malted, Jclic, Hot Potatoes,…)...180`,
         CONP31 = `Actividades guiadas de búsqueda en Internet (Webquest,  caza del tesoro,…)...181`,
         CONP32 = `Mapas conceptuales (cmaptool, mindomo, bubbl.us, MindMeister,…)...182`,
         CONP33 = `Realidad aumentada...183`,
         CONP34 = `Códigos QR...184`,
         CONP35 = `Gamificación...185`)

# imputación de datos ----
data$Edad[141] <- mean(data$Edad,na.rm = TRUE) 
data$P2[136] <- round(mean(data$P2,na.rm = TRUE))

# subset data ----
data %>%
  filter(Paises=="España") -> data_uclm

data %>%
  filter(Paises=="Colombia") -> data_col

# age ----

data %>%
  summarise(n = length(Edad),
            media = mean(Edad),
            sd = sd(Edad),
            mediana = median(Edad),
            min = min(Edad),
            max = max(Edad))

# n media    sd mediana   min   max
# 329  22.8  5.93      21    16    67

data %>%
  group_by(Paises)%>%
  summarise(n = length(Edad),
            media = mean(Edad),
            sd = sd(Edad),
            mediana = median(Edad),
            min = min(Edad),
            max = max(Edad))

# statistical inference

shapiro.test(data$Edad)
# W = 0.68004, p-value < 2.2e-16
ks.test(scale(data$Edad), "pnorm")
# D = 0.24187, p-value < 2.2e-16
lillie.test(data$Edad)
# D = 0.24187, p-value < 2.2e-16

qqnorm(data$Edad, las=1, pch=18, 
       main="age of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$Edad)

shapiro.test(data_col$Edad)
# W = 0.75342, p-value = 1.68e-15
ks.test(scale(data_col$Edad), "pnorm")
# D = 0.20354, p-value = 1.802e-06
lillie.test(data_col$Edad)
# D = 0.20354, p-value < 2.2e-16

qqnorm(data_col$Edad, las=1, pch=18, 
       main="age col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$Edad)

shapiro.test(data_uclm$Edad)
# W = 0.633, p-value < 2.2e-16
ks.test(scale(data_uclm$Edad), "pnorm")
# D = 0.2987, p-value = 6.669e-13
lillie.test(data_uclm$Edad)
# D = 0.2987, p-value < 2.2e-16

qqnorm(data_uclm$Edad, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$Edad)

# non-parametric test
wilcox.test(data_col$Edad,
            data_uclm$Edad,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 11436, p-value = 0.01416
# result of the non-parametric test
# there are statistically significant differences

fligner.test(list(data_col$Edad,
                  data_uclm$Edad))

leveneTest(Edad ~ Paises, data = data,
           center = "median")

var.test(x = data_col$Edad,
         y = data_uclm$Edad)

bartlett.test(list(data_col$Edad,
                   data_uclm$Edad))

z.test(x = data_col$Edad, y = data_uclm$Edad, # Two samples with normal distribution
       alt = "two.sided",                     # Dos colas
       mu = 0,                                # H_0: mu_1 - mu_2 = 0
       sigma.x = sd(data_col$Edad),           # desviación estándar m
       sigma.y = sd(data_uclm$Edad),          # desviación estandar n
       conf.level = 0.95)
# z = 2.7582, p-value = 0.005813

t.test(x = data_col$Edad,
       y = data_uclm$Edad,
       alternative = "two.sided", 
       mu = 0, var.equal = FALSE, 
       conf.level = 0.95)
# t = 2.7582, df = 193.02, p-value = 0.006372

# gender ----
data %>%
  dplyr::count(Género)%>%
  mutate(P = n/sum(n)*100)

d <-table(data$Género,data$Paises)
(d[,1]/168)*100
(d[,2]/161)*100

chisq.test(d)
#X-squared = 0.11103, df = 1, p-value = 0.739

# ¿Te gustan las nuevas tecnologías de la información y la comunicación (TIC) y las usas habitualmente? ----

data %>%
  summarise(n = length(P1),
            media = mean(P1),
            sd = sd(P1),
            mediana = median(P1),
            min = min(P1),
            max = max(P1))

# n   media    sd mediana   min   max
# 329  3.96 0.851       4     2     5

data %>%
  group_by(Paises)%>%
  summarise(n = length(P1),
            media = mean(P1),
            sd = sd(P1),
            mediana = median(P1),
            min = min(P1),
            max = max(P1))

# Paises       n media    sd mediana   min   max
# Colombia   168  3.78 0.918       4     2     5
# España     161  4.14 0.732       4     2     5

# statistical inference

shapiro.test(data$P1)
# W = 0.85173, p-value < 2.2e-16
ks.test(scale(data$P1), "pnorm")
# D = 0.23423, p-value < 2.2e-16
lillie.test(data$P1)
# D = 0.23423, p-value < 2.2e-16

qqnorm(data$P1, las=1, pch=18, 
       main="P1 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$P1)

shapiro.test(data_col$P1)
# W = 0.86703, p-value = 4.953e-11
ks.test(scale(data_col$P1), "pnorm")
# D = 0.20089, p-value = 2.584e-06
lillie.test(data_col$P1)
# D = 0.20089, p-value < 2.2e-16

qqnorm(data_col$P1, las=1, pch=18, 
       main="P1 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$P1)

shapiro.test(data_uclm$P1)
# W = 0.81424, p-value = 4.966e-13
ks.test(scale(data_uclm$P1), "pnorm")
# D = 0.25492, p-value = 1.634e-09
lillie.test(data_uclm$P1)
# D = 0.25492, p-value < 2.2e-16

qqnorm(data_uclm$P1, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$P1)

# non-parametric test
wilcox.test(data_col$P1,
            data_uclm$P1,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 10528, p-value = 0.0002249
# result of the non-parametric test
# there are statistically significant differences

fligner.test(list(data_col$P1,
                  data_uclm$P1))

leveneTest(P1 ~ Paises, data = data,
           center = "median")

var.test(x = data_col$P1,
         y = data_uclm$P1)

bartlett.test(list(data_col$P1,
                   data_uclm$P1))

z.test(x = data_col$P1, y = data_uclm$P1, # Two samples with normal distribution
       alt = "two.sided",                 # Dos colas
       mu = 0,                            # H_0: mu_1 - mu_2 = 0
       sigma.x = sd(data_col$P1),         # desviación estándar m
       sigma.y = sd(data_uclm$P1),        # desviación estandar n
       conf.level = 0.95)
# z = -3.9741, p-value = 7.066e-05

t.test(x = data_col$P1,
       y = data_uclm$P1,
       alternative = "two.sided", 
       mu = 0, var.equal = FALSE, 
       conf.level = 0.95)
# t = -3.9741, df = 316.58, p-value = 8.756e-05

# ¿Consideras que has trabajado la competencia digital durante la carrera? ----

data %>%
  summarise(n = length(P2),
            media = mean(P2),
            sd = sd(P2),
            mediana = median(P2),
            min = min(P2),
            max = max(P2))

# n   media    sd mediana   min   max
# 329  3.48 0.963       4     1     5

data %>%
  group_by(Paises)%>%
  summarise(n = length(P2),
            media = mean(P2),
            sd = sd(P2),
            mediana = median(P2),
            min = min(P2),
            max = max(P2))

# Paises       n media    sd mediana   min   max
# Colombia   168  3.45 1.04        3     1     5
# España     161  3.52 0.881       4     1     5

# statistical inference

shapiro.test(data$P2)
# W = 0.89745, p-value = 4.175e-14
ks.test(scale(data$P2), "pnorm")
# D = 0.20878, p-value = 6.989e-13
lillie.test(data$P2)
# D = 0.20878, p-value < 2.2e-16

qqnorm(data$P2, las=1, pch=18, 
       main="P2 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$P2)

shapiro.test(data_col$P2)
# W = 0.90395, p-value = 5.124e-09
ks.test(scale(data_col$P2), "pnorm")
# D = 0.19735, p-value = 4.147e-06
lillie.test(data_col$P2)
# D = 0.19735, p-value < 2.2e-16

qqnorm(data_col$P2, las=1, pch=18, 
       main="P2 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$P2)

shapiro.test(data_uclm$P2)
# W = 0.88494, p-value = 7.636e-10
ks.test(scale(data_uclm$P2), "pnorm")
# D = 0.22192, p-value = 2.593e-07
lillie.test(data_uclm$P2)
# D = 0.22192, p-value < 2.2e-16

qqnorm(data_uclm$P2, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$P2)

# non-parametric test
wilcox.test(data_col$P2,
            data_uclm$P2,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 13090, p-value = 0.5984
# result of the non-parametric test
# there are statistically significant differences

fligner.test(list(data_col$P2,
                  data_uclm$P2))

leveneTest(P2 ~ Paises, data = data,
           center = "median")

var.test(x = data_col$P2,
         y = data_uclm$P2)

bartlett.test(list(data_col$P2,
                   data_uclm$P2))

z.test(x = data_col$P2, y = data_uclm$P2, # Two samples with normal distribution
       alt = "two.sided",                 # Dos colas
       mu = 0,                            # H_0: mu_1 - mu_2 = 0
       sigma.x = sd(data_col$P2),         # desviación estándar m
       sigma.y = sd(data_uclm$P2),        # desviación estandar n
       conf.level = 0.95)
# z = -0.71102, p-value = 0.4771

t.test(x = data_col$P2,
       y = data_uclm$P2,
       alternative = "two.sided", 
       mu = 0, var.equal = FALSE, 
       conf.level = 0.95)
# t = -0.71102, df = 322.41, p-value = 0.4776

# ¿Has trabajado con el ordenador en el aula? ----

data %>%
  summarise(n = length(P3),
            media = mean(P3),
            sd = sd(P3),
            mediana = median(P3),
            min = min(P3),
            max = max(P3))

# n   media    sd mediana   min   max
# 329  3.60  1.22       4     1     5

data %>%
  group_by(Paises)%>%
  summarise(n = length(P3),
            media = mean(P3),
            sd = sd(P3),
            mediana = median(P3),
            min = min(P3),
            max = max(P3))

# Paises       n media    sd mediana   min   max
# Colombia   168  3.11 1.29        3     1     5
# España     161  4.12 0.883       4     2     5

# statistical inference

shapiro.test(data$P3)
# W = 0.87722, p-value = 1.522e-15
ks.test(scale(data$P3), "pnorm")
# D = 0.20773, p-value = 9.326e-13
lillie.test(data$P3)
# D = 0.20773, p-value < 2.2e-16

qqnorm(data$P3, las=1, pch=18, 
       main="P3 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$P3)

shapiro.test(data_col$P3)
# W = 0.90395, p-value = 5.124e-09
ks.test(scale(data_col$P3), "pnorm")
# D = 0.1556, p-value = 0.0005859
lillie.test(data_col$P3)
# D = 0.1556, p-value = 1.101e-10

qqnorm(data_col$P3, las=1, pch=18, 
       main="P3 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$P3)

shapiro.test(data_uclm$P3)
# W = 0.81661, p-value = 6.135e-13
ks.test(scale(data_uclm$P3), "pnorm")
# D = 0.23566, p-value = 3.425e-08
lillie.test(data_uclm$P3)
# D = 0.23566, p-value < 2.2e-16

qqnorm(data_uclm$P3, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$P3)

# non-parametric test
wilcox.test(data_col$P3,
            data_uclm$P3,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 7476.5, p-value = 4.354e-13
# result of the non-parametric test
# there are statistically significant differences

fligner.test(list(data_col$P3,
                  data_uclm$P3))

leveneTest(P3 ~ Paises, data = data,
           center = "median")

var.test(x = data_col$P3,
         y = data_uclm$P3)

bartlett.test(list(data_col$P3,
                   data_uclm$P3))

z.test(x = data_col$P3, y = data_uclm$P3, # Two samples with normal distribution
       alt = "two.sided",                 # Dos colas
       mu = 0,                            # H_0: mu_1 - mu_2 = 0
       sigma.x = sd(data_col$P3),         # desviación estándar m
       sigma.y = sd(data_uclm$P3),        # desviación estandar n
       conf.level = 0.95)
# z = -8.2653, p-value < 2.2e-16

t.test(x = data_col$P3,
       y = data_uclm$P3,
       alternative = "two.sided", 
       mu = 0, var.equal = FALSE, 
       conf.level = 0.95)
# t = -8.2653, df = 296.04, p-value = 4.737e-15

# CP1 conoces: Editores de texto (Word, OpenOffice Writer, LibreOffice Writer,…) ----

data %>%
  select(Paises,CP1)%>%
  na.omit()%>%
  summarise(n = length(CP1),
            media = mean(CP1),
            sd = sd(CP1),
            mediana = median(CP1),
            min = min(CP1),
            max = max(CP1))

# n media    sd mediana   min   max
# 327  4.06 0.893       4     1     5

data %>%
  select(Paises,CP1)%>%
  na.omit()%>%
  group_by(Paises)%>%
  summarise(n = length(CP1),
            media = mean(CP1),
            sd = sd(CP1),
            mediana = median(CP1),
            min = min(CP1),
            max = max(CP1))

# Paises       n media    sd mediana   min   max
# Colombia   168  3.85 0.973       4     1     5
# España     159  4.28 0.739       4     2     5

# statistical inference

shapiro.test(data$CP1)
# W = 0.87722, p-value = 1.522e-15
ks.test(scale(data$CP1), "pnorm")
# D = 0.20773, p-value = 9.326e-13
lillie.test(data$CP1)
# D = 0.20773, p-value < 2.2e-16

qqnorm(data$CP1, las=1, pch=18, 
       main="CP1 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$CP1)

shapiro.test(data_col$CP1)
# W = 0.90395, p-value = 5.124e-09
ks.test(scale(data_col$CP1), "pnorm")
# D = 0.1556, p-value = 0.0005859
lillie.test(data_col$CP1)
# D = 0.1556, p-value = 1.101e-10

qqnorm(data_col$CP1, las=1, pch=18, 
       main="CP1 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$CP1)

shapiro.test(data_uclm$CP1)
# W = 0.81661, p-value = 6.135e-13
ks.test(scale(data_uclm$CP1), "pnorm")
# D = 0.23566, p-value = 3.425e-08
lillie.test(data_uclm$CP1)
# D = 0.23566, p-value < 2.2e-16

qqnorm(data_uclm$CP1, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$CP1)

# non-parametric test
wilcox.test(data_col$CP1,
            data_uclm$CP1,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 7476.5, p-value = 4.354e-13
# result of the non-parametric test
# there are statistically significant differences

fligner.test(list(data_col$CP1,
                  data_uclm$CP1))

leveneTest(CP1 ~ Paises, data = data,
           center = "median")

var.test(x = data_col$CP1,
         y = data_uclm$CP1)

bartlett.test(list(data_col$CP1,
                   data_uclm$CP1))

z.test(x = data_col$CP1, y = data_uclm$CP1, # Two samples with normal distribution
       alt = "two.sided",                 # Dos colas
       mu = 0,                            # H_0: mu_1 - mu_2 = 0
       sigma.x = sd(data_col$CP1),         # desviación estándar m
       sigma.y = sd(data_uclm$CP1),        # desviación estandar n
       conf.level = 0.95)
# z = -8.2653, p-value < 2.2e-16

t.test(x = data_col$CP1,
       y = data_uclm$CP1,
       alternative = "two.sided", 
       mu = 0, var.equal = FALSE, 
       conf.level = 0.95)
# t = -8.2653, df = 296.04, p-value = 4.737e-15

