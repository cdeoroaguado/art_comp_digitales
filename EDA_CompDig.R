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
# transformación ----
data$CP11 <- as.numeric(data$CP11)

# imputación de datos ----
data$Edad[141] <- mean(data$Edad,na.rm = TRUE) 
data$P2[136] <- round(mean(data$P2,na.rm = TRUE))
data$CP14[68] <- NA

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

# Paises       n media    sd mediana   min   max
# Colombia   168  23.7  7.94      20    16    67
# España     161  21.9  2.18      21    19    37

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


# statistical inference

shapiro.test(data$CP1)
ks.test(scale(data$CP1), "pnorm")
lillie.test(data$CP1)

qqnorm(data$CP1, las=1, pch=18, 
       main="CP1 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$CP1)

shapiro.test(data_col$CP1)
ks.test(scale(data_col$CP1), "pnorm")
lillie.test(data_col$CP1)

qqnorm(data_col$CP1, las=1, pch=18, 
       main="CP1 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$CP1)

shapiro.test(data_uclm$CP1)
ks.test(scale(data_uclm$CP1), "pnorm")
lillie.test(data_uclm$CP1)

qqnorm(data_uclm$CP1, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$CP1)

# non-parametric test
wilcox.test(data_col$CP1,
            data_uclm$CP1,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 10052, p-value = 4.244e-05
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

z.test(x = data_col %>%
         select(CP1)%>%
         na.omit()%>%
         unlist(), y = data_uclm %>%
         select(CP1)%>%
         na.omit()%>%
         unlist(),                                      # Two samples with normal distribution
       alt = "two.sided",                               # Dos colas
       mu = 0,                                          # H_0: mu_1 - mu_2 = 0
       sigma.x = sd(data_col$CP1,na.rm = TRUE),         # desviación estándar m
       sigma.y = sd(data_uclm$CP1,na.rm = TRUE),        # desviación estandar n
       conf.level = 0.95)
# z = -4.5982, p-value = 4.263e-06

t.test(x = data_col$CP1,
       y = data_uclm$CP1,
       alternative = "two.sided", 
       mu = 0, var.equal = FALSE, 
       conf.level = 0.95)
# t = -4.5982, df = 310.67, p-value = 6.211e-06

# CP2 conoces: Hojas de cálculo (Excel, Google Sheets, LibreOffice Calc,…) ----

data %>%
  select(Paises,CP2)%>%
  na.omit()%>%
  summarise(n = length(CP2),
            media = mean(CP2),
            sd = sd(CP2),
            mediana = median(CP2),
            min = min(CP2),
            max = max(CP2))

data %>%
  select(Paises,CP2)%>%
  na.omit()%>%
  group_by(Paises)%>%
  summarise(n = length(CP2),
            media = mean(CP2),
            sd = sd(CP2),
            mediana = median(CP2),
            min = min(CP2),
            max = max(CP2))

# statistical inference

shapiro.test(data$CP2)
ks.test(scale(data$CP2), "pnorm")
lillie.test(data$CP2)

qqnorm(data$CP2, las=1, pch=18, 
       main="CP2 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$CP2)

shapiro.test(data_col$CP2)
ks.test(scale(data_col$CP2), "pnorm")
lillie.test(data_col$CP2)

qqnorm(data_col$CP2, las=1, pch=18, 
       main="CP2 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$CP2)

shapiro.test(data_uclm$CP2)
ks.test(scale(data_uclm$CP2), "pnorm")
lillie.test(data_uclm$CP2)

qqnorm(data_uclm$CP2, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$CP2)

# non-parametric test
wilcox.test(data_col$CP2,
            data_uclm$CP2,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 15948, p-value = 0.0017
# result of the non-parametric test
# there are statistically significant differences


# CP3 conoces: Bases de datos (Access, Base, nuBuilder,…) ----

data %>%
  select(Paises,CP3)%>%
  na.omit()%>%
  summarise(n = length(CP3),
            media = mean(CP3),
            sd = sd(CP3),
            mediana = median(CP3),
            min = min(CP3),
            max = max(CP3))

data %>%
  select(Paises,CP3)%>%
  na.omit()%>%
  group_by(Paises)%>%
  summarise(n = length(CP3),
            media = mean(CP3),
            sd = sd(CP3),
            mediana = median(CP3),
            min = min(CP3),
            max = max(CP3))

# statistical inference

shapiro.test(data$CP3)
ks.test(scale(data$CP3), "pnorm")
lillie.test(data$CP3)

qqnorm(data$CP3, las=1, pch=18, 
       main="CP3 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$CP3)

shapiro.test(data_col$CP3)
ks.test(scale(data_col$CP3), "pnorm")
lillie.test(data_col$CP3)

qqnorm(data_col$CP3, las=1, pch=18, 
       main="CP3 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$CP3)

shapiro.test(data_uclm$CP3)
ks.test(scale(data_uclm$CP3), "pnorm")
lillie.test(data_uclm$CP3)

qqnorm(data_uclm$CP3, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$CP3)

# non-parametric test
wilcox.test(data_col$CP3,
            data_uclm$CP3,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 16324, p-value = 0.0001284
# result of the non-parametric test
# there are statistically significant differences

# CP4 conoces: Creadores de presentaciones visuales (PowerPoint, Google Slides,…) ----

data %>%
  select(Paises,CP4)%>%
  na.omit()%>%
  summarise(n = length(CP4),
            media = mean(CP4),
            sd = sd(CP4),
            mediana = median(CP4),
            min = min(CP4),
            max = max(CP4))

data %>%
  select(Paises,CP4)%>%
  na.omit()%>%
  group_by(Paises)%>%
  summarise(n = length(CP4),
            media = mean(CP4),
            sd = sd(CP4),
            mediana = median(CP4),
            min = min(CP4),
            max = max(CP4))

# statistical inference

shapiro.test(data$CP4)
ks.test(scale(data$CP4), "pnorm")
lillie.test(data$CP4)

qqnorm(data$CP4, las=1, pch=18, 
       main="CP4 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$CP4)

shapiro.test(data_col$CP4)
ks.test(scale(data_col$CP4), "pnorm")
lillie.test(data_col$CP4)

qqnorm(data_col$CP4, las=1, pch=18, 
       main="CP4 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$CP4)

shapiro.test(data_uclm$CP4)
ks.test(scale(data_uclm$CP4), "pnorm")
lillie.test(data_uclm$CP4)

qqnorm(data_uclm$CP4, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$CP4)

# non-parametric test
wilcox.test(data_col$CP4,
            data_uclm$CP4,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 9319, p-value = 3.064e-07
# result of the non-parametric test
# there are statistically significant differences

# CP5 conoces: Programas de edición de imagen (Paint, Adobe PhotoShop, Gimp,…) ----

data %>%
  select(Paises,CP5)%>%
  na.omit()%>%
  summarise(n = length(CP5),
            media = mean(CP5),
            sd = sd(CP5),
            mediana = median(CP5),
            min = min(CP5),
            max = max(CP5))

data %>%
  select(Paises,CP5)%>%
  na.omit()%>%
  group_by(Paises)%>%
  summarise(n = length(CP5),
            media = mean(CP5),
            sd = sd(CP5),
            mediana = median(CP5),
            min = min(CP5),
            max = max(CP5))

# statistical inference

shapiro.test(data$CP5)
ks.test(scale(data$CP5), "pnorm")
lillie.test(data$CP5)

qqnorm(data$CP5, las=1, pch=18, 
       main="CP5 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$CP5)

shapiro.test(data_col$CP5)
ks.test(scale(data_col$CP5), "pnorm")
lillie.test(data_col$CP5)

qqnorm(data_col$CP5, las=1, pch=18, 
       main="CP5 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$CP5)

shapiro.test(data_uclm$CP5)
ks.test(scale(data_uclm$CP5), "pnorm")
lillie.test(data_uclm$CP5)

qqnorm(data_uclm$CP5, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$CP5)

# non-parametric test
wilcox.test(data_col$CP5,
            data_uclm$CP5,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 13998, p-value = 0.3753
# result of the non-parametric test
# there are statistically significant differences

# CP6 conoces: Programas de edición de audio (Audacity, Ocenaudio, Reaper, …) ----

data %>%
  select(Paises,CP6)%>%
  na.omit()%>%
  summarise(n = length(CP6),
            media = mean(CP6),
            sd = sd(CP6),
            mediana = median(CP6),
            min = min(CP6),
            max = max(CP6))


data %>%
  select(Paises,CP6)%>%
  na.omit()%>%
  group_by(Paises)%>%
  summarise(n = length(CP6),
            media = mean(CP6),
            sd = sd(CP6),
            mediana = median(CP6),
            min = min(CP6),
            max = max(CP6))


# statistical inference

shapiro.test(data$CP6)
ks.test(scale(data$CP6), "pnorm")
lillie.test(data$CP6)

qqnorm(data$CP6, las=1, pch=18, 
       main="CP6 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$CP6)

shapiro.test(data_col$CP6)
ks.test(scale(data_col$CP6), "pnorm")
lillie.test(data_col$CP6)

qqnorm(data_col$CP6, las=1, pch=18, 
       main="CP6 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$CP6)

shapiro.test(data_uclm$CP6)
ks.test(scale(data_uclm$CP6), "pnorm")
lillie.test(data_uclm$CP6)

qqnorm(data_uclm$CP6, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$CP6)

# non-parametric test
wilcox.test(data_col$CP6,
            data_uclm$CP6,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 13956, p-value = 0.47
# result of the non-parametric test
# there are statistically significant differences

# CP7 conoces: Programas de edición de video (Kdenlive, Shotcut, Pinnacle, Sony Vegas,…) ----

data %>%
  select(Paises,CP7)%>%
  na.omit()%>%
  summarise(n = length(CP7),
            media = mean(CP7),
            sd = sd(CP7),
            mediana = median(CP7),
            min = min(CP7),
            max = max(CP7))

data %>%
  select(Paises,CP7)%>%
  na.omit()%>%
  group_by(Paises)%>%
  summarise(n = length(CP7),
            media = mean(CP7),
            sd = sd(CP7),
            mediana = median(CP7),
            min = min(CP7),
            max = max(CP7))

# statistical inference

shapiro.test(data$CP7)
ks.test(scale(data$CP7), "pnorm")
lillie.test(data$CP7)

qqnorm(data$CP7, las=1, pch=18, 
       main="CP7 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$CP7)

shapiro.test(data_col$CP7)
ks.test(scale(data_col$CP7), "pnorm")
lillie.test(data_col$CP7)

qqnorm(data_col$CP7, las=1, pch=18, 
       main="CP7 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$CP7)

shapiro.test(data_uclm$CP7)
ks.test(scale(data_uclm$CP7), "pnorm")
lillie.test(data_uclm$CP7)

qqnorm(data_uclm$CP7, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$CP7)

# non-parametric test
wilcox.test(data_col$CP7,
            data_uclm$CP7,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 13982, p-value = 0.4525
# result of the non-parametric test
# there are statistically significant differences

# CP8 conoces: Videoconferencia ----

data %>%
  select(Paises,CP8)%>%
  na.omit()%>%
  summarise(n = length(CP8),
            media = mean(CP8),
            sd = sd(CP8),
            mediana = median(CP8),
            min = min(CP8),
            max = max(CP8))

data %>%
  select(Paises,CP8)%>%
  na.omit()%>%
  group_by(Paises)%>%
  summarise(n = length(CP8),
            media = mean(CP8),
            sd = sd(CP8),
            mediana = median(CP8),
            min = min(CP8),
            max = max(CP8))

# statistical inference

shapiro.test(data$CP8)
ks.test(scale(data$CP8), "pnorm")
lillie.test(data$CP8)

qqnorm(data$CP8, las=1, pch=18, 
       main="CP8 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$CP8)

shapiro.test(data_col$CP8)
ks.test(scale(data_col$CP8), "pnorm")
lillie.test(data_col$CP8)

qqnorm(data_col$CP8, las=1, pch=18, 
       main="CP8 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$CP8)

shapiro.test(data_uclm$CP8)
ks.test(scale(data_uclm$CP8), "pnorm")
lillie.test(data_uclm$CP8)

qqnorm(data_uclm$CP8, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$CP8)

# non-parametric test
wilcox.test(data_col$CP8,
            data_uclm$CP8,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 10428, p-value = 0.0004426
# result of the non-parametric test
# there are statistically significant differences

# CP9 conoces: Listas de distribución ----

data %>%
  select(Paises,CP9)%>%
  na.omit()%>%
  summarise(n = length(CP9),
            media = mean(CP9),
            sd = sd(CP9),
            mediana = median(CP9),
            min = min(CP9),
            max = max(CP9))

data %>%
  select(Paises,CP9)%>%
  na.omit()%>%
  group_by(Paises)%>%
  summarise(n = length(CP9),
            media = mean(CP9),
            sd = sd(CP9),
            mediana = median(CP9),
            min = min(CP9),
            max = max(CP9))

# statistical inference

shapiro.test(data$CP9)
ks.test(scale(data$CP9), "pnorm")
lillie.test(data$CP9)

qqnorm(data$CP9, las=1, pch=18, 
       main="CP9 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$CP9)

shapiro.test(data_col$CP9)
ks.test(scale(data_col$CP9), "pnorm")
lillie.test(data_col$CP9)

qqnorm(data_col$CP9, las=1, pch=18, 
       main="CP9 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$CP9)

shapiro.test(data_uclm$CP9)
ks.test(scale(data_uclm$CP9), "pnorm")
lillie.test(data_uclm$CP9)

qqnorm(data_uclm$CP9, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$CP9)

# non-parametric test
wilcox.test(data_col$CP9,
            data_uclm$CP9,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 16160, p-value = 0.0006903
# result of the non-parametric test
# there are statistically significant differences

# CP10 conoces: Foros ----

data %>%
  select(Paises,CP10)%>%
  na.omit()%>%
  summarise(n = length(CP10),
            media = mean(CP10),
            sd = sd(CP10),
            mediana = median(CP10),
            min = min(CP10),
            max = max(CP10))

data %>%
  select(Paises,CP10)%>%
  na.omit()%>%
  group_by(Paises)%>%
  summarise(n = length(CP10),
            media = mean(CP10),
            sd = sd(CP10),
            mediana = median(CP10),
            min = min(CP10),
            max = max(CP10))

# statistical inference

shapiro.test(data$CP10)
ks.test(scale(data$CP10), "pnorm")
lillie.test(data$CP10)

qqnorm(data$CP10, las=1, pch=18, 
       main="CP10 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$CP10)

shapiro.test(data_col$CP10)
ks.test(scale(data_col$CP10), "pnorm")
lillie.test(data_col$CP10)

qqnorm(data_col$CP10, las=1, pch=18, 
       main="CP10 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$CP10)

shapiro.test(data_uclm$CP10)
ks.test(scale(data_uclm$CP10), "pnorm")
lillie.test(data_uclm$CP10)

qqnorm(data_uclm$CP10, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$CP10)

# non-parametric test
wilcox.test(data_col$CP10,
            data_uclm$CP10,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 14725, p-value = 0.07886
# result of the non-parametric test
# there are statistically significant differences

# CP11 conoces: Mensajería instantánea / Chat ----

data %>%
  select(Paises,CP11)%>%
  na.omit()%>%
  summarise(n = length(CP11),
            media = mean(data$CP11,na.rm = TRUE),
            sd = sd(CP11),
            mediana = median(CP11),
            min = min(CP11),
            max = max(CP11))

data %>%
  select(Paises,CP11)%>%
  na.omit()%>%
  group_by(Paises)%>%
  summarise(n = length(CP11),
            media = mean(CP11),
            sd = sd(CP11),
            mediana = median(CP11),
            min = min(CP11),
            max = max(CP11))

# statistical inference

shapiro.test(data$CP11)
ks.test(scale(data$CP11), "pnorm")
lillie.test(data$CP11)

qqnorm(data$CP11, las=1, pch=18, 
       main="CP11 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$CP11)

shapiro.test(data_col$CP11)
ks.test(scale(data_col$CP11), "pnorm")
lillie.test(data_col$CP11)

qqnorm(data_col$CP11, las=1, pch=18, 
       main="CP11 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$CP11)

shapiro.test(data_uclm$CP11)
ks.test(scale(data_uclm$CP11), "pnorm")
lillie.test(data_uclm$CP11)

qqnorm(data_uclm$CP11, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$CP11)

# non-parametric test
wilcox.test(data_col$CP11,
            data_uclm$CP11,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 9176.5, p-value = 1.315e-07
# result of the non-parametric test
# there are statistically significant differences

# CP12 conoces: Redes sociales (Facebook, twitter, Instagram,…) ----

data %>%
  select(Paises,CP12)%>%
  na.omit()%>%
  summarise(n = length(CP12),
            media = mean(CP12),
            sd = sd(CP12),
            mediana = median(CP12),
            min = min(CP12),
            max = max(CP12))

data %>%
  select(Paises,CP12)%>%
  na.omit()%>%
  group_by(Paises)%>%
  summarise(n = length(CP12),
            media = mean(CP12),
            sd = sd(CP12),
            mediana = median(CP12),
            min = min(CP12),
            max = max(CP12))

# statistical inference

shapiro.test(data$CP12)
ks.test(scale(data$CP12), "pnorm")
lillie.test(data$CP12)

qqnorm(data$CP12, las=1, pch=18, 
       main="CP12 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$CP12)

shapiro.test(data_col$CP12)
ks.test(scale(data_col$CP12), "pnorm")
lillie.test(data_col$CP12)

qqnorm(data_col$CP12, las=1, pch=18, 
       main="CP12 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$CP12)

shapiro.test(data_uclm$CP12)
ks.test(scale(data_uclm$CP12), "pnorm")
lillie.test(data_uclm$CP12)

qqnorm(data_uclm$CP12, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$CP12)

# non-parametric test
wilcox.test(data_col$CP12,
            data_uclm$CP12,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 9681, p-value = 3.666e-07
# result of the non-parametric test
# there are statistically significant differences

# CP13 conoces: Herramientas de trabajo colaborativo (blogs, wikis,…) ----

data %>%
  select(Paises,CP13)%>%
  na.omit()%>%
  summarise(n = length(CP13),
            media = mean(CP13),
            sd = sd(CP13),
            mediana = median(CP13),
            min = min(CP13),
            max = max(CP13))

data %>%
  select(Paises,CP13)%>%
  na.omit()%>%
  group_by(Paises)%>%
  summarise(n = length(CP13),
            media = mean(CP13),
            sd = sd(CP13),
            mediana = median(CP13),
            min = min(CP13),
            max = max(CP13))

# statistical inference

shapiro.test(data$CP13)
ks.test(scale(data$CP13), "pnorm")
lillie.test(data$CP13)

qqnorm(data$CP13, las=1, pch=18, 
       main="CP13 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$CP13)

shapiro.test(data_col$CP13)
ks.test(scale(data_col$CP13), "pnorm")
lillie.test(data_col$CP13)

qqnorm(data_col$CP13, las=1, pch=18, 
       main="CP13 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$CP13)

shapiro.test(data_uclm$CP13)
ks.test(scale(data_uclm$CP13), "pnorm")
lillie.test(data_uclm$CP13)

qqnorm(data_uclm$CP13, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$CP13)

# non-parametric test
wilcox.test(data_col$CP13,
            data_uclm$CP13,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 12202, p-value = 0.1614
# result of the non-parametric test
# there are statistically significant differences

# CP14 conoces: Herramientas de intercambio de archivos (Emule, Torrents,…) ----

data %>%
  select(Paises,CP14)%>%
  na.omit()%>%
  summarise(n = length(CP14),
            media = mean(CP14),
            sd = sd(CP14),
            mediana = median(CP14),
            min = min(CP14),
            max = max(CP14))


data %>%
  select(Paises,CP14)%>%
  na.omit()%>%
  group_by(Paises)%>%
  summarise(n = length(CP14),
            media = mean(CP14),
            sd = sd(CP14),
            mediana = median(CP14),
            min = min(CP14),
            max = max(CP14))


# statistical inference

shapiro.test(data$CP14)
ks.test(scale(data$CP14), "pnorm")
lillie.test(data$CP14)

qqnorm(data$CP14, las=1, pch=18, 
       main="CP14 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$CP14)

shapiro.test(data_col$CP14)
ks.test(scale(data_col$CP14), "pnorm")
lillie.test(data_col$CP14)

qqnorm(data_col$CP14, las=1, pch=18, 
       main="CP14 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$CP14)

shapiro.test(data_uclm$CP14)
ks.test(scale(data_uclm$CP14), "pnorm")
lillie.test(data_uclm$CP14)

qqnorm(data_uclm$CP14, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$CP14)

# non-parametric test
wilcox.test(data_col$CP14,
            data_uclm$CP14,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 12752, p-value = 0.4692
# result of the non-parametric test
# there are statistically significant differences

# CP15 conoces: Herramientas de búsqueda de información en la red (Google,…) ----

data %>%
  select(Paises,CP15)%>%
  na.omit()%>%
  summarise(n = length(CP15),
            media = mean(CP15),
            sd = sd(CP15),
            mediana = median(CP15),
            min = min(CP15),
            max = max(CP15))


data %>%
  select(Paises,CP15)%>%
  na.omit()%>%
  group_by(Paises)%>%
  summarise(n = length(CP15),
            media = mean(CP15),
            sd = sd(CP15),
            mediana = median(CP15),
            min = min(CP15),
            max = max(CP15))

# statistical inference

shapiro.test(data$CP15)
ks.test(scale(data$CP15), "pnorm")
lillie.test(data$CP15)

qqnorm(data$CP15, las=1, pch=18, 
       main="CP15 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$CP15)

shapiro.test(data_col$CP15)
ks.test(scale(data_col$CP15), "pnorm")
lillie.test(data_col$CP15)

qqnorm(data_col$CP15, las=1, pch=18, 
       main="CP15 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$CP15)

shapiro.test(data_uclm$CP15)
ks.test(scale(data_uclm$CP15), "pnorm")
lillie.test(data_uclm$CP15)

qqnorm(data_uclm$CP15, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$CP15)

# non-parametric test
wilcox.test(data_col$CP15,
            data_uclm$CP15,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 8842, p-value = 5.038e-09
# result of the non-parametric test
# there are statistically significant differences

# CP16 conoces: Traductores on-line ----

data %>%
  select(Paises,CP16)%>%
  na.omit()%>%
  summarise(n = length(CP16),
            media = mean(CP16),
            sd = sd(CP16),
            mediana = median(CP16),
            min = min(CP16),
            max = max(CP16))

data %>%
  select(Paises,CP16)%>%
  na.omit()%>%
  group_by(Paises)%>%
  summarise(n = length(CP16),
            media = mean(CP16),
            sd = sd(CP16),
            mediana = median(CP16),
            min = min(CP16),
            max = max(CP16))

# statistical inference

shapiro.test(data$CP16)
ks.test(scale(data$CP16), "pnorm")
lillie.test(data$CP16)

qqnorm(data$CP16, las=1, pch=18, 
       main="CP16 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$CP16)

shapiro.test(data_col$CP16)
ks.test(scale(data_col$CP16), "pnorm")
lillie.test(data_col$CP16)

qqnorm(data_col$CP16, las=1, pch=18, 
       main="CP16 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$CP16)

shapiro.test(data_uclm$CP16)
ks.test(scale(data_uclm$CP16), "pnorm")
lillie.test(data_uclm$CP16)

qqnorm(data_uclm$CP16, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$CP16)

# non-parametric test
wilcox.test(data_col$CP16,
            data_uclm$CP16,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 11108, p-value = 0.006889
# result of the non-parametric test
# there are statistically significant differences

# CP17 conoces: Herramientas de publicación de imágenes en red (Flickr, Google Fotos, Imgur …) ----

data %>%
  select(Paises,CP17)%>%
  na.omit()%>%
  summarise(n = length(CP17),
            media = mean(CP17),
            sd = sd(CP17),
            mediana = median(CP17),
            min = min(CP17),
            max = max(CP17))

data %>%
  select(Paises,CP17)%>%
  na.omit()%>%
  group_by(Paises)%>%
  summarise(n = length(CP17),
            media = mean(CP17),
            sd = sd(CP17),
            mediana = median(CP17),
            min = min(CP17),
            max = max(CP17))

# statistical inference

shapiro.test(data$CP17)
ks.test(scale(data$CP17), "pnorm")
lillie.test(data$CP17)

qqnorm(data$CP17, las=1, pch=18, 
       main="CP17 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$CP17)

shapiro.test(data_col$CP17)
ks.test(scale(data_col$CP17), "pnorm")
lillie.test(data_col$CP17)

qqnorm(data_col$CP17, las=1, pch=18, 
       main="CP17 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$CP17)

shapiro.test(data_uclm$CP17)
ks.test(scale(data_uclm$CP17), "pnorm")
lillie.test(data_uclm$CP17)

qqnorm(data_uclm$CP17, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$CP17)

# non-parametric test
wilcox.test(data_col$CP17,
            data_uclm$CP17,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 13892, p-value = 0.5198
# result of the non-parametric test
# there are statistically significant differences

# CP18 conoces: Presentaciones interactivas en red (prezi, SlideShare, Genially,…) ----

data %>%
  select(Paises,CP18)%>%
  na.omit()%>%
  summarise(n = length(CP18),
            media = mean(CP18),
            sd = sd(CP18),
            mediana = median(CP18),
            min = min(CP18),
            max = max(CP18))

data %>%
  select(Paises,CP18)%>%
  na.omit()%>%
  group_by(Paises)%>%
  summarise(n = length(CP18),
            media = mean(CP18),
            sd = sd(CP18),
            mediana = median(CP18),
            min = min(CP18),
            max = max(CP18))

# statistical inference

shapiro.test(data$CP18)
ks.test(scale(data$CP18), "pnorm")
lillie.test(data$CP18)

qqnorm(data$CP18, las=1, pch=18, 
       main="CP18 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$CP18)

shapiro.test(data_col$CP18)
ks.test(scale(data_col$CP18), "pnorm")
lillie.test(data_col$CP18)

qqnorm(data_col$CP18, las=1, pch=18, 
       main="CP18 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$CP18)

shapiro.test(data_uclm$CP18)
ks.test(scale(data_uclm$CP18), "pnorm")
lillie.test(data_uclm$CP18)

qqnorm(data_uclm$CP18, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$CP18)

# non-parametric test
wilcox.test(data_col$CP18,
            data_uclm$CP18,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 11474, p-value = 0.02862
# result of the non-parametric test
# there are statistically significant differences

# CP19 conoces: Marcadores sociales (Diigo, Pocket,…) ----

data %>%
  select(Paises,CP19)%>%
  na.omit()%>%
  summarise(n = length(CP19),
            media = mean(CP19),
            sd = sd(CP19),
            mediana = median(CP19),
            min = min(CP19),
            max = max(CP19))

data %>%
  select(Paises,CP19)%>%
  na.omit()%>%
  group_by(Paises)%>%
  summarise(n = length(CP19),
            media = mean(CP19),
            sd = sd(CP19),
            mediana = median(CP19),
            min = min(CP19),
            max = max(CP19))

# statistical inference

shapiro.test(data$CP19)
ks.test(scale(data$CP19), "pnorm")
lillie.test(data$CP19)

qqnorm(data$CP19, las=1, pch=18, 
       main="CP19 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$CP19)

shapiro.test(data_col$CP19)
ks.test(scale(data_col$CP19), "pnorm")
lillie.test(data_col$CP19)

qqnorm(data_col$CP19, las=1, pch=18, 
       main="CP19 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$CP19)

shapiro.test(data_uclm$CP19)
ks.test(scale(data_uclm$CP19), "pnorm")
lillie.test(data_uclm$CP19)

qqnorm(data_uclm$CP19, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$CP19)

# non-parametric test
wilcox.test(data_col$CP19,
            data_uclm$CP19,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 18680, p-value = 4.607e-11
# result of the non-parametric test
# there are statistically significant differences

# CP20 conoces: Lectores de RSS (Feedly, NewsTab,…) ----

data %>%
  select(Paises,CP20)%>%
  na.omit()%>%
  summarise(n = length(CP20),
            media = mean(CP20),
            sd = sd(CP20),
            mediana = median(CP20),
            min = min(CP20),
            max = max(CP20))

data %>%
  select(Paises,CP20)%>%
  na.omit()%>%
  group_by(Paises)%>%
  summarise(n = length(CP20),
            media = mean(CP20),
            sd = sd(CP20),
            mediana = median(CP20),
            min = min(CP20),
            max = max(CP20))

# statistical inference

shapiro.test(data$CP20)
ks.test(scale(data$CP20), "pnorm")
lillie.test(data$CP20)

qqnorm(data$CP20, las=1, pch=18, 
       main="CP20 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$CP20)

shapiro.test(data_col$CP20)
ks.test(scale(data_col$CP20), "pnorm")
lillie.test(data_col$CP20)

qqnorm(data_col$CP20, las=1, pch=18, 
       main="CP20 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$CP20)

shapiro.test(data_uclm$CP20)
ks.test(scale(data_uclm$CP20), "pnorm")
lillie.test(data_uclm$CP20)

qqnorm(data_uclm$CP20, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$CP20)

# non-parametric test
wilcox.test(data_col$CP20,
            data_uclm$CP20,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 17964, p-value = 7.939e-09
# result of the non-parametric test
# there are statistically significant differences

# CP21 conoces: Páginas de inicio personalizadas (Start.me, Eversync, Protopage,…) ----

data %>%
  select(Paises,CP21)%>%
  na.omit()%>%
  summarise(n = length(CP21),
            media = mean(CP21),
            sd = sd(CP21),
            mediana = median(CP21),
            min = min(CP21),
            max = max(CP21))

data %>%
  select(Paises,CP21)%>%
  na.omit()%>%
  group_by(Paises)%>%
  summarise(n = length(CP21),
            media = mean(CP21),
            sd = sd(CP21),
            mediana = median(CP21),
            min = min(CP21),
            max = max(CP21))

# statistical inference

shapiro.test(data$CP21)
ks.test(scale(data$CP21), "pnorm")
lillie.test(data$CP21)

qqnorm(data$CP21, las=1, pch=18, 
       main="CP21 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$CP21)

shapiro.test(data_col$CP21)
ks.test(scale(data_col$CP21), "pnorm")
lillie.test(data_col$CP21)

qqnorm(data_col$CP21, las=1, pch=18, 
       main="CP21 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$CP21)

shapiro.test(data_uclm$CP21)
ks.test(scale(data_uclm$CP21), "pnorm")
lillie.test(data_uclm$CP21)

qqnorm(data_uclm$CP21, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$CP21)

# non-parametric test
wilcox.test(data_col$CP21,
            data_uclm$CP21,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 17664, p-value = 7.298e-09
# result of the non-parametric test
# there are statistically significant differences

# CP22 conoces: Live streaming (Twitch, Livestream, …) ----

data %>%
  select(Paises,CP22)%>%
  na.omit()%>%
  summarise(n = length(CP22),
            media = mean(CP22),
            sd = sd(CP22),
            mediana = median(CP22),
            min = min(CP22),
            max = max(CP22))

data %>%
  select(Paises,CP22)%>%
  na.omit()%>%
  group_by(Paises)%>%
  summarise(n = length(CP22),
            media = mean(CP22),
            sd = sd(CP22),
            mediana = median(CP22),
            min = min(CP22),
            max = max(CP22))

# statistical inference

shapiro.test(data$CP22)
ks.test(scale(data$CP22), "pnorm")
lillie.test(data$CP22)

qqnorm(data$CP22, las=1, pch=18, 
       main="CP22 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$CP22)

shapiro.test(data_col$CP22)
ks.test(scale(data_col$CP22), "pnorm")
lillie.test(data_col$CP22)

qqnorm(data_col$CP22, las=1, pch=18, 
       main="CP22 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$CP22)

shapiro.test(data_uclm$CP22)
ks.test(scale(data_uclm$CP22), "pnorm")
lillie.test(data_uclm$CP22)

qqnorm(data_uclm$CP22, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$CP22)

# non-parametric test
wilcox.test(data_col$CP22,
            data_uclm$CP22,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 10292, p-value = 0.0003354
# result of the non-parametric test
# there are statistically significant differences

# CP23 conoces: Editores de páginas web (Wix, Jimdo, WordPress,..) ----

data %>%
  select(Paises,CP23)%>%
  na.omit()%>%
  summarise(n = length(CP23),
            media = mean(CP23),
            sd = sd(CP23),
            mediana = median(CP23),
            min = min(CP23),
            max = max(CP23))

data %>%
  select(Paises,CP23)%>%
  na.omit()%>%
  group_by(Paises)%>%
  summarise(n = length(CP23),
            media = mean(CP23),
            sd = sd(CP23),
            mediana = median(CP23),
            min = min(CP23),
            max = max(CP23))

# statistical inference

shapiro.test(data$CP23)
ks.test(scale(data$CP23), "pnorm")
lillie.test(data$CP23)

qqnorm(data$CP23, las=1, pch=18, 
       main="CP23 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$CP23)

shapiro.test(data_col$CP23)
ks.test(scale(data_col$CP23), "pnorm")
lillie.test(data_col$CP23)

qqnorm(data_col$CP23, las=1, pch=18, 
       main="CP23 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$CP23)

shapiro.test(data_uclm$CP23)
ks.test(scale(data_uclm$CP23), "pnorm")
lillie.test(data_uclm$CP23)

qqnorm(data_uclm$CP23, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$CP23)

# non-parametric test
wilcox.test(data_col$CP23,
            data_uclm$CP23,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 14308, p-value = 0.2109
# result of the non-parametric test
# there are statistically significant differences

# CP24 conoces: Bibliotecas y enciclopedias virtuales ----

data %>%
  select(Paises,CP24)%>%
  na.omit()%>%
  summarise(n = length(CP24),
            media = mean(CP24),
            sd = sd(CP24),
            mediana = median(CP24),
            min = min(CP24),
            max = max(CP24))

data %>%
  select(Paises,CP24)%>%
  na.omit()%>%
  group_by(Paises)%>%
  summarise(n = length(CP24),
            media = mean(CP24),
            sd = sd(CP24),
            mediana = median(CP24),
            min = min(CP24),
            max = max(CP24))

# statistical inference

shapiro.test(data$CP24)
ks.test(scale(data$CP24), "pnorm")
lillie.test(data$CP24)

qqnorm(data$CP24, las=1, pch=18, 
       main="CP24 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$CP24)

shapiro.test(data_col$CP24)
ks.test(scale(data_col$CP24), "pnorm")
lillie.test(data_col$CP24)

qqnorm(data_col$CP24, las=1, pch=18, 
       main="CP24 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$CP24)

shapiro.test(data_uclm$CP24)
ks.test(scale(data_uclm$CP24), "pnorm")
lillie.test(data_uclm$CP24)

qqnorm(data_uclm$CP24, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$CP24)

# non-parametric test
wilcox.test(data_col$CP24,
            data_uclm$CP24,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 12666, p-value = 0.4035
# result of the non-parametric test
# there are statistically significant differences

# CP25 conoces: Cartografía digital (google maps, google earth,…) ----

data %>%
  select(Paises,CP25)%>%
  na.omit()%>%
  summarise(n = length(CP25),
            media = mean(CP25),
            sd = sd(CP25),
            mediana = median(CP25),
            min = min(CP25),
            max = max(CP25))

data %>%
  select(Paises,CP25)%>%
  na.omit()%>%
  group_by(Paises)%>%
  summarise(n = length(CP25),
            media = mean(CP25),
            sd = sd(CP25),
            mediana = median(CP25),
            min = min(CP25),
            max = max(CP25))

# statistical inference

shapiro.test(data$CP25)
ks.test(scale(data$CP25), "pnorm")
lillie.test(data$CP25)

qqnorm(data$CP25, las=1, pch=18, 
       main="CP25 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$CP25)

shapiro.test(data_col$CP25)
ks.test(scale(data_col$CP25), "pnorm")
lillie.test(data_col$CP25)

qqnorm(data_col$CP25, las=1, pch=18, 
       main="CP25 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$CP25)

shapiro.test(data_uclm$CP25)
ks.test(scale(data_uclm$CP25), "pnorm")
lillie.test(data_uclm$CP25)

qqnorm(data_uclm$CP25, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$CP25)

# non-parametric test
wilcox.test(data_col$CP25,
            data_uclm$CP25,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 13956, p-value = 0.47
# result of the non-parametric test
# there are statistically significant differences

# CP26 conoces: Entornos virtuales de aprendizaje (Moodle, Dokeos, Sakai, Canvas,…) ----

data %>%
  select(Paises,CP26)%>%
  na.omit()%>%
  summarise(n = length(CP26),
            media = mean(CP26),
            sd = sd(CP26),
            mediana = median(CP26),
            min = min(CP26),
            max = max(CP26))

data %>%
  select(Paises,CP26)%>%
  na.omit()%>%
  group_by(Paises)%>%
  summarise(n = length(CP26),
            media = mean(CP26),
            sd = sd(CP26),
            mediana = median(CP26),
            min = min(CP26),
            max = max(CP26))

# statistical inference

shapiro.test(data$CP26)
ks.test(scale(data$CP26), "pnorm")
lillie.test(data$CP26)

qqnorm(data$CP26, las=1, pch=18, 
       main="CP26 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$CP26)

shapiro.test(data_col$CP26)
ks.test(scale(data_col$CP26), "pnorm")
lillie.test(data_col$CP26)

qqnorm(data_col$CP26, las=1, pch=18, 
       main="CP26 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$CP26)

shapiro.test(data_uclm$CP26)
ks.test(scale(data_uclm$CP26), "pnorm")
lillie.test(data_uclm$CP26)

qqnorm(data_uclm$CP26, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$CP26)

# non-parametric test
wilcox.test(data_col$CP26,
            data_uclm$CP26,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 6041.5, p-value < 2.2e-16
# result of the non-parametric test
# there are statistically significant differences

# CP27 conoces: Entornos personales de aprendizaje (Symbaloo, Netvibes,...) ----

data %>%
  select(Paises,CP27)%>%
  na.omit()%>%
  summarise(n = length(CP27),
            media = mean(CP27),
            sd = sd(CP27),
            mediana = median(CP27),
            min = min(CP27),
            max = max(CP27))

data %>%
  select(Paises,CP27)%>%
  na.omit()%>%
  group_by(Paises)%>%
  summarise(n = length(CP27),
            media = mean(CP27),
            sd = sd(CP27),
            mediana = median(CP27),
            min = min(CP27),
            max = max(CP27))

# statistical inference

shapiro.test(data$CP27)
ks.test(scale(data$CP27), "pnorm")
lillie.test(data$CP27)

qqnorm(data$CP27, las=1, pch=18, 
       main="CP27 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$CP27)

shapiro.test(data_col$CP27)
ks.test(scale(data_col$CP27), "pnorm")
lillie.test(data_col$CP27)

qqnorm(data_col$CP27, las=1, pch=18, 
       main="CP27 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$CP27)

shapiro.test(data_uclm$CP27)
ks.test(scale(data_uclm$CP27), "pnorm")
lillie.test(data_uclm$CP27)

qqnorm(data_uclm$CP27, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$CP27)

# non-parametric test
wilcox.test(data_col$CP27,
            data_uclm$CP27,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 18050, p-value = 3.265e-09
# result of the non-parametric test
# there are statistically significant differences

# CP28 conoces: Alojamiento de archivos multiplataforma en la nube (Google drive, OneDrive, Dropbox,..) ----

data %>%
  select(Paises,CP28)%>%
  na.omit()%>%
  summarise(n = length(CP28),
            media = mean(CP28),
            sd = sd(CP28),
            mediana = median(CP28),
            min = min(CP28),
            max = max(CP28))

data %>%
  select(Paises,CP28)%>%
  na.omit()%>%
  group_by(Paises)%>%
  summarise(n = length(CP28),
            media = mean(CP28),
            sd = sd(CP28),
            mediana = median(CP28),
            min = min(CP28),
            max = max(CP28))

# statistical inference

shapiro.test(data$CP28)
ks.test(scale(data$CP28), "pnorm")
lillie.test(data$CP28)

qqnorm(data$CP28, las=1, pch=18, 
       main="CP28 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$CP28)

shapiro.test(data_col$CP28)
ks.test(scale(data_col$CP28), "pnorm")
lillie.test(data_col$CP28)

qqnorm(data_col$CP28, las=1, pch=18, 
       main="CP28 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$CP28)

shapiro.test(data_uclm$CP28)
ks.test(scale(data_uclm$CP28), "pnorm")
lillie.test(data_uclm$CP28)

qqnorm(data_uclm$CP28, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$CP28)

# non-parametric test
wilcox.test(data_col$CP28,
            data_uclm$CP28,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 8982.5, p-value = 1.293e-07
# result of the non-parametric test
# there are statistically significant differences

# CP29 conoces: Plataformas educativas (Google classroom, BrainCert, Edmodo,…) ----

data %>%
  select(Paises,CP29)%>%
  na.omit()%>%
  summarise(n = length(CP29),
            media = mean(CP29),
            sd = sd(CP29),
            mediana = median(CP29),
            min = min(CP29),
            max = max(CP29))

data %>%
  select(Paises,CP29)%>%
  na.omit()%>%
  group_by(Paises)%>%
  summarise(n = length(CP29),
            media = mean(CP29),
            sd = sd(CP29),
            mediana = median(CP29),
            min = min(CP29),
            max = max(CP29))

# statistical inference

shapiro.test(data$CP29)
ks.test(scale(data$CP29), "pnorm")
lillie.test(data$CP29)

qqnorm(data$CP29, las=1, pch=18, 
       main="CP29 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$CP29)

shapiro.test(data_col$CP29)
ks.test(scale(data_col$CP29), "pnorm")
lillie.test(data_col$CP29)

qqnorm(data_col$CP29, las=1, pch=18, 
       main="CP29 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$CP29)

shapiro.test(data_uclm$CP29)
ks.test(scale(data_uclm$CP29), "pnorm")
lillie.test(data_uclm$CP29)

qqnorm(data_uclm$CP29, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$CP29)

# non-parametric test
wilcox.test(data_col$CP29,
            data_uclm$CP29,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 12002, p-value = 0.1199
# result of the non-parametric test
# there are statistically significant differences

# CP30 conoces: Software educativo de autor (Cuadernia, EdiLIM, Ardora, Educaplay, eXeLearning, Malted, Jclic, Hot Potatoes,…) ----

data %>%
  select(Paises,CP30)%>%
  na.omit()%>%
  summarise(n = length(CP30),
            media = mean(CP30),
            sd = sd(CP30),
            mediana = median(CP30),
            min = min(CP30),
            max = max(CP30))

data %>%
  select(Paises,CP30)%>%
  na.omit()%>%
  group_by(Paises)%>%
  summarise(n = length(CP30),
            media = mean(CP30),
            sd = sd(CP30),
            mediana = median(CP30),
            min = min(CP30),
            max = max(CP30))

# statistical inference

shapiro.test(data$CP30)
ks.test(scale(data$CP30), "pnorm")
lillie.test(data$CP30)

qqnorm(data$CP30, las=1, pch=18, 
       main="CP30 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$CP30)

shapiro.test(data_col$CP30)
ks.test(scale(data_col$CP30), "pnorm")
lillie.test(data_col$CP30)

qqnorm(data_col$CP30, las=1, pch=18, 
       main="CP30 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$CP30)

shapiro.test(data_uclm$CP30)
ks.test(scale(data_uclm$CP30), "pnorm")
lillie.test(data_uclm$CP30)

qqnorm(data_uclm$CP30, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$CP30)

# non-parametric test
wilcox.test(data_col$CP30,
            data_uclm$CP30,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 17532, p-value = 4.927e-07
# result of the non-parametric test
# there are statistically significant differences

# CP31 conoces: Actividades guiadas de búsqueda en Internet (Webquest,  caza del tesoro,…) ----

data %>%
  select(Paises,CP31)%>%
  na.omit()%>%
  summarise(n = length(CP31),
            media = mean(CP31),
            sd = sd(CP31),
            mediana = median(CP31),
            min = min(CP31),
            max = max(CP31))


data %>%
  select(Paises,CP31)%>%
  na.omit()%>%
  group_by(Paises)%>%
  summarise(n = length(CP31),
            media = mean(CP31),
            sd = sd(CP31),
            mediana = median(CP31),
            min = min(CP31),
            max = max(CP31))

# statistical inference

shapiro.test(data$CP31)
ks.test(scale(data$CP31), "pnorm")
lillie.test(data$CP31)

qqnorm(data$CP31, las=1, pch=18, 
       main="CP31 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$CP31)

shapiro.test(data_col$CP31)
ks.test(scale(data_col$CP31), "pnorm")
lillie.test(data_col$CP31)

qqnorm(data_col$CP31, las=1, pch=18, 
       main="CP31 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$CP31)

shapiro.test(data_uclm$CP31)
ks.test(scale(data_uclm$CP31), "pnorm")
lillie.test(data_uclm$CP31)

qqnorm(data_uclm$CP31, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$CP31)

# non-parametric test
wilcox.test(data_col$CP31,
            data_uclm$CP31,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 15918, p-value = 0.002073
# result of the non-parametric test
# there are statistically significant differences

# CP32 conoces: Mapas conceptuales (cmaptool, mindomo, bubbl.us, MindMeister,…) ----

data %>%
  select(Paises,CP32)%>%
  na.omit()%>%
  summarise(n = length(CP32),
            media = mean(CP32),
            sd = sd(CP32),
            mediana = median(CP32),
            min = min(CP32),
            max = max(CP32))

data %>%
  select(Paises,CP32)%>%
  na.omit()%>%
  group_by(Paises)%>%
  summarise(n = length(CP32),
            media = mean(CP32),
            sd = sd(CP32),
            mediana = median(CP32),
            min = min(CP32),
            max = max(CP32))

# statistical inference

shapiro.test(data$CP32)
ks.test(scale(data$CP32), "pnorm")
lillie.test(data$CP32)

qqnorm(data$CP32, las=1, pch=18, 
       main="CP32 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$CP32)

shapiro.test(data_col$CP32)
ks.test(scale(data_col$CP32), "pnorm")
lillie.test(data_col$CP32)

qqnorm(data_col$CP32, las=1, pch=18, 
       main="CP32 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$CP32)

shapiro.test(data_uclm$CP32)
ks.test(scale(data_uclm$CP32), "pnorm")
lillie.test(data_uclm$CP32)

qqnorm(data_uclm$CP32, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$CP32)

# non-parametric test
wilcox.test(data_col$CP32,
            data_uclm$CP32,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 16700, p-value = 6.063e-05
# result of the non-parametric test
# there are statistically significant differences

# CP33 conoces: Realidad aumentada ----

data %>%
  select(Paises,CP33)%>%
  na.omit()%>%
  summarise(n = length(CP33),
            media = mean(CP33),
            sd = sd(CP33),
            mediana = median(CP33),
            min = min(CP33),
            max = max(CP33))

data %>%
  select(Paises,CP33)%>%
  na.omit()%>%
  group_by(Paises)%>%
  summarise(n = length(CP33),
            media = mean(CP33),
            sd = sd(CP33),
            mediana = median(CP33),
            min = min(CP33),
            max = max(CP33))

# statistical inference

shapiro.test(data$CP33)
ks.test(scale(data$CP33), "pnorm")
lillie.test(data$CP33)

qqnorm(data$CP33, las=1, pch=18, 
       main="CP33 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$CP33)

shapiro.test(data_col$CP33)
ks.test(scale(data_col$CP33), "pnorm")
lillie.test(data_col$CP33)

qqnorm(data_col$CP33, las=1, pch=18, 
       main="CP33 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$CP33)

shapiro.test(data_uclm$CP33)
ks.test(scale(data_uclm$CP33), "pnorm")
lillie.test(data_uclm$CP33)

qqnorm(data_uclm$CP33, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$CP33)

# non-parametric test
wilcox.test(data_col$CP33,
            data_uclm$CP33,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 14150, p-value = 0.242
# result of the non-parametric test
# there are statistically significant differences

# CP34 conoces: Códigos QR ----

data %>%
  select(Paises,CP34)%>%
  na.omit()%>%
  summarise(n = length(CP34),
            media = mean(CP34),
            sd = sd(CP34),
            mediana = median(CP34),
            min = min(CP34),
            max = max(CP34))

data %>%
  select(Paises,CP34)%>%
  na.omit()%>%
  group_by(Paises)%>%
  summarise(n = length(CP34),
            media = mean(CP34),
            sd = sd(CP34),
            mediana = median(CP34),
            min = min(CP34),
            max = max(CP34))

# statistical inference

shapiro.test(data$CP34)
ks.test(scale(data$CP34), "pnorm")
lillie.test(data$CP34)

qqnorm(data$CP34, las=1, pch=18, 
       main="CP34 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$CP34)

shapiro.test(data_col$CP34)
ks.test(scale(data_col$CP34), "pnorm")
lillie.test(data_col$CP34)

qqnorm(data_col$CP34, las=1, pch=18, 
       main="CP34 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$CP34)

shapiro.test(data_uclm$CP34)
ks.test(scale(data_uclm$CP34), "pnorm")
lillie.test(data_uclm$CP34)

qqnorm(data_uclm$CP34, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$CP34)

# non-parametric test
wilcox.test(data_col$CP34,
            data_uclm$CP34,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 9036, p-value = 2.749e-07
# result of the non-parametric test
# there are statistically significant differences

# CP35 conoces: Gamificación ----

data %>%
  select(Paises,CP35)%>%
  na.omit()%>%
  summarise(n = length(CP35),
            media = mean(CP35),
            sd = sd(CP35),
            mediana = median(CP35),
            min = min(CP35),
            max = max(CP35))

data %>%
  select(Paises,CP35)%>%
  na.omit()%>%
  group_by(Paises)%>%
  summarise(n = length(CP35),
            media = mean(CP35),
            sd = sd(CP35),
            mediana = median(CP35),
            min = min(CP35),
            max = max(CP35))

# statistical inference

shapiro.test(data$CP35)
ks.test(scale(data$CP35), "pnorm")
lillie.test(data$CP35)

qqnorm(data$CP35, las=1, pch=18, 
       main="CP35 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$CP35)

shapiro.test(data_col$CP35)
ks.test(scale(data_col$CP35), "pnorm")
lillie.test(data_col$CP35)

qqnorm(data_col$CP35, las=1, pch=18, 
       main="CP35 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$CP35)

shapiro.test(data_uclm$CP35)
ks.test(scale(data_uclm$CP35), "pnorm")
lillie.test(data_uclm$CP35)

qqnorm(data_uclm$CP35, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$CP35)

# non-parametric test
wilcox.test(data_col$CP35,
            data_uclm$CP35,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 10410, p-value = 0.0005656
# result of the non-parametric test
# there are statistically significant differences

# UP1 usas: Editores de texto (Word, OpenOffice Writer, LibreOffice Writer,…) ----

data %>%
  select(Paises,UP1)%>%
  na.omit()%>%
  summarise(n = length(UP1),
            media = mean(UP1),
            sd = sd(UP1),
            mediana = median(UP1),
            min = min(UP1),
            max = max(UP1))


data %>%
  select(Paises,UP1)%>%
  na.omit()%>%
  group_by(Paises)%>%
  summarise(n = length(UP1),
            media = mean(UP1),
            sd = sd(UP1),
            mediana = median(UP1),
            min = min(UP1),
            max = max(UP1))


# statistical inference

shapiro.test(data$UP1)
ks.test(scale(data$UP1), "pnorm")
lillie.test(data$UP1)

qqnorm(data$UP1, las=1, pch=18, 
       main="UP1 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$UP1)

shapiro.test(data_col$UP1)
ks.test(scale(data_col$UP1), "pnorm")
lillie.test(data_col$UP1)

qqnorm(data_col$UP1, las=1, pch=18, 
       main="UP1 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$UP1)

shapiro.test(data_uclm$UP1)
ks.test(scale(data_uclm$UP1), "pnorm")
lillie.test(data_uclm$UP1)

qqnorm(data_uclm$UP1, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$UP1)

# non-parametric test
wilcox.test(data_col$UP1,
            data_uclm$UP1,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 9523, p-value = 1.909e-06
# result of the non-parametric test
# there are statistically significant differences

# UP2 usas: Hojas de cálculo (Excel, Google Sheets, LibreOffice Calc,…) ----

data %>%
  select(Paises,UP2)%>%
  na.omit()%>%
  summarise(n = length(UP2),
            media = mean(UP2),
            sd = sd(UP2),
            mediana = median(UP2),
            min = min(UP2),
            max = max(UP2))

data %>%
  select(Paises,UP2)%>%
  na.omit()%>%
  group_by(Paises)%>%
  summarise(n = length(UP2),
            media = mean(UP2),
            sd = sd(UP2),
            mediana = median(UP2),
            min = min(UP2),
            max = max(UP2))

# statistical inference

shapiro.test(data$UP2)
ks.test(scale(data$UP2), "pnorm")
lillie.test(data$UP2)

qqnorm(data$UP2, las=1, pch=18, 
       main="UP2 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$UP2)

shapiro.test(data_col$UP2)
ks.test(scale(data_col$UP2), "pnorm")
lillie.test(data_col$UP2)

qqnorm(data_col$UP2, las=1, pch=18, 
       main="UP2 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$UP2)

shapiro.test(data_uclm$UP2)
ks.test(scale(data_uclm$UP2), "pnorm")
lillie.test(data_uclm$UP2)

qqnorm(data_uclm$UP2, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$UP2)

# non-parametric test
wilcox.test(data_col$UP2,
            data_uclm$UP2,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 19232, p-value = 2.356e-13
# result of the non-parametric test
# there are statistically significant differences

# UP3 usas: Bases de datos (Access, Base, nuBuilder,…) ----

data %>%
  select(Paises,UP3)%>%
  na.omit()%>%
  summarise(n = length(UP3),
            media = mean(UP3),
            sd = sd(UP3),
            mediana = median(UP3),
            min = min(UP3),
            max = max(UP3))

data %>%
  select(Paises,UP3)%>%
  na.omit()%>%
  group_by(Paises)%>%
  summarise(n = length(UP3),
            media = mean(UP3),
            sd = sd(UP3),
            mediana = median(UP3),
            min = min(UP3),
            max = max(UP3))

# statistical inference

shapiro.test(data$UP3)
ks.test(scale(data$UP3), "pnorm")
lillie.test(data$UP3)

qqnorm(data$UP3, las=1, pch=18, 
       main="UP3 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$UP3)

shapiro.test(data_col$UP3)
ks.test(scale(data_col$UP3), "pnorm")
lillie.test(data_col$UP3)

qqnorm(data_col$UP3, las=1, pch=18, 
       main="UP3 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$UP3)

shapiro.test(data_uclm$UP3)
ks.test(scale(data_uclm$UP3), "pnorm")
lillie.test(data_uclm$UP3)

qqnorm(data_uclm$UP3, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$UP3)

# non-parametric test
wilcox.test(data_col$UP3,
            data_uclm$UP3,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 17500, p-value = 2.529e-07
# result of the non-parametric test
# there are statistically significant differences

# UP4 usas: Creadores de presentaciones visuales (PowerPoint, Google Slides,…) ----

data %>%
  select(Paises,UP4)%>%
  na.omit()%>%
  summarise(n = length(UP4),
            media = mean(UP4),
            sd = sd(UP4),
            mediana = median(UP4),
            min = min(UP4),
            max = max(UP4))

data %>%
  select(Paises,UP4)%>%
  na.omit()%>%
  group_by(Paises)%>%
  summarise(n = length(UP4),
            media = mean(UP4),
            sd = sd(UP4),
            mediana = median(UP4),
            min = min(UP4),
            max = max(UP4))

# statistical inference

shapiro.test(data$UP4)
ks.test(scale(data$UP4), "pnorm")
lillie.test(data$UP4)

qqnorm(data$UP4, las=1, pch=18, 
       main="UP4 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$UP4)

shapiro.test(data_col$UP4)
ks.test(scale(data_col$UP4), "pnorm")
lillie.test(data_col$UP4)

qqnorm(data_col$UP4, las=1, pch=18, 
       main="UP4 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$UP4)

shapiro.test(data_uclm$UP4)
ks.test(scale(data_uclm$UP4), "pnorm")
lillie.test(data_uclm$UP4)

qqnorm(data_uclm$UP4, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$UP4)

# non-parametric test
wilcox.test(data_col$UP4,
            data_uclm$UP4,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 9319, p-value = 3.064e-07
# result of the non-parametric test
# there are statistically significant differences

# UP5 usas: Programas de edición de imagen (Paint, Adobe PhotoShop, Gimp,…) ----

data %>%
  select(Paises,UP5)%>%
  na.omit()%>%
  summarise(n = length(UP5),
            media = mean(UP5),
            sd = sd(UP5),
            mediana = median(UP5),
            min = min(UP5),
            max = max(UP5))

data %>%
  select(Paises,UP5)%>%
  na.omit()%>%
  group_by(Paises)%>%
  summarise(n = length(UP5),
            media = mean(UP5),
            sd = sd(UP5),
            mediana = median(UP5),
            min = min(UP5),
            max = max(UP5))

# statistical inference

shapiro.test(data$UP5)
ks.test(scale(data$UP5), "pnorm")
lillie.test(data$UP5)

qqnorm(data$UP5, las=1, pch=18, 
       main="UP5 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$UP5)

shapiro.test(data_col$UP5)
ks.test(scale(data_col$UP5), "pnorm")
lillie.test(data_col$UP5)

qqnorm(data_col$UP5, las=1, pch=18, 
       main="UP5 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$UP5)

shapiro.test(data_uclm$UP5)
ks.test(scale(data_uclm$UP5), "pnorm")
lillie.test(data_uclm$UP5)

qqnorm(data_uclm$UP5, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$UP5)

# non-parametric test
wilcox.test(data_col$UP5,
            data_uclm$UP5,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 16130, p-value = 0.0002312
# result of the non-parametric test
# there are statistically significant differences



# UP6 usas: Programas de edición de audio (Audacity, Ocenaudio, Reaper, …) ----

data %>%
  select(Paises,UP6)%>%
  na.omit()%>%
  summarise(n = length(UP6),
            media = mean(UP6),
            sd = sd(UP6),
            mediana = median(UP6),
            min = min(UP6),
            max = max(UP6))

data %>%
  select(Paises,UP6)%>%
  na.omit()%>%
  group_by(Paises)%>%
  summarise(n = length(UP6),
            media = mean(UP6),
            sd = sd(UP6),
            mediana = median(UP6),
            min = min(UP6),
            max = max(UP6))

# statistical inference

shapiro.test(data$UP6)
ks.test(scale(data$UP6), "pnorm")
lillie.test(data$UP6)

qqnorm(data$UP6, las=1, pch=18, 
       main="UP6 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$UP6)

shapiro.test(data_col$UP6)
ks.test(scale(data_col$UP6), "pnorm")
lillie.test(data_col$UP6)

qqnorm(data_col$UP6, las=1, pch=18, 
       main="UP6 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$UP6)

shapiro.test(data_uclm$UP6)
ks.test(scale(data_uclm$UP6), "pnorm")
lillie.test(data_uclm$UP6)

qqnorm(data_uclm$UP6, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$UP6)

# non-parametric test
wilcox.test(data_col$UP6,
            data_uclm$UP6,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 16752, p-value = 2.38e-05
# result of the non-parametric test
# there are statistically significant differences

# UP7 usas: Programas de edición de video (Kdenlive, Shotcut, Pinnacle, Sony Vegas,…) ----

data %>%
  select(Paises,UP7)%>%
  na.omit()%>%
  summarise(n = length(UP7),
            media = mean(UP7),
            sd = sd(UP7),
            mediana = median(UP7),
            min = min(UP7),
            max = max(UP7))

data %>%
  select(Paises,UP7)%>%
  na.omit()%>%
  group_by(Paises)%>%
  summarise(n = length(UP7),
            media = mean(UP7),
            sd = sd(UP7),
            mediana = median(UP7),
            min = min(UP7),
            max = max(UP7))

# statistical inference

shapiro.test(data$UP7)
ks.test(scale(data$UP7), "pnorm")
lillie.test(data$UP7)

qqnorm(data$UP7, las=1, pch=18, 
       main="UP7 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$UP7)

shapiro.test(data_col$UP7)
ks.test(scale(data_col$UP7), "pnorm")
lillie.test(data_col$UP7)

qqnorm(data_col$UP7, las=1, pch=18, 
       main="UP7 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$UP7)

shapiro.test(data_uclm$UP7)
ks.test(scale(data_uclm$UP7), "pnorm")
lillie.test(data_uclm$UP7)

qqnorm(data_uclm$UP7, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$UP7)

# non-parametric test
wilcox.test(data_col$UP7,
            data_uclm$UP7,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 15123, p-value = 0.02471
# result of the non-parametric test
# there are statistically significant differences

# UP8 usas: Videoconferencia ----

data %>%
  select(Paises,UP8)%>%
  na.omit()%>%
  summarise(n = length(UP8),
            media = mean(UP8),
            sd = sd(UP8),
            mediana = median(UP8),
            min = min(UP8),
            max = max(UP8))

data %>%
  select(Paises,UP8)%>%
  na.omit()%>%
  group_by(Paises)%>%
  summarise(n = length(UP8),
            media = mean(UP8),
            sd = sd(UP8),
            mediana = median(UP8),
            min = min(UP8),
            max = max(UP8))

# statistical inference

shapiro.test(data$UP8)
ks.test(scale(data$UP8), "pnorm")
lillie.test(data$UP8)

qqnorm(data$UP8, las=1, pch=18, 
       main="UP8 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$UP8)

shapiro.test(data_col$UP8)
ks.test(scale(data_col$UP8), "pnorm")
lillie.test(data_col$UP8)

qqnorm(data_col$UP8, las=1, pch=18, 
       main="UP8 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$UP8)

shapiro.test(data_uclm$UP8)
ks.test(scale(data_uclm$UP8), "pnorm")
lillie.test(data_uclm$UP8)

qqnorm(data_uclm$UP8, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$UP8)

# non-parametric test
wilcox.test(data_col$UP8,
            data_uclm$UP8,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 10926, p-value = 0.005603
# result of the non-parametric test
# there are statistically significant differences

# UP9 usas: Listas de distribución ----

data %>%
  select(Paises,UP9)%>%
  na.omit()%>%
  summarise(n = length(UP9),
            media = mean(UP9),
            sd = sd(UP9),
            mediana = median(UP9),
            min = min(UP9),
            max = max(UP9))

data %>%
  select(Paises,UP9)%>%
  na.omit()%>%
  group_by(Paises)%>%
  summarise(n = length(UP9),
            media = mean(UP9),
            sd = sd(UP9),
            mediana = median(UP9),
            min = min(UP9),
            max = max(UP9))

# statistical inference

shapiro.test(data$UP9)
ks.test(scale(data$UP9), "pnorm")
lillie.test(data$UP9)

qqnorm(data$UP9, las=1, pch=18, 
       main="UP9 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$UP9)

shapiro.test(data_col$UP9)
ks.test(scale(data_col$UP9), "pnorm")
lillie.test(data_col$UP9)

qqnorm(data_col$UP9, las=1, pch=18, 
       main="UP9 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$UP9)

shapiro.test(data_uclm$UP9)
ks.test(scale(data_uclm$UP9), "pnorm")
lillie.test(data_uclm$UP9)

qqnorm(data_uclm$UP9, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$UP9)

# non-parametric test
wilcox.test(data_col$UP9,
            data_uclm$UP9,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 16578, p-value = 1.817e-05
# result of the non-parametric test
# there are statistically significant differences

# UP10 usas: Foros ----

data %>%
  select(Paises,UP10)%>%
  na.omit()%>%
  summarise(n = length(UP10),
            media = mean(UP10),
            sd = sd(UP10),
            mediana = median(UP10),
            min = min(UP10),
            max = max(UP10))

data %>%
  select(Paises,UP10)%>%
  na.omit()%>%
  group_by(Paises)%>%
  summarise(n = length(UP10),
            media = mean(UP10),
            sd = sd(UP10),
            mediana = median(UP10),
            min = min(UP10),
            max = max(UP10))

# statistical inference

shapiro.test(data$UP10)
ks.test(scale(data$UP10), "pnorm")
lillie.test(data$UP10)

qqnorm(data$UP10, las=1, pch=18, 
       main="UP10 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$UP10)

shapiro.test(data_col$UP10)
ks.test(scale(data_col$UP10), "pnorm")
lillie.test(data_col$UP10)

qqnorm(data_col$UP10, las=1, pch=18, 
       main="UP10 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$UP10)

shapiro.test(data_uclm$UP10)
ks.test(scale(data_uclm$UP10), "pnorm")
lillie.test(data_uclm$UP10)

qqnorm(data_uclm$UP10, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$UP10)

# non-parametric test
wilcox.test(data_col$UP10,
            data_uclm$UP10,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 17824, p-value = 1.959e-08
# result of the non-parametric test
# there are statistically significant differences

# UP11 usas: Mensajería instantánea / Chat ----

data %>%
  select(Paises,UP11)%>%
  na.omit()%>%
  summarise(n = length(UP11),
            media = mean(UP11),
            sd = sd(UP11),
            mediana = median(UP11),
            min = min(UP11),
            max = max(UP11))

data %>%
  select(Paises,UP11)%>%
  na.omit()%>%
  group_by(Paises)%>%
  summarise(n = length(UP11),
            media = mean(UP11),
            sd = sd(UP11),
            mediana = median(UP11),
            min = min(UP11),
            max = max(UP11))

# statistical inference

shapiro.test(data$UP11)
ks.test(scale(data$UP11), "pnorm")
lillie.test(data$UP11)

qqnorm(data$UP11, las=1, pch=18, 
       main="UP11 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$UP11)

shapiro.test(data_col$UP11)
ks.test(scale(data_col$UP11), "pnorm")
lillie.test(data_col$UP11)

qqnorm(data_col$UP11, las=1, pch=18, 
       main="UP11 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$UP11)

shapiro.test(data_uclm$UP11)
ks.test(scale(data_uclm$UP11), "pnorm")
lillie.test(data_uclm$UP11)

qqnorm(data_uclm$UP11, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$UP11)

# non-parametric test
wilcox.test(data_col$UP11,
            data_uclm$UP11,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 8493.5, p-value = 8.505e-10
# result of the non-parametric test
# there are statistically significant differences

# UP12 usas: Redes sociales (Facebook, twitter, Instagram,…) ----

data %>%
  select(Paises,UP12)%>%
  na.omit()%>%
  summarise(n = length(UP12),
            media = mean(UP12),
            sd = sd(UP12),
            mediana = median(UP12),
            min = min(UP12),
            max = max(UP12))

data %>%
  select(Paises,UP12)%>%
  na.omit()%>%
  group_by(Paises)%>%
  summarise(n = length(UP12),
            media = mean(UP12),
            sd = sd(UP12),
            mediana = median(UP12),
            min = min(UP12),
            max = max(UP12))

# statistical inference

shapiro.test(data$UP12)
ks.test(scale(data$UP12), "pnorm")
lillie.test(data$UP12)

qqnorm(data$UP12, las=1, pch=18, 
       main="UP12 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$UP12)

shapiro.test(data_col$UP12)
ks.test(scale(data_col$UP12), "pnorm")
lillie.test(data_col$UP12)

qqnorm(data_col$UP12, las=1, pch=18, 
       main="UP12 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$UP12)

shapiro.test(data_uclm$UP12)
ks.test(scale(data_uclm$UP12), "pnorm")
lillie.test(data_uclm$UP12)

qqnorm(data_uclm$UP12, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$UP12)

# non-parametric test
wilcox.test(data_col$UP12,
            data_uclm$UP12,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 8944, p-value = 4.848e-09
# result of the non-parametric test
# there are statistically significant differences

# UP13 usas: Herramientas de trabajo colaborativo (blogs, wikis,…) ----

data %>%
  select(Paises,UP13)%>%
  na.omit()%>%
  summarise(n = length(UP13),
            media = mean(UP13),
            sd = sd(UP13),
            mediana = median(UP13),
            min = min(UP13),
            max = max(UP13))

data %>%
  select(Paises,UP13)%>%
  na.omit()%>%
  group_by(Paises)%>%
  summarise(n = length(UP13),
            media = mean(UP13),
            sd = sd(UP13),
            mediana = median(UP13),
            min = min(UP13),
            max = max(UP13))

# statistical inference

shapiro.test(data$UP13)
ks.test(scale(data$UP13), "pnorm")
lillie.test(data$UP13)

qqnorm(data$UP13, las=1, pch=18, 
       main="UP13 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$UP13)

shapiro.test(data_col$UP13)
ks.test(scale(data_col$UP13), "pnorm")
lillie.test(data_col$UP13)

qqnorm(data_col$UP13, las=1, pch=18, 
       main="UP13 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$UP13)

shapiro.test(data_uclm$UP13)
ks.test(scale(data_uclm$UP13), "pnorm")
lillie.test(data_uclm$UP13)

qqnorm(data_uclm$UP13, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$UP13)

# non-parametric test
wilcox.test(data_col$UP13,
            data_uclm$UP13,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 12756, p-value = 0.5334
# result of the non-parametric test
# there are statistically significant differences

# UP14 usas: Herramientas de intercambio de archivos (Emule, Torrents,…) ----

data %>%
  select(Paises,UP14)%>%
  na.omit()%>%
  summarise(n = length(UP14),
            media = mean(UP14),
            sd = sd(UP14),
            mediana = median(UP14),
            min = min(UP14),
            max = max(UP14))

data %>%
  select(Paises,UP14)%>%
  na.omit()%>%
  group_by(Paises)%>%
  summarise(n = length(UP14),
            media = mean(UP14),
            sd = sd(UP14),
            mediana = median(UP14),
            min = min(UP14),
            max = max(UP14))

# statistical inference

shapiro.test(data$UP14)
ks.test(scale(data$UP14), "pnorm")
lillie.test(data$UP14)

qqnorm(data$UP14, las=1, pch=18, 
       main="UP14 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$UP14)

shapiro.test(data_col$UP14)
ks.test(scale(data_col$UP14), "pnorm")
lillie.test(data_col$UP14)

qqnorm(data_col$UP14, las=1, pch=18, 
       main="UP14 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$UP14)

shapiro.test(data_uclm$UP14)
ks.test(scale(data_uclm$UP14), "pnorm")
lillie.test(data_uclm$UP14)

qqnorm(data_uclm$UP14, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$UP14)

# non-parametric test
wilcox.test(data_col$UP14,
            data_uclm$UP14,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 14380, p-value = 0.1799
# result of the non-parametric test
# there are statistically significant differences

# UP15 usas: Herramientas de búsqueda de información en la red (Google,…) ----

data %>%
  select(Paises,UP15)%>%
  na.omit()%>%
  summarise(n = length(UP15),
            media = mean(UP15),
            sd = sd(UP15),
            mediana = median(UP15),
            min = min(UP15),
            max = max(UP15))

data %>%
  select(Paises,UP15)%>%
  na.omit()%>%
  group_by(Paises)%>%
  summarise(n = length(UP15),
            media = mean(UP15),
            sd = sd(UP15),
            mediana = median(UP15),
            min = min(UP15),
            max = max(UP15))

# statistical inference

shapiro.test(data$UP15)
ks.test(scale(data$UP15), "pnorm")
lillie.test(data$UP15)

qqnorm(data$UP15, las=1, pch=18, 
       main="UP15 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$UP15)

shapiro.test(data_col$UP15)
ks.test(scale(data_col$UP15), "pnorm")
lillie.test(data_col$UP15)

qqnorm(data_col$UP15, las=1, pch=18, 
       main="UP15 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$UP15)

shapiro.test(data_uclm$UP15)
ks.test(scale(data_uclm$UP15), "pnorm")
lillie.test(data_uclm$UP15)

qqnorm(data_uclm$UP15, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$UP15)

# non-parametric test
wilcox.test(data_col$UP15,
            data_uclm$UP15,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 7003, p-value = 5.949e-16
# result of the non-parametric test
# there are statistically significant differences

# UP16 usas: Traductores on-line  ----

data %>%
  select(Paises,UP16)%>%
  na.omit()%>%
  summarise(n = length(UP16),
            media = mean(UP16),
            sd = sd(UP16),
            mediana = median(UP16),
            min = min(UP16),
            max = max(UP16))

data %>%
  select(Paises,UP16)%>%
  na.omit()%>%
  group_by(Paises)%>%
  summarise(n = length(UP16),
            media = mean(UP16),
            sd = sd(UP16),
            mediana = median(UP16),
            min = min(UP16),
            max = max(UP16))

# statistical inference

shapiro.test(data$UP16)
ks.test(scale(data$UP16), "pnorm")
lillie.test(data$UP16)

qqnorm(data$UP16, las=1, pch=18, 
       main="UP16 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$UP16)

shapiro.test(data_col$UP16)
ks.test(scale(data_col$UP16), "pnorm")
lillie.test(data_col$UP16)

qqnorm(data_col$UP16, las=1, pch=18, 
       main="UP16 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$UP16)

shapiro.test(data_uclm$UP16)
ks.test(scale(data_uclm$UP16), "pnorm")
lillie.test(data_uclm$UP16)

qqnorm(data_uclm$UP16, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$UP16)

# non-parametric test
wilcox.test(data_col$UP16,
            data_uclm$UP16,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 10941, p-value = 0.005699
# result of the non-parametric test
# there are statistically significant differences

# UP17 usas: Herramientas de publicación de imágenes en red (Flickr, Google Fotos, Imgur …) ----

data %>%
  select(Paises,UP17)%>%
  na.omit()%>%
  summarise(n = length(UP17),
            media = mean(UP17),
            sd = sd(UP17),
            mediana = median(UP17),
            min = min(UP17),
            max = max(UP17))

data %>%
  select(Paises,UP17)%>%
  na.omit()%>%
  group_by(Paises)%>%
  summarise(n = length(UP17),
            media = mean(UP17),
            sd = sd(UP17),
            mediana = median(UP17),
            min = min(UP17),
            max = max(UP17))

# statistical inference

shapiro.test(data$UP17)
ks.test(scale(data$UP17), "pnorm")
lillie.test(data$UP17)

qqnorm(data$UP17, las=1, pch=18, 
       main="UP17 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$UP17)

shapiro.test(data_col$UP17)
ks.test(scale(data_col$UP17), "pnorm")
lillie.test(data_col$UP17)

qqnorm(data_col$UP17, las=1, pch=18, 
       main="UP17 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$UP17)

shapiro.test(data_uclm$UP17)
ks.test(scale(data_uclm$UP17), "pnorm")
lillie.test(data_uclm$UP17)

qqnorm(data_uclm$UP17, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$UP17)

# non-parametric test
wilcox.test(data_col$UP17,
            data_uclm$UP17,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 12982, p-value = 0.7285
# result of the non-parametric test
# there are statistically significant differences

# UP18 usas: Presentaciones interactivas en red (prezi, SlideShare, Genially,…) ----

data %>%
  select(Paises,UP18)%>%
  na.omit()%>%
  summarise(n = length(UP18),
            media = mean(UP18),
            sd = sd(UP18),
            mediana = median(UP18),
            min = min(UP18),
            max = max(UP18))

data %>%
  select(Paises,UP18)%>%
  na.omit()%>%
  group_by(Paises)%>%
  summarise(n = length(UP18),
            media = mean(UP18),
            sd = sd(UP18),
            mediana = median(UP18),
            min = min(UP18),
            max = max(UP18))

# statistical inference

shapiro.test(data$UP18)
ks.test(scale(data$UP18), "pnorm")
lillie.test(data$UP18)

qqnorm(data$UP18, las=1, pch=18, 
       main="UP18 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$UP18)

shapiro.test(data_col$UP18)
ks.test(scale(data_col$UP18), "pnorm")
lillie.test(data_col$UP18)

qqnorm(data_col$UP18, las=1, pch=18, 
       main="UP18 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$UP18)

shapiro.test(data_uclm$UP18)
ks.test(scale(data_uclm$UP18), "pnorm")
lillie.test(data_uclm$UP18)

qqnorm(data_uclm$UP18, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$UP18)

# non-parametric test
wilcox.test(data_col$UP18,
            data_uclm$UP18,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 11670, p-value = 0.04287
# result of the non-parametric test
# there are statistically significant differences

# UP19 usas: Marcadores sociales (Diigo, Pocket,…) ----

data %>%
  select(Paises,UP19)%>%
  na.omit()%>%
  summarise(n = length(UP19),
            media = mean(UP19),
            sd = sd(UP19),
            mediana = median(UP19),
            min = min(UP19),
            max = max(UP19))

data %>%
  select(Paises,UP19)%>%
  na.omit()%>%
  group_by(Paises)%>%
  summarise(n = length(UP19),
            media = mean(UP19),
            sd = sd(UP19),
            mediana = median(UP19),
            min = min(UP19),
            max = max(UP19))

# statistical inference

shapiro.test(data$UP19)
ks.test(scale(data$UP19), "pnorm")
lillie.test(data$UP19)

qqnorm(data$UP19, las=1, pch=18, 
       main="UP19 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$UP19)

shapiro.test(data_col$UP19)
ks.test(scale(data_col$UP19), "pnorm")
lillie.test(data_col$UP19)

qqnorm(data_col$UP19, las=1, pch=18, 
       main="UP19 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$UP19)

shapiro.test(data_uclm$UP19)
ks.test(scale(data_uclm$UP19), "pnorm")
lillie.test(data_uclm$UP19)

qqnorm(data_uclm$UP19, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$UP19)

# non-parametric test
wilcox.test(data_col$UP19,
            data_uclm$UP19,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 18697, p-value = 1.649e-12
# result of the non-parametric test
# there are statistically significant differences

# UP20 usas: Lectores de RSS (Feedly, NewsTab,…) ----

data %>%
  select(Paises,UP20)%>%
  na.omit()%>%
  summarise(n = length(UP20),
            media = mean(UP20),
            sd = sd(UP20),
            mediana = median(UP20),
            min = min(UP20),
            max = max(UP20))

data %>%
  select(Paises,UP20)%>%
  na.omit()%>%
  group_by(Paises)%>%
  summarise(n = length(UP20),
            media = mean(UP20),
            sd = sd(UP20),
            mediana = median(UP20),
            min = min(UP20),
            max = max(UP20))

# statistical inference

shapiro.test(data$UP20)
ks.test(scale(data$UP20), "pnorm")
lillie.test(data$UP20)

qqnorm(data$UP20, las=1, pch=18, 
       main="UP20 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$UP20)

shapiro.test(data_col$UP20)
ks.test(scale(data_col$UP20), "pnorm")
lillie.test(data_col$UP20)

qqnorm(data_col$UP20, las=1, pch=18, 
       main="UP20 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$UP20)

shapiro.test(data_uclm$UP20)
ks.test(scale(data_uclm$UP20), "pnorm")
lillie.test(data_uclm$UP20)

qqnorm(data_uclm$UP20, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$UP20)

# non-parametric test
wilcox.test(data_col$UP20,
            data_uclm$UP20,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 18358, p-value = 7.204e-12
# result of the non-parametric test
# there are statistically significant differences

# UP21 usas: LPáginas de inicio personalizadas (Start.me, Eversync, Protopage,…) ----

data %>%
  select(Paises,UP21)%>%
  na.omit()%>%
  summarise(n = length(UP21),
            media = mean(UP21),
            sd = sd(UP21),
            mediana = median(UP21),
            min = min(UP21),
            max = max(UP21))

data %>%
  select(Paises,UP21)%>%
  na.omit()%>%
  group_by(Paises)%>%
  summarise(n = length(UP21),
            media = mean(UP21),
            sd = sd(UP21),
            mediana = median(UP21),
            min = min(UP21),
            max = max(UP21))

# statistical inference

shapiro.test(data$UP21)
ks.test(scale(data$UP21), "pnorm")
lillie.test(data$UP21)

qqnorm(data$UP21, las=1, pch=18, 
       main="UP21 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$UP21)

shapiro.test(data_col$UP21)
ks.test(scale(data_col$UP21), "pnorm")
lillie.test(data_col$UP21)

qqnorm(data_col$UP21, las=1, pch=18, 
       main="UP21 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$UP21)

shapiro.test(data_uclm$UP21)
ks.test(scale(data_uclm$UP21), "pnorm")
lillie.test(data_uclm$UP21)

qqnorm(data_uclm$UP21, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$UP21)

# non-parametric test
wilcox.test(data_col$UP21,
            data_uclm$UP21,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 17814, p-value = 2.533e-10
# result of the non-parametric test
# there are statistically significant differences

# UP22 usas: Live streaming (Twitch, Livestream, …) ----

data %>%
  select(Paises,UP22)%>%
  na.omit()%>%
  summarise(n = length(UP22),
            media = mean(UP22),
            sd = sd(UP22),
            mediana = median(UP22),
            min = min(UP22),
            max = max(UP22))

data %>%
  select(Paises,UP22)%>%
  na.omit()%>%
  group_by(Paises)%>%
  summarise(n = length(UP22),
            media = mean(UP22),
            sd = sd(UP22),
            mediana = median(UP22),
            min = min(UP22),
            max = max(UP22))

# statistical inference

shapiro.test(data$UP22)
ks.test(scale(data$UP22), "pnorm")
lillie.test(data$UP22)

qqnorm(data$UP22, las=1, pch=18, 
       main="UP22 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$UP22)

shapiro.test(data_col$UP22)
ks.test(scale(data_col$UP22), "pnorm")
lillie.test(data_col$UP22)

qqnorm(data_col$UP22, las=1, pch=18, 
       main="UP22 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$UP22)

shapiro.test(data_uclm$UP22)
ks.test(scale(data_uclm$UP22), "pnorm")
lillie.test(data_uclm$UP22)

qqnorm(data_uclm$UP22, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$UP22)

# non-parametric test
wilcox.test(data_col$UP22,
            data_uclm$UP22,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 14150, p-value = 0.2857
# result of the non-parametric test
# there are statistically significant differences

# UP23 usas: Editores de páginas web (Wix, Jimdo, WordPress,..) ----

data %>%
  select(Paises,UP23)%>%
  na.omit()%>%
  summarise(n = length(UP23),
            media = mean(UP23),
            sd = sd(UP23),
            mediana = median(UP23),
            min = min(UP23),
            max = max(UP23))

data %>%
  select(Paises,UP23)%>%
  na.omit()%>%
  group_by(Paises)%>%
  summarise(n = length(UP23),
            media = mean(UP23),
            sd = sd(UP23),
            mediana = median(UP23),
            min = min(UP23),
            max = max(UP23))

# statistical inference

shapiro.test(data$UP23)
ks.test(scale(data$UP23), "pnorm")
lillie.test(data$UP23)

qqnorm(data$UP23, las=1, pch=18, 
       main="UP23 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$UP23)

shapiro.test(data_col$UP23)
ks.test(scale(data_col$UP23), "pnorm")
lillie.test(data_col$UP23)

qqnorm(data_col$UP23, las=1, pch=18, 
       main="UP23 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$UP23)

shapiro.test(data_uclm$UP23)
ks.test(scale(data_uclm$UP23), "pnorm")
lillie.test(data_uclm$UP23)

qqnorm(data_uclm$UP23, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$UP23)

# non-parametric test
wilcox.test(data_col$UP23,
            data_uclm$UP23,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 15645, p-value = 0.005489
# result of the non-parametric test
# there are statistically significant differences

# UP24 usas: Bibliotecas y enciclopedias virtuales ----

data %>%
  select(Paises,UP24)%>%
  na.omit()%>%
  summarise(n = length(UP24),
            media = mean(UP24),
            sd = sd(UP24),
            mediana = median(UP24),
            min = min(UP24),
            max = max(UP24))

data %>%
  select(Paises,UP24)%>%
  na.omit()%>%
  group_by(Paises)%>%
  summarise(n = length(UP24),
            media = mean(UP24),
            sd = sd(UP24),
            mediana = median(UP24),
            min = min(UP24),
            max = max(UP24))

# statistical inference

shapiro.test(data$UP24)
ks.test(scale(data$UP24), "pnorm")
lillie.test(data$UP24)

qqnorm(data$UP24, las=1, pch=18, 
       main="UP24 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$UP24)

shapiro.test(data_col$UP24)
ks.test(scale(data_col$UP24), "pnorm")
lillie.test(data_col$UP24)

qqnorm(data_col$UP24, las=1, pch=18, 
       main="UP24 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$UP24)

shapiro.test(data_uclm$UP24)
ks.test(scale(data_uclm$UP24), "pnorm")
lillie.test(data_uclm$UP24)

qqnorm(data_uclm$UP24, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$UP24)

# non-parametric test
wilcox.test(data_col$UP24,
            data_uclm$UP24,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 12834, p-value = 0.5311
# result of the non-parametric test
# there are statistically significant differences

# UP25 usas: Cartografía digital (google maps, google earth,…) ----

data %>%
  select(Paises,UP25)%>%
  na.omit()%>%
  summarise(n = length(UP25),
            media = mean(UP25),
            sd = sd(UP25),
            mediana = median(UP25),
            min = min(UP25),
            max = max(UP25))

data %>%
  select(Paises,UP25)%>%
  na.omit()%>%
  group_by(Paises)%>%
  summarise(n = length(UP25),
            media = mean(UP25),
            sd = sd(UP25),
            mediana = median(UP25),
            min = min(UP25),
            max = max(UP25))

# statistical inference

shapiro.test(data$UP25)
ks.test(scale(data$UP25), "pnorm")
lillie.test(data$UP25)

qqnorm(data$UP25, las=1, pch=18, 
       main="UP25 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$UP25)

shapiro.test(data_col$UP25)
ks.test(scale(data_col$UP25), "pnorm")
lillie.test(data_col$UP25)

qqnorm(data_col$UP25, las=1, pch=18, 
       main="UP25 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$UP25)

shapiro.test(data_uclm$UP25)
ks.test(scale(data_uclm$UP25), "pnorm")
lillie.test(data_uclm$UP25)

qqnorm(data_uclm$UP25, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$UP25)

# non-parametric test
wilcox.test(data_col$UP25,
            data_uclm$UP25,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 9785.5, p-value = 2.452e-05
# result of the non-parametric test
# there are statistically significant differences

# UP26 usas: Entornos virtuales de aprendizaje (Moodle, Dokeos, Sakai, Canvas,…) ----

data %>%
  select(Paises,UP26)%>%
  na.omit()%>%
  summarise(n = length(UP26),
            media = mean(UP26),
            sd = sd(UP26),
            mediana = median(UP26),
            min = min(UP26),
            max = max(UP26))

data %>%
  select(Paises,UP26)%>%
  na.omit()%>%
  group_by(Paises)%>%
  summarise(n = length(UP26),
            media = mean(UP26),
            sd = sd(UP26),
            mediana = median(UP26),
            min = min(UP26),
            max = max(UP26))

# statistical inference

shapiro.test(data$UP26)
ks.test(scale(data$UP26), "pnorm")
lillie.test(data$UP26)

qqnorm(data$UP26, las=1, pch=18, 
       main="UP26 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$UP26)

shapiro.test(data_col$UP26)
ks.test(scale(data_col$UP26), "pnorm")
lillie.test(data_col$UP26)

qqnorm(data_col$UP26, las=1, pch=18, 
       main="UP26 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$UP26)

shapiro.test(data_uclm$UP26)
ks.test(scale(data_uclm$UP26), "pnorm")
lillie.test(data_uclm$UP26)

qqnorm(data_uclm$UP26, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$UP26)

# non-parametric test
wilcox.test(data_col$UP26,
            data_uclm$UP26,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 4712, p-value < 2.2e-16
# result of the non-parametric test
# there are statistically significant differences

# UP27 usas: Entornos personales de aprendizaje (Symbaloo, Netvibes,...) ----

data %>%
  select(Paises,UP27)%>%
  na.omit()%>%
  summarise(n = length(UP27),
            media = mean(UP27),
            sd = sd(UP27),
            mediana = median(UP27),
            min = min(UP27),
            max = max(UP27))

data %>%
  select(Paises,UP27)%>%
  na.omit()%>%
  group_by(Paises)%>%
  summarise(n = length(UP27),
            media = mean(UP27),
            sd = sd(UP27),
            mediana = median(UP27),
            min = min(UP27),
            max = max(UP27))

# statistical inference

shapiro.test(data$UP27)
ks.test(scale(data$UP27), "pnorm")
lillie.test(data$UP27)

qqnorm(data$UP27, las=1, pch=18, 
       main="UP27 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$UP27)

shapiro.test(data_col$UP27)
ks.test(scale(data_col$UP27), "pnorm")
lillie.test(data_col$UP27)

qqnorm(data_col$UP27, las=1, pch=18, 
       main="UP27 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$UP27)

shapiro.test(data_uclm$UP27)
ks.test(scale(data_uclm$UP27), "pnorm")
lillie.test(data_uclm$UP27)

qqnorm(data_uclm$UP27, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$UP27)

# non-parametric test
wilcox.test(data_col$UP27,
            data_uclm$UP27,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 18373, p-value = 6.03e-11
# result of the non-parametric test
# there are statistically significant differences

# UP28 usas: Alojamiento de archivos multiplataforma en la nube (Google drive, OneDrive, Dropbox,..) ----

data %>%
  select(Paises,UP28)%>%
  na.omit()%>%
  summarise(n = length(UP28),
            media = mean(UP28),
            sd = sd(UP28),
            mediana = median(UP28),
            min = min(UP28),
            max = max(UP28))

data %>%
  select(Paises,UP28)%>%
  na.omit()%>%
  group_by(Paises)%>%
  summarise(n = length(UP28),
            media = mean(UP28),
            sd = sd(UP28),
            mediana = median(UP28),
            min = min(UP28),
            max = max(UP28))

# statistical inference

shapiro.test(data$UP28)
ks.test(scale(data$UP28), "pnorm")
lillie.test(data$UP28)

qqnorm(data$UP28, las=1, pch=18, 
       main="UP28 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$UP28)

shapiro.test(data_col$UP28)
ks.test(scale(data_col$UP28), "pnorm")
lillie.test(data_col$UP28)

qqnorm(data_col$UP28, las=1, pch=18, 
       main="UP28 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$UP28)

shapiro.test(data_uclm$UP28)
ks.test(scale(data_uclm$UP28), "pnorm")
lillie.test(data_uclm$UP28)

qqnorm(data_uclm$UP28, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$UP28)

# non-parametric test
wilcox.test(data_col$UP28,
            data_uclm$UP28,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 7988, p-value = 8.845e-11
# result of the non-parametric test
# there are statistically significant differences

# UP29 usas: Plataformas educativas (Google classroom, BrainCert, Edmodo,…) ----

data %>%
  select(Paises,UP29)%>%
  na.omit()%>%
  summarise(n = length(UP29),
            media = mean(UP29),
            sd = sd(UP29),
            mediana = median(UP29),
            min = min(UP29),
            max = max(UP29))

data %>%
  select(Paises,UP29)%>%
  na.omit()%>%
  group_by(Paises)%>%
  summarise(n = length(UP29),
            media = mean(UP29),
            sd = sd(UP29),
            mediana = median(UP29),
            min = min(UP29),
            max = max(UP29))

# statistical inference

shapiro.test(data$UP29)
ks.test(scale(data$UP29), "pnorm")
lillie.test(data$UP29)

qqnorm(data$UP29, las=1, pch=18, 
       main="UP29 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$UP29)

shapiro.test(data_col$UP29)
ks.test(scale(data_col$UP29), "pnorm")
lillie.test(data_col$UP29)

qqnorm(data_col$UP29, las=1, pch=18, 
       main="UP29 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$UP29)

shapiro.test(data_uclm$UP29)
ks.test(scale(data_uclm$UP29), "pnorm")
lillie.test(data_uclm$UP29)

qqnorm(data_uclm$UP29, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$UP29)

# non-parametric test
wilcox.test(data_col$UP29,
            data_uclm$UP29,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 13807, p-value = 0.5874
# result of the non-parametric test
# there are statistically significant differences

# UP30 usas: Software educativo de autor (Cuadernia, EdiLIM, Ardora, Educaplay, eXeLearning, Malted, Jclic, Hot Potatoes,…) ----

data %>%
  select(Paises,UP30)%>%
  na.omit()%>%
  summarise(n = length(UP30),
            media = mean(UP30),
            sd = sd(UP30),
            mediana = median(UP30),
            min = min(UP30),
            max = max(UP30))

data %>%
  select(Paises,UP30)%>%
  na.omit()%>%
  group_by(Paises)%>%
  summarise(n = length(UP30),
            media = mean(UP30),
            sd = sd(UP30),
            mediana = median(UP30),
            min = min(UP30),
            max = max(UP30))

# statistical inference

shapiro.test(data$UP30)
ks.test(scale(data$UP30), "pnorm")
lillie.test(data$UP30)

qqnorm(data$UP30, las=1, pch=18, 
       main="UP30 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$UP30)

shapiro.test(data_col$UP30)
ks.test(scale(data_col$UP30), "pnorm")
lillie.test(data_col$UP30)

qqnorm(data_col$UP30, las=1, pch=18, 
       main="UP30 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$UP30)

shapiro.test(data_uclm$UP30)
ks.test(scale(data_uclm$UP30), "pnorm")
lillie.test(data_uclm$UP30)

qqnorm(data_uclm$UP30, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$UP30)

# non-parametric test
wilcox.test(data_col$UP30,
            data_uclm$UP30,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 17368, p-value = 1.109e-06
# result of the non-parametric test
# there are statistically significant differences

# UP31 usas: Actividades guiadas de búsqueda en Internet (Webquest,  caza del tesoro,…) ----

data %>%
  select(Paises,UP31)%>%
  na.omit()%>%
  summarise(n = length(UP31),
            media = mean(UP31),
            sd = sd(UP31),
            mediana = median(UP31),
            min = min(UP31),
            max = max(UP31))

data %>%
  select(Paises,UP31)%>%
  na.omit()%>%
  group_by(Paises)%>%
  summarise(n = length(UP31),
            media = mean(UP31),
            sd = sd(UP31),
            mediana = median(UP31),
            min = min(UP31),
            max = max(UP31))

# statistical inference

shapiro.test(data$UP31)
ks.test(scale(data$UP31), "pnorm")
lillie.test(data$UP31)

qqnorm(data$UP31, las=1, pch=18, 
       main="UP31 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$UP31)

shapiro.test(data_col$UP31)
ks.test(scale(data_col$UP31), "pnorm")
lillie.test(data_col$UP31)

qqnorm(data_col$UP31, las=1, pch=18, 
       main="UP31 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$UP31)

shapiro.test(data_uclm$UP31)
ks.test(scale(data_uclm$UP31), "pnorm")
lillie.test(data_uclm$UP31)

qqnorm(data_uclm$UP31, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$UP31)

# non-parametric test
wilcox.test(data_col$UP31,
            data_uclm$UP31,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 16668, p-value = 6e-05
# result of the non-parametric test
# there are statistically significant differences

# UP32 usas: Mapas conceptuales (cmaptool, mindomo, bubbl.us, MindMeister,…) ----

data %>%
  select(Paises,UP32)%>%
  na.omit()%>%
  summarise(n = length(UP32),
            media = mean(UP32),
            sd = sd(UP32),
            mediana = median(UP32),
            min = min(UP32),
            max = max(UP32))

data %>%
  select(Paises,UP32)%>%
  na.omit()%>%
  group_by(Paises)%>%
  summarise(n = length(UP32),
            media = mean(UP32),
            sd = sd(UP32),
            mediana = median(UP32),
            min = min(UP32),
            max = max(UP32))

# statistical inference

shapiro.test(data$UP32)
ks.test(scale(data$UP32), "pnorm")
lillie.test(data$UP32)

qqnorm(data$UP32, las=1, pch=18, 
       main="UP32 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$UP32)

shapiro.test(data_col$UP32)
ks.test(scale(data_col$UP32), "pnorm")
lillie.test(data_col$UP32)

qqnorm(data_col$UP32, las=1, pch=18, 
       main="UP32 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$UP32)

shapiro.test(data_uclm$UP32)
ks.test(scale(data_uclm$UP32), "pnorm")
lillie.test(data_uclm$UP32)

qqnorm(data_uclm$UP32, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$UP32)

# non-parametric test
wilcox.test(data_col$UP32,
            data_uclm$UP32,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 16756, p-value = 4.215e-05
# result of the non-parametric test
# there are statistically significant differences

# UP33 usas: Realidad aumentada ----

data %>%
  select(Paises,UP33)%>%
  na.omit()%>%
  summarise(n = length(UP33),
            media = mean(UP33),
            sd = sd(UP33),
            mediana = median(UP33),
            min = min(UP33),
            max = max(UP33))

data %>%
  select(Paises,UP33)%>%
  na.omit()%>%
  group_by(Paises)%>%
  summarise(n = length(UP33),
            media = mean(UP33),
            sd = sd(UP33),
            mediana = median(UP33),
            min = min(UP33),
            max = max(UP33))

# statistical inference

shapiro.test(data$UP33)
ks.test(scale(data$UP33), "pnorm")
lillie.test(data$UP33)

qqnorm(data$UP33, las=1, pch=18, 
       main="UP33 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$UP33)

shapiro.test(data_col$UP33)
ks.test(scale(data_col$UP33), "pnorm")
lillie.test(data_col$UP33)

qqnorm(data_col$UP33, las=1, pch=18, 
       main="UP33 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$UP33)

shapiro.test(data_uclm$UP33)
ks.test(scale(data_uclm$UP33), "pnorm")
lillie.test(data_uclm$UP33)

qqnorm(data_uclm$UP33, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$UP33)

# non-parametric test
wilcox.test(data_col$UP33,
            data_uclm$UP33,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 16424, p-value = 5.93e-05
# result of the non-parametric test
# there are statistically significant differences

# UP34 usas: Códigos QR ----

data %>%
  select(Paises,UP34)%>%
  na.omit()%>%
  summarise(n = length(UP34),
            media = mean(UP34),
            sd = sd(UP34),
            mediana = median(UP34),
            min = min(UP34),
            max = max(UP34))

data %>%
  select(Paises,UP34)%>%
  na.omit()%>%
  group_by(Paises)%>%
  summarise(n = length(UP34),
            media = mean(UP34),
            sd = sd(UP34),
            mediana = median(UP34),
            min = min(UP34),
            max = max(UP34))

# statistical inference

shapiro.test(data$UP34)
ks.test(scale(data$UP34), "pnorm")
lillie.test(data$UP34)

qqnorm(data$UP34, las=1, pch=18, 
       main="UP34 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$UP34)

shapiro.test(data_col$UP34)
ks.test(scale(data_col$UP34), "pnorm")
lillie.test(data_col$UP34)

qqnorm(data_col$UP34, las=1, pch=18, 
       main="UP34 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$UP34)

shapiro.test(data_uclm$UP34)
ks.test(scale(data_uclm$UP34), "pnorm")
lillie.test(data_uclm$UP34)

qqnorm(data_uclm$UP34, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$UP34)

# non-parametric test
wilcox.test(data_col$UP34,
            data_uclm$UP34,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 8695, p-value = 7.925e-08
# result of the non-parametric test
# there are statistically significant differences

# UP35 usas: Gamificación ----

data %>%
  select(Paises,UP35)%>%
  na.omit()%>%
  summarise(n = length(UP35),
            media = mean(UP35),
            sd = sd(UP35),
            mediana = median(UP35),
            min = min(UP35),
            max = max(UP35))

data %>%
  select(Paises,UP35)%>%
  na.omit()%>%
  group_by(Paises)%>%
  summarise(n = length(UP35),
            media = mean(UP35),
            sd = sd(UP35),
            mediana = median(UP35),
            min = min(UP35),
            max = max(UP35))

# statistical inference

shapiro.test(data$UP35)
ks.test(scale(data$UP35), "pnorm")
lillie.test(data$UP35)

qqnorm(data$UP35, las=1, pch=18, 
       main="UP35 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$UP35)

shapiro.test(data_col$UP35)
ks.test(scale(data_col$UP35), "pnorm")
lillie.test(data_col$UP35)

qqnorm(data_col$UP35, las=1, pch=18, 
       main="UP35 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$UP35)

shapiro.test(data_uclm$UP35)
ks.test(scale(data_uclm$UP35), "pnorm")
lillie.test(data_uclm$UP35)

qqnorm(data_uclm$UP35, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$UP35)

# non-parametric test
wilcox.test(data_col$UP35,
            data_uclm$UP35,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 12070, p-value = 0.1756
# result of the non-parametric test
# there are statistically significant differences

# CONP1 consideras: Editores de texto (Word, OpenOffice Writer, LibreOffice Writer,…) ----

data %>%
  select(Paises,CONP1)%>%
  na.omit()%>%
  summarise(n = length(CONP1),
            media = mean(CONP1),
            sd = sd(CONP1),
            mediana = median(CONP1),
            min = min(CONP1),
            max = max(CONP1))


data %>%
  select(Paises,CONP1)%>%
  na.omit()%>%
  groCONP_by(Paises)%>%
  summarise(n = length(CONP1),
            media = mean(CONP1),
            sd = sd(CONP1),
            mediana = median(CONP1),
            min = min(CONP1),
            max = max(CONP1))


# statistical inference

shapiro.test(data$CONP1)
ks.test(scale(data$CONP1), "pnorm")
lillie.test(data$CONP1)

qqnorm(data$CONP1, las=1, pch=18, 
       main="CONP1 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$CONP1)

shapiro.test(data_col$CONP1)
ks.test(scale(data_col$CONP1), "pnorm")
lillie.test(data_col$CONP1)

qqnorm(data_col$CONP1, las=1, pch=18, 
       main="CONP1 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$CONP1)

shapiro.test(data_uclm$CONP1)
ks.test(scale(data_uclm$CONP1), "pnorm")
lillie.test(data_uclm$CONP1)

qqnorm(data_uclm$CONP1, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$CONP1)

# non-parametric test
wilcox.test(data_col$CONP1,
            data_uclm$CONP1,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 10052, p-value = 4.244e-05
# result of the non-parametric test
# there are statistically significant differences

fligner.test(list(data_col$CONP1,
                  data_uclm$CONP1))

leveneTest(CONP1 ~ Paises, data = data,
           center = "median")

var.test(x = data_col$CONP1,
         y = data_uclm$CONP1)

bartlett.test(list(data_col$CONP1,
                   data_uclm$CONP1))

z.test(x = data_col %>%
         select(CONP1)%>%
         na.omit()%>%
         unlist(), y = data_uclm %>%
         select(CONP1)%>%
         na.omit()%>%
         unlist(),                                      # Two samples with normal distribution
       alt = "two.sided",                               # Dos colas
       mu = 0,                                          # H_0: mu_1 - mu_2 = 0
       sigma.x = sd(data_col$CONP1,na.rm = TRUE),         # desviación estándar m
       sigma.y = sd(data_uclm$CONP1,na.rm = TRUE),        # desviación estandar n
       conf.level = 0.95)
# z = -4.5982, p-value = 4.263e-06

t.test(x = data_col$CONP1,
       y = data_uclm$CONP1,
       alternative = "two.sided", 
       mu = 0, var.equal = FALSE, 
       conf.level = 0.95)
# t = -4.5982, df = 310.67, p-value = 6.211e-06

# CONP2 consideras: Hojas de cálculo (Excel, Google Sheets, LibreOffice Calc,…) ----

data %>%
  select(Paises,CONP2)%>%
  na.omit()%>%
  summarise(n = length(CONP2),
            media = mean(CONP2),
            sd = sd(CONP2),
            mediana = median(CONP2),
            min = min(CONP2),
            max = max(CONP2))

data %>%
  select(Paises,CONP2)%>%
  na.omit()%>%
  groCONP_by(Paises)%>%
  summarise(n = length(CONP2),
            media = mean(CONP2),
            sd = sd(CONP2),
            mediana = median(CONP2),
            min = min(CONP2),
            max = max(CONP2))

# statistical inference

shapiro.test(data$CONP2)
ks.test(scale(data$CONP2), "pnorm")
lillie.test(data$CONP2)

qqnorm(data$CONP2, las=1, pch=18, 
       main="CONP2 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$CONP2)

shapiro.test(data_col$CONP2)
ks.test(scale(data_col$CONP2), "pnorm")
lillie.test(data_col$CONP2)

qqnorm(data_col$CONP2, las=1, pch=18, 
       main="CONP2 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$CONP2)

shapiro.test(data_uclm$CONP2)
ks.test(scale(data_uclm$CONP2), "pnorm")
lillie.test(data_uclm$CONP2)

qqnorm(data_uclm$CONP2, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$CONP2)

# non-parametric test
wilcox.test(data_col$CONP2,
            data_uclm$CONP2,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 15948, p-value = 0.0017
# result of the non-parametric test
# there are statistically significant differences


# CONP3 consideras: Bases de datos (Access, Base, nuBuilder,…) ----

data %>%
  select(Paises,CONP3)%>%
  na.omit()%>%
  summarise(n = length(CONP3),
            media = mean(CONP3),
            sd = sd(CONP3),
            mediana = median(CONP3),
            min = min(CONP3),
            max = max(CONP3))

data %>%
  select(Paises,CONP3)%>%
  na.omit()%>%
  groCONP_by(Paises)%>%
  summarise(n = length(CONP3),
            media = mean(CONP3),
            sd = sd(CONP3),
            mediana = median(CONP3),
            min = min(CONP3),
            max = max(CONP3))

# statistical inference

shapiro.test(data$CONP3)
ks.test(scale(data$CONP3), "pnorm")
lillie.test(data$CONP3)

qqnorm(data$CONP3, las=1, pch=18, 
       main="CONP3 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$CONP3)

shapiro.test(data_col$CONP3)
ks.test(scale(data_col$CONP3), "pnorm")
lillie.test(data_col$CONP3)

qqnorm(data_col$CONP3, las=1, pch=18, 
       main="CONP3 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$CONP3)

shapiro.test(data_uclm$CONP3)
ks.test(scale(data_uclm$CONP3), "pnorm")
lillie.test(data_uclm$CONP3)

qqnorm(data_uclm$CONP3, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$CONP3)

# non-parametric test
wilcox.test(data_col$CONP3,
            data_uclm$CONP3,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 16324, p-value = 0.0001284
# result of the non-parametric test
# there are statistically significant differences

# CONP4 consideras: Creadores de presentaciones visuales (PowerPoint, Google Slides,…) ----

data %>%
  select(Paises,CONP4)%>%
  na.omit()%>%
  summarise(n = length(CONP4),
            media = mean(CONP4),
            sd = sd(CONP4),
            mediana = median(CONP4),
            min = min(CONP4),
            max = max(CONP4))

data %>%
  select(Paises,CONP4)%>%
  na.omit()%>%
  groCONP_by(Paises)%>%
  summarise(n = length(CONP4),
            media = mean(CONP4),
            sd = sd(CONP4),
            mediana = median(CONP4),
            min = min(CONP4),
            max = max(CONP4))

# statistical inference

shapiro.test(data$CONP4)
ks.test(scale(data$CONP4), "pnorm")
lillie.test(data$CONP4)

qqnorm(data$CONP4, las=1, pch=18, 
       main="CONP4 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$CONP4)

shapiro.test(data_col$CONP4)
# W = 0.84433, p-value = 4.31e-12
ks.test(scale(data_col$CONP4), "pnorm")
# D = 0.22573, p-value = 7.339e-08
lillie.test(data_col$CONP4)
# D = 0.22573, p-value < 2.2e-16

qqnorm(data_col$CONP4, las=1, pch=18, 
       main="CONP4 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$CONP4)

shapiro.test(data_uclm$CONP4)
# W = 0.68569, p-value < 2.2e-16
ks.test(scale(data_uclm$CONP4), "pnorm")
# D = 0.35902, p-value < 2.2e-16
lillie.test(data_uclm$CONP4)
# D = 0.35902, p-value < 2.2e-16

qqnorm(data_uclm$CONP4, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$CONP4)

# non-parametric test
wilcox.test(data_col$CONP4,
            data_uclm$CONP4,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 9319, p-value = 3.064e-07
# result of the non-parametric test
# there are statistically significant differences

# CONP5 consideras: Programas de edición de imagen (Paint, Adobe PhotoShop, Gimp,…) ----

data %>%
  select(Paises,CONP5)%>%
  na.omit()%>%
  summarise(n = length(CONP5),
            media = mean(CONP5),
            sd = sd(CONP5),
            mediana = median(CONP5),
            min = min(CONP5),
            max = max(CONP5))

# n media    sd mediana   min   max
# 326  3.60  1.09       4     1     5

data %>%
  select(Paises,CONP5)%>%
  na.omit()%>%
  groCONP_by(Paises)%>%
  summarise(n = length(CONP5),
            media = mean(CONP5),
            sd = sd(CONP5),
            mediana = median(CONP5),
            min = min(CONP5),
            max = max(CONP5))

# Paises       n media    sd mediana   min   max
# Colombia   168  3.65  1.11       4     1     5
# España     158  3.55  1.08       4     1     5

# statistical inference

shapiro.test(data$CONP5)
# W = 0.88993, p-value = 1.381e-14
ks.test(scale(data$CONP5), "pnorm")
# D = 0.1874, p-value = 2.272e-10
lillie.test(data$CONP5)
# D = 0.1874, p-value < 2.2e-16

qqnorm(data$CONP5, las=1, pch=18, 
       main="CONP5 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$CONP5)

shapiro.test(data_col$CONP5)
# W = 0.88559, p-value = 4.516e-10
ks.test(scale(data_col$CONP5), "pnorm")
# D = 0.1881, p-value = 1.374e-05
lillie.test(data_col$CONP5)
# D = 0.1881, p-value = 6.046e-16

qqnorm(data_col$CONP5, las=1, pch=18, 
       main="CONP5 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$CONP5)

shapiro.test(data_uclm$CONP5)
# W = 0.89129, p-value = 2.181e-09
ks.test(scale(data_uclm$CONP5), "pnorm")
# D = 0.18665, p-value = 3.309e-05
lillie.test(data_uclm$CONP5)
# D = 0.18665, p-value = 1.008e-14

qqnorm(data_uclm$CONP5, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$CONP5)

# non-parametric test
wilcox.test(data_col$CONP5,
            data_uclm$CONP5,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 13998, p-value = 0.3753
# result of the non-parametric test
# there are statistically significant differences



# CONP6 consideras: Programas de edición de audio (Audacity, Ocenaudio, Reaper, …) ----

data %>%
  select(Paises,CONP6)%>%
  na.omit()%>%
  summarise(n = length(CONP6),
            media = mean(CONP6),
            sd = sd(CONP6),
            mediana = median(CONP6),
            min = min(CONP6),
            max = max(CONP6))

# n media    sd mediana   min   max
# 327  2.60  1.23       3     1     5

data %>%
  select(Paises,CONP6)%>%
  na.omit()%>%
  groCONP_by(Paises)%>%
  summarise(n = length(CONP6),
            media = mean(CONP6),
            sd = sd(CONP6),
            mediana = median(CONP6),
            min = min(CONP6),
            max = max(CONP6))

# Paises       n media    sd mediana   min   max
# Colombia   168  2.63  1.22       3     1     5
# España     159  2.56  1.26       2     1     5

# statistical inference

shapiro.test(data$CONP6)
# W = 0.8962, p-value = 3.765e-14
ks.test(scale(data$CONP6), "pnorm")
# D = 0.17479, p-value = 4.206e-09
lillie.test(data$CONP6)
# D = 0.17479, p-value < 2.2e-16

qqnorm(data$CONP6, las=1, pch=18, 
       main="CONP6 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$CONP6)

shapiro.test(data_col$CONP6)
# W = 0.90033, p-value = 3.105e-09
ks.test(scale(data_col$CONP6), "pnorm")
# D = 0.16682, p-value = 0.0001739
lillie.test(data_col$CONP6)
# D = 0.16682, p-value = 2.242e-12

qqnorm(data_col$CONP6, las=1, pch=18, 
       main="CONP6 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$CONP6)

shapiro.test(data_uclm$CONP6)
# W = 0.88669, p-value = 1.127e-09
ks.test(scale(data_uclm$CONP6), "pnorm")
# D = 0.2004, p-value = 5.683e-06
lillie.test(data_uclm$CONP6)
# D = 0.2004, p-value < 2.2e-16

qqnorm(data_uclm$CONP6, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$CONP6)

# non-parametric test
wilcox.test(data_col$CONP6,
            data_uclm$CONP6,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 13956, p-value = 0.47
# result of the non-parametric test
# there are statistically significant differences

# CONP7 consideras: Programas de edición de video (Kdenlive, Shotcut, Pinnacle, Sony Vegas,…) ----

data %>%
  select(Paises,CONP7)%>%
  na.omit()%>%
  summarise(n = length(CONP7),
            media = mean(CONP7),
            sd = sd(CONP7),
            mediana = median(CONP7),
            min = min(CONP7),
            max = max(CONP7))

# n media    sd mediana   min   max
# 327  2.60  1.23       3     1     5

data %>%
  select(Paises,CONP7)%>%
  na.omit()%>%
  groCONP_by(Paises)%>%
  summarise(n = length(CONP7),
            media = mean(CONP7),
            sd = sd(CONP7),
            mediana = median(CONP7),
            min = min(CONP7),
            max = max(CONP7))

# Paises       n media    sd mediana   min   max
# Colombia   168  2.63  1.22       3     1     5
# España     159  2.56  1.26       2     1     5

# statistical inference

shapiro.test(data$CONP7)
# W = 0.8962, p-value = 3.765e-14
ks.test(scale(data$CONP7), "pnorm")
# D = 0.17479, p-value = 4.206e-09
lillie.test(data$CONP7)
# D = 0.17479, p-value < 2.2e-16

qqnorm(data$CONP7, las=1, pch=18, 
       main="CONP7 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$CONP7)

shapiro.test(data_col$CONP7)
# W = 0.90033, p-value = 3.105e-09
ks.test(scale(data_col$CONP7), "pnorm")
# D = 0.16682, p-value = 0.0001739
lillie.test(data_col$CONP7)
# D = 0.16682, p-value = 2.242e-12

qqnorm(data_col$CONP7, las=1, pch=18, 
       main="CONP7 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$CONP7)

shapiro.test(data_uclm$CONP7)
# W = 0.88669, p-value = 1.127e-09
ks.test(scale(data_uclm$CONP7), "pnorm")
# D = 0.2004, p-value = 5.683e-06
lillie.test(data_uclm$CONP7)
# D = 0.2004, p-value < 2.2e-16

qqnorm(data_uclm$CONP7, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$CONP7)

# non-parametric test
wilcox.test(data_col$CONP7,
            data_uclm$CONP7,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 13956, p-value = 0.47
# result of the non-parametric test
# there are statistically significant differences

# CONP8 consideras: Videoconferencia ----

data %>%
  select(Paises,CONP8)%>%
  na.omit()%>%
  summarise(n = length(CONP8),
            media = mean(CONP8),
            sd = sd(CONP8),
            mediana = median(CONP8),
            min = min(CONP8),
            max = max(CONP8))

# n media    sd mediana   min   max
# 327  2.60  1.23       3     1     5

data %>%
  select(Paises,CONP8)%>%
  na.omit()%>%
  groCONP_by(Paises)%>%
  summarise(n = length(CONP8),
            media = mean(CONP8),
            sd = sd(CONP8),
            mediana = median(CONP8),
            min = min(CONP8),
            max = max(CONP8))

# Paises       n media    sd mediana   min   max
# Colombia   168  2.63  1.22       3     1     5
# España     159  2.56  1.26       2     1     5

# statistical inference

shapiro.test(data$CONP8)
# W = 0.8962, p-value = 3.765e-14
ks.test(scale(data$CONP8), "pnorm")
# D = 0.17479, p-value = 4.206e-09
lillie.test(data$CONP8)
# D = 0.17479, p-value < 2.2e-16

qqnorm(data$CONP8, las=1, pch=18, 
       main="CONP8 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$CONP8)

shapiro.test(data_col$CONP8)
# W = 0.90033, p-value = 3.105e-09
ks.test(scale(data_col$CONP8), "pnorm")
# D = 0.16682, p-value = 0.0001739
lillie.test(data_col$CONP8)
# D = 0.16682, p-value = 2.242e-12

qqnorm(data_col$CONP8, las=1, pch=18, 
       main="CONP8 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$CONP8)

shapiro.test(data_uclm$CONP8)
# W = 0.88669, p-value = 1.127e-09
ks.test(scale(data_uclm$CONP8), "pnorm")
# D = 0.2004, p-value = 5.683e-06
lillie.test(data_uclm$CONP8)
# D = 0.2004, p-value < 2.2e-16

qqnorm(data_uclm$CONP8, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$CONP8)

# non-parametric test
wilcox.test(data_col$CONP8,
            data_uclm$CONP8,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 13956, p-value = 0.47
# result of the non-parametric test
# there are statistically significant differences

# CONP9 consideras: Listas de distribución ----

data %>%
  select(Paises,CONP9)%>%
  na.omit()%>%
  summarise(n = length(CONP9),
            media = mean(CONP9),
            sd = sd(CONP9),
            mediana = median(CONP9),
            min = min(CONP9),
            max = max(CONP9))

# n media    sd mediana   min   max
# 327  2.60  1.23       3     1     5

data %>%
  select(Paises,CONP9)%>%
  na.omit()%>%
  groCONP_by(Paises)%>%
  summarise(n = length(CONP9),
            media = mean(CONP9),
            sd = sd(CONP9),
            mediana = median(CONP9),
            min = min(CONP9),
            max = max(CONP9))

# Paises       n media    sd mediana   min   max
# Colombia   168  2.63  1.22       3     1     5
# España     159  2.56  1.26       2     1     5

# statistical inference

shapiro.test(data$CONP9)
# W = 0.8962, p-value = 3.765e-14
ks.test(scale(data$CONP9), "pnorm")
# D = 0.17479, p-value = 4.206e-09
lillie.test(data$CONP9)
# D = 0.17479, p-value < 2.2e-16

qqnorm(data$CONP9, las=1, pch=18, 
       main="CONP9 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$CONP9)

shapiro.test(data_col$CONP9)
# W = 0.90033, p-value = 3.105e-09
ks.test(scale(data_col$CONP9), "pnorm")
# D = 0.16682, p-value = 0.0001739
lillie.test(data_col$CONP9)
# D = 0.16682, p-value = 2.242e-12

qqnorm(data_col$CONP9, las=1, pch=18, 
       main="CONP9 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$CONP9)

shapiro.test(data_uclm$CONP9)
# W = 0.88669, p-value = 1.127e-09
ks.test(scale(data_uclm$CONP9), "pnorm")
# D = 0.2004, p-value = 5.683e-06
lillie.test(data_uclm$CONP9)
# D = 0.2004, p-value < 2.2e-16

qqnorm(data_uclm$CONP9, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$CONP9)

# non-parametric test
wilcox.test(data_col$CONP9,
            data_uclm$CONP9,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 13956, p-value = 0.47
# result of the non-parametric test
# there are statistically significant differences

# CONP10 consideras: Foros ----

data %>%
  select(Paises,CONP10)%>%
  na.omit()%>%
  summarise(n = length(CONP10),
            media = mean(CONP10),
            sd = sd(CONP10),
            mediana = median(CONP10),
            min = min(CONP10),
            max = max(CONP10))

# n media    sd mediana   min   max
# 327  2.60  1.23       3     1     5

data %>%
  select(Paises,CONP10)%>%
  na.omit()%>%
  groCONP_by(Paises)%>%
  summarise(n = length(CONP10),
            media = mean(CONP10),
            sd = sd(CONP10),
            mediana = median(CONP10),
            min = min(CONP10),
            max = max(CONP10))

# Paises       n media    sd mediana   min   max
# Colombia   168  2.63  1.22       3     1     5
# España     159  2.56  1.26       2     1     5

# statistical inference

shapiro.test(data$CONP10)
# W = 0.8962, p-value = 3.765e-14
ks.test(scale(data$CONP10), "pnorm")
# D = 0.17479, p-value = 4.206e-09
lillie.test(data$CONP10)
# D = 0.17479, p-value < 2.2e-16

qqnorm(data$CONP10, las=1, pch=18, 
       main="CONP10 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$CONP10)

shapiro.test(data_col$CONP10)
# W = 0.90033, p-value = 3.105e-09
ks.test(scale(data_col$CONP10), "pnorm")
# D = 0.16682, p-value = 0.0001739
lillie.test(data_col$CONP10)
# D = 0.16682, p-value = 2.242e-12

qqnorm(data_col$CONP10, las=1, pch=18, 
       main="CONP10 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$CONP10)

shapiro.test(data_uclm$CONP10)
# W = 0.88669, p-value = 1.127e-09
ks.test(scale(data_uclm$CONP10), "pnorm")
# D = 0.2004, p-value = 5.683e-06
lillie.test(data_uclm$CONP10)
# D = 0.2004, p-value < 2.2e-16

qqnorm(data_uclm$CONP10, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$CONP10)

# non-parametric test
wilcox.test(data_col$CONP10,
            data_uclm$CONP10,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 13956, p-value = 0.47
# result of the non-parametric test
# there are statistically significant differences

# CONP11 consideras: Mensajería instantánea / Chat ----

data %>%
  select(Paises,CONP11)%>%
  na.omit()%>%
  summarise(n = length(CONP11),
            media = mean(CONP11),
            sd = sd(CONP11),
            mediana = median(CONP11),
            min = min(CONP11),
            max = max(CONP11))

# n media    sd mediana   min   max
# 327  2.60  1.23       3     1     5

data %>%
  select(Paises,CONP11)%>%
  na.omit()%>%
  groCONP_by(Paises)%>%
  summarise(n = length(CONP11),
            media = mean(CONP11),
            sd = sd(CONP11),
            mediana = median(CONP11),
            min = min(CONP11),
            max = max(CONP11))

# Paises       n media    sd mediana   min   max
# Colombia   168  2.63  1.22       3     1     5
# España     159  2.56  1.26       2     1     5

# statistical inference

shapiro.test(data$CONP11)
# W = 0.8962, p-value = 3.765e-14
ks.test(scale(data$CONP11), "pnorm")
# D = 0.17479, p-value = 4.206e-09
lillie.test(data$CONP11)
# D = 0.17479, p-value < 2.2e-16

qqnorm(data$CONP11, las=1, pch=18, 
       main="CONP11 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$CONP11)

shapiro.test(data_col$CONP11)
# W = 0.90033, p-value = 3.105e-09
ks.test(scale(data_col$CONP11), "pnorm")
# D = 0.16682, p-value = 0.0001739
lillie.test(data_col$CONP11)
# D = 0.16682, p-value = 2.242e-12

qqnorm(data_col$CONP11, las=1, pch=18, 
       main="CONP11 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$CONP11)

shapiro.test(data_uclm$CONP11)
# W = 0.88669, p-value = 1.127e-09
ks.test(scale(data_uclm$CONP11), "pnorm")
# D = 0.2004, p-value = 5.683e-06
lillie.test(data_uclm$CONP11)
# D = 0.2004, p-value < 2.2e-16

qqnorm(data_uclm$CONP11, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$CONP11)

# non-parametric test
wilcox.test(data_col$CONP11,
            data_uclm$CONP11,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 13956, p-value = 0.47
# result of the non-parametric test
# there are statistically significant differences

# CONP12 consideras: Redes sociales (Facebook, twitter, Instagram,…) ----

data %>%
  select(Paises,CONP12)%>%
  na.omit()%>%
  summarise(n = length(CONP12),
            media = mean(CONP12),
            sd = sd(CONP12),
            mediana = median(CONP12),
            min = min(CONP12),
            max = max(CONP12))

# n media    sd mediana   min   max
# 327  2.60  1.23       3     1     5

data %>%
  select(Paises,CONP12)%>%
  na.omit()%>%
  groCONP_by(Paises)%>%
  summarise(n = length(CONP12),
            media = mean(CONP12),
            sd = sd(CONP12),
            mediana = median(CONP12),
            min = min(CONP12),
            max = max(CONP12))

# Paises       n media    sd mediana   min   max
# Colombia   168  2.63  1.22       3     1     5
# España     159  2.56  1.26       2     1     5

# statistical inference

shapiro.test(data$CONP12)
# W = 0.8962, p-value = 3.765e-14
ks.test(scale(data$CONP12), "pnorm")
# D = 0.17479, p-value = 4.206e-09
lillie.test(data$CONP12)
# D = 0.17479, p-value < 2.2e-16

qqnorm(data$CONP12, las=1, pch=18, 
       main="CONP12 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$CONP12)

shapiro.test(data_col$CONP12)
# W = 0.90033, p-value = 3.105e-09
ks.test(scale(data_col$CONP12), "pnorm")
# D = 0.16682, p-value = 0.0001739
lillie.test(data_col$CONP12)
# D = 0.16682, p-value = 2.242e-12

qqnorm(data_col$CONP12, las=1, pch=18, 
       main="CONP12 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$CONP12)

shapiro.test(data_uclm$CONP12)
# W = 0.88669, p-value = 1.127e-09
ks.test(scale(data_uclm$CONP12), "pnorm")
# D = 0.2004, p-value = 5.683e-06
lillie.test(data_uclm$CONP12)
# D = 0.2004, p-value < 2.2e-16

qqnorm(data_uclm$CONP12, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$CONP12)

# non-parametric test
wilcox.test(data_col$CONP12,
            data_uclm$CONP12,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 13956, p-value = 0.47
# result of the non-parametric test
# there are statistically significant differences

# CONP13 consideras: Herramientas de trabajo colaborativo (blogs, wikis,…) ----

data %>%
  select(Paises,CONP13)%>%
  na.omit()%>%
  summarise(n = length(CONP13),
            media = mean(CONP13),
            sd = sd(CONP13),
            mediana = median(CONP13),
            min = min(CONP13),
            max = max(CONP13))

# n media    sd mediana   min   max
# 327  2.60  1.23       3     1     5

data %>%
  select(Paises,CONP13)%>%
  na.omit()%>%
  groCONP_by(Paises)%>%
  summarise(n = length(CONP13),
            media = mean(CONP13),
            sd = sd(CONP13),
            mediana = median(CONP13),
            min = min(CONP13),
            max = max(CONP13))

# Paises       n media    sd mediana   min   max
# Colombia   168  2.63  1.22       3     1     5
# España     159  2.56  1.26       2     1     5

# statistical inference

shapiro.test(data$CONP13)
# W = 0.8962, p-value = 3.765e-14
ks.test(scale(data$CONP13), "pnorm")
# D = 0.17479, p-value = 4.206e-09
lillie.test(data$CONP13)
# D = 0.17479, p-value < 2.2e-16

qqnorm(data$CONP13, las=1, pch=18, 
       main="CONP13 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$CONP13)

shapiro.test(data_col$CONP13)
# W = 0.90033, p-value = 3.105e-09
ks.test(scale(data_col$CONP13), "pnorm")
# D = 0.16682, p-value = 0.0001739
lillie.test(data_col$CONP13)
# D = 0.16682, p-value = 2.242e-12

qqnorm(data_col$CONP13, las=1, pch=18, 
       main="CONP13 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$CONP13)

shapiro.test(data_uclm$CONP13)
# W = 0.88669, p-value = 1.127e-09
ks.test(scale(data_uclm$CONP13), "pnorm")
# D = 0.2004, p-value = 5.683e-06
lillie.test(data_uclm$CONP13)
# D = 0.2004, p-value < 2.2e-16

qqnorm(data_uclm$CONP13, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$CONP13)

# non-parametric test
wilcox.test(data_col$CONP13,
            data_uclm$CONP13,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 13956, p-value = 0.47
# result of the non-parametric test
# there are statistically significant differences

# CONP14 consideras: Herramientas de intercambio de archivos (Emule, Torrents,…) ----

data %>%
  select(Paises,CONP14)%>%
  na.omit()%>%
  summarise(n = length(CONP14),
            media = mean(CONP14),
            sd = sd(CONP14),
            mediana = median(CONP14),
            min = min(CONP14),
            max = max(CONP14))

# n media    sd mediana   min   max
# 327  2.60  1.23       3     1     5

data %>%
  select(Paises,CONP14)%>%
  na.omit()%>%
  groCONP_by(Paises)%>%
  summarise(n = length(CONP14),
            media = mean(CONP14),
            sd = sd(CONP14),
            mediana = median(CONP14),
            min = min(CONP14),
            max = max(CONP14))

# Paises       n media    sd mediana   min   max
# Colombia   168  2.63  1.22       3     1     5
# España     159  2.56  1.26       2     1     5

# statistical inference

shapiro.test(data$CONP14)
# W = 0.8962, p-value = 3.765e-14
ks.test(scale(data$CONP14), "pnorm")
# D = 0.17479, p-value = 4.206e-09
lillie.test(data$CONP14)
# D = 0.17479, p-value < 2.2e-16

qqnorm(data$CONP14, las=1, pch=18, 
       main="CONP14 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$CONP14)

shapiro.test(data_col$CONP14)
# W = 0.90033, p-value = 3.105e-09
ks.test(scale(data_col$CONP14), "pnorm")
# D = 0.16682, p-value = 0.0001739
lillie.test(data_col$CONP14)
# D = 0.16682, p-value = 2.242e-12

qqnorm(data_col$CONP14, las=1, pch=18, 
       main="CONP14 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$CONP14)

shapiro.test(data_uclm$CONP14)
# W = 0.88669, p-value = 1.127e-09
ks.test(scale(data_uclm$CONP14), "pnorm")
# D = 0.2004, p-value = 5.683e-06
lillie.test(data_uclm$CONP14)
# D = 0.2004, p-value < 2.2e-16

qqnorm(data_uclm$CONP14, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$CONP14)

# non-parametric test
wilcox.test(data_col$CONP14,
            data_uclm$CONP14,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 13956, p-value = 0.47
# result of the non-parametric test
# there are statistically significant differences

# CONP15 consideras: Herramientas de búsqueda de información en la red (Google,…) ----

data %>%
  select(Paises,CONP15)%>%
  na.omit()%>%
  summarise(n = length(CONP15),
            media = mean(CONP15),
            sd = sd(CONP15),
            mediana = median(CONP15),
            min = min(CONP15),
            max = max(CONP15))

# n media    sd mediana   min   max
# 327  2.60  1.23       3     1     5

data %>%
  select(Paises,CONP15)%>%
  na.omit()%>%
  groCONP_by(Paises)%>%
  summarise(n = length(CONP15),
            media = mean(CONP15),
            sd = sd(CONP15),
            mediana = median(CONP15),
            min = min(CONP15),
            max = max(CONP15))

# Paises       n media    sd mediana   min   max
# Colombia   168  2.63  1.22       3     1     5
# España     159  2.56  1.26       2     1     5

# statistical inference

shapiro.test(data$CONP15)
# W = 0.8962, p-value = 3.765e-14
ks.test(scale(data$CONP15), "pnorm")
# D = 0.17479, p-value = 4.206e-09
lillie.test(data$CONP15)
# D = 0.17479, p-value < 2.2e-16

qqnorm(data$CONP15, las=1, pch=18, 
       main="CONP15 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$CONP15)

shapiro.test(data_col$CONP15)
# W = 0.90033, p-value = 3.105e-09
ks.test(scale(data_col$CONP15), "pnorm")
# D = 0.16682, p-value = 0.0001739
lillie.test(data_col$CONP15)
# D = 0.16682, p-value = 2.242e-12

qqnorm(data_col$CONP15, las=1, pch=18, 
       main="CONP15 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$CONP15)

shapiro.test(data_uclm$CONP15)
# W = 0.88669, p-value = 1.127e-09
ks.test(scale(data_uclm$CONP15), "pnorm")
# D = 0.2004, p-value = 5.683e-06
lillie.test(data_uclm$CONP15)
# D = 0.2004, p-value < 2.2e-16

qqnorm(data_uclm$CONP15, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$CONP15)

# non-parametric test
wilcox.test(data_col$CONP15,
            data_uclm$CONP15,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 13956, p-value = 0.47
# result of the non-parametric test
# there are statistically significant differences

# CONP16 consideras: Herramientas de búsqueda de información en la red (Google,…) ----

data %>%
  select(Paises,CONP16)%>%
  na.omit()%>%
  summarise(n = length(CONP16),
            media = mean(CONP16),
            sd = sd(CONP16),
            mediana = median(CONP16),
            min = min(CONP16),
            max = max(CONP16))

# n media    sd mediana   min   max
# 327  2.60  1.23       3     1     5

data %>%
  select(Paises,CONP16)%>%
  na.omit()%>%
  groCONP_by(Paises)%>%
  summarise(n = length(CONP16),
            media = mean(CONP16),
            sd = sd(CONP16),
            mediana = median(CONP16),
            min = min(CONP16),
            max = max(CONP16))

# Paises       n media    sd mediana   min   max
# Colombia   168  2.63  1.22       3     1     5
# España     159  2.56  1.26       2     1     5

# statistical inference

shapiro.test(data$CONP16)
# W = 0.8962, p-value = 3.765e-14
ks.test(scale(data$CONP16), "pnorm")
# D = 0.17479, p-value = 4.206e-09
lillie.test(data$CONP16)
# D = 0.17479, p-value < 2.2e-16

qqnorm(data$CONP16, las=1, pch=18, 
       main="CONP16 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$CONP16)

shapiro.test(data_col$CONP16)
# W = 0.90033, p-value = 3.105e-09
ks.test(scale(data_col$CONP16), "pnorm")
# D = 0.16682, p-value = 0.0001739
lillie.test(data_col$CONP16)
# D = 0.16682, p-value = 2.242e-12

qqnorm(data_col$CONP16, las=1, pch=18, 
       main="CONP16 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$CONP16)

shapiro.test(data_uclm$CONP16)
# W = 0.88669, p-value = 1.127e-09
ks.test(scale(data_uclm$CONP16), "pnorm")
# D = 0.2004, p-value = 5.683e-06
lillie.test(data_uclm$CONP16)
# D = 0.2004, p-value < 2.2e-16

qqnorm(data_uclm$CONP16, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$CONP16)

# non-parametric test
wilcox.test(data_col$CONP16,
            data_uclm$CONP16,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 13956, p-value = 0.47
# result of the non-parametric test
# there are statistically significant differences

# CONP17 consideras: Herramientas de publicación de imágenes en red (Flickr, Google Fotos, Imgur …) ----

data %>%
  select(Paises,CONP17)%>%
  na.omit()%>%
  summarise(n = length(CONP17),
            media = mean(CONP17),
            sd = sd(CONP17),
            mediana = median(CONP17),
            min = min(CONP17),
            max = max(CONP17))

# n media    sd mediana   min   max
# 327  2.60  1.23       3     1     5

data %>%
  select(Paises,CONP17)%>%
  na.omit()%>%
  groCONP_by(Paises)%>%
  summarise(n = length(CONP17),
            media = mean(CONP17),
            sd = sd(CONP17),
            mediana = median(CONP17),
            min = min(CONP17),
            max = max(CONP17))

# Paises       n media    sd mediana   min   max
# Colombia   168  2.63  1.22       3     1     5
# España     159  2.56  1.26       2     1     5

# statistical inference

shapiro.test(data$CONP17)
# W = 0.8962, p-value = 3.765e-14
ks.test(scale(data$CONP17), "pnorm")
# D = 0.17479, p-value = 4.206e-09
lillie.test(data$CONP17)
# D = 0.17479, p-value < 2.2e-16

qqnorm(data$CONP17, las=1, pch=18, 
       main="CONP17 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$CONP17)

shapiro.test(data_col$CONP17)
# W = 0.90033, p-value = 3.105e-09
ks.test(scale(data_col$CONP17), "pnorm")
# D = 0.16682, p-value = 0.0001739
lillie.test(data_col$CONP17)
# D = 0.16682, p-value = 2.242e-12

qqnorm(data_col$CONP17, las=1, pch=18, 
       main="CONP17 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$CONP17)

shapiro.test(data_uclm$CONP17)
# W = 0.88669, p-value = 1.127e-09
ks.test(scale(data_uclm$CONP17), "pnorm")
# D = 0.2004, p-value = 5.683e-06
lillie.test(data_uclm$CONP17)
# D = 0.2004, p-value < 2.2e-16

qqnorm(data_uclm$CONP17, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$CONP17)

# non-parametric test
wilcox.test(data_col$CONP17,
            data_uclm$CONP17,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 13956, p-value = 0.47
# result of the non-parametric test
# there are statistically significant differences

# CONP18 consideras: Presentaciones interactivas en red (prezi, SlideShare, Genially,…) ----

data %>%
  select(Paises,CONP18)%>%
  na.omit()%>%
  summarise(n = length(CONP18),
            media = mean(CONP18),
            sd = sd(CONP18),
            mediana = median(CONP18),
            min = min(CONP18),
            max = max(CONP18))

# n media    sd mediana   min   max
# 327  2.60  1.23       3     1     5

data %>%
  select(Paises,CONP18)%>%
  na.omit()%>%
  groCONP_by(Paises)%>%
  summarise(n = length(CONP18),
            media = mean(CONP18),
            sd = sd(CONP18),
            mediana = median(CONP18),
            min = min(CONP18),
            max = max(CONP18))

# Paises       n media    sd mediana   min   max
# Colombia   168  2.63  1.22       3     1     5
# España     159  2.56  1.26       2     1     5

# statistical inference

shapiro.test(data$CONP18)
# W = 0.8962, p-value = 3.765e-14
ks.test(scale(data$CONP18), "pnorm")
# D = 0.17479, p-value = 4.206e-09
lillie.test(data$CONP18)
# D = 0.17479, p-value < 2.2e-16

qqnorm(data$CONP18, las=1, pch=18, 
       main="CONP18 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$CONP18)

shapiro.test(data_col$CONP18)
# W = 0.90033, p-value = 3.105e-09
ks.test(scale(data_col$CONP18), "pnorm")
# D = 0.16682, p-value = 0.0001739
lillie.test(data_col$CONP18)
# D = 0.16682, p-value = 2.242e-12

qqnorm(data_col$CONP18, las=1, pch=18, 
       main="CONP18 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$CONP18)

shapiro.test(data_uclm$CONP18)
# W = 0.88669, p-value = 1.127e-09
ks.test(scale(data_uclm$CONP18), "pnorm")
# D = 0.2004, p-value = 5.683e-06
lillie.test(data_uclm$CONP18)
# D = 0.2004, p-value < 2.2e-16

qqnorm(data_uclm$CONP18, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$CONP18)

# non-parametric test
wilcox.test(data_col$CONP18,
            data_uclm$CONP18,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 13956, p-value = 0.47
# result of the non-parametric test
# there are statistically significant differences

# CONP19 consideras: Marcadores sociales (Diigo, Pocket,…) ----

data %>%
  select(Paises,CONP19)%>%
  na.omit()%>%
  summarise(n = length(CONP19),
            media = mean(CONP19),
            sd = sd(CONP19),
            mediana = median(CONP19),
            min = min(CONP19),
            max = max(CONP19))

# n media    sd mediana   min   max
# 327  2.60  1.23       3     1     5

data %>%
  select(Paises,CONP19)%>%
  na.omit()%>%
  groCONP_by(Paises)%>%
  summarise(n = length(CONP19),
            media = mean(CONP19),
            sd = sd(CONP19),
            mediana = median(CONP19),
            min = min(CONP19),
            max = max(CONP19))

# Paises       n media    sd mediana   min   max
# Colombia   168  2.63  1.22       3     1     5
# España     159  2.56  1.26       2     1     5

# statistical inference

shapiro.test(data$CONP19)
# W = 0.8962, p-value = 3.765e-14
ks.test(scale(data$CONP19), "pnorm")
# D = 0.17479, p-value = 4.206e-09
lillie.test(data$CONP19)
# D = 0.17479, p-value < 2.2e-16

qqnorm(data$CONP19, las=1, pch=18, 
       main="CONP19 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$CONP19)

shapiro.test(data_col$CONP19)
# W = 0.90033, p-value = 3.105e-09
ks.test(scale(data_col$CONP19), "pnorm")
# D = 0.16682, p-value = 0.0001739
lillie.test(data_col$CONP19)
# D = 0.16682, p-value = 2.242e-12

qqnorm(data_col$CONP19, las=1, pch=18, 
       main="CONP19 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$CONP19)

shapiro.test(data_uclm$CONP19)
# W = 0.88669, p-value = 1.127e-09
ks.test(scale(data_uclm$CONP19), "pnorm")
# D = 0.2004, p-value = 5.683e-06
lillie.test(data_uclm$CONP19)
# D = 0.2004, p-value < 2.2e-16

qqnorm(data_uclm$CONP19, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$CONP19)

# non-parametric test
wilcox.test(data_col$CONP19,
            data_uclm$CONP19,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 13956, p-value = 0.47
# result of the non-parametric test
# there are statistically significant differences

# CONP20 consideras: Lectores de RSS (Feedly, NewsTab,…) ----

data %>%
  select(Paises,CONP20)%>%
  na.omit()%>%
  summarise(n = length(CONP20),
            media = mean(CONP20),
            sd = sd(CONP20),
            mediana = median(CONP20),
            min = min(CONP20),
            max = max(CONP20))

# n media    sd mediana   min   max
# 327  2.60  1.23       3     1     5

data %>%
  select(Paises,CONP20)%>%
  na.omit()%>%
  groCONP_by(Paises)%>%
  summarise(n = length(CONP20),
            media = mean(CONP20),
            sd = sd(CONP20),
            mediana = median(CONP20),
            min = min(CONP20),
            max = max(CONP20))

# Paises       n media    sd mediana   min   max
# Colombia   168  2.63  1.22       3     1     5
# España     159  2.56  1.26       2     1     5

# statistical inference

shapiro.test(data$CONP20)
# W = 0.8962, p-value = 3.765e-14
ks.test(scale(data$CONP20), "pnorm")
# D = 0.17479, p-value = 4.206e-09
lillie.test(data$CONP20)
# D = 0.17479, p-value < 2.2e-16

qqnorm(data$CONP20, las=1, pch=18, 
       main="CONP20 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$CONP20)

shapiro.test(data_col$CONP20)
# W = 0.90033, p-value = 3.105e-09
ks.test(scale(data_col$CONP20), "pnorm")
# D = 0.16682, p-value = 0.0001739
lillie.test(data_col$CONP20)
# D = 0.16682, p-value = 2.242e-12

qqnorm(data_col$CONP20, las=1, pch=18, 
       main="CONP20 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$CONP20)

shapiro.test(data_uclm$CONP20)
# W = 0.88669, p-value = 1.127e-09
ks.test(scale(data_uclm$CONP20), "pnorm")
# D = 0.2004, p-value = 5.683e-06
lillie.test(data_uclm$CONP20)
# D = 0.2004, p-value < 2.2e-16

qqnorm(data_uclm$CONP20, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$CONP20)

# non-parametric test
wilcox.test(data_col$CONP20,
            data_uclm$CONP20,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 13956, p-value = 0.47
# result of the non-parametric test
# there are statistically significant differences

# CONP21 consideras: LPáginas de inicio personalizadas (Start.me, Eversync, Protopage,…) ----

data %>%
  select(Paises,CONP21)%>%
  na.omit()%>%
  summarise(n = length(CONP21),
            media = mean(CONP21),
            sd = sd(CONP21),
            mediana = median(CONP21),
            min = min(CONP21),
            max = max(CONP21))

# n media    sd mediana   min   max
# 327  2.60  1.23       3     1     5

data %>%
  select(Paises,CONP21)%>%
  na.omit()%>%
  groCONP_by(Paises)%>%
  summarise(n = length(CONP21),
            media = mean(CONP21),
            sd = sd(CONP21),
            mediana = median(CONP21),
            min = min(CONP21),
            max = max(CONP21))

# Paises       n media    sd mediana   min   max
# Colombia   168  2.63  1.22       3     1     5
# España     159  2.56  1.26       2     1     5

# statistical inference

shapiro.test(data$CONP21)
# W = 0.8962, p-value = 3.765e-14
ks.test(scale(data$CONP21), "pnorm")
# D = 0.17479, p-value = 4.206e-09
lillie.test(data$CONP21)
# D = 0.17479, p-value < 2.2e-16

qqnorm(data$CONP21, las=1, pch=18, 
       main="CONP21 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$CONP21)

shapiro.test(data_col$CONP21)
# W = 0.90033, p-value = 3.105e-09
ks.test(scale(data_col$CONP21), "pnorm")
# D = 0.16682, p-value = 0.0001739
lillie.test(data_col$CONP21)
# D = 0.16682, p-value = 2.242e-12

qqnorm(data_col$CONP21, las=1, pch=18, 
       main="CONP21 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$CONP21)

shapiro.test(data_uclm$CONP21)
# W = 0.88669, p-value = 1.127e-09
ks.test(scale(data_uclm$CONP21), "pnorm")
# D = 0.2004, p-value = 5.683e-06
lillie.test(data_uclm$CONP21)
# D = 0.2004, p-value < 2.2e-16

qqnorm(data_uclm$CONP21, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$CONP21)

# non-parametric test
wilcox.test(data_col$CONP21,
            data_uclm$CONP21,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 13956, p-value = 0.47
# result of the non-parametric test
# there are statistically significant differences

# CONP22 consideras: Live streaming (Twitch, Livestream, …) ----

data %>%
  select(Paises,CONP22)%>%
  na.omit()%>%
  summarise(n = length(CONP22),
            media = mean(CONP22),
            sd = sd(CONP22),
            mediana = median(CONP22),
            min = min(CONP22),
            max = max(CONP22))

# n media    sd mediana   min   max
# 327  2.60  1.23       3     1     5

data %>%
  select(Paises,CONP22)%>%
  na.omit()%>%
  groCONP_by(Paises)%>%
  summarise(n = length(CONP22),
            media = mean(CONP22),
            sd = sd(CONP22),
            mediana = median(CONP22),
            min = min(CONP22),
            max = max(CONP22))

# Paises       n media    sd mediana   min   max
# Colombia   168  2.63  1.22       3     1     5
# España     159  2.56  1.26       2     1     5

# statistical inference

shapiro.test(data$CONP22)
# W = 0.8962, p-value = 3.765e-14
ks.test(scale(data$CONP22), "pnorm")
# D = 0.17479, p-value = 4.206e-09
lillie.test(data$CONP22)
# D = 0.17479, p-value < 2.2e-16

qqnorm(data$CONP22, las=1, pch=18, 
       main="CONP22 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$CONP22)

shapiro.test(data_col$CONP22)
# W = 0.90033, p-value = 3.105e-09
ks.test(scale(data_col$CONP22), "pnorm")
# D = 0.16682, p-value = 0.0001739
lillie.test(data_col$CONP22)
# D = 0.16682, p-value = 2.242e-12

qqnorm(data_col$CONP22, las=1, pch=18, 
       main="CONP22 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$CONP22)

shapiro.test(data_uclm$CONP22)
# W = 0.88669, p-value = 1.127e-09
ks.test(scale(data_uclm$CONP22), "pnorm")
# D = 0.2004, p-value = 5.683e-06
lillie.test(data_uclm$CONP22)
# D = 0.2004, p-value < 2.2e-16

qqnorm(data_uclm$CONP22, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$CONP22)

# non-parametric test
wilcox.test(data_col$CONP22,
            data_uclm$CONP22,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 13956, p-value = 0.47
# result of the non-parametric test
# there are statistically significant differences

# CONP23 consideras: Editores de páginas web (Wix, Jimdo, WordPress,..) ----

data %>%
  select(Paises,CONP23)%>%
  na.omit()%>%
  summarise(n = length(CONP23),
            media = mean(CONP23),
            sd = sd(CONP23),
            mediana = median(CONP23),
            min = min(CONP23),
            max = max(CONP23))

# n media    sd mediana   min   max
# 327  2.60  1.23       3     1     5

data %>%
  select(Paises,CONP23)%>%
  na.omit()%>%
  groCONP_by(Paises)%>%
  summarise(n = length(CONP23),
            media = mean(CONP23),
            sd = sd(CONP23),
            mediana = median(CONP23),
            min = min(CONP23),
            max = max(CONP23))

# Paises       n media    sd mediana   min   max
# Colombia   168  2.63  1.22       3     1     5
# España     159  2.56  1.26       2     1     5

# statistical inference

shapiro.test(data$CONP23)
# W = 0.8962, p-value = 3.765e-14
ks.test(scale(data$CONP23), "pnorm")
# D = 0.17479, p-value = 4.206e-09
lillie.test(data$CONP23)
# D = 0.17479, p-value < 2.2e-16

qqnorm(data$CONP23, las=1, pch=18, 
       main="CONP23 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$CONP23)

shapiro.test(data_col$CONP23)
# W = 0.90033, p-value = 3.105e-09
ks.test(scale(data_col$CONP23), "pnorm")
# D = 0.16682, p-value = 0.0001739
lillie.test(data_col$CONP23)
# D = 0.16682, p-value = 2.242e-12

qqnorm(data_col$CONP23, las=1, pch=18, 
       main="CONP23 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$CONP23)

shapiro.test(data_uclm$CONP23)
# W = 0.88669, p-value = 1.127e-09
ks.test(scale(data_uclm$CONP23), "pnorm")
# D = 0.2004, p-value = 5.683e-06
lillie.test(data_uclm$CONP23)
# D = 0.2004, p-value < 2.2e-16

qqnorm(data_uclm$CONP23, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$CONP23)

# non-parametric test
wilcox.test(data_col$CONP23,
            data_uclm$CONP23,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 13956, p-value = 0.47
# result of the non-parametric test
# there are statistically significant differences

# CONP24 consideras: Bibliotecas y enciclopedias virtuales ----

data %>%
  select(Paises,CONP24)%>%
  na.omit()%>%
  summarise(n = length(CONP24),
            media = mean(CONP24),
            sd = sd(CONP24),
            mediana = median(CONP24),
            min = min(CONP24),
            max = max(CONP24))

# n media    sd mediana   min   max
# 327  2.60  1.23       3     1     5

data %>%
  select(Paises,CONP24)%>%
  na.omit()%>%
  groCONP_by(Paises)%>%
  summarise(n = length(CONP24),
            media = mean(CONP24),
            sd = sd(CONP24),
            mediana = median(CONP24),
            min = min(CONP24),
            max = max(CONP24))

# Paises       n media    sd mediana   min   max
# Colombia   168  2.63  1.22       3     1     5
# España     159  2.56  1.26       2     1     5

# statistical inference

shapiro.test(data$CONP24)
# W = 0.8962, p-value = 3.765e-14
ks.test(scale(data$CONP24), "pnorm")
# D = 0.17479, p-value = 4.206e-09
lillie.test(data$CONP24)
# D = 0.17479, p-value < 2.2e-16

qqnorm(data$CONP24, las=1, pch=18, 
       main="CONP24 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$CONP24)

shapiro.test(data_col$CONP24)
# W = 0.90033, p-value = 3.105e-09
ks.test(scale(data_col$CONP24), "pnorm")
# D = 0.16682, p-value = 0.0001739
lillie.test(data_col$CONP24)
# D = 0.16682, p-value = 2.242e-12

qqnorm(data_col$CONP24, las=1, pch=18, 
       main="CONP24 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$CONP24)

shapiro.test(data_uclm$CONP24)
# W = 0.88669, p-value = 1.127e-09
ks.test(scale(data_uclm$CONP24), "pnorm")
# D = 0.2004, p-value = 5.683e-06
lillie.test(data_uclm$CONP24)
# D = 0.2004, p-value < 2.2e-16

qqnorm(data_uclm$CONP24, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$CONP24)

# non-parametric test
wilcox.test(data_col$CONP24,
            data_uclm$CONP24,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 13956, p-value = 0.47
# result of the non-parametric test
# there are statistically significant differences

# CONP25 consideras: Cartografía digital (google maps, google earth,…) ----

data %>%
  select(Paises,CONP25)%>%
  na.omit()%>%
  summarise(n = length(CONP25),
            media = mean(CONP25),
            sd = sd(CONP25),
            mediana = median(CONP25),
            min = min(CONP25),
            max = max(CONP25))

# n media    sd mediana   min   max
# 327  2.60  1.23       3     1     5

data %>%
  select(Paises,CONP25)%>%
  na.omit()%>%
  groCONP_by(Paises)%>%
  summarise(n = length(CONP25),
            media = mean(CONP25),
            sd = sd(CONP25),
            mediana = median(CONP25),
            min = min(CONP25),
            max = max(CONP25))

# Paises       n media    sd mediana   min   max
# Colombia   168  2.63  1.22       3     1     5
# España     159  2.56  1.26       2     1     5

# statistical inference

shapiro.test(data$CONP25)
# W = 0.8962, p-value = 3.765e-14
ks.test(scale(data$CONP25), "pnorm")
# D = 0.17479, p-value = 4.206e-09
lillie.test(data$CONP25)
# D = 0.17479, p-value < 2.2e-16

qqnorm(data$CONP25, las=1, pch=18, 
       main="CONP25 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$CONP25)

shapiro.test(data_col$CONP25)
# W = 0.90033, p-value = 3.105e-09
ks.test(scale(data_col$CONP25), "pnorm")
# D = 0.16682, p-value = 0.0001739
lillie.test(data_col$CONP25)
# D = 0.16682, p-value = 2.242e-12

qqnorm(data_col$CONP25, las=1, pch=18, 
       main="CONP25 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$CONP25)

shapiro.test(data_uclm$CONP25)
# W = 0.88669, p-value = 1.127e-09
ks.test(scale(data_uclm$CONP25), "pnorm")
# D = 0.2004, p-value = 5.683e-06
lillie.test(data_uclm$CONP25)
# D = 0.2004, p-value < 2.2e-16

qqnorm(data_uclm$CONP25, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$CONP25)

# non-parametric test
wilcox.test(data_col$CONP25,
            data_uclm$CONP25,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 13956, p-value = 0.47
# result of the non-parametric test
# there are statistically significant differences

# CONP26 consideras: Entornos virtuales de aprendizaje (Moodle, Dokeos, Sakai, Canvas,…) ----

data %>%
  select(Paises,CONP26)%>%
  na.omit()%>%
  summarise(n = length(CONP26),
            media = mean(CONP26),
            sd = sd(CONP26),
            mediana = median(CONP26),
            min = min(CONP26),
            max = max(CONP26))

# n media    sd mediana   min   max
# 327  2.60  1.23       3     1     5

data %>%
  select(Paises,CONP26)%>%
  na.omit()%>%
  groCONP_by(Paises)%>%
  summarise(n = length(CONP26),
            media = mean(CONP26),
            sd = sd(CONP26),
            mediana = median(CONP26),
            min = min(CONP26),
            max = max(CONP26))

# Paises       n media    sd mediana   min   max
# Colombia   168  2.63  1.22       3     1     5
# España     159  2.56  1.26       2     1     5

# statistical inference

shapiro.test(data$CONP26)
# W = 0.8962, p-value = 3.765e-14
ks.test(scale(data$CONP26), "pnorm")
# D = 0.17479, p-value = 4.206e-09
lillie.test(data$CONP26)
# D = 0.17479, p-value < 2.2e-16

qqnorm(data$CONP26, las=1, pch=18, 
       main="CONP26 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$CONP26)

shapiro.test(data_col$CONP26)
# W = 0.90033, p-value = 3.105e-09
ks.test(scale(data_col$CONP26), "pnorm")
# D = 0.16682, p-value = 0.0001739
lillie.test(data_col$CONP26)
# D = 0.16682, p-value = 2.242e-12

qqnorm(data_col$CONP26, las=1, pch=18, 
       main="CONP26 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$CONP26)

shapiro.test(data_uclm$CONP26)
# W = 0.88669, p-value = 1.127e-09
ks.test(scale(data_uclm$CONP26), "pnorm")
# D = 0.2004, p-value = 5.683e-06
lillie.test(data_uclm$CONP26)
# D = 0.2004, p-value < 2.2e-16

qqnorm(data_uclm$CONP26, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$CONP26)

# non-parametric test
wilcox.test(data_col$CONP26,
            data_uclm$CONP26,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 13956, p-value = 0.47
# result of the non-parametric test
# there are statistically significant differences

# CONP27 consideras: Entornos personales de aprendizaje (Symbaloo, Netvibes,...) ----

data %>%
  select(Paises,CONP27)%>%
  na.omit()%>%
  summarise(n = length(CONP27),
            media = mean(CONP27),
            sd = sd(CONP27),
            mediana = median(CONP27),
            min = min(CONP27),
            max = max(CONP27))

# n media    sd mediana   min   max
# 327  2.60  1.23       3     1     5

data %>%
  select(Paises,CONP27)%>%
  na.omit()%>%
  groCONP_by(Paises)%>%
  summarise(n = length(CONP27),
            media = mean(CONP27),
            sd = sd(CONP27),
            mediana = median(CONP27),
            min = min(CONP27),
            max = max(CONP27))

# Paises       n media    sd mediana   min   max
# Colombia   168  2.63  1.22       3     1     5
# España     159  2.56  1.26       2     1     5

# statistical inference

shapiro.test(data$CONP27)
# W = 0.8962, p-value = 3.765e-14
ks.test(scale(data$CONP27), "pnorm")
# D = 0.17479, p-value = 4.206e-09
lillie.test(data$CONP27)
# D = 0.17479, p-value < 2.2e-16

qqnorm(data$CONP27, las=1, pch=18, 
       main="CONP27 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$CONP27)

shapiro.test(data_col$CONP27)
# W = 0.90033, p-value = 3.105e-09
ks.test(scale(data_col$CONP27), "pnorm")
# D = 0.16682, p-value = 0.0001739
lillie.test(data_col$CONP27)
# D = 0.16682, p-value = 2.242e-12

qqnorm(data_col$CONP27, las=1, pch=18, 
       main="CONP27 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$CONP27)

shapiro.test(data_uclm$CONP27)
# W = 0.88669, p-value = 1.127e-09
ks.test(scale(data_uclm$CONP27), "pnorm")
# D = 0.2004, p-value = 5.683e-06
lillie.test(data_uclm$CONP27)
# D = 0.2004, p-value < 2.2e-16

qqnorm(data_uclm$CONP27, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$CONP27)

# non-parametric test
wilcox.test(data_col$CONP27,
            data_uclm$CONP27,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 13956, p-value = 0.47
# result of the non-parametric test
# there are statistically significant differences

# CONP28 consideras: Alojamiento de archivos multiplataforma en la nube (Google drive, OneDrive, Dropbox,..) ----

data %>%
  select(Paises,CONP28)%>%
  na.omit()%>%
  summarise(n = length(CONP28),
            media = mean(CONP28),
            sd = sd(CONP28),
            mediana = median(CONP28),
            min = min(CONP28),
            max = max(CONP28))

# n media    sd mediana   min   max
# 327  2.60  1.23       3     1     5

data %>%
  select(Paises,CONP28)%>%
  na.omit()%>%
  groCONP_by(Paises)%>%
  summarise(n = length(CONP28),
            media = mean(CONP28),
            sd = sd(CONP28),
            mediana = median(CONP28),
            min = min(CONP28),
            max = max(CONP28))

# Paises       n media    sd mediana   min   max
# Colombia   168  2.63  1.22       3     1     5
# España     159  2.56  1.26       2     1     5

# statistical inference

shapiro.test(data$CONP28)
# W = 0.8962, p-value = 3.765e-14
ks.test(scale(data$CONP28), "pnorm")
# D = 0.17479, p-value = 4.206e-09
lillie.test(data$CONP28)
# D = 0.17479, p-value < 2.2e-16

qqnorm(data$CONP28, las=1, pch=18, 
       main="CONP28 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$CONP28)

shapiro.test(data_col$CONP28)
# W = 0.90033, p-value = 3.105e-09
ks.test(scale(data_col$CONP28), "pnorm")
# D = 0.16682, p-value = 0.0001739
lillie.test(data_col$CONP28)
# D = 0.16682, p-value = 2.242e-12

qqnorm(data_col$CONP28, las=1, pch=18, 
       main="CONP28 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$CONP28)

shapiro.test(data_uclm$CONP28)
# W = 0.88669, p-value = 1.127e-09
ks.test(scale(data_uclm$CONP28), "pnorm")
# D = 0.2004, p-value = 5.683e-06
lillie.test(data_uclm$CONP28)
# D = 0.2004, p-value < 2.2e-16

qqnorm(data_uclm$CONP28, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$CONP28)

# non-parametric test
wilcox.test(data_col$CONP28,
            data_uclm$CONP28,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 13956, p-value = 0.47
# result of the non-parametric test
# there are statistically significant differences

# CONP29 consideras: Plataformas educativas (Google classroom, BrainCert, Edmodo,…) ----

data %>%
  select(Paises,CONP29)%>%
  na.omit()%>%
  summarise(n = length(CONP29),
            media = mean(CONP29),
            sd = sd(CONP29),
            mediana = median(CONP29),
            min = min(CONP29),
            max = max(CONP29))

# n media    sd mediana   min   max
# 327  2.60  1.23       3     1     5

data %>%
  select(Paises,CONP29)%>%
  na.omit()%>%
  groCONP_by(Paises)%>%
  summarise(n = length(CONP29),
            media = mean(CONP29),
            sd = sd(CONP29),
            mediana = median(CONP29),
            min = min(CONP29),
            max = max(CONP29))

# Paises       n media    sd mediana   min   max
# Colombia   168  2.63  1.22       3     1     5
# España     159  2.56  1.26       2     1     5

# statistical inference

shapiro.test(data$CONP29)
# W = 0.8962, p-value = 3.765e-14
ks.test(scale(data$CONP29), "pnorm")
# D = 0.17479, p-value = 4.206e-09
lillie.test(data$CONP29)
# D = 0.17479, p-value < 2.2e-16

qqnorm(data$CONP29, las=1, pch=18, 
       main="CONP29 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$CONP29)

shapiro.test(data_col$CONP29)
# W = 0.90033, p-value = 3.105e-09
ks.test(scale(data_col$CONP29), "pnorm")
# D = 0.16682, p-value = 0.0001739
lillie.test(data_col$CONP29)
# D = 0.16682, p-value = 2.242e-12

qqnorm(data_col$CONP29, las=1, pch=18, 
       main="CONP29 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$CONP29)

shapiro.test(data_uclm$CONP29)
# W = 0.88669, p-value = 1.127e-09
ks.test(scale(data_uclm$CONP29), "pnorm")
# D = 0.2004, p-value = 5.683e-06
lillie.test(data_uclm$CONP29)
# D = 0.2004, p-value < 2.2e-16

qqnorm(data_uclm$CONP29, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$CONP29)

# non-parametric test
wilcox.test(data_col$CONP29,
            data_uclm$CONP29,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 13956, p-value = 0.47
# result of the non-parametric test
# there are statistically significant differences

# CONP30 consideras: Software educativo de autor (Cuadernia, EdiLIM, Ardora, Educaplay, eXeLearning, Malted, Jclic, Hot Potatoes,…) ----

data %>%
  select(Paises,CONP30)%>%
  na.omit()%>%
  summarise(n = length(CONP30),
            media = mean(CONP30),
            sd = sd(CONP30),
            mediana = median(CONP30),
            min = min(CONP30),
            max = max(CONP30))

# n media    sd mediana   min   max
# 327  2.60  1.23       3     1     5

data %>%
  select(Paises,CONP30)%>%
  na.omit()%>%
  groCONP_by(Paises)%>%
  summarise(n = length(CONP30),
            media = mean(CONP30),
            sd = sd(CONP30),
            mediana = median(CONP30),
            min = min(CONP30),
            max = max(CONP30))

# Paises       n media    sd mediana   min   max
# Colombia   168  2.63  1.22       3     1     5
# España     159  2.56  1.26       2     1     5

# statistical inference

shapiro.test(data$CONP30)
# W = 0.8962, p-value = 3.765e-14
ks.test(scale(data$CONP30), "pnorm")
# D = 0.17479, p-value = 4.206e-09
lillie.test(data$CONP30)
# D = 0.17479, p-value < 2.2e-16

qqnorm(data$CONP30, las=1, pch=18, 
       main="CONP30 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$CONP30)

shapiro.test(data_col$CONP30)
# W = 0.90033, p-value = 3.105e-09
ks.test(scale(data_col$CONP30), "pnorm")
# D = 0.16682, p-value = 0.0001739
lillie.test(data_col$CONP30)
# D = 0.16682, p-value = 2.242e-12

qqnorm(data_col$CONP30, las=1, pch=18, 
       main="CONP30 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$CONP30)

shapiro.test(data_uclm$CONP30)
# W = 0.88669, p-value = 1.127e-09
ks.test(scale(data_uclm$CONP30), "pnorm")
# D = 0.2004, p-value = 5.683e-06
lillie.test(data_uclm$CONP30)
# D = 0.2004, p-value < 2.2e-16

qqnorm(data_uclm$CONP30, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$CONP30)

# non-parametric test
wilcox.test(data_col$CONP30,
            data_uclm$CONP30,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 13956, p-value = 0.47
# result of the non-parametric test
# there are statistically significant differences

# CONP31 consideras: Actividades guiadas de búsqueda en Internet (Webquest,  caza del tesoro,…) ----

data %>%
  select(Paises,CONP31)%>%
  na.omit()%>%
  summarise(n = length(CONP31),
            media = mean(CONP31),
            sd = sd(CONP31),
            mediana = median(CONP31),
            min = min(CONP31),
            max = max(CONP31))

# n media    sd mediana   min   max
# 327  2.60  1.23       3     1     5

data %>%
  select(Paises,CONP31)%>%
  na.omit()%>%
  groCONP_by(Paises)%>%
  summarise(n = length(CONP31),
            media = mean(CONP31),
            sd = sd(CONP31),
            mediana = median(CONP31),
            min = min(CONP31),
            max = max(CONP31))

# Paises       n media    sd mediana   min   max
# Colombia   168  2.63  1.22       3     1     5
# España     159  2.56  1.26       2     1     5

# statistical inference

shapiro.test(data$CONP31)
# W = 0.8962, p-value = 3.765e-14
ks.test(scale(data$CONP31), "pnorm")
# D = 0.17479, p-value = 4.206e-09
lillie.test(data$CONP31)
# D = 0.17479, p-value < 2.2e-16

qqnorm(data$CONP31, las=1, pch=18, 
       main="CONP31 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$CONP31)

shapiro.test(data_col$CONP31)
# W = 0.90033, p-value = 3.105e-09
ks.test(scale(data_col$CONP31), "pnorm")
# D = 0.16682, p-value = 0.0001739
lillie.test(data_col$CONP31)
# D = 0.16682, p-value = 2.242e-12

qqnorm(data_col$CONP31, las=1, pch=18, 
       main="CONP31 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$CONP31)

shapiro.test(data_uclm$CONP31)
# W = 0.88669, p-value = 1.127e-09
ks.test(scale(data_uclm$CONP31), "pnorm")
# D = 0.2004, p-value = 5.683e-06
lillie.test(data_uclm$CONP31)
# D = 0.2004, p-value < 2.2e-16

qqnorm(data_uclm$CONP31, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$CONP31)

# non-parametric test
wilcox.test(data_col$CONP31,
            data_uclm$CONP31,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 13956, p-value = 0.47
# result of the non-parametric test
# there are statistically significant differences

# CONP32 consideras: Mapas conceptuales (cmaptool, mindomo, bubbl.us, MindMeister,…) ----

data %>%
  select(Paises,CONP32)%>%
  na.omit()%>%
  summarise(n = length(CONP32),
            media = mean(CONP32),
            sd = sd(CONP32),
            mediana = median(CONP32),
            min = min(CONP32),
            max = max(CONP32))

# n media    sd mediana   min   max
# 327  2.60  1.23       3     1     5

data %>%
  select(Paises,CONP32)%>%
  na.omit()%>%
  groCONP_by(Paises)%>%
  summarise(n = length(CONP32),
            media = mean(CONP32),
            sd = sd(CONP32),
            mediana = median(CONP32),
            min = min(CONP32),
            max = max(CONP32))

# Paises       n media    sd mediana   min   max
# Colombia   168  2.63  1.22       3     1     5
# España     159  2.56  1.26       2     1     5

# statistical inference

shapiro.test(data$CONP32)
# W = 0.8962, p-value = 3.765e-14
ks.test(scale(data$CONP32), "pnorm")
# D = 0.17479, p-value = 4.206e-09
lillie.test(data$CONP32)
# D = 0.17479, p-value < 2.2e-16

qqnorm(data$CONP32, las=1, pch=18, 
       main="CONP32 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$CONP32)

shapiro.test(data_col$CONP32)
# W = 0.90033, p-value = 3.105e-09
ks.test(scale(data_col$CONP32), "pnorm")
# D = 0.16682, p-value = 0.0001739
lillie.test(data_col$CONP32)
# D = 0.16682, p-value = 2.242e-12

qqnorm(data_col$CONP32, las=1, pch=18, 
       main="CONP32 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$CONP32)

shapiro.test(data_uclm$CONP32)
# W = 0.88669, p-value = 1.127e-09
ks.test(scale(data_uclm$CONP32), "pnorm")
# D = 0.2004, p-value = 5.683e-06
lillie.test(data_uclm$CONP32)
# D = 0.2004, p-value < 2.2e-16

qqnorm(data_uclm$CONP32, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$CONP32)

# non-parametric test
wilcox.test(data_col$CONP32,
            data_uclm$CONP32,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 13956, p-value = 0.47
# result of the non-parametric test
# there are statistically significant differences

# CONP33 consideras: Realidad aumentada ----

data %>%
  select(Paises,CONP33)%>%
  na.omit()%>%
  summarise(n = length(CONP33),
            media = mean(CONP33),
            sd = sd(CONP33),
            mediana = median(CONP33),
            min = min(CONP33),
            max = max(CONP33))

# n media    sd mediana   min   max
# 327  2.60  1.23       3     1     5

data %>%
  select(Paises,CONP33)%>%
  na.omit()%>%
  groCONP_by(Paises)%>%
  summarise(n = length(CONP33),
            media = mean(CONP33),
            sd = sd(CONP33),
            mediana = median(CONP33),
            min = min(CONP33),
            max = max(CONP33))

# Paises       n media    sd mediana   min   max
# Colombia   168  2.63  1.22       3     1     5
# España     159  2.56  1.26       2     1     5

# statistical inference

shapiro.test(data$CONP33)
# W = 0.8962, p-value = 3.765e-14
ks.test(scale(data$CONP33), "pnorm")
# D = 0.17479, p-value = 4.206e-09
lillie.test(data$CONP33)
# D = 0.17479, p-value < 2.2e-16

qqnorm(data$CONP33, las=1, pch=18, 
       main="CONP33 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$CONP33)

shapiro.test(data_col$CONP33)
# W = 0.90033, p-value = 3.105e-09
ks.test(scale(data_col$CONP33), "pnorm")
# D = 0.16682, p-value = 0.0001739
lillie.test(data_col$CONP33)
# D = 0.16682, p-value = 2.242e-12

qqnorm(data_col$CONP33, las=1, pch=18, 
       main="CONP33 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$CONP33)

shapiro.test(data_uclm$CONP33)
# W = 0.88669, p-value = 1.127e-09
ks.test(scale(data_uclm$CONP33), "pnorm")
# D = 0.2004, p-value = 5.683e-06
lillie.test(data_uclm$CONP33)
# D = 0.2004, p-value < 2.2e-16

qqnorm(data_uclm$CONP33, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$CONP33)

# non-parametric test
wilcox.test(data_col$CONP33,
            data_uclm$CONP33,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 13956, p-value = 0.47
# result of the non-parametric test
# there are statistically significant differences

# CONP34 consideras: Códigos QR ----

data %>%
  select(Paises,CONP34)%>%
  na.omit()%>%
  summarise(n = length(CONP34),
            media = mean(CONP34),
            sd = sd(CONP34),
            mediana = median(CONP34),
            min = min(CONP34),
            max = max(CONP34))

# n media    sd mediana   min   max
# 327  2.60  1.23       3     1     5

data %>%
  select(Paises,CONP34)%>%
  na.omit()%>%
  groCONP_by(Paises)%>%
  summarise(n = length(CONP34),
            media = mean(CONP34),
            sd = sd(CONP34),
            mediana = median(CONP34),
            min = min(CONP34),
            max = max(CONP34))

# Paises       n media    sd mediana   min   max
# Colombia   168  2.63  1.22       3     1     5
# España     159  2.56  1.26       2     1     5

# statistical inference

shapiro.test(data$CONP34)
# W = 0.8962, p-value = 3.765e-14
ks.test(scale(data$CONP34), "pnorm")
# D = 0.17479, p-value = 4.206e-09
lillie.test(data$CONP34)
# D = 0.17479, p-value < 2.2e-16

qqnorm(data$CONP34, las=1, pch=18, 
       main="CONP34 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$CONP34)

shapiro.test(data_col$CONP34)
# W = 0.90033, p-value = 3.105e-09
ks.test(scale(data_col$CONP34), "pnorm")
# D = 0.16682, p-value = 0.0001739
lillie.test(data_col$CONP34)
# D = 0.16682, p-value = 2.242e-12

qqnorm(data_col$CONP34, las=1, pch=18, 
       main="CONP34 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$CONP34)

shapiro.test(data_uclm$CONP34)
# W = 0.88669, p-value = 1.127e-09
ks.test(scale(data_uclm$CONP34), "pnorm")
# D = 0.2004, p-value = 5.683e-06
lillie.test(data_uclm$CONP34)
# D = 0.2004, p-value < 2.2e-16

qqnorm(data_uclm$CONP34, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$CONP34)

# non-parametric test
wilcox.test(data_col$CONP34,
            data_uclm$CONP34,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 13956, p-value = 0.47
# result of the non-parametric test
# there are statistically significant differences

# CONP35 consideras: Gamificación ----

data %>%
  select(Paises,CONP35)%>%
  na.omit()%>%
  summarise(n = length(CONP35),
            media = mean(CONP35),
            sd = sd(CONP35),
            mediana = median(CONP35),
            min = min(CONP35),
            max = max(CONP35))

# n media    sd mediana   min   max
# 327  2.60  1.23       3     1     5

data %>%
  select(Paises,CONP35)%>%
  na.omit()%>%
  groCONP_by(Paises)%>%
  summarise(n = length(CONP35),
            media = mean(CONP35),
            sd = sd(CONP35),
            mediana = median(CONP35),
            min = min(CONP35),
            max = max(CONP35))

# Paises       n media    sd mediana   min   max
# Colombia   168  2.63  1.22       3     1     5
# España     159  2.56  1.26       2     1     5

# statistical inference

shapiro.test(data$CONP35)
# W = 0.8962, p-value = 3.765e-14
ks.test(scale(data$CONP35), "pnorm")
# D = 0.17479, p-value = 4.206e-09
lillie.test(data$CONP35)
# D = 0.17479, p-value < 2.2e-16

qqnorm(data$CONP35, las=1, pch=18, 
       main="CONP35 of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data$CONP35)

shapiro.test(data_col$CONP35)
# W = 0.90033, p-value = 3.105e-09
ks.test(scale(data_col$CONP35), "pnorm")
# D = 0.16682, p-value = 0.0001739
lillie.test(data_col$CONP35)
# D = 0.16682, p-value = 2.242e-12

qqnorm(data_col$CONP35, las=1, pch=18, 
       main="CONP35 col of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_col$CONP35)

shapiro.test(data_uclm$CONP35)
# W = 0.88669, p-value = 1.127e-09
ks.test(scale(data_uclm$CONP35), "pnorm")
# D = 0.2004, p-value = 5.683e-06
lillie.test(data_uclm$CONP35)
# D = 0.2004, p-value < 2.2e-16

qqnorm(data_uclm$CONP35, las=1, pch=18, 
       main="age uclm of the teachers", font.main=1,
       xlab="Theoretical quantiles", ylab="Sample quantiles") 
qqline(data_uclm$CONP35)

# non-parametric test
wilcox.test(data_col$CONP35,
            data_uclm$CONP35,paired = F,
            exact = F,correct = T,conf.int = 0.95)

# W = 13956, p-value = 0.47
# result of the non-parametric test
# there are statistically significant differences