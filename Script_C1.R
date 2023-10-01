# Fixer le repertoir de travail
setwd("~/COURS DE R_2022-2023/Cours_R_C1")

#la liste des librairies qui sont déja installés
library()

# Les packages à utiliser
install.packages("map") #
install.packages("fields")
install.packages("spam")# 

#afficher les contours du monde
map()

# afficher les contours des USA et de la France
map("usa")
map("france")

#Exemple
 # il cherche des jeux de données sur des polluant atmospherique
data(ozone)
# conçevoir la carte
map("state", xlim = range(ozone$x), ylim = range(ozone$y))

#afficher les chiffres sur la carte
text(ozone$x, ozone$y, ozone$median)

#afficher le contenu de la données
ozone

#le cadre de la carte 
box()
-----------## EXEMPLE DE CARTOGRAPHIE ---------------
if(require(mapproj)) { # mapproj is used for projection="polyconic"
  # color US county map by 2009 unemployment rate
  # match counties to map using FIPS county codes
  # Based on J's solution to the "Choropleth Challenge"
  # http://blog.revolutionanalytics.com/2009/11/choropleth-challenge-result.html
  # load data
  # unemp includes data for some counties not on the "lower 48 states" county
  # map, such as those in Alaska, Hawaii, Puerto Rico, and some tiny Virginia
  # cities
  data(unemp)
  data(county.fips)
  # define color buckets
  colors = c("#F1EEF6", "#D4B9DA", "#C994C7", "#DF65B0", "#DD1C77", "#980043")
  unemp$colorBuckets <- as.numeric(cut(unemp$unemp, c(0, 2, 4, 6, 8, 10, 100)))
  leg.txt <- c("<2%", "2-4%", "4-6%", "6-8%", "8-10%", ">10%")
  # align data with map definitions by (partial) matching state,county
  # names, which include multiple polygons for some counties
  cnty.fips <- county.fips$fips[match(map("county", plot=FALSE)$names,
                                      county.fips$polyname)]
  colorsmatched <- unemp$colorBuckets [match(cnty.fips, unemp$fips)]
  # draw map
  map("county", col = colors[colorsmatched], fill = TRUE, resolution = 0,
      lty = 0, projection = "polyconic")
  map("state", col = "white", fill = FALSE, add = TRUE, lty = 1, lwd = 0.2,
      projection="polyconic")
  title("unemployment by county, 2009")
  legend("topright", leg.txt, horiz = TRUE, fill = colors)
  # Choropleth Challenge example, based on J's solution, see:
  # http://blog.revolutionanalytics.com/2009/11/choropleth-challenge-result.html
  # To see the faint county boundaries, use RGui menu: File/SaveAs/PDF
}
as.numeric(unemp$unemp)

### FONCTION MATHEMAT
#sqrt : racine carré
sqrt((sum(128+85)*3)) ## Ajouter 128 à 85 multiplier par 3 et ajouter le carré de ce resulatat

## Telecharger des données
# Download the shapefile. (note that I store it in a folder called DATA. You have to change that if needed.)
download.file("http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip" ,
              destfile="COURS DE R_2022-2023/Cours_R_C1/world_shape_file.zip")

##l'uriel de telechargement des données 
# Spécifiez l'URL où le fichier est stocké 
url <-"http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip"
# Spécifiez la destination où le fichier doit être enregistré 
destfile <-  "C:/Users/geographie/Documents/COURS DE R_2022-2023/Cours_R_C1/world_shape_file.zip"
# Apply download.file function in R
download.file(url, destfile)
# You now have it in your current working directory, have a look!
# Unzip this file. You can do it with R (as below), or clicking on the object you downloaded.
system("Cours_R_C1/world_shape_file.zip")
#  -- > You now have 4 files. One of these files is a .shp file! (TM_WORLD_BORDERS_SIMPL-0.3.shp)



