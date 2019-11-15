require(tidyverse)
require(readxl)

#kezdo_datum = "2015-01-01" #paraméter, mikortól kezdődjön a korreláció számítása
#veg_datum = "2016-12-31" #paraméter, meddig tartson a korreláció számítása
#kesleltet<-100 #paraméter, mennyivel történjen a késleltetés
#ablak_meret<-100 #paraméter, mekkora időintervallumon történjen a korreláció számítása
#X = 1 #paraméter, melyik eszköz (ez lesz eltolva)
#Y = 2 #paraméter, melyik másik eszköz
#tipp: az ablak_meret 5-nél nem lehet kisebb

korrelacio <-
  function(kezdo_datum = "2010-01-01",
           veg_datum = "2016-12-31",
           kesleltet = 0,
           ablak_meret = 100,
           X = 1,
           Y = 2) {
    
    
    # Átalakítjuk dátummá
    kezdo_datum = as.Date(kezdo_datum)
    veg_datum = as.Date(veg_datum)
    
    # Beolvassuk a fájlt
    WTI2 <- readxl::read_excel("WTI2.xlsx")
    
    adat_kezdo = as.Date(WTI2[1, 1]$Date) #legelső megfigyelés időpontja
    adat_vegso = as.Date(WTI2[length(WTI2[[1]]), 1]$Date) #utolsó megfigyelés időpontja
    
    
    # Leellenőrizzük, hogy a paramétereink megfelelőek-e
    if (!check_parameters(adat_kezdo,
                          adat_vegso,
                          kezdo_datum,
                          veg_datum,
                          kesleltet,
                          ablak_meret,
                          X,
                          Y)) {
      stop("RIP")
    }
    
    #szám formátumban megkapom a paraméter kezdő dátum és a legelső megfigyelés közötti időt
    decent <-
      as.integer(as.Date(kezdo_datum) - as.Date(adat_kezdo))
    i = decent + kesleltet + 1#az első adatsort eltolom a késleltetéssel, és a kezdő dátummal
    output = NULL
    
    # Szám formátumban megkapom a paraméter végső dátum és a legelső megfigyelés közötti időt
    vegso <-
      as.integer(as.Date(veg_datum) - as.Date(adat_kezdo)) + 1
    
    # A results változóba kapjuk vissza a korrelációkat X és Y adatoszlop között
    # tipp: elég csak a CL* formában megadni ezt a két számot a program automatikusan
    # hozzáad egyet, hogy megkapja az oszlopok indexét az adatokban (tehát nem szükséges CL(* - 1))
    results = calculate_correlation(vegso, ablak_meret, kesleltet, i, X, Y)
    
    # Csak egy külön változóba kitesszük a dátumokat
    dates <-
      as.Date(WTI2$Date[(decent + ablak_meret + kesleltet + 1):vegso])
    
    # Ez lesz az idősor (dátum- és korrelációs vektor)
    data_plot <- cbind(dates, results)
    
    # Hogy jobban értse az olvasó a kódot megváltoztatjuk az oszlopok neveit
    colnames(data_plot) <- c("Dates", "Correlations")
    
    
    # Átalakítjuk az adathalmazunk tibble-re, hogy szebben tudjuk ábrázolni.
    tibble_data <- as_tibble(data_plot)
    
    tibble_data$Dates <-
      as.Date(tibble_data$Dates, origin = "1970-01-01")
    
    # Úgy kerül ábrázolásra, hogy a kezdő időpont +eltolás+ablak mérete lesz az első időpont,
    # amire korreláció lesz, és az utolsó pedig a paraméterként megadott utolsó időpontra lesz
    plot_data_function(tibble_data,
                       kezdo_datum,
                       veg_datum,
                       kesleltet,
                       ablak_meret,
                       X,
                       Y)
  }


# Ez a függvény ellenőrzi le, hogy a paraméterek megfelelően vannak-e megadva
# Ha valamelyik hibás, akkor FALSE visszatérési értéket ad, ezután a program
# egy hibaüzenet ír ki és kilép
check_parameters <-
  function(adat_kezdo,
           adat_vegso,
           kezdo_datum,
           veg_datum,
           kesleltet,
           ablak_meret,
           X,
           Y) {
    # Leellenőrizzük, hogy karakter formátumokba kaptuk-e a kezdő és vég dátumokat, tehát át tudtuk alakítani
    if (typeof(kezdo_datum) != "double" || typeof(veg_datum) != "double") {
      print("Karakter formátumba adja meg a kezdő és végdátumokat pl: \"2010-01-01\"")
      return(FALSE)
      
      # Ha karakterek, akkor megnézzük, hogy a fájlban megadott intervallumba esnek-e
    } else if (kezdo_datum < adat_kezdo || veg_datum > adat_vegso) {
      print(
        paste(
          "Kérjük olyan dátumot adjon meg, ami az elemzés intervallumába beleesik:",
          adat_kezdo,
          "és",
          adat_vegso,
          "között."
        )
      )
      return(FALSE)
      
      # Leellenőrizzük, hogy a többi paramétert egész szám formátumban adta meg
    } else if (typeof(kesleltet) != "double" ||
               typeof(ablak_meret) != "double" ||
               typeof(X) != "double" || typeof(Y) != "double") {
      print("Kérjük a dátumokon kívüli paramétereket egész számok formájában adja meg.")
      return(FALSE)
      
      # Ellenőrizzük, hogy megfelelő intevallumban kaptuk-e meg a paramétereket (pl. az ablak_meret nem negatív)
    } else if (kesleltet < 0 || X < 1 || X > 24 || Y < 1 || Y > 24 ) {
      print("Kérjük megfelelő intevallumban adja meg a paramétereket (pl. a késleltetés ne legyen negatív).")
      return(FALSE)
      
      # Ablak méret ellenőrzése, 5-nél kisebbet nem fogadunk el.
    } else if (ablak_meret <= 4) {
      print("Az ablak_meret nem lehet 5-nél kisebb.")
      return(FALSE)
      
      # Ellenőrizzük, hogy az ablak méret és a késleltetés összege kisebb nagyobb-e, mint az elemzendő intervallum
    } else if (as.integer(as.Date(veg_datum) - as.Date(kezdo_datum)) <= (kesleltet + ablak_meret)) {
      print("Adjon meg bővebb intervallumot, vagy csekélyebb ablak méretet és késleltetést!")
      return (FALSE)
    }
    return(TRUE)
  }


# Ebben a függvényben számoljuk ki a korrelációt. Visszatérésként a korrelációs vektort adja.
# tipp: elég csak a CL* formában megadni ezt a két számot a program automatikusan
# hozzáad egyet, hogy megkapja az oszlopok indexét az adatokban. Tehát ha X = 1 és
# Y = 2 a kapott paraméter, akkor CL1 és CL2 között számol.
calculate_correlation <-
  function(vegso = 2557,
           ablak_meret = 100,
           kesleltet = 0,
           i = 1,
           X,
           Y,
           the_data = readxl::read_excel("WTI2.xlsx")) {
    output = NULL
    # Annyiszor kell korrelációt futtatni, amennyi az utolsó megadott dátum, és az eltolás+késleltetés+kezdő dátum (paraméter) közötti idő
    while (i <= vegso - ablak_meret) {
      #itt számolódik a korreláció
      correlation = cor(the_data[[1 + X]][i:(ablak_meret + i)], the_data[[1 + Y]][(i - kesleltet):(ablak_meret + i - kesleltet)])
      output = rbind(output, correlation) # ez lesz a korreláció vektora
      i = i + 1 # Tovább lépés
    }
    
    # Ellenőrzés, hogy nagyjából megfelelő erdményt kaptunk-e.
    # Ha nem, akkor megszakítja a program futását.
    if(check_results(output)){
      return(output)
    } else {
      print("A korreláció számítása során nem jó adatokat kaptunk.")
      stop("Wrong Results")
    }
    
  }

# Egy rövid vizsgálat, hogy az eredményeink megfelelnek-e a valóságnak
# A korreláció [-1, 1] intervallumon mozog.
check_results <- function(results) {
  
  # Az eredmény mátrixon végigiterálva, megnézzük van-e 1 nél nagyobb
  # vagy 0-nál kisebb szám.
  for(row in 1:nrow(results)){
    if (results[row, 1] < -1.0000 || results[row, 1] > 1.0000){
      return(FALSE)
    }
  }
  
  return(TRUE)
}


# Ábrázolás, a paraméterek kiírásához szükséges azok átadása, paraméterként.
plot_data_function <-
  function(the_data,
           loc_kezdo_datum,
           loc_veg_datum,
           loc_kesleltet,
           loc_ablak_meret,
           loc_X,
           loc_Y) {
    
    gg <- ggplot(the_data) +
      aes(x = Dates, y = Correlations) +
      geom_line(color = "#1f9e98", size = 1) +
      labs(
        title = "Dinamikus Korreláció",
        subtitle = paste(
          "Kezdő dátum: ",
          loc_kezdo_datum,
          "\nVégső dátum: ",
          loc_veg_datum,
          "\nKésleltetés: ",
          loc_kesleltet,
          "\nRolling window: ",
          loc_ablak_meret,
          "\nElső adathalmaz: ",
          loc_X,
          "\nMásodik adathalmaz: ",
          loc_Y
        )
      ) + 
    scale_x_date(date_breaks = "1 year", date_labels = "%Y")
    
    plot(gg)
    
  }


korrelacio("2010-01-01", "2016-12-31", 0, 10, 1, 2)
