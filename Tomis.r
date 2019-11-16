require(tidyverse)
require(readxl)

##the next function converts the raw dataset into a new one by making the returns from day n to day n + 1
##the first column remains the same as it is our date vector, and the first row is empty as we don't have data before the first day
## -!-this function is copied from out pprevious work because this code calculated the correlation with prices
## in this code i have changed ret_WTI_fut output name to WTI2 because this is the basic parameter in the original code

return_maker <- function(WTI_fut)
{
    n <- nrow(WTI_fut)
  m <- ncol(WTI_fut)
  ret_WTI_fut <- matrix(nrow = n, ncol = m)
  for (i in 2:n)
  {
    for (j in 2:m)
    {
      ret_WTI_fut[i,j] <- (WTI_fut[[i,j]] / WTI_fut[[i-1,j]]) - 1
    }
  }
  
  ret_WTI_fut <- cbind(WTI_fut[,1], ret_WTI_fut)
  WTI2 <<- ret_WTI_fut[-1,-2]
  colnames(WTI2) = colnames(WTI_fut)
 # View(ret_WTI_fut)
  return(WTI2)
}


#Lellen??rzi, hogy fent van-e tidyverse, ha nem, telepíti
tidyverse_check_installer <- function()
{
  my_packages <- library()$results
  n = length(my_packages)
  readpackage = 0
  for(i in 1:n)
  {
    if (my_packages[i] == "tidyverse")
    {
      readpackage <- i
    }
  }
  
  if (readpackage==0){
    # readxl package is not installed, so now we have to
    install.packages("tidyverse")
  }
  
}


#kezdo_datum = "2015-01-01" #paraméter, mikortól kezdõdjön a korreláció számítása
#veg_datum = "2016-12-31" #paraméter, meddig tartson a korreláció számítása
#kesleltet<-100 #paraméter, mennyivel történjen a késleltetés
#ablak_meret<-100 #paraméter, mekkora idõintervallumon történjen a korreláció számítása
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
    
    #ellen??rizzük be van-e töltve tidyverse, ha nincs, betöltjük
    tidyverse_check_installer()
    
    
    # Átalakítjuk dátummá
    kezdo_datum = as.Date(kezdo_datum)
    veg_datum = as.Date(veg_datum)
    
    # Beolvassuk a fájlt
    WTI2 <- readxl::read_excel("WTI2.xlsx")
    WTI2 <<-return_maker(WTI2)
    adat_kezdo = as.Date(WTI2[1, 1]$Date) #legelsõ megfigyelés idõpontja
    adat_vegso = as.Date(WTI2[length(WTI2[[1]]), 1]$Date) #utolsó megfigyelés idõpontja
    
    #changed the start-end date to our because i still dont know what dataset we will use
    
    # Leellenõrizzük, hogy a paramétereink megfelelõek-e
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
    
    #szám formátumban megkapom a paraméter kezdõ dátum és a legelsõ megfigyelés közötti idõt
    decent <-
      as.integer(as.Date(kezdo_datum) - as.Date(adat_kezdo))
    i = decent + kesleltet + 1#az elsõ adatsort eltolom a késleltetéssel, és a kezdõ dátummal
    output = NULL
    
    # Szám formátumban megkapom a paraméter végsõ dátum és a legelsõ megfigyelés közötti idõt
    vegso <-
      as.integer(as.Date(veg_datum) - as.Date(adat_kezdo)) + 1
    
    # A results változóba kapjuk vissza a korrelációkat X és Y adatoszlop között
    # tipp: elég csak a CL* formában megadni ezt a két számot a program automatikusan
    # hozzáad egyet, hogy megkapja az oszlopok indexét az adatokban (tehát nem szükséges CL(* - 1))
    results = calculate_correlation(vegso, ablak_meret, kesleltet, i, X, Y)
    
    # Csak egy külön változóba kitesszük a dátumokat
    dates <-
      as.Date(WTI2$Date[(decent + ablak_meret + kesleltet + 1):vegso])
    
    # Ez lesz az idõsor (dátum- és korrelációs vektor)
    data_plot <- cbind(dates, results)
    
    # Hogy jobban értse az olvasó a kódot megváltoztatjuk az oszlopok neveit
    colnames(data_plot) <- c("Dates", "Correlations")
    
    
    # Átalakítjuk az adathalmazunk tibble-re, hogy szebben tudjuk ábrázolni.
    tibble_data <- as_tibble(data_plot)
    
    tibble_data$Dates <-
      as.Date(tibble_data$Dates, origin = "1970-01-01")
    
    # Úgy kerül ábrázolásra, hogy a kezdõ idõpont +eltolás+ablak mérete lesz az elsõ idõpont,
    # amire korreláció lesz, és az utolsó pedig a paraméterként megadott utolsó idõpontra lesz
    plot_data_function(tibble_data,
                       kezdo_datum,
                       veg_datum,
                       kesleltet,
                       ablak_meret,
                       X,
                       Y)
  }
#problem1: wti2 matrix does not get generated in function but <<- sign was used

# Ez a függvény ellenõrzi le, hogy a paraméterek megfelelõen vannak-e megadva
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
    # Leellenõrizzük, hogy karakter formátumokba kaptuk-e a kezdõ és vég dátumokat, tehát át tudtuk alakítani
    if (typeof(kezdo_datum) != "double" || typeof(veg_datum) != "double") {
      print("Karakter formátumba adja meg a kezdõ és végdátumokat pl: \"2010-01-01\"")
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
      
      # Leellenõrizzük, hogy a többi paramétert egész szám formátumban adta meg
    } else if (typeof(kesleltet) != "double" ||
               typeof(ablak_meret) != "double" ||
               typeof(X) != "double" || typeof(Y) != "double") {
      print("Kérjük a dátumokon kívüli paramétereket egész számok formájában adja meg.")
      return(FALSE)
      
      # Ellenõrizzük, hogy megfelelõ intevallumban kaptuk-e meg a paramétereket (pl. az ablak_meret nem negatív)
    } else if (kesleltet < 0 || X < 1 || X > 24 || Y < 1 || Y > 24 ) {
      print("Kérjük megfelelõ intevallumban adja meg a paramétereket (pl. a késleltetés ne legyen negatív).")
      return(FALSE)
      
      # Ablak méret ellenõrzése, 5-nél kisebbet nem fogadunk el.
    } else if (ablak_meret <= 4) {
      print("Az ablak_meret nem lehet 5-nél kisebb.")
      return(FALSE)
      
      # Ellenõrizzük, hogy az ablak méret és a késleltetés összege kisebb nagyobb-e, mint az elemzendõ intervallum
    } else if (as.integer(as.Date(veg_datum) - as.Date(kezdo_datum)) <= (kesleltet + ablak_meret)) {
      print("Adjon meg bõvebb intervallumot, vagy csekélyebb ablak méretet és késleltetést!")
      return (FALSE)
    }
    return(TRUE)
  } # beni: ez fasza nagyon, maradhat


# Ebben a függvényben számoljuk ki a korrelációt. Visszatérésként a korrelációs vektort adja.
# tipp: elég csak a CL* formában megadni ezt a két számot a program automatikusan
# hozzáad egyet, hogy megkapja az oszlopok indexét az adatokban. Tehát ha X = 1 és
# Y = 2 a kapott paraméter, akkor CL1 és CL2 között számol.
calculate_correlation <-
  function(ablak_meret = 100,
           kesleltet = 0,
           i = 1,
           the_data = WTI2) { #beni: átírtam a the_data-t a hozam matrixra azaz WTI2-re. 
    vegso=nrow(WTI2)-1 #beni: egy oszlop címeknek megy 
    output = NULL
    CorMatrixCol=((n)/2*(n-1)) +1# this is the number of columns that contains correlations +1 as date vector [first one]
    pairedCorrelation=matrix(nrow=m,ncol=CorMatrixCol)
    #pairedCorrelation[,1]=WTI2[,1]# fill up the matrix with our dates from the return matrix
    #warning: NEM RAK BELÉJE DÁTIÓUMOT A GENNYLÁTA
    z=2
    j=1
    i=1
    while(j<n){
      for (k in (j+1):n){
        
        while (i <= vegso - ablak_meret) {
          #itt számolódik a korreláció
          pairedCorrelation[i,z] = cor(the_data[[1 + j]][i:(ablak_meret + i)], the_data[[1 + k]][(i - kesleltet):(ablak_meret + i - kesleltet)])
          # output = rbind(output, correlation) # ez lesz a korreláció vektora
          # pairedCorrelation[i,z]=correlation
          i = i + 1 # Tovább lépés
          
        }
        i=1
        z=z+1
        
      } 
      #k=0
      j=j+1   
    }
    # Annyiszor kell korrelációt futtatni, amennyi az utolsó megadott dátum, és az eltolás+késleltetés+kezdõ dátum (paraméter) közötti idõ
    
    
    #beni : itt beraktam a párosító korrelációs izét
    # Ellenõrzés, hogy nagyjából megfelelõ erdményt kaptunk-e.
    # Ha nem, akkor megszakítja a program futását.
    
    #beni: if check_results részt kivettem mert szerintem nem fog kelleni,ez a komment felett volt
    #beni: vegso=2557 ...:(. 2557 hasznos sor van az ár mátrixban, nekünk a hozamnál 2556 lesz úgyhogy nem lehet ezt bent hagyni így
    #beni: a vegso parameter mostmár egy sor számolás lesz abból a mátrixból ami alapból van-1 a sornév miatt.
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
} # erre egyáltalán van szükség?


# Ábrázolás, a paraméterek kiírásához szükséges azok átadása, paraméterként.
plot_data_function <-
  function(the_data,
           loc_kezdo_datum,
           loc_veg_datum,
           loc_kesleltet,
           loc_ablak_meret,
           loc_X,
           loc_Y) {
    
    gg <- ggplot2(the_data) +
      aeys(x = Dates, y = Correlations) +
      geom_line(color = "#1f9e98", size = 1) +
      labs(
        title = "Dinamikus Korreláció",
        subtitle = paste(
          "Kezdõ dátum: ",
          loc_kezdo_datum,
          "\nVégsõ dátum: ",
          loc_veg_datum,
          "\nKésleltetés: ",
          loc_kesleltet,
          "\nRolling window: ",
          loc_ablak_meret,
          "\nElsõ adathalmaz: ",
          loc_X,
          "\nMásodik adathalmaz: ",
          loc_Y
        )
      ) + 
      scale_x_date(date_breaks = "1 year", date_labels = "%Y")
    
    plot(gg)
    
  }


korrelacio("2010-01-01", "2016-12-31", 0, 10)