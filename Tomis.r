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


#Lellen??rzi, hogy fent van-e tidyverse, ha nem, telep�ti
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


#kezdo_datum = "2015-01-01" #param�ter, mikort�l kezd�dj�n a korrel�ci� sz�m�t�sa
#veg_datum = "2016-12-31" #param�ter, meddig tartson a korrel�ci� sz�m�t�sa
#kesleltet<-100 #param�ter, mennyivel t�rt�njen a k�sleltet�s
#ablak_meret<-100 #param�ter, mekkora id�intervallumon t�rt�njen a korrel�ci� sz�m�t�sa
#X = 1 #param�ter, melyik eszk�z (ez lesz eltolva)
#Y = 2 #param�ter, melyik m�sik eszk�z
#tipp: az ablak_meret 5-n�l nem lehet kisebb



korrelacio <-
  function(kezdo_datum = "2010-01-01",
           veg_datum = "2016-12-31",
           kesleltet = 0,
           ablak_meret = 100,
           X = 1,
           Y = 2) {
    
    #ellen??rizz�k be van-e t�ltve tidyverse, ha nincs, bet�ltj�k
    tidyverse_check_installer()
    
    
    # �talak�tjuk d�tumm�
    kezdo_datum = as.Date(kezdo_datum)
    veg_datum = as.Date(veg_datum)
    
    # Beolvassuk a f�jlt
    WTI2 <- readxl::read_excel("WTI2.xlsx")
    WTI2 <<-return_maker(WTI2)
    adat_kezdo = as.Date(WTI2[1, 1]$Date) #legels� megfigyel�s id�pontja
    adat_vegso = as.Date(WTI2[length(WTI2[[1]]), 1]$Date) #utols� megfigyel�s id�pontja
    
    #changed the start-end date to our because i still dont know what dataset we will use
    
    # Leellen�rizz�k, hogy a param�tereink megfelel�ek-e
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
    
    #sz�m form�tumban megkapom a param�ter kezd� d�tum �s a legels� megfigyel�s k�z�tti id�t
    decent <-
      as.integer(as.Date(kezdo_datum) - as.Date(adat_kezdo))
    i = decent + kesleltet + 1#az els� adatsort eltolom a k�sleltet�ssel, �s a kezd� d�tummal
    output = NULL
    
    # Sz�m form�tumban megkapom a param�ter v�gs� d�tum �s a legels� megfigyel�s k�z�tti id�t
    vegso <-
      as.integer(as.Date(veg_datum) - as.Date(adat_kezdo)) + 1
    
    # A results v�ltoz�ba kapjuk vissza a korrel�ci�kat X �s Y adatoszlop k�z�tt
    # tipp: el�g csak a CL* form�ban megadni ezt a k�t sz�mot a program automatikusan
    # hozz�ad egyet, hogy megkapja az oszlopok index�t az adatokban (teh�t nem sz�ks�ges CL(* - 1))
    results = calculate_correlation(vegso, ablak_meret, kesleltet, i, X, Y)
    
    # Csak egy k�l�n v�ltoz�ba kitessz�k a d�tumokat
    dates <-
      as.Date(WTI2$Date[(decent + ablak_meret + kesleltet + 1):vegso])
    
    # Ez lesz az id�sor (d�tum- �s korrel�ci�s vektor)
    data_plot <- cbind(dates, results)
    
    # Hogy jobban �rtse az olvas� a k�dot megv�ltoztatjuk az oszlopok neveit
    colnames(data_plot) <- c("Dates", "Correlations")
    
    
    # �talak�tjuk az adathalmazunk tibble-re, hogy szebben tudjuk �br�zolni.
    tibble_data <- as_tibble(data_plot)
    
    tibble_data$Dates <-
      as.Date(tibble_data$Dates, origin = "1970-01-01")
    
    # �gy ker�l �br�zol�sra, hogy a kezd� id�pont +eltol�s+ablak m�rete lesz az els� id�pont,
    # amire korrel�ci� lesz, �s az utols� pedig a param�terk�nt megadott utols� id�pontra lesz
    plot_data_function(tibble_data,
                       kezdo_datum,
                       veg_datum,
                       kesleltet,
                       ablak_meret,
                       X,
                       Y)
  }
#problem1: wti2 matrix does not get generated in function but <<- sign was used

# Ez a f�ggv�ny ellen�rzi le, hogy a param�terek megfelel�en vannak-e megadva
# Ha valamelyik hib�s, akkor FALSE visszat�r�si �rt�ket ad, ezut�n a program
# egy hiba�zenet �r ki �s kil�p
check_parameters <-
  function(adat_kezdo,
           adat_vegso,
           kezdo_datum,
           veg_datum,
           kesleltet,
           ablak_meret,
           X,
           Y) {
    # Leellen�rizz�k, hogy karakter form�tumokba kaptuk-e a kezd� �s v�g d�tumokat, teh�t �t tudtuk alak�tani
    if (typeof(kezdo_datum) != "double" || typeof(veg_datum) != "double") {
      print("Karakter form�tumba adja meg a kezd� �s v�gd�tumokat pl: \"2010-01-01\"")
      return(FALSE)
      
      # Ha karakterek, akkor megn�zz�k, hogy a f�jlban megadott intervallumba esnek-e
    } else if (kezdo_datum < adat_kezdo || veg_datum > adat_vegso) {
      print(
        paste(
          "K�rj�k olyan d�tumot adjon meg, ami az elemz�s intervallum�ba beleesik:",
          adat_kezdo,
          "�s",
          adat_vegso,
          "k�z�tt."
        )
      )
      return(FALSE)
      
      # Leellen�rizz�k, hogy a t�bbi param�tert eg�sz sz�m form�tumban adta meg
    } else if (typeof(kesleltet) != "double" ||
               typeof(ablak_meret) != "double" ||
               typeof(X) != "double" || typeof(Y) != "double") {
      print("K�rj�k a d�tumokon k�v�li param�tereket eg�sz sz�mok form�j�ban adja meg.")
      return(FALSE)
      
      # Ellen�rizz�k, hogy megfelel� intevallumban kaptuk-e meg a param�tereket (pl. az ablak_meret nem negat�v)
    } else if (kesleltet < 0 || X < 1 || X > 24 || Y < 1 || Y > 24 ) {
      print("K�rj�k megfelel� intevallumban adja meg a param�tereket (pl. a k�sleltet�s ne legyen negat�v).")
      return(FALSE)
      
      # Ablak m�ret ellen�rz�se, 5-n�l kisebbet nem fogadunk el.
    } else if (ablak_meret <= 4) {
      print("Az ablak_meret nem lehet 5-n�l kisebb.")
      return(FALSE)
      
      # Ellen�rizz�k, hogy az ablak m�ret �s a k�sleltet�s �sszege kisebb nagyobb-e, mint az elemzend� intervallum
    } else if (as.integer(as.Date(veg_datum) - as.Date(kezdo_datum)) <= (kesleltet + ablak_meret)) {
      print("Adjon meg b�vebb intervallumot, vagy csek�lyebb ablak m�retet �s k�sleltet�st!")
      return (FALSE)
    }
    return(TRUE)
  } # beni: ez fasza nagyon, maradhat


# Ebben a f�ggv�nyben sz�moljuk ki a korrel�ci�t. Visszat�r�sk�nt a korrel�ci�s vektort adja.
# tipp: el�g csak a CL* form�ban megadni ezt a k�t sz�mot a program automatikusan
# hozz�ad egyet, hogy megkapja az oszlopok index�t az adatokban. Teh�t ha X = 1 �s
# Y = 2 a kapott param�ter, akkor CL1 �s CL2 k�z�tt sz�mol.
calculate_correlation <-
  function(ablak_meret = 100,
           kesleltet = 0,
           i = 1,
           the_data = WTI2) { #beni: �t�rtam a the_data-t a hozam matrixra azaz WTI2-re. 
    vegso=nrow(WTI2)-1 #beni: egy oszlop c�meknek megy 
    output = NULL
    CorMatrixCol=((n)/2*(n-1)) +1# this is the number of columns that contains correlations +1 as date vector [first one]
    pairedCorrelation=matrix(nrow=m,ncol=CorMatrixCol)
    #pairedCorrelation[,1]=WTI2[,1]# fill up the matrix with our dates from the return matrix
    #warning: NEM RAK BEL�JE D�TI�UMOT A GENNYL�TA
    z=2
    j=1
    i=1
    while(j<n){
      for (k in (j+1):n){
        
        while (i <= vegso - ablak_meret) {
          #itt sz�mol�dik a korrel�ci�
          pairedCorrelation[i,z] = cor(the_data[[1 + j]][i:(ablak_meret + i)], the_data[[1 + k]][(i - kesleltet):(ablak_meret + i - kesleltet)])
          # output = rbind(output, correlation) # ez lesz a korrel�ci� vektora
          # pairedCorrelation[i,z]=correlation
          i = i + 1 # Tov�bb l�p�s
          
        }
        i=1
        z=z+1
        
      } 
      #k=0
      j=j+1   
    }
    # Annyiszor kell korrel�ci�t futtatni, amennyi az utols� megadott d�tum, �s az eltol�s+k�sleltet�s+kezd� d�tum (param�ter) k�z�tti id�
    
    
    #beni : itt beraktam a p�ros�t� korrel�ci�s iz�t
    # Ellen�rz�s, hogy nagyj�b�l megfelel� erdm�nyt kaptunk-e.
    # Ha nem, akkor megszak�tja a program fut�s�t.
    
    #beni: if check_results r�szt kivettem mert szerintem nem fog kelleni,ez a komment felett volt
    #beni: vegso=2557 ...:(. 2557 hasznos sor van az �r m�trixban, nek�nk a hozamn�l 2556 lesz �gyhogy nem lehet ezt bent hagyni �gy
    #beni: a vegso parameter mostm�r egy sor sz�mol�s lesz abb�l a m�trixb�l ami alapb�l van-1 a sorn�v miatt.
  }
# Egy r�vid vizsg�lat, hogy az eredm�nyeink megfelelnek-e a val�s�gnak
# A korrel�ci� [-1, 1] intervallumon mozog.
check_results <- function(results) {
  
  # Az eredm�ny m�trixon v�gigiter�lva, megn�zz�k van-e 1 n�l nagyobb
  # vagy 0-n�l kisebb sz�m.
  for(row in 1:nrow(results)){
    if (results[row, 1] < -1.0000 || results[row, 1] > 1.0000){
      return(FALSE)
    }
  }
  
  return(TRUE)
} # erre egy�ltal�n van sz�ks�g?


# �br�zol�s, a param�terek ki�r�s�hoz sz�ks�ges azok �tad�sa, param�terk�nt.
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
        title = "Dinamikus Korrel�ci�",
        subtitle = paste(
          "Kezd� d�tum: ",
          loc_kezdo_datum,
          "\nV�gs� d�tum: ",
          loc_veg_datum,
          "\nK�sleltet�s: ",
          loc_kesleltet,
          "\nRolling window: ",
          loc_ablak_meret,
          "\nEls� adathalmaz: ",
          loc_X,
          "\nM�sodik adathalmaz: ",
          loc_Y
        )
      ) + 
      scale_x_date(date_breaks = "1 year", date_labels = "%Y")
    
    plot(gg)
    
  }


korrelacio("2010-01-01", "2016-12-31", 0, 10)