pairedCorrelation <- function(WTI){
  n <- ncol(ret_matrix)
  m <- nrow(ret_matrix)
  CorMatrixCol=(1+n)/2*n
  pairedCorrelationMatrix=matrix(nrow=m,ncol=CorMatrixCol)
  j=1
  while(j!=n) {
    for(i in j:n){
      for(k in 1:m){
        pairedCorrelationMatrix[k,j]=TomiekCorja(ret_matrix[,j],ret_matrix[,i])
      }
    }
    
    j=j+1
  }
}

#nagyon sablonos vázlat, ITT tomiék kódjával.
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


##
vegso=nrow(WTI2)
ablak_meret=20
kesleltet=0
the_data=WTI2
n<- ncol(WTI2) -1 #-1 because first column kept for the date
m<- vegso - ablak_meret
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
z<-2
j=1
n=24
s=2536
while(j<n){
  for(i in j:n){
    z=z+1
  }
  j=j+1
}