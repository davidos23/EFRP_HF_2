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

#nagyon sablonos v�zlat, ITT tomi�k k�dj�val.
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