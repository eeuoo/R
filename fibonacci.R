while(TRUE) {
  x = as.integer(readline(prompt = "Input the number: "))
  if (x < 0) break
  
  f = 1
  
  if (x == 0)
    print("ERROR")
  
  else {
    
    p1 = 0 
    p2 = 1
    p = p1 + p2
    
    for ( i in 1:(x-1)){
     
     if (i == (x-1)) {
       break
     }
     else {
        
        p1 = ( p2 )
        p2 = ( p )
        p = p1 + p2
        class(p)
        
     }
      
    }
    # print(paste(p1,p2))
  } 
}
