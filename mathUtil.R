library(geometry)
mangle <- function(a){
  b=c(100,37)
  c=c(100,63)
  ab = as.matrix(c((b[1]-a[1]),(b[2]-a[2])))
  ac = as.matrix(c((c[1]-a[1]),(c[2]-a[2])))
  # dot prod
  dv = dot(ab,ac)
  # norm
  nab=norm(ab,type="2")
  nac=norm(ac,type="2")
  # angel in rad
  ang=acos(dv/(nab*nac))
  retval = rad2deg(ang)
  return(retval)
}


disttorecip <- function(a,b){
  ab = as.matrix(c((b[1]-a[1]),(b[2]-a[2])))
  nab=norm(ab,type="2")
  return(nab)
}
disttogoal <- function(a){
  m=c(100,50)
  am = as.matrix(c((m[1]-a[1]),(m[2]-a[2])))
  nam=norm(am,type="2")
  return(nam)
}

dist2d <- function(a,b,c) {
  v1 <- b - c
  v2 <- a - b
  m <- cbind(v1,v2)
  d <- abs(det(m))/sqrt(sum(v1*v1))
  return(d)
} 


oppInTri <- function (s,p) {
  
}



rad2deg <- function(rad) {(rad * 180) / (pi)}

