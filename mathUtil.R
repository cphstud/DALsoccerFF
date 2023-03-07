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
  # from a to line mellem b and c
  v1 <- b - c
  v2 <- a - b
  m <- cbind(v1,v2)
  d <- abs(det(m))/sqrt(sum(v1*v1))
  return(d)
} 

area <- function(x1,y1,x2,y2,x3,y3) {
    val=abs((x1 * (y2 - y3) + x2 * (y3 - y1) + x3 * (y1 - y2)) / 2.0)
    return(val)
  }

areaTriToGoal <- function(x1,y1) {
    x2=120
    y2=36
    x3=120
    y3=44
    
    val=abs((x1 * (y2 - y3) + x2 * (y3 - y1) + x3 * (y1 - y2)) / 2.0)
    return(val)
}

oppInTri <- function (x1,y1,x,y) {
    x2=120
    y2=36
    x3=120
    y3=44
    # Calculate area of triangle ABC
    A = area(x1, y1, x2, y2, x3, y3)
    # Calculate area of triangle PBC
    A1 = area (x, y, x2, y2, x3, y3)
    # Calculate area of triangle PAC
    A2 = area (x1, y1, x, y, x3, y3)
    # Calculate area of triangle PAB
    A3 = area (x1, y1, x2, y2, x, y)
    retval=ifelse(A == A1 + A2 + A3,T,F)
    return(retval)
}



rad2deg <- function(rad) {(rad * 180) / (pi)}

