program exercice1
implicit none 
real,parameter :: m=16. , a=5. , c=1. 
real :: x,r,moy,s,moy1,var
integer :: n ,i

open(unit=5,file='ex1.xls')
   x=1
 

  do n=10,1000,10
    s=0.

       do i=1,n
 
         x=modulo(a*x+c,m)
		 r=x/(m-1)
    	 s=s+r
  
       enddo
    moy=s/n
	moy1=(s*s)/n
	var=(moy1-(moy*moy)	)/n
    write(5,*)n,moy,var
  
  enddo
end program

