program exercice2
implicit none 
real,parameter :: m=16. , a1=5. , c=1. 
real :: x,a,b,r,moy,s,e,var,moy1
integer :: n ,i
open(unit=5,file='ex2.xls')
  b=1.
  a=-1

  do n=10,1000,10
     s=0.

       do i=1,n
          x=modulo(a1*x+c,m)
          r=x/(m-1)
          e=a+(b-a)*r	
          s=s+e
       enddo
     moy=s/n
	 moy1=(s*s)/n
	 var=(moy1-(moy*moy))/n
     write(5,*)n,moy,var
  
  enddo
end program
