program aissam 
implicit none
real,parameter :: a1=2.,d1=4.
real :: a,y,Q,pi,b1,u1,u2,b,moy,x,moy1,var
integer :: n,i,j,nb,nbr,y1 
integer :: sec
  b1=3.14
  n=1000
  a=0.
    b=(d1/2)
  x=0.
  y1=0.
  nbr=0

       do j=1,n,10
          i=j*n
          nbr=nbr+1
          nb=0
          sec=0.
               do while  (nb<i)
                         nb=nb+1
                         call random_number(u1)
                         call random_number(u2)
                         y=a+(b-a)*u1
                         Q=a+(b1-a)*u2
                             if(y<=(a1/2)*sin(Q)) then
                                   sec=sec+1
                             endif 
               enddo
          pi=((2*a1*i)/(sec*d1))
          x=x+pi
          y1=y1+(pi*pi)
          print*,'la valeur de pi',pi
 
       enddo
  moy=(x/nbr)
  moy1=(y1/nbr)
  var=moy1-(moy*moy)
  print*,'la moy',moy,moy1,var
 
 end program
  
