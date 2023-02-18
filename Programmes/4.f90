program calcul_pi
implicit none
real::som,x,pi,var,som2,y,m
integer::n,i
n=1000
som=0.
som2=0.
     do i=0,n
       call random_number(x)
		y=sqrt(1-x*x)
        som=som+y
		som2=som2+y*y
        
     end do
  pi=4*som/n
  var=(som2/n)-(((som)/n)*2)
  m=acos(-1.)
  print*,'',pi,var,m

end program