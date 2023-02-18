program solid_car
 
implicit none
real::r,r2,x,y,z,q,l,t,pi,h,secs,a,u1,u2,q0,a0,ai,b
integer::n,j,i
ai=3.5
b=3.9
pi=3.14
r=1
r2=r*r
secs=0
n=1000
do i=10,n,100
 do z=1,20
     secs=0
       do  j=1,i
          call random_number(u1)
          call random_number(u2)
          q=acos(2*u1-1)
          l=2*pi*u2
          t=z/cos(q)
          x=t*sin(q)*cos(l)
          y=t*sin(q)*sin(l)
          h=x*x+y*y
             if ((-ai/2)< x.and. x>(ai/2).and.(-b/2)<y .and. y>(b/2)) then
                  secs=secs+1					  
             end if
			 
       end do
	   q0=atan(r/z)
	   a0=2*pi*(1-cos(q0))
    a=2*pi*(secs/n)
	print*,'la valeur est:',a,z,a0

 end do
 enddo

 end  