program solid
 
implicit none
real::r,r2,x,y,z,q,l,t,pi,h,secs,a,u1,u2,q0,a0
integer::n,i
pi=3.14
r=1
r2=r*r
secs=0
n=1000

 do z=1,15
     secs=0
       do  i=1,n
          call random_number(u1)
          call random_number(u2)
          q=acos(2*u1-1)
          l=2*pi*u2
          t=z/cos(q)
          x=t*sin(q)*cos(l)
          y=t*sin(q)*sin(l)
          h=x*x+y*y
             if (h<=r2) then
                  secs=secs+1
             end if
			 
       end do
	   q0=atan(r/z)
	   a0=2*pi*(1-cos(q0))
    a=2*pi*(secs/n)
	print*,'la valeur est:',a,z,a0

 end do


 end  