program exerice1
implicit none 
real*8 :: x,y,a,b,u1,u2,r,r2,h,secs,pi,m
integer:: i,n
n=10000000
secs=0.
a=-0.5
b=0.5
r=0.5
r2=r*r
do i=1,n
     call random_number(u1)
     call random_number(u2) 
        x=a+(b-a)*u1
        y=a+(b-a)*u2
        h=(x*x)+(y*y)
          if(h<=r2) then 
          secs=secs+1
          endif
enddo
     pi=4*(secs/n)
     m=acos(-1.)
     print*,secs,pi,m 
     
end
