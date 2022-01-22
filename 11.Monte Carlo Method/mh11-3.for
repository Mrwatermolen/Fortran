      program main
      parameter(k1=200000)
      real r1,r2,m1,h1,hy,x
      open(1, file='banzhenngtai.dat')
      r=0 
      m=0
      m1=sqrt(2*exp(1.0)/3.1415926) !h的最大值
      do i=1,k1
         call rand(r)
         r1=r
         call rand(r)
         r2=r
         y=-log(1-r1)
         call h(y,h1)
         hy=h1/m1
         if(hy.gt.r2) then
           m=m+1
           x=y
           write(1,*)x,sqrt(2/3.1415926)*exp(-0.5*x**2.)
           end if
      end do
      write(*,*)real(m)/real(k1),m
      end

      subroutine rand(R)
      data K,J,M,RM/5701,3613,566927,566927./
      ir=int(R*RM)
      irand=mod(J*ir+k,M)
      R=(real(irand)+0.5)/RM
      return
      end

      subroutine h(y,h1)
      real h1
      h1=sqrt(2*exp(1.0)/3.1415926)*exp(-0.5*(x-1)**2.)
      end


         
