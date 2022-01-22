      program main
      parameter(k1=200000)
      real r1,r2,m1,h1,hy,x
      open(1, file='fengzireyundong.dat')
      r=0 
      m=0
      m1=sqrt(1.5/3.1415926)*3*exp(-0.5) !h的最大值
      do i=1,k1
         call rand(r)
         r1=r
         call rand(r)
         r2=r
         y=-1.5*log(r1)
         call h(y,h1)
         hy=h1/m1
         if(hy.gt.r2) then
           m=m+1
           x=y
           write(1,*)x,2*sqrt(x/3.1415926)*exp(-x)
           end if
      end do
      write(*,*)real(m)/real(k1),m
      read(*,*)r1
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
      h1=(3.*sqrt(y/3.1415926))*exp(-(1./3.)*x)
      end


         
