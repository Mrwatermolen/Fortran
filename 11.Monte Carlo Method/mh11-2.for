      program main
      parameter(k1=200000)
      real r,r1,r2,d2,cosph,sinph
      open(1,file='sincoschouyang.dat')
      r=0.5
      do i=1,k1
         call rand(r)
         r1=r
         call rand(r)
         r2=r
         x1=2*r1-1
         x2=r2
         d2=x1**2+x2**2
         if(d2.le.1) then
            cosph=(x1**2-x2**2)/d2
            sinph=2*x1*x2/d2
            write(1,*)cosph,sinph
         end if
      end do
      end

      subroutine rand(R)
      data K,J,M,RM/5701,3613,566927,566927/
      ir=int(R*RM)
      irand=mod(J*ir+k,M)
      R=(real(irand)+0.5)/RM
      return
      end


         
