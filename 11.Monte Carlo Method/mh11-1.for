      program main
      real ss1,jieMian,r
      open(1,file="zhijiechouyang.dat")
      r=0.5
      jieMian = 1.0
      do k=1,200000
         call rand(r)
         ss1=-log(r)/jieMian
         write(1,*)ss1
      end do
      end 
       subroutine rand(R)
       data K,J,M,RM/5701,3613,566927,566927/
       ir=int(R*RM)
       irand=mod(J*ir+k,M)
       R=(real(irand)+0.5)/RM
       return
       end


         
