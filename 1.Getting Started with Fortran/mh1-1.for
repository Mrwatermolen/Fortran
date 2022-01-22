      program mh
      m=0
      i=1
      j=10000
50    if(m.gt.j)then
          goto 100
      else
          m=m+i*i*i
          i=i+1
          write(*,*)m
      end if
      goto 50
100   write(*,*)m
      end