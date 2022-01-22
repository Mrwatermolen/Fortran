      program mh3
      t=1
      s=0
      o=1
      p=0.001
50    s=1/(o*(o+1))
      if(s .lt.p) goto 100
      t=t+s
      o=o+1
      goto 50
100   write(*,*)t
      end
          
          