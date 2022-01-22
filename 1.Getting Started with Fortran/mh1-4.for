      program mh4
      s=0
      v=20
      n=100000
      dt=1/float(n)
      t=0
50    v=v-(1/float(300))*v*v*dt
      s=s+v*dt
      t=t+dt
      if(t.ge.15) goto 100
      goto 50
100   write(*,*)v,s
      end
          