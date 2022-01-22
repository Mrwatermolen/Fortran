      program mh
      external k1,k2,k3,k4
      open(2,file="4.5.2ty.dat")
      y = 0
      t = 0.0
      y1 = 0
      rang =  1.
      dt = 0.1
      N = rang/dt
      do j = 0, N
          write(2,*)t,y
          t1 = k1(t,y)
          t2 = k2( (t+dt/2), (y+t1*dt/float(2)) )
          t3 = k3( (t+dt/2), (y+t2*dt/float(2)) )
          t4 = k4( (t+dt), (y+t3*dt) )
          y = (t1+2*t2+2*t3+t4)*(dt/float(6)) + y
          write(*,*)t1,t2,t3,t4,y
          t = t + dt
      end do

      end
      
      
      function k1(t,y)
      k1 = 1 + (sin(y)) * (exp(-t))
      return
      end
      
      function k2(t,y)
      k2 = 1 + (sin(y)) * (exp(-t))
      return
      end
      
      function k3(t,y)
      k3 = 1 + (sin(y)) * (exp(-t))
      return
      end
      
      function k4(t,y)
      k4 = 1 + (sin(y)) * (exp(-t))
      return
      end