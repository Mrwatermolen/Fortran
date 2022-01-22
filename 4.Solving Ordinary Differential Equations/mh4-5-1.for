      program mh
      external f
      open(2,file="4.5.1ty.dat")
      y = 0
      t = 0.0
      y1 = 0
      rang =  1.
      dt = 0.1
      N = rang/dt
      do j = 0, N
       write(2,*)t,y
       y1 = y + f(t,y) * dt
       t = t + dt
       y = 0.5 * (y + y1 + f(t,y1) * dt)
      end do
      end
      
      
      function f(t,y)
      f = 1 + (sin(y)) * (exp(-t))
      return
      end