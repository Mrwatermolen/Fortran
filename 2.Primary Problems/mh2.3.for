      program mh
      open(10,file='2_3.dat')
      t = 2
      v = 0
      x = 10
      A = -9.8
      B = 0.5
      N = 1000
      dt = t/float(N)
      do 100 i=1,N
      tt = t*(float(i))/N
      dx = v*dt
      dv = (A - B*v)*dt
      v = v + dv
      x = x + dx
100   write(10,*)tt,x
      end