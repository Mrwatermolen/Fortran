      program mh
      open(10,file='2_4.dat')
      pi = 3.14159265359
      x = -2*pi
      t = 1
      N = 1000
      dx  = x/N
      x = 0
      do 100 i = 1,N
      y1 = 2 * cos(2*pi*(t/3-x/5))
      y2 = 2 * cos((2*pi*(t/3+x/5)+pi))
      y3 = y1 + y2
      write(10,*)x,y1,y2,y3
100   x = x + dx
      end