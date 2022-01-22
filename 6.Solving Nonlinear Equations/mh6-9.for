      program mh
      external f
      eps = 10e-4
      a = 0
      b = 1
10    c = b - ((b-a)/(f(b) - f(a)))*f(b)
      if(abs(c-b)<=eps)  goto 100
      temp = a
      a = b
      b = c
      goto 10
100   write(*,*)c
      end
      
      function f(x)
      f = 1-x-sin(x)
      return
      end