      program mh
      external f
      eps = 10e-6
      a = 120.0
      b = 150.0
10    c = b - ((b-a)/(f(b) - f(a)))*f(b)
      if(abs(c-b)<=eps)  goto 100
      temp = a
      a = b
      b = c
      goto 10
100   write(*,*)c
      end
      
      function f(x)
      f = x*0.5*(exp(50/x) + 1/exp(50/x))-x-10
      return
      end