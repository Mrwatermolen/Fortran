      program mh
      external f,f1
      pi = 3.14159265
      eps = 10e-6
      call newtonMethod(f,f1,3.0,eps)
      read(*,*)wwww
      end
      
      function f(x)
      f = x*x - 7
      return
      end
      
      function f1(x)
      f1 = 2*x
      return
      end
      
      subroutine newtonMethod(f,f1,x0,eps)
      x = x0
      write(*,*)x,x0
10    y0 = x
      y = f(y0)
      y1 = f1(y0)
      x = y0 - y/y1
      if(abs(x-y0)>=eps) goto 10
      write(*,*)x
      return
      end