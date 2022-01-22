      program mh
      external f,f1
      pi = 3.14159265
      eps = 10e-4
      call newtonMethod(f,f1,pi,eps)
      call newtonMethod(f,f1,0.5*pi,eps)
      read(*,*)wwww
      end
      
      function f(x)
      f = sin(x) - x*0.5
      return
      end
      
      function f1(x)
      f1 = cos(x) - 0.5
      return
      end
      
      subroutine newtonMethod(f,f1,x0,eps)
      x = x0
10    x0 = x
      y = f(x0)
      y1 = f1(x0)
      x = x0 - y/y1
      if(abs(x-x0)>=eps) goto 10
      write(*,*)x
      return
      end