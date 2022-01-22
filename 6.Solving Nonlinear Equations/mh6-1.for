      program mh
      real * 8 A(3,3),b(3),x(3),w(3)
      data A/4,-1,0, -1,4,-1, 0,-1,4/
      data b/1,4,3/
      data w/1.03,1,1.1/
      eps = 1.0e-2
      call bisection(1.0,1.5,eps)
      end
      
      subroutine bisection(a1,b1,eps)
      a = a1
      b = b1
      f1 = a**3 - a - 1
      f2 = b**3 -b - 1
20    x = (a + b)/2.
      f = x**3 - x - 1
      if(f==0) goto 100
      if(f2*f<0) then
          a = x
          f1 = f
      else
          b = x
          f2 = f
      end if
      if(abs(a-b) >= eps) goto 20
100   x = (a+b)/2.
      write(*,*)x
      return
      end