      program mh
      eps = 1.0e-6
      call bisection(2.0,4.0,eps)
      end
      
      subroutine bisection(a1,b1,eps)
      n=0
      a = a1
      b = b1
      f1 = a**2 - 0.9*a - 8.5
      f2 = b**2 - 0.9*b - 8.5
20    x = (a + b)/2.
      n = n+1
      f = x**2 - 0.9*x - 8.5
      write(*,*)x,a,b,f1,f2,f
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
      write(*,*)x,"二分次数",n
      return
      end