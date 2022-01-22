      subroutine simp(a,b,e,f,s2)
      h = b - a
      T1 = (h/2.) * (f(a)+f(b))
      n = 1
5     s = 0.
      do k = 1, n
          s = s + f(a + (k-0.5)*h)
      end do
      T2 = T1/2. + h/2. * s
      s2 = T2 + (T2 - T1)/3.
      if(n/=1) goto 20
15    n = n+n
      h = h/2
      T1 = T2
      s1 = s2
      goto 5
20    if(abs(s2) <= 1.) d = abs(s2 - s1)
      if(abs(s2) > 1.) d = abs((s2 - s1) / s2)
      if(d >= e) goto 15
      return
      end
      
      program mh
      external f
      call simp(0.000001, 1., 1e-5 , f, s2)
      write(*,*)s2
      end
      
      function f(x)
      a1 = 400
      b1 = 100
      f = atan(x)/x
      write(*,*)f
      end