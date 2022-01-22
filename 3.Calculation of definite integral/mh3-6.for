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
      pi = 3.14159265359
      call simp(0., pi, 1e-5 , f, s2)
      x = 0
      y3 = 0
      N = 1000
      dx = pi/float(N)
      do j = 0,N
          dy =f(x) * dx
          x = x + dx
          if(j.eq.0.or.j.eq.N) then
              y3 = y3 + 1./3. * dy
          else
               k = j - 2*int(j/2)
          if(k.eq.0) then
              y3 = y3 + 2./3. * dy
          else
              y3 = y3 + 4./3. * dy
          end if
          end if
      end do
      write(*,*)s2,y3      
      end
      
      function f(x)
      a1 = 400
      b1 = 100
      f = sqrt(a1 * sin(x) * sin(x) + b1 * cos(x) * cos(x))
       
      end