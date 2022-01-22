      subroutine romb(a,b,e,f,r2)
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
20    c2 = s2 + (s2 - s1) / 15.
      if(n/=2) goto 40
30    c1 = c2
      goto 15
40    r2 = c2 + (c2 - c1)/63.
      if(n/=4) goto 60
50    r1 = r2
      goto 30  
60    if(abs(r2) - 1.) 70,70,80
70    if(abs(r2 - r1) > e) goto 50
      return
80    if((abs(r2 - r1) / r2) >= e) goto 50
      return
      end
      
      program mh
      external f
      call romb(0.1, 0.6, 1e-5 , f, r2)
      write(*,*)r2
      end
      
      function f(x)
      f = 0.02792*(2 - x)/( ( (1.449*x + 1)**0.8 ) * ( (1-x)**1.2 ) * x)
      end