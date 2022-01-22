      program mh
      pi = 3.14159265359
!     1
      N = 1000
      x = pi
      dx = x/float(N)
      x = 0
      y1 = 0
      do i = 0,N-1
          dy = f(x) * dx
          x = x + dx
          y1 = y1 + dy
      end do
      write(*,*)'y1 = ',y1
!     2
      x = 0
      y2 = 0
      do j = 0,N
          dy = f(x) * dx
          x = x + dx
          if(j.eq.0.or.j.eq.N) then
              y2 = y2 + 0.5 * dy
          else
              y2 = y2 + dy
          end if
      end do
      write(*,*)'y2 = ',y2
!    3
      x = 0
      y3 = 0
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
      write(*,*)'y3 = ',y3
      end
      
      function f(x)
      f = cos(1/(1+x*x))
      end