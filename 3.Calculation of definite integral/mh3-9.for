
      program mh
      dimension t3(3),w3(3),t5(5),w5(5)
      data t3/0,0.7745967,-0.7745967/
      data w3/0.8888889,0.5555556,0.5555556/
      data t5/0,0.5384693,-0.5384693,0.9061799,-0.9061799/
      data w5/0.5688889,0.4786287,0.4786287,0.2369269,0.2369269/
      a = 1
      b = 3
      g = 0.0
      do 10 i =1,3
      x = 0.5 *((a + b)+(b - a)*t3(i))
10    g = g + w3(i)*f(x)
      g = 0.5*(b - a)*g
      write(*,*)g
      g = 0.0
      do 20 j =1,5
      x = 0.5 *((a + b)+(b - a)*t5(j))
20    g = g + w5(j)*f(x)
      g = 0.5*(b - a)*g
      write(*,*)g
      
      
      N = 5000
      dx = 2./N
      
      x = 1
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
      write(*,*)y2
      
      x = 1
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
      write(*,*)y3
      end
      
      function f(x)
      f = 1/x
      end