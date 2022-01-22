
      program mh
      dimension t4(4),w4(4),t3(3),w3(3)
      data t4/0.3399810,-0.3399810,0.8611363 ,-0.8611363/
      data w4/0.6521452,0.6521452,0.3478548,0.3478548/
      data t3/0,0.7745967,-0.7745967/
      data w3/0.8888889,0.5555556,0.5555556/
      a = 0
      b = 1.5708
      g = 0.0
      do i =1,4
          x = 0.5 *((a + b)+(b - a)*t4(i))
          g = g + w4(i)*f(x)
      end do
      g = 0.5*(b - a)*g
      write(*,*)g
      g = 0.0
      do j =1,3
          x = 0.5 *((a + b)+(b - a)*t3(j))
          g = g + w3(j)*f(x)
      end do
      g = 0.5*(b - a)*g
      write(*,*)g
      end
      
      function f(x)
      f = 1/(cos(x) ** 2 + 4 * sin(x) ** 2)
      end