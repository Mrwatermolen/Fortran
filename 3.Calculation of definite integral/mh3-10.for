
      program mh
      dimension t4(4),w4(4)
      data t4/0.3399810,-0.3399810,0.8611363 ,-0.8611363/
      data w4/0.6521452,0.6521452,0.3478548,0.3478548/
      a = 1.0
      b = 1.5
      c = 1.4
      d = 2.0
      g = 0.0
      do i =1,4
          x = 0.5 *((a + b)+(b - a)*t4(i))
          do j = 1,4
              y = 0.5 *((c + d)+(d - c)*t4(j))
              g = g + w4(j)*f(x,y)
          end do
      end do

      g = 0.075*(b - a)*g
      write(*,*)g
      end
      
      function f(x,y)
      f = alog(x + 2*y)
      end