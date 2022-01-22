      program mh
      external f
      real * 4 x1,h(8),rel 
      x1 = 0.04
      data h/0.0001,0.002,0.005,0.01,0.02,0.05,0.1/
      rel = 1./(sin(x1)*sin(x1))
      do i = 1,7
          write(*,*)"h=",h(i)
          yy = f(h(i),x1)
          write(*,*)"近似值为",yy,"误差",abs(yy-rel)
      end do
      
      read(*,*)wwww
      end
      
      function f(h,x1)
      real * 4 x(3),x1,h,y(3),y1
      x(1) = x1 - h
      x(2) = x1
      x(3) = x1 + h
      do i = 1,3
          y(i) = - 1/tan(x(i)) 
      end do
      f = (1./(2.*h))*(-y(1)+y(3))
      return
      end