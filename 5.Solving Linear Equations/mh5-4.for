      program mh
      dimension a(4),b(4),c(4),f(4),y(4),b0(4),x(4)
      data b/2,2,2,2/
      data a/0,0.5,0.5,1/
      data c/1,0.5,0.5,0/
      data f/-0.5,0,0,0/
      n = 4
      y(1) = f(1)/b(1)
      d = b(1)
      do i = 2,n
          b0(i-1) = c(i-1)/d
          d = b(i) - a(i) * b0(i-1)
          y(i) = (f(i) - a(i)*y(i-1))/d
      end do
      do i =1,n
          x(i) = y(i)
      end do
      do i = 1,n-1
          x(n-i) = y(n-i) - b0(n-i)*x(n-i+1)
      end do
      write(*,*)x
      end