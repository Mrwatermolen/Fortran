      program mh
      dimension a(6,6)
      real * 8 x(6),y(6),x1,x2,sum,times(6)
      data x/0.125,0.250,0.375,0.500,0.625,0.750/
      data y/0.796,0.773,0.744,0.704,0.656,0.602/
      data times/1,1,1,1,1,1/
      x1 = 0.158
      x2 = 0.638
      do i = 1,6
          a(1,i) = y(i)
      end do
      do i = 1,5
          do j = 1,6-i
              a(i+1,j) = ( a(i,j+1) - a(i,j) ) / ( x(j+i) - x(j) )
          end do
      end do
      do i = 1,6
          do j = 1,6
          write(*,*)i,"col",j,"lll",a(i,j)
          end do
      end do
      sum = 0
      do i =2,6
          do j = 2,i
              times(i) = times(i)*(x1 - x(j-1))
          end do
      end do
      do i = 1,6
          sum = sum + a(i,1)*times(i)
      end do
      write(*,*)sum
      sum = 0
      do i = 1,6
          sum = sum + a(i,1)*times(i)
      end do
      write(*,*)sum
      end