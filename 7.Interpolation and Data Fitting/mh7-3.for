      program mh
      dimension a(7,7)
      real * 8 x(7),y(7),x1,sum,times(7)
      data x/0,5,10,15,20,25,30/
      data y/75.64,74.92,74.22,73.49,72.75,71.97,71.18/
      data times/1,1,1,1,1,1,1/
      x1 = 13.2
      N = 7
      do i = 1,N
          a(1,i) = y(i)
      end do
      sum = 0
      call NewtonPolynomial(a,x,N,x1,sum)
      write(*,*)sum
      b = x1
      call LagrangePolynomial(x,y,N,b,sum)
      write(*,*)sum
      end
      
      
      subroutine NewtonPolynomial(form,x,N,x1,sum)
      dimension form(N,N)
      real * 8 x(N),times(N),sum,x1
      do i = 1,N
          times(i) = 1
      end do
!     *********构造差商表*************
      do i = 1,N-1
          do j = 1,N-i
              form(i+1,j) = ( form(i,j+1) - form(i,j) ) / (x(j+i)-x(j))
          end do
      end do
!    *********构造相乘系数*************
      do i =2,6
          do j = 2,i
              times(i) = times(i)*(x1 - x(j-1))
          end do
      end do
!     *********计算结果*************
      sum = 0
      do i = 1,N
          sum = sum + form(i,1)*times(i)
      end do
      return
      end
      
      subroutine LagrangePolynomial(x,y,N,b,sum)
      real * 8 x(7),y(7),x1,sum
      do i = 2,N-1
             ti = i
              if(i == N-1) goto 70
              if(b<x(i)) then
                  if(i==2) goto 70
                  if(b-x(i-1)<x(i)-b) then
                      ti=i-1
                      goto 70
                  end if
              end if
         end do
70       x1 = x(ti-1)
         x2 = x(ti)
         x3 = x(ti+1)
         b1 = (b-x2)*(b-x3)/((x1-x2)*(x1-x3))
         b2 = (b-x1)*(b-x3)/((x2-x1)*(x2-x3))
         b3 = (b-x1)*(b-x2)/((x3-x1)*(x3-x2))
         sum = b1*y(ti-1) + b2*y(ti) + b3*y(ti+1)
         return
         end