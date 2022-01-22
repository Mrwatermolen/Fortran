      program mh
      dimension x(1000),y(1000)
      real * 8 a(4)
      pi = 3.14159265
      data a/0.25,0.5,0.75,1.75/
      do i =1,4
          a(i) = a(i)*pi
      end do
      
      N = 1000
      add = 8/float(N)
      do i = 1,N
          x(i) = float(i)*add
          y(i) = cos(x(i))
      end do
      do j = 1,4
         do i = 2,N-1
             ti = i
              if(i == N-1) goto 70
              if(a(j)<x(i)) then
                  if(i==2) goto 70
                  if(a(j)-x(i-1)<x(i)-a(j)) then
                      ti=i-1
                      goto 70
                  end if
              end if
         end do
70       x1 = x(ti-1)
         x2 = x(ti)
         x3 = x(ti+1)
         a1 = (a(j)-x2)*(a(j)-x3)/((x1-x2)*(x1-x3))
         a2 = (a(j)-x1)*(a(j)-x3)/((x2-x1)*(x2-x3))
         a3 = (a(j)-x1)*(a(j)-x2)/((x3-x1)*(x3-x2))
         res = a1*y(ti-1) + a2*y(ti) + a3*y(ti+1)
         rel = cos(a(j))
         write(*,*)a(j),res,rel,abs(res-rel)
      end do
      end