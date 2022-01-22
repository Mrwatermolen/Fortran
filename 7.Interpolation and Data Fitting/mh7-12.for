      program mh
      dimension x(1000),y(1000),A(1000,1000),B(1000),a1(1000)
      real * 8 s,t,p,q
      write(*,*)"要输入的数据点数n=?拟合多项式最高次数为m=？"
      read(*,*)N,m
      m = m + 1
      write(*,*)"依次输入(x,y)"
      do i = 1,N
          read(*,*)x(i),y(i)
      end do
      do i = 1,m
          do j = 1,m
              A(i,j) = 0
              do k = 1,n
                  A(i,j) = A(i,j) + x(k)**(i+j-2)
              end do
          end do
          B(i) = 0
          do o = 1,n
              B(i) =  B(i) + y(o)*x(o)**(i-1)
          end do
      end do
      do i =1,m
          a1(i) = 0
      end do
20    p = 0
      do i = 1,m
          s = 0
          t = a1(i)
          do j = 1,N
             s = s + A(i,j)*a1(j)
          end do
          a1(i) = a1(i) + ((B(i) - s))/A(i,i)
          q = abs(a1(i) - t)
          if(p < q) p = q
      end do
      if (p>10e-6) goto 20
      do i = 1,m
          write(*,*)"次数为",i-1,"的系数",a1(i)
      end do
      read(*,*)wwww
      end
      