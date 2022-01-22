      program mh
      dimension A(3,3), b(3)
      data A/-1,3,2,2,-1,-3,-2,4,-2/
      data b/-1,7,0/
      A(1,1) = -1
      A(1,2) = 2
      A(1,3) = -2
      A(2,1) = 3
      A(2,2) = -1
      A(2,3) = 4
      A(3,1) = 2
      A(3,2) = -3
      A(3,3) = -2
      n = 3
      do i = 1,n-1
          temp = 0
          mark = 0
          do j = i,n
              if (abs(A(j,i)) > abs(temp)) then
                  temp = A(j,i)
                  mark = j
              end if
          end do
!         交换      
          do j = i,n
              t = A(i,j)
              A(i,j) = A(mark,j)
              A(mark,j) = t
          end do
          t = b(i)
          b(i) = b(mark)
          b(mark) = t
!         消元
          do j = i+1,n
!         记住A(j,i)              
              care = A(j,i)
              do k = i,n
                  A(j,k) = A(j,k) - (care * A(i,k))/A(i,i)
                  if (k==i) b(j) = b(j) -  (b(i) * care)/A(i,i)
              end do
          end do
      end do
!         回带
      b(n) = b(n)/A(n,n)
      do i = 1,n-1
          s = 0
          k = n - i + 1
          do j =k,n
              s = s + A(n-i,j) * b(j)
          end do
          b(n-i) = (b(n-i) - s)/A(n-i,n-i)
      end do
      write(*,*)b
      end