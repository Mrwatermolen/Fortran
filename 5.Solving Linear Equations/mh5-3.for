      program mh
      dimension A(3,3),b(3)
      b(1) = 2*9.8
      b(2) = 3*9.8
      b(3) = 9.8
      A(1,1) = 2
      A(1,2) = 0
      A(1,3) = 1
      A(2,1) = 0
      A(2,2) = 3
      A(2,3) = 1
      A(3,1) = 1
      A(3,2) = 1
      A(3,3) = 0
      n = 3
      call gauss(n,A,b)
      write(*,*)b
      end
      
      subroutine gauss(n,A,b)
      dimension A(n,n), b(n)
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
      end