      program mh
      dimension A(5,5),b(5)
      b(1) = -14.38*14.38
      b(2) = -14.38*14.38
      b(3) = -7.42* 7.42
      b(4) = -6.38*6.38
      b(5) = -8.81* 8.81
      A(1,1) = 14.38*3.94
      A(1,2) = 11.38*2.79
      A(1,3) = 7.42*3.07
      A(1,4) = 6.38*5.11
      A(1,5) = 8.81*2.59
      A(2,1) = 3.94*3.94
      A(2,2) = 2.79*2.79
      A(2,3) = 3.07*3.07
      A(2,4) = 5.11*5.11
      A(2,5) = 2.59*2.59
      A(3,1) = 14.38
      A(3,2) = 11.38
      A(3,3) = 7.42
      A(3,4) = 6.38
      A(3,5) = 8.81
      A(4,1) = 3.94
      A(4,2) = 2.79
      A(4,3) = 3.07
      A(4,4) = 5.11 
      A(4,5) = 2.59
      do i = 1,5
          A(5,i) = 1
      end do
      n = 5
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