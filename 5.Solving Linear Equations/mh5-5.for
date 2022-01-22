      program mh
      real * 8 A(4,4),b(4),x(4)
      data A/5,-1,-1,-1, -1,10,-1,-1, -1,-1,5,-1, -1,-1,-1,10/
      data b/4,12,8,34/
      eps = 1.0e-4
      N = 4
      call yakobi(A,b,N,x,eps)
      write(*,*)x
      do i = 1,N
          x(i) = 0
      end do
      w = 1
      call gaus_sor(A,b,N,x,eps,w)
      write(*,*)x
            do i = 1,N
          x(i) = 0
      end do
      w = 1.15
      call gaus_sor(A,b,N,x,eps,w)
      write(*,*)x
      end
      
      subroutine yakobi(A,b,N,x,eps)
      real * 8 A(4,4),b(4),x(4),s,t(4),p,q
10    p = 0
      t = x
      do i = 1,N
          s = 0
          do j = 1,N
              s = s + A(i,j)*t(j)
          end do
          x(i) = x(i) + (b(i) - s)/A(i,i)
          q = abs(x(i) - t(i))
          if(p < q) p = q
      end do
      if (p>eps) goto 10
      return
      end
      
      subroutine gaus_sor(A,b,N,x,eps,w)
      real * 8 A(4,4),b(4),x(4),s,t,p,q
20    p = 0
      do i = 1,N
          s = 0
          t = x(i)
          do j = 1,N
             s = s + A(i,j)*x(j)
          end do
          x(i) = x(i) + w*((b(i) - s))/A(i,i)
          q = abs(x(i) - t)
          if(p < q) p = q
      end do
      if (p>eps) goto 20
      return
      end