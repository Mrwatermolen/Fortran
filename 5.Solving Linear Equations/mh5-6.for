      program mh
      real * 8 A(3,3),b(3),x(3),w(3)
      data A/4,-1,0, -1,4,-1, 0,-1,4/
      data b/1,4,3/
      data w/1.03,1,1.1/
      eps = 1.0e-5
      N = 3
      do i = 1,N
          do j = 1,N
              x(j) = 0
          end do
          t = w(i)
          call gaus_sor(A,b,N,x,eps,t)
          write(*,*)x
      end do
      read(*,*)sss
      end
      
      subroutine gaus_sor(A,b,N,x,eps,w)
      real * 8 A(3,3),b(3),x(3),s,t,p,q
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