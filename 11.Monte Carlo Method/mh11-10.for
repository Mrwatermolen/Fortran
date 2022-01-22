      program main
        call solve()
        read(*,*)
        end

      subroutine solve()
        dimension f(3),f1(3),x(3),x1(3),b(3)
        real x0,b,eps,r
        N=0
        write(*,*)"x1,x2,x3,b1,b2,b3,N,eps"
        read(*,*)x(1),x(2),x(3),b(1),b(2),b(3),N,eps
        m=0
        call group(f,x)
4       if(abs(f(1))<=eps.and.abs(f(2))<=eps.and.abs(f(3))<=eps) goto 10
5       m=m+1
        if(m<=N) goto 7
        b=b/2.
        m=0      
7       do i=1,3
            call rand(r)
            x1(i)=x(i)+b(i)*(2.*r-1)
        end do
        call group(f1,x1)
        if(abs(f1(1))>abs(f(1)).and.abs(f1(2))>abs(f(2))
     &   .and.abs(f1(3))>abs(f(3))) goto 5
        do i=1,3
            x(i)=x1(i)
            f(i)=f1(i)
        end do
        do i=1,3
            write(*,*)x(i)
          end do  
        goto 4
10      do i=1,3
          write(*,*)x(i)
        end do     
        end

      subroutine group(f,x)
        dimension f(3),x(i)
        f(1)=3*x(1)+x(2)+2*x(3)*x(3)-3
        f(2)=-3*x(1)+5*x2*x(2)+2*x(1)*x(3)-1
        f(3)=25*x(1)*x(2)+20*x(3)+12
        return
        end

      subroutine rand(R)
      data K,J,M,RM/5701,3613,566927,566927./
      ir=int(R*RM)
      irand=mod(J*ir+k,M)
      R=(real(irand)+0.5)/RM
      return
      end

