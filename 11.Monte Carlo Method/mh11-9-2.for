      program main
        call solve()
        read(*,*)
        end

      subroutine solve()
        real x0,b,eps,r
        N=0
        write(*,*)"x0,b,N,eps"
        read(*,*)x0,b,N,eps
        m=0
        f0=f(x0)
4       if(abs(f0)<=eps) goto 10
5       m=m+1
        if(m<=N) goto 7
        b=b/2.
        m=0
7       call rand(r)
        x1=x0+b*(2.*r-1)
        f1=f(x1)
        if(abs(f1)>abs(f0)) goto 5
        x0=x1
        f0=f1
        goto 4
10      write(*,*)x0
        end

      function f(x)
        f=6*exp(-x)+x-5
        return
        end

      subroutine rand(R)
      data K,J,M,RM/5701,3613,566927,566927./
      ir=int(R*RM)
      irand=mod(J*ir+k,M)
      R=(real(irand)+0.5)/RM
      return
      end

