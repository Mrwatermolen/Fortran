      program main
        dimension x(6)
        real RND
        data Nt,Ng,Nf,Ns,dx/10,100,10,1000,0.5/
        RND=0.5
        do i=1,6
            call rand(RND)
            x(i) = 0.01*(RND-0.5)
        end do
        do j=1,Nt
            call walk(RND,dx,x)
        end do
        su=0
        su1=0
        sdu=0
        uba=0
        eps=0
        do 40 ig=1,Ng
            ug=0
            ug2=0
            do 30 k=1,Nf
                call walk(RND,dx,x)
                if(Mod(k,Nf).ne.0) goto 30
                x12=x(1)-x(4)
                y12=x(2)-x(5)
                z12=x(3)-x(6)
                r12=dist(x12,y12,z12)
                u=1/real(r12)
                ug=ug+u
                ug2=ug2+u*u
30              continue
            ug=ug/real(Ns)
            ug2=ug2/real(Ns)-ug*ug
            epsug=sqrt(ug2/real(Ns))
            su=su+ug
            su2=su2+ug*ug
40          sdu=sdu+epsug
        uba=su/real(Ng)
        eps=su2/real(Ng)-uba*uba
        du1=sqrt(eps/real(Ng))
        du2=sdu/real(Ng)
        d=du1-du2
        write(*,*)uba,du1,du2,d
        read(*,*)r1
        end

      subroutine rand(R)
      data K,J,M,RM/5701,3613,566927,566927./
      ir=int(R*RM)
      irand=mod(J*ir+k,M)
      R=(real(irand)+0.5)/RM
      return
      end

      subroutine walk(RND,dx,x)
        dimension x(6),x0(6)
        real f0,f
        call weight(x,f0)
        do i=1,6
            x0(i)=x(i)
            call rand(RND)
            x(i) = x(i) + dx*(RND-0.5)
        end do
        call weight(x,f)
        call rand(RND)
        if(f.ge.f0*RND) return
        do i=1,6
            x(i)=x0(i)
        end do
        return
        end

      subroutine weight(x,f)
        dimension x(6)
        r1=dist(x(1),x(2),x(3))
        r2=dist(x(4),x(5),x(6))
        f=2.34*exp(-3.375*(r1+r2))
        return
        end

      function dist(x,y,z)
        dist=sqrt(x*x+y*y+z*z)
        return
        end