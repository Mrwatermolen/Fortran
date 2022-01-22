      program mh
      integer i,j,r
      real x(30),y(16),el(30),u(30,16),a0,a1,a2,t,tt,p,q,z
      real h,w,e,eps,pi,z0
      pi=3.1415926
      eps=1e-4
      e=pi/8.0
      h=0.1
      w=1.25
      z0=377.0
      open(1,file="1-3电势沿径向分布近似解.dat")
      open(2,file="1-3特性阻抗近似解.dat")

      do i=1,30
        x(i) = 4 + (i-1)*h
      end do
      do j=1,16
        y(j) = (j-1)*e
        u(1,j) = 18
        u(30,j) = 0
      end do
      

      do i=2,29
        do j=1,16
            u(i,j) = 0
        end do
      end do

10    p=0.0
      
      do i=2,29
        a0 = 1/(e*(i-1))**2
        a1 = 1.0-1/(2.0*i-2.0)
        a2 = 2-a1
        t=u(i,1)
      u(i,1)=(a0*(u(i,16)+u(i,2))+a1*u(i-1,1)+a2*u(i+1,1))/(2+2*a0)
        tt = u(i,1)
        u(i,1) = w*tt+(1-w)*t
        q=abs(t-u(i,1))
        if(q.gt.p) p=q
        do j=2,15
          t = u(i,j)
      u(i,j)=(a0*(u(i,j-1)+u(i,j+1))+a1*u(i-1,j)+a2*u(i+1,j))/(2+2*a0)
          tt = u(i,j)
          u(i,j)=w*tt+(1-w)*t
          q = abs(t-u(i,j))
          if(q.gt.p) p=q
        end do
        t=u(i,16)       
      u(i,16)=(a0*(u(i,15)+u(i,1))+a1*u(i-1,16)+a2*u(i+1,16))/(2+2*a0)
        tt=u(i,16)
        u(i,16)=w*tt+(1-w)*t
        q=abs(t-u(i,16))
        if(q.gt.p) p=q
      end do
      if(p.gt.eps) goto 10         

      do i=1,30
        do j=1,16
            write(1,*)x(i),y(j),u(i,j)
        end do
      end do

      do i=1,30
        if(i<=28) then
          el(i) = -(-u(i+2,1)+4*u(i+1,1)-3*u(i,1))/(2*h)
        else
          el(i) = -(u(i-2,1)-4*u(i-1,1)+3*u(i,1))/(2*h)
        end if
      end do

      s=0
      do i=1,30
        s = s + el(i)*h
      end do

      z=18*z0/(10*s)
      do r=1,5
        write(2,*)"Er=",r,z/(float(r))**0.5
      end do
      end
