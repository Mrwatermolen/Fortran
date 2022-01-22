      program mh
      integer i,j,r
      real x(11),y(30),u(11,30),a0,a1,a2,t,tt,p,q,f
      real h,w,e,eps,pi
      pi=3.1415926
      eps=1e-5
      e=pi/15.0
      h=0.1
      w=1.25
      open(1,file="1-4近似解以及与精确解的绝对误差.dat")
      open(2,file="1-4精确解.dat")

      do i=1,11
        x(i) = (i-1)*h
      end do
      do j=1,30
        y(j) = (j-1)*e
        u(11,j) = 0
      end do
      

      do i=1,11
        do j=1,30
            u(i,j) = 0
        end do
      end do
      

10    p=0.0
      
      do i=2,10 
        a0 = 1/(e*(i-1))**2
        a1 = 1.0-1/(2.0*i-2.0)
        a2 = 2-a1
        t=u(i,1)
        f = -50*x(i)*x(i)*sin(2*y(1))*h*h
      u(i,1)=(a0*(u(i,30)+u(i,2))+a1*u(i-1,1)+a2*u(i+1,1)-f)/(2+2*a0)
        tt = u(i,1)
        u(i,1) = w*tt+(1-w)*t
        q=abs(t-u(i,1))
        if(q.gt.p) p=q
        do j=2,29
          f = -50*x(i)*x(i)*sin(2*y(j))*h*h
          t = u(i,j)
      u(i,j)=(a0*(u(i,j-1)+u(i,j+1))+a1*u(i-1,j)+a2*u(i+1,j)-f)/(2+2*a0)
          tt = u(i,j)
          u(i,j)=w*tt+(1-w)*t
          q = abs(t-u(i,j))
          if(q.gt.p) p=q
        end do
        t=u(i,30)
        f = -50*x(i)*x(i)*sin(2*y(30))*h*h   
      u(i,30)=(a0*(u(i,29)+u(i,1))+a1*u(i-1,30)+a2*u(i+1,30)-f)/(2+2*a0)
        tt=u(i,30)
        u(i,30)=w*tt+(1-w)*t
        q=abs(t-u(i,30))
        if(q.gt.p) p=q
      end do
      if(p.gt.eps) goto 10

      do i=1,11
        do j=1,30

          t=(25/6.0)*(x(i)*x(i))*(1-x(i)*x(i))*sin(2*y(j))
          write(1,*)x(i),y(j),u(i,j),abs(t-u(i,j))
          write(2,*)x(i),y(j),u(i,j),t
        end do
      end do

      end
