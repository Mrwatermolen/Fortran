      program mh
      integer i,j
      real x(50),y(40),u(50,40),t,p,q
      real w,esp,h,pi
      open(1,file="1-1.dat")
      w=1.25
      eps=1e-4
      h=0.1
      pi=3.1415926

      do i=1,50
        x(i) = (i-1)*h
        u(i,1) = sin(pi*x(i)/4)
        u(i,40) = 0.0
      end do
      do j=1,40
        y(j) = (j-1)*h
        u(1,j) = y(j) * (y(j) - 3)
        u(50,j) = 0.0
      end do

      do i =2,49
        do j=2,39
            u(i,j)=0.0
        end do
      end do
10    p = 0.0
      do i =2,49
        do j=2,39
            t = u(i,j)
          u(i,j)=w*(u(i,j-1)+u(i-1,j)+u(i+1,j)+u(i,j+1))/4+(1-w)*u(i,j)
            q = abs(u(i,j) - t)
            if(q.gt.p) p=q
        end do
      end do
      if(p.gt.eps) goto 10

      do i =1,50
        do j=1,40
            write(*,*)x(i),y(j),u(i,j)
            write(1,*)x(i),y(j),u(i,j)
        end do
      end do  
      end