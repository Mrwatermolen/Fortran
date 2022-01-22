      program mh
      integer i,j
      real x(10),y(5),u(10,5),u1(10,5),t,p,q
      real h,w,e,eps
      open(1,file="approximate1-2.dat")
      h = 0.2
      w = 1.25
      e = exp(1.0)
      eps = 1e-4


      do i=1,10
        x(i) = (i-1)*h
        u(i,1) = x(i)
        u(i,5) = e*x(i)
      end do
      do j=1,5
        y(j) = (j-1)*h
        u(1,j) = 0
        u(10,j) = 2*e**y(j)
      end do

      do i =2,9
        do j=2,4
            u(i,j)=0.0
        end do
      end do
10    p = 0.0
      do i =2,9
        do j=2,4
            t = u(i,j)
          u(i,j)=w*(u(i,j-1)+u(i-1,j)+u(i+1,j)+u(i,j+1))/4+(1-w)*u(i,j)
            q = abs(u(i,j) - t)
            if(q.gt.p) p=q
        end do
      end do
      if(p.gt.eps) goto 10
      
      do i=1,10
        do j=1,5
            u1(i,j) = abs(u(i,j) - x(i)*e**y(j))
            write(*,*)"***********Aproximate***********"
            write(*,*)x(i),y(j),u(i,j)
            write(*,*)"***********Contrast**************"
            write(*,*)u1(i,j)
            write(1,*)x(i),y(j),u(i,j),u1(i,j)
        end do
      end do
      end

      
      