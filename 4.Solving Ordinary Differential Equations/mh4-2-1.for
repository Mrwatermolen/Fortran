      program mh
      external f
      dimension b(2)
      data b/0.1,0.4/
!     声明语句必须在执行前面。。。 
      open(1,file="vt.dat")
      open(2,file="xt.dat")
      w = 1
      x = 1.0
      t = 0
      v = 0.0
      v1 = 0
      rang =  100.
      dt = 0.5
      N = rang/dt
      do i = 1, 2
          do j = 0, N
          write(1,*)t,v
          write(2,*)t,x
          temp = x + v * dt
          v1 = v + f(v,x,b(i),w) * dt
          t = t + dt
          v = 0.5 * (v + v1 + f(v1,x,b(i),w) * dt)
          x = temp
          end do
          t = 0
          x = 1.0
          v = 0.0
      end do
      end
      
      
      function f(v,x,b,w)
      f = -(2*b*v + w*w*x)
      return
      end