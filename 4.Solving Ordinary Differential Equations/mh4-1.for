
      program mh
      open(1,file="vt.dat")
      N = 1000
      xk = 0.05
      xm = 0.05
      g = 9.81
      v = 0.
      v0 = 0.
      t = 0
      rane =  3.
      dt = rane/N
!     1 
      do i = 0, N
          write(1,*)t,v
          t = t + dt
          v = v + (g - xk*v*v/xm) * dt
      end do
!     2
      v = 0
      t = 0
      v1 = 0
      do j = 0, N
          write(1,*)t,v
          v1 = v + (g - xk*v*v/xm) * dt
          t = t + dt
          v = 0.5 * (v + v1 + (g - xk*v1*v1/xm) * dt)
      end do
      end
      