      program mh
      external f
      open(2,file="xd.dat")
      qm = 0.05
      qk = 0.02
      ql = 1.016
      g = 9.81
      w = 0.0
      d = 0.001
      t = 0.0
      w1 = 0.0
      rang =  25.
      dt = 0.01
      N = rang/dt
      do j = 0, N
       write(2,*)t,d
       temp = d + w * dt
       w1 = w + f(qm,ql,qk,g,w,d) * dt
       t = t + dt
       w = 0.5 * (w + w1 + f(qm,ql,qk,g,w1,d) * dt)
       d = temp
      end do
      end
      
      
      function f(qm,ql,qk,g,w,d)
      f = -(qm*g*sin(d) + qk*ql*w)/(qm * ql)
      return
      end