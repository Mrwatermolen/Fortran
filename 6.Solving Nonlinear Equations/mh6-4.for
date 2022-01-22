      program mh
      eps =  10e-4
      a = 3.592
      b = 0.04267
      p = 200
      t = 500
      r = 0.082054
      call smipleMethod(a,b,p,t,r,eps)
      end
      
      subroutine smipleMethod(a,b,p,t,r,eps)
      y = 1
10    x = y
      y = (r*t*x*x)/(p*v*v+a) + b
      if(abs(x-y)>=eps) goto 10
      write(*,*)y
      return
      end