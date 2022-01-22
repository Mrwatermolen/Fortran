      program mh
      external f,f1,f2
!     （1）能够直接迭代求解
      eps =  10e-5
      call smipleMethod(f,eps)
      call aitkenMethod(f,eps)
!     （2）不能能够直接迭代求解,转变为x=(ln(4-x))/(ln(2))
      call smipleMethod(f1,eps)
      call aitkenMethod(f1,eps)
!     （3）变为x=(((x+1)*0.5)**(1./3.))
      call smipleMethod(f2,eps)
      call aitkenMethod(f2,eps)
      end
      
      
      function f(x)
      f = (cos(x) + sin(x))/4.
      return
      end
      
      function f1(x)
      f1 = alog(4-x)/alog(2.0)
      return
      end
      
      function f2(x)
      f2 = (((x+1)*0.5)**(1./3.))
      return
      end
            
      subroutine smipleMethod(fun,eps)
      y = 1
10    x = y
      y = fun(x)
      if(abs(x-y)>=eps) goto 10
      write(*,*)fun(y)
      return
      end
      
      subroutine aitkenMethod(fun,eps)
      y = 1
20    x = y
      x1 = fun(x)
      x2 = fun(x1)
      if(x2==x1) then 
          write(*,*)x2
          return
      else
          y = (x*x2-x1**2)/(x-2*x1+x2)
      end if
      !     *********!!!!!!!!!!!!!*************
      if(abs(fun(y)-y)>=eps) goto 20
100   write(*,*)y
      return
      end