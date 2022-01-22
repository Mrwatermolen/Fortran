      program mh1
      write(*,*)"Input:"
      read(*,*)x
      y=0
      if(x.ge.0.and.x.lt.10)y=x
      if(x.ge.10.and.x.lt.20)y=x*x+1
      if(x.ge.20.and.x.lt.30)y=x**3+x**2+1
      write(*,*)y
      end
          