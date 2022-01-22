      program mh
        dimension x(11),u(11),R(11),A(11,3)
        integer N,i,j,k,kmax
        real a0,a1,a2,b1,b2,c1,c2,t,h,tt,D,P
        real P1,P2
        a0 = 1.0
        a1 = 1
        a2 = 1
        b1 = 1
        b2 = -1
        c1 = 0
        c2 = 0
        t = 10
        h = 0.1
        D = 0.1
        tt = 0.1
        P = tt*D/(h*h)
        P1 = 1/P+1
        P2 = 1/P-1
        N = 1 + a0/h
        kmax=t/tt
        open(1,file="1-5近似解及其与精确解的相对误差.dat")
        
        do i=1,N
            x(i)=(i-1)*h
            u(i)=exp(x(i))
        end do
        
        call ceof(N,a1,b1,a2,b2,h,P1,A)

        do k=1,kmax
            t = k*tt
            R(1) = (b1*P2-h*a1)*u(1)+b1*u(2)+2*h*c1
            R(N) = (b2*P2-h*a2)*u(N)+b2*u(N-1)+2*h*c2
            do i=2,N-1
                R(i) = u(i-1)+2*P2*u(i)+u(i+1)
            end do
            call solve(N,A,R)
            do i=1,N
                x(i)=(i-1)*h
                u(i)=R(i)
                temp = exp(x(i)+0.1*t)
                write(*,*)t,x(i),u(i),abs(u(i)-temp)/temp
                write(1,*)t,x(i),u(i),abs(u(i)-temp)/temp
            end do
        end do
        end

        subroutine ceof(N,a1,b1,a2,b2,h,P1,A)
            dimension A(N,3)
            A(1,2) = P1*b1+h*a1
            A(1,3) = -b1
            A(N,2) = P1*b2+h*a2
            A(N,1) = -b2

            do i=2,N-1
                A(i,1) = -1
                A(i,2) = 2*P1
                A(i,3) = -1
            end do

            do i=2,N
                A(i,2) = A(i,2)-A(i,1)*A(i-1,3)/A(i-1,2)
            end do
            return
        end

        subroutine solve(N,A,R)
            dimension A(N,3),R(N)
            do i=2,N
                R(i) = R(i)-A(i,1)*R(i-1)/A(i-1,2)
            end do
            R(N)=R(N)/A(N,2)

            do j=1,N-1
                i = N-j
                R(i) = (R(i)-A(i,3)*R(i+1))/A(i,2)
            end do
                return
        end