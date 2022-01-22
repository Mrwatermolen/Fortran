      program main
        dimension x(4),y(4),G(3,3),gama(3),uq0(3),uq(3)
	    x(1)=0.
	    y(1)=0.
	    x(2)=1.
	    y(2)=0.
	    x(3)=0.
	    y(3)=1.
	    xd=0.3333
        yd=0.3333
        uq0(1)=0.
        uq0(2)=1.
        uq0(3)=0.
        gama(1)=0
        gama(2)=0
        gama(3)=1
        call matrix(3,x,y,gama,uq0,G,uq)
        write(*,20)G,uq
20      format(1x,3d15.6)
        call gauss(3,G,uq)
	    do 5 j=1,3
	    if(gama(j))5,5,4
4       aa=uq0(j)
        uq0(j)=uq(j)
	    uq(j)=aa
5       continue
        write(*,*)(uq0(i),i=1,3)
        write(*,*)(uq(i),i=1,3)
        read(*,*)dadasda
        end


        subroutine matrix(n,x,y,gama,uq0,G,uq)
        dimension x(4),y(4),xm(3),ym(3),H(3,3),G(3,3),uq(3)
        dimension uq0(3),gama(3)
        x(n+1)=x(1)
        y(n+1)=y(1)
        do 10 j=1,n
        xm(j)=0.5*(x(j)+x(j+1))
10      ym(j)=0.5*(y(j)+y(j+1))
        do 30 i=1,n
        H(i,i)=-3.14159
        s=sqrt((x(i+1)-x(i))**2+(y(i+1)-y(i))**2)
        G(i,i)=s*(alog(s)-1.69315)
        do 30 j=1,n
        if((i-j).eq.0) goto 30
        call HGij(xm(i),ym(i),x(j),y(j),x(j+1),y(j+1),H(i,j),G(i,j))
30      continue
        do 50 j=1,n
        if(gama(j)) 50,50,40
40      do 55 i=1,n
        CH=G(i,j)
        G(i,j)=-H(i,j)
        H(i,j)=-CH
55      continue
50      continue
        do 60 i=1,n
        uq(i)=0.
        do 70 j=1,n
70      uq(i)=uq(i)+H(i,j)*uq0(j)
60      continue
        return   
        end   


        subroutine HGij(xi,yi,x1,y1,x2,y2,H,G)
        x21=x2-x1
        y21=y2-y1
        x1i=x1-xi
        y1i=y1-yi
        x2i=x2-xi
        y2i=y2-yi
        s=sqrt((x21*x21+y21*y21))
        d=-(x1i*x21+y1i*y21)/s
        r1=sqrt(x1i*x1i+y1i*y1i)
        r2=sqrt(x2i*x2i+y2i*y2i)
        rd=(x1i*y21-y1i*x21)/s
        H=acos((x1i*x2i+y1i*y2i)/(r2*r1))
        G=(s-d)*alog(r2)+d*alog(r1)-s+rd*H
        return
        end


        subroutine gauss(N,A,B)
        dimension A(N,N),B(N)
        do 60 K=1,N-1
        P=0.0
        do 30 I=K,N
        if(abs(A(I,K)).le.abs(P)) goto 30
        P=A(I,K)
        I0=I
30      continue
        do 40 J=K,N
        T=A(K,J)
        A(K,J)=A(I0,J)
40      A(I0,J)=T
        T=B(K)
        B(K)=B(I0)
        B(I0)=T
        B(K)=B(K)/A(K,K)
        do 60 J=K+1,N
        A(K,J)=A(K,J)/A(K,K)
        do 50 I=K+1,N
50      A(I,J)=A(I,J)-A(I,K)*A(K,J)
60      B(J)=B(J)-A(J,K)*B(K)
        B(N)=B(N)/A(N,N)
        do 80 K=1,N-1
        I=N-K
        S=0.0
        do 70 J=I+1,N
70      S=S+A(I,J)*B(J)
80     B(I)=B(I)-S
        return
        end


        subroutine inter(n,xd,yd,x,y,uq0,uq,ud)
        dimension uq0(3),uq(3),x(4),y(4)
        x(n+1)=x(1)
        y(n+1)=y(1)
        ud=0.
        do 30 j=1,n
        call HGij(xd,yd,x(j),y(j),x(j+1),y(j+1),A,B)
30      ud=ud+A*uq0(j)-B*uq(j)
        ud=ud/6.2838
        return
        end