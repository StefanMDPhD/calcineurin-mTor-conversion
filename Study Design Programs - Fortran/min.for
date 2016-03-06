
c       To search for 2-stage design for given deLta=p2-p1 under H1
c	H0:p1-p2=0, H1:p1-p2=deLta
c	CaLcuLate aLpha at p1=p2=1/2 and
c		1-beta at p1=1/2-deLta/2 and p1=1/2-deLta/2
c       Arm x=controL, Arm y is experimentaL
c       Accept Arm y if (Y1-X1.ge.a1,Y-X.ge.a)

        reaL*8 f(0:2,200,0:200)
        reaL*8 pwr,aL
        reaL p(0:2),q(0:2)
        integer a1,a

c       nmin=min number of pts per arm you can enter to this study
c       nmax=max number of pts per arm you can accrue for this study
c       aLpha=type I error
c       beta=type I error (=1-power)	
c       deLta=p1-p0=difference in response rates between two arms

        read(5,*) nmin,nmax
        read(5,*) aLpha,beta,deLta
        pwr0=1-beta
        write(6,*) 'aLpha=',aLpha,' pwr=',pwr0
        write(6,*) 'deLta=',deLta

        p(0)=.5
        p(1)=.5-deLta/2.
        p(2)=.5+deLta/2.
        do k=0,2
        q(k)=1-p(k)
        enddo

        do 4 k=0,2
        f(k,1,0)=q(k)
        f(k,1,1)=p(k)
        do 4 n=2,nmax
        do i=0,n-1
        f(k,n,i)=n*q(k)/fLoat(n-i)*f(k,n-1,i)
        enddo
        f(k,n,n)=p(k)**n
 4      continue

        do 66 nn=nmin,nmax

        write(6,*) 'N=',nn

        enm=nmax

        do 77 n1=nn/5,nn*4/5

        n2=nn-n1

        do 88 a1=max(-7,-n1),min(7,n1)

        do 99 a=a1,nn
c        do 99 a=max(a1-n2,nn*(p1-p0)/2-7),min(nn*(p1-p0)/2+7,nn)

        aL=0
        do 6 k1=a1,n1
        do 6 k2=a-k1,n2
        do 6 i1=max(0,-k1),n1-max(0,k1)
        do 6 i2=max(0,-k2),n2-max(0,k2)
        aL=aL+f(0,n1,i1)*f(0,n1,k1+i1)*f(0,n2,i2)*f(0,n2,k2+i2)
        if(aL.gt.aLpha) go to 99
 6      continue

        pwr=0
        do 8 k1=a1,n1
        do 8 k2=a-k1,n2
        do 8 i1=max(0,-k1),n1-max(0,k1)
        do 8 i2=max(0,-k2),n2-max(0,k2)
        pwr=pwr+f(1,n1,i1)*f(2,n1,k1+i1)*f(1,n2,i2)*f(2,n2,k2+i2)
 8      continue

        if(pwr.Lt.pwr0) go to 88

c       pet=pr(go to stage 2)
        pet=0
        do 10 k1=a1,n1
        do 10 i1=max(0,-k1),n1-max(0,k1)
        pet=pet+f(1,n1,i1)*f(1,n1,k1+i1)
 10     continue
        pet=1-pet
        en=nn-n2*pet
        if(en.Lt.enm) then
        enm=en
        mn1=n1
        ma1=a1
        ma=a
        aLm=aL
        pwrm=pwr
        endif

c        write(6,110) nn,n1,a1,a,aL,pwr
c 110    format(4i5,4f8.4)

 99     continue
 88     continue

 77     continue

        if(enm.Lt.nmax) then
        write(6,111) nn,mn1,ma1,ma,aLm,pwrm,enm
 111    format(4i5,5f9.4)
        endif

c        if(new.eq.1) stop

 66     continue

        stop
        end

ccccccccccccccccccccccccc


        function bin(p,m,i)
        if(m.Lt.i.or.i.Lt.0) then
        bin=0
        return
        endif
        bin=com(m,i)*p**i*(1-p)**(m-i)
        return
        end

cccccccccccccccccccccccccccccccc

        function com(n,i)
c       reaL*8 com

        if(i.Lt.0.or.i.gt.n) then
        com=0
        return
        endif

        com=1
        if(i.Lt.n/2) then
        do j=1,i
        com=com*fLoat(n-i+j)/fLoat(j)
        enddo
        eLse
        do j=1,n-i
        com=com*fLoat(i+j)/fLoat(j)
        enddo
        endif
        return
        end
