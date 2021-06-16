      implicit none
      integer,parameter :: hashn=10000000,maxnd=600000
      integer :: id,iw1,iw2,na,np,kp(50),jken,iken,kai(maxnd),num,i,j
      integer :: ip,neqn,count,nn,kaik(maxnd),nut,ja(maxnd),ija,iw3,iw11
      integer :: nk,mp,lp(50),nnn(50),newc(maxnd),numc(hashn),ipa,ns
      integer :: numc2(hashn),kk,num2(maxnd),ick(hashn)
      integer :: iw4,ip2,idchange(hashn)
      character :: aa40*40,akm(hashn)*40,aa61*61,ap(50)*2,akai(maxnd)*40
      character :: fm*14,fmc(maxnd)*14,bp(50)*2,zen*49,kou*40
      character :: oldc(maxnd)*14,akaik(maxnd)*40,cp*2,b11*11,
     +             qfarm(50000)*14 
c      open(1,file='sosikidt',status='old')
      open(1,file='kaikumidt.prn',status='old') !kaikumidtは旧新のsosikidtを比較して，新規追加あればkaikumidtに追加していく。
      open(2,file='kaiexg3',status='old')
      open(3,file='fmc2.dat',status='old')
      open(4,file='fmc2add_notG.dat')
      open(5,file='repeatV_afc.dat',status='old')
      open(90,file='input_id.txt')
      open(91,file='input_cid.txt')
      open(100,file='99.tmp')
      write(*,*)
     +'何県のデータを分析しますか'
      write(*,*)'全県はALを入力,東北はTH,中部はCT,中国地方はCH,九州はQI'
      read(*,*) cp
      write(90,*) cp
      write(91,*) cp
      open(6,file='repeatT_afc2_'//cp//'_notG.dat')
c      write(*,*) '何頭以上を単独で扱うか？'
c      read(*,*) nn
      nn=0
      akm='＊　　　　　　　　　　　　　　　　　　　'
      do while( .not.eof(1) )
        read(1,'(i6,x,a40,x,i6)') id,aa40,iw1
        call hash(id,ip,neqn,count )
        idchange(ip)=iw1
        call hash(iw1,ip,neqn,count )
        akm(ip)=aa40
      end do
      np=0
      jken=0
      na=0
      do while( .not.eof(2) )
        read(2,'(i4,i3,a61,i2,i4)') iw1,iw3,aa61,iw2,iw4
        backspace(2)
        na=na+1
        read(2,'(a14)') fmc(na)
        iken=(iw1-mod(iw1,100))/100
        if( iken.ne.jken ) then
          np=np+1
          ap(np)=fmc(na)(1:2)
          kp(np)=na
          jken=iken
        end if
        if( iw2.ne.0 ) then
          iw11=iw1*100+iw2
        else
          iw11=iw1*100+iw2
          call hash( iw11,ip,neqn,count )
          call hash( idchange(ip),ip2,neqn,count)
          if( akm(ip2)(1:1).ne.'＊') go to 55
          if( iw4.ne.0 ) then
            do i=1,99
              iw11=iw1*100+i
              call hash( iw11,ip,neqn,count )
              call hash( idchange(ip),ip2,neqn,count)
              if( akm(ip2)(1:1).ne.'＊') go to 55
            end do
          else
            iw11=iw1*100+iw2
          end if
        end if
        iw11=iw1*100+iw2
55      continue
        call hash( iw11,ip,neqn,count )
        call hash( idchange(ip),ip2,neqn,count)
        ija=mod(iw1,100)*1000+iw3
        kai(na)=idchange(ip)
        ja(na)=ija
        akai(na)=akm(ip2)
      end do
      np=np+1
      kp(np)=na+1
      mp=0
      nnn=0
      nk=0
c      do i=1,np-1
c        write(*,*) ap(i),kp(i)
c      end do
      do while( .not.eof(3) )
        read(3,'(a14,i6)') fm,num
        if( cp.ge.'01'.and.cp.le.'47'.and.fm(1:2).ne.cp ) cycle
        if( cp.eq.'TH'.and.fm(1:2).ge.'11' ) cycle
        if( cp.eq.'CT' ) then
          if( fm(1:2).le.'14'.or.fm(1:2).ge.'28') cycle
        end if
        if( cp.eq.'CH' ) then
          if( fm(1:2).le.'27'.or.fm(1:2).ge.'36') cycle
          if( fm(1:2).eq.'29'.or.fm(1:2).eq.'30') cycle
        end if
        if( cp.eq.'QI' ) then
          if( fm(1:2).le.'39'.or.fm(1:2).ge.'48') cycle
        end if
c        write(*,*) fm
        do i=1,np
          if( ap(i).eq.fm(1:2) ) then
            do j=kp(i),kp(i+1)-1
              if( fmc(j).eq.fm ) then
                nk=nk+1
                num2(nk)=num
                oldc(nk)=fm
                kaik(nk)=kai(j)
c                write(*,*) kai(j)
                akaik(nk)=akai(j)
                iken=(kai(j)-mod(kai(j),10000))/10000
                if( jken.ne.iken ) then
                  mp=mp+1
                  bp(mp)=fm(1:2)
                  lp(mp)=nk
                  jken=iken
                end if
                if( num.ge.nn ) then
                  nnn(iken)=nnn(iken)+1
                  newc(nk)=iken*1000000+100000+nnn(iken)
                  call hash( newc(nk),ip,neqn,count )
                  numc(ip)=num
                else
                  if( akai(j).ne.
     +                   '　　　　　　　　　　　　　　　　　　　　'.
     +                 or.akai(j)(1:2).ne.'＊')then
                    newc(nk)=iken*1000000+700000+mod(kai(j),10000)
                  else
                    newc(nk)=iken*1000000+800000+ja(j)
                  end if
                  call hash( newc(nk),ip,neqn,count )
                  numc(ip)=numc(ip)+num
                end if
                go to 9
              end if
            end do
            nk=nk+1
            num2(nk)=num
            oldc(nk)=fm
            akaik(nk)='　　　　　　　　　　　　　　　　　　　　'
            write(100,'(a2)') fm(1:2)
            backspace(100)
            read(100,'(i2)') iken
            nnn(iken)=nnn(iken)+1
            newc(nk)=iken*1000000+100000+nnn(iken)
            call hash( newc(nk),ip,neqn,count )
            numc(ip)=num
9           continue
          end if
        end do
      end do
      mp=mp+1
      lp(mp)=nk+1
      ns=0
      numc2=0
      do i=1,nk
        call hash(newc(i),ip,neqn,count )
        if( numc(ip).lt.nn ) then
          iken=(newc(i)-mod(newc(i),1000000))/1000000
c          newc(i)=iken*1000000+900000
          kk=num2(i)
          call hash(newc(i),ip,neqn,count )
          numc2(ip)=numc2(ip)+kk
          write(4,'(a14,i6,i7,x,a40,i9)') 
     +   oldc(i),numc2(ip),kaik(i),akaik(i),newc(i)
          go to 101
        end if
        write(4,'(a14,i6,i7,x,a40,i9)') 
     +   oldc(i),numc(ip),kaik(i),akaik(i),newc(i)
101     continue
      end do
      do i=1,nk
        call hash(newc(i),ip,neqn,count )
        if( numc(ip).ge.nn ) cycle
        iken=(newc(i)-mod(newc(i),1000000))/1000000
c        newc(i)=iken*1000000+900000
        call hash(newc(i),ip,neqn,count )
        if( numc2(ip).ge.nn ) then
          write(4,'(a14,i6,i7,x,a40,i9)') 
     +   oldc(i),numc2(ip),kaik(i),akaik(i),newc(i)
          go to 102
        else
          newc(i)=48*1000000+900000
         end if
        write(4,'(a14,i6,i7,x,a40,i9)') 
     +   oldc(i),numc2(ip),kaik(i),akaik(i),newc(i)
102     continue
      end do
      
      rewind(4)
      ick=0
      do while( .not.eof(4) )
        read(4,'(68x,i9)') iw1
        call hash( iw1,ip,neqn,count )
        ick(ip)=1
      end do
      
      nk=0
      b11='00000000000'
      do while( .not.eof(5) )
        read(5,'(a49,a30,x,a14,x,i3)') zen,kou,fm,ipa
        if( cp.ge.'01'.and.cp.le.'47'.and.fm(1:2).ne.cp ) cycle
        if( cp.eq.'TH'.and.fm(1:2).ge.'11' ) cycle
        if( cp.eq.'CT' ) then
          if( fm(1:2).le.'14'.or.fm(1:2).ge.'28') cycle
        end if
        if( cp.eq.'CH' ) then
          if( fm(1:2).le.'27'.or.fm(1:2).ge.'36') cycle
          if( fm(1:2).eq.'29'.or.fm(1:2).eq.'30') cycle
        end if
        if( cp.eq.'QI' ) then
          if( fm(1:2).le.'39'.or.fm(1:2).ge.'48') cycle
        end if
        if( cp.ne.'AL') then
          if( b11.ne.zen(1:11)) write(90,'(a11)') zen(1:11)
          nk=nk+1
          write(91,'(i11,a22)') nk,zen(12:22)//zen(1:11)
        end if
        b11=zen(1:11)
        do i=1,mp
          if( bp(i).eq.fm(1:2) ) then
            do j=lp(i),lp(i+1)-1
              if( oldc(j).eq.fm ) then
                write(6,'(a49,i3,i9,a30,x,a14)') 
     +           zen,ipa,newc(j),kou(1:30),fm
                go to 100
              end if
            end do
          end if
        end do
c        write(6,'(a49,i3,x,a8,a30)') zen,ipa,fm(1:2)//'900000',kou(1:30)
        write(100,'(a8)') fm(1:2)//'900000'
        do i=1,ns
          if( qfarm(i).eq.fm ) then
            write(6,'(a49,i3,x,a8,a30,x,a14)') 
     +       zen,ipa,fm(1:2)//'900000',kou(1:30),fm
            go to 100
          end if
        end do
        ns=ns+1
        qfarm(ns)=fm
        backspace(100)
        read(100,'(i8)') iw1
c        call hash(iw1,ip,neqn,count)
c        if( ick(ip).eq.0 ) then 
c          iw1=48900000
c          write(6,'(a49,i3,i9,a30,x,a14)') zen,ipa,iw1,kou(1:30),fm
c          write(4,'(a14,i6,x,a6,x,20a2,i9)') 
c     +   fm,numc(ip),fm(1:2)//'0000',("　",j=1,20),iw1
c          go to 100
c        end if
        write(6,'(a49,i3,x,a8,a30,x,a14)') 
     +   zen,ipa,fm(1:2)//'900000',kou(1:30),fm
c        write(4,'(a14,i6,x,a6,x,20a2,i9)') 
c     +   fm,numc(ip),fm(1:2)//'0000',("　",j=1,20),iw1
100     cycle
      end do
      
      
      
      
      stop
      end

c----------------------------------------------------------------------
c
      SUBROUTINE HASH(K,IP,NEQN,COUNT)
c
c-----------------------------------------------------------------------
c      PARAMETER(M=31,M1=17) !origin
       PARAMETER(M=10000000,M1=137) !素数

      INTEGER COUNT
      INTEGER NQARAY(M),IDARAY(M)
      DATA NQARAY/M*0/
      DATA IDARAY/M*0/

      I=MOD(K,M)+1
      J=MOD(K,M1)+2
      IQ=IDARAY(I)
      
 1    CONTINUE
      IF(IQ.NE.K.AND.IQ.NE.0) THEN
        I=I+J
        IF(I.GT.M) I=I-M
        IQ=IDARAY(I)
        GO TO 1
      END IF
      IF(IQ.EQ.0) IDARAY(I)=K
        NQARAY(I)=NQARAY(I)+COUNT
      IP=I
      NEQN=NQARAY(I)
      RETURN
      END
