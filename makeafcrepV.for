c     農家不明は除外
      implicit none
      integer,parameter :: hashn=13466917
      integer :: isex,id,iw1,idam(hashn),isan,iwv(4),iy(3),isire,jy(3)
      integer :: iglt,idop,iciv,ir,nsan,ky(3),ip,neqn,count,nage,idx,nc
      integer :: iday,kday,jd,iyear,imon,i,j,ido,nnut(hashn),nut,nk,iyn
      integer :: idono,msan,idr,jd2,jwv(3),ly(3),my(3),jym(hashn),iym,
     +jciv,icz,iyn2,ir2
      real(8) :: afc,rciv,rage,cage,rday,rciv2
      character :: fm15*15,afarm*15,aken*2,aja*7,bb15*15,ff(3)*15
c      open(1,file='ETcut730_a.dat',status='old')
      open(1,file='ETcut730_t.dat',status='old')
c      open(2,file='RepeatA_afc.dat',status='unknown')
      open(2,file='RepeatV_afc.dat',status='unknown')
c      open(21,file='Repeat12_afc.dat',status='unknown')
      open(21,file='Repeat13_afc.dat',status='unknown')
      open(3,file='input_id.dat',status='unknown')
      open(31,file='input_cid.dat',status='unknown')
      open(4,file='ketof',status='old')
      open(6,file='kihoncombo',status='old')
      open(5,file='fmc.dat')
      open(7,file='nage_p3.dat')
      open(99,file='farm_unknown')
      open(100,file='dop_irreglar')
      open(200,file='dead_next')
      open(300,file='dop_365over')

c      write(*,*) '何産次の到達日齢を取り出しますか'
c      read(*,*) nage
      write(*,*) 
     + '審査年月日に制限を加えますか？（抽出日から２年半以前が目安）'
      write(*,*) 'yes=>[1],no=>[2]'
      read(*,*) iyn
      if( iyn.eq.1 ) then
        write(*,*) 
     +   '何年何月以前のものにしますか？ex:201010 (2010年10月)'
        read(*,*) iym
      else
        iym=300000
      end if
      write(*,*)'空胎期間の上限を364日に制限しますか？yes=>[1],no=>[2]'
      read(*,*) iyn2
      if( iyn2.eq.1 ) then
        icz=364
      else
        icz=999
      end if
      jd=0
      write(3,*) nage
      write(31,*) nage
      do while( .not.eof(4) )
        read(4,'(i1,i9,54x,i9)') isex,id,iw1
        call hash( id,ip,neqn,count )
        idam(ip)=iw1
      end do
      nnut=0
      do while( .not.eof(6) )
        read(6,'(i10,147x,i1,x,i1,i2,i2)') id,nut,(iwv(j),j=1,3)
        call hash( id,ip,neqn,count )
        nnut(ip)=nut
        if( iwv(1).eq.4) jym(ip)=(1988+iwv(2))*100+iwv(3)
        if( iwv(1).eq.5) jym(ip)=(2018+iwv(2))*100+iwv(3)
      end do
      nk=0
      jd=0
      jd2=0
      do while( .not.eof(1) )
        read(1,*,end=500) id,isan,(iwv(j),j=1,4),(iy(j),j=1,3),isire,
     +  (jy(j),j=1,3),iglt,idop,iciv,ir,nsan,
     +  (ky(j),j=1,3),cage,afc,idr,iw1,fm15,(ly(j),j=1,3),(my(j),j=1,3) !my（前の産次の年月日，初産月齢は当該年月日）
        if( id.ne.jd2 ) then
          afarm=fm15
          aken=fm15(1:2)
          aja=fm15(1:7)
          rciv=0.0d0
          msan=0
c          idr=0
          nc=0
          ido=0
          ir2=0
        end if
        if( nsan.ge.2.and.idop.le.20 ) then
          write(100,'(i10,i4,4i2,2(i5,2i3),2i5,x,a15)') 
     +     id,isan,(iwv(j),j=1,4),(jwv(j),j=1,3),(jy(j),j=1,3),
     +idop,iciv,fm15
           go to 100 !!!!!!!!!!!!!!!!!空胎期間20日以内は除く!!!!!!!!!!!!!!!!!!!!! 2013/11/24 14=>20
        end if
        if( nsan.ge.2.and.idop.gt.icz ) then
          write(300,'(i10,i4,4i2,2(i5,2i3),2i5,x,a15)') 
     +     id,isan,(iwv(j),j=1,4),(jwv(j),j=1,3),(jy(j),j=1,3),
     +idop,iciv,fm15
           go to 100 !!!!!!!!!!!!!!!!!空胎期間の上限値を除く!!!
        end if
        call hash( id,ip,neqn,count )
c        if( fm15.eq.'000000000000000') then !農家不明も分析対象とする
c          idx=id
c          write(99,'(i10,i4)') id,isan
c        end if
c        if( iwv(4).ge.4 ) cycle
         if( iwv(4).ge.4 ) then
c            idx=id
c            cycle
             go to 100
         end if
         if( iwv(2).ge.2 ) then  
           if( iwv(2).ne.5 ) then
c             if( nsan.eq.1 ) then
c               idx=id !初産月齢の記録を持たないものは分析から除外
c             end if
c            cycle
             go to 100
           end if
         endif
         if( iwv(1).eq.1 ) then  !ドナーの場合(nage5の!newaddを有効にする必要あり) !レシピは有効
c           if( nsan.eq.1 ) then
c               idx=id !初産月齢の記録を持たないものは分析から除外
c           end if
c           cycle !ドナーやレシピに供した記録だけを削除newadd
            go to 100
         endif
c        if( id.eq.idx ) cycle
        if( nnut(ip).eq.0 ) then
c           write(*,*) '基本ファイルに無い=>除外',id
c           cycle
            go to 100
        endif
c        if( nsan.ge.2 ) then
c          if( fm15(1:2).ne.aken ) then
c            ido=4
c          end if
c          if( fm15(1:7).ne.aja ) then
c            if( ido.lt.3 ) ido=3
c          end if
c          if( fm15(1:14).ne.afarm(1:14) ) then
c            if( ido.lt.2 ) ido=2
c          end if
c        rciv=rciv+dble( iciv )
c        end if
c        if( nsan.eq.(nage-1) ) then
c        end if
        if( fm15(1:2).ne.'45' ) then !宮崎口蹄疫フラグは宮崎県のみ有効にする(ir:nage_recegg.forでフラグを立てている）
          ir=1
        else
          ir=ir+1
        end if
        if( nsan.ge.1 ) then
c          call calend (iday,iy)
          call calend (iday,my)!年齢を前の産次で計算する
          call calend (kday,ky)
          rday= dble( iday-kday )
          rciv2=rciv/dble( nsan-1 )
          rage=rday/365.0d0
c          if( ido.eq.0 ) ido=1
          nk=nk+1
          if( nsan.eq.1 ) then
            idono=1
          else
            if( iw1.eq.1 ) then !前の産次が流死産
c              idono=4 !前の産次
               write(200,'(i10,i4,4i2,2(i5,2i3),2i5,x,a15)') 
     +         id,isan,(iwv(j),j=1,4),(jwv(j),j=1,3),(jy(j),j=1,3),
     +         idop,iciv,fm15
               go to 100 !前の産次が流死産は除く
            else
              if( iw1.eq.2 ) then!前の産次がレシピ
                idono=3
              else
                idono=idr+1 ! [1normal][2donornext][3recipnext]
                if( iwv(1).eq.2 ) then
                  idono=4
                end if
                if( ly(1).ne.0 ) then
                  idono=5 !2013/11/26
                end if
              end if
            end if
          end if
          if( jym(ip).gt.iym ) go to 100
          if( nsan.ge.2 ) then
            write(2,'(3i11,i5,i3,4i2,3f10.3,x,a15,x,i2,i7)') 
     +      id,isire,idam(ip),my(1),my(2),iwv(4),nnut(ip),idono,ir,
     +      rage,0.0d0,dble(iciv),
     +      fm15,nsan,jym(ip)
            msan=nsan
            nc=nc+1
            if( nsan.eq.2.and.nc.eq.2 ) then
              jciv=jciv+iciv
              ff(2)=fm15
              if( ff(2)(1:14).ne.ff(1)(1:14) ) then
                if( ff(2)(1:2).ne.ff(1)(1:2) ) then
                  ido=4
                else
                  if( ff(2)(1:7).ne.ff(1)(1:7) ) then
                    ido=3
                  else
                    ido=2
                  end if
                end if
              end if
              if( idono.eq.5 ) then !2013/11/26
                ir2=3
              else
                if( idono.eq.4 ) then
                  ir2=2
                else
                  if( ir2.ne.2 ) ir2=1
                end if
              end if
            end if
            if( nsan.eq.3.and.nc.eq.3 ) then
              jciv=jciv+iciv
              if( fm15(1:14).ne.ff(2)(1:14) ) then
                if( fm15(1:2).ne.ff(2)(1:2) ) then
                  ido=4
                else
                  if( fm15(1:7).ne.ff(2)(1:7) ) then
                    if( ido.le.2 ) ido=3
                  else
                    if( ido.le.1 ) ido=2
                  end if
                end if
              else
                if( ido.eq.0 ) then
                  ido=1
                end if
              end if
              if( idono.eq.5 ) then
                ir2=3
              else
                if( idono.eq.4 ) then
                  if( ir2.ne.3 ) ir2=2
                else
                  if( ir2.ne.2.and.ir2.ne.3 ) ir2=1
                end if
              end if
              write(7,'(3i11,i5,i3,4i2,3f10.3,x,a15,x,i2,i7)') 
     +        id,isire,idam(ip),my(1),my(2),iwv(4),nnut(ip),ido,ir2,
     +        rage,afc,dble(jciv)/2.00d0,fm15,nsan,jym(ip)
            end if
          else
            write(2,'(3i11,i5,i3,4i2,3f10.3,x,a15,x,i2,i7)') 
     +      id,isire,idam(ip),my(1),my(2),iwv(4),nnut(ip),idono,ir,
     +      0.0d0,afc,dble(iciv),
     +      fm15,nsan,jym(ip)
            msan=nsan
            nc=nc+1
            if( nsan.eq.1.and.nc.eq.1 ) then
              ff(1)=fm15
              ido=0
              jciv=iciv
              if( idono.eq.5 ) then
                ir2=3
              else
                if( idono.eq.4 ) then
                  if( ir2.ne.3 ) ir2=2
                else
                  if( ir2.ne.2.and.ir2.ne.3 ) ir2=1
                end if
              end if
            end if
          end if
          if( nsan.ge.2.and.nsan.le.3.and.iwv(4).le.2 ) then
            write(21,'(3i11,i5,i3,4i2,3f10.3,x,a15,x,i2,i7)') 
     +      id,isire,idam(ip),my(1),my(2),iwv(4),nnut(ip),idono,ir,
     +      rage,0.0d0,dble(iciv),
     +      fm15,nsan,jym(ip)
            write(31,'(3i11)') nk,isire,id
            msan=nsan
            if( id.ne.jd ) then
               write(3,'(i10)') id
               jd=id
            end if
          end if
          if( nsan.eq.1 ) then
            write(21,'(3i11,i5,i3,4i2,3f10.3,x,a15,x,i2,i7)') 
     +      id,isire,idam(ip),my(1),my(2),iwv(4),nnut(ip),idono,ir,
     +      0.0d0,afc,dble(iciv),
     +      fm15,nsan,jym(ip)
            write(31,'(3i11)') nk,isire,id
            write(5,'(a14)') fm15(1:14)
            bb15=fm15
            msan=nsan
           if( id.ne.jd ) then
             write(3,'(i10)') id
             jd=id
           end if
          else
            if( nsan.le.3.and.fm15(1:14).ne.bb15(1:14) ) then
              write(5,'(a14)') fm15(1:14)
              bb15=fm15
            end if
          endif
        end if
100     continue
c        iyear=iy(1)
c        imon=iy(2)
c        idr=iwv(1)
        jd2=id
        do i=1,3
          jwv(i)=iy(i)
        end do
      end do
500   continue
      stop
      end
c----------------------------------------------------------------------
c
      SUBROUTINE HASH(K,IP,NEQN,COUNT)
c
c-----------------------------------------------------------------------
c      PARAMETER(M=31,M1=17) !origin
       PARAMETER(M=13466917,M1=9999991) !素数

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
c-------------------------------------------------------
      SUBROUTINE CALEND( ICHI,IWV )
        IMPLICIT NONE
        INTEGER :: IWV(3),ICHI
        INTEGER :: IM1,IM2,IM3,ID1,ID2,ID3,URU
C-----------------------------------------------------------------------
        IM1=MOD(IWV(1),4)
        ID1=(IWV(1)-IM1)/4
        IM2=MOD(IWV(1),400)
        ID2=(IWV(1)-IM2)/400
        IM3=MOD(IWV(1),100)
        ID3=(IWV(1)-IM3)/100
        URU=ID1+ID2-ID3
        IF( IM1.EQ.0.AND.IM3.NE.0 ) THEN
          IF( IWV(2).LE.2 ) THEN
            URU=URU-1
          END IF
        END IF
        IF( IM2.EQ.0 ) THEN
          IF( IWV(2).LE.2 ) THEN
            URU=URU-1
          END IF
        END IF
        ICHI=IWV(1)*365+IWV(3)+URU
        IF( IWV(2).EQ.2 ) THEN
          ICHI=ICHI+31
        ELSE IF ( IWV(2).EQ.3 ) THEN
          ICHI=ICHI+59
        ELSE IF ( IWV(2).EQ.4 ) THEN
          ICHI=ICHI+90
        ELSE IF ( IWV(2).EQ.5 ) THEN
          ICHI=ICHI+120
        ELSE IF ( IWV(2).EQ.6 ) THEN
          ICHI=ICHI+151
        ELSE IF ( IWV(2).EQ.7 ) THEN
          ICHI=ICHI+181
        ELSE IF ( IWV(2).EQ.8 ) THEN
          ICHI=ICHI+212
        ELSE IF ( IWV(2).EQ.9 ) THEN
          ICHI=ICHI+243
        ELSE IF ( IWV(2).EQ.10 ) THEN
          ICHI=ICHI+273
        ELSE IF ( IWV(2).EQ.11 ) THEN
          ICHI=ICHI+304
        ELSE IF ( IWV(2).EQ.12 ) THEN
          ICHI=ICHI+334
        END IF
        RETURN
      END SUBROUTINE CALEND        
