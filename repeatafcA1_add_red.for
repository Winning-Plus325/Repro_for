      implicit none
      integer,parameter :: hashn=200000,hashn2=5000000
      integer :: iwv(10),NF,NR,i,j,ip,neqn,count,nc,iwv2,iwv3,ip3,ip4
      integer :: iw1,ipo(hashn),id,ifarm,num,iken,ja,ii,iw2,ip2,k,ip5
      integer,allocatable:: na(:,:,:),lev(:,:),ipf(:,:,:),naa(:,:),
     +                      nnf(:,:),nnf2(:,:),newf(:,:),newf2(:),
     +                      iflag(:),honf(:),newf3(:),newf4(:),
     +                      nnf3(:,:),nnf4(:,:),nnf5(:,:),iflag2(:)
      real(8) :: rwv(2),rw1
      character :: dfile*50,cv*1,aa14*14,aa40*40,aa85*85,aa42*42,AAp*2
      allocate( na(8,hashn,2),lev(8,hashn),ipf(hashn2,8,2),
     + naa(hashn2,2),nnf(hashn,2),nnf2(hashn,2),newf(hashn,4) ,
     + newf2(hashn),iflag(hashn),nnf3(hashn,2),nnf4(hashn,2),
     +newf3(hashn),newf4(hashn),honf(hashn),nnf5(hashn,2),iflag2(hashn))





c      write(*,*) 'データファイル名は？'
c      read(*,*) dfile
c      open(1,file=dfile,status='old')
c      open(1,file='RepeatT_afc2_AL.dat',status='old')
      write(*,*) 'どこの県のデータにしますか？ 県番号を入力 全てはAL'
      read(*,'(a2)') aap
      open(1,file='repeatT_afc2_'//aap//'.dat',status='old')
      open(11,file='repeatT_afc3_'//aap//'.dat')
      open(12,file='repeatT_afc3q_'//aap//'.dat')
      
      open(2,file='count_farm.txt')
      open(3,file='fmc2add.dat',status='old')
      open(4,file='fmc2add_re.dat')
c      open(5,file='kaikumidt.txt',status='old')
      write(*,*) '農家の効果は何列目？'
      read(*,*) nc
      nnf=0
      naa=0
      do while( .not.eof(1) )
        read(1,*) id,(iw1,j=1,nc-2),ifarm,rw1,(rwv(j),j=1,2)
        call hash2( id,ip2,neqn,count )
        do i=1,2
          if( rwv(i).gt.0 ) then
            do j=1,naa(ip2,i)
              if( ipf(ip2,j,i).eq.ifarm ) then
                go to 10
              end if
            end do
            naa(ip2,i)=naa(ip2,i)+1
            ipf(ip2,naa(ip2,i),i)=ifarm
            call hash( ifarm,ip,count,neqn )
            nnf(ip,i)=nnf(ip,i)+1
10          continue
          end if
        end do
      end do
      do i=1,hashn
        do j=1,2
           nnf2(i,j)=nnf(i,j)
        end do
      end do
      iflag=0
      newf2=0
      newf3=0
      newf4=0
      do while( .not.eof(3) )
        read(3,'(a14,i6,i7,x,a40,i9)') aa14,num,iw1,aa40,ifarm
        backspace(3)
        read(3,'(i4,i3,62x,i2,i1)') ii,ja,iken,iw2
        JA=mod(ii,100)*1000+JA
        if( iw2.eq.1 ) then !単独のとき
          call hash( ifarm,ip,neqn,count )
          if(iflag(ip).eq.1 ) cycle
          newf(ip,1)=ifarm
          if( aa40(1:2).eq.'　') then
            newf(ip,2)=iken*1000000+800000+JA
          else
            newf(ip,2)=iken*1000000+700000+mod(iw1,10000)
          end if
c          newf(ip,3)=iken*1000000+900000
c          newf(ip,4)=48*1000000+900000
          if( nnf(ip,1).le.4.or.nnf(ip,2).le.4 ) then
            newf2(ip)=0
            call hash( newf(ip,2),ip2,neqn,count )
            newf2(ip)=newf(ip,2) !単位Ｇ
            if( newf(ip,2).eq.ifarm ) cycle
            do k=1,2
              nnf2(ip2,k)=nnf2(ip2,k)+nnf(ip,k)
            end do
          else
            newf2(ip)=newf(ip,1) !そのまま
          end if
          iflag(ip)=1
        end if
      end do
      call hash( 39100201,ip,neqn,count )
      call hash( newf2(ip),ip2,neqn,count )
      write(*,*) nnf2(ip2,2)
      rewind(3)
      do i=1,hashn
        do j=1,2
           nnf3(i,j)=nnf2(i,j)
        end do
      end do
      
      do while( .not.eof(3) )
        read(3,'(a14,i6,i7,x,a40,i9)') aa14,num,iw1,aa40,ifarm
        backspace(3)
        read(3,'(i4,i3,62x,i2,i1)') ii,ja,iken,iw2
        JA=mod(ii,100)*1000+JA
        if( iw2.eq.7.or.iw2.eq.8 ) then !JAor改良組合
          call hash( ifarm,ip,neqn,count )
          if(iflag(ip).eq.1 ) cycle
          newf(ip,1)=ifarm
c          if( aa40(1:2).eq.'　') then
c            newf(ip,2)=iken*1000000+800000+JA
c          else
c            newf(ip,2)=iken*1000000+700000+mod(iw1,10000)
c          end if
          newf(ip,3)=iken*1000000+900000
c          newf(ip,4)=48*1000000+900000
          if( nnf2(ip,1).le.4.or.nnf2(ip,2).le.4 ) then
            call hash( newf(ip,3),ip2,neqn,count )
            newf3(ip)=newf(ip,3) !県単位Ｇ
            if( newf(ip,3).eq.ifarm ) cycle
            do k=1,2
              nnf3(ip2,k)=nnf3(ip2,k)+nnf2(ip,k)
            end do
          else
            newf3(ip)=newf(ip,1) !単位Ｇまま
          end if
          iflag(ip)=1
        end if
      end do
      rewind(3)
      do i=1,hashn
        do j=1,2
           nnf4(i,j)=nnf3(i,j)
        end do
      end do
      
      do while( .not.eof(3) )
        read(3,'(a14,i6,i7,x,a40,i9)') aa14,num,iw1,aa40,ifarm
        backspace(3)
        read(3,'(i4,i3,62x,i2,i1)') ii,ja,iken,iw2
        JA=mod(ii,100)*1000+JA
        if( iw2.eq.9 ) then !県単位Ｇ
          call hash( ifarm,ip,neqn,count )
          if(iflag(ip).eq.1 ) cycle
          newf(ip,1)=ifarm
          newf(ip,4)=48*1000000+900000
          if( nnf3(ip,1).le.4.or.nnf3(ip,2).le.4 ) then
            newf2(ip)=0
            call hash( newf(ip,4),ip2,neqn,count )
            newf4(ip)=newf(ip,4) !県単位Ｇ
            if( newf(ip,4).eq.ifarm ) cycle
            do k=1,2
              nnf4(ip2,k)=nnf4(ip2,k)+nnf3(ip,k)
            end do
          else
            newf4(ip)=newf(ip,1) !県単位Ｇまま
          end if
          iflag(ip)=1
        endif
      end do
      do i=1,hashn
        do j=1,2
           nnf5(i,j)=nnf4(i,j)
        end do
      end do
      rewind(3)
      iflag2=0
      do while( .not.eof(3) )
        read(3,'(a14,i6,i7,x,a40,i9)') aa14,num,iw1,aa40,ifarm
        backspace(3)
        read(3,'(i4,i3,62x,i2,i1)') ii,ja,iken,iw2
        JA=mod(ii,100)*1000+JA
        if( iw2.eq.1 ) then
          call hash( ifarm,ip,neqn,count )
          call hash( newf2(ip),ip2,neqn,count ) !単位Ｇorそのまま
          if( newf3(ip2).ne.0 ) then
            call hash( newf3(ip2),ip3,neqn,count ) !県単位
            if( newf4(ip3).ne.0 ) then !全国Ｇ
              call hash( newf4(ip3),ip4,neqn,count )
              write(4,'(a14,i6,i7,x,a40,2i9,2i6)') 
     +        aa14,num,iw1,aa40,(newf4(ip3),j=1,2),(nnf4(ip4,j),j=1,2)
              honf(ip)=newf4(ip3)
            else !県単位
              if( nnf4(ip3,1).le.4.or.nnf4(ip3,2).le.4 ) then
                if( iflag2(ip).eq.1 ) cycle
                iflag2(ip)=1
                iw1=48*1000000+900000
                call hash( iw1,ip5,neqn,count )
                do j=1,2
                  nnf5(ip5,j)=nnf5(ip5,j)+nnf4(ip3,j)
                end do
                honf(ip)=iw1
                cycle
              end if
              write(4,'(a14,i6,i7,x,a40,2i9,2i6)') 
     +        aa14,num,iw1,aa40,(newf3(ip2),j=1,2),(nnf4(ip3,j),j=1,2)
              honf(ip)=newf3(ip2)
            end if
          else !単位Ｇorそのまま
            if( nnf4(ip2,1).le.4.or.nnf4(ip2,2).le.4 ) then
              if( iflag2(ip).eq.1 ) cycle
              iflag2(ip)=1
              iw1=iken*1000000+900000
              call hash( iw1,ip5,neqn,count )
              if( iw1.ne.newf2(ip) ) then
                do j=1,2
                  nnf5(ip5,j)=nnf5(ip5,j)+nnf4(ip2,j)
                end do
              end if
              honf(ip)=iw1
              cycle
            end if
            write(4,'(a14,i6,i7,x,a40,2i9,2i6)') 
     +        aa14,num,iw1,aa40,(newf2(ip),j=1,2),(nnf4(ip2,j),j=1,2)
              honf(ip)=newf2(ip)
          end if
        end if
        if( iw2.eq.7.or.iw2.eq.8 ) then
          call hash( ifarm,ip,neqn,count )
          call hash( newf3(ip),ip2,neqn,count )
          if( newf4(ip2).ne.0 ) then
              call hash( newf4(ip2),ip3,neqn,count )
              if( nnf4(ip3,1).le.4.or.nnf4(ip3,2).le.4 ) then
                if( iflag2(ip).eq.1 ) cycle
                iflag2(ip)=1
                iw1=48*1000000+900000
                call hash( iw1,ip5,neqn,count )
                if( iw1.ne.newf4(ip2)) then
                do j=1,2
                  nnf5(ip5,j)=nnf5(ip5,j)+nnf4(ip3,j)
                end do
                end if
                honf(ip)=iw1
                cycle
              end if
              write(4,'(a14,i6,i7,x,a40,2i9,2i6)') 
     +        aa14,num,iw1,aa40,(newf4(ip2),j=1,2),(nnf4(ip3,j),j=1,2)
              honf(ip)=newf4(ip2)
          else
              if( nnf4(ip2,1).le.4.or.nnf4(ip2,2).le.4 ) then
                if( iflag2(ip).eq.1 ) cycle
                iflag2(ip)=1
                iw1=iken*1000000+900000
                call hash( iw1,ip5,neqn,count )
                if( iw1.ne.newf3(ip) ) then
                  do j=1,2
                    nnf5(ip5,j)=nnf5(ip5,j)+nnf4(ip2,j)
                  end do
                end if
                honf(ip)=iw1
                cycle
              end if
              write(4,'(a14,i6,i7,x,a40,2i9,2i6)') 
     +        aa14,num,iw1,aa40,(newf3(ip),j=1,2),(nnf4(ip2,j),j=1,2)
              honf(ip)=newf3(ip)
          end if
        end if
        if( iw2.eq.9 ) then
          call hash( ifarm,ip,neqn,count )
          call hash( newf4(ip),ip2,neqn,count )
          if( nnf4(ip2,1).le.4.or.nnf4(ip2,2).le.4 ) then
            if( iflag2(ip).eq.1 ) cycle
            iflag2(ip)=1
            iw1=48*1000000+900000
            call hash( iw1,ip5,neqn,count )
            if( iw1.ne.newf4(ip) ) then
            do j=1,2
              nnf5(ip5,j)=nnf5(ip5,j)+nnf4(ip2,j)
            end do
            end if
            honf(ip)=iw1
            cycle
          end if
          write(4,'(a14,i6,i7,x,a40,2i9,2i6)') 
     +    aa14,num,iw1,aa40,(newf4(ip),j=1,2),(nnf4(ip2,j),j=1,2)
          honf(ip)=newf4(ip)
        end if
      end do
      rewind(3)
      do while( .not.eof(3) )
        read(3,'(a14,i6,i7,x,a40,i9)') aa14,num,iw1,aa40,ifarm
        backspace(3)
        read(3,'(i4,i3,62x,i2,i1)') ii,ja,iken,iw2
        JA=mod(ii,100)*1000+JA
        call hash( ifarm,ip,neqn,count )
        if( iflag2(ip).ne.1 ) cycle
        call hash( honf(ip),ip2,neqn,count )
        if( nnf5(ip2,1).le.4.or.nnf5(ip2,2).le.4 ) then
          iw2=48*1000000+900000
          honf(ip)=iw2
          call hash( iw2,ip3,neqn,count )
          write(4,'(a14,i6,i7,x,a40,2i9,2i6)') 
     +  aa14,num,iw1,aa40,(honf(ip),j=1,2),
     +  ((nnf5(ip2,j)+nnf5(ip3,j)),j=1,2)
        else
          write(4,'(a14,i6,i7,x,a40,2i9,2i6)') 
     +    aa14,num,iw1,aa40,(honf(ip),j=1,2),
     +  (nnf5(ip2,j),j=1,2)
        end if
      end do
      rewind(1)
      do while( .not.eof(1) )
        read(1,*) id,(iwv(j),j=1,nc-2),ifarm,rw1,(rwv(j),j=1,2)
        call hash( ifarm,ip,neqn,count )
        if( rwv(1).ne.0.0d0 ) then
          iwv2=iwv(3)
          iwv3=0
        else
          iwv2=0
          iwv3=iwv(3)
        end if
        write(11,'(3i11,2i5,i3,4i2,2i3,i9,2f10.3)') 
     +  id,(iwv(j),j=1,2),iwv2,iwv3,
     +  (iwv(j),j=4,nc-2),int(rw1),honf(ip),(rwv(j),j=1,2)
         write(12,'(3i11,2i5,i3,4i2,i3,i9,3f10.3)') 
     +  id,(iwv(j),j=1,2),iwv2,iwv3,
     +  (iwv(j),j=4,nc-2),honf(ip),rw1,(rwv(j),j=1,2)
      end do
      stop
      end
c----------------------------------------------------------------------
c
      SUBROUTINE HASH(K,IP,NEQN,COUNT)
c
c-----------------------------------------------------------------------
c      PARAMETER(M=31,M1=17) !origin
       PARAMETER(M=200000,M1=195277) !素数

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
c----------------------------------------------------------------------
c
      SUBROUTINE HASH2(K,IP,NEQN,COUNT)
c
c-----------------------------------------------------------------------
c      PARAMETER(M=31,M1=17) !origin
       PARAMETER(M=5000000,M1=999991) !素数

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
