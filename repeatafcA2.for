c年齢回帰用
      implicit none
      integer,parameter :: hashn=10000000,maxnd=600000
      integer :: nf,nc,mc(20),ic(20),jc(20),nk,lev(30)
      integer :: iken,nn(50),iwv(20),iw1,ip,neqn,count,i,j,k,iw2
      integer :: nr,nxf,nxr,ixf(20),ixr(20),kk,jj,nt,iyes
c      integer :: nnn(hashn)
      real(8) :: 
     + rw1,rwv(4),rmin(50,4),rmax(50,4),ave(50,4),
     + ave2(50,4),sd(50,4)
      integer,allocatable :: id(:,:,:),nnn(:),nff(:,:)
      real(8),allocatable :: rd(:,:,:)
      character :: aap*2
      allocate( id(hashn,10,20),rd(hashn,5,20),nnn(hashn),nff(hashn,2) )
      
      write(*,*) 'どこの県のデータにしますか？ 県番号を入力 全てはAL'
      read(*,'(a2)') aap
      write(*,*) '年齢の効果は実数(回帰）ですか？ yes=>1 NO=>2'
      read(*,*) iyes
      if( iyes.eq.1 ) then
        open(1,file='repeatT_afc3q_'//aap//'.dat',status='old')
        open(5,file='repeatT_afc4q_'//aap//'.dat')
      else
        open(1,file='repeatT_afc3_'//aap//'.dat',status='old')
        open(5,file='repeatT_afc4_'//aap//'.dat')
      end if
      open(2,file='exped2.dat',status='old')
      open(3,file='out_idinb.dat',status='old')
      open(31,file='out_cinb.dat',status='old')
c      open(4,file='LEVELAVE_afc.txt',status='old')
c      open(41,file='LEVELAVE_civ.txt',status='old')
      open(8,file='kenstat.dat')
      write(*,*) '農家の効果は何列目？'
      read(*,*) nf
      write(*,*) '農家の効果以外に変換したいコードはいくつありますか？'
      read(*,*) nc
      do i=1,nc
        write(*,*) i,'つ目に変換するコードは何列目のものですか？'
        read(*,*)  mc(i)
        write(*,*) 'どのコードを？(何を？)'
        read(*,*)  ic(i)
        write(*,*) 'どのコードへ(何へ？)'
        read(*,*)  jc(i)
      end do
      write(*,*) '水準効果のうち採用しない水準効果はいくつ？'
      read(*,*) nxf
      do i=1,nxf
        write(*,*) i,'つめに採用しない効果は何列目？'
        read(*,*) ixf(i)
      end do
      write(*,*) '実数の入った列はいくつありますか？'
      read(*,*) NR
      write(*,*) 'うち採用しないのはいくつ？'
      read(*,*) nxR
      do i=1,nxR
        write(*,*) i,'つめに採用しないの実数列のうち何列目？'
        read(*,*) ixR(i)
      end do
      
      id=0
      rd=0.0d0
      nn=0
      nnn=0
      DO WHILE( .not.eof(1) )
        READ(1,*,end=100) (iwv(J),J=1,NF),(rwv(J),J=1,nr)
        iken=( iwv(nf)-mod(iwv(nf),1000000) )/1000000
        if( iken.eq.0.or.99 ) iken=48
        nn(iken)=nn(iken)+1
         DO I=1,NC
           IF(iwv(MC(I)).EQ.IC(I) ) iwv(mc(i))=JC(I)
         END DO
c         do j=1,nk
c           if( lev(j).eq.iwv(nf) ) then
c             iwv(nf)=48999999
c             exit
c           end if
c         end do
c         if( iwv(nf).ne.48999999 ) then
c           call hash( iwv(nf),ip,neqn,count )
c           if( nff(ip,1).lt.10.or.nff(ip,2).lt.10 ) then
c             iwv(nf)=iken*1000000+900000
c           end if
c         end if
         if( nn(iken).eq.1) then
           DO J=1,2
             rmin(iken,J)=rwv(J)
             rmax(iken,J)=rwv(J)
           END DO
         end if
         DO J=1,2
           if( rwv(J).lt.rmin(iken,J)) rmin(iken,J)=rwv(J)
           if( rwv(J).gt.rmax(iken,J) ) rmax(iken,J)=rwv(J)
           ave(iken,J)=ave(iken,J)+rwv(J)
           ave2(iken,J)=ave2(iken,J)+rwv(J)**2
         END DO
         call hash( iwv(1),ip,neqn,count )
         nnn(ip)=nnn(ip)+1
         kk=0
         do j=1,NF-3
           do k=1,nxf
             if( j+3.eq.ixf(k) ) go to 11
           end do
           kk=kk+1
           id(ip,kk,nnn(ip))=iwv(3+j)
11         continue
         end do
         jj=0
         do j=1,NR
           do k=1,nxr
             if(j.eq.ixr(k) ) go to 22
           end do
           jj=jj+1
           rd(ip,2+jj,nnn(ip))=rwv(j)
22         continue
         end do
      END DO
100   continue
      do i=1,49
        if( nn(i).le.1 ) cycle
        DO J=1,4
        sd(i,J)=
     +   dsqrt( ( ave2(i,J)-ave(i,J)**2/dble(nn(i)) ) /dble(nn(i)-1) )
        ave(i,J)=ave(i,J)/dble(nn(i))
        END DO
      end do
      do i=1,49
        write(8,'(i3,4(2f10.3,2F10.3),i7)') 
     +   i,((ave(i,J),sd(i,J),rmin(i,J),rmax(i,J)),J=1,4),nn(i)
      end do
      nnn=0
      nt=0
      do while( .not.eof(31) )
        read(31,'(11x,2i11,f10.5)') iw1,iw2,rw1
        nt=nt+1
        call hash( iw2,ip,neqn,count )
        nnn(ip)=nnn(ip)+1
        rd(ip,2,nnn(ip))=rw1
      end do
      write(*,*) nt
      do while( .not.eof(3) )
        read(3,'(i11,33x,f10.5)'),iw1,rw1
        if( iw1.le.100000000 ) cycle
        call hash( iw1,ip,neqn,count )
        do i=1,nnn(ip)
          rd(ip,1,i)=rw1
        end do
      end do
      iw1=0
      rw1=0.0d0
c      nnn=0
      write(*,*) kk,jj
      do while( .not.eof(2) )
        read(2,*,end=200) (iwv(j),j=1,6)
        if( iwv(6).eq.0 ) then
          write(5,'(4i11,<kk>i9,<1+jj>f10.3)')
     +      (iwv(j),j=2,4),(iw1,j=1,kk+1),(rw1,j=1,1+jj)
        else
          call hash( iwv(2),ip,neqn,count )
c          nnn(ip)=nnn(ip)+1
          if( nnn(ip).eq.1.and.rd(ip,2+jj,1).eq.0 ) then !１つも分娩間隔の記録がない個体
            iwv(5)=-9999
          else
            iwv(5)=iwv(2)
          end if
          do k=1,nnn(ip)
            if( k.eq.1.and.rd(ip,1+jj,1).ne.0.0d0 ) then
c              id(ip,4,1)=-9999
c              if( id(ip,1,k).ne.0 ) then
                write(5,'(4i11,<kk>i9,<jj+1>f10.3)')
     +          (iwv(j),j=2,4),-9999,(id(ip,j,k),j=1,kk),
     +          rd(ip,1,k),(rd(ip,j,k),j=3,2+jj)
c              end if
            else
c              if(id(ip,1,k).ne.0 ) then
                write(5,'(4i11,<kk>i9,<jj+1>f10.3)')
     +          (iwv(j),j=2,4),iwv(5),(id(ip,j,k),j=1,kk),
     +          rd(ip,1,k),(rd(ip,j,k),j=3,2+jj)
c              end if
            end if
          end do
        end if
      end do
200   continue
      stop
      end

c----------------------------------------------------------------------
c
      SUBROUTINE HASH(K,IP,NEQN,COUNT)
c
c-----------------------------------------------------------------------
c      PARAMETER(M=31,M1=17) !origin
       PARAMETER(M=10000000,M1=9999991) !素数

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
