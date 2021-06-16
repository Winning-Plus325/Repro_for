c     mhanz‚Éö¸‚Ì”_‰Æ”Ô†‚ğ‚­‚Á‚Â‚¯‚é
      implicit none
      integer,parameter :: hashn=20000000
      integer,allocatable :: nn(:),iday(:,:),jym(:)
      character,allocatable :: farm(:,:)*15
      integer :: isex,id,ib,ido,is,iw1,i,ip,neqn,count,id2,ip2,ij,nut
      character :: aa15*15,bb15*15,aa51*51,aa3*3,fm15*15
      
      allocate( nn(hashn),iday(hashn,15),farm(hashn,15),jym(hashn) )
      
      open(1,file='idodt',status='old')
      open(2,file='ketof',status='old')
      open(3,file='chkketid_all',status='old')
      open(4,file='chkket_ifarm.dat')
      open(6,file='kihoncombo',status='old')
      do while( .not.eof(6) )
        read(6,'(i10,147x,i1,x,i7)') id,nut,ij
        call hash( id,ip,neqn,count )
        jym(ip)=ij
      end do
      nn=0
      iday=0
      do while( .not.eof(2) )
        read(2,'(i1,i9,36x,i7,25x,a15,a15)') isex,id,ib,aa15,bb15 !”ÉBÒ
        if( isex.eq.1 ) cycle
        if( bb15.ne.'000000000000000') aa15=bb15
        call hash( id,ip,neqn,count )
        nn(ip)=1
        farm(ip,nn(ip))=aa15
        iday(ip,nn(ip))=ib
      end do
      do while( .not.eof(1) )
        read(1,'(1x,i9,i2,i7,a15,a15)') id,iw1,ido,aa15,bb15
        write(*,*) id
        if( bb15.ne.'000000000000000') aa15=bb15
        call hash( id,ip,neqn,count )
        nn(ip)=nn(ip)+1
        farm(ip,nn(ip))=aa15
        iday(ip,nn(ip))=ido
      end do
      rewind(2)
      do while( .not.eof(2) )
        read(2,'(i1,i9,100x,i7,a15,a15)') isex,id,ib,aa15,bb15 !Œ»Š—LÒ
        if( isex.eq.1 ) cycle
        if( bb15.ne.'000000000000000') aa15=bb15
        call hash( id,ip,neqn,count )
        if( nn(ip).eq.0 ) cycle
        if( ib.gt.iday(ip,nn(ip)) ) then
          nn(ip)=nn(ip)+1
          farm(ip,nn(ip))=aa15
          iday(ip,nn(ip))=ib
        end if
      end do
      rewind(2)
      do while( .not.eof(2) )
        read(2,'(i1,i9,36x,i7,25x,a15,a15)') isex,id,ib,aa15,bb15
        if( isex.eq.1 ) cycle
        if( bb15.ne.'000000000000000') aa15=bb15
        if( aa15.eq.'000000000000000') then
          backspace(2)
          read(2,'(64x,i9)') id2
          call hash( id2,ip2,neqn,count ) !•ê‹
          call hash( id,ip,neqn,count )
          do i=1,nn(ip2)-1
            if( ib.ge.iday(ip2,i).and.ib.lt.iday(ip2,i+1) ) then !•ê‹‚©‚ç”ÉBÒ‚ğŠ„‚èo‚·
              farm(ip,1)=farm(ip2,i)
              iday(ip,1)=ib
              go to 10
            end if
          end do
          if( nn(ip2).eq.0 ) cycle
          if( ib.ge.iday(ip2,nn(ip2)) ) then
            iday(ip,1)=ib
            farm(ip,1)=farm(ip2,nn(ip2))
          end if
        end if
10      continue
      end do
      do while( .not.eof(3) )
        read(3,'(i1,i9,a3,x,i7,x,i7)') isex,id,aa3,is,ib
        call hash( id,ip,neqn,count )
        fm15='000000000000000'
        if( nn(ip).eq.0 ) then
          write(*,*) id
          go to 100
        end if
        if( iday(ip,nn(ip)).eq.0 ) go to 100
        do i=1,nn(ip)-1
          if( is.ge.iday(ip,i).and.is.lt.iday(ip,i+1) ) then
            fm15=farm(ip,i)
            if( iday(ip,i+1).eq.jym(ip) ) then !ˆÙ“®”NŒ“ú‚ª“o˜^R¸”NŒ“ú‚Æ“¯‚¶ê‡C‚à‚Á‚Æ‘O‚ÉˆÚ“®‚ª‚ ‚Á‚½‚Æ‚İ‚È‚µ‚ÄC‰Y‚Ìí•t‚¯‚Ì”_‰Æ‚ÍR¸“_‚Ì”_‰Æ‚Æ‚·‚é
              fm15=farm(ip,i+1)
            end if
            if( iday(ip,i+1).eq.jym(ip)-1 ) then !ˆÙ“®”NŒ“ú‚ª“o˜^R¸”NŒ“ú‚Ìˆê“ú‘O‚Ìê‡C‚à‚Á‚Æ‘O‚ÉˆÚ“®‚ª‚ ‚Á‚½‚Æ‚İ‚È‚µ‚ÄC‰Y‚Ìí•t‚¯‚Ì”_‰Æ‚ÍR¸“_‚Ì”_‰Æ‚Æ‚·‚é
              fm15=farm(ip,i+1)
            end if
            if( i.eq.1.and.ib.le.jym(ip) ) then
              fm15=farm(ip,i+1)
            end if
            go to 100
          end if
        end do
        if( is.ge.iday(ip,nn(ip)) ) fm15=farm(ip,nn(ip))
100     continue
        write(4,'(i1,i9,a3,x,i7,x,a15)') isex,id,aa3,is,fm15
      end do
      stop
      end
c----------------------------------------------------------------------
c
      SUBROUTINE HASH(K,IP,NEQN,COUNT)
c
c-----------------------------------------------------------------------
c      PARAMETER(M=31,M1=17) !origin
       PARAMETER(M=20000000,M1=9999991) !‘f”

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
