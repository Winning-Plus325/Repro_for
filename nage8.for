      implicit none
      integer,parameter :: hashn=20000000
      integer :: isex,id,isan,i,ip,neqn,count,iwv(6),j
      integer,allocatable :: idnum(:,:),nc(:)
      character,allocatable :: farm(:,:)*15,bfarm(:)*15
      character :: fm15*15,lg*97,aa15*15,bb15*15
      allocate( idnum(1:hashn,1:25),farm(1:hashn,1:25),nc(1:hashn),
     +          bfarm(hashn)  )
c      open(1,file='mhanext.dat',status='old')
      open(1,file='chkket_ifarm.dat',status='old')
      open(11,file='ketof',status='old')
c      open(2,file='intervalETcut730.dat',status='old')
      open(2,file='730cutREC.dat',status='old')
      
c      open(3,file='ETcut730_a.dat')
      open(3,file='ETcut730_t.dat')
      open(4,file='noMHAN.txt')
      open(5,file='noFARM.txt')
      nc=0
      do while( .not.eof(1) )
        read(1,'(i1,i9,i3,9x,a15)') isex,id,isan,fm15
        call hash( id,ip,neqn,count )
        nc(ip)=nc(ip)+1
        idnum(ip,nc(ip))=isan
        farm(ip,nc(ip))=fm15
      end do
      do while( .not.eof(11) )
        read(11,'(i1,i9,68x,a15,a15)') isex,id,aa15,bb15
        if( isex.eq.1 ) cycle
        call hash( id,ip,neqn,count )
        if( bb15(1:2).ne.'00')  aa15=bb15
        bfarm(ip)=aa15 !î…êBé“ä«óùé“
      end do
      do while( .not.eof(2) )
        read(2,'(i1,i9,i3,a97,2(i5,2i3))') isex,id,isan,lg,
     +   (iwv(j),j=1,6)
        call hash( id,ip,neqn,count )
        fm15='999999999999999'
        do i=1,nc(ip)
          if( idnum(ip,i).eq.isan ) then
            fm15=farm(ip,i)
          end if
        end do
        if( lg(2:2).eq.'0'.and.fm15.eq.'999999999999999' ) then
          write(4,'(i10,i3,a97,x,a15)') id,isan,lg,fm15
          cycle
        end if
        if( fm15.eq.'000000000000000' ) then
          fm15=bfarm(ip)
          if( fm15.eq.'000000000000000') then
            write(5,'(i10,i3,a97,x,a15)') id,isan,lg,fm15
            cycle
          end if
        end if
        write(3,'(i10,i3,a97,x,a15,2(i5,2i3))') id,isan,lg,fm15,
     +  (iwv(j),j=1,6)   
      end do
      stop
      end
c----------------------------------------------------------------------
c
      SUBROUTINE HASH(K,IP,NEQN,COUNT)
c
c-----------------------------------------------------------------------
c      PARAMETER(M=31,M1=17) !origin
       PARAMETER(M=20000000,M1=9999991) !ëfêî

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
