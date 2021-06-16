ckihonÇÃãåÇ∆êVÇÇ≠Ç¡Ç¬ÇØÇÈ
      implicit none
      integer,parameter :: hashn=10000000
      integer :: idd2(hashn,11),iwv(4),judg(11),ib(11),iwv2(5),ih
      integer :: iflg(hashn),id,iy,isex,id2,isib,jy,jplace
      integer :: jmen(3),ip,neqn,count,i,j,iw1
      character :: anut*2,ason*40

      open(1,file='kihonold',status='old')
      open(2,file='kihonnew',status='old')
      open(3,file='kihoncombo')
      iflg=0
      idd2=0
      do while( .not.eof(2) )
        read(2,
     +'(x,i9,i7,x,i9,i2,5x,10i2,i3,4i4,6i3,i4,a1,2x,i7,i5,3i6,a40,i1)')
     +   (iwv(j),j=1,4),(judg(j),j=1,11),(ib(j),j=1,11),anut(2:2),
     +   (iwv2(j),j=1,5),ason,ih
        call hash( iwv(1),ip,neqn,count )
        iflg(ip)=1
        do j=1,11
          idd2(ip,j)=judg(j)
        end do
      end do
     
      do while( .not.eof(1) )
        read(1,
     +'(i1,i9,i7,i1,i9,i2,5x,9i2,i3,4i4,6i3,i4,a2,2x,i7,i5,3i6,a20,i1)')
     + isex,id,iy,isex,id2,isib,(judg(j),j=1,10),(ib(j),j=1,11),anut,jy,
     + jplace,(jmen(j),j=1,3),ason,ih
       if( jy.lt.4021001) cycle
       if( jy.ge.4021001.and.anut(1:1).ne.' '.and.anut(2:2).eq.' ')then
         anut(2:2)=anut(1:1)
         anut(1:1)=' '
       end if
       if( anut(1:1).ne.' '.and.anut(1:1).ne.'0' ) then
         write(*,*) id
         cycle
       end if
       call hash( id,ip,neqn,count )
       write(3,
     +'(i10,i8,i10,i3,9i3,f5.1,10i3,f5.1,4f6.1,
     +6f5.1,i4,x,a1,i8,i6,3i7,x,a40,i2)')
     +id,iy,id2,isib,(judg(j),j=1,9),dble(judg(10))/10.0d0,
     + (idd2(ip,j),j=1,10),dble(idd2(ip,11))/10.0d0,
     + (dble(ib(j))/10.0d0,j=1,10),ib(11),anut(2:2),jy,
     + jplace,(jmen(j),j=1,3),ason,ih
       iflg(ip)=2
      end do 
      rewind(2)
      iw1=0
      do while( .not.eof(2) )
        read(2,
     +'(x,i9,i7,x,i9,i2,5x,10i2,i3,4i4,6i3,i4,a1,2x,i7,i5,3i6,a40,i1)')
     +   (iwv(j),j=1,4),(judg(j),j=1,11),(ib(j),j=1,11),anut(2:2),
     +   (iwv2(j),j=1,5),ason,ih
        call hash( iwv(1),ip,neqn,count )
        if(iflg(ip).ne.2 ) then
          write(3,
     +      '(i10,i8,i10,i3,9i3,f5.1,10i3,f5.1,4f6.1,
     +        6f5.1,i4,x,a1,i8,i6,3i7,x,a40,i2)')
     +         (iwv(j),j=1,4),(iw1,j=1,9),0.0d0,(judg(j),j=1,10),
     +         dble(judg(11))/10.0d0,(dble(ib(j))/10.0d0,j=1,10),ib(11)
     +         ,anut(2:2),(iwv2(j),j=1,5),ason,ih
        end if
      end do
      stop
      end
c----------------------------------------------------------------------
c
      SUBROUTINE HASH(K,IP,NEQN,COUNT)
c
c-----------------------------------------------------------------------
c      PARAMETER(M=31,M1=17) !origin
       PARAMETER(M=10000000,M1=9999991) !ëfêî

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
      SUBROUTINE HASH(K,IP,NEQN,COUNT)
c
c-----------------------------------------------------------------------
c      PARAMETER(M=31,M1=17) !origin
       PARAMETER(M=10000000,M1=137) !ëfêî

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
