      implicit none
      integer,parameter :: hashn=20000000
      integer :: ib(hashn),isex,id,iw1,ip,neqn,count,i,j
      character :: aa1*1,aa2*2,bb2*2,awv(7)*1,aa58*58
      
      open(1,file='jyuranf',status='old')
      open(2,file='ketof',status='old')
      open(3,file='jyuranf_edit')
      ib=0
      do while( .not.eof(2) )
        read(2,'(i1,i9,36x,i3)') isex,id,iw1
        if( isex.eq.1 ) cycle
        call hash( id,ip,neqn,count )
        ib(ip)=iw1
      end do
      do while( .not.eof(1) )
        read(1,'(i1,i9,a1,a2,a2,7a1,a58)') 
     +   isex,id,aa1,aa2,bb2,(awv(j),j=1,7),aa58
        call hash( id,ip,neqn,count )
        if( ib(ip).le.362 ) cycle
        if( aa1.ne.'0'.and.aa1.ne.'1' ) cycle
        do i=1,7
          if( awv(i).lt.'0'.or.awv(i).gt.'9' ) go to 100
        end do
        write(3,'(i1,i9,7a1,a1,a2,a2,a58)') 
     +   isex,id,(awv(j),j=1,7),aa1,aa2,bb2,aa58
100     continue
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
c-------------------------------------------------------
