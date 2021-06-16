      implicit none
      integer,parameter  :: hashn=20000000,maxn=8100000
      integer :: ipo(hashn,2),iwv(9),id(maxn,9),jd,nd,i,j,ip,neqn,count
      real(8) :: rwv(4),rd(maxn,4)
      open(1,file='repeatafctq_AL.dat',status='old')
      open(2,file='repeatafctq_testAL.dat',status='old')
      jd=0
      nd=0
      do while( .not.eof(1) )
        nd=nd+1
        read(1,*) (id(nd,j),j=1,9),(rd(nd,j),j=1,4)
        if( jd.ne.id(nd,1) ) then
          if( nd.ne.1 ) then
            ipo(ip,2)=nd-1
          end if
          call hash( id(nd,1),ip,neqn,count )
          ipo(ip,1)=nd
        end if
        call hash( id(nd,1),ip,neqn,count )
        jd=id(nd,1)
      end do
      do while( .not.eof(2) )
        read(2,*) (iwv(j),j=1,9),(rwv(j),j=1,4)
        call hash( iwv(1),ip,neqn,count )
        do i=ipo(ip,1),ipo(ip,2)
          if( rwv(2).eq.rd(i,2) ) go to 100
        end do
        write(3,'(4i11,5i9,4f10.3)') (iwv(j),j=1,9),(rwv(j),j=1,4)
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
