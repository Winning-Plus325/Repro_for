      implicit none
      integer,parameter :: hashn=20000000
      integer :: id,iw1,iw2,i,ip,neqn,count,nn(hashn),j
      real(8) :: rwv(3),rw1
      real(8),allocatable :: age(:,:),afc(:,:),civ(:,:)
      character :: cv*1,dfile*50
      cv=','
      allocate (age(hashn,20),afc(hashn,20),civ(hashn,20))
      write(*,*) 'çÏê¨ÇµÇΩÇfÇhÇaÇaÇrï™êÕÉtÉ@ÉCÉãñºÇÕÅH'
      read(*,*) dfile
      open(1,file=dfile,status='old')
      open(2,file='repeatT_afc2_AL.dat',status='old')
      open(3,file='AgeParity.csv')
      write(3,'(5a)') 'ID',',Age',',afc',',civ',',parity'
      do while( .not.eof(1) )
        read(1,*) id,(iw1,j=1,9),(rwv(j),j=1,3)
        call hash( id,ip,neqn,count )
        nn(ip)=nn(ip)+1
        AGE(ip,nn(ip))=rwv(1)
        AFC(ip,nn(ip))=rwv(2)
        CIV(ip,nn(ip))=rwv(3)
        if( rwv(3).eq.-9999 ) then
          write(3,'(i10,3(a,f10.3),a,i3)') 
     +    id,cv,rwv(1),cv,rwv(2),cv,rwv(3),cv,1
        end if
      end do
      do while(.not.eof(2) )
        read(2,*) id,(iw1,j=1,8),iw2,iw1,rw1
        call hash( id,ip,neqn,count )
        do i=1,nn(ip)
          if( age(ip,i).eq.rw1 ) then
            write(3,'(i10,3(a,f10.3),a,i3)') 
     +      id,cv,age(ip,i),cv,afc(ip,i),cv,civ(ip,i),cv,iw2
            exit
          end if
        end do
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
