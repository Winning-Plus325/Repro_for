      implicit none
      integer,parameter :: hashn=12000000
      integer :: iwv(15),nf,nr,nrec,i,j,jd,k,it(15),jt(15),iq(15),jq(15)
      integer :: nfq,nrq,iy,iy2
      real(8) :: rwv(15)
      integer :: ip,neqn,count
      character :: dfile*40,gfile*20
      write(*,*) 'ＲＥＭＬ分析データファイル名は？'
      read(*,*) dfile
      open(1,file=dfile,status='old')
      write(*,*) '作成するＧＩＢＢＳ分析ファイル名は？'
      read(*,*) gfile
      open(2,file=gfile)
      open(3,file='pedf.dat')
      write(*,*) '水準効果はいくつ？'
      read(*,*) NF
      do i=1,NF
        write(*,*) i,'番目の水準効果は何番目の形質に対して？全ては99'
        read(*,*) it(i)
      end do
      write(*,*) '回帰はいくつ？'
      read(*,*) NR
      do i=1,NR
        write(*,*) i,'番目の回帰効果は何番目の形質に対して？全ては99'
        read(*,*) jt(i)
      end do
      write(*,*) '水準効果の中で-9999にする効果があるか?[yes=1][no=2]'
      read(*,*) iy
      if( iy.eq.1 ) then
        write(*,*)'いくつある？'
        read(*,*) nfq
        do i=1,nfq
          write(*,*) i,'番目の-9999にする効果は水準効果の何番目？'
          read(*,*) iq(i)
        end do
      end if
      write(*,*) '回帰効果の中で-9999にする効果があるか?[yes=1][no=2]'
      read(*,*) iy2
      if( iy2.eq.1 ) then
        write(*,*)'いくつある？'
        read(*,*) nrq
        do i=1,nrq
          write(*,*) i,'番目の-9999にする効果は回帰の何番目？'
          read(*,*) jq(i)
        end do
      end if
      write(*,*) '形質はいくつ？'
      read(*,*) NREC
      jd=0
      do while( .not.eof(1) )
        read(1,*) (iwv(j),j=1,3+nf),(rwv(j),j=1,nr+nrec)
        if( iwv(1).ne.jd ) then
          write(3,'(6i11)') (iwv(j),j=1,3),(iwv(j),j=1,3)
          jd=iwv(1)
        end if
        if( iwv(4).ne.0 ) then
          if( iy.eq.1 ) then
            do i=1,nfq
              if( it(iq(i)).ne.99 ) then
              if( rwv(nr+it(iq(i))).eq.0.0d0.or.
     +            rwv(nr+it(iq(i))).eq.-9999.0d0 ) then
                  iwv(3+iq(i))=-9999
              end if
              end if
            end do
          end if
          if( iy2.eq.1 ) then
            do i=1,nrq
              if( jt(jq(i)).ne.99 ) then
              if( rwv(nr+jt(jq(i))).eq.0.0d0.or.
     +            rwv(nr+jt(jq(i))).eq.-9999.0d0 ) then
                  rwv(jq(i))=-9999.0d0
              end if
              end if
            end do
          end if
          do i=1,nrec
            if( rwv(nr+i).eq.0.0d0 ) then
              rwv(nr+i)=-9999.0d0
              k=i
              if( k.ne.1 ) then
          write(2,
     +  '(3i11,<nf>i10,<nr>f11.3,<k-1>f10.4,i10,<nrec-k>f10.4)') 
     +  (iwv(j),j=1,3+nf),(rwv(j),j=1,nr+k-1),int(rwv(nr+k)),
     +  (rwv(j),j=nr+k+1,nr+nrec)
              else
          write(2,'(3i11,<nf>i10,<nr>f11.3,i10,<nrec-1>f10.4)') 
     +  (iwv(j),j=1,3+nf),(rwv(j),j=1,nr),int(rwv(nr+k)),
     +  (rwv(j),j=nr+2,nr+nrec)
              end if
              go to 100
            end if
          end do
          write(2,'(3i11,<nf>i10,<nr>f11.3,<nrec>f10.4)') 
     +   (iwv(j),j=1,3+nf),(rwv(j),j=1,nr+nrec)
100       continue
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
       PARAMETER(M=12000000,M1=137) !素数

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
