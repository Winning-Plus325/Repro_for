      implicit none
      integer,parameter :: hashn=200000,hashn2=5000000
      integer :: iwv(10),NF,NR,i,j,ip,neqn,count,nc,iwv2,iwv3,ip3,ip4
      integer :: iw1,ipo(hashn),id,ifarm,num,iken,ja,ii,iw2,ip2,k,ip5
      real(8),allocatable::
     +             ave(:,:,:),sd(:,:,:),rmin(:,:,:),rmax(:,:,:),rdata(:)
      real(8) :: rwv(2),rw1
      integer,allocatable:: na(:,:,:),lev(:,:),ipf(:,:,:),naa(:,:),
     +                      nnf(:,:),nnf2(:,:),newf(:,:),newf2(:),
     +                      iflag(:),honf(:),newf3(:),newf4(:),
     +                      nnf3(:,:),nnf4(:,:),nnf5(:,:),iflag2(:)
      character :: dfile*50,cv*1,aa14*14,aa40*40,aa85*85,aa42*42,AAp*2
      allocate( na(8,hashn,2),lev(8,hashn),ipf(hashn2,8,2),
     + naa(hashn2,2),nnf(hashn,2),nnf2(hashn,2),newf(hashn,4) ,
     + newf2(hashn),iflag(hashn),nnf3(hashn,2),nnf4(hashn,2),
     +newf3(hashn),newf4(hashn),honf(hashn),nnf5(hashn,2),iflag2(hashn))

      allocate( ave(8,hashn,2),sd(8,hashn,2),rmin(8,hashn,2),
     + rmax(8,hashn,2),rdata(hashn) )
      write(*,*) 'çÏê¨ÇµÇΩÇfÇhÇaÇaÇrï™êÕÉtÉ@ÉCÉãñºÇÕÅH'
      read(*,*) dfile
      open(1,file=dfile,status='old')
      do i=1,50
        if( dfile(i:i).eq.' ' ) then
          ii=i
          exit
        end if
      end do
      open(11,file=dfile(1:ii-1)//'2')
      open(2,file=dfile(1:ii-1)//'.csv')
      open(3,file='Lasstat.txt')
      open(4,file='fmc2add_re.dat')
      open(5,file='fmc2add_re2.dat')
      write(*,*) 'âië±ìIä¬ã´ÇèúÇ≠êÖèÄå¯â ÇÃêîÇÕ'
      read(*,*) NF
      write(*,*) 'âÒãAå¯â ÇÃêîÇÕ'
      read(*,*) NR
      na=0
      cv=','
      nnf=0
      naa=0
      do while( .not.eof(1) )
        read(1,*) id,(iw1,j=1,3),(iwv(j),j=1,NF),(rw1,j=1,NR),
     +  (rwv(j),j=1,2)
        call hash2( id,ip2,neqn,count )
        do i=1,2
          if( rwv(i).gt.0 ) then
            do j=1,naa(ip2,i)
              if( ipf(ip2,j,i).eq.iwv(nf) ) then
                go to 10
              end if
            end do
            naa(ip2,i)=naa(ip2,i)+1
            ipf(ip2,naa(ip2,i),i)=iwv(nf)
            call hash( iwv(nf),ip,count,neqn )
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
      do while( .not.eof(4) )
        read(4,'(a14,i6,i7,x,a40,i9)') aa14,num,iw1,aa40,ifarm
        backspace(4)
        read(4,'(i4,i3,62x,i2,i1)') ii,ja,iken,iw2
        ja=mod(ii,100)*1000+ja
        if( iw2.eq.1 ) then !íPì∆ÇÃÇ∆Ç´
          call hash( ifarm,ip,neqn,count )
          if(iflag(ip).eq.1 ) cycle
          newf(ip,1)=ifarm
          if( aa40(1:2).eq.'Å@') then
            newf(ip,2)=iken*1000000+800000+JA
          else
            newf(ip,2)=iken*1000000+700000+mod(iw1,10000)
          end if
c          newf(ip,3)=iken*1000000+900000
c          newf(ip,4)=48*1000000+900000
          if( nnf(ip,1).le.4.or.nnf(ip,2).le.4 ) then
            newf2(ip)=0
            call hash( newf(ip,2),ip2,neqn,count )
            newf2(ip)=newf(ip,2) !íPà Çf
            if( newf(ip,2).eq.ifarm ) cycle
            do k=1,2
              nnf2(ip2,k)=nnf2(ip2,k)+nnf(ip,k)
            end do
          else
            newf2(ip)=newf(ip,1) !ÇªÇÃÇ‹Ç‹
          end if
          iflag(ip)=1
        end if
      end do
      rewind(4)
      do i=1,hashn
        do j=1,2
           nnf3(i,j)=nnf2(i,j)
        end do
      end do
      
      do while( .not.eof(4) )
        read(4,'(a14,i6,i7,x,a40,i9)') aa14,num,iw1,aa40,ifarm
        backspace(4)
        read(4,'(i4,i3,62x,i2,i1)') ii,ja,iken,iw2
        ja=mod(ii,100)*1000+ja
        if( iw2.eq.7.or.iw2.eq.8 ) then !JAorâ¸ó«ëgçá
          call hash( ifarm,ip,neqn,count )
          if(iflag(ip).eq.1 ) cycle
          newf(ip,1)=ifarm
c          if( aa40(1:2).eq.'Å@') then
c            newf(ip,2)=iken*1000000+800000+JA
c          else
c            newf(ip,2)=iken*1000000+700000+mod(iw1,10000)
c          end if
          newf(ip,3)=iken*1000000+900000
c          newf(ip,4)=48*1000000+900000
          if( nnf2(ip,1).le.4.or.nnf2(ip,2).le.4 ) then
            call hash( newf(ip,3),ip2,neqn,count )
            newf3(ip)=newf(ip,3) !åßíPà Çf
            if( newf(ip,3).eq.ifarm ) cycle
            do k=1,2
              nnf3(ip2,k)=nnf3(ip2,k)+nnf2(ip,k)
            end do
          else
            newf3(ip)=newf(ip,1) !íPà ÇfÇ‹Ç‹
          end if
          iflag(ip)=1
        end if
      end do
      rewind(4)
      do i=1,hashn
        do j=1,2
           nnf4(i,j)=nnf3(i,j)
        end do
      end do
      
      do while( .not.eof(4) )
        read(4,'(a14,i6,i7,x,a40,i9)') aa14,num,iw1,aa40,ifarm
        backspace(4)
        read(4,'(i4,i3,62x,i2,i1)') ii,ja,iken,iw2
        ja=mod(ii,100)*1000+ja
        if( iw2.eq.9 ) then !åßíPà Çf
          call hash( ifarm,ip,neqn,count )
          if(iflag(ip).eq.1 ) cycle
          newf(ip,1)=ifarm
          newf(ip,4)=39*1000000+900000
          if( nnf3(ip,1).le.4.or.nnf3(ip,2).le.4 ) then
            newf2(ip)=0
            call hash( newf(ip,4),ip2,neqn,count )
            newf4(ip)=newf(ip,4) !åßíPà Çf
            if( newf(ip,4).eq.ifarm ) cycle
            do k=1,2
              nnf4(ip2,k)=nnf4(ip2,k)+nnf3(ip,k)
            end do
          else
            newf4(ip)=newf(ip,1) !åßíPà ÇfÇ‹Ç‹
          end if
          iflag(ip)=1
        endif
      end do
      do i=1,hashn
        do j=1,2
           nnf5(i,j)=nnf4(i,j)
        end do
      end do
      rewind(4)
      iflag2=0
      do while( .not.eof(4) )
        read(4,'(a14,i6,i7,x,a40,i9)') aa14,num,iw1,aa40,ifarm
        backspace(4)
        read(4,'(i4,i3,62x,i2,i1)') ii,ja,iken,iw2
        ja=mod(ii,100)*1000+ja
        if( iw2.eq.1 ) then
          call hash( ifarm,ip,neqn,count )
          call hash( newf2(ip),ip2,neqn,count ) !íPà ÇforÇªÇÃÇ‹Ç‹
          if( newf3(ip2).ne.0 ) then
            call hash( newf3(ip2),ip3,neqn,count ) !åßíPà 
            if( newf4(ip3).ne.0 ) then !ëSçëÇf
              call hash( newf4(ip3),ip4,neqn,count )
              write(5,'(a14,i6,i7,x,a40,2i9,2i6)') 
     +        aa14,num,iw1,aa40,(newf4(ip3),j=1,2),(nnf4(ip4,j),j=1,2)
              honf(ip)=newf4(ip3)
            else !åßíPà 
              if( nnf4(ip3,1).le.4.or.nnf4(ip3,2).le.4 ) then
                if( iflag2(ip).eq.1 ) cycle
                iflag2(ip)=1
                iw1=39*1000000+900000
                call hash( iw1,ip5,neqn,count )
                do j=1,2
                  nnf5(ip5,j)=nnf5(ip5,j)+nnf4(ip3,j)
                end do
                honf(ip)=iw1
                cycle
              end if
              write(5,'(a14,i6,i7,x,a40,2i9,2i6)') 
     +        aa14,num,iw1,aa40,(newf3(ip2),j=1,2),(nnf4(ip3,j),j=1,2)
              honf(ip)=newf3(ip2)
            end if
          else !íPà ÇforÇªÇÃÇ‹Ç‹
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
            write(5,'(a14,i6,i7,x,a40,2i9,2i6)') 
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
                iw1=39*1000000+900000
                call hash( iw1,ip5,neqn,count )
                if( iw1.ne.newf4(ip2)) then
                do j=1,2
                  nnf5(ip5,j)=nnf5(ip5,j)+nnf4(ip3,j)
                end do
                end if
                honf(ip)=iw1
                cycle
              end if
              write(5,'(a14,i6,i7,x,a40,2i9,2i6)') 
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
              write(5,'(a14,i6,i7,x,a40,2i9,2i6)') 
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
            iw1=39*1000000+900000
            call hash( iw1,ip5,neqn,count )
            if( iw1.ne.newf4(ip) ) then
            do j=1,2
              nnf5(ip5,j)=nnf5(ip5,j)+nnf4(ip2,j)
            end do
            end if
            honf(ip)=iw1
            cycle
          end if
          write(5,'(a14,i6,i7,x,a40,2i9,2i6)') 
     +    aa14,num,iw1,aa40,(newf4(ip),j=1,2),(nnf4(ip2,j),j=1,2)
          honf(ip)=newf4(ip)
        end if
      end do
      rewind(4)
      do while( .not.eof(4) )
        read(4,'(a14,i6,i7,x,a40,i9)') aa14,num,iw1,aa40,ifarm
        backspace(4)
        read(4,'(i4,i3,62x,i2,i1)') ii,ja,iken,iw2
        ja=mod(ii,100)*1000+ja
        call hash( ifarm,ip,neqn,count )
        if( iflag2(ip).ne.1 ) cycle
        call hash( honf(ip),ip2,neqn,count )
        if( nnf5(ip2,1).le.4.or.nnf5(ip2,2).le.4 ) then
          iw2=39*1000000+900000
          honf(ip)=iw2
          call hash( iw2,ip3,neqn,count )
          write(5,'(a14,i6,i7,x,a40,2i9,2i6)') 
     +  aa14,num,iw1,aa40,(honf(ip),j=1,2),
     +  ((nnf5(ip2,j)+nnf5(ip3,j)),j=1,2)
        else
          write(5,'(a14,i6,i7,x,a40,2i9,2i6)') 
     +    aa14,num,iw1,aa40,(honf(ip),j=1,2),
     +  (nnf5(ip2,j),j=1,2)
        end if
      end do
      rewind(1)
      do while( .not.eof(1) )
        read(1,'(A85,i8,A42)') aa85,ifarm,aa42
        call hash( ifarm,ip,neqn,count )
        write(11,'(A85,i8,A42)') aa85,honf(ip),aa42
      end do
      rewind(11)
      do while( .not.eof(11) )
        read(11,*) id,(iw1,j=1,3),(iwv(j),j=1,NF),(rw1,j=1,NR),
     +  (rwv(j),j=1,2)
        do i=1,NF
          do j=1,2
            if( rwv(j).le.0 ) cycle
            if( iwv(i).gt.0 ) then
              call hash( iwv(i),ip,neqn,count )
              na(i,ip,j)=na(i,ip,j)+1
              LEV(i,ip)=iwv(i)
              ave(i,ip,j)=ave(i,ip,j)+rwv(j)
              SD(i,ip,j)=SD(i,ip,j)+rwv(j)**2
              if( na(i,ip,j).eq.1 ) then
                rmin(i,ip,j)=rwv(j)
                rmax(i,ip,j)=rwv(j)
              else
                if( rwv(j).lt.rmin(i,ip,j) ) rmin(i,ip,j)=rwv(j)
                if( rwv(j).gt.rmax(i,ip,j) ) rmax(i,ip,j)=rwv(j)
              end if
            end if
          end do
        end do
        write(2,'(i10,<NF>(a,i9),2(a,f10.3))') 
     +   id,((cv,iwv(j)),j=1,NF),cv,rw1,cv,rwv(2)
      end do
      RDATA=0
      do i=1,nf
        do j=1,hashn
          do k=1,2
            if( na(i,j,k).ne.0 ) then
            if( na(i,j,k).ge.2 ) then
              sd(i,j,k)=
     +                dsqrt((sd(i,j,k)-ave(i,j,k)**2/dble(na(i,j,k)))/
     +                                               dble(na(i,j,k)-1))
            end if
                 ave(i,j,k)=ave(i,j,k)/dble(na(i,j,k))
            end if
          end do
       end do
      end do
      do i=1,nf
        do j=1,hashn
          rdata(j)=dble(lev(i,j))
        end do
        CALL SORTRX(hashn,RDATA,IPO)
        do j=1,hashn
          if( na(i,ipo(j),1).eq.0.and.na(i,ipo(j),2).eq.0 ) cycle
          write(3,'(i3,i10,2(4f10.3,i7))') 
     +     i,lev(i,ipo(j)),((ave(i,ipo(j),k),sd(i,ipo(j),k),
     +     rmin(i,ipo(j),k),rmax(i,ipo(j),k),na(i,ipo(j),k)),k=1,2)
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
       PARAMETER(M=200000,M1=195277) !ëfêî

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
       PARAMETER(M=5000000,M1=999991) !ëfêî

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
C From Leonard J. Moss of SLAC:

C Here's a hybrid QuickSort I wrote a number of years ago.  It's
C based on suggestions in Knuth, Volume 3, and performs much better
C than a pure QuickSort on short or partially ordered input arrays.  

      SUBROUTINE SORTRX(N,DATA,INDEX)
C===================================================================
C
C     SORTRX -- SORT, Real input, indeX output
C
C
C     Input:  N     INTEGER
C             DATA  REAL
C
C     Output: INDEX INTEGER (DIMENSION N)
C
C This routine performs an in-memory sort of the first N elements of
C array DATA, returning into array INDEX the indices of elements of
C DATA arranged in ascending order.  Thus,
C
C    DATA(INDEX(1)) will be the smallest number in array DATA;
C    DATA(INDEX(N)) will be the largest number in DATA.
C
C The original data is not physically rearranged.  The original order
C of equal input values is not necessarily preserved.
C
C===================================================================
C
C SORTRX uses a hybrid QuickSort algorithm, based on several
C suggestions in Knuth, Volume 3, Section 5.2.2.  In particular, the
C "pivot key" [my term] for dividing each subsequence is chosen to be
C the median of the first, last, and middle values of the subsequence;
C and the QuickSort is cut off when a subsequence has 9 or fewer
C elements, and a straight insertion sort of the entire array is done
C at the end.  The result is comparable to a pure insertion sort for
C very short arrays, and very fast for very large arrays (of order 12
C micro-sec/element on the 3081K for arrays of 10K elements).  It is
C also not subject to the poor performance of the pure QuickSort on
C partially ordered data.
C
C Created:  15 Jul 1986  Len Moss
C
C===================================================================
 
      INTEGER   N,INDEX(N)
      REAL(8)      DATA(N)
 
      INTEGER   LSTK(31),RSTK(31),ISTK
      INTEGER   L,R,I,J,P,INDEXP,INDEXT
      REAL(8)   DATAP
 
C     QuickSort Cutoff
C
C     Quit QuickSort-ing when a subsequence contains M or fewer
C     elements and finish off at end with straight insertion sort.
C     According to Knuth, V.3, the optimum value of M is around 9.
 
      INTEGER   M
      PARAMETER (M=9)
 
C===================================================================
C
C     Make initial guess for INDEX
 
      DO 50 I=1,N
         INDEX(I)=I
   50    CONTINUE
 
C     If array is short, skip QuickSort and go directly to
C     the straight insertion sort.
 
      IF (N.LE.M) GOTO 900
 
C===================================================================
C
C     QuickSort
C
C     The "Qn:"s correspond roughly to steps in Algorithm Q,
C     Knuth, V.3, PP.116-117, modified to select the median
C     of the first, last, and middle elements as the "pivot
C     key" (in Knuth's notation, "K").  Also modified to leave
C     data in place and produce an INDEX array.  To simplify
C     comments, let DATA[I]=DATA(INDEX(I)).
 
C Q1: Initialize
      ISTK=0
      L=1
      R=N
 
  200 CONTINUE
 
C Q2: Sort the subsequence DATA[L]..DATA[R].
C
C     At this point, DATA[l] <= DATA[m] <= DATA[r] for all l < L,
C     r > R, and L <= m <= R.  (First time through, there is no
C     DATA for l < L or r > R.)
 
      I=L
      J=R
 
C Q2.5: Select pivot key
C
C     Let the pivot, P, be the midpoint of this subsequence,
C     P=(L+R)/2; then rearrange INDEX(L), INDEX(P), and INDEX(R)
C     so the corresponding DATA values are in increasing order.
C     The pivot key, DATAP, is then DATA[P].
 
      P=(L+R)/2
      INDEXP=INDEX(P)
      DATAP=DATA(INDEXP)
 
      IF (DATA(INDEX(L)) .GT. DATAP) THEN
         INDEX(P)=INDEX(L)
         INDEX(L)=INDEXP
         INDEXP=INDEX(P)
         DATAP=DATA(INDEXP)
      ENDIF
 
      IF (DATAP .GT. DATA(INDEX(R))) THEN
         IF (DATA(INDEX(L)) .GT. DATA(INDEX(R))) THEN
            INDEX(P)=INDEX(L)
            INDEX(L)=INDEX(R)
         ELSE
            INDEX(P)=INDEX(R)
         ENDIF
         INDEX(R)=INDEXP
         INDEXP=INDEX(P)
         DATAP=DATA(INDEXP)
      ENDIF
 
C     Now we swap values between the right and left sides and/or
C     move DATAP until all smaller values are on the left and all
C     larger values are on the right.  Neither the left or right
C     side will be internally ordered yet; however, DATAP will be
C     in its final position.
 
  300 CONTINUE
 
C Q3: Search for datum on left >= DATAP
C
C     At this point, DATA[L] <= DATAP.  We can therefore start scanning
C     up from L, looking for a value >= DATAP (this scan is guaranteed
C     to terminate since we initially placed DATAP near the middle of
C     the subsequence).
 
         I=I+1
         IF (DATA(INDEX(I)).LT.DATAP) GOTO 300
 
  400 CONTINUE
 
C Q4: Search for datum on right <= DATAP
C
C     At this point, DATA[R] >= DATAP.  We can therefore start scanning
C     down from R, looking for a value <= DATAP (this scan is guaranteed
C     to terminate since we initially placed DATAP near the middle of
C     the subsequence).
 
         J=J-1
         IF (DATA(INDEX(J)).GT.DATAP) GOTO 400
 
C Q5: Have the two scans collided?
 
      IF (I.LT.J) THEN
 
C Q6: No, interchange DATA[I] <--> DATA[J] and continue
 
         INDEXT=INDEX(I)
         INDEX(I)=INDEX(J)
         INDEX(J)=INDEXT
         GOTO 300
      ELSE
 
C Q7: Yes, select next subsequence to sort
C
C     At this point, I >= J and DATA[l] <= DATA[I] == DATAP <= DATA[r],
C     for all L <= l < I and J < r <= R.  If both subsequences are
C     more than M elements long, push the longer one on the stack and
C     go back to QuickSort the shorter; if only one is more than M
C     elements long, go back and QuickSort it; otherwise, pop a
C     subsequence off the stack and QuickSort it.
 
         IF (R-J .GE. I-L .AND. I-L .GT. M) THEN
            ISTK=ISTK+1
            LSTK(ISTK)=J+1
            RSTK(ISTK)=R
            R=I-1
         ELSE IF (I-L .GT. R-J .AND. R-J .GT. M) THEN
            ISTK=ISTK+1
            LSTK(ISTK)=L
            RSTK(ISTK)=I-1
            L=J+1
         ELSE IF (R-J .GT. M) THEN
            L=J+1
         ELSE IF (I-L .GT. M) THEN
            R=I-1
         ELSE
C Q8: Pop the stack, or terminate QuickSort if empty
            IF (ISTK.LT.1) GOTO 900
            L=LSTK(ISTK)
            R=RSTK(ISTK)
            ISTK=ISTK-1
         ENDIF
         GOTO 200
      ENDIF
 
  900 CONTINUE
 
C===================================================================
C
C Q9: Straight Insertion sort
 
      DO 950 I=2,N
         IF (DATA(INDEX(I-1)) .GT. DATA(INDEX(I))) THEN
            INDEXP=INDEX(I)
            DATAP=DATA(INDEXP)
            P=I-1
  920       CONTINUE
               INDEX(P+1) = INDEX(P)
               P=P-1
               IF (P.GT.0) THEN
                  IF (DATA(INDEX(P)).GT.DATAP) GOTO 920
               ENDIF
            INDEX(P+1) = INDEXP
         ENDIF
  950    CONTINUE
 
C===================================================================
C
C     All done
 
      END
