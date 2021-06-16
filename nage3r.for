c     îDêPä˙ä‘ÇÃéZèo
      implicit none
      integer :: iwv(4),iw(4),ir(2),i,j,jw(4),isire,ibun,ibre,iw2(3)
      integer :: jw2(3),ninsin,ii,nd
      character :: aid*13,delid*10,mid*60,aidd(1000000)*10,c10*10
      open(1,file='kotomsl',status='old')
      open(2,file='kotomsa_1d.txt',status='unknown')
      open(3,file='kotoms_f1.txt',status='unknown')
      open(99,file='glt.txt')
      open(88,file='tmp.txt')
      delid='0000000000'
      do while(.not.eof(1) )
        read(1,'(a13,4i1,i1,3i2,i10,i1,3i2)') aid,(iwv(j),j=1,4),
     +   (iw(j),j=1,4),iSIRE,(jw(j),j=1,4)
         if( aid(1:10).eq.delid ) go to 10
         if( iw(1).eq.3 ) iw(2)=iw(2)+1925
         if( iw(1).eq.4 ) iw(2)=iw(2)+1988
         if( iw(1).eq.5 ) iw(2)=iw(2)+2018
         if( aid.eq.'2912289450008'.and.jw(1).eq.0) then !éˆê∏îNåéì˙òRÇÍ
           write(*,*) aid
           jw(1)=4
           jw(2)=26
           jw(3)=7
           jw(4)=10
         end if
         if( iw(2).le.1925 ) then
           write(3,'(a13,a)') aid,'ï™ïÿîNåéì˙ïsñæ'
           delid=aid(1:10)
           go to 10
         end if
         if( jw(1).eq.3 ) jw(2)=jw(2)+1925
         if( jw(1).eq.4 ) jw(2)=jw(2)+1988
         if( jw(1).eq.5 ) jw(2)=jw(2)+2018
         if( jw(2).le.1985 ) then
           write(3,'(a13,a)') aid,'éÌïtîNåéì˙ïsñæ'
           delid=aid(1:10)
           go to 10
         end if
         do i=1,3
           iw2(i)=iw(1+i)
           jw2(i)=jw(1+i)
         end do
         call calend(ibun,iw2)
         call calend(ibre,jw2)
         ninsin=ibun-ibre
         if( jw(2).eq.0 ) ninsin=0
         if( ninsin.lt.0 ) ninsin=0
c         if( ninsin.le.240 ) then
c           write(3,'(a13,a,i4)') aid,'îDêPä˙ä‘ëÅéY',ninsin
c           delid=aid(1:10)
c           if( iwv(2).le.1 ) write(99,'(a13,x,i5,i2)') aid,ninsin,iwv(2)
c           go to 10
c            if( iwv(2).eq.0 ) iwv(2)=3  !ó¨éYàµÇ¢
c     +      write(3,'(a13,a,i4)') aid,'îDêPä˙ä‘ëÅéY',ninsin
c            if( iwv(2).eq.1 ) iwv(2)=3  !ó¨éYàµÇ¢
c         else
c            if( ninsin.le.260 ) then
c            if( iwv(2).eq.0 ) iwv(2)=1
c            end if
c         end if
c         if( ninsin.ge.311 ) then
c           write(3,'(a13,a,i4)') aid,'îDêPä˙ä‘í∑ä˙ç›ëŸ',ninsin
c           delid=aid(1:10)
c           if( iwv(2).le.1 ) write(99,'(a13,x,i5,i2)') aid,ninsin,iwv(2)
c           go to 10
c            if( iwv(2).eq.0 )
c     +       write(3,'(a13,a,i4)') aid,'îDêPä˙ä‘í∑ä˙ç›ëŸ',ninsin
c         end if
         write(88,'(a13,4i2,i5,2i3,i11,i5,2i3,i6)') aid,(iwv(j),j=1,4),
     +   (iw(j),j=2,4),iSIRE,(jw(j),j=2,4),ninsin
         if( iwv(2).le.1 ) write(99,'(a13,x,i5,i2)') aid,ninsin,iwv(2)
10    continue
      end do
      rewind(3)
      nd=0
      do while( .not.eof(3))
        nd=nd+1
        read(3,'(a10)') aidd(nd)
      end do
      rewind(88)
      ii=1
      c10='0000000000'
      do while( .not.eof(88) )
        read(88,'(a60)') mid
        do i=ii,nd
          if( aidd(i).eq.mid(1:10) ) then
            if( mid(1:10).ne.c10) then
              ii=i
              c10=mid(1:10)
            end if
            go to 50
          end if
          if( aidd(i).gt.mid(1:10)) exit
        end do
c        IF( mid(15:15).eq.'1') CYCLE
        write(2,'(a60)') mid
50      continue
      end do
      stop
      end
c-------------------------------------------------------
      SUBROUTINE CALEND( ICHI,IWV )
        IMPLICIT NONE
        INTEGER :: IWV(3),ICHI
        INTEGER :: IM1,IM2,IM3,ID1,ID2,ID3,URU
C-----------------------------------------------------------------------
        IM1=MOD(IWV(1),4)
        ID1=(IWV(1)-IM1)/4
        IM2=MOD(IWV(1),400)
        ID2=(IWV(1)-IM2)/400
        IM3=MOD(IWV(1),100)
        ID3=(IWV(1)-IM3)/100
        URU=ID1+ID2-ID3
        IF( IM1.EQ.0.AND.IM3.NE.0 ) THEN
          IF( IWV(2).LE.2 ) THEN
            URU=URU-1
          END IF
        END IF
        IF( IM2.EQ.0 ) THEN
          IF( IWV(2).LE.2 ) THEN
            URU=URU-1
          END IF
        END IF
        ICHI=IWV(1)*365+IWV(3)+URU
        IF( IWV(2).EQ.2 ) THEN
          ICHI=ICHI+31
        ELSE IF ( IWV(2).EQ.3 ) THEN
          ICHI=ICHI+59
        ELSE IF ( IWV(2).EQ.4 ) THEN
          ICHI=ICHI+90
        ELSE IF ( IWV(2).EQ.5 ) THEN
          ICHI=ICHI+120
        ELSE IF ( IWV(2).EQ.6 ) THEN
          ICHI=ICHI+151
        ELSE IF ( IWV(2).EQ.7 ) THEN
          ICHI=ICHI+181
        ELSE IF ( IWV(2).EQ.8 ) THEN
          ICHI=ICHI+212
        ELSE IF ( IWV(2).EQ.9 ) THEN
          ICHI=ICHI+243
        ELSE IF ( IWV(2).EQ.10 ) THEN
          ICHI=ICHI+273
        ELSE IF ( IWV(2).EQ.11 ) THEN
          ICHI=ICHI+304
        ELSE IF ( IWV(2).EQ.12 ) THEN
          ICHI=ICHI+334
        END IF
        RETURN
      END SUBROUTINE CALEND
      