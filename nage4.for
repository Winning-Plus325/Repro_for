c     ‚QY‚Ü‚Å‚Ì‹L˜^A—‘‹æ•ª‚Í‚O‚Ì‚İj‚·‚éA«•Ê‹¨‚Í—Y‚Ö
c     ‚½‚¾‚µA•¡Y‚É’ˆÓF¶”NŒ“ú‚ª“™‚µ‚¢ê‡
c     —Y‚ªŠÜ‚Ü‚ê‚Ä‚¢‚½‚ç—Y‚ğc‚·B‚»‚Ì‚È‚©‚É€Y‚ª‚ ‚ê‚Î“ü‚ê‚È‚¢

      implicit none
      integer,parameter :: maxn=2500000
      integer :: iwv(4),nd,ifr(maxn,4),i,j,k,ninsin
      integer :: na,iw1,ii
      integer,allocatable :: nind(:,:),jwv(:,:,:),nc(:)
      character :: aid*10,ch10*10,ab*11,a10*10,c10*10,a13*13,
     +             bidd(100000)*10
      character :: ac*22,ch11*11,aaa*3,reas*10,awv1*1,ch13*22,mid*60
      character, allocatable :: 
     +  bid(:)*10,cid(:,:)*11,did(:,:)*22,eid(:,:)*3
      open(1,file='kotomsa_1d.txt',status='old')
      open(2,file='kotomsa_2d.txt',status='unknown')      
      open(22,file='2kotomsa_2.txt',status='unknown')
      open(33,file='3kotomsa_2.txt',status='unknown')
      open(3,file='kotoms_f1.txt',status='unknown')
      open(88,file='tmp',status='unknown')
      open(55,file='fukusan.txt',status='unknown')
c      do while(.not.eof(3) )
c        read(3,*) awv1
c      end do

      nd=0
      jwv(:,:,:)=0
      a10='0000000000'
      do while( .not.eof(1) )
        read(1,'(a10)') aid
        if( aid.ne.a10 ) then
          na=na+1
          a10=aid
        end if
      end do
      rewind(1)
      allocate ( bid(1:na),cid(1:na,1:130),did(1:na,1:130),
     +          eid(1:na,1:130),
     +          nind(1:na,1:130),jwv(1:na,1:4,1:130),nc(1:na) )
      do while( .not.eof(1) )
        read(1,'(a10,a3,4i2,a11,a22,i6)') aid,aaa,(iwv(j),j=1,4),ab,ac,
     +  ninsin
        if( iwv(4).eq.3 ) iwv(4)=1
        if( ch10.eq.aid ) then
          if (nc(nd).ge.25 ) go to 10
          nc(nd)=nc(nd)+1
          eid(nd,nc(nd))=aaa
          do j=1,4
              jwv(nd,j,nc(nd))=iwv(j)
          end do
          cid(nd,nc(nd))=ab
          nind(nd,nc(nd))=ninsin
          did(nd,nc(nd))=ac
c         •¡Y‚Ìê‡
c          if( ch11.eq.ab.and.iwv(1).eq.0 ) then
        if( iwv(1).ne.1 ) then
          if( ch11.eq.ab.and.iwv(1).ne.1 ) then  !•¡Y
            write(55,*) aid,(iwv(j),j=1,4)
            jwv(nd,:,nc(nd))=0
            nc(nd)=nc(nd)-1
            if( iwv(2).le.1 ) eid(nd,nc(nd))=aaa !Yq”Ô†
            if( iwv(4).eq.1 ) then           !“Ç‚ñ‚Å‚¢‚éƒf[ƒ^‚ª—Y
              jwv(nd,4,nc(nd))=iwv(4)            !«•Ê‚ğ—Y‚Æ‚·‚é
            end if
            if( jwv(nd,2,nc(nd)).ge.2.and.iwv(2).le.1 ) then
              jwv(nd,2,nc(nd))=iwv(2)
            end if
            if( jwv(nd,2,nc(nd)).le.1.and.iwv(2).ge.2 ) then
              iwv(2)=jwv(nd,2,nc(nd))
            end if
            go to 55
          elseif( ch13.eq.ac ) then !•ƒ‹‚Æí•t‚¯”NŒ“ú
            jwv(nd,:,nc(nd))=0
            nc(nd)=nc(nd)-1
            if( iwv(2).le.1 ) eid(nd,nc(nd))=aaa
            if( iwv(4).eq.1 ) then
              jwv(nd,4,nc(nd))=iwv(4)
            end if
            if( jwv(nd,2,nc(nd)).ge.2.and.iwv(2).le.1 ) then
              jwv(nd,2,nc(nd))=iwv(2)
            end if
            if( jwv(nd,2,nc(nd)).le.1.and.iwv(2).ge.2 ) then
              iwv(2)=jwv(nd,2,nc(nd))
            end if
          end if
        end if
55        continue
c          if( iwv(1).ne.0 ) ifr(nd,1)=1                     !—‘‹æ•ªˆÙí
          if( iwv(2).ge.2 ) ifr(nd,2)=2                     !—¬YA€Y
          if( iwv(4).ge.4 ) ifr(nd,4)=4                     !«•ÊˆÙí
          ch11=ab
          ch13=ac
          go to 10
        end if
        nd=nd+1
        nc(nd)=1
        if( mod(nd,100000).eq.0 ) write(*,*) nd
        bid(nd)=aid
        eid(nd,1)=aaa
        ch10=aid
        ch11=ab
        ch13=ac
        cid(nd,1)=ab
        did(nd,1)=ac
        nind(nd,1)=ninsin
        ifr(nd,:)=0
        do j=1,4
          jwv(nd,j,1)=iwv(j)
        end do
c        if( iwv(1).ne.0 ) ifr(nd,1)=1                                   !—‘‹æ•ªˆÙí
        if( iwv(2).ge.2 ) ifr(nd,2)=2                                   !—¬YA€Y
        if( iwv(4).ge.4 ) ifr(nd,4)=4                                   !«•ÊˆÙí
10      continue
      end do
      do i=1,nd
        do j=1,4
        if( ifr(i,j).ne.0 ) then
c          if(j.eq.1) reas='—‘‹æ•ª'
          if(j.eq.2) reas='—¬YA€Y'
          if(j.eq.4) reas='«•ÊˆÙí'
          write(3,'(a11,4i2,a10)') bid(i),(ifr(i,k),k=1,4),reas
c         go to 20
        end if
        end do
        do j=1,nc(i)
c          if( nind(i,j).ne.0.and.jwv(i,4,j).ne.0 ) then
            write(88,'(a10,a3,4i2,a11,a22,i6)')
     +        bid(i),eid(i,j),(jwv(i,k,j),k=1,4),cid(i,j),did(i,j)
     +,nind(i,j)
c         end if
        end do
c        do j=2,3
c          if( jwv(i,4,2).ne.0.and.jwv(i,4,3).ne.0 ) then
c            write(22,'(a10,a3,4i2,a11,a22,i6)')
c     +        bid(i),eid(i,j),(jwv(i,k,j),k=1,4),cid(i,j),did(i,j)
c     +,nind(i,j)
c          end if
c        end do
c        do j=3,4
c          if( jwv(i,4,3).ne.0.and.jwv(i,4,4).ne.0 ) then
c            write(33,'(a10,a3,4i2,a11,a22,i6)')
c     +        bid(i),eid(i,j),(jwv(i,k,j),k=1,4),cid(i,j),did(i,j)
c     +,nind(i,j)
c          end if
c        end do
20      continue
      end do
      rewind(3)
      nd=1
      bid(1)='0000000000'
      c10='0000000000'
c      rewind(88)
c      do while( .not.eof(88) )
c        read(88,'(a10,4x,i1)') a10,iw1
c        if( iw1.ge.1 ) then
c          if( a10.ne.c10 ) then
c            nd=nd+1
c            bidd(nd)=a10
c            c10=a10
c          end if
c        end if
c      end do
      rewind(88)
      ii=2
      do while( .not.eof(88) )
        read(88,'(a60)') mid
        do i=ii,nd
          if( bidd(i).eq.mid(1:10) ) then
c            if( mid(1:10).ne.c10) ii=i+1
c            c10=mid(1:10)
            ii=i
            go to 50
          end if
          if( bidd(i).gt.mid(1:10) ) exit
        end do
        write(2,'(a60)') mid
50      continue
      end do
      stop
      end