      implicit none
      integer,parameter :: hashn=40000000,maxnd=600000
      integer :: iyes,nf,nr,iwv(15),i,j,ne,ip,neqn,count,ip3
      integer :: nb,idop,ip2,id2,jp
      integer :: nn(25),idel,iy(hashn),nk(hashn),nx(hashn),nxx(hashn),no
      integer :: ngg(hashn),ng,nk2(hashn),iken,iw1,ipym,iw2,ip4,maxy
      integer,allocatable :: nxxg(:,:),inpy(:,:),npy(:),npl(:),iclose(:)
     +,iclose2(:) ,iflg(:),iflg2(:),np1(:),np2(:)
      integer :: iflg3(hashn)
      real(8) :: rwv(5),rw1,ave(25),sd(25),rmin(25),
     +rmax(25),rlim(25),rlim2,rlim3(25)
      character :: aap*2
      real(8), allocatable :: rp(:),rd(:)
      allocate ( nxxg(hashn,20),inpy(hashn,20),
     +           npy(hashn),npl(hashn),iclose(hashn),iclose2(hashn)  )
      allocate( iflg(hashn),iflg2(hashn),
     + np1(hashn),np2(hashn),rp(hashn),rd(hashn) )
c-----------------------------------------------------------------------
      write(*,*) 'Ç«Ç±ÇÃåßÇÃÉfÅ[É^Ç…ÇµÇ‹Ç∑Ç©ÅH åßî‘çÜÇì¸óÕ ëSÇƒÇÕAL'
      read(*,'(a2)') aap
      write(*,*) 'îNóÓÇÃå¯â ÇÕé¿êî(âÒãAÅjÇ≈Ç∑Ç©ÅH yes=>1 NO=>2'
      read(*,*) iyes
      nb=0
      if( iyes.eq.1 ) then
        open(1,file='repeatT_afc4q_'//aap//'.dat',status='old')
        open(5,file='RepeatAFCtq_'//aap//'.dat')
      else
        open(1,file='repeatT_afc4_'//aap//'.dat',status='old')
        open(5,file='RepeatAFCt_'//aap//'.dat')
        write(*,*) 'ïÍêîå¯â Ç™éYéüÇ»ÇÁÇPÅCîNóÓÇ»ÇÁÇQÇì¸óÕ'
        read(*,*) nb
      end if
      open(6,file='repeatT_afc2_'//aap//'.dat',status='old')
      open(10,file='limit'//aap//'.dat')
      open(500,file='lastdel_repeatT_afc2_'//aap//'.dat')
      open(600,file='zerodel_repeatT_afc2_'//aap//'.dat')
      open(666,file='prefYM_LEVEL.txt')
      open(777,file='prefYM_LEVEL2.txt')
      open(555,file='renewup.txt')
      write(*,*) 'î_â∆ÇÃå¯â ÇÕâΩóÒñ⁄ÅH'
      read(*,*) nf
      write(*,*) 'ÇdÇsÇÃå¯â ÇÕâΩóÒñ⁄ÅH'
      read(*,*) ne
      write(*,*) 'é¿êîÇÃì¸Ç¡ÇΩóÒÇÕÇ¢Ç≠Ç¬Ç†ÇËÇ‹Ç∑Ç©ÅH'
      read(*,*) NR
      write(*,*) 'ãÛëŸä˙ä‘ÇÃêßå¿ÇÕÇ†ÇËÇ‹Ç∑Ç©ÅH[yes]=>1 [NO]=>2'
      read(*,*) idop
      write(*,*) 'éYéüÇ™òAë±ÇµÇƒÇ¢Ç»Ç¢èÍçáÇÕçÌèúÇµÇ‹Ç∑Ç©ÅH[yes]1 [NO]2'
      read(*,*) idel
      write(*,*) 'îNóÓÇÕëOÇÃéYéüÇÃîNóÓÇ…ÇµÇƒÇ¢Ç‹Ç∑Ç©ÅH[yes]1 [NO]2'
      read(*,*) no
      write(*,*) 'ETèúäOÇÕëŒè€ÇÃéYéüÇæÇØÇ…ÇµÇ‹Ç∑Ç©ÅH[yes]1 [NO]2'
      read(*,*) ng
      write(*,*) 'åßîNåéÇÃå¯â ÇÇ∆ÇËÇ‹Ç∑Ç©ÅH[yes]1 [NO]2'
      read(*,*) ipym
c-----------------------------------------------------------------------
      if( ipym.eq.1 ) then
c-----------------------------------------------------------------------
      write(*,*) 'èoéYéûÇÃîNéüÇ≈ç≈Ç‡íºãﬂÇÃîNéüÇÕ'
      read(*,*) maxy
      npy=0
      npl=0
      do while( .not.eof(1) )
        read(1,*) (iwv(j),j=1,nf)
c        if( iwv(6).eq.maxy ) then
c          iwv(7)=0
c        end if
        call hash (iwv(1),ip,neqn,count ) 
        npy(ip)=npy(ip)+1
        if( iwv(6).eq.0 ) cycle
        iken=(iwv(nf)-mod(iwv(nf),1000000))/1000000
        iw1=iken*1000000+iwv(6)*100+iwv(7)
        inpy(ip,npy(ip))=iw1
        call hash(iw1,ip2,neqn,count)
        npl(ip2)=npl(ip2)+1
      end do
      npy=0
      rewind(1)
      do while ( .not.eof(1) )
        read(1,*) (iwv(j),j=1,nf)
        call hash (iwv(1),ip,neqn,count ) 
        npy(ip)=npy(ip)+1
        if( iwv(6).eq.0 ) cycle
        iken=(iwv(nf)-mod(iwv(nf),1000000))/1000000
        iw1=inpy(ip,npy(ip))
c        if( iwv(6).eq.maxy ) then
c          iwv(7)=0
c        end if
        call hash(iw1,ip2,neqn,count)
        if( iwv(6).ne.maxy ) then
          if( npl(ip2).lt.1200 )  then!åßîNåéÇ≈1200ì™ñ¢ñûÇ≈Ç†ÇÍÇŒÅCÉuÉçÉbÉNÅi[51]ìåïî [52]íÜïîÅEíÜélçë [54]ã„èBÅjÇ≈èWñÒÇ∑ÇÈÅB
            if( iken.le.15 ) then
              iw1=51*1000000+iwv(6)*100+iwv(7)
              inpy(ip,npy(ip))=iw1
              call hash( iw1,ip2,neqn,count )
              npl(ip2)=npl(ip2)+1
            else
              if( iken.le.25 ) then
                iw1=52*1000000+iwv(6)*100+iwv(7)
                inpy(ip,npy(ip))=iw1
                call hash( iw1,ip2,neqn,count )
                npl(ip2)=npl(ip2)+1
              else
                if( iken.le.39 ) then
                  iw1=52*1000000+iwv(6)*100+iwv(7)
                  inpy(ip,npy(ip))=iw1
                  call hash( iw1,ip2,neqn,count )
                  npl(ip2)=npl(ip2)+1
                else
                  iw1=54*1000000+iwv(6)*100+iwv(7)
                    inpy(ip,npy(ip))=iw1
                    call hash( iw1,ip2,neqn,count )
                    npl(ip2)=npl(ip2)+1
                end if
              end if
            end if
          end if
        else
          if( iken.le.15 ) then!ç≈èIîNÇÕÅCÉuÉçÉbÉNÇÃåéì˙Åi[51]ìåïî [52]íÜïîÅEíÜélçë [54]ã„èBÅjÇ≈èWñÒÇ∑ÇÈÅB
              iw1=51*1000000+iwv(6)*100+iwv(7)
              inpy(ip,npy(ip))=iw1
              call hash( iw1,ip2,neqn,count )
              npl(ip2)=npl(ip2)+1
          else
            if( iken.le.25 ) then
              iw1=52*1000000+iwv(6)*100+iwv(7)
              inpy(ip,npy(ip))=iw1
              call hash( iw1,ip2,neqn,count )
              npl(ip2)=npl(ip2)+1
            else
              if( iken.le.39 ) then
                iw1=52*1000000+iwv(6)*100+iwv(7)
                inpy(ip,npy(ip))=iw1
                call hash( iw1,ip2,neqn,count )
                npl(ip2)=npl(ip2)+1
              else
                iw1=54*1000000+iwv(6)*100+iwv(7)
                  inpy(ip,npy(ip))=iw1
                  call hash( iw1,ip2,neqn,count )
                  npl(ip2)=npl(ip2)+1
              end if
            end if
          end if
        end if
      end do
      rewind(1)
      iclose=0
      iclose2=0
      npy=0
      do while( .not.eof(1) )
        read(1,*) (iwv(j),j=1,nf)
        call hash (iwv(1),ip,neqn,count ) 
c        if( iwv(6).eq.maxy ) then
c          iwv(7)=0
c        end if
        npy(ip)=npy(ip)+1
        if( iwv(6).eq.0 ) cycle
        iw1=inpy(ip,npy(ip))
        call hash(iw1,ip2,neqn,count)
        if( iclose(ip2).ne.1 ) then
          write(666,'(i8,i7)') iw1,npl(ip2)
          iclose(ip2)=1
        end if
        if( iwv(6).ne.maxy ) then
          if( npl(ip2).lt.1200 )then
            iken=(iwv(nf)-mod(iwv(nf),1000000))/1000000
            if( iken.le.15 ) then
              iken=51
            else
              if( iken.le.39 ) then
                iken=52
              else
                iken=54
              end if
            end if
            do i=1,5
              iw1=iken*1000000+(iwv(6)-i)*100+iwv(7)
              iw2=iken*1000000+(iwv(6)+i)*100+iwv(7)
              call hash( iw1,ip3,neqn,count )
              call hash( iw2,ip4,neqn,count )
              if( npl(ip3).ge.npl(ip4) ) then
                if( npl(ip3).lt.1200) cycle
              else
                ip3=ip4
                iw1=iw2
                if( npl(ip3).lt.1200) cycle
              end if
              inpy(ip,npy(ip))=iw1
              if( iclose2(ip2).eq.0 ) then
                npl(ip3)=npl(ip3)+npl(ip2)
                iclose2(ip2)=1
              end if
            end do
          end if
        else
          iken=(iwv(nf)-mod(iwv(nf),1000000))/1000000
          if( iken.le.15 ) then
            iken=51
          else
            if( iken.le.39 ) then
              iken=52
            else
              iken=54
            end if
          end if
          do i=1,5
            iw1=iken*1000000+iwv(6)*100+iwv(7)
c            iw2=iken*1000000+(iwv(6)+i)*100+iwv(7)
            call hash( iw1,ip3,neqn,count )
c            call hash( iw2,ip4,neqn,count )
c            if( npl(ip3).ge.npl(ip4) ) then
c              if( npl(ip3).lt.1200) cycle
c            else
c              ip3=ip4
c              iw1=iw2
c              if( npl(ip3).lt.1200) cycle
c            end if
            inpy(ip,npy(ip))=iw1
            if( iclose2(ip2).eq.0 ) then
              npl(ip3)=npl(ip3)+npl(ip2)
              iclose2(ip2)=1
            end if
          end do
        end if
      end do
      rewind(1)
      iclose=0
      npy=0
      iclose2=0
      do while( .not.eof(1) )
        read(1,*) (iwv(j),j=1,nf)
c        if( iwv(6).eq.maxy ) then
c          iwv(7)=0
c        end if
        call hash (iwv(1),ip,neqn,count )
        npy(ip)=npy(ip)+1
        if( iwv(6).eq.0 ) cycle
        iw1=inpy(ip,npy(ip))
        call hash(iw1,ip2,neqn,count)
        if( iclose(ip2).ne.1 ) then
          write(777,'(i8,i7)') iw1,npl(ip2)
          iclose(ip2)=1
        end if
      end do
c----------------------------------------------------------------------
      end if
c-----------------------------------------------------------------------
!      go to 999
c     éYéüÇ™òAë±ÇµÇƒÇ¢Ç»Ç¢Ç‡ÇÃÇÕÇªÇÍà»ç~ÇçÌèúÇÃï˚êjì]ä∑
      id2=0
      nk=0
      iflg2=0
      iflg3=0
c      if( idel.eq.1 ) then
        do while( .not.eof(6) )
          read(6,*) (iwv(j),j=1,11),(rwv(j),j=1,3)
          call hash( iwv(1),ip,neqn,count )
          if( iflg2(ip).ne.0.or.iflg3(ip).ne.0 ) then
            go to 100
          end if
          if( iwv(1).ne.id2 ) then
            nk(ip)=1
            if( rwv(2).eq.0.0d0 ) then !èâéYåéóÓÇ™ñ≥Ç¢
              if( idel.eq.1 ) then
                iflg2(ip)=2
              end if
              iflg3(ip)=1
              np1(ip)=iwv(10)
              np2(ip)=int(rwv(1))
              rp(ip)=rwv(1)
              rd(ip)=rwv(3)
              iy(ip)=iwv(4)
              nx(ip)=1
            end if
          else
            nk(ip)=nk(ip)+1
            if( iwv(10).ne.jp+1 ) then
              if( idel.eq.1 ) then
                iflg2(ip)=1
              end if
              iflg3(ip)=1
              np1(ip)=iwv(10)
              np2(ip)=int(rwv(1))
              rp(ip)=rwv(1)
              rd(ip)=rwv(3)
              iy(ip)=iwv(4)
              nx(ip)=nk(ip)
            end if
          end if
100       continue
          id2=iwv(1)
          jp=iwv(10)
        end do
c      end if
      if( no.eq.1 ) then
        no=2
      else
        no=1
      end if
      iflg=0
      nn=0
      ave=0.0d0
      sd=0.0d0
      nk=0
      id2=0
      ngg=0
      rewind(6)
      do while( .not.eof(6) )
        read(6,*) (iwv(j),j=1,11),(rwv(j),j=1,3)
        if( idop.eq.1 ) then
          rw1=(365.0d0+310.0d0)/365.0d0
        else
          rw1=2.0d0
        end if
        call hash( iwv(1),ip,neqn,count )
        if( iwv(1).ne.id2 ) then
          nk(ip)=1
        else
          nk(ip)=nk(ip)+1
        end if
        if( iflg2(ip).ne.0.and.idel.eq.1 ) then
          go to 10
        end if
c        if( rwv(1).ge.( rw1*(iwv(10)-1)+(37.0d0/12.0d0) ) )then
        if( iwv(10).ge.3.and.rwv(1).ge.
     +      ( rw1*(iwv(10)-no)+(37.0d0/12.0d0)).and.iwv(8).eq.1.and.
     +        iflg3(ip).eq.0 )then !ëOÇÃéYéüÇÃîNóÓÇÕ éYéüÇ™òAë±ÇµÇƒÇ¢Ç»Ç¢=>ä˘Ç…èúäOÇ≥ÇÍÇƒÇ¢ÇÈÇ≈ÇnÇjÇ…Ç∑ÇÈÅ@2015/4.2
          iflg(ip)=1 !
c          np1(ip)=iwv(10)
c          np2(ip)=int(rwv(1))
c          rp(ip)=rwv(1)
c          rd(ip)=rwv(3)
c          iy(ip)=iwv(4)
           ngg(ip)=ngg(ip)+1
           nxxg(ip,ngg(ip))=nk(ip)
           if( iwv(1).eq.912072068) then
             write(*,*) '912072068',nk(ip),ngg(ip)
           end if
        else
          if(  iwv(10).ge.3.and.rwv(1).ge.
     +      ( rw1*(iwv(10)-no)+(37.0d0/12.0d0)).and.iwv(8).eq.1.and.
     +        iflg3(ip).eq.1 )then 
              write(555,'(i10,3f10.3)') iwv(1),(rwv(j),j=1,3)
          end if
        end if
        if( iwv(10).eq.2.and.rwv(1).ge.
     +      ( rw1*(iwv(10)-1)+(37.0d0/12.0d0)).and.iwv(8).eq.1 )then !èâéYÇ™37Éïåéà»è„ÇÃèÍçáÅCÇQéYñ⁄Ç™37ÉïåéÅ{675ì˙à»ì‡Ç»ÇÁÇnÇj<=2014.05í«â¡ 
          iflg(ip)=1 !
c          np1(ip)=iwv(10)
c          np2(ip)=int(rwv(1))
c          rp(ip)=rwv(1)
c          rd(ip)=rwv(3)
c          iy(ip)=iwv(4)
           ngg(ip)=ngg(ip)+1
           nxxg(ip,ngg(ip))=nk(ip)
           if( iwv(1).eq.912072068) then
             write(*,*) '912072068',nk(ip),ngg(ip)
           end if
        end if

        ip2=iwv(10)
        nn(ip2)=nn(ip2)+1
        ave(ip2)=ave(ip2)+rwv(1)
        sd(ip2)=sd(ip2)+rwv(1)**2
        if( nn(ip2).eq.1 ) then
          rmax(ip2)=rwv(1)
          rmin(ip2)=rwv(1)
        else
          if( rwv(1).lt.rmin(ip2) ) rmin(ip2)=rwv(1)
          if( rwv(1).gt.rmax(ip2) ) rmax(ip2)=rwv(1)
        end if
10      continue
        id2=iwv(1)
      end do
      do i=1,hashn
        nk2(i)=nk(i)
      end do
      do i=2,25
        if( nn(i).eq.0 ) cycle
        sd(i)=sqrt((sd(i)-ave(i)**2/dble(nn(i)))/dble(nn(i)-1) )
        ave(i)=ave(i)/dble(nn(i))
        rlim(i)=ave(i)+3.0d0*sd(i)
        rlim2=rw1*(i-no)+(37.0d0/12.0d0)
        if( rlim(i).lt.rlim2 ) then
          rlim3(i)=rlim2
        else
          rlim3(i)=rlim(i)
        end if
        write(10,'(i3,i9,7f10.3)') 
     +   i,nn(i),ave(i),sd(i),rmin(i),rmax(i),rlim(i),rlim2,rlim3(i)
      end do
c-----------------------------------------------------------------------
      nk=0
      id2=0
      rewind(1)
      DO WHILE( .not.eof(1) )
        READ(1,*) (iwv(J),J=1,NF),(rwv(J),J=1,nr)
        call hash( iwv(1),ip,neqn,count )
        if(iwv(1).ne.id2 ) then 
          nk(ip)=1
        else
          nk(ip)=nk(ip)+1
        end if
        if( ng.eq.2 ) then
          if( iwv(ne).ge.2.and.nxx(ip).eq.0 ) then
            iflg(ip)=1
            nxx(ip)=nk(ip)
          end if
        else
          if( iwv(ne).ge.2 ) then
            do i=1,ngg(ip)
              if( nxxg(ip,i).eq.nk(ip) ) go to 1
            end do
            ngg(ip)=ngg(ip)+1
            nxxg(ip,ngg(ip))=nk(ip)
            iflg(ip)=1
1           continue
          end if
        end if
        id2=iwv(1)
      end do
      rewind(1)
      write(*,*) nb
      nk=0
      id2=0
      DO WHILE( .not.eof(1) )
        READ(1,*) (iwv(J),J=1,NF),(rwv(J),J=1,nr)
        call hash( iwv(1),ip,neqn,count )
        if(iwv(1).ne.id2 ) then 
          nk(ip)=1
        else
          nk(ip)=nk(ip)+1
        end if
        if( ng.eq.2 ) then!ÇdÇså„ÇÕëSÇƒèúÇ≠èÍçá
          if( iflg(ip).eq.1 ) then
            if( nxx(ip).eq.1 ) then
              do j=4,NF
                iwv(j)=0
              end do
              do j=1,NR
                rwv(j)=0.0d0
              end do
              iflg(ip)=2
              write(600,'(4i11,<NF-5>i9,<NR>f10.3)') 
     +       (iwv(J),J=1,ne-1),(iwv(j),j=ne+1,NF),(rwv(J),J=1,nr)
            else
              if( nk(ip).ge.nxx(ip) ) then
                write(500,'(4i11,<NF-5>i9,<NR>f10.3)') 
     +     (iwv(J),J=1,ne-1),(iwv(j),j=ne+1,NF),(rwv(J),J=1,nr)
                iflg(ip)=2
                go to 200
              end if
            end if
          else
            if( iflg(ip).eq.2 ) then
              write(500,'(4i11,<NF-5>i9,<NR>f10.3)') 
     +   (iwv(J),J=1,ne-1),(iwv(j),j=ne+1,NF),(rwv(J),J=1,nr)
              go to 200
            end if
          end if
        else !ëŒè€éYéüÇèúÇ≠èÍçá
          if( iflg(ip).eq.1) then
            if( ngg(ip).eq.nk2(ip) ) then !ÇdÇséYÇµÇ©Ç»Ç¢èÍçá
              do j=4,NF
                iwv(j)=0
              end do
              do j=1,NR
                rwv(j)=0.0d0
              end do
              write(600,'(4i11,<NF-5>i9,<NR>f10.3)') 
     +       (iwv(J),J=1,ne-1),(iwv(j),j=ne+1,NF),(rwv(J),J=1,nr)
             iflg(ip)=2
            else
              do i=1,ngg(ip)
                if( nxxg(ip,i).eq.nk(ip) ) then
                    write(500,'(4i11,<NF-5>i9,<NR>f10.3)') 
     +              (iwv(J),J=1,ne-1),(iwv(j),j=ne+1,NF),(rwv(J),J=1,nr)
                    go to 200
                end if
              end do
            end if
          else
            if( iflg(ip).eq.2) then
              write(500,'(4i11,<NF-5>i9,<NR>f10.3)') 
     +   (iwv(J),J=1,ne-1),(iwv(j),j=ne+1,NF),(rwv(J),J=1,nr)
              go to 200
            end if
          end if
        end if        
        if( iflg2(ip).eq.1 ) then
          if( np1(ip).eq.2.or.nx(ip).eq.1 ) then
            do j=4,NF
              iwv(j)=0
            end do
            do j=1,NR
              rwv(j)=0.0d0
            end do
            iflg(ip)=2
            write(600,'(4i11,<NF-5>i9,<NR>f10.3)') 
     +   (iwv(J),J=1,ne-1),(iwv(j),j=ne+1,NF),(rwv(J),J=1,nr)
          else
            if(  nb.eq.0  ) then
              if( rwv(2).ge.rp(ip) ) then
                write(500,'(4i11,<NF-5>i9,<NR>f10.3)') 
     +   (iwv(J),J=1,ne-1),(iwv(j),j=ne+1,NF),(rwv(J),J=1,nr)
                go to 200
              end if
              if( nk(ip).ge.nx(ip) ) then
                write(500,'(4i11,<NF-5>i9,<NR>f10.3)') 
     +   (iwv(J),J=1,ne-1),(iwv(j),j=ne+1,NF),(rwv(J),J=1,nr)
                go to 200
              end if
            else
              if( nb.eq.1 ) then
                if( iwv(NF-1).eq.np1(ip).
     +           and.rd(ip).eq.rwv(nr).and.iy(ip).eq.iwv(6) ) then
                   write(500,'(4i11,<NF-5>i9,<NR>f10.3)') 
     +   (iwv(J),J=1,ne-1),(iwv(j),j=ne+1,NF),(rwv(J),J=1,nr)
                   go to 200
                end if
                if( iwv(NF-1).gt.np1(ip) ) then
                   write(500,'(4i11,<NF-5>i9,<NR>f10.3)') 
     +   (iwv(J),J=1,ne-1),(iwv(j),j=ne+1,NF),(rwv(J),J=1,nr)
                   go to 200
                end if
                if( nk(ip).ge.nx(ip) ) then
                   write(500,'(4i11,<NF-5>i9,<NR>f10.3)') 
     +   (iwv(J),J=1,ne-1),(iwv(j),j=ne+1,NF),(rwv(J),J=1,nr)
                   go to 200
                end if
              else
                if( iwv(NF-1).eq.np2(ip).
     +           and.rd(ip).eq.rwv(nr).and.iy(ip).eq.iwv(6) )then
                   write(500,'(4i11,<NF-5>i9,<NR>f10.3)') 
     +   (iwv(J),J=1,ne-1),(iwv(j),j=ne+1,NF),(rwv(J),J=1,nr)
                   go to 200
                end if
                if( iwv(NF-1).gt.np2(ip) ) then
                   write(500,'(4i11,<NF-5>i9,<NR>f10.3)') 
     +   (iwv(J),J=1,ne-1),(iwv(j),j=ne+1,NF),(rwv(J),J=1,nr)
                   go to 200
                end if
                if( nk(ip).ge.nx(ip) ) then
                   write(500,'(4i11,<NF-5>i9,<NR>f10.3)') 
     +   (iwv(J),J=1,ne-1),(iwv(j),j=ne+1,NF),(rwv(J),J=1,nr)
                   go to 200
                end if
              end if
            end if
          end if
        end if
        if( iflg2(ip).eq.2 ) then
          do j=4,NF
            iwv(j)=0
          end do
          do j=1,NR
            rwv(j)=0.0d0
          end do
          iflg(ip)=2
          write(600,'(4i11,<NF-5>i9,<NR>f10.3)') 
     +   (iwv(J),J=1,ne-1),(iwv(j),j=ne+1,NF),(rwv(J),J=1,nr)
        end if
        if( ipym.ne.1 ) then
        write(5,'(4i11,<NF-5>i9,<NR>f10.3)')
     +   (iwv(J),J=1,ne-1),(iwv(j),j=ne+1,NF),(rwv(J),J=1,nr)
        else
        write(5,'(4i11,<NF-5>i9,<NR>f10.3)')
     +   (iwv(J),J=1,5),iwv(7),inpy(ip,nk(ip)),
     +        (iwv(j),j=8,ne-1),(iwv(j),j=ne+1,NF),(rwv(J),J=1,nr)
        end if
200     continue
        id2=iwv(1)
      end do
999   continue
      stop
      end

c----------------------------------------------------------------------
c
      SUBROUTINE HASH(K,IP,NEQN,COUNT)
c
c-----------------------------------------------------------------------
c      PARAMETER(M=31,M1=17) !origin
       PARAMETER(M=40000000,M1=30422360) !ëfêî

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
