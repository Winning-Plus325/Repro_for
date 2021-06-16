      implicit none
      integer,parameter :: hashn=10000000
      integer,allocatable :: hb(:,:),idday(:,:),nn(:),jdday(:,:,:)
      integer :: isex,id,isan,iwv(4),jwv(3),ip,neqn,count,idop,iciv,
     + iglt,jdop,jglt,kdop,kglt,ichi1,ichi2,ichi3,jchi1,jchi2,jd,
     +sire,cday(3),sday(3),iflag,nc,ichi,i,j,eday(3),izero,n(11),bday(3)
     +,cday2(3)
      real(8) :: rage,rafc,ave(11),sd(11)
      character :: tr(11)*15,cv*1
      open(1,file='ketof',status='old')
      open(2,file='kotomsa_2d.txt',status='old')
      open(3,file='jyuranf_edits',status='old')
      open(50,file='nocutREC.dat')
      open(51,file='cutREC.dat')
      open(52,file='730cutREC.dat')
      open(53,file='730cutREC.csv')
      cv=','
      allocate( hb(hashn,3),idday(hashn,70),nn(hashn),jdday(hashn,70,3))
      do while( .not.eof(1) )
        read(1,'(i1,i9,36x,i1,3i2 )') isex,id,(iwv(j),j=1,4)
        if( isex.eq.1 ) cycle
        if( iwv(1).eq.2 ) cycle
        if( iwv(1).eq.3 ) jwv(1)=1925+iwv(2)
        if( iwv(1).eq.4 ) jwv(1)=1988+iwv(2)
        if( iwv(1).eq.5 ) jwv(1)=2018+iwv(2)
        do j=1,2
          jwv(j+1)=iwv(j+2)
        end do
        call hash ( id,ip,neqn,count )
        do j=1,3
          hb(ip,j)=jwv(j)
        end do
      end do
      nn=0
      do while( .not.eof(3) )
        read(3,'(i1,i9,i1,3i2)') isex,id,(iwv(j),j=1,4)
        if( iwv(1).eq.3 ) jwv(1)=1925+iwv(2)
        if( iwv(1).eq.4 ) jwv(1)=1988+iwv(2)
        if( iwv(1).eq.5 ) jwv(1)=2018+iwv(2)
        do j=1,2
          jwv(j+1)=iwv(j+2)
        end do
        call calend( ichi,jwv )
        call hash( id,ip,neqn,count )
        do i=1,nn(ip)
          if( idday(ip,i).eq.ichi ) go to 100
        end do
        nn(ip)=nn(ip)+1
        idday(ip,nn(ip))=ichi
        do i=1,3
          jdday(ip,nn(ip),i)=jwv(i)
        end do
100     continue
      end do
      jdop=0
      jglt=0
      kdop=0
      kglt=0
      izero=0
!     kdop=1[前の産次に採卵あり],=2[前の産次にレシピ供す]
!     kglt=1[前の産次に流死産],=2[前の産次にレシピ供す]
      do while( .not.eof(2) )
        read(2,'(i1,i9,i3,4i2,i5,2i3,i11,i5,2i3,i6)')
     +  isex,id,isan,(iwv(j),j=1,4),(cday(j),j=1,3),sire,(sday(j),j=1,3)
     +  ,iglt
        call hash( id,ip,neqn,count )
        if( hb(ip,1).le.1987 ) cycle !1987年生まれ以下は除外
        call calend( ichi1,cday )
        call calend( ichi2,sday )
        call calend( ichi3,hb(ip,:) )
        rage=0.0d0
        eday=0
        iflag=0 !宮崎県口蹄疫対応（前の分娩が2009年12月～2010年６月かつ種付け年月日が2010７月以降のものにフラグを立てる。この時点では県不明
        iciv=0
        idop=0
        if( id.ne.jd ) then
          idop=0
          iciv=0
          jdop=0
          jglt=0
          kdop=0
          kglt=0
          rafc=0.0d0
          nc=0
          iflag=0
          if( iwv(1).eq.0 .or.iwv(1).eq.2 ) then
            do i=1,3
              cday2(i)=cday(i) !初産月齢用分娩年月日
            end do
          else
            do i=1,3
              cday2(i)=0 !初産月齢用分娩年月日
            end do
          end if
          if( sday(1).eq.2010.and.sday(2).eq.7 ) iflag=1
          if( iwv(1).eq.0.or.iwv(1).eq.2 ) then
            do i=1,nn(ip)
              if( idday(ip,i).le.ichi2 ) then !授精日前の採卵あり
                idday(ip,i)=999999999! (この採卵日は用いないclose）
                do j=1,3
                  eday(j)=jdday(ip,i,j) !edayが0でない場合，初産月齢を分析記録としない
c                  iflag=1
                  jdop=1
                end do
                exit
              end if
            end do
          end if
c          if( iwv(1).eq.1 ) then
c            jdop=1
c          end if
          if( iwv(1).eq.2 ) then
            jglt=2
            jdop=2
          end if
          if( iwv(1).eq.0.or.iwv(1).eq.2 ) then !普通分娩またはレシピは産次でカウント
            nc=nc+1
            jchi1=ichi1
            jchi2=ichi2
            rafc=dble(ichi1-ichi3)/(365.0d0/12.0d0)
            rage=dble(ichi1-ichi3)/365.0d0
          end if
          if( iwv(2).ge.2.and.iwv(2).ne.5 ) then
            jglt=1
          endif
c          if( iwv(1).eq.0.and.iwv(2).le.1.and.iflag.eq.0 ) then !自卵かつ正常分娩と早産，採卵がなかった場合は初産月齢を計算
c            rafc=dble(ichi1-ichi3)/(365.0d0/12.0d0)
c            rage=dble(ichi1-ichi3)/365.0d0
c          end if
          write(50,'(i1,i9,i3,4i2,i5,2i3,i11,i5,2i3,3i6,i2,i3,i5,2i3,
     +               2f9.3,2i3,2(i5,2i3))')
     +    isex,id,isan,(iwv(j),j=1,4),(cday(j),j=1,3),sire,
     +    (sday(j),j=1,3),iglt,idop,iciv,iflag,nc,(hb(ip,j),j=1,3),
     +    rage,rafc,kdop,kglt,(eday(j),j=1,3),(cday2(j),j=1,3)
c            if( jdop.ne.0.and.jdop.ne.2 ) then !2013/11/24レシピエントは活き
          if( iwv(1).eq.1 ) then
            rage=0.0d0
            rafc=0.0d0
            if( jglt.eq.1 ) iglt=-999
            if( jglt.eq.1 ) idop=-999
          end if
          if( jglt.eq.1 ) then               !2013/11/24レシピエントは活き
              iglt=-999
              rage=0.0d0
              rafc=0.0d0
          end if
          write(51,'(i1,i9,i3,4i2,i5,2i3,i11,i5,2i3,3i6,i2,i3,i5,2i3,
     +               2f9.3,2i3,2(i5,2i3))')
     +    isex,id,isan,(iwv(j),j=1,4),(cday(j),j=1,3),sire,
     +    (sday(j),j=1,3),iglt,idop,iciv,iflag,nc,(hb(ip,j),j=1,3),
     +    rage,rafc,kdop,kglt,(eday(j),j=1,3),(cday2(j),j=1,3)
          jd=id
          kdop=jdop
          kglt=jglt
        else
          idop=0
          iciv=0
          if( iwv(1).ne.1 ) then
            jdop=0
            jglt=0
          else
            jdop=kdop
            jglt=kglt
            kdop=0
          endif
          iflag=0
          if( cday2(1).eq.2009.and.cday2(2).eq.12.and.sday(1).eq.2010.
     +      and.sday(2).ge.7 ) then
            if( sday(2).eq.7 ) then
              if( sday(3).ge.27 ) then
                iflag=1
              endif
            else
              iflag=1
            endif
          end if
          if( cday2(1).eq.2010.and.cday2(2).eq.1.and.sday(1).eq.2010.
     +      and.sday(2).ge.7 ) then
            if( sday(2).eq.7 ) then
              if( sday(3).ge.27 ) then
                iflag=2
              endif
            else
              iflag=2
            endif
          end if
          if( cday2(1).eq.2010.and.cday2(2).eq.2.and.sday(1).eq.2010.
     +      and.sday(2).ge.7 ) then
            if( sday(2).eq.7 ) then
              if( sday(3).ge.27 ) then
                iflag=3
              endif
            else
              iflag=3
            endif
          end if
          if( cday2(1).eq.2010.and.cday2(2).eq.3.and.sday(1).eq.2010.
     +      and.sday(2).ge.7 ) then
            if( sday(2).eq.7 ) then
              if( sday(3).ge.27 ) then
                iflag=4
              endif
            else
              iflag=4
            endif
          end if
          if( cday2(1).eq.2010.and.cday2(2).eq.4.and.sday(1).eq.2010.
     +      and.sday(2).ge.7 ) then
            if( sday(2).eq.7 ) then
              if( sday(3).ge.27 ) then
                iflag=5
              endif
            else
              iflag=5
            endif
          end if
          if( cday2(1).eq.2010.and.cday2(2).eq.5.and.sday(1).eq.2010.
     +      and.sday(2).ge.7 ) then
            if( sday(2).eq.7 ) then
              if( sday(3).ge.27 ) then
                iflag=6
              endif
            else
              iflag=6
            endif
          end if
          if( cday2(1).eq.2010.and.cday2(2).eq.6.and.sday(1).eq.2010.
     +      and.sday(2).ge.7 ) then
            if( sday(2).eq.7 ) then
              if( sday(3).ge.27 ) then
                iflag=7
              endif
            else
              iflag=7
            endif
          end if
          if( cday2(1).eq.0 ) then
            if( iwv(1).eq.0.or.iwv(1).eq.2 ) then
              do i=1,3
                cday2(i)=cday(i) !初産月齢用分娩年月日
              end do
            else
              do i=1,3
                cday2(i)=0 !初産月齢用分娩年月日
              end do
            end if
          end if
          if( iwv(1).eq.0.or.iwv(1).eq.2 ) then
            do i=1,nn(ip)
              if( idday(ip,i).gt.jchi1.and.idday(ip,i).le.ichi2 ) then !授精日前の採卵あり
                idday(ip,i)=999999999 !close
                do j=1,3
                  eday(j)=jdday(ip,i,j) !edayが0でない場合，初産月齢を分析記録としない
c                  iflag=1
c                  idop=-999 !採卵があっても分析する2013/11/25
                  jdop=1
                end do
                exit
              end if
            end do
          end if
c          if( iwv(1).eq.1 ) then
c            jdop=1
c          end if
          if( iwv(1).eq.2 ) then
            jdop=2
            jglt=2
          end if
          if( iwv(2).ge.2.and.iwv(2).ne.5 ) then
            jglt=1
          endif
c          if( iwv(1).eq.0.and.iwv(2).le.1.and.iflag.eq.0 ) then !自卵かつ正常分娩と早産，採卵がなかった場合
c            rage=dble(ichi1-ichi3)/365.0d0
c            iciv=ichi1-jchi1
c            idop=ichi2-jchi1
c          end if
          if( iwv(1).eq.0.or.iwv(1).eq.2 ) then !普通分娩またはレシピは産次でカウント
            nc=nc+1
            rage=dble(ichi1-ichi3)/365.0d0
            if( nc.ge.2 ) then
              iciv=ichi1-jchi1
              idop=ichi2-jchi1
            end if
            jchi1=ichi1
            jchi2=ichi2
            write(50,'(i1,i9,i3,4i2,i5,2i3,i11,i5,2i3,3i6,i2,i3,i5,2i3,
     +                 2f9.3,2i3,2(i5,2i3))')
     +      isex,id,isan,(iwv(j),j=1,4),(cday(j),j=1,3),sire,
     +      (sday(j),j=1,3),iglt,idop,iciv,iflag,nc,(hb(ip,j),j=1,3),
     +      rage,rafc,kdop,kglt,(eday(j),j=1,3),(cday2(j),j=1,3)
c            if( jdop.ne.0.and.jdop.ne.2 ) then !2013/11/24レシピエントは活き
            if( iwv(1).eq.1 ) then
              iciv=-999
              idop=-999
              rage=0.0d0
            end if
            if( jglt.eq.1 ) then               !2013/11/24レシピエントは活き
              iglt=-999
              iciv=-999
              rage=0.0d0
            end if
            write(51,'(i1,i9,i3,4i2,i5,2i3,i11,i5,2i3,3i6,i2,i3,i5,2i3,
     +                 2f9.3,2i3,2(i5,2i3))')
     +      isex,id,isan,(iwv(j),j=1,4),(cday(j),j=1,3),sire,
     +      (sday(j),j=1,3),iglt,idop,iciv,iflag,nc,(hb(ip,j),j=1,3),
     +      rage,rafc,kdop,kglt,(eday(j),j=1,3),(cday2(j),j=1,3)
          else
            write(50,'(i1,i9,i3,4i2,i5,2i3,i11,i5,2i3,3i6,i2,i3,i5,2i3,
     +               2f9.3,2i3,2(i5,2i3))')
     +    isex,id,isan,(iwv(j),j=1,4),(cday(j),j=1,3),sire,
     +    (sday(j),j=1,3),iglt,idop,iciv,iflag,izero,(hb(ip,j),j=1,3),  !izero
     +    rage,rafc,kdop,kglt,(eday(j),j=1,3),(cday2(j),j=1,3)
c            if( jdop.ne.0.and.jdop.ne.2 ) then !2013/11/24レシピエントは活き
            if( iwv(1).eq.1 ) then
              iciv=-999
              idop=-999
              rage=0.0d0
            end if
            if( jglt.eq.1 ) then
              iglt=-999
              iciv=-999
              rage=0.0d0
            end if
            write(51,'(i1,i9,i3,4i2,i5,2i3,i11,i5,2i3,3i6,i2,i3,i5,2i3,
     +                 2f9.3,2i3,2(i5,2i3))')
     +      isex,id,isan,(iwv(j),j=1,4),(cday(j),j=1,3),sire,
     +      (sday(j),j=1,3),iglt,idop,iciv,iflag,izero,(hb(ip,j),j=1,3),
     +      rage,rafc,kdop,kglt,(eday(j),j=1,3),(cday2(j),j=1,3)
          end if
          jd=id
          kdop=jdop
          if( iwv(1).eq.0.or.iwv(1).eq.2 ) kglt=jglt
          if(iwv(1).eq.0.or.iwv(1).eq.2 ) then
            do i=1,3
              cday2(i)=cday(i) !前の分娩年
            end  do
          end if
        end if
      end do
      rewind(50)
!全ての妊娠期間，空胎期間，分娩間隔
!流死産の妊娠期間と次の空胎期間
!採卵のあった空胎期間と次の空胎期間
!レシピに供した空胎，妊娠とその次の妊娠期間
      do while( .not.eof(50) )
        read(50,'(i1,i9,i3,4i2,i5,2i3,i11,i5,2i3,3i6,i2,i3,i5,2i3,
     +               2f9.3,2i3,2(i5,2i3))')
     +    isex,id,isan,(iwv(j),j=1,4),(cday(j),j=1,3),sire,
     +    (sday(j),j=1,3),iglt,idop,iciv,iflag,nc,(bday(j),j=1,3),  !izero
     +    rage,rafc,kdop,kglt,(eday(j),j=1,3),(cday2(j),j=1,3)
        if( iglt.gt.0 ) then
          n(1)=n(1)+1
          ave(1)=ave(1)+dble(iglt)
          sd(1)=sd(1)+dble(iglt)**2
          if( iwv(2).eq.0 ) then
            n(4)=n(4)+1
            ave(4)=ave(4)+dble(iglt)
            sd(4)=sd(4)+dble(iglt)**2
          else
            if( iwv(2).eq.2.or.iwv(2).eq.3 ) then
            n(5)=n(5)+1
            ave(5)=ave(5)+dble(iglt)
            sd(5)=sd(5)+dble(iglt)**2
            end if
          end if
          if( iwv(1).eq.2 ) then
            n(10)=n(10)+1
            ave(10)=ave(10)+dble(iglt)
            sd(10)=sd(10)+dble(iglt)**2
          end if
        end if
        if( idop.gt.0 ) then
          n(2)=n(2)+1
          ave(2)=ave(2)+dble(idop)
          sd(2)=sd(2)+dble(idop)**2
          if( kglt.eq.1 ) then
            n(6)=n(6)+1
            ave(6)=ave(6)+dble(idop)
            sd(6)=sd(6)+dble(idop)**2
          end if
          if( eday(1).ne.0 ) then
            n(7)=n(7)+1
            ave(7)=ave(7)+dble(idop)
            sd(7)=sd(7)+dble(idop)**2
          end if
          if( kdop.eq.1 ) then
            n(8)=n(8)+1
            ave(8)=ave(8)+dble(idop)
            sd(8)=sd(8)+dble(idop)**2
          end if
          if( iwv(1).eq.2 ) then
            n(9)=n(9)+1
            ave(9)=ave(9)+dble(idop)
            sd(9)=sd(9)+dble(idop)**2
          end if
          if( kdop.eq.2 ) then
            n(11)=n(11)+1
            ave(11)=ave(11)+dble(idop)
            sd(11)=sd(11)+dble(idop)**2
          end if
        end if
        if( iciv.gt.0 ) then
          n(3)=n(3)+1
          ave(3)=ave(3)+dble(iciv)
          sd(3)=sd(3)+dble(iciv)**2
        end if
      end do
      tr(1)='all_glt'
      tr(2)='all_dop'
      tr(3)='all_civ'
      tr(4)='dead0_glt'
      tr(5)='dead23_glt'
      tr(6)='deadnext_dop'
      tr(7)='getegg_dop'
      tr(8)='gegnext_dop'
      tr(9)='recip_dop'
      tr(10)='recip_glt'
      tr(11)='rpnext_dop'
      open(10,file='stat_nocut.txt')
      open(11,file='stat_cut.txt')
      do i=1,11
        sd(i)=sqrt((sd(i)-ave(i)**2/dble(n(i)))/dble(n(i)-1))
        ave(i)=ave(i)/dble(n(i))
        write(10,'(a15,2f10.3,i8)') tr(i),ave(i),sd(i),n(i)
      end do
      ave=0
      sd=0
      n=0
      rewind(51)
      write(53,'(12a8)')
     +'id',',ET',',dead',',twin',',sex',',glt',',dop',',civ',
     +',afc',',nextdop',',nextglt',',embryo'
      do while( .not.eof(51) )
        read(51,'(i1,i9,i3,4i2,i5,2i3,i11,i5,2i3,3i6,i2,i3,i5,2i3,
     +               2f9.3,2i3,2(i5,2i3))')
     +    isex,id,isan,(iwv(j),j=1,4),(cday(j),j=1,3),sire,
     +    (sday(j),j=1,3),iglt,idop,iciv,iflag,nc,(bday(j),j=1,3),  !izero
     +    rage,rafc,kdop,kglt,(eday(j),j=1,3),(cday2(j),j=1,3)
        if( iglt.gt.0 ) then
          n(1)=n(1)+1
          ave(1)=ave(1)+dble(iglt)
          sd(1)=sd(1)+dble(iglt)**2
          if( iwv(2).eq.0 ) then
            n(4)=n(4)+1
            ave(4)=ave(4)+dble(iglt)
            sd(4)=sd(4)+dble(iglt)**2
          else
            if( iwv(2).eq.2.or.iwv(2).eq.3 ) then
            n(5)=n(5)+1
            ave(5)=ave(5)+dble(iglt)
            sd(5)=sd(5)+dble(iglt)**2
            end if
          end if
          if( iwv(1).eq.2 ) then
            n(10)=n(10)+1
            ave(10)=ave(10)+dble(iglt)
            sd(10)=sd(10)+dble(iglt)**2
          end if
        end if
        if( idop.gt.0 ) then
          n(2)=n(2)+1
          ave(2)=ave(2)+dble(idop)
          sd(2)=sd(2)+dble(idop)**2
          if( kglt.eq.1 ) then
            n(6)=n(6)+1
            ave(6)=ave(6)+dble(idop)
            sd(6)=sd(6)+dble(idop)**2
          end if
          if( eday(1).ne.0 ) then
            n(7)=n(7)+1
            ave(7)=ave(7)+dble(idop)
            sd(7)=sd(7)+dble(idop)**2
          end if
          if( kdop.eq.1 ) then
            n(8)=n(8)+1
            ave(8)=ave(8)+dble(idop)
            sd(8)=sd(8)+dble(idop)**2
          end if
          if( iwv(1).eq.2 ) then
            n(9)=n(9)+1
            ave(9)=ave(9)+dble(idop)
            sd(9)=sd(9)+dble(idop)**2
          end if
          if( kdop.eq.2 ) then
            n(11)=n(11)+1
            ave(11)=ave(11)+dble(idop)
            sd(11)=sd(11)+dble(idop)**2
          end if
        end if
        if( iciv.gt.0 ) then
          n(3)=n(3)+1
          ave(3)=ave(3)+dble(iciv)
          sd(3)=sd(3)+dble(iciv)**2
        end if
        if( nc.eq.1.and.rafc.ge.37.0d0 ) cycle
        if( nc.eq.1.and.rafc.lt.18.0d0 ) cycle
        if( iglt.lt.260.or.iglt.ge.311 ) cycle
        if( nc.ge.2 ) then
          if( iciv.le.275.or.iciv.ge.731 ) cycle
        end if
        if( iciv.lt.0.or.iglt.lt.0.or.idop.lt.0 ) cycle
        !dop保留
        write(52,'(i1,i9,i3,4i2,i5,2i3,i11,i5,2i3,3i6,i2,i3,i5,2i3,
     +               2f8.3,2i3,2(i5,2i3))')
     +    isex,id,isan,(iwv(j),j=1,4),(cday(j),j=1,3),sire,
     +    (sday(j),j=1,3),iglt,idop,iciv,iflag,nc,(bday(j),j=1,3),  
     +    rage,rafc,kdop,kglt,(eday(j),j=1,3),(cday2(j),j=1,3)
        write(53,'(i10,4(a1,i2),3(a1,i5),a,f10.3,2(a1,i2),a,i4)')
     +   id,((cv,iwv(j)),j=1,4),cv,iglt,cv,idop,cv,iciv,cv,rafc
     +  ,cv,kdop,cv,kglt,cv,eday(1)
      end do
      do i=1,11
        sd(i)=sqrt((sd(i)-ave(i)**2/dble(n(i)))/dble(n(i)-1))
        ave(i)=ave(i)/dble(n(i))
        write(11,'(a15,2f10.3,i8)') tr(i),ave(i),sd(i),n(i)
      end do
      
      stop
      end
c----------------------------------------------------------------------
c
      SUBROUTINE HASH(K,IP,NEQN,COUNT)
c
c-----------------------------------------------------------------------
c      PARAMETER(M=31,M1=17) !origin
       PARAMETER(M=10000000,M1=9999991) !素数

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
