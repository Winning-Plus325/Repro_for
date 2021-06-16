c     県別のＡＢＣindexを作成する!子牛生産指数のデータを追加
C     ８歳時期待産子数の計算
c     ８歳時産子数とは現所有者が育成時から８歳まで飼養した時に期待される産子数
c     初産月齢および分娩間隔が実際の記録を持って分析された牛のみ計算する
      implicit none
      integer,parameter :: hashn=10000000
      integer :: isex,id,ibir,isire,idam,jc,ilast,iw1,ip,neqn,count,iw2,
     + na(47,2),nk,i,j,iwv(3),ip2,ip3,nc,jlast,iw(4),is(3),ip4,ic,k,if8
      integer :: iciv,iym,nut,iip(50),nf,nnp,ifc(500000),idbs,iyy
      integer,allocatable :: ireal(:),iped(:,:),nn(:),iflag(:),np(:),
     + iflag2(:),ikigo(:,:),ib(:,:),irecF(:,:),jflag(:),ncp(:)
      real(8) :: rwv(15),rafc,rciv,ave(47,2),sd(47,2),rmin(47,2),
     +rmax(47,2),ave2(47,2),sg(2),rwv2(2),rw1,pebv,rm,rn,rbv2(2),rac2(2)
      real(8),allocatable :: rac(:,:),rbv(:,:),rd(:,:),rv(:),pe(:),
     +crec(:),bc(:),sg2(:,:),rcage(:),rk8(:)
      character :: aa16*16,aa62*62,aa15*15,aa20*20,aj10*10,ap(47)*2,
     +aa2*2,bb2*2,ABC(2)*3,dfile*30,bb24*24,aaa*20,anum(10)*15,
     +kfile*20,bfile*20,anen*2,akai*1,abun*3,aa45*45,aa27*27,bken*2,
     +app(50)*2,fmc(500000)*14,abs*16,aa7*7
      character,allocatable :: aken(:)*2,kigo(:,:)*2,name(:)*16
      real(8) :: avea(2),avek(2),aveh(2),
     + aves(2),aver(2),aven(2),sda(2),sdk(2),sdh(2),sds(2),sdr(2),sdn(2)
     +,rmina(2),rmink(2),rminh(2),rmins(2),rminr(2),rminn(2),
     +rmaxa(2),rmaxk(2),rmaxh(2),rmaxs(2),rmaxr(2),rmaxn(2),
     +avea2(2),avek2(2),aveh2(2),aves2(2),aver2(2),aven2(2),rk(3),age
      real(8) :: rua,ruc,r8,rf(2),rfarm(500000,2),rka,rkc,rage
      integer :: naa(2),nak(2),nah(2),nas(2),nar(2),nan(2),ibb(4)
      allocate( ireal(hashn),iped(hashn,2),nn(hashn),iflag(hashn),
     + rac(hashn,2),rbv(hashn,2),rd(hashn,2),aken(hashn),kigo(hashn,2),
     + rv(hashn),np(hashn),pe(hashn),iflag2(hashn),ikigo(hashn,2),
     + ib(hashn,3),name(hashn),crec(hashn),bc(hashn),sg2(hashn,2),
     +  irecF(hashn,2),jflag(hashn),ncp(hashn),rcage(hashn),rk8(hashn) )
      open(2222,file='test_aomori.txt')
      open(1,file='bv_acc.dat',status='old')
      open(11,file='bv_acc.tmp')
      open(999,file='tmp.tmp')
      open(2,file='bvinf',status='old') !本番は平成元年以降生まれの雌登録牛のbvinfを作成する
      write(*,*) '分析ファイル名は？'
      read(*,*) dfile
      open(3,file=dfile,status='old')
      open(4,file='C:\data\katsu\inbred\近交係数\ketof',status='old')
      open(5,file='life1.dat',status='old')
      open(6,file='pe.dat',status='old')
      open(7,file='nocutREC.dat',status='old')
      open(8,file='kihoncombo',status='old')
      open(9,file='outlaw.txt')
      open(10,file='KENbvstat.txt')
      open(111,file='fmc2add_re2_sort.dat',status='old')
      open(112,file='fm15_code_effect')
      open(12,file='effect_summary2.txt',status='old')
c      open(13,file='kotomsa_c8.txt',status='old')!８歳時子牛数のファイルを作成しておく
      open(13,file='Kc8.txt',status='old') !nocutmakeC8.exeを実行
c      open(50,file='BVafccivALL39.dat')
c      open(51,file='BVafccivALLEX.dat')
      open(60,file='stat_MESU.txt')
      open(101,file='bv_mesuALL.dat')
      open(102,file='bv_osuALL.dat')
      open(401,file='HANBVF')
      open(402,file='HANSTF')
      !全平均の取得
      read(12,*) aaa,rua,ruc
      write(*,*) rua,ruc
      write(*,*) '分娩間隔の記録は何列目に入っていますか？'
      read(*,*) nc
      write(*,*) '最終分娩年月日は？'
      read(*,*) jlast
      write(*,*) 'トレサ情報を考慮して供用中判定を行うか？YES=>=1,NO=>2'
      read(*,*) iyy
      write(*,*) '子牛生産指数のデータファイルは？'
      read(*,*) kfile
      open(21,file=kfile,status='old')
      write(*,*) '子牛生産指数の育種価ファイルは？'
      read(*,*) bfile
      open(22,file=bfile,status='old')
      write(*,*) '子牛生産指数の全平均は？（書かない場合は0.000）'
      read(*,*) rm
      write(*,*) 
     +'子牛生産指数のドナー効果のノーマルの値は？(書かない場合は0.000)'
      read(*,*) rn
      write(*,*) '更新用ファイルもここで作ります'
      write(*,*) '評価年は？（西暦の２桁：ex 2014年=>14)'
      read(*,*) Anen
      write(*,*) '評価回数は？'
      read(*,*) Akai
      Abun=Anen//Akai
      do i=1,3
        if( Abun(i:i).eq.' ') Abun(i:i)='0'
      end do
      do i=1,47
        write(99,'(i2)') i
        backspace(99)
        read(99,'(a2)') ap(i)
        if(ap(i)(1:1).eq.' ' ) ap(i)(1:1)='0'
        open(200+i,file='Bvafcciv_add'//ap(i)//'.dat')
      end do
      write(*,*) '審査年月日の制限は(2015年４月分析ex:201209)'
      read(*,*) iym
      jflag=0
      write(9876,'(i7)') jlast
      rewind(9876)
      read(9876,'(i1,3i2)') (iw(j),j=1,4)
      call wasei(iw,is)
      call calend(iw2,is)
      do while( .not.eof(8) )
        read(8,'(i10,147x,i1,x,i1,i2,i2)') id,nut,(iwv(j),j=1,3)
        call hash( id,ip,neqn,count )
        if(iwv(1).eq.4 ) iw1=(1988+iwv(2))*100+iwv(3)
        if(iwv(1).eq.5 ) iw1=(2018+iwv(2))*100+iwv(3)
        if( nut.ge.2.and.nut.le.9.and.iw1.le.iym) then
          jflag(ip)=1
        end if
      end do
      rk8=0.0d0
      do while( .not.eof(13) )
c        read(13,'(13x,i9,93x,f10.3)') id,rw1
        read(13,'(i10,66x,f10.3)')id,rw1
        call hash( id,ip,neqn,count )
        rk8(ip)=rw1 !８歳時子牛数
      end do
      irecF=0
      rcage=0.0d0
      ncp=0.0d0
      do while( .not.eof(7) )
        read(7,'(x,i9,56x,i6,2x,i3,11x,f9.3,f9.3)') id,iciv,iw1,rage,rw1
        call hash( id,ip,neqn,count )
        if( rk8(ip).eq.0.and.iw1.ne.0.and.iw1.le.8.and.rage.lt.8.0)then
          rcage(ip)=rage !直近の産次の年齢
          ncp(ip)=iw1 !直近の産次
        end if
        if( iw1.ge.2.and.iciv.ne.0.and.jflag(ip).eq.1 ) then
          irecF(ip,2)=1
        end if
        if( jflag(ip).eq.1 ) then
          irecF(ip,1)=1
          if( rw1.eq.0.0d0 ) irecF(ip,1)=0 !初産が流死産およびETに供したものは初産の記録はそもそも無かったことにする
        end if
      end do
      bken='00'
      nf=0
      nnp=0
      do while( .not.eof(111) )
        nf=nf+1
        read(111,'( a14,55x,i8 )') fmc(nf),ifc(nf)
        iw1=(ifc(nf)-mod(ifc(nf),100000))/100000
        if(mod(iw1,10).ne.1 ) then
          nf=nf-1
          cycle
        end if
        if( fmc(nf)(1:2).ne.bken ) then
          nnp=nnp+1
          app(nnp)=fmc(nf)(1:2)
          iip(nnp)=nf
          bken=fmc(nf)(1:2)
          write(*,*) bken,iip(nnp),fmc(nf),ifc(nf)
        end if
      end do
      nnp=nnp+1
      iip(nnp)=nf+1
      rfarm=0
      do while( .not.eof(12) )
        read(12,'(a20,i3,i10,2f15.5,f10.3,i8,f10.3,i8)') 
     +   aaa,(iwv(j),j=1,2),(rwv(j),j=1,2)
        write(3333,*) aaa(1:4)
        if(aaa(1:4).ne.'Farm') cycle
        iw1=(iwv(2)-mod(iwv(2),100000))/100000
c        if( iw1.eq.111 ) write(*,*)iwv(2)
        if(mod(iw1,10).eq.1 ) then
          iw1=(iw1-mod(iw1,10))/10
c          if( iw1.eq.11 ) write(*,*) '*',iwv(2),ap(iw1)
          do i=1,nnp
            if( app(i).eq.ap(iw1) ) then
c              if( ap(iw1).eq.'11') write(*,*) i !
              do j=iip(i),iip(i+1)-1
                if(ifc(j).eq.iwv(2) ) then
c                   if( app(i).eq.'11') write(*,*) ifc(j),j !
               write(112,'(a14,i9,2f10.3)') fmc(j),ifc(j),(rwv(k),k=1,2)
c                   write(*,*) '*' !
                   do k=1,2
                     rfarm(j,k)=rwv(k) !14桁の農家コードと対応した効果を取得
                   end do
                end if
              end do
            end if
          end do
        end if
      end do
      crec=0.0d0
      bc=0.0d0
      do while( .not.eof(21) )
        read(21,'(i11,58x,f8.3)') id,rw1
        if( rw1.eq.0.0d0 ) cycle
        call hash( id,ip,neqn,count )
        crec(ip)=rw1
      end do
      read(22,*) aaa
      do while( .not.eof(22) )
        read(22,'(i11,f10.4)') id,rw1
        rw1=rw1+rm+rn
        call hash( id,ip,neqn,count )
        bc(ip)=rw1
      end do
      nn=0
      rd=0
      open(248,file='Bvafcciv48.dat')
      anum(1)='035100625000040'
      anum(2)='100106000010970'
      anum(3)='430300100030630'
      anum(4)='430500100046530'
      anum(5)='330513400423280'
      ireal=0
      do while( .not.eof(2) )
        read(2,'(i1,i9,200x,a2,36x,i7,18x,a7)') isex,id,aa2,iw1,aa7
        if( isex.eq.1 ) id=1000000000+id
        call hash(id,ip,neqn,count )
        aken(ip)=aa2
        if( iw1.ge.jlast.and.isex.eq.2 ) then
          if( iyy.ne.1 )then
            ireal(ip)=1
          else
            if( aa7.ge.'4140000') then
              ireal(ip)=0
            else
              ireal(ip)=1
            end if
          end if
        end if
      end do
      rewind(2)
      do while( .not.eof(5) )
        read(5,'(13x,i9,73x,f7.1,27x,i3)') id,rwv(1),iwv(1)
        call hash(id,ip,neqn,count )
        rv(ip)=rwv(1)
        np(ip)=iwv(1)
      end do
      name='　　　　　　　　'
      do while( .not.eof(4) )
        read(4,
     +'(i1,i9,16x,a16,4x,i7,i10,x,i9,44x,a2,13x,a2,13x,14x,i7,8x,a7)') 
     +isex,id,aa16,ibir,isire,idam,aa2,bb2,iw1,aa7
        if( isex.eq.1 ) id=1000000000+id
        call hash(id,ip,neqn,count )
        iped(ip,1)=isire
        iped(ip,2)=idam
        if( bb2.ne.'00') aa2=bb2
        aken(ip)=aa2
c        if( iw1.ge.jlast.and.isex.eq.2 ) then !bvinfへ
c          if( iyy.ne.1 )then
c            ireal(ip)=1
c          else
c            if( aa7.ge.'4140000') then
c              ireal(ip)=0
c            else
c              ireal(ip)=1
c            end if
c          end if
c        end if
        iw(1)=(ibir-mod(ibir,1000000))/1000000
        iw(2)=ibir-iw(1)*1000000
        iw(2)=(iw(2)-mod(iw(2),10000))/10000
        iw(3)=ibir-(iw(1)*1000000+iw(2)*10000)
        iw(3)=(iw(3)-mod(iw(3),100))/100
        iw(4)=ibir-(iw(1)*1000000+iw(2)*10000+iw(3)*100)
        call wasei( iw,is )
        do j=1,3
          ib(ip,j)=is(j)
        end do
        name(ip)=aa16
      end do
      do while( .not.eof(3) )
        read(3,'(3i11,6i10,2f11.3,2f10.4)') 
     +   id,(rwv(j),j=2,nc-2),rafc,rciv
        call hash( id,ip,neqn,count)
        if( rafc.gt.0 ) then
          rd(ip,1)=rafc
        end if
        if( rciv.gt.0 ) then
          nn(ip)=nn(ip)+1
          rd(ip,2)=rd(ip,2)+rciv
        end if
      end do
      pe=-999.999d0
      do while( .not.eof(6) )
        read(6,'(a20,i3,i10,2f15.5,2i5)') aaa,iw1,id,rw1,rwv(1)
        call hash( id,ip,neqn,count )
        pe(ip)=rwv(1)
      end do
      rbv=-999.999d0
      rac=-999.999d0
      iflag=0
      iflag2=0
      ic=3
      do while( .not.eof(1) )
        read(1,'(2i11,4f10.4,2i8)')
     +   id,iw1,((rwv(j),rwv2(j)),j=1,2),(iwv(j),j=1,2)
        do j=1,2
c          call rounda2(rwv(j),ic) !小数点第４位を四捨五入して小数点第３位まであつかう
c          call rounda2(rwv2(j),ic)!
           call rounda(rwv(j))
           call rounda(rwv2(j))
        end do
        write(11,'(2i11,4f10.3,2i8)')
     +   id,iw1,((rwv(j),rwv2(j)),j=1,2),(iwv(j),j=1,2)
      end do
      rewind(11)
      do while( .not.eof(11))
c        read(11,'(2i11,4f10.4,2i8)') id,iw1,((rwv(j),rwv2(j)),j=1,2),(iwv(j),j=1,2) !del !2014 /06 /01 INSERT
        read(11,'(2i11,4f10.3,2i8)') 
     +   id,iw1,((rwv(j),rwv2(j)),j=1,2),(iwv(j),j=1,2) !2014 /06 /01 INSERT
        call hash( id,ip,neqn,count )
        call hash( iped(ip,1),ip2,neqn,count )
        call hash( iped(ip,2),ip3,neqn,count )
        if( nn(ip).ne.0 ) then
          rd(ip,2)=rd(ip,2)/nn(ip)
        end if
        do j=1,2
c          call rounda(rwv(j))
c          write(999,'(f10.3)') rwv(j)
c          backspace(999)
c          read(999,'(f10.3)') rwv(j)
c          call rounda(rwv2(j))
          rbv(ip,j)=rwv(j)
          rac(ip,j)=rwv2(j)
        end do
        if( iwv(1).ne.0 ) iflag2(ip2)=iflag2(ip2)+1
        if( iwv(2).ne.0 ) iflag(ip2)=iflag(ip2)+1 !記録のある後代がいますフラグを父に立てる
        iflag(ip3)=1 !後代がいますフラグを母にたてる=>後代がいますフラグの立っていない記録なしの個体は期待育種価扱いにする。
      end do
      rewind(11)
      kigo='　'
      na=0
      ave=0
      sd=0
      ave2=0
      naa=0
      avea=0.0d0
      avea2=0.0d0
      sda=0.0d0
      avek=0.0d0
      avek2=0.0d0
      sdk=0.0d0
      nak=0
      nah=0
      nas=0
      aves=0.0d0
      aves2=0.0d0
      sds=0.0d0
      nar=0
      aver=0.0d0
      aver2=0.0d0
      sdr=0.0d0
      nan=0
      aven=0.0d0
      aven2=0.0d0
      sdn=0.0d0
      ikigo=0
      do while( .not.eof(11) )
        read(11,'(2i11,4f10.4,2i8)') 
     +   id,iw1,((rwv(j),rwv2(j)),j=1,2),(iwv(j),j=1,2)
        call hash( id,ip,neqn,count )
　      do j=1,2!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!記録を持つ後代のいないものを期待育種価にしていたが201910評価よりこのルーチンを削除した。しかし、このルーチンは202110より復活させる。
          if( iflag(ip).eq.0.and.iwv(j).eq.0 ) then
            kigo(ip,j)='期'
            ikigo(ip,j)=2
            write(99,'(i10)') id
          else
            ikigo(ip,j)=1
          end if!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!記録を持つ後代のいないものを期待育種価にしていたが201910評価よりこのルーチンを削除。しかし、このルーチンは202110より復活させる。
        end do
        if( id.lt.1000000000 ) then
          write(101,'(i11,4f10.4,2i3,2i7,i5,2i3)')
     +         id,((rwv(j),rwv2(j)),j=1,2),
     +         ((ireal(ip)*10+ikigo(ip,j)),j=1,2),(iwv(j),j=1,2),
     +         (ib(ip,j),j=1,3)
          
        else
          write(102,'(i11,4f10.4,2i3,2i7,i5,2i3)')
     +         id,((rwv(j),rwv2(j)),j=1,2),
     +         ((ireal(ip)*10+ikigo(ip,j)),j=1,2),iflag2(ip),iflag(ip),
     +         (ib(ip,j),j=1,3)
        end if
        if( ireal(ip).eq.1) then
          do i=1,47
            if( ap(i).eq.aken(ip) ) then
              do j=1,2
                if( kigo(ip,j).ne.'期' ) then
                  na(i,j)=na(i,j)+1
                  if( i.eq.2.and.j.eq.2) write(2222,'(i10)') id
                  if( na(i,j).eq.1 ) then
                    rmin(i,j)=rwv(j)
                    rmax(i,j)=rwv(j)
                  end if
                  if( rwv(j).lt.rmin(i,j)) rmin(i,j)=rwv(j)
                  if( rwv(j).gt.rmax(i,j)) rmax(i,j)=rwv(j)
                  ave(i,j)=ave(i,j)+rwv(j)
                  sd(i,j)=sd(i,j)+rwv(j)**2
                  ave2(i,j)=ave2(i,j)+rwv2(j)
                  nak(j)=nak(j)+1
                  avek(j)=avek(j)+rwv(j)
                  avek2(j)=avek2(j)+rwv2(j)
                  sdk(j)=sdk(j)+rwv(j)**2
                  if( nak(j).eq.1 ) then
                    rmink(j)=rwv(j)
                    rmaxk(j)=rwv(j)
                  else
                    if( rwv(j).lt.rmink(j) ) rmink(j)=rwv(j)
                    if( rwv(j).gt.rmaxk(j) ) rmaxk(j)=rwv(j)
                  end if
                  naa(j)=naa(j)+1
                  avea(j)=avea(j)+rwv(j)
                  avea2(j)=avea2(j)+rwv2(j)
                  sda(j)=sda(j)+rwv(j)**2
                  if( naa(j).eq.1 ) then
                    rmina(j)=rwv(j)
                    rmaxa(j)=rwv(j)
                  else
                    if( rwv(j).lt.rmina(j) ) rmina(j)=rwv(j)
                    if( rwv(j).gt.rmaxa(j) ) rmaxa(j)=rwv(j)
                  end if
                end if
              end do
            end if
          end do
        else
          do j=1,2
          if( id.lt.1000000000) then
            if( kigo(ip,j).ne.'期' ) then
              nah(j)=nah(j)+1
              aveh(j)=aveh(j)+rwv(j)
              aveh2(j)=aveh2(j)+rwv2(j)
              sdh(j)=sdh(j)+rwv(j)**2
              if( nak(j).eq.1 ) then
                rminh(j)=rwv(j)
                rmaxh(j)=rwv(j)
              else
                if( rwv(j).lt.rminh(j) ) rminh(j)=rwv(j)
                if( rwv(j).gt.rmaxh(j) ) rmaxh(j)=rwv(j)
              end if
              naa(j)=naa(j)+1
              avea(j)=avea(j)+rwv(j)
              avea2(j)=avea2(j)+rwv2(j)
              sda(j)=sda(j)+rwv(j)**2
              if( naa(j).eq.1 ) then
                rmina(j)=rwv(j)
                rmaxa(j)=rwv(j)
              else
                if( rwv(j).lt.rmina(j) ) rmina(j)=rwv(j)
                if( rwv(j).gt.rmaxa(j) ) rmaxa(j)=rwv(j)
              end if
            end if
          else
            nas(j)=nas(j)+1
            aves(j)=aves(j)+rwv(j)
            aves2(j)=aves2(j)+rwv2(j)
            sds(j)=sds(j)+rwv(j)**2
            if( nas(j).eq.1 ) then
              rmins(j)=rwv(j)
              rmaxs(j)=rwv(j)
            else
              if( rwv(j).lt.rmins(j) ) rmins(j)=rwv(j)
              if( rwv(j).gt.rmaxs(j) ) rmaxs(j)=rwv(j)
            end if
            if( iflag(ip).ne.0 ) then
              nar(j)=nar(j)+1
              aver(j)=aver(j)+rwv(j)
              aver2(j)=aver2(j)+rwv2(j)
              sdr(j)=sdr(j)+rwv(j)**2
              if( nar(j).eq.1 ) then
                rminr(j)=rwv(j)
                rmaxr(j)=rwv(j)
              else
                if( rwv(j).lt.rminr(j) ) rminr(j)=rwv(j)
                if( rwv(j).gt.rmaxr(j) ) rmaxr(j)=rwv(j)
              end if
            else
              nan(j)=nan(j)+1
              aven(j)=aven(j)+rwv(j)
              aven2(j)=aven2(j)+rwv2(j)
              sdn(j)=sdn(j)+rwv(j)**2
              if( nar(j).eq.1 ) then
                rminn(j)=rwv(j)
                rmaxn(j)=rwv(j)
              else
                if( rwv(j).lt.rminn(j) ) rminn(j)=rwv(j)
                if( rwv(j).gt.rmaxn(j) ) rmaxn(j)=rwv(j)
              end if
            end if
          end if
          end do
        end if
      end do
      do i=1,47
        do j=1,2
          if( na(i,j).ge.2 ) then
            sd(i,j)=
     +   dsqrt((sd(i,j)-ave(i,j)**2/dble(na(i,j)))/dble(na(i,j)-1))
            ave(i,j)=ave(i,j)/dble(na(i,j))
            ave2(i,j)=ave2(i,j)/dble(na(i,j)) !正確度の平均
          else
            sd(i,j)=0
          end if
c          call rounda2(ave(i,j),ic)
c          rewind(999)
c          write(999,'(f10.3)') ave(i,j)
c          rewind(999)
c          read(999,'(f10.3)') ave(i,j)
          call rounda(ave(i,j))
          rewind(999)                                                   !2014 /06 /01 INSERT
          write(999,'(f10.3)') ave(i,j)                                 !2014 /06 /01 INSERT
          rewind(999)                                                   !2014 /06 /01 INSERT
          read(999,'(f10.3)') ave(i,j)                                  !2014 /06 /01 INSERT
c          call rounda2(sd(i,j),ic)
c          rewind(999)
c          write(999,'(f10.3)') sd(i,j)
c          rewind(999)
c          read(999,'(f10.3)') sd(i,j)
          call rounda(sd(i,j))
          rewind(999)                                                   !2014 /06 /01 INSERT
          write(999,'(f10.3)') sd(i,j)                                  !2014 /06 /01 INSERT
          rewind(999)                                                   !2014 /06 /01 INSERT
          read(999,'(f10.3)') sd(i,j)                                   !2014 /06 /01 INSERT
        end do
        write(10,'(i2,2(4f10.4,i7),2f10.4)') i,
     + ((ave(i,j),sd(i,j),rmin(i,j),rmax(i,j),na(i,j)),j=1,2),
     + (ave2(i,j),j=1,2)
        write(902,'(a2,a3,2(i6,i5))') ap(i),abun,
     +   ((int(ave(i,j)*1000.0d0),int(sd(i,j)*1000.0d0)),j=2,1,-1)
        backspace(902)
        read(902,'(a27)') aa27
        if( aa27(10:10).eq.'-') then
          aa27(6:10)='-0000'
        else
          if( aa27(9:9).eq.'-') then
            aa27(6:9)='-000'
          else
            if( aa27(8:8).eq.'-') then
              aa27(6:8)='-00'
            else
              if( aa27(7:7).eq.'-') then
                aa27(6:7)='-0'
              else
                if( aa27(6:6).ne.'-' ) aa27(6:6)=' '
                do j=7,10
                  if(aa27(j:j).eq.' ') aa27(j:j)='0'
                end do
              end if
            end if
          end if
        end if
        do j=12,15
          if( aa27(j:j).eq.' ' ) aa27(j:j)='0'
        end do
        if( aa27(21:21).eq.'-') then
          aa27(17:21)='-0000'
        else
          if( aa27(20:20).eq.'-') then
            aa27(17:20)='-000'
          else
            if( aa27(19:19).eq.'-') then
              aa27(17:19)='-00'
            else
              if( aa27(18:18).eq.'-') then
                aa27(17:18)='-0'
              else
                if( aa27(17:17).ne.'-' ) aa27(17:17)=' '
                do j=18,21
                  if(aa27(j:j).eq.' ') aa27(j:j)='0'
                end do
              end if
            end if
          end if
        end if
        do j=23,26
          if( aa27(j:j).eq.' ' ) aa27(j:j)='0'
        end do
        write(402,'(a27)') aa27
      end do
      do j=1,2
        sda(j)=dsqrt((sda(j)-avea(j)**2/dble(naa(j)))/dble(naa(j)-1))
        sdk(j)=dsqrt((sdk(j)-avek(j)**2/dble(nak(j)))/dble(nak(j)-1))
        sdh(j)=dsqrt((sdh(j)-aveh(j)**2/dble(nah(j)))/dble(nah(j)-1))
        sds(j)=dsqrt((sds(j)-aves(j)**2/dble(nas(j)))/dble(nas(j)-1))
        sdr(j)=dsqrt((sdr(j)-aver(j)**2/dble(nar(j)))/dble(nar(j)-1))
        sdn(j)=dsqrt((sdn(j)-aven(j)**2/dble(nan(j)))/dble(nan(j)-1))
        avea(j)=avea(j)/dble(naa(j))
        avea2(j)=avea2(j)/dble(naa(j))
        avek(j)=avek(j)/dble(nak(j))
        avek2(j)=avek2(j)/dble(nak(j))
        aveh(j)=aveh(j)/dble(nah(j))
        aveh2(j)=aveh2(j)/dble(nah(j))
        aves(j)=aves(j)/dble(nas(j))
        aves2(j)=aves2(j)/dble(nas(j))
        aver(j)=aver(j)/dble(nar(j))
        aver2(j)=aver2(j)/dble(nar(j))
        aven(j)=aven(j)/dble(nan(j))
        aven2(j)=aven2(j)/dble(nan(j))
c        call rounda2(avea(j),ic)
c        call rounda2(sda(j),ic)
c        call rounda2(avea2(j),ic)
c        call rounda2(avek(j),ic)
c        call rounda2(sdk(j),ic)
c        call rounda2(avek2(j),ic)
c        call rounda2(aveh(j),ic)
c        call rounda2(sdh(j),ic)
c        call rounda2(aveh2(j),ic)
c        call rounda2(aves(j),ic)
c        call rounda2(sds(j),ic)
c        call rounda2(aves2(j),ic)
c        call rounda2(aver(j),ic)
c        call rounda2(sdr(j),ic)
c        call rounda2(aver2(j),ic)
c        call rounda2(aven(j),ic)
c        call rounda2(sdn(j),ic)
c        call rounda2(aven2(j),ic)
        call rounda( avea(j) )
        call rounda( sda(j) )
        call rounda( avea2(j) )
        call rounda( avek(j) )
        call rounda( sdk(j) )
        call rounda( avek2(j) )
        call rounda( aveh(j) )
        call rounda( sdh(j) )
        call rounda( aveh2(j) )
        call rounda( aves(j) )
        call rounda( sds(j) )
        call rounda( aves2(j) )
        call rounda( aver(j) )
        call rounda( sdr(j) )
        call rounda( aver2(j) )
        call rounda( aven(j) )
        call rounda( sdn(j) )
        call rounda( aven2(j) )
        rewind(999)
        write(999,'(18f10.3)') avea(j),sda(j),avea2(j),
     +                        avek(j),sdk(j),avek2(j),
     +                        aveh(j),sdh(j),aveh2(j),
     +                        aves(j),sds(j),aves2(j),
     +                        aver(j),sdr(j),aver2(j),
     +                        aven(j),sdn(j),aven2(j)
        rewind(999)
        read(999,'(18f10.3)') avea(j),sda(j),avea2(j),
     +                        avek(j),sdk(j),avek2(j),
     +                        aveh(j),sdh(j),aveh2(j),
     +                        aves(j),sds(j),aves2(j),
     +                        aver(j),sdr(j),aver2(j),
     +                        aven(j),sdn(j),aven2(j)
      end do
      write(10,'(a2,2(4f10.5,i7),2f10.4)') 'ma',
     + ((avea(j),sda(j),rmina(j),rmaxa(j),naa(j)),j=1,2),
     + (avea2(j),j=1,2)
      write(10,'(a2,2(4f10.5,i7),2f10.4)') 'mk',
     + ((avek(j),sdk(j),rmink(j),rmaxk(j),nak(j)),j=1,2),
     + (avek2(j),j=1,2)
      write(10,'(a2,2(4f10.5,i7),2f10.4)') 'mh',
     + ((aveh(j),sdh(j),rminh(j),rmaxh(j),nah(j)),j=1,2),
     + (aveh2(j),j=1,2)
      write(10,'(a2,2(4f10.5,i7),2f10.4)') 'Sa',
     + ((aves(j),sds(j),rmins(j),rmaxs(j),nas(j)),j=1,2),
     + (aves2(j),j=1,2)
      write(10,'(a2,2(4f10.5,i7),2f10.4)') 'Sr',
     + ((aver(j),sdr(j),rminr(j),rmaxr(j),nar(j)),j=1,2),
     + (aver2(j),j=1,2)
      write(10,'(a2,2(4f10.5,i7),2f10.4)') 'Sn',
     + ((aven(j),sdn(j),rminn(j),rmaxn(j),nan(j)),j=1,2),
     + (aven2(j),j=1,2)

      rewind(2)
      sg2=-9.999d0
      rk(1)=-0.67d0
      rk(2)=0.0d0
      rk(3)=0.67d0
      do k=1,3
c        call rounda2(rk(k),ic)
        call rounda(rk(k))
        rewind(999)
        write(999,'(f10.3)') rk(k)
        rewind(999)
        read(999,'(f10.3)') rk(k)
      end do
      rk(2)=0.0d0
      write(*,'(f10.3)') rk(1),rk(2),rk(3)
      do while( .not.eof(2) )
        read(2,
     +'(i1,i9,21x,a16,i1,3i2,i10,16x,x,i9,42x,i5,11x,a62,a15,a20,3x,
     + i7,a10,8x,7x,i11,x,a16)') !i7,a10,8x,7x,<a11>,x,a16を修正
     +isex,id,aa16,(ibb(j),j=1,4),
     +isire,idam,jc,aa62,aa15,aa20,ilast,aj10,idbs,abs
        call wasei(ibb,is)
        call calend(iw1,is)
        age=3.0d0+dble(iw2-iw1)/365.0d0
        ibir=ibb(1)*1000000+ibb(2)*10000+ibb(3)*100+ibb(4)
        if( isex.eq.1 ) id=1000000000+id
        nk=0
        bb24='　　　　　　　　　　　　'
        do i=1,31
          if( aa62(2*i-1:2*i).ne.'　' ) then
            nk=nk+1
            if(nk.ge.13) exit
            bb24(2*nk-1:2*nk)=aa62(2*i-1:2*i)
          end if
        end do
        call hash(id,ip,neqn,count )
        call hash(iped(ip,1),ip2,neqn,count )
        call hash(iped(ip,2),ip3,neqn,count )
        call hash(iped(ip3,1),ip4,neqn,count )
        do j=1,2
          if( rbv(ip,j).ne.-999.999d0.and.kigo(ip,j).eq.'　' ) then
            kigo(ip,j)='推'
          else
            if( kigo(ip,j).eq.'期' ) then
              cycle
            else !期待育種価の計算
              if( rbv(ip2,j).ne.-999.999d0.and.kigo(ip2,j).ne.'期'
     +       .and.rbv(ip3,j).ne.-999.999d0.and.kigo(ip3,j).ne.'期'
     +        .and.rac(ip2,1).ge.0.85d0.and.rac(ip2,2).ge.0.85d0)then !種雄牛の正確度は0.85以上
c                if( irecF(ip,j).ne.1 ) then !実際の記録があるのに除外された個体については期待育種価を計算しない!!!!!!!!!!2019/10より削除
                  rbv(ip,j)=(rbv(ip2,j)+rbv(ip3,j))/2.0d0
c                  call rounda2(rbv(ip,j),ic) !小数点第４位を四捨五入して３桁に
                  call rounda(rbv(ip,j))
                  write(999,'(f10.3)') rbv(ip,j)
                  backspace(999)
                  read(999,'(f10.3)') rbv(ip,j)
                  rac(ip,j)=sqrt(rac(ip2,j)**2+rac(ip3,j)**2)/2.0d0
c                  call rounda2(rac(ip,j),ic) !小数点第４位を四捨五入して３桁に
                  call rounda(rac(ip,j))
                  write(999,'(f10.3)') rac(ip,j)
                  backspace(999)
                  read(999,'(f10.3)') rac(ip,j)
                  kigo(ip,j)='期'
                  write(98,'(i10,i2)') id,j
c                end if!!!!!!!!!!2019/10より削除
              end if
            end if
          end if
        end do
        ABC='-　'
        SG=-9.999d0
        do i=1,47
          if( ap(i).eq.aken(ip) ) then
            do j=1,2
              if( rbv(ip,j).ne.-999.999d0 ) then
                sg(j)=(rbv(ip,j)-ave(i,j))/sd(i,j)
c                call rounda2(sg(j),ic)
                call rounda(sg(j))
                rewind(999)
                write(999,'(f10.3)') sg(j)
                rewind(999)
                read(999,'(f10.3)') sg(j)
                sg2(ip,j)=sg(j)
                if( sg(j).le.-0.67d0 ) then
                  ABC(j)='A　'
                else
                  if( sg(j).lt.-0.0d0 ) then
                    ABC(j)='B　'
                  else
                    if( rbv(ip,j).le.ave(i,j) ) then
                      ABC(j)='B　'
                    else
                      if( sg(j).le.0.67d0) then
                        ABC(j)='C　'
                      else
                        ABC(j)="C´"
                      end if
                    end if
                  end if
                end if
              end if
            end do
          end if
        end do
c        do j=1,2
c          call rounda(sg(j))
c        end do
        if( rd(ip,2).ge.280 ) then
          pebv=rbv(ip,2)+pe(ip)
        else
          pebv=-999.999d0
        end if
c        do j=1,2 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!ここのルーチンは削除。「期待育種価」(実際は推定のものも含む）は生かす。2019/10/3
c          if( irecF(ip,j).eq.1.and. !実際に記録はある
c     +        iflag(ip).eq.0.and.   !後代がいない
c     +        kigo(ip,j).eq.'期' ) then !期待育種価が計算されている
c              rbv(ip,j)=-999.999d0
c              sg(j)=-9.999d0
c              rac(ip,j)=-999.999d0
c              kigo(ip,j)='　'
c              abc(j)='-　'
c              if( j.eq.2 ) pebv=-999.999d0
c              write(9,'(i10)') id
c          end if!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c        end do
        !８歳時期待産子数の計算
        r8=0.0d0
        rka=0.0d0
        rkc=0.0d0
        rf=0.0d0
        if8=0
        if( kigo(ip,1).eq.'推'.and.pebv.ge.-999.and.rd(ip,1).ge.18.0d0  !初産月齢も分娩間隔も分析した記録を持っているものが対象。
     +      .and.rd(ip,1).lt.37.0d0.and.irecF(ip,1).eq.1   ) then
c         if( rbv(ip,1).ge.-999.and.rbv(ip,2).ge.-999) then
          do i=1,nnp
            if( app(i).eq.aa15(1:2) ) then
              do j=iip(i),iip(i+1)-1
                if( fmc(j).eq.aa15(1:14) )then
                  do k=1,2
                    rf(k)=rfarm(j,k) !現所有者の農家の効果
                  end do
                  go to 9
                end if
              end do
            end if
          end do
          do j=1,2
            rf(j)=0.0d0
          end do
9         continue
c          rka=rua+rbv(ip,1)+rf(1)
          rka=rd(ip,1) !初産月齢は実績をもちいる
          rkc=ruc+pebv+rf(2)
          R8=(8.0d0*365.0d0-rka*(365.0d0/12.0d0))/rkc+1
          if( age.ge.8.0d0) then!4280401
            if8=1
          else
            if8=2
          end if
c          if( rk8(ip).ne.0.0d0 ) then
c            r8=rk8(ip)
c            if8=1
c          else
c            if( rcage(ip).eq.0 ) then
c              rka=rua+rbv(ip,1)+rf(1)
c              if( pebv.ge.-999) then
c                rkc=ruc+pebv+rf(2)
c              else
c                rkc=ruc+rf(2)
c              end if
c              R8=(8.0d0*365.0d0-rka*(365.0d0/12.0d0))/rkc+1
c              if8=3
c            else
c              rka=rcage(ip)*365.0d0
c              if( pebv.ge.-999) then
c                rkc=ruc+pebv+rf(2)
c              else
c                rkc=ruc+rf(2)
c              end if
c              R8=(8.0d0*365.0d0-rka)/rkc+ncp(ip)
c              if8=2
c            end if
c          end if
        end if
        if( id.ge.1000000000) cycle
        do i=1,5
          if( anum(i).eq.aa15 ) then
            do j=1,2
              abc(j)=' '
            end do
            if( rm.eq.0.000.and.rn.eq.0.000) then
               write(248,'(i11,x,a16,4(i11,x,a16),
     +         2(f10.3,f7.3,f10.3,x,a2,x,a3),2f10.3,2i8,i2,x,a15,x,a20,x
     +           ,a24,i6,2f10.3,i7,f7.1,i3,x,a10,7f10.3,i3)')
     +   id,aa16,isire,name(ip2),idam,name(ip3),iped(ip3,1),name(ip4),
     +   idbs,abs,
     +((rbv(ip,j),sg(j),rac(ip,j),kigo(ip,j),ABC(j)),j=1,2),pe(ip),pebv,
     +  ibir,ilast,ireal(ip),aa15,aa20,bb24,jc,
     +  (rd(ip,j),j=1,2),nn(ip),rv(ip),np(ip),aj10,
     +      R8,rua+rbv(ip,1),rbv(ip,2),pe(ip),rf(1),rf(2),rkc,if8
            else
              write(248,'(i11,x,a16,4(i11,x,a16),
     +         2(f10.3,f7.3,f10.3,x,a2,x,a3),2f10.3,2i8,i2,x,a15,x,a20,x
     +          ,a24,i6,2f10.3,i7,f7.1,i3,2f7.3,x,a10,7f10.3,i3)')
     +     id,aa16,isire,name(ip2),idam,name(ip3),iped(ip3,1),name(ip4),
     +   idbs,abs,
     +((rbv(ip,j),sg(j),rac(ip,j),kigo(ip,j),ABC(j)),j=1,2),pe(ip),pebv,
     +      ibir,ilast,ireal(ip),aa15,aa20,bb24,jc,
     +      (rd(ip,j),j=1,2),nn(ip),rv(ip),np(ip),crec(ip),bc(ip),aj10,
     +      R8,rua+rbv(ip,1),rbv(ip,2),pe(ip),rf(1),rf(2),rkc,if8
            end if
            go to 100
          end if
        end do
        do i=1,47
          if( aa15(1:2).eq.ap(i)) then
            if( rm.eq.0.000.and.rn.eq.0.000) then
               write(200+i,'(i11,x,a16,4(i11,x,a16),
     +         2(f10.3,f7.3,f10.3,x,a2,x,a3),2f10.3,2i8,i2,x,a15,x,a20,x
     +           ,a24,i6,2f10.3,i7,f7.1,i3,x,a10,7f10.3,i3)')
     +   id,aa16,isire,name(ip2),idam,name(ip3),iped(ip3,1),name(ip4),
     +   idbs,abs,
     +((rbv(ip,j),sg(j),rac(ip,j),kigo(ip,j),ABC(j)),j=1,2),pe(ip),pebv,
     +  ibir,ilast,ireal(ip),aa15,aa20,bb24,jc,
     +  (rd(ip,j),j=1,2),nn(ip),rv(ip),np(ip),aj10,
     +      R8,rua+rbv(ip,1),rbv(ip,2),pe(ip),rf(1),rf(2),rkc,if8
            else
              write(200+i,'(i11,x,a16,4(i11,x,a16),
     +         2(f10.3,f7.3,f10.3,x,a2,x,a3),2f10.3,2i8,i2,x,a15,x,a20,x
     +          ,a24,i6,2f10.3,i7,f7.1,i3,2f7.3,x,a10,7f10.3,i3)')
     +   id,aa16,isire,name(ip2),idam,name(ip3),iped(ip3,1),name(ip4),
     +   idbs,abs,
     +((rbv(ip,j),sg(j),rac(ip,j),kigo(ip,j),ABC(j)),j=1,2),pe(ip),pebv,
     +  ibir,ilast,ireal(ip),aa15,aa20,bb24,jc,
     +  (rd(ip,j),j=1,2),nn(ip),rv(ip),np(ip),crec(ip),bc(ip),aj10,
     +      R8,rua+rbv(ip,1),rbv(ip,2),pe(ip),rf(1),rf(2),rkc,if8
            end if
          end if
        end do
100     continue
      end do
      rewind(1)
      do while( .not.eof(1) )
        read(1,'(i11)') id
        call hash(id,ip,neqn,count )
        if( id.lt.1000000000 ) then
          do j=1,2
            if( ikigo(ip,j).ne.1 ) then
              rbv(ip,j)=-99.999d0
              rac(ip,j)=0.0d0
              sg2(ip,j)=-9.999d0
            else
              rbv2(j)=rbv(ip,j)
              rac2(j)=rac(ip,j)
              if( sg2(ip,j).eq.-9.999d0 ) then
                do i=1,47
                  if( ap(i).eq.aken(ip) )then
                    sg(j)=(rbv(ip,j)-ave(i,j))/sd(i,j)
c                    call rounda2(sg(j),ic)
                    call rounda(sg(j))
                    rewind(999)
                    write(999,'(f10.3)') sg(j)
                    rewind(999)
                    read(999,'(f10.3)') sg(j)
                    sg2(ip,j)=sg(j)
                  end if
                end do
                if( aken(ip).eq.'00' ) go to 900
              end if
            end if
          end do
          if( ikigo(ip,1).ne.1.and.ikigo(ip,2).ne.1 ) go to 900
c          write(401,'(i1,i9,a3,a2,f7.3,f6.3,f5.3,f7.3,f6.3,f5.3)')
c     +    2,id,abun,aken(ip),((rbv(ip,j),sg2(ip,j),rac(ip,j)),j=2,1,-1)
               write(901,'(i1,i9,a3,a2,2(i6,i4,i5))')
     +        2,id,abun,aken(ip),
     +                  ((int(rbv(ip,j)*1000.0d0),
     +                  int(rac(ip,j)*1000.0d0),
     +                  int(sg2(ip,j)*1000.0d0)),j=2,1,-1)
               backspace(901)
               read(901,'(a45)') aa45
               if( aa45(20:20).eq.'-' ) then
                 aa45(16:20)='-0000'
               else
                 if( aa45(19:19).eq.'-' ) then
                   aa45(16:19)='-000'
                 else
                   if( aa45(18:18).eq.'-' ) then
                     aa45(16:18)='-00'
                   else
                     if( aa45(17:17).eq.'-' ) then
                       aa45(16:17)='-0'
                     else
                       if( aa45(16:16).ne.'-') aa45(16:16)=' '
                       do j=17,20
                         if( aa45(j:j).eq.' ' ) aa45(j:j)='0'
                       end do
                     end if
                   end if
                 end if
               end if
               do j=22,24
                 if( aa45(j:j).eq.' ') aa45(j:j)='0'
               end do
               if( aa45(29:29).eq.'-' ) then
                 aa45(26:29)='-000'
               else
                 if( aa45(28:28).eq.'-' ) then
                   aa45(26:28)='-00'
                 else
                   if( aa45(27:27).eq.'-') then
                     aa45(26:27)='-0'
                   else
                     if( aa45(26:26).ne.'-') aa45(26:26)=' '
                     do j=27,29
                       if( aa45(j:j).eq.' ' ) aa45(j:j)='0'
                     end do
                   end if
                 end if
               end if
               if( aa45(35:35).eq.'-' ) then
                 aa45(31:35)='-0000'
               else
                 if( aa45(34:34).eq.'-' ) then
                   aa45(31:34)='-000'
                 else
                   if( aa45(33:33).eq.'-' ) then
                     aa45(31:33)='-00'
                   else
                     if( aa45(32:32).eq.'-' ) then
                       aa45(31:32)='-0'
                     else
                       if( aa45(31:31).ne.'-' ) aa45(31:31)=' '
                       do j=32,35
                         if( aa45(j:j).eq.' ' ) aa45(j:j)='0'
                       end do
                     end if
                   end if
                 end if
               end if
               do j=37,39
                 if( aa45(j:j).eq.' ' ) aa45(j:j)='0'
               end do
               if( aa45(44:44).eq.'-' ) then
                 aa45(41:44)='-000'
               else
                 if( aa45(43:43).eq.'-' ) then
                   aa45(41:43)='-00'
                 else
                   if( aa45(42:42).eq.'-') then
                     aa45(41:42)='-0'
                   else
                     if( aa45(41:41).ne.'-') aa45(41:41)=' '
                     do j=42,44
                       if( aa45(j:j).eq.' ' ) aa45(j:j)='0'
                     end do
                   end if
                 end if
               end if
               write(401,'(a45)') aa45
        else
          do j=1,2
c            if( sg2(ip,j).eq.-9.999d0 ) then
c              do i=1,47
c                if( ap(i).eq.aken(ip) )then
c                   sg(j)=(rbv(ip,j)-ave(i,j))/sd(i,j)
c                   call rounda2(sg(j),ic)
c                   rewind(999)
c                   write(999,'(f10.3)') sg(j)
c                   rewind(999)
c                   read(999,'(f10.3)') sg(j)
c                   call rounda(sg(j))
c                   sg2(ip,j)=sg(j)
c                 end if
c              end do
c            end if
             sg2(ip,j)=0.0d0
          end do
          if(rac(ip,1).ge.0.85d0.and.rac(ip,2).ge.0.85d0.and.ikigo(ip,1)
     +         .eq.1.and.ikigo(ip,2).eq.1 ) then
c               write(401,'(i10,a3,a2,f7.3,f5.3,f6.3,f7.3,f5.3,f6.3)')
               write(901,'(i10,a3,a2,2(i6,i4,i5))')
     +         id,abun,'00',
     +                  ((int(rbv(ip,j)*1000.0d0),
     +                  int(rac(ip,j)*1000.0d0),
     +                  int(sg2(ip,j)*1000.0d0)),j=2,1,-1)
               backspace(901)
               read(901,'(a45)') aa45
               if( aa45(20:20).eq.'-' ) then
                 aa45(16:20)='-0000'
               else
                 if( aa45(19:19).eq.'-' ) then
                   aa45(16:19)='-000'
                 else
                   if( aa45(18:18).eq.'-' ) then
                     aa45(16:18)='-00'
                   else
                     if( aa45(17:17).eq.'-' ) then
                       aa45(16:17)='-0'
                     else
                       if( aa45(16:16).ne.'-') aa45(16:16)=' '
                       do j=17,20
                         if( aa45(j:j).eq.' ' ) aa45(j:j)='0'
                       end do
                     end if
                   end if
                 end if
               end if
               do j=22,24
                 if( aa45(j:j).eq.' ') aa45(j:j)='0'
               end do
               if( aa45(29:29).eq.'-' ) then
                 aa45(26:29)='-000'
               else
                 if( aa45(28:28).eq.'-' ) then
                   aa45(26:28)='-00'
                 else
                   if( aa45(27:27).eq.'-') then
                     aa45(26:27)='-0'
                   else
                     if( aa45(26:26).ne.'-') aa45(26:26)=' '
                     do j=27,29
                       if( aa45(j:j).eq.' ' ) aa45(j:j)='0'
                     end do
                   end if
                 end if
               end if
               if( aa45(35:35).eq.'-' ) then
                 aa45(31:35)='-0000'
               else
                 if( aa45(34:34).eq.'-' ) then
                   aa45(31:34)='-000'
                 else
                   if( aa45(33:33).eq.'-' ) then
                     aa45(31:33)='-00'
                   else
                     if( aa45(32:32).eq.'-' ) then
                       aa45(31:32)='-0'
                     else
                       if( aa45(31:31).ne.'-' ) aa45(31:31)=' '
                       do j=32,35
                         if( aa45(j:j).eq.' ' ) aa45(j:j)='0'
                       end do
                     end if
                   end if
                 end if
               end if
               do j=37,39
                 if( aa45(j:j).eq.' ' ) aa45(j:j)='0'
               end do
               if( aa45(44:44).eq.'-' ) then
                 aa45(41:44)='-000'
               else
                 if( aa45(43:43).eq.'-' ) then
                   aa45(41:43)='-00'
                 else
                   if( aa45(42:42).eq.'-') then
                     aa45(41:42)='-0'
                   else
                     if( aa45(41:41).ne.'-') aa45(41:41)=' '
                     do j=42,44
                       if( aa45(j:j).eq.' ' ) aa45(j:j)='0'
                     end do
                   end if
                 end if
               end if
               write(401,'(a45)') aa45
          end if
        end if
900     continue
      end do
c      close(901,status='delete') 
      stop
      end



c----------------------------------------------------------------------
c
      SUBROUTINE HASH(K,IP,NEQN,COUNT)
c
c-----------------------------------------------------------------------
c      PARAMETER(M=31,M1=17) !origin
       PARAMETER(M=10000000,M1=999983) !素数

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
      SUBROUTINE rounda(rv1)
c
c-----------------------------------------------------------------------
c     四捨五入サブルーチン
      implicit none
      real(8) :: rv1
      if( rv1.ge.0.0d0 ) then
        rv1=rv1+(1.0e-10)
        else
        rv1=rv1-(1.0e-10)
      end if
      return
      end
c----------------------------------------------------------------------
c----------------------------------------------------------------------
c
      SUBROUTINE rounda3(rv1)
c
c-----------------------------------------------------------------------
c     四捨五入サブルーチン
      implicit none
      real(8) :: rv1
      if( rv1.gt.0.0d0 ) then
        rv1=rv1+(1.0e-10)
        else
        rv1=rv1-(1.0e-10)
      end if
      return
      end
c----------------------------------------------------------------------
c-------------------------------------------------------
c     和暦から西暦に変換する
      SUBROUTINE WASEI( Iw,Is )
        IMPLICIT NONE
        INTEGER :: Iw(4),Is(3),j
C-----------------------------------------------------------------------
        IF( IW(1).EQ. 2 ) IS(1)=1910+IW(2)
        IF( IW(1).EQ. 3 ) IS(1)=1925+IW(2)
        IF( IW(1).EQ. 4 ) IS(1)=1988+IW(2)
        IF( IW(1).EQ. 5 ) IS(1)=2018+IW(2)
        DO J=2,3
          IS(J)=IW(J+1)
        END DO
        RETURN
      END SUBROUTINE WASEI
c-------------------------------------------------------
c     西暦0年1月1日からの日数を計算する
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
c----------------------------------------------------------------------
c
      SUBROUTINE rounda2(rv1,ic)
c
c-----------------------------------------------------------------------
c     四捨五入サブルーチン
      implicit none
      real(8) :: rv1
      integer ic,iv1
      call rounda(rv1)
      ic=ic+1
      iv1=int(rv1*(1.0d0*10**ic))
      rv1=dble(iv1)/(1.0d0*10**ic)
      call rounda(rv1)
      ic=ic-1
      return
      end
