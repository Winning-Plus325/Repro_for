      implicit none
      integer,parameter :: hashn=10000000
      integer :: isex,id,ibir,isire,idam,jc,ilast,iw1,ip,neqn,count,
     + na(47,2),nk,i,j,iwv(2),ip2,ip3,nc,jlast,iw(4),is(3),ip4,ic,idbs
      integer,allocatable :: ireal(:),iped(:,:),nn(:),iflag(:),np(:),
     + iflag2(:),ikigo(:,:),ib(:,:),iend(:,:)
      real(8) :: rwv(15),rafc,rciv,ave(47,2),sd(47,2),rmin(47,2),
     +rmax(47,2),ave2(47,2),sg(2),rwv2(2),rw1,pebv,rm,rn
      real(8),allocatable :: rac(:,:),rbv(:,:),rd(:,:),rv(:),pe(:),
     +crec(:),bc(:),rds(:,:)
      character :: aa16*16,aa62*62,aa15*15,aa20*20,aj10*10,ap(47)*2,
     +aa2*2,bb2*2,ABC(2)*3,dfile*30,bb24*24,aaa,anum(10)*15,
     +kfile*20,bfile*20,abs*16,aa7*7
      character,allocatable :: aken(:)*2,kigo(:,:)*2,name(:)*16
      real(8) :: avea(2),avek(2),aveh(2),
     + aves(2),aver(2),aven(2),sda(2),sdk(2),sdh(2),sds(2),sdr(2),sdn(2)
     +,rmina(2),rmink(2),rminh(2),rmins(2),rminr(2),rminn(2),
     +rmaxa(2),rmaxk(2),rmaxh(2),rmaxs(2),rmaxr(2),rmaxn(2),
     +avea2(2),avek2(2),aveh2(2),aves2(2),aver2(2),aven2(2),rk(3)
      integer :: naa(2),nak(2),nah(2),nas(2),nar(2),nan(2)
      allocate( ireal(hashn),iped(hashn,2),nn(hashn),iflag(hashn),
     + rac(hashn,2),rbv(hashn,2),rd(hashn,2),aken(hashn),kigo(hashn,2),
     + rv(hashn),np(hashn),pe(hashn),iflag2(hashn),ikigo(hashn,2),
     + ib(hashn,3),name(hashn),crec(hashn),bc(hashn),rds(hashn,2),
     + iend(hashn,2) )
      
      open(1,file='bv_acc.dat',status='old')
      open(11,file='bv_tmp.tmp')
      open(999,file='tmp.tmp')
      open(2,file='bvinf_osu',status='old') !本番は平成元年以降生まれの雄登録牛のbvinfを作成する
      write(*,*) '分析ファイル名は？'
      read(*,*) dfile
      open(3,file=dfile,status='old')
      open(4,file='C:\data\katsu\inbred\近交係数\ketof',status='old')
      open(5,file='C:\data\katsu\KYOYU\syugyu\MHAN_civ\life1.dat')
      open(6,file='pe.dat',status='old')
c      open(10,file='KENbvstat.txt')
      open(50,file='BVafccivOSUALL.dat')
c      open(51,file='BVafccivALL_OSUEX.dat')
c      open(60,file='stat_OSU.txt')
c      open(101,file='bv_mesuALL.dat')
c      open(102,file='bv_osuALL.dat')
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
        read(22,*) id,rw1
        rw1=rw1+rm+rn
        call hash( id,ip,neqn,count )
        bc(ip)=rw1
      end do
      nn=0
      rd=0
      do i=1,47
        write(99,'(i2)') i
        backspace(99)
        read(99,'(a2)') ap(i)
        if(ap(i)(1:1).eq.' ' ) ap(i)(1:1)='0'
        open(200+i,file='BvafccivOSU_add'//ap(i)//'.dat')
      end do
      open(248,file='BvafccivOSU_add48.dat')
      anum(1)='035100625000040'
      anum(2)='100106000010970'
      anum(3)='430300100030630'
      anum(4)='430500100046530'
      anum(5)='330513400423280'
      do while( .not.eof(5) )
        read(5,'(13x,i9,73x,f7.1,27x,i3)') id,rwv(1),iwv(1)
        call hash(id,ip,neqn,count )
        rv(ip)=rwv(1)
        np(ip)=iwv(1)
      end do
      ireal=0
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
        read(3,*) id,(rwv(j),j=2,nc-2),rafc,rciv
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
        read(6,*) aaa,iw1,id,rw1,rwv(1)
        call hash( id,ip,neqn,count )
        pe(ip)=rwv(1)
      end do
      rbv=-999.999d0
      rac=-999.999d0
      iflag=0
      iflag2=0
      rds=0.0d0
      ic=3
      do while( .not.eof(1) )
        read(1,*) id,iw1,((rwv(j),rwv2(j)),j=1,2),(iwv(j),j=1,2)
        do j=1,2
c          call rounda2(rwv(j),ic) !小数点第４位を四捨五入して小数点第３位まであつかう
c          call rounda2(rwv2(j),ic)!
          call rounda(rwv(j)) !小数点第４位を四捨五入して小数点第３位まであつかう
          call rounda(rwv2(j))!
        end do
        write(11,'(2i11,4f10.3,2i8)')
     +   id,iw1,((rwv(j),rwv2(j)),j=1,2),(iwv(j),j=1,2)
      end do
      rewind(11)
      do while( .not.eof(11))
        read(11,'(2i11,4f10.3,2i8)')
     +   id,iw1,((rwv(j),rwv2(j)),j=1,2),(iwv(j),j=1,2)
        call hash( id,ip,neqn,count )
        call hash( iped(ip,1),ip2,neqn,count )
        call hash( iped(ip,2),ip3,neqn,count )
        if( nn(ip).ne.0 ) then
          rd(ip,2)=rd(ip,2)/nn(ip)
        end if
        do j=1,2
c          call rounda(rwv(j)) 
c          call rounda(rwv2(j))
          rbv(ip,j)=rwv(j)
          rac(ip,j)=rwv2(j)
        end do
        if( iwv(1).ne.0 ) then 
          iflag2(ip2)=iflag2(ip2)+1
          rds(ip2,1)=rds(ip2,1)+rd(ip,1)
        end if
        if( iwv(2).ne.0 ) then
          iflag(ip2)=iflag(ip2)+1 !記録のある後代がいますフラグを父に立てる
          rds(ip2,2)=rds(ip2,2)+rd(ip,2)
        end if
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
      sda=0.0d0
      avek=0.0d0
      sdk=0.0d0
      nak=0
      nah=0
      nas=0
      aves=0.0d0
      sds=0.0d0
      nar=0
      aver=0.0d0
      sdr=0.0d0
      nan=0
      aven=0.0d0
      sdn=0.0d0
      ikigo=0
      iend=0
      do while( .not.eof(11) )
        read(11,*) id,iw1,((rwv(j),rwv2(j)),j=1,2),(iwv(j),j=1,2)
        call hash( id,ip,neqn,count )
        call hash( iped(ip,1),ip2,neqn,count )
        call hash( iped(ip,2),ip3,neqn,count )
        if( iflag2(ip2).ne.0.and.iend(ip2,1).eq.0 ) then
          rds(ip2,1)=rds(ip2,1)/dble(iflag2(ip2))
          iend(ip2,1)=1
        end if
        if( iflag(ip2).ne.0.and.iend(ip2,2).eq.0 ) then
          rds(ip2,2)=rds(ip2,2)/dble(iflag(ip2))
          iend(ip2,2)=1
        end if
        do j=1,2
          if( iflag(ip).eq.0.and.iwv(j).eq.0 ) then
            kigo(ip,j)='期'
            ikigo(ip,j)=2
            write(99,'(i10)') id
          else
            ikigo(ip,j)=1
          end if
        end do
c        if( id.lt.1000000000 ) then
c          write(101,'(i11,4f10.4,2i3,2i7,i5,2i3)')
c     +         id,((rwv(j),rwv2(j)),j=1,2),
c     +         ((ireal(ip)*10+ikigo(ip,j)),j=1,2),(iwv(j),j=1,2),
c     +         (ib(ip,j),j=1,3)
c        else
c          write(102,'(i11,4f10.4,2i3,2i7,i5,2i3)')
c     +         id,((rwv(j),rwv2(j)),j=1,2),
c     +         ((ireal(ip)*10+ikigo(ip,j)),j=1,2),iflag2(ip),iflag(ip),
c     +         (ib(ip,j),j=1,3)
c        end if
        if( ireal(ip).eq.1) then
          do i=1,47
            if( ap(i).eq.aken(ip) ) then
              do j=1,2
                if( kigo(ip,j).ne.'期' ) then
                  na(i,j)=na(i,j)+1
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
          call rounda(ave(i,j))
          rewind(999)
          write(999,'(f10.3)') ave(i,j)
          rewind(999)
          read(999,'(f10.3)') ave(i,j)
c          call rounda2(sd(i,j),ic)
          call rounda(sd(i,j))
          rewind(999)
          write(999,'(f10.3)') sd(i,j)
          rewind(999)
          read(999,'(f10.3)') sd(i,j)
        end do
        write(10,'(i2,2(5f10.4,i7))') i,
     + ((ave(i,j),sd(i,j),rmin(i,j),rmax(i,j),ave2(i,j),na(i,j)),j=1,2)
      end do
      do j=1,2
        sda(j)=dsqrt((sda(j)-avea(j)**2/dble(naa(j)))/dble(naa(j)-1))
        sdk(j)=dsqrt((sdk(j)-avek(j)**2/dble(nak(j)))/dble(nak(j)-1))
        sdh(j)=dsqrt((sdh(j)-aveh(j)**2/dble(nah(j)))/dble(nah(j)-1))
        sds(j)=dsqrt((sds(j)-aves(j)**2/dble(nas(j)))/dble(nas(j)-1))
        sdr(j)=dsqrt((sdr(j)-aver(j)**2/dble(nar(j)))/dble(nar(j)-1))
        sdn(j)=dsqrt((sdn(j)-aven(j)**2/dble(nan(j)))/dble(nan(j)-1))
        avea(j)=avea(j)/dble(naa(j))
        avek(j)=avek(j)/dble(nak(j))
        aveh(j)=aveh(j)/dble(nah(j))
        aves(j)=aves(j)/dble(nas(j))
        aver(j)=aver(j)/dble(nar(j))
        aven(j)=aven(j)/dble(nan(j))
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

      write(10,'(a2,2(4f10.5,10x,i7))') 'ma',
     + ((avea(j),sda(j),rmina(j),rmaxa(j),naa(j)),j=1,2)
      write(10,'(a2,2(4f10.5,10x,i7))') 'mk',
     + ((avek(j),sdk(j),rmink(j),rmaxk(j),nak(j)),j=1,2)
      write(10,'(a2,2(4f10.5,10x,i7))') 'mh',
     + ((aveh(j),sdh(j),rminh(j),rmaxh(j),nah(j)),j=1,2)
      write(10,'(a2,2(4f10.5,10x,i7))') 'Sa',
     + ((aves(j),sds(j),rmins(j),rmaxs(j),nas(j)),j=1,2)
      write(10,'(a2,2(4f10.5,10x,i7))') 'Sr',
     + ((aver(j),sdr(j),rminr(j),rmaxr(j),nar(j)),j=1,2)
      write(10,'(a2,2(4f10.5,10x,i7))') 'Sn',
     + ((aven(j),sdn(j),rminn(j),rmaxn(j),nan(j)),j=1,2)
      rewind(2)
      do while( .not.eof(2) )
        read(2,
     +'(i1,i9,21x,a16,i7,i10,16x,x,i9,42x,i5,11x,a62,a15,a20,3x,
     + i7,a10,8x,7x,i11,x,a16)') 
     +isex,id,aa16,ibir,isire,idam,jc,aa62,aa15,aa20,ilast,aj10,idbs,abs
        if( isex.eq.1 ) id=1000000000+id
        ilast=0
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
c                call rounda2(rbv(ip,j),ic) !小数点第４位を四捨五入して３桁に
                call rounda(rbv(ip,j))
                write(999,'(f10.3)') rbv(ip,j)
                backspace(999)
                read(999,'(f10.3)') rbv(ip,j)
                rac(ip,j)=sqrt(rac(ip2,j)**2+rac(ip3,j)**2)/2.0d0
c                call rounda2(rac(ip,j),ic) !小数点第４位を四捨五入して３桁に
                call rounda(rbv(ip,j))
                write(999,'(f10.3)') rac(ip,j)
                backspace(999)
                read(999,'(f10.3)') rac(ip,j)
                kigo(ip,j)='期'
                write(98,'(i10,i2)') id,j
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
        do j=1,2
          call rounda(sg(j))
        end do
        if( rd(ip,2).ge.280 ) then
          pebv=rbv(ip,2)+pe(ip)
        else
          pebv=-999.999d0
        end if
        if( kigo(ip,1).ne.'推'.or.kigo(ip,2).ne.'推') cycle !雄は期待育種価は算出しない
        if( rac(ip,1).lt.0.85d0.or.rac(ip,2).lt.0.85d0 ) cycle !雄は正確度0.85以上でないと返さない
        do i=1,5
          if( anum(i).eq.aa15 ) then
c            do j=1,2
c              abc(j)=' '
c            end do
            do j=1,2
              if( rbv(ip,j).ne.-999.999d0 ) then
                sg(j)=(rbv(ip,j)-avek(j))/sdk(j) !事業団の雄牛は全国供用中雌牛で算出 2015/4/28
c                call rounda2(sg(j),ic)
                call rounda(sg(j))
                rewind(999)
                write(999,'(f10.3)') sg(j)
                rewind(999)
                read(999,'(f10.3)') sg(j)
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
            write(248,'(i11,x,a16,4(i11,x,a16),
     +      2(f10.3,f7.3,f10.3,x,a2,x,a3),2f10.3,2i8,i2,x,a15,x,a20,x
     +      ,a24,i6,2f10.3,i7,f7.1,i3,2f7.3,x,a10)')
     +     id,aa16,isire,name(ip2),idam,name(ip3),iped(ip3,1),name(ip4),
     +idbs,abs,
     +((rbv(ip,j),sg(j),rac(ip,j),kigo(ip,j),ABC(j)),j=1,2),pe(ip),pebv,
     +      ibir,ilast,ireal(ip),aa15,aa20,bb24,jc,
     +    (rds(ip,j),j=1,2),iflag(ip),rv(ip),np(ip),crec(ip),bc(ip),aj10
            go to 100
          end if
        end do
        do i=1,47
          if( aa15(1:2).eq.ap(i)) then
          write(200+i,'(i11,x,a16,4(i11,x,a16),
     +         2(f10.3,f7.3,f10.3,x,a2,x,a3),2f10.3,2i8,i2,x,a15,x,a20,x
     +           ,a24,i6,2f10.3,i7,f7.1,i3,2f7.3,x,a10)')
     +   id,aa16,isire,name(ip2),idam,name(ip3),iped(ip3,1),name(ip4),
     +idbs,abs,
     +((rbv(ip,j),sg(j),rac(ip,j),kigo(ip,j),ABC(j)),j=1,2),pe(ip),pebv,
     +  ibir,ilast,ireal(ip),aa15,aa20,bb24,jc,
     +    (rds(ip,j),j=1,2),iflag(ip),rv(ip),np(ip),crec(ip),bc(ip),aj10
          end if
        end do
100     continue
        write(50,'(i11,x,a16,4(i11,x,a16),
     +         2(f10.3,f7.3,f10.3,x,a2,x,a3),2f10.3,2i8,i2,x,a15,x,a20,x
     +           ,a24,i6,2f10.3,i7,f7.1,i3,2f7.3,x,a10)')
     +   id,aa16,isire,name(ip2),idam,name(ip3),iped(ip3,1),name(ip4),
     +idbs,abs,
     +((rbv(ip,j),sg(j),rac(ip,j),kigo(ip,j),ABC(j)),j=1,2),pe(ip),pebv,
     +  ibir,ilast,ireal(ip),aa15,aa20,bb24,jc,
     +    (rds(ip,j),j=1,2),iflag(ip),rv(ip),np(ip),crec(ip),bc(ip),aj10
      end do
      !推定育種価の雌牛全体，供用中，非供用中の育種価の基本統計量を求める
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
c-------------------------------------------------------
c     和暦から西暦に変換する
      SUBROUTINE WASEI( Iw,Is )
        IMPLICIT NONE
        INTEGER :: Iw(4),Is(3),j
C-----------------------------------------------------------------------
        IF( IW(1).EQ. 2 ) IS(1)=1910+IW(2)
        IF( IW(1).EQ. 3 ) IS(1)=1925+IW(2)
        IF( IW(1).EQ. 4 ) IS(1)=1988+IW(2)
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
c      SUBROUTINE rounda2(rv1,ic)
c
c-----------------------------------------------------------------------
c     四捨五入サブルーチン
c      implicit none
c      real(8) :: rv1
c      integer ic,iv1
c      call rounda(rv1)
c      ic=ic+1
c      iv1=int(rv1*(1.0d0*10**ic))
c      rv1=dble(iv1)/(1.0d0*10**ic)
c      call rounda(rv1)
c      ic=ic-1
c      return
c      end
