C     プログラム№７
C     ヒストグラム
C-----------------------------------------------------------------------型宣言文
      implicit none
      integer,parameter :: maxnd=8000000,maxnc=3000
      integer :: nt,nd,iw1,nc,nct(maxnc),i,j,na,ij
      real(8) :: rmin,rmax,ave,sd,rsum,rsum2,rz,rv(0:maxnc)
      real(8) :: rmin2,rmax2,r
      real(8),allocatable :: rd(:,:)
      character :: dfile*20,ofile*20
      allocate ( rd(maxnd,16) )
c-----------------------------------------------------------------------
      write(*,*) 'データファイルは？'
      read(*,*) dfile  !data2.dat
      open(1,file=dfile,status='old')
c
      write(*,*) 'ヒストグラムを読み込むデータは何列目ですか？'
      read(*,*) nt
      write(*,*) '条件は？ 0:all 1:ge 2:le 3:eq'
      read(*,*) ij
      write(*,*) '条件の値は？ '
      read(*,*) r
      write(*,*) '条件の列は何列目？ '
      read(*,*) nc
      
c
      write(*,*) '出力ファイルは？'
      read(*,*) ofile  !任意
      open(2,file=ofile,status='unknown')
      nd=0
      if( nt.ge.nc ) then
        na=nt
      else
        na=nc
      end if
      do while( .not.eof(1) )
        nd=nd+1
        read(1,*) (rd(nd,j),j=1,na)
        if( ij.ne.0 ) then
          if( ij.eq.1 ) then
            if( rd(nd,nc).lt.r ) then
              nd=nd-1
              cycle
            end if
          else if ( ij.eq.2 ) then
            if( rd(nd,nc).gt.r ) then
              nd=nd-1
              cycle
            end if
          else if ( ij.eq.3 ) then
            if( rd(nd,nc).ne.r ) then
              nd=nd-1
              cycle
            end if
          end if
        end if
        if( nd.eq.1 ) then
          rmin=rd(1,nt)
          rmax=rd(1,nt)
        end if
        if( rd(nd,nt).lt.rmin ) rmin=rd(nd,nt)
        if( rd(nd,nt).gt.rmax ) rmax=rd(nd,nt)
        rsum=rsum+rd(nd,nt)
        rsum2=rsum2+rd(nd,nt)**2
      end do
      sd=dsqrt( (rsum2-rsum**2/dble(nd))/dble(nd-1) )
      ave=rsum/dble(nd)
      write(*,'(5a9)')
     +'  average','       sd','      min','      max','        n'
      write(*,'(4f9.3,i9)') ave,sd,rmin,rmax,nd
      write(2,'(5a9)')
     +'  average','       sd','      min','      max','        n'
      write(2,'(4f9.3,i9)') ave,sd,rmin,rmax,nd
c-----------------------------------------------------------------------
      write(*,'(/a)') 'いくつからいくつまでの階級にしますか'
      write(*,*) 'いくつから？'
      read(*,*) rmin2
      write(*,*) 'いくつまで？'
      read(*,*) rmax2
      write(*,*) 'いくつ刻みの階級にしますか'
      read(*,*) rz
      nc=dint( (rmax2-rmin2)/rz )+2
      rv(0)=-9999999.9999
      rv(nc+1)=9999999.999
      do i=1,nc
        rv(i)=rmin2+dble(i-1)*rz
      end do
      if( rv(nc).gt.rmax2 ) nc=nc-1
      nct=0
      do i=1,nd
        do j=1,nc+1
          if( rd(i,nt).ge.rv(j-1).and.rd(i,nt).lt.rv(j) ) then
            nct(j)=nct(j)+1
            exit
          end if
        end do
      end do
      write(*,'(/8x,a,f8.2,i8,f8.2)')
     + '   x< ',rv(1),nct(1),dble(nct(1))/dble(nd)*100.0d0
      do i=2,nc
        write(*,'(f8.2,a,f8.2,i8,f8.2 )') 
     +   rv(i-1),' <_x< ',rv(i),nct(i),dble(nct(i))/dble(nd)*100.0d0
      end do
      write(*,'(f8.2,a,8x,i8,f8.2)')
     +rv(nc),' <_x  ',nct(nc+1),dble(nct(nc+1))/dble(nd)*100.0d0
      write(2,'(/8x,a,f8.2,i8,f8.2)')
     + '   x< ',rv(1),nct(1),dble(nct(1))/dble(nd)*100.0d0
      do i=2,nc
        write(2,'(f8.2,a,f8.2,i8,f8.2 )') 
     +   rv(i-1),' <_x< ',rv(i),nct(i),dble(nct(i))/dble(nd)*100.0d0
      end do
      write(2,'(f8.2,a,8x,i8,f8.2)')
     +rv(nc),' <_x  ',nct(nc+1),dble(nct(nc+1))/dble(nd)*100.0d0
      stop
      end

c------課題
c     ヒストグラムを求める列が複数ある場合（３列あって，２列目，５列目，６列目とばらばら）
c     に対応できるプログラムを作りなさい。
