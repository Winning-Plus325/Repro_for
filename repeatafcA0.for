	  implicit none
      integer :: i,j,na,nb,nk,nf
      character :: ap*2,aid*11,long*87,blong(30)*87,bid*11,bb14(30)*14
c      open(1,file='repeatA_afc.dat',status='old')
      open(1,file='repeatV_afc.dat',status='old')
      open(3,file='fmc.dat')
      open(4,file='id_inb.dat')
      open(41,file='cid_inb.dat')

      na=0
      bid='00000000000'
      write(4,*) 'all'
      write(41,*) 'all'
      nb=0
      nk=0
      do while( .not.eof(1) )
        read(1,'(a11,a87)' ) aid,long
        na=na+1
        write(41,'(i10,2a11)') na,long(1:11),aid
        if( aid.ne.bid ) then
          write(4,'(a11)') aid
          bid=aid
          write(3,'(a14)') long(70:83)
          nf=1
          bb14(1)=long(70:83)
        else
          do i=1,nf
            if( bb14(i).eq.long(70:83) ) go to 10
          end do
          write(3,'(a14)') long(70:83)
          nf=nf+1
          bb14(nf)=long(70:83)
10        continue
        end if
      end do
      stop
      end
