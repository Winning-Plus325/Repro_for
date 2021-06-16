      implicit none
      integer,parameter :: maxn=6000000
      integer :: na,k,i,iw1,iw2,iw3
      character :: aid*13,aa2*2,bb2*2,cc2*2,dd2*2

      
c      open(1,file='intervalETcut730.dat')
c      OPEN(11,FILE='CHKKETD1')
c      OPEN(12,FILE='CHKKETD2')
c      OPEN(13,FILE='CHKKETD3')
c      OPEN(14,FILE='CHKKETD4')
c      OPEN(15,FILE='CHKKETD5')
c      OPEN(16,FILE='CHKKETD6')
c      OPEN(17,FILE='CHKKETD7')
c      OPEN(18,FILE='CHKKETD8')
c      OPEN(19,FILE='CHKKETD9')
c      OPEN(20,FILE='CHKKETDA')
c      OPEN(21,FILE='CHKKETDB')
c      OPEN(22,FILE='CHKKETDC')
c      OPEN(23,FILE='CHKKETDD')
c      OPEN(24,FILE='CHKKETDE')
c      OPEN(25,FILE='CHKKETDF')
c      OPEN(26,FILE='CHKKETDG')
c      OPEN(27,FILE='CHKKETDH')
c      OPEN(28,FILE='CHKKETDI')
c      OPEN(29,FILE='CHKKETDJ')
c      OPEN(30,FILE='CHKKETDK')
c      OPEN(31,FILE='CHKKETDL')
c      OPEN(32,FILE='CHKKETDM')
c      OPEN(33,FILE='CHKKETDN')
c      OPEN(34,FILE='CHKKETDO')
c      OPEN(35,FILE='CHKKETDP')
c      OPEN(36,FILE='CHKKETDQ')
c      OPEN(37,FILE='CHKKETDR')
c      OPEN(38,FILE='CHKKETDS')
      open(1,file='730cutREC.dat',status='old')
      open(100,file='chkketid_all')
      na=0
      k=0
      do while(.not.eof(1))
        na=na+1
        if( mod(na,300000).eq.0 ) then
          k=k+1
        end if
        read(1,'(a13,i2,6x,i5,2(x,a2),11x,i5,2(x,a2))') 
     +   aid,iw1,iw3,cc2,dd2,iw2,aa2,bb2
        if( iw1.eq.1 ) cycle !ドナーレシピはmhan抽出しない[ge.1]→[eq.1] レシピは抽出するに変更
        aid(1:1)='2'
        do i=11,13
          if(aid(i:i).eq.' ') aid(i:i)='0'
        end do
c        write(11+k,'(a5,a13)') '00000',aid
        if( iw2.ge.1989 ) then
          iw2=400+(iw2-1988)
        else
          iw2=300+(iw2-1925)
        end if
        if( iw3.ge.1989 ) then
          iw3=400+(iw3-1988)
        else
          iw3=300+(iw3-1925)
        end if
        if( aa2(1:1).eq.' ') aa2(1:1)='0'
        if( bb2(1:1).eq.' ') bb2(1:1)='0'
        if( cc2(1:1).eq.' ') cc2(1:1)='0'
        if( dd2(1:1).eq.' ') dd2(1:1)='0'
        write(100,'(a13,i4,2a2,i4,2a2)') aid,iw2,aa2,bb2,iw3,cc2,dd2
      end do
      stop
      end
