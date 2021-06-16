      implicit none
      integer :: ii
      character :: cha(11)*2,long*143
      open(1,file='mhanz.dat',status='old')
      open(50,file='kotomsl')
      do while( .not.eof(1) )
        read(1,'(a143)') long
        write(50,'(a10,a7,a3,a4,a7,a10,a7)') long(1:10),long(47:53),
     +   long(11:13),long(43:46),long(47:53),long(76:85),long(86:92)
      end do
      stop
      end
      