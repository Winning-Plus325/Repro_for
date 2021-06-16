c     新出組織コードのチェック
c     新規改良組合コードがあれば，kaikumidtに追加する
      implicit none
      character sos(1600)*62,jou*62
      character*20 new,hi1
      integer iflag(1600),i,nd
      write(*,*)'input oldfile name?'
      read(*,'(a)')hi1
      open(6,file=hi1,status='old')
      write(*,*)'input newfile name?'
      read(*,'(a)')new
      open(5,file=new,status='old')

      open(4,file='kaikumidt.new',status='unknown')
      open(7,file='inakunattayo_sosi',status='unknown')
      nd=0
      iflag=0
      do while( .not.eof(6) )
        nd=nd+1
300     read(6,'(a62)') sos(nd)
      end do

      do while( .not.eof(5) )
200     read(5,'(a62)') jou
        do i=1,nd
          if(jou(1:6).eq.sos(i)(1:6))then
             iflag(i)=1
             write(4,'(a6,x,a40,x,a1)') jou(1:6),jou(7:46),'1'
             go to 10
          endif
        end do
        write(4,'(a6,x,a40,x,a1)') jou(1:6),jou(7:46),'2'
10      continue
      end do
      do i=1,nd
        if( iflag(i).eq.0 ) then
          write(7,'(a6,x,a40,x,a1)') sos(i)(1:6),sos(i)(7:46),'3'
        end if
      end do
400   write(*,*)'complete end'
      write(*,*) 'kaikumidt.newの最終列が[2]のデータを'
      write(*,*) '前回評価のkaikumidt.txtに追加してください'
      write(*,*) 
     + 'inakunattayo_sosiのデータはkaikumidt.txtから削除してください'
      close(5)
      close(4)
      close(6)
      stop
      end
