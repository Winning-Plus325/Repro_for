c     fmc2.datに改良組合コード等を付加する
       implicit none
       integer i,i1
       character data1*178
       character id(2500000)*89
       CHARACTER infile*20,outfile*20

       open(10,file='kaiexg3')
       write(*,'(a)')'入力ファイル名は（fmc2.dat）？'
       read(*,'(a)')infile
       open(11,file=infile,status='old')
       write(*,'(a)')'出力ファイル名は？'
       read(*,'(a)')outfile
       open(12,file=outfile)

        i=1
100     read(10,'(a84)',end=109)id(i)

         i=i+1
         goto 100
109     i=i-1
110    read(11,'(a20)',end=119)data1
        do i1=1,i
          if(id(i1)(1:14).eq.data1(1:14))then
        write(12,111)id(i1)(1:4),id(i1)(69:70),data1
111     format(a4,a2,1x,a20)
           goto 110
          endif
        enddo
        write(12,'(a2,a1,a3,1x,a20)')data1(1:2),'9',data1(5:7),data1
         goto 110
119    close(10)
       close(11)
       close(12)
       stop
       end 