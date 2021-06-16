c     新出組織コードのチェック
c     新規改良組合コードがあれば，kaikumidtに追加する
       character no(1600)*62,jou*62
       character*20 new,hi1
       integer a,n
       write(*,*)'input oldfile name?'
       read(*,'(a)')hi1
       write(*,*)'input newfile name?'
       read(*,'(a)')new
       open(4,file='news',status='new')
       open(5,file=new,status='old')
       open(6,file=hi1,status='old')
        a=1
        n=1
100    format(a62)
300    read(6,100,end=200)no(a)
          a=a+1
          goto 300
200     read(5,100,end=400)jou
              n=n+1
         do 60  i=1,a-1
          if(jou(1:6).eq.no(i)(1:6))then
           goto 200
            endif
60          continue
            write(4,110)jou
110    format(a62)
            goto 200
400       write(*,*)'comprete end'
           close(5)
           close(4)
            close(6)
           stop
           end
