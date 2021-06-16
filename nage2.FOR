C     KOTOMSをソートようにファイルを作り直したものを戻す
      CHARACTER :: KTM*48
      OPEN(1,FILE='KOTOMSll',STATUS='OLD')
      OPEN(2,FILE='KOTOMSL',STATUS='UNKNOWN')
      DO WHILE( .NOT.EOF(1) )
        READ(1,'(A48)') KTM
        WRITE(2,'(A41)') KTM(1:10)//KTM(18:48)
      END DO
      STOP
      END
      