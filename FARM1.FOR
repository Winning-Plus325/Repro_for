C     é©åßÇÃî_â∆î‘çÜÇÃÇ›èdï°ÇÃñ≥Ç¢ÇÊÇ§Ç…î≤Ç´èoÇµÇƒÇ≠ÇÈ
      implicit none
      INTEGER,PARAMETER :: MAXND=300000
      INTEGER :: NC,ND(50),NP,I,J,NCH,ICH(50,MAXND),NND,NPF,NK,II
      CHARACTER :: FARM(50,MAXND)*14,FM*14,AW*1,DFILE*15,RFILE*15
      CHARACTER :: FFARM(MAXND)*14
      CHARACTER :: FK(MAXND)*20,AAP(MAXND)*2,PF(MAXND)*2,AP(50)*2
      OPEN(1,FILE='fmc.dat',STATUS='OLD')
c      OPEN(2,FILE='fmc2.dat',STATUS='UNKNOWN')
      ND=0
      DO WHILE( .NOT.EOF(1) )
        READ(1,'(A14)') FM
        DO II=1,NP
          IF( AP(II).EQ.FM(1:2) ) THEN
            DO I=1,ND(II)
              IF( FARM(II,I).EQ.FM ) THEN
                  ICH(II,I)=ICH(II,I)+1
                  GO TO 10
              END IF
            END DO
            ND(II)=ND(II)+1
            FARM(II,ND(II))=FM
            ICH(II,ND(II))=1
            GO TO 10
          END IF
        END DO
        NP=NP+1
        WRITE(*,*) NP,FM
        AP(NP)=FM(1:2)
        ND(NP)=1
        FARM(NP,1)=FM
        ICH(II,1)=1
10      CONTINUE
      END DO
      NND=0
      OPEN(3,FILE='MID.TXT',STATUS='UNKNOWN')
      DO II=1,NP
        DO I=1,ND(II)
c          IF( ICH(II,I).NE.1 ) THEN
              NND=NND+1
c             FFARM(NND)=FARM(II,I)
              write(3,'(a14,i6)') farm(ii,i),ich(ii,i)
c          END IF
        END DO
      END DO
c      REWIND(3)
c      NK=0
c      DO WHILE( .NOT.EOF(3) )
c        NK=NK+1
c        READ(3,'(A20)') FK(NK)
c      END DO
c      NCH=20
c      CALL SORTA( FK,MAXND,NK,NCH )      
c      DO I=1,NK
c        WRITE(2,'(A20)') FK(I)
c      END DO
      STOP
      END

      SUBROUTINE SORTA(CHA,MAXND,ND,NCH)
      INTEGER ND,NCH
      CHARACTER*NCH :: CHA(MAXND),W

      DO K=1,ND-1
       KMAX=K
        DO H=K+1,ND
         IF(CHA(H).LT.CHA(KMAX)) KMAX=H
        END DO
       W=CHA(K)
       CHA(K)=CHA(KMAX)
       CHA(KMAX)=W
      END DO
      
      RETURN
      END SUBROUTINE SORTA