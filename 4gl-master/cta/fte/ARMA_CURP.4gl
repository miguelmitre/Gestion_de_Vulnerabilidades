###########################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                        #
#Propietario       => E.F.P.                                              #
#Programa ARMA_CURP=> FUNCION QUE ARMA LA CURP DEL TRABAJADOR             #
#Sistema           => AFI.                                                #
#Autor             => FERNANDO HERRERA HERNANDEZ                          #
#Fecha             => 26 DE AGOSTO DEL 2010                               #
###########################################################################
DATABASE safre_af


GLOBALS 
  DEFINE
     vnss                                       CHAR(11),
     vcurp                                      CHAR(18),
     vpaterno                                   CHAR(40),
     vmaterno                                   CHAR(40),
     vnombres                                   CHAR(40),
     vsexo                                      SMALLINT,
     vestadon                                   SMALLINT,
     vn_unico                                   CHAR(18),
     vcurp_arma1                                CHAR(18),
     curp                                       CHAR(18),
     vfena                                      DATE,

     vsexo_val                                  CHAR(1),
     vedo_val                                   CHAR(2),
     largo                                      SMALLINT,
     car1                                       CHAR(1),
     car2                                       CHAR(1),
     car3                                       CHAR(1),

     vcons                                      CHAR(40),

     arr_curp                                   ARRAY[18] OF RECORD
                                                curp_pos        CHAR(1)
                                                END RECORD,
     i                                          SMALLINT,
     arr_letr                                   ARRAY[27] OF RECORD
                                                car             CHAR(1)
                                                END RECORD,
     j                                          SMALLINT,
     arr_nume                                   ARRAY[10] OF RECORD
                                                num             CHAR(1)
                                                END RECORD,
     k                                          SMALLINT,
     pasa                                       CHAR(1),
     contador1                                  SMALLINT,
     contador2                                  SMALLINT,
     contador3                                  SMALLINT,
     contador4                                  SMALLINT,
     contador5                                  SMALLINT,
     desc_err                                   CHAR(60),
     desp_err                                   SMALLINT,
     val_mes                                    CHAR(02),

     nom_b , nom_b1, nom_b2,
     nom_b3, nom_b4, nom_b5                     CHAR(40),
     bla, ban, long, bb                         SMALLINT,
     no_t1                                      CHAR(40),
     pa_t, ma_t, no_t                           CHAR(02),
     patmatnom, patmatnom1                      CHAR(04)

     define enter char(1)

     define vcve_cur                            CHAR(18)
     define vpasa                               CHAR(1)
     define vdig_ver_curp                       SMALLINT

END GLOBALS

MAIN

   LET vnss = ARG_VAL(1)

   CALL STARTLOG("ARMA_CURP.log")

   SELECT paterno,
          materno,
          nombres,
          fena   ,
          estadon,
          sexo
   INTO   vpaterno,
          vmaterno,
          vnombres,
          vfena   ,
          vestadon,
          vsexo
   FROM   afi_mae_afiliado
   WHERE  n_seguro = vnss

   DATABASE safre_tmp

   WHENEVER ERROR CONTINUE
      DROP TABLE tmp_curp_base
      CREATE TABLE tmp_curp_base (curp CHAR(16))
   WHENEVER ERROR STOP

   DATABASE safre_af

   {DECLARE c1 CURSOR FOR
   SELECT *
   FROM   nss_curp
   FOREACH c1 INTO vnss, vpaterno, vmaterno, vnombres, vfena, vestadon, vsexo }

   CALL arma_clave(vpaterno, vmaterno, vnombres, vfena, vestadon, vsexo)
   RETURNING vcve_cur

   LET pasa  = 0
   LET largo = 0
   LET vcons = NULL

   {### Convierte campo Sexo
   IF vsexo = 1 THEN
      LET vsexo_val = 'H'
   ELSE
      LET vsexo_val = 'M'
   END IF

   ### Convierte campo estado de nacimiento
   IF vestadon = 1  THEN LET vedo_val = 'AS' END IF
   IF vestadon = 2  THEN LET vedo_val = 'BC' END IF
   IF vestadon = 3  THEN LET vedo_val = 'BS' END IF
   IF vestadon = 4  THEN LET vedo_val = 'CC' END IF
   IF vestadon = 5  THEN LET vedo_val = 'CL' END IF
   IF vestadon = 6  THEN LET vedo_val = 'CM' END IF
   IF vestadon = 7  THEN LET vedo_val = 'CS' END IF
   IF vestadon = 8  THEN LET vedo_val = 'CH' END IF
   IF vestadon = 9  THEN LET vedo_val = 'DF' END IF
   IF vestadon = 10 THEN LET vedo_val = 'DG' END IF
   IF vestadon = 11 THEN LET vedo_val = 'GT' END IF
   IF vestadon = 12 THEN LET vedo_val = 'GR' END IF
   IF vestadon = 13 THEN LET vedo_val = 'HG' END IF
   IF vestadon = 14 THEN LET vedo_val = 'JC' END IF
   IF vestadon = 15 THEN LET vedo_val = 'MC' END IF
   IF vestadon = 16 THEN LET vedo_val = 'MN' END IF
   IF vestadon = 17 THEN LET vedo_val = 'MS' END IF
   IF vestadon = 18 THEN LET vedo_val = 'NT' END IF
   IF vestadon = 19 THEN LET vedo_val = 'NL' END IF
   IF vestadon = 20 THEN LET vedo_val = 'OC' END IF
   IF vestadon = 21 THEN LET vedo_val = 'PL' END IF
   IF vestadon = 22 THEN LET vedo_val = 'QT' END IF
   IF vestadon = 23 THEN LET vedo_val = 'QR' END IF
   IF vestadon = 24 THEN LET vedo_val = 'SP' END IF
   IF vestadon = 25 THEN LET vedo_val = 'SL' END IF
   IF vestadon = 26 THEN LET vedo_val = 'SR' END IF
   IF vestadon = 27 THEN LET vedo_val = 'TC' END IF
   IF vestadon = 28 THEN LET vedo_val = 'TS' END IF
   IF vestadon = 29 THEN LET vedo_val = 'TL' END IF
   IF vestadon = 30 THEN LET vedo_val = 'VZ' END IF
   IF vestadon = 31 THEN LET vedo_val = 'YN' END IF
   IF vestadon = 32 THEN LET vedo_val = 'ZS' END IF
   IF vestadon = 33 THEN LET vedo_val = 'NE' END IF
   IF vestadon = 35 THEN LET vedo_val = 'NE' END IF
   IF vestadon = 39 THEN LET vedo_val = 'NE' END IF}

   ### SEPARA CURP POR POSICIONES
   LET arr_curp[01].curp_pos = curp[01]  LET arr_curp[02].curp_pos = curp[02]
   LET arr_curp[03].curp_pos = curp[03]  LET arr_curp[04].curp_pos = curp[04]
   LET arr_curp[05].curp_pos = curp[05]  LET arr_curp[06].curp_pos = curp[06]
   LET arr_curp[07].curp_pos = curp[07]  LET arr_curp[08].curp_pos = curp[08]
   LET arr_curp[09].curp_pos = curp[09]  LET arr_curp[10].curp_pos = curp[10]
   LET arr_curp[11].curp_pos = curp[11]  LET arr_curp[12].curp_pos = curp[12]
   LET arr_curp[13].curp_pos = curp[13]  LET arr_curp[14].curp_pos = curp[14]
   LET arr_curp[15].curp_pos = curp[15]  LET arr_curp[16].curp_pos = curp[16]
   LET arr_curp[17].curp_pos = curp[17]  LET arr_curp[18].curp_pos = curp[18]

   ### INICIALIZA ARREGLO CON VALORES ALFABETICOS
   LET arr_letr[01].car = 'B' LET arr_letr[02].car = 'C'
   LET arr_letr[03].car = 'D' LET arr_letr[04].car = 'F'
   LET arr_letr[05].car = 'G' LET arr_letr[06].car = 'H'
   LET arr_letr[07].car = 'J' LET arr_letr[08].car = 'K'
   LET arr_letr[09].car = 'L' LET arr_letr[10].car = 'M'
   LET arr_letr[11].car = 'N' LET arr_letr[12].car = '\321'
   LET arr_letr[13].car = 'P' LET arr_letr[14].car = 'Q'
   LET arr_letr[15].car = 'R' LET arr_letr[16].car = 'S'
   LET arr_letr[17].car = 'T' LET arr_letr[18].car = 'V'
   LET arr_letr[19].car = 'W' LET arr_letr[20].car = 'X'
   LET arr_letr[21].car = 'Y' LET arr_letr[22].car = 'Z'

   ### INICIALIZA ARREGLO CON VALORES NUMERICOS
   LET k = 0
   FOR k = 1 TO 9
     LET arr_nume[k].num = k
   END FOR
   LET arr_nume[10].num = 0

   ### Valida curp
   LET i         = 0
   LET j         = 0
   LET k         = 0
   LET contador1 = 0
   LET contador2 = 0
   LET contador3 = 0
   LET contador4 = 0
   LET contador5 = 0
   LET desp_err  = 0


   ### Obtiene posicion 14
   LET vpaterno = vpaterno CLIPPED
   LET largo    = LENGTH(vpaterno CLIPPED)

   FOR i = 2 TO largo
        FOR j = 1 TO 22
          IF vpaterno[i] = arr_letr[j].car THEN
             LET contador1 = contador1 + 1
             IF contador1 >= 1 THEN
                LET vcons = vcons CLIPPED, vpaterno[i] CLIPPED
             END IF
          END IF
        END FOR
   END FOR

   IF (vcons  IS NULL) OR
      (vcons = ' ')    OR
      (vcons[1] = '\321') THEN
     LET car1 = 'X'
   ELSE
     LET car1 = vcons[1]
   END IF

   ### Obtiene posicion 15
   LET i        = 0
   LET j        = 0
   LET vmaterno = vmaterno CLIPPED
   LET largo    = LENGTH(vmaterno CLIPPED)
   LET vcons    = NULL

   FOR i = 2 TO largo
        FOR j = 1 TO 22
          IF vmaterno[i] = arr_letr[j].car THEN
             LET contador2 = contador2 + 1
             IF contador2 >= 1 THEN
                LET vcons = vcons CLIPPED, vmaterno[i] CLIPPED
             END IF
          END IF
        END FOR
   END FOR

   IF (vcons  IS NULL) OR
      (vcons = ' ')    OR
      (vcons[1] = '\321') THEN
     LET car2 = 'X'
   ELSE
     LET car2 = vcons[1]
   END IF

   ### Obtener nombre compuesto
   INITIALIZE nom_b, nom_b1, nom_b2, nom_b3, nom_b4, nom_b5  TO NULL

   LET long = 0
   LET i    = 0
   LET bb   = 0
   LET bla  = 0
   LET j    = 1

   LET no_t1 = vnombres CLIPPED
   LET long  = LENGTH(no_t1 CLIPPED)

   FOR i = 1 TO long
       IF no_t1[i,i] = " " THEN
          LET bla = bla + 1
          CASE bla
             WHEN 1
                LET nom_b1 = nom_b CLIPPED
                LET nom_b1 = nom_b1 CLIPPED

                SELECT "f.X"
                FROM   afi_articulo f
                WHERE  f.palabra MATCHES nom_b1
                IF STATUS != NOTFOUND THEN
                   INITIALIZE nom_b TO NULL
                   LET j = 1
                ELSE
                   LET bb = 1
                   EXIT FOR
                END IF
             WHEN 2
                LET nom_b2 = nom_b CLIPPED

                SELECT "X"
                FROM   afi_articulo
                WHERE  palabra = nom_b2
                IF STATUS != NOTFOUND THEN
                   INITIALIZE nom_b TO NULL
                   LET j = 1
                ELSE
                   LET bb = 2
                   EXIT FOR
                END IF
             WHEN 3
                LET nom_b3 = nom_b CLIPPED

                SELECT "X"
                FROM   afi_articulo
                WHERE  palabra = nom_b3
                IF STATUS != NOTFOUND THEN
                   INITIALIZE nom_b TO NULL
                   LET j = 1
                ELSE
                   LET bb = 3
                   EXIT FOR
                END IF
             WHEN 4
                LET nom_b4 = nom_b CLIPPED

                SELECT "X"
                FROM   afi_articulo
                WHERE  palabra = nom_b4
                IF STATUS != NOTFOUND THEN
                   INITIALIZE nom_b TO NULL
                   LET j = 1
                ELSE
                   LET bb = 4
                   EXIT FOR
                END IF
             WHEN 5
                LET nom_b5 = nom_b CLIPPED

                SELECT "X"
                FROM   afi_articulo
                WHERE  palabra = nom_b5
                IF STATUS != NOTFOUND THEN
                   LET j = 1
                   INITIALIZE nom_b TO NULL
                ELSE
                   LET bb = 5
                   EXIT FOR
                END IF
          END CASE
       ELSE
          LET nom_b[j,j] = no_t1[i,i]
          LET bb         = 6
          LET j          = j + 1
       END IF
   END FOR

   CASE bb
        WHEN 1 LET no_t1 = nom_b1
        WHEN 2 LET no_t1 = nom_b2
        WHEN 3 LET no_t1 = nom_b3
        WHEN 4 LET no_t1 = nom_b4
        WHEN 5 LET no_t1 = nom_b5
        WHEN 6 LET no_t1 = nom_b
   END CASE

   IF no_t1 IS NULL OR no_t1 = " " THEN
      LET nom_b1 = no_t1
   END IF

   LET vnombres = no_t1

   FOR i = 1 TO long
     IF 1 = 1 THEN
        LET no_t[i,i] = no_t1[i,i]
        IF no_t[i,i] = "\321" OR no_t[i,i] = "\361" THEN
           LET no_t[i,i] = "X"
        END IF
        EXIT FOR
     END IF
   END FOR

   LET patmatnom = pa_t CLIPPED, ma_t CLIPPED, no_t CLIPPED

   SELECT b.palabra_si
   INTO   patmatnom1
   FROM   afi_no_conviene b
   WHERE  palabra_no = patmatnom
   IF STATUS != NOTFOUND THEN
      LET patmatnom = patmatnom1 CLIPPED
   END IF

   ### Obtiene posicion 16
   LET i        = 0
   LET j        = 0
   LET vnombres = vnombres CLIPPED
   LET largo    = LENGTH(vnombres CLIPPED)
   LET vcons    = NULL

   FOR i = 2 TO largo
        FOR j = 1 TO 22
          IF vnombres[i] = arr_letr[j].car THEN
             LET contador3 = contador3 + 1
             IF contador3 >= 1 THEN
                LET vcons = vcons CLIPPED, vnombres[i] CLIPPED
             END IF
          END IF
        END FOR
   END FOR

   IF (vcons  IS NULL) OR
      (vcons = ' ')    OR
      (vcons[1] = '\321') THEN
     LET car3 = 'X'
   ELSE
     LET car3 = vcons[1]
   END IF

   ### Obtiene posicion 16

   LET curp = vcurp_arma1 CLIPPED, vsexo_val CLIPPED, vedo_val CLIPPED,
              car1 CLIPPED, car2 CLIPPED, car3 CLIPPED, vn_unico[17,18]

   LET vcurp = curp

   LET vcurp = vcve_cur[1,13], car1 CLIPPED, car2 CLIPPED, car3 CLIPPED, '00'

   CALL var_dig_curp(vcurp) RETURNING vpasa, vdig_ver_curp

   --display vnss, " ", vcve_cur, " ", vcurp, " ", vpasa, vdig_ver_curp

   IF vpasa = 0 THEN
      LET vcurp = vcurp[1,17], vdig_ver_curp USING "#"
   END IF

   --RETURN curp

   {update nss_curp set curp = vcurp
   where nss = vnss }
   
   INSERT INTO safre_tmp:tmp_curp_base VALUES(vcurp);

   --END FOREACH 
END MAIN

FUNCTION arma_clave(paterno, materno, nombres, fena, estadon, sexo)
#ac----------------------------------------------------------------
   DEFINE paterno, materno, nombres     CHAR(40),
          fena                          DATE    ,
          sexo                          SMALLINT,
          estadon                       SMALLINT,
          sexo1                         CHAR(01),
          fena1                         CHAR(06),
          pa_t1, ma_t1, no_t1           CHAR(40),
          pa_t,  ma_t,  no_t            CHAR(02),
          pater,  pater1, pater2        CHAR(40),
          pater3, pater4, pater5        CHAR(40),
          pa_papa, ma_mama              CHAR(40),
          patmat                        CHAR(03),
          mater , mater1, mater2,
          mater3, mater4, mater5        CHAR(40),
          patmatnom, patmatnom1         CHAR(04),
          nom_b , nom_b1, nom_b2,
          nom_b3, nom_b4, nom_b5        CHAR(40),
          cve_mex                       CHAR(02),
          ent_fed1                      CHAR(02),
          cve_cur                       CHAR(17),
          pa_pa, ma_ma, no_no           CHAR(01),
          ch_ll                         CHAR(02),
          consonante                    CHAR(03),

          bla, ban, i, long, bb, j      SMALLINT,
          enter                         CHAR(1)

   INITIALIZE pa_t1, ma_t1, no_t1 TO NULL
   INITIALIZE pa_t,  ma_t,  no_t  TO NULL
   INITIALIZE pa_pa, ma_ma, no_no, consonante, ch_ll TO NULL
   INITIALIZE pater, pater1, pater2, pater3, pater4, pater5, pa_papa  TO NULL

   LET long = 0  LET i = 0   LET bb = 0   LET j = 0

##paterno

   LET j = 1
   LET pa_t1 = paterno CLIPPED
   LET long = LENGTH(pa_t1 CLIPPED)
   LET long = long + 1

   FOR i = 1 TO long
       IF pa_t1[i,i] = " " THEN
          LET bla = bla + 1
          CASE bla
             WHEN 1
                    LET pater1 = pater
                    SELECT "X" FROM afi_articulo
                            WHERE palabra = pater1
                    IF STATUS != NOTFOUND THEN
                       INITIALIZE pater TO NULL
                       LET j = 1
                    ELSE
                       LET bb = 1
                       EXIT FOR
                    END IF
             WHEN 2
                    LET pater2 = pater
                    SELECT "X" FROM afi_articulo
                            WHERE palabra = pater2
                    IF STATUS != NOTFOUND THEN
                       INITIALIZE pater TO NULL
                       LET j = 1
                    ELSE
                       LET bb = 2
                       EXIT FOR
                    END IF
             WHEN 3
                    LET pater3 = pater
                    SELECT "X" FROM afi_articulo
                            WHERE palabra = pater3
                    IF STATUS != NOTFOUND THEN
                       INITIALIZE pater TO NULL
                       LET j = 1
                    ELSE
                       LET bb = 3
                       EXIT FOR
                    END IF
             WHEN 4
                    LET pater4 = pater
                    SELECT "X" FROM afi_articulo
                            WHERE palabra = pater4
                    IF STATUS != NOTFOUND THEN
                       INITIALIZE pater TO NULL
                       LET j = 1
                    ELSE
                       LET bb = 4
                       EXIT FOR
                    END IF
             WHEN 5
                    LET pater5 = pater
                    SELECT "X" FROM afi_articulo
                            WHERE palabra = pater5
                    IF STATUS != NOTFOUND THEN
                       INITIALIZE pater TO NULL
                       LET j = 1
                    ELSE
                       LET bb = 5
                       EXIT FOR
                    END IF
          END CASE
       ELSE
           LET pater[j,j] = pa_t1[i,i]
           LET bb = 6
           LET j = j + 1
       END IF
   END FOR

   CASE bb
        WHEN 1 LET pa_t1 = pater1
        WHEN 2 LET pa_t1 = pater2
        WHEN 3 LET pa_t1 = pater3
        WHEN 4 LET pa_t1 = pater4
        WHEN 5 LET pa_t1 = pater5
        WHEN 6 LET pa_t1 = pater
   END CASE
   IF pa_t1 IS NULL OR pa_t1 = " " THEN
      LET pa_t1 = pater1 CLIPPED
   END IF

   LET pa_papa = pa_t1 CLIPPED

   LET j = 1
   FOR i = 1 TO long
       IF j = 1 THEN
          LET pa_t[j,j] = pa_t1[i,i]
          IF pa_t[j,j] = "\321" OR pa_t[j,j] = "\361" THEN
             LET pa_t[j,j] = "X"
          END IF
          IF pa_t[j,j] = "C" OR pa_t[j,j] = "L" THEN
             LET ch_ll[j,j] = pa_t[j,j]
          END IF
          LET j = j + 1

       ELSE

          IF pa_t1[i,i] MATCHES "[AEIOU]" THEN
             LET pa_t[j,j] = pa_t1[i,i]

             IF j = 2 THEN
                EXIT FOR
             END IF
{
          ELSE
             LET ch_ll[j,j] = pa_t1[i,i]
             IF ch_ll = "CH" OR ch_ll[j,j] = "LL" THEN
                LET j = 2
             ELSE
                 LET pa_t[j,j] = pa_t1[i,i]
                 IF j = 2 THEN
                    EXIT FOR
                 END IF
             END IF
}
          END IF
       END IF
   END FOR

   LET j = 1
   FOR i = 1 TO long
      LET pa_pa[j,j] = pa_papa[i,i]
      IF i > 1 THEN
         IF pa_pa[j,j] NOT MATCHES "[AEIOU]" THEN
            IF pa_pa[j,j] = "\321" OR pa_pa[j,j] = "\361" THEN
               LET pa_pa = "X"
               EXIT FOR
            ELSE
               LET pa_pa = pa_papa[i,i]
               EXIT FOR
            END IF
         END IF
      END IF
   END FOR
## materno
   INITIALIZE mater, mater1, mater2, mater3, mater4, mater5  TO NULL

   LET long = 0  LET i = 0   LET bb = 0  LET bla = 0  LET j = 1

   LET ma_t1 = materno CLIPPED
   LET long = LENGTH(ma_t1 CLIPPED)

   IF long IS NULL OR long = 0 THEN
      LET ma_t = "X"
      LET patmat = pa_t CLIPPED, ma_t CLIPPED
      LET ma_ma = "X"

   ELSE
         FOR i = 1 TO long
             IF ma_t1[i,i] = " " THEN
                LET bla = bla + 1
                CASE bla
                   WHEN 1
                          LET mater1 = mater CLIPPED
                          SELECT "X" FROM afi_articulo
                                  WHERE palabra = mater1
                          IF STATUS != NOTFOUND THEN
                             INITIALIZE mater TO NULL
                             LET j = 1
                          ELSE
                             LET bb = 1
                             EXIT FOR
                          END IF
                   WHEN 2
                          LET mater2 = mater CLIPPED
                          SELECT "X" FROM afi_articulo
                                  WHERE palabra = mater2
                          IF STATUS != NOTFOUND THEN
                             INITIALIZE mater TO NULL
                             LET j = 1
                          ELSE
                             LET bb = 2
                             EXIT FOR
                          END IF
                   WHEN 3
                          LET mater3 = mater CLIPPED
                          SELECT "X" FROM afi_articulo
                                  WHERE palabra = mater3
                          IF STATUS != NOTFOUND THEN
                             INITIALIZE mater TO NULL
                             LET j = 1
                          ELSE
                             LET bb = 3
                             EXIT FOR
                          END IF
                   WHEN 4
                          LET mater4 = mater CLIPPED
                          SELECT "X" FROM afi_articulo
                               WHERE palabra = mater4
                          IF STATUS != NOTFOUND THEN
                             INITIALIZE mater TO NULL
                             LET j = 1
                          ELSE
                             LET bb = 4
                             EXIT FOR
                          END IF
                   WHEN 5
                          LET mater5 = mater CLIPPED
                          SELECT "X" FROM afi_articulo
                                  WHERE palabra = mater5
                          IF STATUS != NOTFOUND THEN
                             INITIALIZE mater TO NULL
                             LET j = 1
                          ELSE
                             LET bb = 5
                             EXIT FOR
                          END IF
                END CASE
             ELSE
                 LET mater[j,j] = ma_t1[i,i]
                 LET bb = 6
                 LET j  = j + 1
             END IF
         END FOR

         CASE bb
              WHEN 1 LET ma_t1 = mater1
              WHEN 2 LET ma_t1 = mater2
              WHEN 3 LET ma_t1 = mater3
              WHEN 4 LET ma_t1 = mater4
              WHEN 5 LET ma_t1 = mater5
              WHEN 6 LET ma_t1 = mater
         END CASE
         IF ma_t1  IS NULL OR ma_t1 = " " THEN
            LET ma_t1 = mater1
         END IF

         LET ma_mama = ma_t1 CLIPPED
         FOR i = 1 TO long
             IF i = 1 THEN
                LET ma_t[i,i] = ma_t1[i,i]
                IF ma_t[i,i] = "\321" OR ma_t[i,i] = "\361" THEN
                   LET ma_t[i,i] = "X"
                END IF
                EXIT FOR
             END IF
         END FOR

         LET j = 1
         FOR i = 1 TO long
            LET ma_ma[j,j] = ma_mama[i,i]
            IF i > 1 THEN
               IF ma_ma[j,j] NOT MATCHES "[AEIOU]" THEN
                  IF ma_ma[j,j] = "\321" OR ma_ma[j,j] = "\361" THEN
                     LET ma_ma = "X"
                     EXIT FOR
                  ELSE
                     LET ma_ma = ma_mama[i,i]
                     EXIT FOR
                  END IF
               END IF
            END IF
         END FOR
   END IF

## nombres
   INITIALIZE nom_b, nom_b1, nom_b2, nom_b3, nom_b4, nom_b5  TO NULL

   LET long = 0  LET i = 0   LET bb = 0  LET bla = 0  LET j = 1

   LET no_t1 = nombres CLIPPED
   LET long = LENGTH(no_t1 CLIPPED)
##   LET long = long + 1
   FOR i = 1 TO long
       IF no_t1[i,i] = " " THEN
          LET bla = bla + 1
          CASE bla
             WHEN 1
                    LET nom_b1 = nom_b CLIPPED
                    LET nom_b1 = nom_b1 CLIPPED
                    SELECT "f.X" FROM afi_articulo f
                            WHERE f.palabra MATCHES nom_b1
                    IF STATUS != NOTFOUND THEN
                       INITIALIZE nom_b TO NULL
                       LET j = 1
                    ELSE
                       LET bb = 1
                       EXIT FOR
                    END IF
             WHEN 2
                    LET nom_b2 = nom_b CLIPPED
                    SELECT "X" FROM afi_articulo
                            WHERE palabra = nom_b2
                    IF STATUS != NOTFOUND THEN
                       INITIALIZE nom_b TO NULL
                       LET j = 1
                    ELSE
                       LET bb = 2
                       EXIT FOR
                    END IF
             WHEN 3
                    LET nom_b3 = nom_b CLIPPED
                    SELECT "X" FROM afi_articulo
                            WHERE palabra = nom_b3
                    IF STATUS != NOTFOUND THEN
                       INITIALIZE nom_b TO NULL
                       LET j = 1
                    ELSE
                       LET bb = 3
                       EXIT FOR
                    END IF
             WHEN 4
                    LET nom_b4 = nom_b CLIPPED
                    SELECT "X" FROM afi_articulo
                            WHERE palabra = nom_b4
                    IF STATUS != NOTFOUND THEN
                       INITIALIZE nom_b TO NULL
                       LET j = 1
                    ELSE
                       LET bb = 4
                       EXIT FOR
                    END IF
             WHEN 5
                    LET nom_b5 = nom_b CLIPPED
                    SELECT "X" FROM afi_articulo
                            WHERE palabra = nom_b5
                    IF STATUS != NOTFOUND THEN
                       LET j = 1
                       INITIALIZE nom_b TO NULL
                    ELSE
                       LET bb = 5
                       EXIT FOR
                    END IF
          END CASE
       ELSE
           LET nom_b[j,j] = no_t1[i,i]
           LET bb = 6
           LET j  = j + 1
       END IF
   END FOR

   CASE bb
        WHEN 1 LET no_t1 = nom_b1
        WHEN 2 LET no_t1 = nom_b2
        WHEN 3 LET no_t1 = nom_b3
        WHEN 4 LET no_t1 = nom_b4
        WHEN 5 LET no_t1 = nom_b5
        WHEN 6 LET no_t1 = nom_b
   END CASE

   IF no_t1 IS NULL OR no_t1 = " " THEN
      LET nom_b1 = no_t1
   END IF

   FOR i = 1 TO long
     IF i = 1 THEN
        LET no_t[i,i] = no_t1[i,i]
        IF no_t[i,i] = "\321" OR no_t[i,i] = "\361" THEN
           LET no_t[i,i] = "X"
        END IF
        EXIT FOR
     END IF
   END FOR
   LET patmatnom = pa_t CLIPPED, ma_t CLIPPED, no_t CLIPPED

   SELECT b.palabra_si INTO patmatnom1 FROM afi_no_conviene b
          WHERE palabra_no = patmatnom
   IF STATUS != NOTFOUND THEN
      LET patmatnom = patmatnom1 CLIPPED
   END IF

   LET j = 1
   FOR i = 1 TO long
      LET no_no[j,j] = no_t1[i,i]
      IF i > 1 THEN
         IF no_no[j,j] MATCHES "[AEIOU]" THEN
            DISPLAY ""
         ELSE
            IF no_no[j,j] = "\245" OR no_no[j,j] = "\244" THEN
               LET no_no = "X"
               EXIT FOR
            ELSE
               LET no_no = no_t1[i,i]
               EXIT FOR
            END IF
         END IF
      ELSE
           DISPLAY ""
      END IF
   END FOR

##fecha nacimiento
   LET fena1 = fena USING "YYMMDD"
##sexo
   CASE sexo
     WHEN 1 LET sexo1 = "H"
     WHEN 2 LET sexo1 = "M"
   END CASE

##ent. federativa
   SELECT a.estad_ren INTO ent_fed1 FROM tab_edo_norma a
          WHERE a.estad_cod = estadon
   IF STATUS = NOTFOUND THEN
      LET ent_fed1 = "  "
   END IF


## consonantes
 LET consonante = pa_pa CLIPPED, ma_ma CLIPPED, no_no CLIPPED

## cve_cur
   LET cve_cur = patmatnom CLIPPED, fena1 CLIPPED, sexo1 CLIPPED,
                 ent_fed1  CLIPPED, consonante CLIPPED

   RETURN cve_cur

END FUNCTION

###########################################################################
FUNCTION var_dig_curp(curp)
   DEFINE
     dv_curp            CHAR(1),
     curp               CHAR(18),
     arr                ARRAY[18] OF RECORD
                        curp_pos        CHAR(1)
                        END RECORD,
     i                  SMALLINT,
     arr1               ARRAY[36] OF RECORD
                        char            CHAR(1),
                        val             SMALLINT
                        END RECORD,
     j                  SMALLINT,
     arr2               ARRAY[17] OF RECORD
                        cons            SMALLINT
                        END RECORD,
     k                  SMALLINT,
     resultado          INTEGER,
     dism               SMALLINT,
     f                  SMALLINT,
     n                  SMALLINT,
     a                  SMALLINT,
     arr3               ARRAY[17] OF RECORD
                        mult            INTEGER
                        END RECORD,
     res_mult           INTEGER,
     acu_mult           INTEGER,
     residuo            SMALLINT,
     dig_ver_curp       SMALLINT,
     pasa               CHAR(1)

   LET pasa = 0

   ### SEPARA CURP POR POSICIONES
   LET arr[1].curp_pos  = curp[1]  LET arr[2].curp_pos  = curp[2]
   LET arr[3].curp_pos  = curp[3]  LET arr[4].curp_pos  = curp[4]
   LET arr[5].curp_pos  = curp[5]  LET arr[6].curp_pos  = curp[6]
   LET arr[7].curp_pos  = curp[7]  LET arr[8].curp_pos  = curp[8]
   LET arr[9].curp_pos  = curp[9]  LET arr[10].curp_pos = curp[10]
   LET arr[11].curp_pos = curp[11] LET arr[12].curp_pos = curp[12]
   LET arr[13].curp_pos = curp[13] LET arr[14].curp_pos = curp[14]
   LET arr[15].curp_pos = curp[15] LET arr[16].curp_pos = curp[16]
   LET arr[17].curp_pos = curp[17] LET arr[18].curp_pos = curp[18]

   ### PREPARA CARACTER PARA VALORES
   LET j = 0
   FOR j = 1 TO 36
      LET arr1[j].char = j
      LET arr1[j].val  = j
   END FOR

   LET arr1[10].char = 'A' LET arr1[11].char = 'B' LET arr1[12].char = 'C'
   LET arr1[13].char = 'D' LET arr1[14].char = 'E' LET arr1[15].char = 'F'
   LET arr1[16].char = 'G' LET arr1[17].char = 'H' LET arr1[18].char = 'I'
   LET arr1[19].char = 'J' LET arr1[20].char = 'K' LET arr1[21].char = 'L'
   LET arr1[22].char = 'M' LET arr1[23].char = 'N' LET arr1[24].char = '\321'
   LET arr1[25].char = 'O' LET arr1[26].char = 'P' LET arr1[27].char = 'Q'
   LET arr1[28].char = 'R' LET arr1[29].char = 'S' LET arr1[30].char = 'T'
   LET arr1[31].char = 'U' LET arr1[32].char = 'V' LET arr1[33].char = 'W'
   LET arr1[34].char = 'X' LET arr1[35].char = 'Y' LET arr1[36].char = 'Z'

   ### PREPARA CONSTANTES
   LET k    = 0
   LET dism = 18
   FOR k = 1 TO 17
      LET arr2[k].cons = dism
      LET dism = dism - 1
   END FOR

   ### OBTIENE DIGITO
   LET f = 0
   LET n = 0
   LET a = 0
   LET res_mult     = 0
   LET residuo      = 0
   LET dig_ver_curp = 0
   FOR f = 1 TO 17
      FOR n = 1 TO 36
        IF arr[f].curp_pos  = arr1[n].char THEN
           LET arr3[f].mult = arr1[n].val * arr2[f].cons
           LET res_mult     = arr3[f].mult
           LET acu_mult     = acu_mult + res_mult
        END IF
      END FOR
   END FOR

   ### OBTIENE RESIDUO Y SE RESTA CON CONSTANTE
   LET residuo = acu_mult MOD 10
   IF residuo = 0 THEN
      LET dig_ver_curp = 0
   ELSE
      LET dig_ver_curp = 10 - residuo
   END IF

   ### VALIDA RESULTADO DE D.V. VS POS. 18
   IF arr[18].curp_pos = dig_ver_curp THEN
      LET pasa = 1
   ELSE
      LET pasa = 0
   END IF

   RETURN pasa, dig_ver_curp

END FUNCTION

