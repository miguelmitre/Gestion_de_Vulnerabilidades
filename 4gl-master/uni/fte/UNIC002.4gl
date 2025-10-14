#############################################################################
#Proyecto          => SISTEMA DE AFORES( MEXICO )                           #
#Propietario       => EFP                                                   #
#Programa          => REGISTRO DE CONFRONTA                                 #
#Fecha creacion    => 08 DE FEBRERO DEL 2000.                               #
#Por               => MIGUEL ANGEL HERNANDEZ MARTINEZ.                      #
#Fecha actualiza   => 27 de marzo del 2001                                  #
#Por               => ARMANDO RODRIGUEZ CASTROPAREDES                       #
#Fecha actualiza   => 30 de mayo  del 2005                                  #
#Por               => MIGUEL ANGEL HERNANDEZ MARTINEZ.                      #
#Fecha actualiza   => 26 de septiembre del 2007                             #
#Sistema           => UNI                                                   #
#############################################################################
DATABASE safre_af
GLOBALS
    DEFINE reg_1 RECORD 
        folio                 INTEGER
    END RECORD

    DEFINE l_record RECORD
        nss_prueba            CHAR(11),
        marca                 CHAR(03)
    END RECORD

    DEFINE arr_1 ARRAY[400] OF RECORD 
        nss_uni               CHAR(11),
        paterno_uni           CHAR(40),
        materno_uni           CHAR(40),
        nombre_uni            CHAR(40),
        nombre_imss           CHAR(50),
        cve_ent_nss           CHAR(03),
        cve_afo_aclara        CHAR(03)
    END RECORD

    DEFINE arr_2 ARRAY[400] OF RECORD 
        marca                 CHAR(03),
        nss_uni               CHAR(11),
        cve_afo               CHAR(03),
        nombre                CHAR(40)
    END RECORD

    DEFINE arr  RECORD 
        marca                 CHAR(03),
        nss_uni               CHAR(11),
        nombre                CHAR(40)
    END RECORD

    DEFINE arr_3 ARRAY[100] OF RECORD 
        marca_cta1            CHAR(02),
        nss_cta1              CHAR(11),
        cve_afo1              CHAR(03),
        nombre_cta1           CHAR(40)
    END RECORD

    DEFINE arr_4 ARRAY[100] OF RECORD 
        diag_unifica          CHAR(02),
        nss_cta1              CHAR(11),
        paterno_cta1          CHAR(40),
        materno_cta1          CHAR(40),
        nombre_cta1           CHAR(40),
        nombre_imss           CHAR(50),
		  cve_ent_cta1          CHAR(03)
    END RECORD

    DEFINE c11_nss            CHAR(11),
           arc_1              ,
           arc_2              ,
           scr_1              ,
           scr_2              ,
           cont_1             ,
           cont_2             ,
           cont_3             ,
           cont_4             SMALLINT,
           cont_5             SMALLINT,
           HOY                DATE,
           enter              CHAR(01),
           display            CHAR(11)

   DEFINE act_edo             SMALLINT
END GLOBALS
#####################################################
MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP,
        PROMPT LINE LAST,
        ACCEPT KEY CONTROL-I 

    CALL STARTLOG("UNIC002.log")

    LET act_edo = 0

    WHENEVER ERROR CONTINUE
       DROP TABLE tmp_uni_unificador

       CREATE TEMP TABLE tmp_uni_unificador
       ( nss_uni               CHAR(11),
         paterno_uni           CHAR(40),
         materno_uni           CHAR(40),
         nombre_uni            CHAR(40),
         nombre_imss           CHAR(50),
         cve_ent_nss           CHAR(03),
         cve_afo_aclara        CHAR(03)
       )
    WHENEVER ERROR STOP

    CALL proceso()
END MAIN
#####################################################
FUNCTION Inicializa()
    INITIALIZE arr_1[cont_1].* TO NULL
    INITIALIZE arr_2[cont_1].* TO NULL
END FUNCTION
#####################################################
FUNCTION Inicializa1()
    INITIALIZE arr_4[cont_3].* TO NULL
    INITIALIZE arr_3[cont_3].* TO NULL
    INITIALIZE l_record.* TO NULL
END FUNCTION
#####################################################
FUNCTION proceso()
    LET HOY = DATE

    OPEN WINDOW ventana_1 AT 3,3 WITH FORM "UNIC0021" ATTRIBUTE(BORDER)

    DISPLAY "  [ Esc ] Grabar                                        [ Ctrl-C ] Salir    " AT 1,1 ATTRIBUTE(REVERSE)

    DISPLAY " UNIC002          RESULTADOS DE CONFRONTA PARA UNIFICACION                     " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    INPUT BY NAME reg_1.* WITHOUT DEFAULTS
       ON KEY (ESC)
          EXIT INPUT
       ON KEY (INTERRUPT)
          PROMPT " PROCESO CANCELADO  [ ENTER ]PARA SALIR " ATTRIBUTE(REVERSE)
          FOR CHAR enter
          EXIT PROGRAM
    END INPUT

    CALL primer_paso()
END FUNCTION
#####################################################
FUNCTION primer_paso()
   DEFINE cuantos      SMALLINT,
          marca_ar     CHAR(3),
          x_pos        SMALLINT,
          flag1        SMALLINT,
          flag2        SMALLINT,
          rev          SMALLINT

   LET flag1 = 0

   ERROR " PROCESANDO INFORMACION "

   DECLARE cur_1 CURSOR FOR
   SELECT a.nss_uni,
          a.paterno_uni,
          a.materno_uni,
          a.nombre_uni,
          a.nombre_imss_uni,
          a.cve_ent_nss,
          a.cve_afo_aclara,
          " "
   FROM   uni_unificador a
   WHERE  a.folio           = reg_1.folio
   ---------AND    a.status_convoca = 1  #caso de cta1 verificar status
   AND    a.estado          = 10 
   AND    a.estado_familia  = 1
   ORDER BY 1
 
   LET cont_1 = 1
   FOREACH cur_1 INTO arr_1[cont_1].*

      LET arr_2[cont_1].nss_uni = arr_1[cont_1].nss_uni
      LET arr_2[cont_1].cve_afo = arr_1[cont_1].cve_ent_nss

       IF  arr_2[cont_1].marca   >= 700 AND
           arr_2[cont_1].marca   <= 799 THEN
            LET arr_2[cont_1].cve_afo = arr_1[cont_1].cve_afo_aclara
       END IF

       LET arr_2[cont_1].nombre  = arr_1[cont_1].paterno_uni CLIPPED," ",
                                   arr_1[cont_1].materno_uni CLIPPED," ",
                                   arr_1[cont_1].nombre_uni CLIPPED

       IF arr_2[cont_1].nombre IS NULL 
       OR arr_2[cont_1].nombre = " "   THEN
           LET arr_2[cont_1].nombre = arr_1[cont_1].nombre_imss
       END IF

       INSERT INTO tmp_uni_unificador VALUES(arr_1[cont_1].*)

       LET cont_1 = cont_1 + 1
   END FOREACH
   LET cont_1 = cont_1 - 1

   IF cont_1 >= 1 THEN
     CALL SET_COUNT(cont_1)

      DISPLAY " clave     nss     clave      nombre                                            " AT 7,1 ATTRIBUTE(REVERSE)
      DISPLAY " recep  unificador afore   unificador                         Unificados        " AT 8,1 ATTRIBUTE(REVERSE)

      LET arc_1 = 0

      INPUT ARRAY arr_2 WITHOUT DEFAULTS FROM scr_1.*
      ATTRIBUTE(MAXCOUNT = cont_1, COUNT = cont_1)

         BEFORE FIELD marca
            LET arc_1 = ARR_CURR()
            LET scr_1 = SCR_LINE()

         AFTER FIELD marca
            IF arr_2[arc_1].marca IS NULL THEN
            ELSE
               LET arc_1 = ARR_CURR()
               LET l_record.nss_prueba = arr_2[arc_1].nss_uni

               SELECT "X"
               FROM   uni_unificado
               WHERE  nss_uni = l_record.nss_prueba
               AND    folio   = reg_1.folio 
               AND    estado  = 10
               GROUP BY 1

               IF SQLCA.SQLCODE = 0 THEN
                 CALL segundo_paso(l_record.nss_prueba) 
                    RETURNING marca_ar,
                              flag1

                 LET arr_2[arc_1].marca = marca_ar
                 LET flag2 = flag1 + 1
               END IF

               #LET flag2 = 0  --desabilitado temporalmente(parte del control-p)

               IF arr_2[arc_1].marca = "000" THEN
               ELSE
                  SELECT "X"
                  FROM   uni_unificador a
                  WHERE  a.nss_uni     = arr_1[arc_1].nss_uni
                  AND    a.folio       = reg_1.folio
                  AND    a.cve_ent_nss = arr_2[arc_1].marca
                  GROUP BY 1

                  IF STATUS = NOTFOUND THEN
                     SELECT "X"
                     FROM   uni_unificado a
                     WHERE  a.nss_uni      = arr_1[arc_1].nss_uni
                     AND    a.folio        = reg_1.folio
                     AND    a.cve_ent_cta1 = arr_2[arc_1].marca
                     GROUP BY 1

                     IF STATUS = NOTFOUND THEN
                        ERROR" CLAVE DE LA AFORE INCORRECTA "
                        NEXT FIELD marca
                     END IF
                  END IF
               END IF
            END IF

       ON KEY (ESC)
          IF flag2 > 1 THEN
             FOR cont_2 = 1 TO cont_1
                 IF arr_2[cont_2].marca >= 500 AND
                    arr_2[cont_2].marca <= 599 THEN

                       UPDATE uni_unificador
                       SET    cve_afo_recep = arr_2[cont_2].marca,
                              estado        = 20
                       WHERE  nss_uni       = arr_1[cont_2].nss_uni
                       AND    folio         = reg_1.folio 
                 END IF
                 IF arr_2[cont_2].marca = 000  THEN

                       UPDATE uni_unificador
                       SET    cve_afo_recep = arr_2[cont_2].marca,
                              estado        = 20
                       WHERE  nss_uni       = arr_1[cont_2].nss_uni
                       AND    folio         = reg_1.folio 
                 END IF
             END FOR

             PROMPT " PROCESO FINALIZADO ...<ENTER> PARA SALIR " 
             ATTRIBUTE(REVERSE) FOR CHAR enter
             EXIT INPUT
          ELSE
             ERROR "El campo marca no puede ser NULO ..."
             NEXT FIELD marca   
          END IF

       ON KEY (INTERRUPT)
          DECLARE tmp_reverso CURSOR FOR
          SELECT *
          FROM   tmp_uni_unificador
 
          LET rev = 1

          IF flag1 = 1 THEN
             FOREACH tmp_reverso INTO arr_1[rev].*
                WHENEVER ERROR CONTINUE

                UPDATE uni_unificado
                SET    estado = 10
                WHERE  nss_uni = arr_1[rev].nss_uni
                AND    folio   = reg_1.folio 
                AND    estado  = 20

                WHENEVER ERROR STOP
                LET rev = rev + 1
             END FOREACH

             WHENEVER ERROR CONTINUE
             DROP TABLE tmp_uni_unificador
             WHENEVER ERROR STOP

             PROMPT " PROCESO CANCELADO ...<ENTER> PARA SALIR " 
             ATTRIBUTE(REVERSE) FOR enter
          ELSE
             PROMPT " PROCESO CANCELADO ...<ENTER> PARA SALIR " 
             ATTRIBUTE(REVERSE) FOR enter
          END IF

          CALL Inicializa()
          EXIT INPUT
{---desabilitado temporalmente
       ON KEY (CONTROL-P)
          LET arc_1 = ARR_CURR()
          LET l_record.nss_prueba = arr_2[arc_1].nss_uni
          LET x_pos = arc_1
          ---------LET l_record.marca      = arr.marca
          CALL segundo_paso(l_record.nss_prueba) 
             RETURNING marca_ar,
                       flag1
          LET arr_2[x_pos].marca = marca_ar
          LET flag2 = flag1 + 1
          ---------CALL segundo_paso(l_record.nss_prueba,l_record.marca) 
          CALL Inicializa1()
}
      END INPUT
   ELSE
      PROMPT " NO EXISTEN REGISTROS ...<ENTER> PARA SALIR " ATTRIBUTE(REVERSE) 
      FOR CHAR enter
   END IF

   CLOSE WINDOW ventana_1
END FUNCTION
#####################################################
FUNCTION segundo_paso(nss_prueba)
#sp------------------------------
   DEFINE nss_prueba         CHAR(11),
          x_marca            CHAR(2),
          marca_ar           CHAR(3),
          flag1              SMALLINT

   OPEN WINDOW ventana_2 AT 11,10 WITH FORM "UNIC0022" ATTRIBUTE(BORDER)
   DISPLAY "     [ Esc ] Grabar                   [ Ctrl-C ] Salir                          " AT 1,1  ATTRIBUTE(REVERSE)
   DISPLAY "diag      nss    afore     nombre                                              " AT 5,1  ATTRIBUTE(REVERSE)
   ERROR "BUSCANDO INFORMACION"

   UPDATE uni_cza_notifica
   SET    estado = 20
   WHERE  folio  = reg_1.folio

   UPDATE uni_sum_notifica
   SET    estado = 20
   WHERE  folio  = reg_1.folio

   DECLARE cur_2 CURSOR FOR
   SELECT b.diag_unifica,
          b.nss_cta1,
          b.paterno_cta1,
          b.materno_cta1,
          b.nombre_cta1,
          b.nombre_imss_cta1,
	  b.cve_ent_cta1
   FROM   uni_unificado b
   WHERE  b.nss_uni = nss_prueba
   AND    b.folio   = reg_1.folio
   AND    b.estado  = 10 
   AND    b.estado_unifica  = 1

   LET cont_3 = 1
   FOREACH cur_2 INTO arr_4[cont_3].*

      LET arr_3[cont_3].marca_cta1  = arr_4[cont_3].diag_unifica
      LET arr_3[cont_3].nss_cta1    = arr_4[cont_3].nss_cta1
      LET arr_3[cont_3].cve_afo1    = arr_4[cont_3].cve_ent_cta1
      LET arr_3[cont_3].nombre_cta1 = arr_4[cont_3].paterno_cta1 CLIPPED," ",
                                      arr_4[cont_3].materno_cta1 CLIPPED," ",
                                      arr_4[cont_3].nombre_cta1 CLIPPED
       
       IF arr_3[cont_3].nombre_cta1 IS NULL 
       OR arr_3[cont_3].nombre_cta1 = " "   THEN
           LET arr_3[cont_3].nombre_cta1 = arr_4[cont_3].nombre_imss
       END IF

      LET cont_3 = cont_3 + 1
   END FOREACH

   LET cont_3 = cont_3 - 1
   CALL SET_COUNT(cont_3)
   DISPLAY BY NAME nss_prueba

   INPUT ARRAY arr_3 WITHOUT DEFAULTS FROM scr_2.*
   ATTRIBUTE(MAXCOUNT = cont_3,COUNT = cont_3)

      BEFORE FIELD marca_cta1
         LET arc_2 = ARR_CURR()
         LET scr_2 = SCR_LINE()

         IF x_marca = "01" THEN
            LET arr_3[arc_2].marca_cta1 = x_marca 
            LET marca_ar = arr_2[arc_1].marca
         END IF
         IF x_marca = "02" THEN
            LET arr_3[arc_2].marca_cta1 = x_marca 
            LET marca_ar = "000"
         END IF
         IF x_marca = "05" THEN
            LET arr_3[arc_2].marca_cta1 = x_marca 
            LET marca_ar = "000"
         END IF

      AFTER FIELD marca_cta1
         IF arr_3[arc_2].marca_cta1 IS NULL THEN
            ERROR" SOLO PUEDE MARCAR 01,02,05"
            NEXT FIELD marca_cta1
         END IF

         LET x_marca = arr_3[arc_2].marca_cta1

         #IF marca1 >= 500 AND
            #marca1 <= 599 THEN
            #LET arr_3[arc_2].marca_cta1 = "01"
         #ELSE
	 #END IF

      ON KEY (ESC)
         FOR cont_4 = 1 TO cont_3
             IF arr_3[cont_4].marca_cta1 IS NOT NULL THEN
                 IF arr_3[cont_4].marca_cta1 <> "03" THEN
                     UPDATE uni_unificado
                     SET    diag_unifica  = arr_3[cont_4].marca_cta1,
                            estado        = 20
                     WHERE  nss_cta1      = arr_3[cont_4].nss_cta1
                     AND    folio         = reg_1.folio 
 
                     LET flag1 = 1
                 END IF
              END IF
         END FOR
      
         EXIT INPUT   
         CLOSE WINDOW ventana_2
 
      ON KEY (INTERRUPT)
         CALL Inicializa1()
         EXIT INPUT
         CLOSE WINDOW ventana_2
     END INPUT

     CLOSE WINDOW ventana_2
     RETURN marca_ar,
            flag1
END FUNCTION
