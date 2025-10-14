################################################################################
#Proyecto          => SISTEMA DE AFORES( MEXICO )                              #
#Propietario       => EFP                                                      #
#Programa SEPC002  => REGISTRO DE CONFRONTA                                    #
#Fecha creacion    => 08 DE AGOSTO DEL 2000                                    #
#Por               => ARMANDO RODRIGUEZ CASTROPAREDES                          #
#Modicado  Por     => MARCO ANTONIO GONZALEZ ROJAS                             #
#Sistema           => SEP                                                      #
################################################################################
#Modificacion      => Se agrega funcion para insertar en sep_bitacora          #
#                  => un historial de los cambios de estado que se hacen       #
#                  => en sep_det_reg_sol_reclamante                            #
#Autor             => Carlos Trejo Meixueiro                                   #
#Fecha             => 14 Marzo 2013                                            #
#Requerimiento     => INV-1872                                                 #
################################################################################
#Modificacion      => Se agrega funcion para insertar en sep_reclasifica       #
#                  => un registro de cuando es reclasificacion                 #
#Autor             => Carlos Trejo Meixueiro                                   #
#Fecha             => 06 Junio 2013                                            #
#Requerimiento     => INV-1944                                                 #
#Req CPL-2076 => CMR 15/10/2015 adecuacion para nvo diagnostico por nueva plata#
################################################################################


DATABASE safre_af

    DEFINE g_construct char(100)

    DEFINE l_construct char(1000)

    DEFINE band   smallint

    DEFINE arr_corr ARRAY[2500] OF RECORD
       idSolicitudSeparacion INTEGER
    END RECORD

    DEFINE arr_1 ARRAY[2500] OF RECORD
	diag_confronta        CHAR(02),
	clasifica_separacion  CHAR(01),
        n_seguro              CHAR(11),
        nss                   CHAR(11),
        fecha_marca_infosar   date   ,
        estado                SMALLINT,
        des_estado            CHAR(20)
    END RECORD

    DEFINE arr_2 ARRAY[2500] OF RECORD
        diag_confronta        CHAR(02),
        clasifica_separacion  CHAR(01),
        n_seguro              CHAR(11),
        nss                   CHAR(40),
        fecha_marca_infosar   date   ,
        estado                SMALLINT,
        des_estado            CHAR(20)
    END RECORD


    DEFINE arc_1              ,
           lastkey            ,
           scr_1              ,
           total_pa           ,
           vcodigo            ,
           cont_1             ,
           i                  ,
           g_env_tip_rep      ,
           cont_2             SMALLINT,
           HOY                DATE,
           enter              CHAR(01),
           g_env_nss_o_fol    ,
           g_ruta_arch        CHAR(100),
           g_lanza_rep        CHAR(1500),
           gs_afore           SMALLINT

MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP,
        PROMPT LINE LAST,
        ACCEPT KEY CONTROL-I

    CALL STARTLOG(FGL_GETENV('USER')||".SEPC002.log")
    CALL init()
    CALL proceso()
END MAIN

FUNCTION Inicializa()
    INITIALIZE arr_1[cont_1].* TO NULL
    INITIALIZE arr_2[cont_1].* TO NULL
END FUNCTION


FUNCTION proceso()

 WHILE TRUE
    OPEN WINDOW ventana_1 AT 3,3 WITH FORM "SEPC0021" ATTRIBUTE(BORDER)

    DISPLAY "[ Esc ] Grabar                                              [ Ctrl-C ] Salir" AT 1,1 ATTRIBUTE(REVERSE)

    DISPLAY " SEPC002   RESULTADOS DE CONFRONTA PARA SEPARACION DE CUENTAS               " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,67 ATTRIBUTE(REVERSE)


    LET int_flag = FALSE

    CONSTRUCT BY NAME g_construct ON a.n_seguro             ,
                                     b.nss                  ,
                                     a.fecha_marca_infosar  ,
                                     b.estado
    ON KEY (ESC)
       EXIT CONSTRUCT
    ON KEY (INTERRUPT)
       EXIT PROGRAM
    END CONSTRUCT

    call primer_paso()

 END WHILE

END FUNCTION

FUNCTION primer_paso()
#pp-------------------
    DEFINE cuantos         ,
           l_estado_ant    ,
           l_diag_conf     ,
           ls_estado       SMALLINT,      #CPL-2076
           l_clas_sep      CHAR(2) ,
           lc_diag_ant     CHAR(02),
           lc_clas_ant     CHAR(01)

    ERROR " PROCESANDO INFORMACION "

    CALL hay_inf_s_o_n()

   if cont_1 = 1 then
      ERROR ""
      PROMPT "Sin registros...<Enter> para continuar..." for char enter
      close window ventana_1
      return

   end if

    ERROR ""

    CALL SET_COUNT(cont_1-1)

  WHILE TRUE
    LET arc_1 = 0
   LET band = 0
    DISPLAY "<CTRL -N> Vaciar Movimientos del Invadido" AT 5,1
    DISPLAY "<CTRL -E> Mod NSS Asociado <CTRL-P>Reenviar" AT 1, 17 ATTRIBUTE (REVERSE)       #CPL-2076
    INPUT ARRAY arr_2 WITHOUT DEFAULTS FROM scr_1.*

       ON KEY ( CONTROL-N )
          LET i                           =                    ARR_CURR()
          LET g_env_tip_rep               =                    1
          LET g_env_nss_o_fol             =                    arr_2[i].n_seguro
          PROMPT "Desea Generar Reporte del Invadido: ",g_env_nss_o_fol CLIPPED ," Seleccionado [S/N] ? "  FOR enter
             IF ( enter MATCHES "[sSnN]" ) THEN
                IF ( enter MATCHES "[sS]" ) THEN
                   ERROR "Procesando Vaciado de Movimientos de Aportes e Icefas..."
                   LET 	g_lanza_rep       =     "fglgo SEPF003" CLIPPED,g_env_tip_rep CLIPPED," ",g_env_nss_o_fol
                   RUN  g_lanza_rep
                   ERROR "Generado en  ==>",g_ruta_arch
                   SLEEP 3
                   ERROR " "
                   DISPLAY "                                                " AT 18,1
                   ELSE
                   ERROR "CANCELANDO ..."  ATTRIBUTE(REVERSE)
                   SLEEP 2
                   ERROR " "
                   DISPLAY "                                                " AT 18,1
                END IF
             END IF

       BEFORE FIELD diag_confronta
          LET arc_1 = ARR_CURR()
          LET scr_1 = SCR_LINE()
          LET total_pa = ARR_COUNT()
          DISPLAY BY NAME arc_1
          DISPLAY BY NAME total_pa

       AFTER FIELD diag_confronta
          LET arc_1 = ARR_CURR()

         IF arr_2[arc_1].diag_confronta = "02" THEN   #inv-3417

            IF arr_1[arc_1].diag_confronta = "01" THEN
               ERROR "NO SE PUEDE CAMBIAR EL VALOR DEL DIAGNOSTICO"
               NEXT FIELD diag_confronta
            END IF
         END IF


          IF arc_1 >= (total_pa) THEN
             LET  lastkey = FGL_LASTKEY()
                IF  ((lastkey = FGL_KEYVAL("down"))
                     OR  (lastkey = FGL_KEYVAL("return"))
                     OR  (lastkey = FGL_KEYVAL("tab"))
                     OR  (lastkey = FGL_KEYVAL("right")))
                     THEN
                           ERROR "No hay mas registros en esa direccion."
                           NEXT FIELD clasifica_separacion
                END IF
          END IF

      AFTER FIELD clasifica_separacion
          LET arc_1 = ARR_CURR()

         IF arr_2[arc_1].diag_confronta = "01" THEN

            IF arr_1[arc_1].clasifica_separacion = "D" THEN
               IF NOT arr_2[arc_1].clasifica_separacion = "B" THEN
                 IF NOT arr_2[arc_1].clasifica_separacion = "C" THEN
                  ERROR "O SE PUEDE CAMBIAR A ESTA CLASIFICACION"
                  NEXT FIELD clasifica_separacion
                 END IF
               END IF
            END IF
            IF arr_1[arc_1].clasifica_separacion = "A" THEN
               IF NOT arr_2[arc_1].clasifica_separacion = "E" THEN
               ERROR "NO SE PUEDE CAMBIAR A ESTA CLASIFICACION"
               NEXT FIELD clasifica_separacion
               END IF
            END IF

            --IF arr_1[arc_1].clasifica_separacion = "B" THEN
               --IF NOT arr_2[arc_1].clasifica_separacion = "C" THEN
                  --ERROR "NO SE PUEDE CAMBIAR A ESTA CLASIFICACION"
                  --NEXT FIELD clasifica_separacion
               --END IF
            --END IF

            IF arr_1[arc_1].clasifica_separacion = "C" THEN
               IF NOT arr_2[arc_1].clasifica_separacion = "B" THEN
                  ERROR "NO SE PUEDE CAMBIAR A ESTA CLASIFICACION"
                  NEXT FIELD clasifica_separacion
               END IF
            END IF

            IF arr_1[arc_1].clasifica_separacion = "E" THEN
               ERROR "NO SE PUEDE CAMBIAR A ESTA CLASIFICACION"
               NEXT FIELD clasifica_separacion
            END IF


           IF (arr_2[arc_1].clasifica_separacion IS NULL OR
               arr_2[arc_1].clasifica_separacion = " " OR
               arr_2[arc_1].clasifica_separacion = "" ) THEN

                ERROR" CON DIAG 01 CLASIFICAR COMO  A,B,C,D o E"
                NEXT FIELD clasifica_separacion
           END IF
         ELSE
            IF  (arr_2[arc_1].clasifica_separacion IS NULL OR
                 arr_2[arc_1].clasifica_separacion = " "   OR
                 arr_2[arc_1].clasifica_separacion = ''   ) THEN
            ELSE
               ERROR" CLASIFICACION NO PROCEDE CON DIAG <> 01"
               NEXT FIELD clasifica_separacion
            END IF
         END IF
         IF arc_1 >= (total_pa) THEN
              LET  lastkey = FGL_LASTKEY()
                      IF  ((lastkey = FGL_KEYVAL("down"))
                       OR  (lastkey = FGL_KEYVAL("return"))
                       --OR  (lastkey = FGL_KEYVAL("up"))
                       OR  (lastkey = FGL_KEYVAL("tab"))
                       OR  (lastkey = FGL_KEYVAL("right")))

                      THEN
                        ERROR "No hay mas registros en esa direccion."
                        NEXT FIELD diag_confronta
                      END IF
         ELSE
              LET  lastkey = FGL_LASTKEY()
                      IF  ((lastkey = FGL_KEYVAL("down"))
                       OR  (lastkey = FGL_KEYVAL("up")))
                      THEN
                        NEXT FIELD diag_confronta
                      END IF

	 END IF

      ON KEY (CONTROL-E)  #CPL-2076
      IF arr_1[arc_1].estado = 1 THEN
         ERROR "SOLO SOLICITUDES CON RESP OP 27"
         SLEEP 2
         ERROR ""
      ELSE

         CALL fn_modifica_nss_reclamante(arr_1[arc_1].n_seguro                 ,
                                         arr_corr[arc_1].idSolicitudSeparacion ,
                                         arr_1[arc_1].nss)
         RETURNING arr_2[arc_1].nss
         DISPLAY BY NAME arr_2[arc_1].nss
      END IF

      ON KEY (CONTROL-P) #CPL-2076
      IF arr_1[arc_1].estado = 1 THEN
         ERROR "SOLO SOLICITUDES RECHAZADAS ..."
         SLEEP 2
         ERROR ""
      ELSE
           WHILE  TRUE
               PROMPT "ESTA SEGURO DE REENVIAR REGISTRO [S/N] ? " FOR enter
               IF      enter  MATCHES "[sSnN]" THEN
                       IF      enter  MATCHES "[sS]" THEN
                               LET band = 0
                               EXIT WHILE
                       ELSE
                               ERROR  "REGRESO A CAPTURA" ATTRIBUTE(REVERSE)
                               SLEEP  2
                               LET band = 1
                               EXIT WHILE
                       END IF
              END IF
           END WHILE
         IF band = 0 THEN
            UPDATE sep_det_reg_sol_reclamante
            SET estado = 3
            WHERE correlativo = arr_corr[arc_1].idSolicitudSeparacion

            ERROR "REGISTRO ACTUALIZADO PARA REENVIO ..."
            SLEEP 1
            ERROR ""
            EXIT INPUT
         END IF 
       END IF

      ON KEY (ESC)
      LET band = 0
  FOR arc_1 = 1 to total_pa
         IF arr_2[arc_1].diag_confronta = "01" THEN
           IF (arr_2[arc_1].clasifica_separacion IS NULL OR
               arr_2[arc_1].clasifica_separacion = " " OR
               arr_2[arc_1].clasifica_separacion = "" ) THEN
                LET band = 1
                EXIT FOR
           END IF
         ELSE
            IF  (arr_2[arc_1].clasifica_separacion IS NULL OR
                 arr_2[arc_1].clasifica_separacion = " "   OR
                 arr_2[arc_1].clasifica_separacion = ''   ) THEN
            ELSE
                LET band = 1
                EXIT FOR
            END IF
         END IF
  END FOR

  IF band = 1 THEN
   ERROR "INCONSISTENCIAS EN LA CLASIFICACION Y/O DIAGNOSTICO...REVISAR..."
   EXIT INPUT
  END IF
           WHILE  TRUE
               PROMPT "ESTA SEGURO DE GENERAR EL PROCESO [S/N] ? " FOR enter
               IF      enter  MATCHES "[sSnN]" THEN
                       IF      enter  MATCHES "[sS]" THEN
                               LET band = 0
                               EXIT WHILE
                       ELSE
                               ERROR  "REGRESO A CAPTURA" ATTRIBUTE(REVERSE)
                               SLEEP  2
                               LET band = 1
                               EXIT WHILE
                       END IF
              END IF
           END WHILE

         IF band = 0 THEN
          FOR cont_2 = 1 TO cont_1-1
             IF arr_2[cont_2].diag_confronta IS NOT NULL THEN
                 IF arr_2[cont_2].diag_confronta <> "00" THEN
                     SELECT "OK"
                     FROM   sep_mod_o27  a
                      WHERE a.idSolicitudSeparacion = arr_corr[cont_2].idSolicitudSeparacion
                      AND   a.diag_confronta        = arr_2[cont_2].diag_confronta
                      AND   a.clasifica_separacion  = arr_2[cont_2].clasifica_separacion

                     IF STATUS = NOTFOUND THEN

                           LET l_diag_conf = NULL
                           LET l_clas_sep = NULL
#--                             INV-1944 INI                                 --#
                           SELECT diag_confronta, clasifica_separacion
                             INTO l_diag_conf, l_clas_sep
                             FROM sep_det_solicitud
                            WHERE idSolicitudSeparacion = arr_corr[cont_2].idSolicitudSeparacion
                            IF l_diag_conf IS NOT NULL THEN
                               IF l_clas_sep IS NOT NULL THEN
                                  CALL f_inserta_sep_reclasifica (arr_corr[cont_2].idSolicitudSeparacion)
                               END IF
                            END IF
#--                             INV-1944 FIN                                 --#

                     UPDATE sep_det_solicitud
                     SET    diag_confronta        = arr_2[cont_2].diag_confronta,
                            clasifica_separacion  = arr_2[cont_2].clasifica_separacion
                     WHERE  idSolicitudSeparacion = arr_corr[cont_2].idSolicitudSeparacion

#--                             INV-1872 INI                                 --#
                        SELECT estado
                          INTO l_estado_ant
                          FROM safre_af:sep_det_reg_sol_reclamante
                         WHERE correlativo = arr_corr[cont_2].idSolicitudSeparacion
#--                             INV-1872 FIN                                 --#

                     --CPL-2076--
                     IF arr_2[cont_2].clasifica_separacion = "E" THEN
                        LET ls_estado = 57
                     ELSE
                        LET ls_estado = 3
                     END IF
                     --CPL-2076--


                     UPDATE sep_det_reg_sol_reclamante
                     SET    estado      =  ls_estado
                     WHERE  correlativo = arr_corr[cont_2].idSolicitudSeparacion
                        IF gs_afore != 568 THEN     #CPL-2076
                           IF SQLCA.SQLERRD[3] = 1 THEN#--INV-1872
                              CALL f_inserta_sep_bitacora(arr_corr[cont_2].idSolicitudSeparacion, 27, l_estado_ant, ls_estado) #CPL-2076
                           END IF
                        END IF

                    END IF
                 END IF

              END IF
           END FOR
        ELSE
           EXIT INPUT
        END IF

      EXIT INPUT
       ON KEY (INTERRUPT)
         CALL Inicializa()
	      LET band = 0
         EXIT INPUT
   END INPUT
    IF band = 0 THEN
       EXIT WHILE
    END IF
END WHILE
    IF cont_1 = 1 THEN
        PROMPT " NO EXISTEN REGISTROS ...<ENTER> PARA SALIR "
        ATTRIBUTE(REVERSE)
        FOR CHAR enter
    END IF

    --EXIT PROGRAM
    CLOSE WINDOW ventana_1

END FUNCTION
################################################################################
#h_i_s_o_n------------
FUNCTION hay_inf_s_o_n()
DEFINE l_diag_conf     SMALLINT,
       l_clas_sep      CHAR(2)

   SELECT a.codigo_afore
   INTO   vcodigo
   FROM   tab_afore_local a

    WHENEVER ERROR CONTINUE
     DROP TABLE sep_mod_o27
    WHENEVER ERROR STOP


    CREATE TEMP TABLE sep_mod_o27  (
    idSolicitudSeparacion integer   ,
    diag_confronta char(002),
    clasifica_separacion    char(001) )

    UPDATE sep_det_solicitud
    SET    diag_confronta = " "
    WHERE  diag_confronta is null or diag_confronta = ""

    UPDATE sep_det_solicitud
    SET    clasifica_separacion = " "
    WHERE  clasifica_separacion is null or clasifica_separacion = ""


   LET l_construct =
   " SELECT unique a.diag_confronta, ",
   "   a.clasifica_separacion, ",
   "   a.n_seguro, ",
   "   b.nss    , ",
   "   a.fecha_marca_infosar, ",
   "   b.estado , ",
   "   c.des_estado , ",
   "   b.correlativo         ",
   "  FROM   sep_det_solicitud a , ",
   "         sep_det_reg_sol_reclamante b , ",
   "         sep_estado_separacion c  ",
   "  WHERE  ",g_construct  ,
   "  AND    b.correlativo = a.idSolicitudSeparacion ",
   "  AND    b.estado  in (1,5,52,53,6) ",
   "  AND    b.estado = c.estado " ,
   " ORDER BY 1 "

   PREPARE qry_construct FROM l_construct
   DECLARE cur_1 CURSOR FOR qry_construct

   LET cont_1 = 1

   FOREACH cur_1 INTO arr_1[cont_1].*,arr_corr[cont_1].*

      LET arr_2[cont_1].diag_confronta        = arr_1[cont_1].diag_confronta
      LET arr_2[cont_1].clasifica_separacion  = arr_1[cont_1].clasifica_separacion
      LET arr_2[cont_1].n_seguro             = arr_1[cont_1].n_seguro
      LET arr_2[cont_1].nss                   = arr_1[cont_1].nss
      LET arr_2[cont_1].fecha_marca_infosar   = arr_1[cont_1].fecha_marca_infosar
      LET arr_2[cont_1].estado                = arr_1[cont_1].estado
      LET arr_2[cont_1].des_estado            = arr_1[cont_1].des_estado

     INSERT INTO sep_mod_o27 VALUES (arr_corr[cont_1].idSolicitudSeparacion ,
                                     arr_1[cont_1].diag_confronta           ,
                                     arr_1[cont_1].clasifica_separacion )

      LET cont_1 = cont_1 + 1

   END FOREACH


END FUNCTION
################################################################################
FUNCTION init()

   LET band                           =                            0

   LET HOY                            =                            TODAY

   SELECT A.ruta_listados
   INTO   g_ruta_arch
   FROM   seg_modulo A
   WHERE  A.modulo_cod = "sep"

   SELECT   codigo_afore
   INTO     gs_afore
   FROM     safre_af:tab_afore_local
   GROUP BY 1

END FUNCTION

######################################################################--INV-1872
FUNCTION f_inserta_sep_bitacora(l_id, l_operacion, l_estado_anterior, l_estado_actualiza)

DEFINE l_id                 INTEGER,
       l_operacion          SMALLINT,
       l_estado_anterior    SMALLINT,
       l_estado_actualiza   SMALLINT,
       l_programa           CHAR(15)

LET l_programa = ARG_VAL(0)

INSERT INTO safre_af:sep_bitacora
VALUES (
l_id,
l_operacion,
l_estado_anterior,
l_estado_actualiza,
CURRENT,
l_programa,
USER)

END FUNCTION

######################################################################--INV-1944
FUNCTION f_inserta_sep_reclasifica (id_solicitud)

DEFINE id_solicitud       ,
       l_id_ok            INTEGER

SELECT UNIQUE id
  INTO l_id_ok
  FROM safre_af:sep_reclasifica
 WHERE id = id_solicitud

IF l_id_ok = id_solicitud THEN
 ELSE
   INSERT INTO sep_reclasifica VALUES (id_solicitud, CURRENT, USER)
END IF

END FUNCTION

FUNCTION fn_modifica_nss_reclamante(lc_nseguro,li_correlativo,lc_nss)  #CPL-2076
#cnr---------------------------

DEFINE vnss
       ,lc_nseguro
       ,lc_nss          CHAR(011)
       ,li_correlativo  INTEGER
       ,ls_band         SMALLINT

      LET vnss = lc_nss
      LET ls_band = 0
  OPEN WINDOW sepb0123 AT 12,20 WITH FORM "SEPB0123" ATTRIBUTE(BORDER)
  DISPLAY "[ Esc ] Grabar        [ Ctrl-C ] Salir" AT 1,1 ATTRIBUTE(REVERSE)
  DISPLAY "           NSS A MODIFICAR            " AT 4,1

       INPUT BY NAME vnss WITHOUT DEFAULTS
       --BEFORE FIELD vnss


       AFTER FIELD   vnss
       ON KEY (ESC)

         IF vnss IS NULL OR
            vnss = " " THEN
             ERROR  "FAVOR DE REGISTRAR NSS "
             SLEEP 2
             ERROR ""
             NEXT FIELD vnss
         END IF
         WHILE TRUE
           PROMPT "ESTA SEGURO S/N ? " FOR CHAR enter
           IF enter MATCHES "[sSnN]" THEN
            IF enter MATCHES "[sS]" THEN
               EXIT WHILE
            ELSE
             LET vnss = NULL
             DISPLAY BY NAME vnss
             NEXT FIELD vnss
            END IF
           END IF
         END WHILE

         SELECT "OK"
         FROM   sep_det_reg_sol_reclamante  A
         WHERE  A.n_seguro = lc_nseguro
         AND    A.nss      = vnss

         IF STATUS <> NOTFOUND THEN
            ERROR "Registro Duplicado..."
            SLEEP 2
            ERROR ""
            NEXT FIELD vnss
         END IF

         UPDATE sep_det_reg_sol_reclamante
         SET    nss   = vnss
         WHERE  n_seguro = lc_nseguro
         AND    correlativo =  li_correlativo

         IF SQLCA.SQLERRD[3] = 1 THEN

             INSERT INTO sep_bit_nss_mod
             VALUES (li_correlativo       ,
                     lc_nseguro           ,
                     lc_nss               ,
                     vnss                 ,
                     arr_1[arc_1].estado  ,
                     TODAY                ,
                     USER                  )

             ERROR "NSS MODIFICADO..."
             SLEEP 2
             ERROR ""
         END IF


        EXIT INPUT

      ON KEY (INTERRUPT)
                  LET vnss = " "
                  DISPLAY BY NAME vnss
                  LET ls_band = 1
                  EXIT INPUT
       END INPUT
    CLOSE WINDOW sepb0123
    IF ls_band = 0 THEN
      RETURN vnss
   ELSE
      RETURN lc_nss
   END IF

END FUNCTION
