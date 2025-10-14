#############################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                          #
#Propietario       => E.F.P.                                                #
#Programa AFIB003  => TRASPASA AFILIADOS APROBADOS POR AL MAESTRO           #
#Sistema           => AFI.                                                  #
#Autor             => MAURO MUNIZ CABALLERO                                 #
#Fecha             => 17 DE ENERO DE 2001                                   #
#Autor             => MAURO MUNIZ CABALLERO (Proceso batch)                 #
#Modificado        => ISABEL FONSECA    / EDUARDO RESENDIZ 03-12-04         #
#Fecha             => 19 DE NOVIEMBRE DE 2004                               # 
#Fecha 						 => CPL-1368 23/07/2013 FSR
#############################################################################

DATABASE safre_af

GLOBALS

    DEFINE reg_fn_fnacimiento RECORD 
            existe   ,    
            edad     ,
            criterio , 
            indicador_edad  SMALLINT,
            curp    CHAR(018) ,
            rfc    CHAR(013) ,
            fena   DATE  
    END RECORD
    DEFINE reg_cancelada RECORD LIKE cta_act_marca.*
    DEFINE 
        enter       CHAR(1),
        generar     CHAR(1),
        aux_pausa   CHAR(1),
        opc         CHAR(1),
        g_usuario   CHAR(8),
        HORA        CHAR(8),
        vnss        CHAR(11),
        operacion   CHAR(40),
        v_sql_2     CHAR(50),
        v_sql_3     CHAR(50),
        HOY         DATE,
        f_ini_tmte  DATE 

     DEFINE w_aux  RECORD
         n_seguro            LIKE afi_solicitud.n_seguro,
         n_unico             LIKE afi_solicitud.n_unico ,
         n_rfc               LIKE afi_solicitud.n_rfc   ,
         paterno             LIKE afi_solicitud.paterno ,
         materno             LIKE afi_solicitud.materno ,
         nombres             LIKE afi_solicitud.nombres ,
         fena                LIKE afi_solicitud.fena    ,
         sexo                LIKE afi_solicitud.sexo    ,
         frecafor            LIKE afi_solicitud.frecafor,
         status_interno      SMALLINT
     END RECORD

     DEFINE g_afore       RECORD LIKE tab_afore_local.*
     DEFINE g_paramgrales RECORD LIKE seg_modulo.*
     DEFINE g_aficefa     RECORD LIKE afi_icefa.*
     DEFINE gr_ctanssreg  RECORD LIKE cta_nss_regimen.*

     DEFINE reg_bat RECORD
         pid            INTEGER,
         proceso_cod    INTEGER,
         opera_cod      INTEGER,
         nombre_archivo CHAR(25)
     END RECORD

     DEFINE
         bnd_proceso     SMALLINT ,
         pestado_marca   SMALLINT ,
         pcodigo_rechazo SMALLINT ,
         ejecuta         CHAR(300),
         xcodigo_marca   SMALLINT ,
         xcodigo_rechazo SMALLINT ,
         pmarca_entra    SMALLINT ,
         con_curp        SMALLINT ,
         sin_curp        SMALLINT ,
         pmarca_causa    SMALLINT ,
         pfecha_causa    SMALLINT ,
         xmarca_estado   SMALLINT ,
         edo_proc        SMALLINT

    DEFINE consulta_carta CHAR(120)

    DEFINE reg_carta RECORD LIKE int_ctr_carta.*
    DEFINE afi       RECORD LIKE afi_solicitud.*

END GLOBALS

MAIN
    CALL STARTLOG("SEPC105.log")

    CALL inicio()              #i

    IF NOT bnd_proceso THEN
      DISPLAY " "
      DISPLAY ".1"
        DEFER INTERRUPT
        OPTIONS INPUT WRAP,
            PROMPT LINE LAST,
            ACCEPT KEY CONTROL-I

        CALL proceso_principal()   #pp
        --PROMPT "Proceso finalizado, [Enter] para salir"
        --FOR enter
    ELSE
        CALL traspasa_datos()     #rv
        CALL actualiza_bat_f(0) #rv
        DISPLAY "Proceso finalizado"
    END IF

    CALL despliega_resultados()

END MAIN

FUNCTION inicio()
#i---------------

    LET reg_bat.pid            = ARG_VAL(1)
    LET reg_bat.proceso_cod    = ARG_VAL(2)
    LET reg_bat.opera_cod      = ARG_VAL(3)

    LET bnd_proceso = 0

    IF reg_bat.pid THEN
        DISPLAY "INICIANDO PROCESO ..."
        LET bnd_proceso = 1
    END IF

    DATABASE safre_tmp

    WHENEVER ERROR CONTINUE
        DROP TABLE afi_2
    WHENEVER ERROR STOP

    CREATE TABLE afi_2
       (n_seguro CHAR(11) ,
        n_unico  CHAR(18) ,
        n_rfc    CHAR(13) ,
        paterno  CHAR(40) ,
        materno  CHAR(40) ,
        nombres  CHAR(40) ,
        fena     DATE     ,
        sexo     SMALLINT ,
        frecafor DATE     ,
        status_interno  SMALLINT);

    DATABASE safre_af

    SELECT *
    INTO   g_paramgrales.*
    FROM   seg_modulo 
    WHERE  modulo_cod = 'afi'

    SELECT *, USER
    INTO   g_afore.*, g_usuario
    FROM   tab_afore_local

    LET HOY      = TODAY
    LET HORA     = TIME

    LET pmarca_entra    = 605
    LET pestado_marca   = 0
    LET pcodigo_rechazo = 0
    LET pmarca_causa    = 280
    LET pfecha_causa    = ""

    LET operacion = 'ALTA EN MAESTRO DE AFILIADOS'

    INITIALIZE reg_carta.* TO NULL
    INITIALIZE gr_ctanssreg.* TO NULL

    LET v_sql_2 = "EXECUTE PROCEDURE fn_regimen_inv(?,?,?,?,?,?)"

    PREPARE stmt2 FROM v_sql_2


    LET v_sql_3 = "EXECUTE PROCEDURE fn_fnacimiento(?,?)"
    PREPARE stmt3 FROM v_sql_3


END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------

    OPEN WINDOW ventana_1 AT 4,4 WITH FORM "SEPC105" ATTRIBUTE(BORDER)
    DISPLAY " SEPB003          TRASPASA AFILIADOS APROBADOS AL MAESTRO                      " AT 3,1 ATTRIBUTE(REVERSE)

    DISPLAY "                                < Ctrl-C > Salir                               " AT 1,2 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"dd-mm-yyyy" AT 3,63 ATTRIBUTE(REVERSE)

    INPUT BY NAME generar
    AFTER FIELD generar
        IF generar NOT MATCHES "[SsNn]" THEN
           ERROR "Opcion solo puede ser [S / N]"
        ELSE
            IF generar MATCHES "[Nn]" THEN
                ERROR "PROCESO CANCELADO"
                SLEEP 2
                EXIT PROGRAM
            ELSE
                ERROR "Procesando Informacion... Espere un momento"

                CALL traspasa_datos() #td
            END IF

            EXIT INPUT
        END IF

        ON KEY ( INTERRUPT )
            EXIT PROGRAM

    END INPUT

END FUNCTION

FUNCTION traspasa_datos()
#td----------------------

    DEFINE
        pat RECORD LIKE afi_patron.* ,
        ben RECORD LIKE afi_beneficiario.*,
        mae RECORD LIKE afi_mae_afiliado.*,
        cta RECORD LIKE cta_ctr_cuenta.*

    DEFINE
        mensaje   CHAR(050),
        G_LISTA   CHAR(300)

    DEFINE
        i             ,
        cont          ,
        HAY           ,
        v_porc        SMALLINT,
        v_sql_1       CHAR(50),
        v_existe      ,
        v_es_menor    ,
        v_criterio    SMALLINT,
        v_crea_fecha  DATE,
        v_tipo_proc   ,
        v_tipo_trasp  ,
        v_medio       ,
        v_cve_siefore ,
        v_cve_sief_i  ,
        v_cve_sief_f  ,
        v_edad        ,
        v_rechazo     SMALLINT,
        v_folioatencion INTEGER

    DEFINE regrowid RECORD
       v_rowid DECIMAL(10,0)
    END RECORD

    DEFINE v_afisolreg RECORD
         nss   LIKE afi_solicitud_regimen.nss,
         fol   LIKE afi_solicitud_regimen.n_folio,
         ts    LIKE afi_solicitud_regimen.tipo_solicitud,
         edo   LIKE afi_solicitud_regimen.estado
    END RECORD

    DEFINE v_ctareg     RECORD LIKE cta_regimen.*

    LET mensaje = "NO SE HICIERON TRASPASOS AL MAESTRO DE AFILIADOS"

    DECLARE cursor_1 CURSOR FOR 
    SELECT rowid,A.* 
    FROM   afi_solicitud A
    WHERE  A.status_interno = 60
    AND    A.tipo_solicitud = 6
    ORDER  BY n_seguro

    FOREACH cursor_1 INTO  regrowid.v_rowid,afi.*

       IF afi.n_unico IS NULL THEN
          LET sin_curp = sin_curp + 1
       ELSE
          LET con_curp = con_curp + 1
       END IF

       LET afi.status = 210

       LET HAY = FALSE

       SELECT COUNT(*)
       INTO   HAY
       FROM   afi_mae_afiliado m
       WHERE  m.n_seguro = afi.n_seguro

       IF HAY THEN
           SELECT *
           INTO   mae.*
           FROM   afi_mae_afiliado ma
           WHERE  ma.n_seguro = afi.n_seguro

           IF SQLCA.SQLCODE = 0 THEN
               INSERT INTO afi_his_afiliado VALUES (mae.*)

               IF SQLCA.SQLCODE = 0 THEN
                   DELETE
                   FROM   cta_afi_nip
                   WHERE  nss = afi.n_seguro

                   DELETE
                   FROM   afi_mae_afiliado
                   WHERE  n_seguro = afi.n_seguro
               END IF

               SELECT b.*
               INTO   cta.*
               FROM   cta_ctr_cuenta b
               WHERE  b.nss = afi.n_seguro

               IF SQLCA.SQLCODE = 0 THEN
                   SELECT "X"
                   FROM   cta_ctr_cuenta
                   WHERE  cta_ctr_cuenta.nss = afi.n_seguro

                   IF SQLCA.SQLCODE = 0 THEN
                       INSERT INTO cta_his_cuenta VALUES (cta.*)
                   END IF

               END IF

 # mod 19-11-2004   se sustituyo afi_mae_afilidado por cta_regimen
 #  y n_seguro por nss

            {
               SELECT 'X'
               FROM   cta_regimen ams  
               WHERE  ams.nss = afi.n_seguro
               GROUP BY 1

               IF SQLCA.SQLCODE = 0 THEN
                   INSERT INTO cta_his_regimen         ##
                   SELECT *, USER, TODAY
                   FROM   cta_regimen ams
                   WHERE  ams.nss = afi.n_seguro
                   
                   DELETE
                   FROM   cta_regimen
                   WHERE  nss = afi.n_seguro
               END IF
            }

               IF mae.tipo_solicitud = 5 THEN
                   LET afi.finicta  = mae.finicta
                   LET afi.finitmte = mae.fentcons

                   UPDATE afi_det_asignado
                   SET    fecha_afiliacion = afi.fentcons,
                          estado_asignado  = 100
                   WHERE  n_seguro = mae.n_seguro
                   AND    n_folio  = mae.n_folio
                   AND    tipo_solicitud = mae.tipo_solicitud

                   LET afi.status = 220
               END IF

               LET HAY = FALSE
           END IF
       END IF

       LET afi.status_interno = 100
       LET afi.status_captura = 100

       IF NOT HAY THEN
           IF afi.n_unico IS NOT NULL AND
              afi.n_unico <> "                  " AND
               LENGTH(afi.n_unico) = 18 THEN
               LET afi.status_interno = 200
               LET afi.status_captura = 0
           ELSE
               LET afi.status_interno = 100
               LET afi.status_captura = 0
           END IF


           INSERT INTO afi_mae_afiliado VALUES(afi.*)

           IF SQLCA.SQLCODE <> 0 THEN
               INSERT INTO safre_tmp:nss_dup VALUES (afi.n_seguro)
           END IF

           INSERT INTO afi_mae_patron      -------- Patrones
           SELECT *
           FROM   afi_patron
           WHERE  n_folio = afi.n_folio
           AND    tipo_solicitud = afi.tipo_solicitud

           INSERT INTO afi_mae_benefici    -------- Beneficiarios
           SELECT *
           FROM   afi_beneficiario
           WHERE  n_seguro = afi.n_seguro
           AND    n_folio = afi.n_folio

 --EJRM =>

           LET v_crea_fecha = HOY

           LET v_sql_1 = "EXECUTE PROCEDURE fn_edad(?,?)"

           PREPARE stmt1 FROM v_sql_1

           DECLARE curs1 CURSOR FOR stmt1

           OPEN  curs1 USING afi.n_seguro,                               
                             v_crea_fecha

           FETCH curs1 INTO v_existe, v_es_menor, v_criterio
           CLOSE curs1

           INITIALIZE v_afisolreg.*  TO NULL

         {
           DELETE
           FROM   cta_nss_regimen
           WHERE  nss       = afi.n_seguro
         }

           SELECT nss,n_folio,tipo_solicitud,estado
           INTO   v_afisolreg.*
           FROM   afi_solicitud_regimen
           WHERE  nss            = afi.n_seguro
           AND    n_folio        = afi.n_folio
           AND    tipo_solicitud = afi.tipo_solicitud
           AND    estado         = 1

           IF SQLCA.SQLCODE = 0 THEN
               INITIALIZE   v_ctareg.* TO NULL

               UPDATE afi_solicitud_regimen
               SET    estado = 2
               WHERE  nss            = v_afisolreg.nss
               AND    n_folio        = v_afisolreg.fol
               AND    tipo_solicitud = v_afisolreg.ts
               AND    estado         = v_afisolreg.edo

               LET v_cve_sief_f = 1
               LET v_tipo_trasp = 6

             {
               INSERT INTO cta_regimen
               SELECT *
               FROM afi_regimen

               LET gr_ctanssreg.nss          = afi.n_seguro
               LET gr_ctanssreg.tipo_regimen = 1
               LET gr_ctanssreg.siefore_rcv  = 1
               LET gr_ctanssreg.usuario      = g_usuario
               LET gr_ctanssreg.factualiza   = TODAY

               INSERT INTO cta_nss_regimen VALUES (gr_ctanssreg.*)
             }
           ELSE
               IF v_es_menor = 0 THEN
                   LET v_cve_sief_f = 2
               ELSE
                   LET v_cve_sief_f = 1
               END IF

               IF mae.tipo_solicitud = 5 THEN
                   LET v_tipo_trasp = 5
               ELSE
                   LET v_tipo_trasp = 6
               END IF
           END IF

           LET v_tipo_proc = 1
           LET v_medio     = 10

--ejrm <=


           SELECT "OK" 
           FROM cta_act_marca 
           WHERE nss = afi.n_seguro 
           AND  marca_cod = 150 

           IF status <> NOTFOUND THEN 

           SELECT a.*
           INTO reg_cancelada.*
           FROM cta_act_marca  a
           WHERE a.nss = afi.n_seguro 
           AND  a.marca_cod = 150 

           LET ejecuta = "EXECUTE PROCEDURE desmarca_cuenta(?,?,?,?,?,?)"


           PREPARE clausula_spl FROM ejecuta
           EXECUTE clausula_spl USING afi.n_seguro,
                                      reg_cancelada.marca_cod,
                                      reg_cancelada.correlativo,
                                      pestado_marca,
                                      pmarca_causa,
                                      g_usuario

           END IF

           SELECT "X"
           FROM   cta_ctr_cuenta
           WHERE  cta_ctr_cuenta.nss = afi.n_seguro

           IF SQLCA.SQLCODE = 0 THEN
               UPDATE cta_ctr_cuenta
               SET     fecha_pri_rcv      = NULL,
                       fecha_ult_rcv      = NULL,
                       fecha_pri_general  = NULL, 
                       fecha_ult_general  = NULL,
                       fecha_vol_pat      = NULL,
                       fecha_vol_ven      = NULL,
                       ind_actividad      = 1,
                       fecha_actividad    = HOY,
                       ind_edad           = v_es_menor, #ejrm
                       fecha_edad         = HOY,        #ejrm
                       criterio_edad      = v_criterio, #ejrm
                       ind_transferencia  = 0,          #ejrm
                       fecha_ind_transf   = HOY,        #ejrm
                       ind_saldo_cero     = 0,
                       fecha_saldo_cero   = NULL,
                       estado_impresion   = 0,
                       periodo_ult_aporte = NULL,
                       dias_cotizados     = 0,
                       ult_sal_integrado  = 0,
                       tipo_informe       = 0,
                       fecha_informe      = NULL,
                       fecha_registro     = HOY,
                       usuario            = g_usuario
                 WHERE nss                = afi.n_seguro
           ELSE
               INSERT INTO cta_ctr_cuenta     #------ Control cuenta
               VALUES ( afi.n_seguro,       #nss
                        "",                 #fecha_pri_rcv
                        "",                 #fecha_ult_rcv
                        "01/01/0001",       #fecha_pri_general
                        "",                 #fecha_ult_general
                        "",                 #fecha_vol_pat
                        "",                 #fecha_vol_ven
                        0,                  #ind_saldo_cero
                        "",                 #fecha_saldo_cero
                        1,                  #ind_actividad
                        HOY,                #fecha_actividad
                        v_es_menor,         #ind_edad
                        HOY,                #fecha_edad
                        v_criterio,         #criterio_edad
                        0,                  #ind_transferencia
                        HOY,                #fecha_ind_transf
                        0,                  #estado_impresion
                        "",                 #periodo_ult_aporte
                        0,                  #dias_cotizados
                        0,                  #ult_sal_integrado
                        0,                  #tipo_informe
                        "",                 #fecha_informe
                        HOY,                #fecha_registro
                        g_usuario           #usuario
                        )
           END IF


           DECLARE curs3 CURSOR FOR stmt3
           OPEN curs3 USING afi.n_seguro ,
                            HOY 
           FETCH curs3 INTO reg_fn_fnacimiento.*

           CLOSE curs3

           DECLARE curs2 CURSOR FOR stmt2

           OPEN curs2 USING afi.n_seguro,
                            reg_fn_fnacimiento.indicador_edad, 
                            reg_fn_fnacimiento.indicador_edad,
                            #v_cve_sief_f, CPL-1368
                            v_tipo_proc,
                            v_tipo_trasp,
                            v_medio

           FETCH curs2 INTO v_existe, v_edad, v_rechazo, v_folioatencion

             update sep_det_reg_sol_reclamante
             set estado = 52  -- asociado registrado
             where nss = afi.n_seguro
             and   estado = 51

           CLOSE curs2
       END IF





           --IF afi.status_interno = 100 THEN

--               LET ejecuta = "EXECUTE PROCEDURE marca_cuenta(",
                             --"'",afi.n_seguro,"'",
                             --",",pmarca_entra,
                             --",",afi.n_folio,
                             --",",pestado_marca,
                             --",",pcodigo_rechazo,
                             --",",pmarca_causa,
                             --",","'","'", ",",
                             --"'",g_usuario,"'",")" 
           
              -- LET ejecuta = ejecuta CLIPPED
           
              -- PREPARE clausula_spl FROM ejecuta
           
               --DECLARE cursor_marca CURSOR FOR clausula_spl
           
              -- OPEN cursor_marca
           
               --FETCH cursor_marca INTO xcodigo_marca, xcodigo_rechazo
           
             --  CLOSE cursor_marca
         --  END IF

           LET HORA = TIME
           
           INSERT INTO afi_ctr_logico
           VALUES (afi.n_folio,
                   afi.tipo_solicitud,
                   afi.n_seguro,
                   afi.status_interno,
                   g_usuario,
                   HOY,
                   HORA,
                   operacion)
           
           INSERT INTO safre_tmp:afi_2
           VALUES (afi.n_seguro       ,
                   afi.n_unico        ,
                   afi.n_rfc          ,
                   afi.paterno        ,
                   afi.materno        ,
                   afi.nombres        ,
                   afi.fena           ,
                   afi.sexo           ,
                   afi.frecafor       ,
                   afi.status_interno
                   )
           
           WHENEVER ERROR CONTINUE
           INSERT INTO cta_afi_nip
           VALUES(afi.n_seguro,
                  afi.n_unico,
                  'XXXXX',
                  g_usuario,
                  HOY,
                  HORA) 
           WHENEVER ERROR STOP
                 
           UPDATE afi_solicitud 
           SET    afi_solicitud.status         = 100 ,
                  afi_solicitud.status_interno = 100 ,
                  afi_solicitud.status_captura = 100
           WHERE  afi_solicitud.n_seguro       = afi.n_seguro
           AND    afi_solicitud.n_folio        = afi.n_folio
           AND    afi_solicitud.tipo_solicitud = afi.tipo_solicitud

          {
           IF afi.finitmte IS NULL THEN
              LET reg_carta.docto_cod = 30201 
              CALL det_carta() #dc
           ELSE
              LET reg_carta.docto_cod = 30202
              CALL det_carta() #dc
           END IF
          }

          SELECT "X"
          FROM   safre_af:rec_solicitud mr
          WHERE  mr.n_seguro   = afi.n_seguro
          AND    mr.origen_rec <> 1
          GROUP BY 1

          IF SQLCA.SQLCODE = 0 THEN
              UPDATE safre_af:rec_solicitud
              SET    safre_af:rec_solicitud.origen_rec = 1
              WHERE  safre_af:rec_solicitud.n_seguro = afi.n_seguro
              AND    safre_af:rec_solicitud.origen_rec <> 1
          END IF      
    END FOREACH

    SELECT "X"
    FROM   safre_tmp:afi_2
    GROUP BY 1

    LET G_LISTA = g_paramgrales.ruta_listados CLIPPED,"/",g_usuario CLIPPED,
                  ".ENVIA_REG_MAESTRO." CLIPPED,
                  HOY USING "dd-mm-yy","_",HORA CLIPPED

    IF STATUS = NOTFOUND THEN 
        START REPORT listado_1 TO G_LISTA
            OUTPUT TO REPORT listado_1(mensaje)
        FINISH REPORT listado_1
    ELSE
        DECLARE cur_1 CURSOR FOR
        SELECT *
        FROM   safre_tmp:afi_2

        START REPORT listado_2 TO G_LISTA
            FOREACH cur_1 INTO w_aux.*
                OUTPUT TO REPORT listado_2(w_aux.*)
            END FOREACH
        FINISH REPORT listado_2
    END IF 

    LET G_LISTA = "chmod 777 ",g_paramgrales.ruta_listados CLIPPED,
		 "/",g_usuario CLIPPED,".ENVIA_REG_MAESTRO." CLIPPED,
                  HOY USING "dd-mm-yy","_",HORA CLIPPED
    RUN G_LISTA

END FUNCTION

FUNCTION despliega_resultados()
#dr----------------------------

    DEFINE total_resp SMALLINT

    LET total_resp = con_curp + sin_curp

    IF bnd_proceso THEN
        DISPLAY "          NSS INCORPORADOS AL MAESTRO DE AFILIADOS           "
        DISPLAY "Total de Registros Incorporados : ", total_resp USING "#####&"

        DISPLAY "Registros con curp asignada     : ", con_curp   USING "#####&"

        DISPLAY "Registros sin curp asignada     : ", sin_curp   USING "#####&"
    ELSE
        DISPLAY "          NSS INCORPORADOS AL MAESTRO DE AFILIADOS           "
        AT 10,1 ATTRIBUTE(REVERSE)
        DISPLAY "Total de Registros Incorporados : ", total_resp USING "#####&"
        AT 11,15

        DISPLAY "Registros con curp asignada     : ", con_curp   USING "#####&"
        AT 12,15

        DISPLAY "Registros sin curp asignada     : ", sin_curp   USING "#####&"
        AT 13,15

        PROMPT "Presione [Enter] para terminar" FOR enter
    END IF

END FUNCTION

REPORT listado_1(mensaje)
#------------------------

    DEFINE
        mensaje             CHAR(50)

    OUTPUT
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0

    FORMAT
    PAGE HEADER
        PRINT
            COLUMN 03,"========================================",
            COLUMN 40,"==================================="
        PRINT
            COLUMN 20," TRASPASO  AL  MAESTRO  DE  AFILIADOS "
        PRINT
            COLUMN 03,"----------------------------------------",
            COLUMN 40,"-----------------------------------"
        PRINT
            COLUMN 01,"N.S.S. "  ,
            COLUMN 13,"CURP   "  ,
            COLUMN 33,"R.F.C. "  ,
            COLUMN 48,"Paterno"  ,
            COLUMN 68,"Materno"  ,
            COLUMN 88,"Nombres"       
        PRINT
            COLUMN 05,"Fecha Nac."     ,
            COLUMN 17,"Sexo"           ,
            COLUMN 28,"Fecha Frecafor" ,
            COLUMN 43,"Edo. Afiliado" 
        PRINT
            COLUMN 03,"========================================",
            COLUMN 40,"==================================="
        PRINT 
        PRINT 
        PRINT 
            COLUMN 15,mensaje

END REPORT

REPORT listado_2(w_aux)
#----------------------
    DEFINE w_aux  RECORD
        n_seguro            LIKE afi_solicitud.n_seguro,
        n_unico             LIKE afi_solicitud.n_unico ,
        n_rfc               LIKE afi_solicitud.n_rfc   ,
        paterno             LIKE afi_solicitud.paterno ,
        materno             LIKE afi_solicitud.materno ,
        nombres             LIKE afi_solicitud.nombres ,
        fena                LIKE afi_solicitud.fena    ,
        sexo                LIKE afi_solicitud.sexo    ,
        frecafor            LIKE afi_solicitud.frecafor,
        status_interno      SMALLINT
    END RECORD

    DEFINE 
        l_estado    CHAR(16) ,
        aux_sexo    CHAR(10)

    DEFINE 
        cont        INTEGER

    OUTPUT
	LEFT MARGIN   0
	RIGHT MARGIN  0
	TOP MARGIN    0
	BOTTOM MARGIN 0

    FORMAT
    PAGE HEADER
        PRINT
            COLUMN 03,"========================================",
            COLUMN 40,"==================================="
        PRINT
            COLUMN 20," TRASPASO  AL  MAESTRO  DE  AFILIADOS "
        PRINT
            COLUMN 03,"----------------------------------------",
            COLUMN 40,"-----------------------------------"
        PRINT
            COLUMN 01,"N.S.S  "       ,
            COLUMN 13,"CURP   "       ,
            COLUMN 33,"R.F.C. "       ,
            COLUMN 48,"Paterno"       ,
            COLUMN 68,"Materno"       ,
            COLUMN 88,"Nombres"       
        PRINT
            COLUMN 05,"Fecha Nac."    ,
            COLUMN 17,"Sexo"          ,
            COLUMN 28,"Fecha Frecafor",
            COLUMN 43,"Edo. Afiliado" 
        PRINT
            COLUMN 03,"========================================",
            COLUMN 40,"==================================="
    ON EVERY ROW
        IF w_aux.n_unico IS NULL OR w_aux.n_unico = " " THEN
            LET l_estado = NULL
        END IF

        SELECT @estado_desc
          INTO l_estado
          FROM tab_status_afi
         WHERE @estado_cod = w_aux.status_interno

        LET l_estado = l_estado CLIPPED

        SELECT sexo_desc
        INTO   aux_sexo
        FROM   tab_sexo
        WHERE  sexo_cod = w_aux.sexo

        PRINT
            COLUMN 01,w_aux.n_seguro                   ,
            COLUMN 13,w_aux.n_unico                    ,
            COLUMN 33,w_aux.n_rfc                      ,
            COLUMN 48,w_aux.paterno CLIPPED            ,
            COLUMN 68,w_aux.materno CLIPPED            ,
            COLUMN 88,w_aux.nombres CLIPPED
        PRINT
            COLUMN 05,w_aux.fena  USING "dd-mm-yyyy"   ,
            COLUMN 17,aux_sexo    ,
            COLUMN 28,w_aux.frecafor USING "dd-mm-yyyy",
            COLUMN 43,l_estado CLIPPED

    ON LAST ROW
           SELECT COUNT(*)
           INTO   cont
           FROM   safre_tmp:afi_2

        PRINT
        PRINT
        PRINT
            COLUMN 03,"----------------------------------------",
            COLUMN 40,"-----------------------------------"

        PRINT
        PRINT
           COLUMN 03,"NUMERO TOTAL DE REGISTROS ---> ",cont
END REPORT

FUNCTION actualiza_bat_f(v_folio)
#ab------------------------------

define v_cat          CHAR(600),
       vv_fecha_log   CHAR(030),
       vv_prog        CHAR(010),
       paso           CHAR(100)

define v_fecha_log DATETIME YEAR TO SECOND

define v_folio  integer
define reg_ruta RECORD LIKE seg_modulo.*

SELECT A.*
INTO reg_ruta.*
FROM  seg_modulo A
WHERE modulo_cod = "bat"
 
UPDATE bat_ctr_operacion
set    folio      = NULL ,      
       estado_cod = 4    ,
       fecha_fin  = CURRENT
WHERE pid         = reg_bat.pid
and   proceso_cod = reg_bat.proceso_cod
and   opera_cod   = reg_bat.opera_cod

UPDATE bat_ctr_proceso
set    folio       = NULL ,      
       estado_cod  = 4    ,
       fecha_fin   = CURRENT
WHERE  pid         = reg_bat.pid
and    proceso_cod = reg_bat.proceso_cod

UPDATE bat_tmp_predecesor
SET    bandera_ejecuta  = 1
WHERE  pid_prod         = reg_bat.pid
AND    proceso_cod_prod = reg_bat.proceso_cod
AND    opera_cod_prod   = reg_bat.opera_cod

LET v_fecha_log = CURRENT
LET vv_fecha_log = v_fecha_log

SELECT A.programa_cod 
INTO   vv_prog 
FROM   bat_ctr_operacion A
WHERE  A.pid         = reg_bat.pid
AND    A.proceso_cod = reg_bat.proceso_cod
AND    A.opera_cod   = reg_bat.opera_cod

LET paso = "nohup:"            ,
    reg_bat.pid         USING"&&&&&",":",
    reg_bat.proceso_cod USING"&&&&&",":",
    reg_bat.opera_cod   USING"&&&&&"

                 LET v_cat = "echo '"                ,
                             vv_fecha_log[1,4]       ,   
                             vv_fecha_log[6,7]       ,  
                             vv_fecha_log[9,10]      ,  
                             vv_fecha_log[12,13]     ,   
                             vv_fecha_log[15,16]     ,    
                             vv_fecha_log[18,19]     ,
                             "|"                    ,
                             vv_prog  CLIPPED        ,
                             "|"                     ,
                             "FINOK"                ,
                             "|"                     ,
                             reg_ruta.ruta_listados CLIPPED,  
                             "/"                     ,
                             paso CLIPPED            ,
                             "'"                     ,
                             " >> "                  ,
                             reg_ruta.ruta_envio CLIPPED ,
                             "/"                     ,
                             "aad_safre.log"

                  LET v_cat = v_cat CLIPPED
                  RUN v_cat
END FUNCTION

FUNCTION det_carta()
#dc-----------------

   LET reg_carta.nss            = afi.n_seguro 
   LET reg_carta.n_folio        = afi.n_folio
   LET reg_carta.tipo_solicitud = afi.tipo_solicitud
   LET reg_carta.fecha_registro = afi.fentcons
   LET reg_carta.opera_cod      = NULL
   LET reg_carta.edo_genera     = '10'
   LET reg_carta.fecha_genera   = TODAY
   LET reg_carta.hora_genera    = TIME
   LET reg_carta.lote_genera    = 0
   LET reg_carta.consecutivo    = 0
   LET reg_carta.id_sepomex     = 0

   LET consulta_carta = "EXECUTE PROCEDURE inserta_carta (?,?,?,?,?,?,",
 			"?,?,?,?,?,?)"
                        
   PREPARE exe_sql FROM consulta_carta
   EXECUTE exe_sql USING reg_carta.* 

   INITIALIZE reg_carta.* TO NULL

END FUNCTION

