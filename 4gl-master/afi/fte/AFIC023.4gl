############################################################################
#Propietario       => E.F.P.                                               #
#Programa AFIC023  => RECIBE ARCHIVOS OPERACION 54 NOTIFICACION DE CUENTAS #
#                     PROCESO TRASPASOS AFORE - AFORE INTERNET             #
#Fecha creacion    => 19 DE AGOSTO DE 2005                                 #
#Por               => EDUARDO JOAQUIN RESENDIZ MEDINA                      #
#Sistema           => AFI                                                  #
#Modificacion      => 21 FERBRERO 2012 ORIGEN 74                           #
#Modificacion      => 09 Abril 2012 CORREO-E  50 POS                       #
#                  => EDUARDO JOAQUIN RESENDIZ MEDINA                      #
#Modificacion      => 16 JULIO 2012 AGREGAR CAMPOS (ID 32 RFC AL ID 35 ACT)#
#                  => EDUARDO JOAQUIN RESENDIZ MEDINA ID 36 36M y st_int 65#
############################################################################

DATABASE safre_af

GLOBALS

    DEFINE reg_cza_internet   RECORD
        tipo_registro         CHAR(2)       ,
        ident_servicio        CHAR(2)       ,
        ident_operacion       CHAR(2)       ,
        tipo_ent_origen       CHAR(2)       ,
        cve_ent_origen        CHAR(3)       ,
        tipo_ent_destino      CHAR(2)       ,
        cve_ent_destino       CHAR(3)       ,
        periodicidad          CHAR(1)       ,
        fecha_presentacion    DATE          ,
        consec_lote_dia       SMALLINT      ,
        cve_mod_recepcion     CHAR(2)
    END RECORD

    DEFINE reg_det_internet   RECORD
        tipo_registro         CHAR(2)       ,
        cont_servicio         DECIMAL(10,0) ,
        tipo_recep_cuenta     CHAR(2)       ,
        cve_recep_cuenta      CHAR(3)       ,
        tipo_ced_cuenta       CHAR(2)       ,
        cve_ced_cuenta        CHAR(3)       ,
        origen_tipo_traspaso  CHAR(2)       ,
        fecha_presentacion    DATE          ,
        n_seguro              CHAR(11)      ,
        fol_sol               DECIMAL(10,0) ,
        n_rfc                 CHAR(13)      ,
        paterno               CHAR(40)      ,
        materno               CHAR(40)      ,
        nombres               CHAR(40)      ,
        n_unico               CHAR(18)      ,
        cve_sector            CHAR(1)       ,
        sexo                  SMALLINT      ,
        estadon               SMALLINT      ,
        fena                  DATE          ,
        correoe_clip          CHAR(50)      ,
        correoe_sol_tra_int   CHAR(50)      ,
        calle                 CHAR(65)      ,
        numero                CHAR(15)      ,
        depto                 CHAR(15)      ,
        colonia               CHAR(65)      ,
        delega                CHAR(65)      ,
        codpos                CHAR(5)       ,
        estado                CHAR(65)      ,
        tel_casa              CHAR(15)      ,
        tel_ofic              CHAR(15)      ,
        fecha_prim_afi        DATE          ,
        periodo_pago          DECIMAL(6,0)  ,
        nombre_trab_pro       CHAR(50)      ,
        fecha_actualiza_sa    DATE          ,
        salario_actual        DECIMAL(5,2)  , 
        nacionalidad          CHAR(3)       ,
        fecha_cert            DATE          ,
        n_folio         DECIMAL(10,0)       ,
        ind_notificacion      SMALLINT      ,
        fecha_liquidacion     DATE          ,
        fecha_notificacion    DATE          ,
        id_bono_issste        CHAR(1)       ,
        id_apor_vol_may       CHAR(1)       ,
        cod_promotor          CHAR(10)      ,
        ocupacion             CHAR(30)      ,       --16072012
        actividad             CHAR(30)              --16072012
    END RECORD

    DEFINE  mas_2tras_36m     CHAR(1)

    DEFINE reg_sum_internet   RECORD
        tipo_registro         CHAR(2)       ,
        cant_reg_det          DECIMAL(9,0)  ,
        fecha_presentacion    DATE
    END RECORD

    DEFINE g_param_afi RECORD LIKE seg_modulo.*

    DEFINE
        HOY                     ,
        d_fecha_cza             ,
        d_fecha_det             DATE

    DEFINE
        v_hora                  DATETIME HOUR TO SECOND 

    DEFINE
          enter                 CHAR(1)     ,
          genera                CHAR(25)    ,
          archivo_traspaso      CHAR(200)   ,
          v_fecha_cza           CHAR(8)     ,
          f_fecha_cza           CHAR(10)    ,
          v_fecha_det           CHAR(8)     ,
          f_fecha_det           CHAR(10)    ,
          v_fecha_fena          CHAR(8)     ,
          f_fecha_fena          CHAR(10)    ,
          v_fecha_sol           CHAR(8)     ,
          f_fecha_sol           CHAR(10)    ,
          v_folio               DECIMAL(10,0)  ,
          v_nombre              CHAR(120)   ,
          v_horario             CHAR(8)     ,
          f_horario             CHAR(10)    ,
          v_fecha_prim_afi      CHAR(8)     ,
          f_fecha_prim_afi      CHAR(10)    ,
          v_fecha_actualiza_sa  CHAR(8)     ,
          f_fecha_actualiza_sa  CHAR(10)    ,
          v_fecha_cert          CHAR(8)     ,
          f_fecha_cert          CHAR(10)   

    DEFINE
        cuantos                 INTEGER,
        s_codigo_afore          ,
        v_delega                ,
        v_ciudad                ,
        v_estado                SMALLINT,
        cont                    INTEGER

    DEFINE g RECORD
        nss                    CHAR(11)     ,
        paterno                CHAR(40)     ,
        materno                CHAR(40)     ,
        nombres                CHAR(40)
    END RECORD

    DEFINE
        g_usuario              CHAR(8)      ,
        vrazon_social          CHAR(25)     ,
        archivo_op             CHAR(100)    ,
        v_cod_correoe          CHAR(1)      ,
        v_correo_e             CHAR(100)    ,
        carga                  CHAR(150)    ,
        v_marca                CHAR(1)      ,
        v_cod_telefono         CHAR(1)      ,
        v_telefono             CHAR(40)     ,
        folio                  INTEGER      ,
        v_salario              DECIMAL(8,2)

    DEFINE varchivo CHAR(25)
    DEFINE vtipo_solicitud SMALLINT      --taa x urp
    DEFINE v_aux_n_seguro  CHAR(11),      --taa x curp
           ejecuta         CHAR(150)
    DEFINE G_LISTA         CHAR(200)
    DEFINE 
           contar          INTEGER    #-->SALVADOR: contar es el contador de registros insertdos en afi_det_internet

END GLOBALS

MAIN

    DEFER INTERRUPT
    OPTIONS 
        INPUT WRAP,
        PROMPT LINE LAST  ,
        ACCEPT KEY CONTROL-I

    CALL STARTLOG("AFIC023.log")
    CALL inicio()
    CALL proceso_principal()

END MAIN

FUNCTION proceso_principal()
#pp-------------------------

   OPEN WINDOW ventana_1 AT 4,4 WITH FORM "AFIC0231" ATTRIBUTE(BORDER)
   DISPLAY " AFIC023  RECEPCION ARCHIVO OPERACION 54 NOTIFICACION CUENTAS                  " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY "                           < Ctrl-C > Salir                                    " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

   INPUT BY NAME genera
       AFTER FIELD genera
       IF genera IS NULL THEN
           ERROR "Campo NO puede ser NULO"
           NEXT FIELD genera
       END IF

       SELECT @nombre_archivo
       INTO   varchivo
       FROM   afi_ctr_arh_internet
       WHERE  @nombre_archivo = genera

       IF STATUS <> NOTFOUND THEN
           ERROR "ARCHIVO YA PROCESADO"
           SLEEP 1
           ERROR " "
           INITIALIZE genera TO NULL
           CLEAR FORM
           NEXT FIELD genera
       END IF

           WHENEVER ERROR CONTINUE

               LET archivo_op = g_param_afi.ruta_rescate CLIPPED,
                                "/",genera CLIPPED

               LET carga = "echo 'LOAD FROM ", archivo_op CLIPPED,
                           " INSERT INTO safre_tmp:tmp_sol_tra_internet' | dbaccess safre_tmp"
               RUN carga

               SELECT count(*)
               INTO   cuantos
               FROM   safre_tmp:tmp_sol_tra_internet

               IF cuantos = 0 THEN
                   DISPLAY  " NOMBRE DE ARCHIVO INCORRECTO O ARCHIVO VACIO "
                   AT 19,2 ATTRIBUTE(REVERSE)
                   SLEEP 3
                   NEXT FIELD genera
               ELSE
                   EXIT INPUT
               END IF
           WHENEVER ERROR STOP

       ON KEY (INTERRUPT)
           DISPLAY "PROCESO CANCELADO" AT 19,1 ATTRIBUTE(REVERSE)
           SLEEP 2
           EXIT PROGRAM
   END INPUT

   ERROR "PROCESANDO INFORMACION "

   CALL validacion_previa() #vp
   CALL crea_tablas()
   CALL lee_archivo_plano_prev() #lapp
   CALL lee_archivo_plano() #lap
   CALL impresion_reporte() #ir

   INSERT INTO afi_ctr_arh_internet
   VALUES (genera,contar,HOY)

   PROMPT "Proceso finalizado, [Enter] para salir" FOR enter

END FUNCTION

FUNCTION crea_tablas() 
#ct-------------------

    WHENEVER ERROR CONTINUE
    DATABASE safre_tmp
        DROP TABLE encabezadoi
        DROP TABLE detallei
        DROP TABLE sumarioi
    WHENEVER ERROR STOP

    CREATE TABLE encabezadoi
        (campo1  CHAR(2),
         campo2  CHAR(2),
         campo3  CHAR(2),
         campo4  CHAR(2),
         campo5  CHAR(3),
         campo6  CHAR(2),
         campo7  CHAR(3),
         campo8  CHAR(1),
         campo9  CHAR(10),
         campo10 CHAR(3),
         campo11 CHAR(2))

    CREATE TABLE detallei
        (tipo_registro         CHAR(2)       ,
        cont_servicio         DECIMAL(10,0) ,
        tipo_recep_cuenta     CHAR(2)       ,
        cve_recep_cuenta      CHAR(3)       ,
        tipo_ced_cuenta       CHAR(2)       ,
        cve_ced_cuenta        CHAR(3)       ,
        origen_tipo_traspaso  CHAR(2)       ,
        fecha_presentacion    DATE          ,
        n_seguro              CHAR(11)      ,
        fol_sol               DECIMAL(10,0) ,
        n_rfc                 CHAR(13)      ,
        paterno               CHAR(40)      ,
        materno               CHAR(40)      ,
        nombres               CHAR(40)      ,
        n_unico               CHAR(18)      ,
        cve_sector            CHAR(1)       ,
        sexo                  SMALLINT      ,
        estadon               SMALLINT      ,
        fena                  DATE          ,
        correoe_clip          CHAR(50)      ,
        correoe_sol_tra_int   CHAR(50)      ,
        calle                 CHAR(65)      ,
        numero                CHAR(15)      ,
        depto                 CHAR(15)      ,
        colonia               CHAR(65)      ,
        delega                CHAR(65)      ,
        codpos                CHAR(5)       ,
        estado                CHAR(65)      ,
        tel_casa              CHAR(15)      ,
        tel_ofic              CHAR(15)      ,
        fecha_prim_afi        DATE          ,
        periodo_pago          DECIMAL(6,0)  ,
        nombre_trab_pro       CHAR(50)      ,
        fecha_actualiza_sa    DATE          ,
        salario_actual        DECIMAL(5,2)  , 
        nacionalidad          CHAR(3)       ,
        fecha_cert            DATE          ,
        n_folio         DECIMAL(10,0)       ,
        ind_notificacion      SMALLINT      ,
        fecha_liquidacion     DATE          ,
        fecha_notificacion    DATE          ,
        id_bono_issste        CHAR(1)         ,
        id_apor_vol_may       CHAR(1)         ,
        cod_promotor          CHAR(10)        ,
        ocupacion             CHAR(30)      ,       --16072012
        actividad             CHAR(30))             --16072012


    CREATE TABLE sumarioi
        (campo1 CHAR(2),
         campo2 CHAR(9),
         fechap DATE)

    DATABASE safre_af

END FUNCTION


FUNCTION inicio()
#i---------------

    LET HOY = TODAY
    LET v_hora = CURRENT HOUR TO SECOND

    SELECT codigo_afore, razon_social
    INTO   s_codigo_afore, vrazon_social
    FROM   tab_afore_local

    SELECT *, USER 
    INTO   g_param_afi.*, g_usuario
    FROM   seg_modulo
    WHERE  modulo_cod = 'afi'

    WHENEVER ERROR CONTINUE
        DATABASE safre_tmp
        DROP TABLE tmp_sol_tra_internet

        CREATE TABLE tmp_sol_tra_internet
          (n_registros CHAR(700))         --16072012

        DATABASE safre_af
    WHENEVER ERROR STOP

END FUNCTION

FUNCTION validacion_previa()
#vp-------------------------
#vp Valida que los registros en sus primeras 2 posiciones contenga
# 01 cza, 02 det, 09 sumario

    DEFINE
        c2_tipo_registro      CHAR(2)

    DEFINE
        sw_1                  ,
        sw_2                  ,
        sw_9                  SMALLINT

    DECLARE cur_2 CURSOR FOR
    SELECT UNIQUE(n_registros[1,2])
    FROM   safre_tmp:tmp_sol_tra_internet

    LET sw_1 = 0
    LET sw_2 = 0
    LET sw_9 = 0

    FOREACH cur_2 INTO c2_tipo_registro
        CASE c2_tipo_registro
            WHEN "01"
                LET sw_1 = 1
            WHEN "02"
                LET sw_2 = 1
            WHEN "09"
                LET sw_9 = 1
        END CASE
    END FOREACH

    IF sw_1 = 0 THEN
        PROMPT "SE RECHAZA EL LOTE. NO EXISTE ENCABEZADO" FOR enter
        EXIT PROGRAM
    END IF

    IF sw_2 = 0 THEN
        PROMPT "SE RECHAZA EL LOTE. NO EXISTEN REGISTROS DE DETALLE" FOR enter
        EXIT PROGRAM
    END IF

    IF sw_9 = 0 THEN
        PROMPT "SE RECHAZA EL LOTE. NO EXISTE SUMARIO" FOR enter
        EXIT PROGRAM
    END IF

END FUNCTION

FUNCTION lee_archivo_plano()
#lap------------------------


    DEFINE 
        carga_reg            CHAR(700) ,      --16072012
        c2_ident_operacion   CHAR(2)

    DECLARE cur_1 CURSOR FOR
    SELECT  * 
    FROM    safre_tmp:tmp_sol_tra_internet

    LET cont = 0
    LET contar = 0
    LET c2_ident_operacion = ""

    FOREACH cur_1 INTO carga_reg
        LET cont = cont + 1

               #---ENCABEZADO ---#
        IF carga_reg[5,6] = "54" THEN
            LET c2_ident_operacion = "54"
            LET reg_cza_internet.tipo_registro     = carga_reg[001,002] 
            LET reg_cza_internet.ident_servicio    = carga_reg[003,004]
            LET reg_cza_internet.ident_operacion   = carga_reg[005,006]
            LET reg_cza_internet.tipo_ent_origen   = carga_reg[007,008]
            LET reg_cza_internet.cve_ent_origen    = carga_reg[009,011]
            LET reg_cza_internet.tipo_ent_destino  = carga_reg[012,013]
            LET reg_cza_internet.cve_ent_destino   = carga_reg[014,016]
            LET reg_cza_internet.periodicidad      = carga_reg[017,017]         --taa x curp
            LET v_fecha_cza                        = carga_reg[018,025]         --taa x curp
            LET reg_cza_internet.consec_lote_dia   = carga_reg[026,028]         --taa x curp
            LET reg_cza_internet.cve_mod_recepcion = carga_reg[029,030]         --taa x curp
                                                            --[033,730]
            LET f_fecha_cza = v_fecha_cza[5,6],"/",
                              v_fecha_cza[7,8],"/",
                              v_fecha_cza[1,4] 
            LET reg_cza_internet.fecha_presentacion = f_fecha_cza

            INSERT INTO afi_cza_internet VALUES(reg_cza_internet.*)
        END IF

                #---DETALLE---#
        IF carga_reg[1,2] = "02" AND c2_ident_operacion = "54" THEN
            LET reg_det_internet.tipo_registro        = carga_reg[001,002]
            LET reg_det_internet.cont_servicio        = carga_reg[003,012]
            LET reg_det_internet.tipo_recep_cuenta    = carga_reg[013,014]
            LET reg_det_internet.cve_recep_cuenta     = carga_reg[015,017]
            LET reg_det_internet.tipo_ced_cuenta      = carga_reg[018,019]
            LET reg_det_internet.cve_ced_cuenta       = carga_reg[020,022]
            LET reg_det_internet.origen_tipo_traspaso = carga_reg[023,024]
            LET v_fecha_det                           = carga_reg[025,032]
            LET reg_det_internet.n_seguro             = carga_reg[033,043]
            LET reg_det_internet.n_unico              = carga_reg[044,061]
            LET reg_det_internet.fol_sol              = carga_reg[062,071]
                                                               --[054,058]
            LET reg_det_internet.n_rfc                = carga_reg[608,620]        --16072012
                                                               --[072,079]
            LET reg_det_internet.paterno              = carga_reg[072,111]
            LET reg_det_internet.materno              = carga_reg[112,151]
            LET reg_det_internet.nombres              = carga_reg[152,191]
            --LET reg_det_internet.n_unico              = ''
            --                                            ''
            LET reg_det_internet.cve_sector           = ''
            LET reg_det_internet.sexo                 = '0'
            LET reg_det_internet.estadon              = '0'
            LET v_fecha_fena                          = '18990101'          --default de informix
            LET reg_det_internet.correoe_clip         = ''
            LET reg_det_internet.correoe_sol_tra_int  = carga_reg[192,241]
            LET reg_det_internet.calle                = carga_reg[242,306]
            LET reg_det_internet.numero               = carga_reg[307,321]
            LET reg_det_internet.depto                = carga_reg[322,336]
            LET reg_det_internet.colonia              = carga_reg[337,401]
            LET reg_det_internet.delega               = carga_reg[402,466]
            LET reg_det_internet.codpos               = carga_reg[467,471]
            LET reg_det_internet.estado               = carga_reg[472,536]
            LET reg_det_internet.tel_casa             = carga_reg[537,551]
            LET reg_det_internet.tel_ofic             = carga_reg[552,566]
            LET v_fecha_prim_afi                      = carga_reg[567,574]
            LET reg_det_internet.nombre_trab_pro      = ''
            LET reg_det_internet.periodo_pago         = carga_reg[575,580]
                                                        --PROCANASE
            LET v_fecha_actualiza_sa                  = ''
                                                        --periodo de pago
            LET reg_det_internet.salario_actual       = ''
            LET v_salario                             = carga_reg[581,587]
            LET reg_det_internet.nacionalidad         = carga_reg[621,623]        --16072012
            LET v_fecha_cert                          = carga_reg[588,595]
                                                               --[716,730]
            LET reg_det_internet.id_bono_issste       = carga_reg[596,596]
            LET reg_det_internet.id_apor_vol_may      = carga_reg[597,597]
            LET reg_det_internet.cod_promotor         = carga_reg[598,607]
-->16072012
            LET reg_det_internet.ocupacion            = carga_reg[624,653]
            LET reg_det_internet.actividad            = carga_reg[654,683]
            LET mas_2tras_36m                         = carga_reg[684,684]

            LET f_fecha_det = v_fecha_det[5,6],"/",
                              v_fecha_det[7,8],"/",
                              v_fecha_det[1,4] 
            LET reg_det_internet.fecha_presentacion  = f_fecha_det

            LET f_fecha_fena = v_fecha_fena[5,6],"/",
                               v_fecha_fena[7,8],"/",
                               v_fecha_fena[1,4] 
            LET reg_det_internet.fena   = f_fecha_fena

            LET f_fecha_prim_afi = v_fecha_prim_afi[5,6],"/",
                                   v_fecha_prim_afi[7,8],"/",
                                   v_fecha_prim_afi[1,4] 
            LET reg_det_internet.fecha_prim_afi   = f_fecha_prim_afi

            LET f_fecha_actualiza_sa = v_fecha_actualiza_sa[5,6],"/01/",
                                       v_fecha_actualiza_sa[1,4] 
            LET reg_det_internet.fecha_actualiza_sa   = f_fecha_actualiza_sa


            LET f_fecha_cert = v_fecha_cert[5,6],"/",
                               v_fecha_cert[7,8],"/",
                               v_fecha_cert[1,4] 
            LET reg_det_internet.fecha_cert = f_fecha_cert
--->erm 11 Enero 2008
            IF (reg_det_internet.fecha_cert IS NULL OR 
               reg_det_internet.fecha_cert MATCHES "[ *]") THEN
                LET reg_det_internet.fecha_cert = reg_det_internet.fecha_presentacion
            END IF
---<
            LET reg_det_internet.salario_actual = v_salario / 100

-->16072012
            LET reg_det_internet.ocupacion            = reg_det_internet.ocupacion CLIPPED
            LET reg_det_internet.actividad            = reg_det_internet.actividad CLIPPED

            LET v_folio = NULL

            --IF reg_det_internet.origen_tipo_traspaso = 72 THEN
            CASE reg_det_internet.origen_tipo_traspaso
            WHEN 72
               LET vtipo_solicitud = 16           --TRASPASO INTERNET CURP ORIG 72

               SELECT MAX(n_folio)
               INTO   v_folio
               FROM   afi_solicitud
               WHERE  tipo_solicitud = 16         --taa x curp

               IF v_folio IS NULL THEN 
                   LET v_folio = 1
               ELSE 
                   LET v_folio = v_folio + 1
               END IF


               IF reg_det_internet.n_seguro IS NULL OR
                  reg_det_internet.n_seguro = '           ' THEN
                  LET ejecuta = "EXECUTE PROCEDURE fn_obtiene_nti(",
                             "'", reg_det_internet.n_unico, "')"
                  LET ejecuta = ejecuta CLIPPED

                  PREPARE eje_obtiene2 FROM ejecuta
                  DECLARE cur_obtiene2 CURSOR FOR eje_obtiene2
                  OPEN    cur_obtiene2
                  FETCH   cur_obtiene2 INTO v_aux_n_seguro
                  CLOSE   cur_obtiene2

                  --ERROR "Nuevo NTI asignado, ", v_aux_n_seguro SLEEP 5
                  LET reg_det_internet.n_seguro = v_aux_n_seguro
               END IF

            WHEN 74
              LET vtipo_solicitud = 18           --TRASPASO INTERNET CURP ORIG 74
#se agrega el select MAX(n_folio) a solictud de la afore CPL-977
               SELECT MAX(n_folio)
               INTO   v_folio
               FROM   afi_solicitud
               WHERE  tipo_solicitud = 18

               IF v_folio IS NULL THEN 
                   LET v_folio = 1
               ELSE 
                   LET v_folio = v_folio + 1
               END IF

              --LET v_folio = reg_det_internet.fol_sol

               IF reg_det_internet.n_seguro IS NULL OR
                  reg_det_internet.n_seguro = '           ' THEN
                  LET ejecuta = "EXECUTE PROCEDURE fn_obtiene_nti(",
                             "'", reg_det_internet.n_unico, "')"
                  LET ejecuta = ejecuta CLIPPED

                  PREPARE eje_obtiene3 FROM ejecuta
                  DECLARE cur_obtiene3 CURSOR FOR eje_obtiene3
                  OPEN    cur_obtiene3
                  FETCH   cur_obtiene3 INTO v_aux_n_seguro
                  CLOSE   cur_obtiene3

                  --ERROR "Nuevo NTI asignado, ", v_aux_n_seguro SLEEP 5
                  LET reg_det_internet.n_seguro = v_aux_n_seguro
               END IF


            OTHERWISE
               LET vtipo_solicitud = 9

               SELECT MAX(n_folio)
               INTO   v_folio
               FROM   afi_solicitud
               WHERE  tipo_solicitud = 9

               IF v_folio IS NULL THEN 
                   LET v_folio = 1
               ELSE 
                   LET v_folio = v_folio + 1
               END IF

            END CASE

            CASE vtipo_solicitud 
               WHEN 9
                  LET reg_det_internet.cod_promotor = reg_det_internet.cod_promotor CLIPPED
               WHEN 18
                  LET reg_det_internet.cod_promotor = reg_det_internet.cod_promotor CLIPPED
               OTHERWISE
                  LET reg_det_internet.cod_promotor = "0000000000"
            END CASE
{
            IF v_folio IS NULL THEN 
                LET v_folio = 1
            ELSE 
                LET v_folio = v_folio + 1
            END IF
}
            LET reg_det_internet.n_folio            = v_folio
            --LET reg_det_internet.ind_notificacion   = 0
            --LET reg_det_internet.fecha_liquidacion  = ''
            --LET reg_det_internet.fecha_notificacion = ''

            CALL valida_caracteres(reg_det_internet.calle,
                                   reg_det_internet.colonia,
                                   reg_det_internet.delega) 
            RETURNING reg_det_internet.calle,
                      reg_det_internet.colonia,
                      reg_det_internet.delega

            INSERT INTO afi_det_internet VALUES(reg_det_internet.*)
            LET contar=contar + 1
            DISPLAY contar," Registros Cargados" AT 19,2
            ATTRIBUTE(REVERSE)
                LET v_delega = 0
                LET v_ciudad = 0
                LET v_estado = 0

                SELECT estad_cod,deleg_cod,ciudad_cod
                INTO   v_estado,v_delega,v_ciudad
                FROM   tab_codpos
                WHERE  cpos_cod = reg_det_internet.codpos

                INSERT INTO afi_domicilio
                VALUES (reg_det_internet.n_seguro           , --n_seguro
                        v_folio                             , --n_folio
                        --9                                   , --tipo_solicitud
                        vtipo_solicitud                     , --tipo_solicitud
                        reg_det_internet.calle              , --calle
			''                                  , --calle ref1
			''                                  , --calle ref2
                        reg_det_internet.numero             , --numero
                        reg_det_internet.depto              , --depto
                        reg_det_internet.colonia            , --colonia
                        v_delega                            , --delega
                        v_ciudad                            , --ciudad
                        v_estado                            , --estado
                        reg_det_internet.codpos             , --codpos
                        1                                   , --dom_cod
                        "MEX"                               , --pais_cod
                        "X"                                 , --marca_envio
                        g_usuario                           , --usuario
                        HOY                                 ) --factualiza


                LET v_nombre = reg_det_internet.paterno," ",
                               reg_det_internet.materno," ",
                               reg_det_internet.nombres

                INSERT INTO afi_ctr_solicitud
                --VALUES (9                                   , --tipo_solicitud
                VALUES (vtipo_solicitud                     , --tipo_solicitud
                        v_folio                             , --n_folio
                        reg_det_internet.n_seguro           , --n_seguro
                        v_nombre                            , --nombre_trab_afo
                        65                                  , --status_interno  #antes 70
                        reg_det_internet.fecha_presentacion , --fecha_envio
                        reg_det_internet.fecha_presentacion , --fecha_recepcion
                        ""                                  , --cve_afore_ced
                        ""                                  , --ind_nss_modif
                        reg_det_internet.fecha_cert         , --fentcons
                        ""                                  , --curp_oficial
                        ""                                  , --ind_curp_modif
                        reg_det_internet.n_rfc              , --rfc_trab_bd     --16072012
                        ""                                  , --nombre_trab_bd
                        reg_det_internet.nombre_trab_pro    , --nombre_trab_pro
                        ""                                  , --fena_trab_bd
                        ""                                  , --codven_bd
                        ""                                  , --sexo_bd
                        ""                                  , --estadon_bd
                        reg_det_internet.fecha_prim_afi     , --fecha_prim_afi
                        ""                                  , --fecha_alta_act
                        ""                                  , --cve_afore_afi
                       reg_det_internet.nacionalidad        , --nacion_bd     --16072012
                        ""                                  , --tipo_prob
                        ""                                  , --folio_prob
                        ""                                  , --doc_prob
                        ""                                  , --ind_envio
                        ""                                  , --ind_nombre
                        ""                                  , --cod_operacion
                        ""                                  , --diag_proceso
			""                                  , --agenc cod
                        g_usuario                           , --usuario
                        HOY                                 ) --factualiza

                LET v_cod_telefono = NULL
                LET v_telefono     = NULL

                IF reg_det_internet.tel_casa = carga_reg[537,551] THEN
                   LET v_cod_telefono = "1"
                   LET v_telefono = reg_det_internet.tel_casa

                   INSERT INTO afi_telefono
                   VALUES (reg_det_internet.n_seguro           , --nss
                           v_folio                             , --n_folio
                           --9                                   , --tipo_solicitud
                           vtipo_solicitud                     , --tipo_solicitud
                           52                                  , --pais_cod
                           ""                                  , --clave_lada
                           ""                                  , --extension
                           v_telefono                          , --telefono
                           v_cod_telefono                      , --tel_cod
                           ""                                  , --hora_ini,                 ---28-18
                           ""                                  , --tipo_hora_ini,            ---28-18
                           ""                                  , --hora_fin,                 ---28-18
                           ""                                  , --tipo_hora_fin,            ---28-18
                           ""                                  , --dia,                      ---28-18
                           g_usuario                           , --usuario
                           HOY                                 ) --factualiza
                END IF

                IF reg_det_internet.tel_ofic = carga_reg[552,566] THEN
                   LET v_cod_telefono = "2"
                   LET v_telefono = reg_det_internet.tel_ofic

                   INSERT INTO afi_telefono
                   VALUES (reg_det_internet.n_seguro           , --nss
                           v_folio                             , --n_folio
                           --9                                   , --tipo_solicitud
                           vtipo_solicitud                     , --tipo_solicitud
                           52                                  , --pais_cod
                           ""                                  , --clave_lada
                           ""                                  , --extension
                           v_telefono                          , --telefono
                           v_cod_telefono                      , --tel_cod
                           ""                                  , --hora_ini,                 ---28-18
                           ""                                  , --tipo_hora_ini,            ---28-18
                           ""                                  , --hora_fin,                 ---28-18
                           ""                                  , --tipo_hora_fin,            ---28-18
                           ""                                  , --dia,                      ---28-18                           
                           g_usuario                           , --usuario
                           HOY                                 ) --factualiza
                END IF

                LET v_cod_correoe = NULL
                LET v_correo_e    = NULL 
                LET v_marca       = 'X'
                INITIALIZE folio TO NULL

                --IF reg_det_internet.correoe_clip = carga_reg[233,272] THEN
                IF reg_det_internet.correoe_clip = reg_det_internet.correoe_clip THEN
                   IF (reg_det_internet.correoe_clip IS NULL OR
                      reg_det_internet.correoe_clip = " ") THEN
                   ELSE
                      LET v_cod_correoe = "1"
                      LET v_correo_e = reg_det_internet.correoe_clip
                      INSERT INTO afi_correo_elect
                      VALUES (reg_det_internet.n_seguro           , --nss
                              v_folio                             , --n_folio
                              --9                                   , --tipo_solicitud
                              vtipo_solicitud                 , --tipo_solicitud
                              v_cod_correoe                       , --cod_correo_e
                              v_correo_e                          , --correo_elec
                              v_marca                             , --marca_envio
                              folio                               , --folio
                              HOY                                 , --factualiza
                              g_usuario                           ) --usuario
                   END IF
                END IF

                IF reg_det_internet.correoe_sol_tra_int = carga_reg[192,241] THEN 
                   IF (reg_det_internet.correoe_sol_tra_int IS NULL OR 
                      reg_det_internet.correoe_sol_tra_int = " ") THEN
                   ELSE
                      LET v_cod_correoe = "2"
                      LET v_correo_e = reg_det_internet.correoe_sol_tra_int
                      INSERT INTO afi_correo_elect
                      VALUES (reg_det_internet.n_seguro           , --nss
                              v_folio                             , --n_folio
                              --9                               , --tipo_solicitud
                              vtipo_solicitud                     , --tipo_solicitud
                              v_cod_correoe                       , --cod_correo_e
                              v_correo_e                          , --correo_elec
                              v_marca                             , --marca_envio
                              folio                               , --folio
                              HOY                                 , --factualiza
                              g_usuario                           ) --usuario
                   END IF
                END IF

            UPDATE afi_solicitud
            SET    status_interno = 45
            WHERE  n_seguro       = reg_det_internet.n_seguro
            AND    tipo_solicitud = vtipo_solicitud
            AND    status_interno = 65   #antes 70

            INSERT INTO afi_solicitud
                VALUES (reg_det_internet.n_seguro           , --n_seguro
                        reg_det_internet.n_unico            , --n_unico
                        --reg_det_internet.n_rfc              , --n_rfc
                        reg_det_internet.n_rfc              , --n_rfc     --16072012
                        reg_det_internet.paterno            , --paterno
                        reg_det_internet.materno            , --materno
                        reg_det_internet.nombres            , --nombres
                        reg_det_internet.fena               , --fena
                        v_folio                             , --v_folio
                        0                                   , --edo_civil
                        ""                                  , --localn
                        reg_det_internet.estadon            , --estadon
                        0                                   , --tiptr
                        --"0000000000"                        , --cod_promotor
                        reg_det_internet.cod_promotor       , --cod_promotor
                        reg_det_internet.sexo               , --sexo
                        0                                   , --n_operac
                        reg_det_internet.fecha_presentacion , --frecafor
                        reg_det_internet.fecha_cert         , --fentcons
                        ""                                  , --femision
                        ""                                  , --finitmte
                        ""                                  , --finicta
                        ""                                  , --status
                        0                                   , --agenc_cod
                        65                                  , --status_interno  #antes 70
                        reg_det_internet.nacionalidad       , --nacionalidad
                        6                                   , --tip_prob
                        ""                                  , --fol_prob
                        ""                                  , --doc_prob
                        0                                   , --ind_infonavit
                        ""                                  , --documento_1
                        ""                                  , --documento_2
                        ""                                  , --documento_3
                        ""                                  , --documento_4
                        ""                                  , --documento_5
                        ""                                  , --documento_6
                        ""                                  , --envio_dom
                        ""                                  , --entidad_curp
                        ""                                  , --asigna_curp
                        ""                                  , --const_curp
                        g_usuario                           , --usuario
                        v_hora                              , --hora
                        ""                                  , --status_captura
                        vtipo_solicitud                     , --tipo_solicitud 
                        reg_det_internet.fecha_presentacion , --fecha_elaboracion
                        ""                                  , --lote
                        ""                                  , --fecha_envio
                        0                                   , --cod_esq_comision
                        ""                                  , --ubicacion
                        reg_det_internet.fecha_prim_afi     , --fecha_1a_afil
                        ""                                  , --indicador_c
                        ""                                  , --indicador_d
                        ""                                  , --indicador_e
                        ""                                  , --cod_error_origen
                        ""                                  , --folio_edo_cta
                        ""                                  , --cod_afore_ced
                        0                                   , --salario_base_comis
                        reg_det_internet.salario_actual     , --salario_actual
                        reg_det_internet.fecha_actualiza_sa , --fecha_actualiza_sa
                        0                                   , --coduni_n1
                        0                                   , --indicador_comision
                        reg_det_internet.cod_promotor       , --codven
                        ""                                  , --cod_captura
                        ""                                  , --lote_captura
                        ""                                  , --folio_captura
                        ""                                  ) --sello_electronico

            CASE vtipo_solicitud 
               WHEN 16
                   INSERT INTO cta_ctr_reg_ind
                   VALUES (reg_det_internet.n_unico ,
                           reg_det_internet.n_seguro,
                           '',                   #nss_issste
                           '2',                  #tipo_trab_ind       issste
                           '01',                 #tipo_administracion 
                           "",                   #ind_deposito
                           HOY,
                           g_usuario)
               WHEN 18
                   INSERT INTO cta_ctr_reg_ind
                   VALUES (reg_det_internet.n_unico ,
                           reg_det_internet.n_seguro,
                           '',                   #nss_issste
                           '2',                  #tipo_trab_ind       issste
                           '01',                 #tipo_administracion 
                           "",                   #ind_deposito
                           HOY,
                           g_usuario)
             END CASE

             IF mas_2tras_36m = "1" THEN
               INSERT INTO afi_ctr_mas_dos_tra
               VALUES      (reg_det_internet.n_seguro , --nss
                            v_folio                   , --n_folio
                            vtipo_solicitud           , --vtipo_solicitud
                            "1"                       , --indicador
                            g_usuario                 , --usuario
                            HOY                       )
             END IF

             INSERT INTO afi_ctr_actividad_internet
             VALUES (reg_det_internet.n_seguro , -- nss            
                     v_folio                   , -- n_folio        
                     vtipo_solicitud           , -- tipo_solicitud 
                     reg_det_internet.ocupacion, -- ocupacion_desc 
                     reg_det_internet.actividad, -- actividad_desc 
                     g_usuario                 , -- usuario        
                     HOY                       ) -- factualiza     

        END IF


                #---SUMARIO---#

        IF carga_reg[1,2] = "09" AND c2_ident_operacion = "54" THEN
            LET c2_ident_operacion = ""
            LET reg_sum_internet.tipo_registro  = carga_reg[001,002]
            LET reg_sum_internet.cant_reg_det   = carga_reg[003,011]
                                                         --[012,730]
            LET reg_sum_internet.fecha_presentacion = 
                reg_cza_internet.fecha_presentacion
 
            INSERT INTO afi_sum_internet VALUES(reg_sum_internet.*)
        END IF

    END FOREACH
       --- DISPLAY contar," Registros Cargados" AT 19,2
        ---ATTRIBUTE(REVERSE)

    LET cont = cont - 2

END FUNCTION

FUNCTION impresion_reporte()

    DEFINE G_IMPRE        CHAR(300)
    DEFINE c_impre        CHAR(300)
    DEFINE gimpresion     CHAR(300)
    DEFINE hora           CHAR (08)

    LET G_IMPRE = g_param_afi.ruta_listados CLIPPED,"/",g_usuario CLIPPED,
                  ".NOTI_CUENTAS_OP_54.",hoy USING "DD-MM-YYYY"

    START REPORT det_noti_cuentas_rpt TO  G_IMPRE

    OUTPUT TO REPORT det_noti_cuentas_rpt(g.*)

    FINISH REPORT det_noti_cuentas_rpt

    LET c_impre = ' chmod 777 ', G_IMPRE CLIPPED
    RUN c_impre

    --LET gimpresion = "lp ",G_IMPRE # SALVADOR 16/10/2007 se elimina la 
                                     # impresion
    --LET gimpresion = "vi ",G_IMPRE
    --RUN gimpresion

END FUNCTION

REPORT det_noti_cuentas_rpt(g)

    DEFINE g RECORD
        nss                 CHAR(11),
        paterno             CHAR(40),
        materno             CHAR(40),
        nombres             CHAR(40)
    END RECORD

    DEFINE v_nombre_comp    CHAR(120)
    DEFINE fecha_max        DATE
    DEFINE contador         INTEGER
    DEFINE contador_afi_sol INTEGER 

    OUTPUT
        TOP MARGIN 1
        BOTTOM MARGIN 0
        LEFT MARGIN 0
        RIGHT MARGIN 0
        PAGE LENGTH 60

    FORMAT
        PAGE HEADER

        PRINT COLUMN 01,s_codigo_afore,"  ",vrazon_social CLIPPED,
              COLUMN 68,TODAY USING "dd-mm-yyyy"
        SKIP 2 LINE
        PRINT COLUMN 08,"REGISTROS RECIBIDOS POR OP.54 NOTIFICACION DE CUENTAS "
        SKIP 1 LINE
        PRINT COLUMN 01,"           REGISTROS GUARDADOS EN EL DETALLE DE INTERNET             "
        PRINT COLUMN 01,"---------------------------------------------------------------------"
        PRINT COLUMN 02,"NSS",
              COLUMN 15,"NOMBRE"
        PRINT COLUMN 01,"---------------------------------------------------------------------"
        SKIP 1 LINE
    ON EVERY ROW

     DECLARE cur_rep CURSOR FOR
     SELECT  a.nss,a.paterno,a.materno,a.nombres
     FROM    afi_det_internet a
     WHERE a.fecha_presentacion= f_fecha_det -->SALVADOR

     LET contador = 0
     FOREACH cur_rep INTO g.nss,
                          g.paterno,
                          g.materno,
                          g.nombres

        LET v_nombre_comp = g.paterno CLIPPED, " ",g.materno CLIPPED, " ",g.nombres CLIPPED
        LET contador = contador + 1

        PRINT COLUMN 02,g.nss,
              COLUMN 15,v_nombre_comp

     END FOREACH

     SELECT COUNT(*) 
     INTO contador_afi_sol
     FROM afi_det_internet

     PAGE TRAILER
        SKIP 2 LINE
        PRINT COLUMN 10,"Total Registros Leidos del Detalle de Internet : ",contador USING "####"
        PRINT COLUMN 10,"Total de Registros Guardados en afi_solicitud  : ",contador_afi_sol USING "####"
        PRINT COLUMN 60,"Pagina:",PAGENO USING "<<<<"

        PAUSE "Presione enter para continuar."

END REPORT

FUNCTION valida_caracteres(vcalle, vcolonia, vdelega)
  DEFINE 
    vcalle     CHAR(65),
    vcolonia   CHAR(65),
    vdelega    CHAR(65),
    acalle     CHAR(65),
    acolonia   CHAR(65),
    adelega    CHAR(65),
    scalle     CHAR(65),
    scolonia   CHAR(65),
    sdelega    CHAR(65),
    i          SMALLINT,
    j          SMALLINT

    LET vcalle   = vcalle   CLIPPED
    LET vcolonia = vcolonia CLIPPED
    LET vdelega  = vdelega  CLIPPED

    LET j = 1
    FOR i = 1 TO 65
        IF vcalle[i,i] <> '"' THEN
           LET acalle[j,j] = vcalle[i,i]
           LET j = j + 1
        END IF
    END FOR
    LET scalle = acalle
   
    LET j = 1
    FOR i = 1 TO 65
        IF vcolonia[i,i] <> '"' THEN
           LET acolonia[j,j] = vcolonia[i,i]
           LET j = j + 1
        END IF
    END FOR
    LET scolonia = acolonia

    LET j = 1
    FOR i = 1 TO 65
        IF vdelega[i,i] <> '"' THEN
           LET adelega[j,j]= vdelega[i,i]
           LET j = j + 1
        END IF
    END FOR
    LET sdelega = adelega 
   
    RETURN scalle, scolonia, sdelega
END FUNCTION


FUNCTION lee_archivo_plano_prev()
#lap------------------------


    DEFINE 
        carga_reg            CHAR(700) ,           --16072012
        c2_ident_operacion   CHAR(2),
        vrechazo CHAR(50)

    DEFINE ban_val SMALLINT
    DEFINE vn_seguro CHAR(11)
    DEFINE vmarca_cod SMALLINT
    DEFINE prev_n_folio DECIMAL(10,0)
    DEFINE prev_tipo_solic SMALLINT

    LET G_LISTA   = g_param_afi.ruta_listados CLIPPED, "/", 
                    g_usuario CLIPPED,
                    ".rech_curp_internet",
                    TODAY USING "DDMMYY"

    LET ban_val = 0
    LET vmarca_cod = 0
    LET prev_n_folio = 0
    LET prev_tipo_solic = 0

    START REPORT gen_rechazo   TO G_LISTA

    DECLARE cur_1_p CURSOR FOR
    SELECT  * 
    FROM    safre_tmp:tmp_sol_tra_internet

    LET cont = 0
    LET contar = 0
    LET c2_ident_operacion = ""

    FOREACH cur_1_p INTO carga_reg
        LET cont = cont + 1

               #---ENCABEZADO ---#
        IF carga_reg[5,6] = "54" THEN
            LET c2_ident_operacion = "54"
            LET reg_cza_internet.tipo_registro     = carga_reg[001,002] 
            LET reg_cza_internet.ident_servicio    = carga_reg[003,004]
            LET reg_cza_internet.ident_operacion   = carga_reg[005,006]
            LET reg_cza_internet.tipo_ent_origen   = carga_reg[007,008]
            LET reg_cza_internet.cve_ent_origen    = carga_reg[009,011]
            LET reg_cza_internet.tipo_ent_destino  = carga_reg[012,013]
            LET reg_cza_internet.cve_ent_destino   = carga_reg[014,016]
            LET reg_cza_internet.periodicidad      = carga_reg[017,017]         --taa x curp
            LET v_fecha_cza                        = carga_reg[018,025]         --taa x curp
            LET reg_cza_internet.consec_lote_dia   = carga_reg[026,028]         --taa x curp
            LET reg_cza_internet.cve_mod_recepcion = carga_reg[029,030]         --taa x curp
                                                            --[033,730]
            LET f_fecha_cza = v_fecha_cza[5,6],"/",
                              v_fecha_cza[7,8],"/",
                              v_fecha_cza[1,4] 
            LET reg_cza_internet.fecha_presentacion = f_fecha_cza

            INSERT INTO safre_tmp:encabezadoi VALUES(reg_cza_internet.*)
        END IF

                #---DETALLE---#
        IF carga_reg[1,2] = "02" AND c2_ident_operacion = "54" THEN
            LET reg_det_internet.tipo_registro        = carga_reg[001,002]
            LET reg_det_internet.cont_servicio        = carga_reg[003,012]
            LET reg_det_internet.tipo_recep_cuenta    = carga_reg[013,014]
            LET reg_det_internet.cve_recep_cuenta     = carga_reg[015,017]
            LET reg_det_internet.tipo_ced_cuenta      = carga_reg[018,019]
            LET reg_det_internet.cve_ced_cuenta       = carga_reg[020,022]
            LET reg_det_internet.origen_tipo_traspaso = carga_reg[023,024]
            LET v_fecha_det                           = carga_reg[025,032]
            LET reg_det_internet.n_seguro             = carga_reg[033,043]
            LET reg_det_internet.n_unico              = carga_reg[044,061]
            LET reg_det_internet.fol_sol              = carga_reg[062,071]
            LET reg_det_internet.n_rfc                = carga_reg[608,620]     --16072012
            LET reg_det_internet.paterno              = carga_reg[072,111]
            LET reg_det_internet.materno              = carga_reg[112,151]
            LET reg_det_internet.nombres              = carga_reg[152,191]
            LET reg_det_internet.cve_sector           = ''
            LET reg_det_internet.sexo                 = '0'
            LET reg_det_internet.estadon              = '0'
            LET v_fecha_fena                          = '18990101'          --default de informix
            LET reg_det_internet.correoe_clip         = ''
            LET reg_det_internet.correoe_sol_tra_int  = carga_reg[192,241]
            LET reg_det_internet.calle                = carga_reg[242,306]
            LET reg_det_internet.numero               = carga_reg[307,321]
            LET reg_det_internet.depto                = carga_reg[322,336]
            LET reg_det_internet.colonia              = carga_reg[337,401]
            LET reg_det_internet.delega               = carga_reg[402,466]
            LET reg_det_internet.codpos               = carga_reg[467,471]
            LET reg_det_internet.estado               = carga_reg[472,536]
            LET reg_det_internet.tel_casa             = carga_reg[537,551]
            LET reg_det_internet.tel_ofic             = carga_reg[552,566]
            LET v_fecha_prim_afi                      = carga_reg[567,574]
            LET reg_det_internet.nombre_trab_pro      = ''
            LET reg_det_internet.periodo_pago         = carga_reg[575,580]
                                                        --PROCANASE
            LET v_fecha_actualiza_sa                  = ''
                                                        --periodo de pago
            LET reg_det_internet.salario_actual       = ''
            LET v_salario                             = carga_reg[581,587]
            LET reg_det_internet.nacionalidad         = carga_reg[621,623]        --16072012
            LET v_fecha_cert                          = carga_reg[588,595]
                                                               --[716,730]
            LET reg_det_internet.id_bono_issste       = carga_reg[596,596]
            LET reg_det_internet.id_apor_vol_may      = carga_reg[597,597]
            LET reg_det_internet.cod_promotor         = carga_reg[598,607]
-->16072012
            LET reg_det_internet.ocupacion            = carga_reg[624,653]
            LET reg_det_internet.actividad            = carga_reg[654,683]
            LET mas_2tras_36m                         = carga_reg[684,684]

            LET f_fecha_det = v_fecha_det[5,6],"/",
                              v_fecha_det[7,8],"/",
                              v_fecha_det[1,4] 
            LET reg_det_internet.fecha_presentacion  = f_fecha_det

            LET f_fecha_fena = v_fecha_fena[5,6],"/",
                               v_fecha_fena[7,8],"/",
                               v_fecha_fena[1,4] 
            LET reg_det_internet.fena   = f_fecha_fena

            LET f_fecha_prim_afi = v_fecha_prim_afi[5,6],"/",
                                   v_fecha_prim_afi[7,8],"/",
                                   v_fecha_prim_afi[1,4] 
            LET reg_det_internet.fecha_prim_afi   = f_fecha_prim_afi

            LET f_fecha_actualiza_sa = v_fecha_actualiza_sa[5,6],"/01/",
                                       v_fecha_actualiza_sa[1,4] 
            LET reg_det_internet.fecha_actualiza_sa   = f_fecha_actualiza_sa


            LET f_fecha_cert = v_fecha_cert[5,6],"/",
                               v_fecha_cert[7,8],"/",
                               v_fecha_cert[1,4] 
            LET reg_det_internet.fecha_cert = f_fecha_cert

            IF (reg_det_internet.fecha_cert IS NULL OR 
               reg_det_internet.fecha_cert MATCHES "[ *]") THEN
                LET reg_det_internet.fecha_cert = reg_det_internet.fecha_presentacion
            END IF

            LET reg_det_internet.salario_actual = v_salario / 100

            LET v_folio = NULL

            --IF reg_det_internet.origen_tipo_traspaso = 72 THEN
            CASE reg_det_internet.origen_tipo_traspaso
            WHEN 72
               LET vtipo_solicitud = 16 

               SELECT MAX(n_folio)
               INTO   v_folio
               FROM   afi_solicitud
               WHERE  tipo_solicitud = 16 

               IF reg_det_internet.n_seguro IS NULL OR
                  reg_det_internet.n_seguro = '           ' THEN
                  LET ejecuta = "EXECUTE PROCEDURE fn_obtiene_nti(",
                             "'", reg_det_internet.n_unico, "')"
                  LET ejecuta = ejecuta CLIPPED

                  {PREPARE eje_obtiene2 FROM ejecuta
                  DECLARE cur_obtiene2 CURSOR FOR eje_obtiene2
                  OPEN    cur_obtiene2
                  FETCH   cur_obtiene2 INTO v_aux_n_seguro
                  CLOSE   cur_obtiene2}

                  LET reg_det_internet.n_seguro = v_aux_n_seguro
               END IF

            WHEN 74
              LET vtipo_solicitud = 18           --TRASPASO INTERNET CURP ORIG 74
#se agrega el select MAX(n_folio) a solictud de la afore CPL-977
               SELECT MAX(n_folio)
               INTO   v_folio
               FROM   afi_solicitud
               WHERE  tipo_solicitud = 18

               IF reg_det_internet.n_seguro IS NULL OR
                  reg_det_internet.n_seguro = '           ' THEN
                  LET ejecuta = "EXECUTE PROCEDURE fn_obtiene_nti(",
                             "'", reg_det_internet.n_unico, "')"
                  LET ejecuta = ejecuta CLIPPED

                  {PREPARE eje_obtiene2 FROM ejecuta
                  DECLARE cur_obtiene2 CURSOR FOR eje_obtiene2
                  OPEN    cur_obtiene2
                  FETCH   cur_obtiene2 INTO v_aux_n_seguro
                  CLOSE   cur_obtiene2}

                  LET reg_det_internet.n_seguro = v_aux_n_seguro
               END IF
 
            OTHERWISE
 
              LET vtipo_solicitud = 9

               SELECT MAX(n_folio)
               INTO   v_folio
               FROM   afi_solicitud
               WHERE  tipo_solicitud = 9
            END CASE

            IF v_folio IS NULL THEN 
                LET v_folio = 1
            ELSE 
                LET v_folio = v_folio + 1
            END IF

            LET reg_det_internet.n_folio            = v_folio

            CALL valida_caracteres(reg_det_internet.calle,
                                   reg_det_internet.colonia,
                                   reg_det_internet.delega) 
            RETURNING reg_det_internet.calle,
                      reg_det_internet.colonia,
                      reg_det_internet.delega

            INSERT INTO safre_tmp:detallei VALUES(reg_det_internet.*)
        END IF

                #---SUMARIO---#
        IF carga_reg[1,2] = "09" AND c2_ident_operacion = "54" THEN
            LET c2_ident_operacion = ""
            LET reg_sum_internet.tipo_registro  = carga_reg[001,002]
            LET reg_sum_internet.cant_reg_det   = carga_reg[003,011]
                                                         --[012,730]
            LET reg_sum_internet.fecha_presentacion = 
                reg_cza_internet.fecha_presentacion
 
            INSERT INTO safre_tmp:sumarioi VALUES(reg_sum_internet.*)
        END IF
    END FOREACH

    DECLARE cursor_1i CURSOR FOR 
    SELECT * 
    FROM   safre_tmp:detallei

    FOREACH cursor_1i INTO reg_det_internet.*

{       IF reg_det_internet.origen_tipo_traspaso = '74' THEN
          IF reg_det_internet.fol_sol IS NULL OR
             reg_det_internet.fol_sol = 0 THEN
             LET ban_val = 1
             LET vrechazo = "FOLIO NULO"
             --EXIT FOREACH
          END IF
}
          IF reg_det_internet.origen_tipo_traspaso = '72' OR
             reg_det_internet.origen_tipo_traspaso = '74' THEN
             SELECT 'X' 
             FROM   afi_mae_afiliado
             WHERE  n_unico = reg_det_internet.n_unico
             GROUP BY 1

             IF SQLCA.SQLCODE = 0 THEN
                LET ban_val = 2
                LET vrechazo = "CURP YA EXISTE EN MAESTRO DE AFILIADOS"
                OUTPUT TO REPORT gen_rechazo (reg_det_internet.n_unico,vrechazo)
             END IF
          END IF
 --      END IF
    END FOREACH
    FINISH REPORT gen_rechazo

    IF ban_val = 1 THEN
       ERROR "Validacion previa: Registros con Origen 74 NO contienen Folio o Numero de METI"
       SLEEP 2
       PROMPT "**NO SE PERMITE CARGAR EL ARCHIVO, PROCESO CANCELADO, Enter para SALIR**" FOR enter
          EXIT PROGRAM
    END IF

    IF ban_val = 2 THEN
       ERROR "Validacion previa: Registros Origen 72/74 CURP ya existe en maestro de afiliados"
       SLEEP 2
       PROMPT "**NO SE PERMITE CARGAR EL ARCHIVO, PROCESO CANCELADO, Enter para SALIR**" FOR enter
          EXIT PROGRAM
    END IF

    IF ban_val = 3 THEN
       ERROR "Validacion previa: Registros Origen 72/74 CURP ya existente y Folio Nulo"
       SLEEP 2
       PROMPT "**NO SE PERMITE CARGAR EL ARCHIVO, PROCESO CANCELADO, Enter para SALIR**" FOR enter
          EXIT PROGRAM
    END IF

END FUNCTION

REPORT gen_rechazo (rechazo,rrechazo)
  DEFINE 
    rechazo       LIKE afi_mae_afiliado.n_unico
  DEFINE
    rrechazo   CHAR(50)

  OUTPUT              
  TOP MARGIN 0     
  LEFT MARGIN 0    
  BOTTOM MARGIN 0  
  PAGE LENGTH 60    

    FORMAT
        PAGE HEADER

        PRINT COLUMN 01,s_codigo_afore,"  ",vrazon_social CLIPPED,
              COLUMN 68,TODAY USING "dd-mm-yyyy"
        SKIP 2 LINE
        PRINT COLUMN 08,"REGISTROS RECIBIDOS POR OP.54 NOTIFICACION DE CUENTAS "
        SKIP 1 LINE
        PRINT COLUMN 01,"           REGISTROS RECHAZADOS EN LA CARGA PREVIA"
        PRINT COLUMN 01,"---------------------------------------------------------------------"
        PRINT COLUMN 02,"CURP",
              COLUMN 20,"MOTIVO RECHAZO"
        PRINT COLUMN 01,"---------------------------------------------------------------------"
        SKIP 1 LINE

    ON EVERY ROW
        PRINT COLUMN 001,rechazo, " ",rrechazo


END REPORT