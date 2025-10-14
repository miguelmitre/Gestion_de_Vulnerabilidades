######################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                   #
#Propietario       => EFP                                            #
#Programa          => ACRC024                                        #
#Descripcion       => RECIBE ARCHIVO OPER.15 DEVOLUCIONES DE SALDO   #
#                     DE ANUALIDADES GARANTIZADAS                    #
#Sistema           => ACR                                            #
#Fecha creacion    => 19 NOVIEMBRE 2009                              #
#Por               => STEFANIE DANIELA VERA PIÑA                     #
######################################################################

DATABASE safre_af

GLOBALS

    DEFINE reg_cza_dev_sal_ag RECORD
        tipo_registro         CHAR(2)  ,
        ident_servicio        CHAR(2)  ,
        ident_operacion       CHAR(2)  ,
        tipo_ent_origen       CHAR(2)  ,
        cve_ent_origen        CHAR(3)  ,
        tipo_ent_destino      CHAR(2)  ,
        cve_ent_destino       CHAR(3)  ,
        ent_fed_envio_lote    CHAR(3)  ,
        fecha_presentacion    DATE     ,
        consec_lote_dia       SMALLINT ,
        cve_mod_recepcion     CHAR(2)  ,
        cod_result_operac     CHAR(2)  ,
        rechazo               CHAR(3)
    END RECORD

    DEFINE reg_det_dev_sal_ag RECORD
        tipo_registro         CHAR(2)       ,
        cont_servicio         DECIMAL(10,0) ,
        tipo_recep_cuenta     CHAR(2)       ,
        cve_recep_cuenta      CHAR(3)       ,
        tipo_ced_cuenta       CHAR(2)       ,
        cve_ced_cuenta        CHAR(3)       ,
		  tipo_transferencia    CHAR(002)     , 
        fecha_presentacion    DATE          ,
        fecha_movimiento      DATE          ,
        curp                  CHAR(18)      ,
        nss                   CHAR(11)      ,
        id_asignacion         CHAR(1)       ,
		  tipo_devolucion       CHAR(1)       ,
        rfc_infonavit         CHAR(13)      ,
        paterno               CHAR(40)      ,
        materno               CHAR(40)      ,
        nombres               CHAR(40)      ,
        ident_lote_devol      CHAR(16)      ,
		  aivs_v97              DECIMAL(22,6) ,
		  nss_separacion        CHAR(11)      ,
        origen_dev            SMALLINT      ,
        mto_aport_5porc       DECIMAL(13,2) ,
		  int_aport_5porc       DECIMAL(22,6) ,
		  aivs_v92              DECIMAL(22,6) ,
        cod_result_operac     CHAR(2)       ,
        diag_proceso          CHAR(3)       ,
        nombre_imss           CHAR(50)      ,
        num_cred_infonavit    DECIMAL(10,0) 
    END RECORD

    DEFINE reg_sum_dev_sal_ag RECORD
        tipo_registro         CHAR(02)      ,
        cant_reg_det          DECIMAL(9,0)  , 
        sum_aivs_v97          DECIMAL(22,6) ,
        sum_aivs_v92          DECIMAL(22,6) 
    END RECORD

    DEFINE g_param_taa RECORD LIKE seg_modulo.*

    DEFINE
        HOY                   DATE

    DEFINE
        enter                 CHAR(1)   ,
        generar               CHAR(25)  ,
        archivo_traspaso      CHAR(200) ,
        c_fecha_cza           CHAR(8)   ,
        f_fecha_cza           CHAR(10)  ,
        c_fecha_det           CHAR(8)   ,
        f_fecha_det           CHAR(10)  ,
        c_fecha_bnx           CHAR(8)   ,
        f_fecha_bnx           CHAR(10)

    DEFINE
        vtotal_reg            INTEGER

    DEFINE 
        cuantos               ,
        s_codigo_afore        ,
        cont                  SMALLINT

    DEFINE g RECORD
        nss                   CHAR(11),
        tipo_transferencia    CHAR(2),
        descripcion           CHAR(30)
    END RECORD

    DEFINE
        g_usuario             CHAR(8),
        vrazon_social         CHAR(25)

END GLOBALS


MAIN

    DEFER INTERRUPT
    OPTIONS 
        INPUT WRAP,
        PROMPT LINE LAST  ,
        ACCEPT KEY CONTROL-I

   CALL inicio() #i
   CALL proceso_principal() #pp

END MAIN


FUNCTION proceso_principal()
#pp-------------------------

   OPEN WINDOW ventana_1 AT 4,4 WITH FORM "ACRC0241" ATTRIBUTE(BORDER)
   DISPLAY " ACRC024  RECIBE ARCHIVO OPER.15 ANUALIDADES GARANTIZADAS                      " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY "<ESC> Ejecutar                                          <Ctrl-C> Salir " AT 1,1
   DISPLAY HOY USING"DD-MM-YYYY" AT 3,60 ATTRIBUTE(REVERSE)

   INPUT BY NAME generar
       AFTER FIELD generar
	        IF generar IS NULL THEN
	            ERROR " CAMPO NO PUEDE SER NULO "
	            NEXT FIELD generar
	        END IF

           LET archivo_traspaso = g_param_taa.ruta_rescate CLIPPED,
                                  "/",generar CLIPPED

           LOAD FROM archivo_traspaso INSERT INTO safre_tmp:tmp_dev_sal_ag
                     
           SELECT count(*)
           INTO   cuantos
           FROM   safre_tmp:tmp_dev_sal_ag
                    
           IF cuantos = 0 THEN
               DISPLAY  " NOMBRE DE ARCHIVO INCORRECTO O ARCHIVO VACIO "
               AT 19,2 ATTRIBUTE(REVERSE)
               SLEEP 3
               NEXT FIELD generar
           ELSE
               EXIT INPUT
           END IF

           WHENEVER ERROR STOP

       ON KEY (INTERRUPT)
           DISPLAY " PROCESO CANCELADO " AT 19,1 ATTRIBUTE(REVERSE)
           SLEEP 2
           EXIT PROGRAM
   END INPUT

   CALL crea_tablas()       #ct
   CALL validacion_previa() #vp
   CALL lee_archivo_plano() #lap
   CALL impresion_reporte() #ir

   SELECT COUNT(*)
   INTO   vtotal_reg
   FROM   safre_tmp:det_dev_sal_ag

   DISPLAY "TOTAL DE REGISTROS : ",vtotal_reg AT 11,5

   PROMPT " PROCESO FINALIZADO...<ENTER> PARA SALIR " FOR CHAR enter

END FUNCTION


FUNCTION inicio()
#i-------------

    LET HOY = TODAY

    SELECT codigo_afore, razon_social
    INTO   s_codigo_afore, vrazon_social
    FROM   tab_afore_local             

    SELECT *, USER 
    INTO   g_param_taa.*, g_usuario
    FROM   seg_modulo
    WHERE  modulo_cod = 'acr'

    WHENEVER ERROR CONTINUE
        DATABASE safre_tmp
        DROP TABLE tmp_dev_sal_ag

        CREATE TABLE tmp_dev_sal_ag
          (n_registros CHAR(730))

        DATABASE safre_af
    WHENEVER ERROR STOP

END FUNCTION


FUNCTION crea_tablas()
#ct-------------------

    WHENEVER ERROR CONTINUE
        DATABASE safre_tmp

        DROP TABLE cza_dev_sal_ag
        DROP TABLE det_dev_sal_ag
        DROP TABLE sum_dev_sal_ag
    WHENEVER ERROR STOP

    CREATE TABLE cza_dev_sal_ag
       (
        tipo_registro         CHAR(2)  ,
        ident_servicio        CHAR(2)  ,
        ident_operacion       CHAR(2)  ,
        tipo_ent_origen       CHAR(2)  ,
        cve_ent_origen        CHAR(3)  ,
        tipo_ent_destino      CHAR(2)  ,
        cve_ent_destino       CHAR(3)  ,
        ent_fed_envio_lote    CHAR(3)  ,
        fecha_presentacion    DATE     ,
        consec_lote_dia       SMALLINT ,
        cve_mod_recepcion     CHAR(2)  ,
        cod_result_operac     CHAR(2)  ,
        rechazo               CHAR(3)
       );

    CREATE TABLE det_dev_sal_ag 
       (
        tipo_registro         CHAR(2)       ,
        cont_servicio         DECIMAL(10,0) ,
        tipo_recep_cuenta     CHAR(2)       ,
        cve_recep_cuenta      CHAR(3)       ,
        tipo_ced_cuenta       CHAR(2)       ,
        cve_ced_cuenta        CHAR(3)       ,
        tipo_transferencia    CHAR(002)     ,
        fecha_presentacion    DATE          ,
		  fecha_movimiento      DATE          ,
        curp                  CHAR(18)      ,
        nss                   CHAR(11)      ,
        id_asignacion         CHAR(1)       ,
        tipo_devolucion       CHAR(1)       ,
        rfc_infonavit         CHAR(13)      ,
        paterno               CHAR(40)      ,
        materno               CHAR(40)      ,
        nombres               CHAR(40)      ,
        ident_lote_devol      CHAR(16)      ,
        aivs_v97              DECIMAL(22,6) ,
        nss_separacion        CHAR(11)      ,
        origen_dev            SMALLINT      ,
        mto_aport_5porc       DECIMAL(13,2) ,
        int_aport_5porc       DECIMAL(22,6) ,
        aivs_v92              DECIMAL(22,6) ,
        cod_result_operac     CHAR(2)       ,
        diag_proceso          CHAR(3)       ,
        nombre_imss           CHAR(50)      ,
        num_cred_infonavit    DECIMAL(10,0) ,
		  estado                SMALLINT
       )

    CREATE TABLE sum_dev_sal_ag
       (
        tipo_registro         CHAR(02)      ,
        cant_reg_det          DECIMAL(9,0)  ,
        sum_aivs_v97          DECIMAL(22,6) ,
        sum_aivs_v92          DECIMAL(22,6)
       );

      DATABASE safre_af

END FUNCTION


FUNCTION validacion_previa()
#vp------------------------

    DEFINE
        c2_tipo_registro      CHAR(2)

    DEFINE
        sw_1                  ,
        sw_2                  ,
        sw_9                  SMALLINT            

    DECLARE cur_2 CURSOR FOR
    SELECT UNIQUE(n_registros[1,2])
    FROM   safre_tmp:tmp_dev_sal_ag

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
        cont                 SMALLINT
 
    DEFINE 
        carga_reg            CHAR(730) ,
        c2_ident_operacion   CHAR(2)

    DECLARE cur_1 CURSOR FOR
    SELECT  * 
    FROM    safre_tmp:tmp_dev_sal_ag
   
    LET cont = 0
    LET c2_ident_operacion = ""
    FOREACH cur_1 INTO carga_reg
        LET cont = cont + 1

        #---ENCABEZADO DEVOLUCION DE SALDOS---#

        IF carga_reg[5,6] = "15" THEN
            LET c2_ident_operacion = "15"
            LET reg_cza_dev_sal_ag.tipo_registro      = carga_reg[001,002] 
            LET reg_cza_dev_sal_ag.ident_servicio     = carga_reg[003,004]
            LET reg_cza_dev_sal_ag.ident_operacion    = carga_reg[005,006]
            LET reg_cza_dev_sal_ag.tipo_ent_origen    = carga_reg[007,008]
            LET reg_cza_dev_sal_ag.cve_ent_origen     = carga_reg[009,011]
            LET reg_cza_dev_sal_ag.tipo_ent_destino   = carga_reg[012,013]
            LET reg_cza_dev_sal_ag.cve_ent_destino    = carga_reg[014,016]
            LET reg_cza_dev_sal_ag.ent_fed_envio_lote = carga_reg[017,019]
            LET c_fecha_cza                           = carga_reg[020,027]
            LET reg_cza_dev_sal_ag.consec_lote_dia    = carga_reg[028,030]
            LET reg_cza_dev_sal_ag.cve_mod_recepcion  = carga_reg[031,032]
            LET reg_cza_dev_sal_ag.cod_result_operac  = carga_reg[033,034]
            LET reg_cza_dev_sal_ag.rechazo            = carga_reg[035,043]

            LET f_fecha_cza = c_fecha_cza[5,6],"/",
                              c_fecha_cza[7,8],"/",
                              c_fecha_cza[1,4] 

            LET reg_cza_dev_sal_ag.fecha_presentacion = f_fecha_cza

            INSERT INTO safre_tmp:cza_dev_sal_ag VALUES(reg_cza_dev_sal_ag.*)
        END IF

        #---DETALLE DEVOLUCION DE SALDOS---#

        IF carga_reg[1,2] = "02" AND c2_ident_operacion = "15" THEN
            LET reg_det_dev_sal_ag.tipo_registro      = carga_reg[001,002]
            LET reg_det_dev_sal_ag.cont_servicio      = carga_reg[003,012]
            LET reg_det_dev_sal_ag.tipo_recep_cuenta  = carga_reg[013,014]
            LET reg_det_dev_sal_ag.cve_recep_cuenta   = carga_reg[015,017]
            LET reg_det_dev_sal_ag.tipo_ced_cuenta    = carga_reg[018,019]
            LET reg_det_dev_sal_ag.cve_ced_cuenta     = carga_reg[020,022]
            LET reg_det_dev_sal_ag.tipo_transferencia = carga_reg[023,024] 
            LET c_fecha_det                           = carga_reg[025,032]
            LET c_fecha_bnx                           = carga_reg[033,040]
            LET reg_det_dev_sal_ag.curp               = carga_reg[041,058]
            LET reg_det_dev_sal_ag.nss                = carga_reg[059,069]
            LET reg_det_dev_sal_ag.id_asignacion      = carga_reg[083,083]
				LET reg_det_dev_sal_ag.tipo_devolucion    = carga_reg[084,084]
            LET reg_det_dev_sal_ag.rfc_infonavit      = carga_reg[085,097]
            LET reg_det_dev_sal_ag.paterno            = carga_reg[098,137]
            LET reg_det_dev_sal_ag.materno            = carga_reg[138,177]
            LET reg_det_dev_sal_ag.nombres            = carga_reg[178,217]
            LET reg_det_dev_sal_ag.ident_lote_devol   = carga_reg[240,255]
            LET reg_det_dev_sal_ag.aivs_v97           = carga_reg[475,489]
				LET reg_det_dev_sal_ag.nss_separacion     = carga_reg[505,515]
            LET reg_det_dev_sal_ag.origen_dev         = carga_reg[516,517]
            LET reg_det_dev_sal_ag.mto_aport_5porc    = carga_reg[518,532] 
            LET reg_det_dev_sal_ag.int_aport_5porc    = carga_reg[533,547]
				LET reg_det_dev_sal_ag.aivs_v92           = carga_reg[550,564]
            LET reg_det_dev_sal_ag.cod_result_operac  = carga_reg[583,584]
            LET reg_det_dev_sal_ag.diag_proceso       = carga_reg[585,599]
            LET reg_det_dev_sal_ag.nombre_imss        = carga_reg[600,649]
            LET reg_det_dev_sal_ag.num_cred_infonavit = carga_reg[650,659]

            LET f_fecha_det = c_fecha_det[5,6],"/",
                              c_fecha_det[7,8],"/",
                              c_fecha_det[1,4] 

            LET reg_det_dev_sal_ag.fecha_presentacion  = f_fecha_det

            LET f_fecha_bnx = c_fecha_bnx[5,6],"/",
                              c_fecha_bnx[7,8],"/",
                              c_fecha_bnx[1,4] 

            LET reg_det_dev_sal_ag.fecha_movimiento    = f_fecha_bnx

            LET reg_det_dev_sal_ag.aivs_v97 = reg_det_dev_sal_ag.aivs_v97 
                                                 / 1000000
            LET reg_det_dev_sal_ag.aivs_v92 = reg_det_dev_sal_ag.aivs_v92 
                                                 / 1000000

            SELECT "X"
            FROM   afi_mae_afiliado
            WHERE  n_seguro = reg_det_dev_sal_ag.nss
            
            IF SQLCA.SQLCODE = 0 THEN 
                INSERT INTO safre_tmp:det_dev_sal_ag 
                VALUES(reg_det_dev_sal_ag.*,1)
            END IF
        END IF

        #---SUMARIO DEVOLUCION DE SALDOS---#

        IF carga_reg[1,2] = "09" AND c2_ident_operacion = "15" THEN
            LET c2_ident_operacion = ""
            LET reg_sum_dev_sal_ag.tipo_registro  = carga_reg[001,002]
            LET reg_sum_dev_sal_ag.cant_reg_det   = carga_reg[003,011]
            LET reg_sum_dev_sal_ag.sum_aivs_v97   = carga_reg[042,059]
            LET reg_sum_dev_sal_ag.sum_aivs_v92   = carga_reg[120,137]

            LET reg_sum_dev_sal_ag.sum_aivs_v97 = 
                reg_sum_dev_sal_ag.sum_aivs_v97 / 1000000

            LET reg_sum_dev_sal_ag.sum_aivs_v92 = 
                reg_sum_dev_sal_ag.sum_aivs_v92 / 1000000

            INSERT INTO safre_tmp:sum_dev_sal_ag VALUES(reg_sum_dev_sal_ag.*)
        END IF

    END FOREACH

END FUNCTION


FUNCTION impresion_reporte()
#ir-------------------------

    DEFINE G_IMPRE        CHAR(300)
    DEFINE c_impre        CHAR(300)
    DEFINE gimpresion     CHAR(300)
    DEFINE hora           CHAR (08)

    LET G_IMPRE = g_param_taa.ruta_listados CLIPPED,"/",g_usuario CLIPPED,
                  ".DEV_EXC_AG.",hoy USING "DD-MM-YYYY"

    START REPORT det_tra_cred_imp TO  G_IMPRE

    OUTPUT TO REPORT det_tra_cred_imp(g.*)

    FINISH REPORT det_tra_cred_imp

    LET gimpresion = "lp ",G_IMPRE
    RUN gimpresion

    LET c_impre = ' chmod 777 ', G_IMPRE CLIPPED
    RUN c_impre

END FUNCTION


REPORT det_tra_cred_imp(g)
#dtai---------------------

    DEFINE g RECORD
        nss                 CHAR(11),
        tipo_transferencia  CHAR(2),
        descripcion         CHAR(30)
    END RECORD

    DEFINE
        tot_partic_v97      DECIMAL(18,6),
        tot_partic_v92      DECIMAL(18,6),
        tot_saldo_v97       DECIMAL(18,6),
        tot_saldo_v92       DECIMAL(18,6)

    DEFINE fecha_max DATE

    OUTPUT
        TOP MARGIN 1
        BOTTOM MARGIN 0
        LEFT MARGIN 0
        RIGHT MARGIN 0
        PAGE LENGTH 60

    FORMAT
        PAGE HEADER

        SELECT SUM(aivs_v97),
               SUM(aivs_v92)
        INTO tot_partic_v97,
             tot_partic_v92
        FROM safre_tmp:det_dev_sal_ag

        PRINT COLUMN 01,s_codigo_afore,"  ",vrazon_social CLIPPED,
            COLUMN 68,HOY USING "dd-mm-yyyy"
        SKIP 2 LINE
        PRINT COLUMN 08,"REGISTROS ENVIADOS POR DEVOLUCION DE EXCEDENTES DE ANUALIDADES GARANTIZADAS "
        SKIP 1 LINE
        PRINT COLUMN 01,"---------------------------------------------------------------------"
        PRINT COLUMN 02,"NSS",
              COLUMN 15,"TIPO DE TRANSFERENCIA",
              COLUMN 43,"DESCRIPCION"
        PRINT COLUMN 01,"---------------------------------------------------------------------"
        SKIP 1 LINE
    ON EVERY ROW

      DECLARE cur1 CURSOR FOR
      SELECT a.nss,
             '15'
      FROM   safre_tmp:det_dev_sal_ag a

      FOREACH cur1 INTO g.nss,
                        g.tipo_transferencia

      LET g.descripcion = "DEVOLUCION EXCEDENTES (AG) "

        PRINT COLUMN 02,g.nss,
              COLUMN 15,g.tipo_transferencia,
              COLUMN 43,g.descripcion
      END FOREACH

     PAGE TRAILER
      SKIP 2 LINE
      PRINT COLUMN 10, "TOTAL VIVIENDA 97 : ", tot_partic_v97
      PRINT COLUMN 10, "TOTAL VIVIENDA 92 : ", tot_partic_v92
      PRINT COLUMN 60,"Pagina:",PAGENO USING "<<<<"
      PAUSE "Presione enter para continuar."

END REPORT
