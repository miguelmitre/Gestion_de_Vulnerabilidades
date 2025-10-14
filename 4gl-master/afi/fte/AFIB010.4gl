############################################################################
#Proyecto          => Sistema de Afore. ( MEXICO )                         #
#Propietario       => E.F.P.                                               #
#Programa AFIB010  => GENERA RESULTADO DE LA CONFRONTA                     #
#Fecha             => 13 DE OCTUBRE DEL 2000.                              #
#Actualizado       => ALEJANDRO CAMPOS SUAREZ.                             #
#Sistema           => AFI                                                  #
############################################################################
DATABASE safre_af

GLOBALS

    DEFINE enter CHAR(1)

    DEFINE reg_cza_dup RECORD
        folio             INTEGER ,
        tipo_registro     CHAR(2) ,
        id_servicio       CHAR(2) ,
        id_operacion      CHAR(2) ,
        tipo_ent_origen   CHAR(2) ,
        cve_ent_origen    CHAR(3) ,
        tipo_ent_dest     CHAR(2) ,
        cve_ent_dest      CHAR(3) ,
        fe_presen         DATE    ,
        consecutivo       SMALLINT,
        cod_result        CHAR(2) ,
        mot_rechazo       CHAR(9) ,
        usuario           CHAR(8) ,
        factualiza        DATE
    END RECORD

    DEFINE reg_det_dup_sol RECORD
        folio            INTEGER  ,
        tipo_registro    CHAR(2)  ,
        contador         DECIMAL(10,0),
        tipo_ent_sol     CHAR(2)  ,
        cve_ent_sol      CHAR(3)  ,
        curp_sol         CHAR(18) ,
        nss_sol          CHAR(11) ,
        rfc_sol          CHAR(13) ,
        paterno          CHAR(40) ,
        materno          CHAR(40) ,
        nombres          CHAR(40) ,
        sexo             CHAR(1)  ,
        ent_nac          CHAR(2)  ,
        fena             DATE     ,
        nombre_imss      CHAR(50) ,
        tip_prob         CHAR(1)  ,
        reg_asociados    SMALLINT ,
        diag_aclara      CHAR(2)  ,
        cod_result       CHAR(2)  ,
        diag_proceso1    CHAR(3)  ,
        diag_proceso2    CHAR(3)  ,
        diag_proceso3    CHAR(3)  ,
        diag_proceso4    CHAR(3)  ,
        diag_proceso5    CHAR(3)  ,
        id_lote_origen   CHAR(16) ,
        id_origen_reg    CHAR(1)
    END RECORD

    DEFINE reg_det_dup_aso RECORD
        folio            INTEGER  ,
        tipo_registro    CHAR(2)  ,
        contador         DECIMAL(10,0),
        nss_sol          CHAR(11) ,
        tipo_ent_dup     CHAR(2)  ,
        cve_ent_dup      CHAR(3)  ,
        curp_dup         CHAR(18) ,
        nss_dup          CHAR(11) ,
        rfc_dup          CHAR(13) ,
        paterno          CHAR(40) ,
        materno          CHAR(40) ,
        nombres          CHAR(40) ,
        sexo             CHAR(1)  ,
        ent_nac          CHAR(2)  ,
        fena             DATE     ,
        nombre_imss      CHAR(50) ,
        diag_aclara      CHAR(2)  ,
        cod_result       CHAR(2)  ,
        diag_proceso1    CHAR(3)  ,
        diag_proceso2    CHAR(3)  ,
        diag_proceso3    CHAR(3)  ,
        diag_proceso4    CHAR(3)  ,
        diag_proceso5    CHAR(3)  ,
        id_lote_origen   CHAR(16) ,
        id_origen_reg    CHAR(01)
    END RECORD

    DEFINE reg_sum_dup RECORD
        folio              INTEGER ,
        tipo_registro      CHAR(2) ,
        num_registros      DECIMAL(9,0),
        tot_reg_aso        DECIMAL(9,0),
        fecha_presentacion DATE,
        usuario            CHAR(8),
        factualiza         DATE
    END RECORD

    DEFINE 
        cont           ,
        cont1          ,
        cont2          ,
        cont3          ,
        tot_registros  INTEGER

    DEFINE 
        HOY             DATE     ,
        aux_pausa       CHAR(1)  ,
        char            CHAR(1)  ,
        vcodigo_afore   CHAR(3)  ,
        vconfronta      CHAR(40) ,
        borra          	CHAR(200),
        G_LISTA	     	CHAR(500),
        cat            	CHAR(500),
        vlote          	INTEGER  , 
        vfolio2        	INTEGER  , 
        vfolio         	INTEGER  ,
        vtotal          INTEGER  ,
        vtot_reg        INTEGER  ,
        vtot_noa        INTEGER  ,  
        g_lista1        CHAR(200),
        usuario         CHAR(8)  ,
        imp             CHAR(200)

    DEFINE g_seg_modulo RECORD LIKE seg_modulo.*
    DEFINE cla_sel       CHAR(200)
    DEFINE cla_sel1      CHAR(200)
    DEFINE x_lotes RECORD LIKE tab_lote.*

    DEFINE vcod_afore       CHAR(05)
    DEFINE vrazon_social    CHAR(50)
    DEFINE nom_archivo      CHAR(100)

END GLOBALS

MAIN

    OPTIONS INPUT WRAP,
    PROMPT LINE LAST,
    ACCEPT KEY CONTROL-I
    DEFER INTERRUPT

    CALL STARTLOG("AFIB010.log")
    CALL inicio()  #i
    CALL proceso_principal()  #pp

END MAIN

FUNCTION inicio()
#i---------------

    LET HOY = TODAY

    SELECT codigo_afore
    INTO   vcodigo_afore
    FROM   tab_afore_local

    SELECT MAX(lotes_fecha),
           lotes_cod,
           lotes_desc,
           0,
           1
    INTO   x_lotes.* 
    FROM   tab_lote
    WHERE  lotes_cod = 14
    GROUP BY 2,3,4,5

    IF x_lotes.lotes_fecha <> HOY THEN
        UPDATE tab_lote 
        SET    lotes_fecha = HOY,
               lotes_correlativo = 1
        WHERE  lotes_cod = 14
        AND    lotes_fecha = x_lotes.lotes_fecha
    ELSE
        UPDATE tab_lote 
        SET    lotes_correlativo = lotes_correlativo + 1
        WHERE  lotes_cod = 14
        AND    lotes_fecha = x_lotes.lotes_fecha
    END IF

    SELECT lotes_correlativo
    INTO   vlote
    FROM   tab_lote
    WHERE  lotes_cod = 14
    AND    lotes_fecha = HOY

    SELECT *, USER
    INTO   g_seg_modulo.*, usuario
    FROM   seg_modulo
    WHERE  modulo_cod = 'afi'

    LET vtotal   = 0
    LET vtot_reg = 0
    LET vtot_noa = 0

    LET g_lista1 = g_seg_modulo.ruta_listados CLIPPED,"/",usuario CLIPPED,
                  "CIFCTR_RESP_CONF.", HOY USING "DDMMYY"

END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------

    OPEN WINDOW ventana_1 AT 2,2 WITH FORM "AFIB0101" ATTRIBUTE(BORDER)

    DISPLAY "AFIB010            GENERA RESPUESTA DE CONFRONTA PARA                                " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY "                          POSIBLES DUPLICADOS                                        " AT 4,1 ATTRIBUTE(REVERSE)

    DISPLAY "    < Esc > Grabar                                        < Ctrl-C > Salir   " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"dd-mm-yyyy" AT 3,67 ATTRIBUTE(REVERSE)

    INPUT BY NAME vfolio

    AFTER FIELD vfolio

        SELECT "X"
        FROM   afi_cza_dup
        WHERE  folio = vfolio
        GROUP BY 1

        IF vfolio IS NULL THEN
            ERROR "NO EXISTE EL FOLIO"
            NEXT FIELD vfolio
        END IF

        ON KEY (INTERRUPT)
            ERROR " PROCESO CANCELADO "
            SLEEP 2
            EXIT PROGRAM

        ON KEY (ESC)
            ERROR " PROCESANDO INFORMACION " 

        CALL genera_reporte() #gr
        CALL actualiza_marca()

        ERROR "PROCESO TERMINADO" SLEEP 2
        PROMPT "Archivo plano generado, [Enter] p/salir " FOR enter
        EXIT PROGRAM

    END INPUT

    CLEAR WINDOW ventana_1
    CLOSE WINDOW ventana_1

END FUNCTION

FUNCTION actualiza_marca()
#am-----------------------

    UPDATE afi_det_dup_sol
    SET    marca       = 2,
           fecha_envio = TODAY
    WHERE  folio       = vfolio
    AND    marca       = 1

    UPDATE afi_det_dup_aso
    SET    marca       = 2,
           fecha_envio = TODAY
    WHERE  folio       = vfolio
    AND    marca       = 1

END FUNCTION

FUNCTION genera_reporte()
#gr----------------------

    LET G_LISTA = g_seg_modulo.ruta_envio CLIPPED,"/DETDUP"

    LET cont  = 0
    LET cont2 = 0
    LET cont3 = 0

    LET cla_sel = "SELECT * "           ,
                  "FROM   afi_det_dup_aso " ,
                  "WHERE  folio    = ? " ,
                  "AND    nss_sol  = ? " ,
                  "AND    curp_dup = ? " ,
                  "AND    marca in(1,2)"

    PREPARE claexe FROM cla_sel
    DECLARE cur_3 CURSOR FOR claexe

    START REPORT listado_2 TO G_LISTA
    DECLARE cur_2 CURSOR FOR
        SELECT *
        FROM   afi_det_dup_sol
        WHERE  folio = vfolio
        AND    marca in(1,2)

    FOREACH cur_2 INTO reg_det_dup_sol.*
        LET cont  = cont  + 1
        LET cont2 = cont2 + 1

        LET vtotal = vtotal + 1

        IF reg_det_dup_sol.id_origen_reg = "1" THEN
           LET vtot_reg = vtot_reg + 1
        END IF
        IF reg_det_dup_sol.id_origen_reg = "2" THEN
           LET vtot_noa = vtot_noa + 1
        END IF

        OUTPUT TO REPORT listado_2(reg_det_dup_sol.*) #l2
    END FOREACH

    FINISH REPORT listado_2

    LET G_LISTA = g_seg_modulo.ruta_envio CLIPPED,"/CZADUP"

    DECLARE cur_4 CURSOR FOR
    SELECT *
    FROM   afi_cza_dup
    WHERE  folio  = vfolio

    START REPORT listado_4 TO G_LISTA 
        FOREACH cur_4 INTO reg_cza_dup.*
            LET reg_cza_dup.tipo_ent_origen = "01"
            LET reg_cza_dup.cve_ent_origen  = vcodigo_afore
            LET reg_cza_dup.tipo_ent_dest   = "03"
            LET reg_cza_dup.cve_ent_dest    = "001"
            LET reg_cza_dup.fe_presen       = HOY 
            LET reg_cza_dup.consecutivo     = vlote 

            OUTPUT TO REPORT listado_4(reg_cza_dup.*) #l4
        END FOREACH
    FINISH REPORT listado_4

    LET G_LISTA = g_seg_modulo.ruta_envio CLIPPED,"/SUMDUP" 

    DECLARE cur_5 CURSOR FOR
    SELECT *
    FROM   afi_sum_dup
    WHERE  folio  = vfolio

    START REPORT listado_5 TO G_LISTA
        FOREACH cur_5 INTO reg_sum_dup.*
            OUTPUT TO REPORT listado_5(reg_sum_dup.*) #l5
        END FOREACH
    FINISH REPORT listado_5

    LET cat = "cat ",g_seg_modulo.ruta_envio CLIPPED,"/CZADUP ",
                     g_seg_modulo.ruta_envio CLIPPED,"/DETDUP ",
                     g_seg_modulo.ruta_envio CLIPPED,"/SUMDUP > ",
                     g_seg_modulo.ruta_envio CLIPPED,"/",
                     "D",HOY USING"YYYYMMDD",vlote USING"&&",".DUP"
    RUN cat
    DISPLAY "Nombre archivo: ", 
    g_seg_modulo.ruta_envio CLIPPED,
    "/D",HOY USING "YYYYMMDD", vlote USING "&&", ".DUP" AT 18,02

    START REPORT listado1 TO g_lista1

    DISPLAY "TOTAL REGISTROS NORMALES     : ",vtot_reg  AT 14,18
    DISPLAY "TOTAL REGISTROS NO AFILIADOS : ",vtot_noa  AT 15,18
    DISPLAY "TOTAL DE REGISTROS           : ",vtotal    AT 16,18

    OUTPUT TO REPORT listado1(vtotal, vtot_noa, vtot_reg)

    FINISH REPORT listado1

    LET imp = "lp ", g_lista1
    RUN imp

END FUNCTION

REPORT listado_2(reg_2)
#l2--------------------

    DEFINE reg_2 RECORD
        folio            INTEGER  ,
        tipo_registro    CHAR(2)  ,
        contador         DECIMAL(10,0),
        tipo_ent_sol     CHAR(2)  ,
        cve_ent_sol      CHAR(3)  ,
        curp_sol         CHAR(18) ,
        nss_sol          CHAR(11) ,
        rfc_sol          CHAR(13) ,
        paterno          CHAR(40) ,
        materno          CHAR(40) ,
        nombres          CHAR(40) ,
        sexo             CHAR(1)  ,
        ent_nac          CHAR(2)  ,
        fena             DATE     ,
        nombre_imss      CHAR(50) ,
        doc_prob         CHAR(1)  ,
        reg_asociados    SMALLINT ,
        diag_aclara      CHAR(2)  ,
        cod_result       CHAR(2)  ,
        diag_proceso1    CHAR(3)  ,
        diag_proceso2    CHAR(3)  ,
        diag_proceso3    CHAR(3)  ,
        diag_proceso4    CHAR(3)  ,
        diag_proceso5    CHAR(3)  ,
        id_lote_origen   CHAR(16) ,
        id_origen_reg    CHAR(1)
    END RECORD

    DEFINE reg_3 RECORD
        folio            INTEGER  ,
        tipo_registro    CHAR(2)  ,
        contador         DECIMAL(10,0),
        nss_sol          CHAR(11) ,
        tipo_ent_dup     CHAR(2)  ,
        cve_ent_dup      CHAR(3)  ,
        curp_dup         CHAR(18) ,
        nss_dup          CHAR(11) ,
        rfc_dup          CHAR(13) ,
        paterno          CHAR(40) ,
        materno          CHAR(40) ,
        nombres          CHAR(40) ,
        sexo             CHAR(1)  ,
        ent_nac          CHAR(2)  ,
        fena             DATE     ,
        nombre_imss      CHAR(50) ,
        diag_aclara      CHAR(2)  ,
        cod_result       CHAR(2)  ,
        diag_proceso1    CHAR(3)  ,
        diag_proceso2    CHAR(3)  ,
        diag_proceso3    CHAR(3)  ,
        diag_proceso4    CHAR(3)  ,
        diag_proceso5    CHAR(3)  ,
        id_lote_origen   CHAR(16) ,
        id_origen_reg    CHAR(1)
    END RECORD

    DEFINE
        rcurp            CHAR(16)

    OUTPUT
        PAGE LENGTH 1
        LEFT MARGIN 0
        RIGHT MARGIN 0
        TOP MARGIN 0
        BOTTOM MARGIN 0

    FORMAT
    ON EVERY ROW
        PRINT 
            COLUMN 001,"02",#tipo_registro
            COLUMN 003,cont USING"&&&&&&&&&&" ,#cont_servicio ,
            COLUMN 013,reg_2.tipo_ent_sol,
            COLUMN 015,reg_2.cve_ent_sol,
            COLUMN 018,reg_2.curp_sol,
            COLUMN 036,reg_2.nss_sol,
            COLUMN 047,reg_2.rfc_sol,
            COLUMN 060,reg_2.paterno,
            COLUMN 100,reg_2.materno,
            COLUMN 140,reg_2.nombres,
            COLUMN 180,reg_2.sexo,
            COLUMN 181,reg_2.ent_nac,
            COLUMN 183,reg_2.fena USING "yyyymmdd", 
            COLUMN 191,reg_2.nombre_imss,
            COLUMN 241,reg_2.doc_prob,
            COLUMN 242,reg_2.reg_asociados using "&&",
            COLUMN 244,reg_2.diag_aclara,
            COLUMN 246,reg_2.cod_result,
            COLUMN 248,reg_2.diag_proceso1,
            COLUMN 251,reg_2.diag_proceso2,
            COLUMN 254,reg_2.diag_proceso3,
            COLUMN 257,reg_2.diag_proceso4,
            COLUMN 260,reg_2.diag_proceso5,
            COLUMN 263,reg_2.id_lote_origen,
            COLUMN 279,reg_2.id_origen_reg,
            101 SPACES

            LET rcurp = reg_2.curp_sol[1,16]

    OPEN cur_3
        USING reg_2.folio,reg_2.nss_sol,rcurp
        WHILE TRUE
            FETCH cur_3 INTO reg_3.*
            LET cont = cont + 1
            IF SQLCA.SQLCODE <> 0 THEN
                CLOSE cur_3
                LET cont = cont - 1
                EXIT WHILE
            END IF
        PRINT 
            COLUMN 001,"03",#tipo_registro
            COLUMN 003,cont USING"&&&&&&&&&&" ,#cont_servicio ,
            COLUMN 013,reg_3.nss_sol,
            COLUMN 024,reg_3.tipo_ent_dup,
            COLUMN 026,reg_3.cve_ent_dup,
            COLUMN 029,reg_3.curp_dup,
            COLUMN 047,reg_3.nss_dup,
            COLUMN 058,reg_3.rfc_dup,
            COLUMN 071,reg_3.paterno,
            COLUMN 111,reg_3.materno,
            COLUMN 151,reg_3.nombres,
            COLUMN 191,reg_3.sexo,
            COLUMN 192,reg_3.ent_nac,
            COLUMN 194,reg_3.fena USING "yyyymmdd",
            COLUMN 202,reg_3.nombre_imss,
            COLUMN 252,reg_3.diag_aclara,
            COLUMN 254,reg_3.cod_result,
            COLUMN 256,reg_3.diag_proceso1,
            COLUMN 259,reg_3.diag_proceso2,
            COLUMN 262,reg_3.diag_proceso3,
            COLUMN 265,reg_3.diag_proceso4,
            COLUMN 268,reg_3.diag_proceso5,
            COLUMN 271,reg_3.id_lote_origen,
            COLUMN 287,reg_3.id_origen_reg,
            93 SPACES

            LET cont3 = cont3 + 1
     END WHILE
END REPORT

REPORT listado_4(reg_4)
#l4--------------------

    DEFINE reg_4 RECORD
        folio             INTEGER ,
        tipo_registro     CHAR(2) ,
        id_servicio       CHAR(2) ,
        id_operacion      CHAR(2) ,
        tipo_ent_origen   CHAR(2) ,
        cve_ent_origen    CHAR(3) ,
        tipo_ent_dest     CHAR(2) ,
        cve_ent_dest      CHAR(3) ,
        fe_presen         DATE    ,
        consecutivo       SMALLINT,
        cod_result        CHAR(2) ,
        mot_rechazo       CHAR(9) ,
        usuario           CHAR(8) ,
        factualiza        DATE
    END RECORD

    OUTPUT
        PAGE LENGTH 1
        LEFT MARGIN 0
        RIGHT MARGIN 0
        TOP MARGIN 0
        BOTTOM MARGIN 0

    FORMAT
    ON EVERY ROW
        PRINT
            COLUMN 001,reg_4.tipo_registro,#tipo_registro
            COLUMN 003,reg_4.id_servicio,
            COLUMN 005,reg_4.id_operacion,
            COLUMN 007,reg_4.tipo_ent_origen,
            COLUMN 009,reg_4.cve_ent_origen,
            COLUMN 012,reg_4.tipo_ent_dest,
            COLUMN 014,reg_4.cve_ent_dest,
            COLUMN 017,reg_4.fe_presen USING "YYYYMMDD",
            COLUMN 025,reg_4.consecutivo USING "&&&",
            COLUMN 028, 353 SPACES

END REPORT 

REPORT listado_5(reg_5)
#l5-------------------

    DEFINE reg_5 RECORD
        folio              INTEGER ,
        tipo_registro      CHAR(2) ,
        num_registros      DECIMAL(9,0),
        tot_reg_aso        DECIMAL(9,0),
        fecha_presentacion DATE,
        usuario            CHAR(8),
        factualiza         DATE
    END RECORD

    OUTPUT
        PAGE LENGTH 1
        LEFT MARGIN 0
        RIGHT MARGIN 0
        TOP MARGIN 0
        BOTTOM MARGIN 0

    FORMAT
    ON EVERY ROW
        PRINT
            COLUMN 01,reg_5.tipo_registro,#tipo_registro
            COLUMN 03,cont2 USING "&&&&&&&&&",
            COLUMN 12,cont3 USING "&&&&&&&&&",
            COLUMN 21, 360 SPACES

END REPORT

REPORT listado1(rnss_sol_2,rcurp_sol_2,rnss_tot)

  DEFINE rnss_sol_2 ,
         rcurp_sol_2,
         rnss_tot         INTEGER

    OUTPUT
      PAGE LENGTH   155
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0

    FORMAT
      PAGE HEADER

        SELECT afore_cod, afore_desc
          INTO vcod_afore, vrazon_social
          FROM tab_afore
         WHERE marca = 1 

        LET nom_archivo = "D",HOY USING "YYYYMMDD", vlote USING "&&", 
                             ".DUP"

--        PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
        PRINT COLUMN 03,vcod_afore, "  ",vrazon_social,
              COLUMN 65,"FECHA  : ",TODAY USING "DD-MM-YYYY"
        PRINT COLUMN 65,"Pagina : ",PAGENO USING "####,###"
        SKIP 2 LINE
        PRINT COLUMN 03,"AFIB010       ",
                        " GENERACION ARCHIVO RESULTADO DE CONFRONTA "
        PRINT COLUMN 03,"--------------------------------------------------",
                        "------------------------------------"

        SKIP 2 LINES

        PRINT COLUMN 03,"FOLIO ASIGNADO POR EL SISTEMA:  ",vfolio      
        USING "##########"

        PRINT COLUMN 03,"NOMBRE DEL ARCHIVO CARGADO   :  ",nom_archivo CLIPPED
        SKIP 2 LINE

        PRINT COLUMN 03,"TOTAL DE REGISTROS POR NSS   :  ",rnss_sol_2  
        USING "##########"
        SKIP 1 LINE

        PRINT COLUMN 03,"TOTAL DE REGISTROS POR CURP  :  ",rcurp_sol_2 
        USING "##########"
        SKIP 1 LINE

        PRINT COLUMN 03,"TOTAL DE REGISTROS           :  ",rnss_tot    
        USING "##########"

END REPORT
