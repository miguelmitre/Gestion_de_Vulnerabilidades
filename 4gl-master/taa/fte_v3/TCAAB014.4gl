###########################################################################
#Proyecto          => AFORE ( MEXICO )                                    
#Propietario       => E.F.P.                                              
#Programa TCAAB014  => VERIFICA DEVOLUCION POR VALIDACION DOCUMENTOS (CED)  
#Fecha             => 10 DE OCTUBRE DE 2001                                
#Autor             => JESUS DAVID YAÑEZ MORENO
#Sistema           => TCAA(CEDENTE)                                         
############################################################################

DATABASE safre_af

GLOBALS
    DEFINE reg ARRAY[20000] OF RECORD
        consec             SMALLINT       ,
        n_seguro           CHAR(011)      ,
        monto              DECIMAL(16,6)  ,
        tipo_traspaso      SMALLINT       ,
        estado             SMALLINT       ,
        desc_estado        CHAR(030)
    END RECORD

    DEFINE r_unico ARRAY[20000] OF RECORD
           n_unico         CHAR(018)
    END RECORD

    DEFINE
        g_afore            RECORD LIKE tab_afore_local.*   ,
        g_paramgrales      RECORD LIKE seg_modulo.*        ,
        ruta_afi           RECORD LIKE seg_modulo.*        ,
        ruta_cta           RECORD LIKE seg_modulo.*


    DEFINE
        HOY                                     , 
        fecha_trasp                             ,
        fecha_presentacion                   DATE

    DEFINE
        enter                  CHAR(0001)       ,
        HORA                   CHAR(0008)       ,
        g_usuario              CHAR(0010)       ,
        ejecuta                CHAR(0500)       ,
        sel_where              CHAR(1500)       ,
        cla_where              CHAR(1000)       ,
        n_seguro               CHAR(0011)       ,
        individual             CHAR(0002)       ,
        unificacion            CHAR(0002)       ,
        asig_uni               CHAR(0002)       ,
        asignacion             CHAR(0002)       

    DEFINE
        edo_ant                             ,
        i                                   ,
        ban                                 ,
        procedente                          ,
        tipo_devuelta_val                   ,
        cero                                ,
        uno                                 ,
        a_afiliacion                        ,
        a_traspasos                         ,
        a_unificacion                       ,
        a_verificacion                      ,
        a_recepcion                         ,
        a_sistemas                          ,
        a_uaep                              ,
        v_motivo                            ,
        captura_cambio                      ,
        captura_no_cambio            SMALLINT 

    DEFINE
        tipo_traspaso                       ,
        arr                                 ,
        src                                 , 
        pos                                 ,
        tot_01                              ,
        v_folio                             ,
        g_num_linea                 INTEGER ,
        g_raz_social        LIKE    tab_afore_local.razon_social ,
        g_cod_afore         LIKE    tab_afore_local.codigo_afore ,
        g_param_taa       RECORD    LIKE seg_modulo.*            ,
        reg_ced_valida    RECORD  
            n_seguro                CHAR(011),
            factualiza              DATE     ,
            hora                    CHAR(008)
      END RECORD,
       reg_ced_valida_max RECORD LIKE taa_cd_ced_valida.*,
       g_ruta             CHAR(100) 
      DEFINE  l_reg       RECORD
              linea       CHAR(01)
      END  RECORD

    DEFINE   taa_cd_paso_taab014                 RECORD
       folio                         INTEGER,
       n_seguro                     CHAR(11),
       cve_rechazo_ant              SMALLINT,
       desc_estado_ant              CHAR(30),
       cve_rechazo_act              SMALLINT,
       desc_estado_act              CHAR(30),
       hora                         CHAR(08) 
                             END     RECORD

  DEFINE t_fecha_presentacion       DATE
 
  DEFINE  t_paterno                  CHAR(40)
  DEFINE t_materno                  CHAR(40)
  DEFINE t_nombre                   CHAR(040)
END GLOBALS

MAIN
    DEFINE salida SMALLINT
    OPTIONS 
    PROMPT LINE LAST,
    INPUT WRAP
    CALL inicio()

    LET salida = 1

    WHILE salida = TRUE 
          CALL despliega_folios() RETURNING salida
    END WHILE

    CALL     f_100_genera_reporte()

END MAIN

FUNCTION inicio()
#i---------------

    LET HOY   = TODAY
    LET HORA  = TIME

    LET captura_cambio    = 0
    LET captura_no_cambio = 1
    LET cero              = 0
    LET uno               = 1

    SELECT *, USER
    INTO   g_paramgrales.*, g_usuario
    FROM   seg_modulo
    WHERE  modulo_cod = 'taa'

    SELECT a.*
    INTO   ruta_afi.*
    FROM   seg_modulo a
    WHERE  modulo_cod = 'afi'

    SELECT a.*
    INTO   ruta_cta.*
    FROM   seg_modulo a
    WHERE  modulo_cod = 'cta'

    SELECT a.estado
    INTO   procedente 
    FROM   safre_af:taa_cd_edo_cedente  a
    WHERE  a.descripcion = "PROCEDENTE"
    AND    a.tipo      = 2

    LET tipo_devuelta_val = 2

    SELECT a.tipo_traspaso
    INTO   individual
    FROM   safre_af:taa_cd_tipo_traspaso a
    WHERE  a.descripcion = "TRASPASO INDIVIDUAL"

    SELECT a.tipo_traspaso
    INTO   unificacion
    FROM   safre_af:taa_cd_tipo_traspaso a
    WHERE  a.descripcion = "TRASPASO UNIFICACION"

    SELECT a.tipo_traspaso
    INTO   asig_uni
    FROM   safre_af:taa_cd_tipo_traspaso a
    WHERE  a.descripcion = "ASIG AFORE UNIFICA"

    SELECT a.tipo_traspaso
    INTO   asignacion
    FROM   safre_af:taa_cd_tipo_traspaso a
    WHERE  a.descripcion = "TRASPASO ASIGNACION"

    CREATE   TEMP     TABLE      taa_cd_paso_taab014
       (
       folio                         INTEGER,
       n_seguro                     CHAR(11),
       cve_rechazo_ant              SMALLINT,
       desc_estado_ant              CHAR(30),
       cve_rechazo_act              SMALLINT,
       desc_estado_act              CHAR(30),
       hora                         CHAR(08)
       )

END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------

    OPEN WINDOW ventana_1 AT 3,2 WITH FORM "TCAAB0141" ATTRIBUTE( BORDER)

    DISPLAY " TCAAB014         CONSULTA DE SOLICITUDES DE TRASPASO AF-AF               " AT 3,1 ATTRIBUTE(REVERSE)

    DISPLAY HOY USING "DD-MM-YYYY" AT 3,69 ATTRIBUTE(REVERSE)

    DISPLAY "                 NSS       ACCIONES   T.T       ESTADO                   " AT 5,1 ATTRIBUTE(REVERSE)

    DISPLAY "FP: ", fecha_presentacion AT  5,65 ATTRIBUTE(REVERSE)

    MENU " TRASPASOS CEDIDOS "
        COMMAND "Consulta" " Consulta Traspasos"
            CALL Consulta()
        COMMAND "Salir " " Salir de Programa"
            EXIT MENU
    END MENU
    CLOSE WINDOW ventana_1
END FUNCTION

FUNCTION Consulta()
#C-----------------

    DEFINE      g_manda     RECORD
 	             n_seguro          CHAR(011) ,
	             estado            SMALLINT  ,
	             desc_estado       CHAR(030)
	 END RECORD
    DEFINE      v_n_unico         CHAR(018)
    DEFINE      var                          ,
                sali                  SMALLINT


    INITIALIZE g_manda.* TO NULL


    DISPLAY "                 NSS       ACCIONES   T.T       ESTADO                   " AT 5,1 ATTRIBUTE(REVERSE)

    DISPLAY "FP: ", fecha_presentacion AT  5,65 ATTRIBUTE(REVERSE)

    LET int_flag = FALSE
    LET sali = 0
    CONSTRUCT cla_where ON a.n_seguro,
                           a.estado
                      FROM n_seguro,
                           estado

    ON KEY(ESC)
       LET int_flag = FALSE
       EXIT CONSTRUCT
  
    ON KEY(INTERRUPT)
       LET int_flag = FALSE
       ERROR "BUSQUEDA CANCELADA..."
       SLEEP 2
       ERROR ""
       CLEAR FORM
       LET sali = 1
       EXIT CONSTRUCT

    END CONSTRUCT
    IF sali = 1 THEN
       RETURN
    END IF

    LET sel_where = " SELECT a.n_seguro,",
                           " SUM(b.monto_en_acciones),",
                           " a.tipo_traspaso,",
                           " a.estado,a.n_unico",
                    " FROM safre_af:taa_cd_det_cedido a,",
                    " OUTER (dis_cuenta b) ",
                    " WHERE a.folio = ",v_folio ,
                    " AND ", cla_where CLIPPED,
                    " AND a.n_seguro = b.nss",
                    " AND b.subcuenta  BETWEEN 0 AND 99",
                    " AND b.tipo_movimiento BETWEEN 0 AND 9999",
                    " AND a.tipo_traspaso IN (SELECT tipo_traspaso ",
                                             " FROM taa_cd_tipo_traspaso) ",
                    " AND a.estado IN (SELECT d.estado",
                    " FROM safre_af:taa_cd_edo_cedente d" , 
                    " WHERE d.tipo = ",tipo_devuelta_val, ")",
                    " GROUP BY 1,3,4,5" ,
                    " UNION ",
                    " SELECT a.n_seguro,",
                    " SUM(b.monto_en_acciones),",
                    " a.tipo_traspaso,",
                    " a.estado,a.n_unico",
                    " FROM safre_af:taa_cd_det_cedido a,",
                    " OUTER (dis_cuenta b)",
                    " WHERE a.folio = ",v_folio ,
                    " AND ", cla_where CLIPPED,
                    " AND a.n_seguro = b.nss",
                    " AND b.subcuenta  BETWEEN 0 AND 99",
                    " AND b.tipo_movimiento BETWEEN 0 AND 9999",
                    " AND a.tipo_traspaso IN (SELECT c.tipo_traspaso        ",
                                            " FROM taa_cd_tipo_traspaso c ) ",
                    " GROUP BY 1,3,4,5",
                    " ORDER BY 3,2 DESC"

    LET sel_where = sel_where CLIPPED
    PREPARE qry_consul FROM sel_where 

    DECLARE cursor_c CURSOR FOR qry_consul

    LET pos = 1

    FOREACH cursor_c INTO reg[pos].n_seguro THRU reg[pos].estado,
            r_unico[pos].n_unico

        IF reg[pos].monto IS NULL THEN  
           LET reg[pos].monto = 0
        END IF

        SELECT trt.descripcion
        INTO   reg[pos].desc_estado
        FROM   taa_cd_edo_cedente trt
        WHERE  trt.estado = reg[pos].estado
        #AND    trt.tipo     = tipo_devuelta_val
        AND    trt.tipo     IN (1,2)

        LET    reg[pos].consec = pos

        LET pos = pos + 1
    END FOREACH

    IF (pos-1) >= 1 THEN
        DISPLAY " <Enter>Saldos                                                                 " AT 4,1 ##ATTRIBUTE(REVERSE)

     LET tot_01 = pos - 1
     DISPLAY BY NAME tot_01
     DISPLAY "Registros" AT 19,09

     CALL SET_COUNT(pos-1)
     DISPLAY ARRAY reg TO scr_1.*
        ON KEY (ESC)
           LET arr = ARR_CURR()
	     ON KEY ( F1 )
                LET arr = ARR_CURR()
                LET src = SCR_LINE()

                INITIALIZE ejecuta TO NULL

                LET ejecuta = "cd ",ruta_afi.ruta_exp CLIPPED,
                              "; fglgo AFIM011 ",
                              reg[arr].n_seguro,
                              " 0, 0, C, 0 "
		          RUN ejecuta

	     ON KEY ( F2 )
                LET arr = ARR_CURR()
                LET src = SCR_LINE()

                INITIALIZE ejecuta TO NULL

                LET ejecuta = "cd ",ruta_cta.ruta_exp CLIPPED,
                              "; fglgo CTACL001 ",
                              reg[arr].n_seguro,
                              " C "
                RUN ejecuta

	     ON KEY ( RETURN )

                LET arr = ARR_CURR()
                LET src = SCR_LINE()
                LET g_manda.n_seguro = reg[arr].n_seguro
                LET v_n_unico        = r_unico[arr].n_unico

                CALL captura(g_manda.n_seguro,v_n_unico) 
                RETURNING var                 , 
			                 g_manda.estado      ,   
                          g_manda.desc_estado 

                IF var = captura_cambio THEN
                             INSERT  INTO  taa_cd_paso_taab014
                                    VALUES(
                                           v_folio               ,
                                           reg[arr].n_seguro     ,
                                           reg[arr].estado       ,
                                           reg[arr].desc_estado  ,
                                           g_manda.estado        ,
                                           g_manda.desc_estado   ,
                                           HORA
                                           )
		             LET reg[arr].estado      = g_manda.estado
		             LET reg[arr].desc_estado = g_manda.desc_estado

                   DISPLAY reg[arr].estado      TO scr_1[src].estado
                   DISPLAY reg[arr].desc_estado TO scr_1[src].desc_estado
                END IF 

        ON KEY(INTERRUPT)

            ERROR "BUSQUEDA TERMINADA..."
            SLEEP 2
            ERROR ""
            CLEAR FORM
            EXIT DISPLAY 
        END DISPLAY
   ELSE 
          ERROR "NO EXISTEN REGISTROS CON ESAS CONDICIONES"
          SLEEP 2
          ERROR ""
   END IF 

END FUNCTION

FUNCTION captura(n_seguro,n_unico)
#c--------------------------------

    DEFINE    l_criterio  ARRAY[10] OF RECORD
              codigo                  INTEGER ,
              desc                    CHAR(21)
    END RECORD

    DEFINE var                                ,
           cve_rechazo                        ,  
           bandera                            , 
           a_rr                       SMALLINT

    DEFINE n_seguro                  CHAR(011),
           n_unico                   CHAR(018),
           desc                      CHAR(030)

    DEFINE codigo                     INTEGER


    LET bandera   = cero     
    LET a_rr      = uno   
    LET codigo    = cero
{
    DECLARE cat_dig_cur CURSOR FOR          

        SELECT a.area_cod       , 
               a.area_desc[1,21] 
        FROM   tab_area a              
        WHERE  a.area_cod IN(SELECT w.* 
                             FROM taa_cd_autoriza_mod w)
        ORDER BY 1 ASC

        FOREACH cat_dig_cur INTO l_criterio[a_rr].*  
                LET a_rr = a_rr + 1                 
        END FOREACH                             
					       
        CALL SET_COUNT(a_rr-1)
        OPEN WINDOW ventan_11 AT 5,20 WITH FORM "TCAAB0143" ATTRIBUTE( BORDER)
        DISPLAY "<ENTER>Aceptar <Ctrl-C>Cancelar             " 
	     AT 1,1 ATTRIBUTE(REVERSE)
        DISPLAY "             SELECCIONAR AREA               " 
	     AT 2,1 ATTRIBUTE(REVERSE)
	 
        DISPLAY ARRAY l_criterio TO l_scr.*

          ON KEY(RETURN)
             LET a_rr    =  ARR_CURR()
             LET bandera = cero
             LET codigo  = l_criterio[a_rr].codigo

             SELECT UNIQUE "X" 
             FROM   seg_usuario a
             WHERE  a.usuario_cod = g_usuario
             AND    a.area_cod    = codigo

             IF STATUS = NOTFOUND THEN
                ERROR "Usuario no AUTORIZADO"
                LET bandera = 1
             END IF

             EXIT DISPLAY
          ON KEY(ESC)
             LET a_rr       = ARR_CURR() 
          ON KEY(CONTROL-C)
             LET bandera = uno
             EXIT DISPLAY

          ON KEY(INTERRUPT)
             LET bandera = uno
             EXIT DISPLAY
        END DISPLAY

        CLOSE WINDOW ventan_11
}
   IF bandera = cero  THEN

		  CALL codigo_01(n_seguro,n_unico) 
		  RETURNING var         ,      
                  cve_rechazo , 
                  desc 

        RETURN    var         , 
                  cve_rechazo ,  
                  desc

   ELSE

        LET var         = captura_no_cambio  
        LET cve_rechazo = ""  
        LET desc        = ""

        RETURN    var         , 
                  cve_rechazo , 
                  desc

   END IF

END FUNCTION

FUNCTION codigo_01(c01_n_seguro,c01_n_unico)
#c01-----------------------------------------

   DEFINE c01_n_seguro    CHAR(011),
          c01_n_unico     CHAR(018),
          desc            CHAR(030),
          nombre          CHAR(040)

   DEFINE var                      , 
          sw                       , 
          estado                   , 
          cve_rechazo       SMALLINT

   DEFINE rcv                      , 
          vol                      ,
          sar                      ,  
          v97                      ,
          v92           DECIMAL(15,2)

   DEFINE afi     RECORD
          paterno                  ,
          materno                  , 
          nombres          CHAR(040)
   END RECORD

   INITIALIZE estado, cve_rechazo, desc, afi.*, nombre TO NULL

        LET rcv = cero    
        LET vol = cero   
        LET sw  = cero
        LET sar = cero    
        LET v97 = cero  
        LET v92 = cero    
        LET var = captura_cambio

   OPEN WINDOW ventana_2 AT 5,2 WITH FORM "TCAAB0142" ATTRIBUTE( BORDER)

   --DISPLAY " < ESC > Aceptar                                     ",
   DISPLAY "                                                     ",
 	        "< Ctrl-C > Regresar      " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY "                 CONSULTA DE SALDOS DE LA CUENTA A CEDER                       " AT 3,1 ATTRIBUTE (REVERSE)


   INPUT BY NAME cve_rechazo  WITHOUT DEFAULTS

	     BEFORE FIELD cve_rechazo

          IF sw = 0 THEN

		       SELECT f.paterno, 
                    f.materno, 
                    f.nombres 
             INTO   afi.* 
		       FROM   afi_mae_afiliado f
		       WHERE  f.n_seguro = c01_n_seguro
   
             LET nombre = afi.paterno CLIPPED, " ",
                          afi.materno CLIPPED, " ",
                          afi.nombres CLIPPED
		   
             CALL trae_acciones(c01_n_seguro,"rcv")   RETURNING rcv
             CALL trae_acciones(c01_n_seguro,"vol")   RETURNING vol
             CALL trae_acciones(c01_n_seguro,"sar")   RETURNING sar
             CALL trae_acciones(c01_n_seguro,"viv92") RETURNING v92 
             CALL trae_acciones(c01_n_seguro,"viv97") RETURNING v97
 
		       DISPLAY c01_n_seguro TO n_seguro 
		       DISPLAY BY NAME nombre
		       DISPLAY BY NAME rcv   
		       DISPLAY BY NAME vol  
		       DISPLAY BY NAME sar 
		       DISPLAY BY NAME v97 
		       DISPLAY BY NAME v92 
             ERROR ""
             LET sw = 1 
        END IF 
  { 
	     AFTER FIELD cve_rechazo
           IF cve_rechazo IS NULL THEN

              CALL despliega_rch_taa() 
              RETURNING cve_rechazo ,
                        desc

              IF cve_rechazo IS NULL THEN
                 NEXT FIELD cve_rechazo
              END IF

              DISPLAY BY NAME desc

           ELSE
               SELECT a.descripcion
               INTO   desc
               FROM   taa_cd_edo_cedente a
               WHERE  a.estado = cve_rechazo
               AND    a.tipo = tipo_devuelta_val

               IF STATUS = NOTFOUND THEN
                  ERROR "TIPO DE VALIDACION INVALIDO..."
                  NEXT FIELD cve_rechazo
               END IF

               DISPLAY BY NAME desc

           END IF

        ON KEY(ACCEPT)
              IF cve_rechazo IS NULL THEN
                  ERROR "TIPO DE VALIDACION INVALIDO..."
                 SLEEP 3
                 NEXT FIELD cve_rechazo
              END IF

               SELECT a.descripcion
               INTO   desc
               FROM   taa_cd_edo_cedente a
               WHERE  a.estado = cve_rechazo
               AND    a.tipo = tipo_devuelta_val

               IF STATUS = NOTFOUND THEN
                  ERROR "TIPO DE VALIDACION INVALIDO..."
                  NEXT FIELD cve_rechazo
               END IF

            IF cve_rechazo = procedente THEN

            UPDATE safre_af:taa_cd_det_cedido
            SET    safre_af:taa_cd_det_cedido.estado    = procedente
            WHERE  safre_af:taa_cd_det_cedido.folio     = v_folio
            AND    safre_af:taa_cd_det_cedido.n_seguro  = c01_n_seguro

            ELSE 

            UPDATE safre_af:taa_cd_det_cedido
            SET    safre_af:taa_cd_det_cedido.estado    = cve_rechazo
            WHERE  safre_af:taa_cd_det_cedido.folio     = v_folio
            AND    safre_af:taa_cd_det_cedido.n_seguro  = c01_n_seguro
  
            END IF

            LET HORA = TIME

            INSERT INTO taa_cd_ced_valida
            VALUES(v_folio             ,
                   c01_n_seguro        ,
                   c01_n_unico         ,
                   HOY                 ,
                   HORA                ,
                   fecha_presentacion  ,
                   0                   ,
                   0                   ,
                   0                   ,
                   0                   ,
                   cve_rechazo         ,
                   g_usuario           )

            IF STATUS = NOTFOUND THEN
              PROMPT "NO PUDO HACER EL CAMBIO DE LA CAPTURA, <ENTER> " FOR enter
              LET var = captura_no_cambio
              EXIT INPUT
            ELSE
              PROMPT "CAPTURA ACEPTADA...<ENTER> PARA CONTINUAR" FOR enter
              LET var = captura_cambio
              EXIT INPUT
            END IF
}   
	     ON KEY (CONTROL-C)
            INITIALIZE estado, cve_rechazo, desc TO NULL
            PROMPT "<ENTER> PARA SALIR" FOR enter
            LET var = captura_no_cambio
            EXIT INPUT
   
	     ON KEY (INTERRUPT)
            INITIALIZE estado, cve_rechazo, desc TO NULL
            PROMPT "<ENTER> PARA SALIR" FOR enter
            LET var = captura_no_cambio
            EXIT INPUT
   
       END INPUT
       CLOSE WINDOW ventana_2
   
       RETURN var          , 
              cve_rechazo  ,   
              desc
END FUNCTION


FUNCTION trae_acciones(vnseguro,vtipo)
#ta----------------------------------

     DEFINE   vnseguro      CHAR(011),
              vtipo         CHAR(010)

     DEFINE   vacciones     DECIMAL(15,2)

     LET      vacciones = cero

     SELECT SUM(a.monto_en_acciones)
     INTO   vacciones 
	  FROM   dis_cuenta a
	  WHERE  a.nss       = vnseguro
	  AND    a.subcuenta IN(SELECT w.subcuenta
                           FROM safre_af:taa_cd_agrupa_subcta w
                           WHERE w.tipo = vtipo)
          AND    a.tipo_movimiento  BETWEEN  0  AND  999

     IF vacciones IS NULL THEN
        LET vacciones = cero
     END IF 

     RETURN vacciones

END FUNCTION

FUNCTION despliega_rch_taa()
#dtci-----------------------

    DEFINE l_reg ARRAY[1000] OF RECORD
        estado               LIKE safre_af:taa_cd_edo_cedente.estado   ,
        descripcion              LIKE safre_af:taa_cd_edo_cedente.descripcion
    END RECORD

    DEFINE
        x_x               CHAR(100) ,
        x_buscar          CHAR(030)

    DEFINE
        pos                          ,
        bandera                      ,
        aux_val               SMALLINT

    OPEN WINDOW taab0144  AT 05,12 WITH FORM "TCAAB0144" ATTRIBUTE(BORDER)
    DISPLAY "                    TIPOS DE VALIDACIONES                "
            AT 2,1 ATTRIBUTE(REVERSE)

    LET x_buscar = "*"

    WHILE TRUE
        LET x_x = " SELECT a.estado,a.descripcion FROM taa_cd_edo_cedente a ",
                  " WHERE  a.tipo = 2 ",
                  " ORDER BY 1 " CLIPPED

        PREPARE pre_4 FROM x_x

        DECLARE cur_4 CURSOR FOR pre_4

        LET pos = 1

        FOREACH cur_4 INTO l_reg[pos].*
           LET pos = pos + 1
           IF  pos >= 1000 THEN
              ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
              EXIT FOREACH
           END IF
        END FOREACH

        IF (pos-1) < 1 THEN
            ERROR "TABLA TIPO DE VALIDACIONES VACIA ..."
        END IF

        CALL SET_COUNT(pos-1)

        DISPLAY ARRAY l_reg TO scr_1.*

         ON KEY ( INTERRUPT )
            LET pos = 0
            EXIT DISPLAY

         ON KEY ( RETURN )
            LET pos = ARR_CURR()
            EXIT DISPLAY
         ON KEY ( CONTROL-M )
            LET pos = ARR_CURR()
        END DISPLAY
           EXIT WHILE
    END WHILE

    CLOSE WINDOW taab0144

    IF pos <> 0 THEN
    RETURN l_reg[pos].estado,l_reg[pos].descripcion
    ELSE
    RETURN "",""
    END IF

END FUNCTION

FUNCTION despliega_folios()
#-------------------------

DEFINE f_b SMALLINT
DEFINE arr_taa_cd_ctr_folio ARRAY[100] OF RECORD 
       folio                LIKE safre_af:taa_cd_ctr_folio.folio,
       fecha_presentacion   LIKE safre_af:taa_cd_ctr_folio.fecha_presentacion
END RECORD


DEFINE reg_taa_cd_ctr_folio RECORD 
       folio                LIKE safre_af:taa_cd_ctr_folio.folio,
       fecha_presentacion   LIKE safre_af:taa_cd_ctr_folio.fecha_presentacion
END RECORD

DEFINE arr_c                ,
       i             SMALLINT


LET f_b = 1
    OPEN WINDOW taab0145 AT 3,2 WITH FORM "TCAAB0145" ATTRIBUTE(BORDER)
    DISPLAY" TCAAB0145       CONSULTA DE SOLICITUDES TRASPASO AF - AF                 " AT 3,1 ATTRIBUTE(REVERSE)   
    DISPLAY "<Enter>Seleccionar <Ctrl-C>Salir                                             " AT 1,1 ATTRIBUTE(REVERSE)

    DISPLAY HOY USING"DD-MM-YYYY" AT 3,68 ATTRIBUTE(REVERSE)


     DECLARE cur_dsply_folio CURSOR FOR
      
     SELECT A.folio,A.fecha_presentacion      
     FROM   safre_af:taa_cd_ctr_folio A
     WHERE  A.estado = procedente
     ORDER BY A.folio

     LET i = 1

     FOREACH cur_dsply_folio
     INTO reg_taa_cd_ctr_folio.*

          LET  arr_taa_cd_ctr_folio[i].* =
               reg_taa_cd_ctr_folio.*

          LET i = i + 1

     END FOREACH

     IF i > 1 THEN

     CALL SET_COUNT(i-1)

     DISPLAY ARRAY arr_taa_cd_ctr_folio
                TO sr_ctr_folio.*
     ON KEY(ESC)
        LET arr_c = ARR_CURR()
     ON KEY(RETURN) 
        LET arr_c = ARR_CURR()
        LET v_folio            = arr_taa_cd_ctr_folio[arr_c].folio
        LET fecha_presentacion = arr_taa_cd_ctr_folio[arr_c].fecha_presentacion
        LET t_fecha_presentacion = arr_taa_cd_ctr_folio[arr_c].fecha_presentacion
        CALL proceso_principal()
     ON KEY(INTERRUPT)
         LET f_b = 0
        EXIT DISPLAY
     END DISPLAY

    ELSE

      ERROR"  NO EXISTEN FOLIOS PARA VALIDACION OPERATIVA..."
      PROMPT  " TECLEE  <ENTER>  Para Salir "  FOR  enter
      EXIT PROGRAM
    END IF


RETURN f_b

END FUNCTION

FUNCTION  f_100_genera_reporte()
#gr-----------------------------


    CLEAR     FORM
    DISPLAY    "            GENERANDO    REPORTE  .........         "
                AT  19,1     ATTRIBUTE(REVERSE)
    SELECT    codigo_afore,razon_social
      INTO    g_cod_afore,g_raz_social
      FROM    safre_af:tab_afore_local

    LET       g_ruta  =  g_paramgrales.ruta_listados CLIPPED ,"/",
                         g_usuario CLIPPED,".devol_ctas.",HOY USING "DDMMYY"

    START     REPORT     salida     TO  g_ruta

    DECLARE   C_rep          CURSOR   FOR

    SELECT    *
      FROM    taa_cd_paso_taab014
     ORDER    BY  1,2,7
     
    FOREACH   C_rep            INTO  taa_cd_paso_taab014.*
        SELECT    a.paterno_cedente,
                  a.materno_cedente,
                  a.nombre_cedente 
          INTO    t_paterno,
                  t_materno,
                  t_nombre
          FROM   safre_af:taa_cd_det_cedido a
         WHERE   a.n_seguro        =  taa_cd_paso_taab014.n_seguro
           AND   a.folio           =  taa_cd_paso_taab014.folio;
        OUTPUT          TO   REPORT  salida(l_reg.*)
    END FOREACH
    FINISH  REPORT    salida
END FUNCTION

REPORT salida(l_rep)
#s-----------------


    DEFINE    l_rep                         RECORD
              linea                         CHAR(01)
                                      END   RECORD,
              nombre                        CHAR(40)



    OUTPUT
        TOP      MARGIN  1
        BOTTOM   MARGIN  0
        LEFT     MARGIN  0
        RIGHT    MARGIN  0
        PAGE     LENGTH  60

    FORMAT
        PAGE HEADER

        PRINT   '\033e\033(10U\033&l1O\033&k2S\033&l12d'      
      # PRINT    COLUMN    1,"\033e\033(0U\033&k2S\033&l12d"
        PRINT    COLUMN    1,"(TCAAB014)",
                 COLUMN  150,"Pagina: ",PAGENO USING "<<<<"
        PRINT    COLUMN   01,g_cod_afore               USING  "&&&","  ",
                             g_raz_social    CLIPPED,
                 COLUMN   65,"REPORTE DE VALIDACION DE DEVOLUCIONES",
                 COLUMN  146,"fecha_proc: ",
                 COLUMN  158,TODAY USING "dd-mm-yyyy"
        PRINT    COLUMN   65,"       TRASPASOS AFORE - AFORE  "
        PRINT    COLUMN   01, "_______________________________________________________________________________________________________________________________________________________________________"

        PRINT
        PRINT    COLUMN    1,"LINEA",
                 COLUMN    7,"FOLIO",
                 COLUMN   15,"F_PRESEN",
                 COLUMN   28,"N_SEGURO",
                 COLUMN   42,"NOMBRE",
                 COLUMN   92,"E S T A D O    A N T E R I O R",
                 COLUMN  133,"E S T A D O    A C T U A L    "
        PRINT    COLUMN   01, "_______________________________________________________________________________________________________________________________________________________________________"

    ON EVERY ROW

        LET      nombre = t_paterno CLIPPED, " ",
                 t_materno CLIPPED, " ",
                 t_nombre CLIPPED
        LET       g_num_linea             =  g_num_linea      +  1
        PRINT
        PRINT  COLUMN   01,g_num_linea         USING  "#####",
               COLUMN   07,taa_cd_paso_taab014.folio  USING  "#####",
               COLUMN   15,t_fecha_presentacion,
               COLUMN   28,taa_cd_paso_taab014.n_seguro,
               COLUMN   42,nombre,
               COLUMN   91,taa_cd_paso_taab014.cve_rechazo_ant  USING "##&",
               COLUMN   95,taa_cd_paso_taab014.desc_estado_ant ,
               COLUMN  132,taa_cd_paso_taab014.cve_rechazo_act  USING "##&",
               COLUMN  136,taa_cd_paso_taab014.desc_estado_act

    ON LAST ROW
        SKIP      4    LINES
        PRINT     COLUMN    2,  "Total de registros enviados: ",
        COUNT    (*)   USING  "<<<<"

    PAGE TRAILER
        SKIP      2    LINE
        PAUSE    "Presione enter para continuar...."

END REPORT                      
