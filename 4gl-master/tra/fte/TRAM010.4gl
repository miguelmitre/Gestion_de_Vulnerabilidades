##############################################################################
#Owner             => E.F.P.
#Programa TRAM010  => MANTENEDOR DE ICEFAS                     
#Fecha creacion    => 23 ENERO DE 1998   
#By                => JESUS DAVID YANEZ MORENO
#Modificado  By    => MARCO ANTONIO GONZALEZ ROJAS
#Fecha de Mod      => 04 DE FEBRERO DEL 2005 
#Sistema           => TRA-ICE-IMSS   
##############################################################################
DATABASE safre_af
GLOBALS
define reg_fusion_icefa record like safre_tmp:tra_fusion_icefa.*
define u smallint
    DEFINE r_cod   RECORD LIKE tab_supervisor.*

    DEFINE ff smallint

    DEFINE su_estatus  like tra_status.estado

    DEFINE des_estatus like tra_status.des_estado

    DEFINE z_reg2 ARRAY[6] OF SMALLINT

      DEFINE reg  ARRAY[10000] OF RECORD
        usuario          CHAR(08)                         ,
        n_seguro         like tra_mae_icefa.n_seguro      ,
        nss              like tra_mae_icefa.nss           ,
        n_rfc            like afi_mae_afiliado.n_rfc      ,
        rfc              like tra_mae_icefa.rfc           ,
        icefa_cod        like tra_mae_icefa.icefa_cod     ,
        fecha_captura    like tra_mae_icefa.fecha_captura ,
        nro_int_cta      like tra_mae_icefa.nro_int_cta   ,
        status           like tra_mae_icefa.status        ,
        des_status       CHAR(013)                        ,
        correlativo      like tra_mae_icefa.correlativo   ,
	c1               smallint                         ,
	c2               smallint                         
      END RECORD

DEFINE z_reg RECORD
    orden_1  SMALLINT ,
    orden_2  SMALLINT ,
    orden_3  SMALLINT ,
    orden_4  SMALLINT ,
    orden_5  SMALLINT ,
    orden_6  SMALLINT 
END RECORD

define g_afore record like tab_afore.*
define g_usuario char(008)
define ba smallint
define fff smallint
define cad_nom char(100)
    DEFINE actual_habil       SMALLINT
    DEFINE x smallint
    DEFINE reg_1 RECORD #glo #reg_1
        n_folio               LIKE afi_mae_afiliado.n_folio        ,
        tipo_solicitud        LIKE afi_mae_afiliado.tipo_solicitud ,
        n_seguro              LIKE afi_mae_afiliado.n_seguro       ,
        n_rfc                 LIKE afi_mae_afiliado.n_rfc          ,
        n_unico               LIKE afi_mae_afiliado.n_unico        ,
        fentcons              LIKE afi_mae_afiliado.fentcons      ,
        paterno               LIKE afi_mae_afiliado.paterno        ,
        materno               LIKE afi_mae_afiliado.materno        ,
        nombres               LIKE afi_mae_afiliado.nombres 
    END RECORD

    DEFINE reg_2 RECORD #glo #reg_2
        n_folio_tra           LIKE tra_mae_icefa.n_folio_tra      ,
        nss                   LIKE tra_mae_icefa.nss              ,
        rfc                   LIKE tra_mae_icefa.rfc              ,
        fecha_solic_tra       LIKE tra_mae_icefa.fecha_solic_tra  ,
        fecha_captura         LIKE tra_mae_icefa.fecha_captura    ,
        fecha_comp_icefa      LIKE tra_mae_icefa.fecha_comp_icefa ,
        paterno2              LIKE tra_mae_icefa.paterno          ,
        materno2              LIKE tra_mae_icefa.materno          ,
        nombres2              LIKE tra_mae_icefa.nombres          ,
        icefa_cod             LIKE tra_mae_icefa.icefa_cod        ,
        des_icefa             CHAR(20)                       ,
        nro_int_cta           LIKE tra_mae_icefa.nro_int_cta      ,
        saldo_sar_92          LIKE tra_mae_icefa.saldo_sar_92     ,
        saldo_viv_92          LIKE tra_mae_icefa.saldo_viv_92     ,
        fuente               LIKE tra_mae_icefa.fuente,
        usuario               LIKE tra_mae_icefa.usuario,
        tipo_comp_icefa       LIKE tra_mae_icefa.tipo_comp_icefa  ,
        tipcom_desc           LIKE tab_tipo_com.tipcom_desc     ,
        fecha_genera      LIKE tra_mae_icefa.fecha_genera    ,
        fecha_proceso         LIKE tra_mae_icefa.fecha_proceso    ,
        fecha_liquidacion     DATE                           ,
        status                LIKE tra_mae_icefa.status           ,
        des_estado            CHAR(20)                       ,
        correlativo           LIKE tra_mae_icefa.correlativo      ,
        diag_proceso          CHAR(3)                        ,
        des_diagnostico       CHAR(30)
    END RECORD

    DEFINE arr_m  ARRAY[1000]  OF RECORD #arr_m
        n_folio_tra           LIKE tra_mae_icefa.n_folio_tra      ,
        nss                   LIKE tra_mae_icefa.nss              ,
        rfc                   LIKE tra_mae_icefa.rfc              ,
        fecha_solic_tra       LIKE tra_mae_icefa.fecha_solic_tra  ,
        fecha_captura         LIKE tra_mae_icefa.fecha_captura    ,
        fecha_comp_icefa      LIKE tra_mae_icefa.fecha_comp_icefa ,
        paterno2              LIKE tra_mae_icefa.paterno          ,
        materno2              LIKE tra_mae_icefa.materno          ,
        nombres2              LIKE tra_mae_icefa.nombres          ,
        icefa_cod             LIKE tra_mae_icefa.icefa_cod        ,
        des_icefa             CHAR(20)                       ,
        nro_int_cta           LIKE tra_mae_icefa.nro_int_cta      ,
        saldo_sar_92          LIKE tra_mae_icefa.saldo_sar_92     ,
        saldo_viv_92          LIKE tra_mae_icefa.saldo_viv_92     ,
        fuente                LIKE tra_mae_icefa.fuente          ,
        usuario               LIKE tra_mae_icefa.usuario          ,
        tipo_comp_icefa       LIKE tra_mae_icefa.tipo_comp_icefa  ,
        tipcom_desc           LIKE tab_tipo_com.tipcom_desc     ,
        fecha_genera         LIKE tra_mae_icefa.fecha_genera    ,
        fecha_proceso         LIKE tra_mae_icefa.fecha_proceso    ,
        fecha_liquidacion     DATE                           ,
        estado                LIKE tra_mae_icefa.status           ,
        des_estado            CHAR(20)                       ,
        correlativo           LIKE tra_mae_icefa.correlativo      ,
        diag_proceso          CHAR(3)                        ,
        des_diagnostico       CHAR(30)
    END RECORD

    DEFINE #char
        HORA                  CHAR(008) ,
        c2_ok                 CHAR(002) ,
        x_x                   CHAR(300) ,
        ejecuta               CHAR(200) ,
        x_buscar              CHAR(100) ,
        c8_usuario            CHAR(008) ,
        enter                 CHAR(001) ,
        enter2                CHAR(001) ,
        aux_pausa             CHAR(001) ,
 vdes_icefa            CHAR(020)

    DEFINE #date
 HOY        DATE

    DEFINE #integer
        folio_seleccionado    ,
        i_n_folio             ,
        vrowid                ,
        vfolio                INTEGER

    DEFINE #smallint
        cuantos               ,
        sw_5                  ,
        sw_4                  ,
        sw_3                  ,
        sw_2                  ,
        sw_1                  ,
        sw_11                 ,
        arr_c                 ,
        scr_l                 ,
        ya_existe             ,
        folio_reg             ,
        n_seguro_reg          ,
        rfc_reg               ,
        i            SMALLINT

 
    DEFINE arr_2 ARRAY[1000] OF RECORD
        cod_error             INTEGER ,
        des_error             CHAR(60)
    END RECORD

    DEFINE #smallint
        s_nro_icefa           ,
        item_row_cnt          ,
        cont_inp              SMALLINT

    DEFINE #integer
        folio                 ,
        folio_interno         ,
        sql_stat              ,
        i_cont_reg            INTEGER

    DEFINE #serial            #correlativo para elimina
        e_correlativo         LIKE tra_mae_icefa.correlativo

END GLOBALS

MAIN
  OPTIONS PROMPT LINE LAST,
  INPUT WRAP,
  --ACCEPT KEY CONTROL-I
  COMMENT LINE LAST
  DEFER INTERRUPT

  SELECT "OK"
  FROM safre_af:tra_status a
  WHERE a.estado = 51

  IF STATUS = NOTFOUND THEN
     INSERT INTO safre_af:tra_status VALUES (51,"BAJA")
  END IF

 CALL STARTLOG("TRAM010.log")
 CALL init()            #i
 CALL proceso_principal() #pp

END MAIN

FUNCTION init()
#i-------------
    LET HOY  = TODAY
    LET HORA = TIME
LET hoy = TODAY

SELECT *, USER
INTO   g_afore.*, g_usuario
FROM   tab_afore
WHERE  marca = 1
END FUNCTION

FUNCTION inicializa()
#i-------------------
    INITIALIZE reg_1 TO NULL
    INITIALIZE arr_m TO NULL
 LET  x = 0
    CLEAR FORM
END FUNCTION

FUNCTION inicializa_reg_1()
#ir------------------------
    INITIALIZE reg_1.* TO NULL
    DISPLAY BY NAME reg_1.*
END FUNCTION


FUNCTION despliega_icefas()
#di------------------------
 DEFINE aux_val  SMALLINT
 DEFINE l_reg ARRAY[1000] OF RECORD
        codigo  INTEGER,
        descripcion CHAR(50)
 END RECORD
 DEFINE x_x  char(100),
 x_buscar  char(30)
 DEFINE pos  SMALLINT

 OPEN WINDOW vent_1 AT 05,12 WITH FORM "PANTALLAP1" ATTRIBUTE(BORDER)
 DISPLAY "            I C E F A S                                  " AT 2,1 ATTRIBUTE(REVERSE)
 INPUT BY NAME x_buscar
            BEFORE FIELD x_buscar
                LET x_buscar = "*"

     AFTER FIELD x_buscar
         IF x_buscar IS NULL THEN
             ERROR "Descripcion a Buscar NO puede ser nulo"
             NEXT FIELD x_buscar
         ELSE
             EXIT INPUT
         END IF
 END INPUT

 WHILE TRUE
       LET x_x = " SELECT * FROM tab_icefa ",
                 " WHERE icefa_desc MATCHES ",'"',x_buscar CLIPPED,'"',
                 " ORDER BY 1 " CLIPPED
       PREPARE curg19 FROM x_x
       DECLARE cur_g19 CURSOR FOR curg19
       LET pos = 1
       FOREACH cur_g19 INTO l_reg[pos].*
        LET pos = pos + 1
        IF pos >= 1000 THEN
    ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
    EXIT FOREACH
        END IF
       END FOREACH
       IF (pos-1) < 1 THEN
          ERROR "ARCHIVO ICEFAS... VACIO"
       END IF
       CALL SET_COUNT(pos-1)
       DISPLAY ARRAY l_reg TO scr_1.*
               ON KEY ( INTERRUPT )
           LET pos = 0
           EXIT DISPLAY
        ON KEY ( CONTROL-M )
           LET pos = ARR_CURR()
           EXIT DISPLAY
       END DISPLAY
       IF pos <> 0 THEN
   EXIT WHILE
       END IF
 END WHILE
 CLOSE WINDOW vent_1
 RETURN l_reg[pos].codigo,l_reg[pos].descripcion
END FUNCTION

FUNCTION despliega_tipo_comp_icefas()
#dtci--------------------------------
    DEFINE l_reg ARRAY[1000] OF RECORD
        codigo                INTEGER ,
        descripcion       CHAR(50)
    END RECORD

    DEFINE
        x_x        CHAR(100) ,
        x_buscar       CHAR(030)

    DEFINE
        pos                   ,
        aux_val               SMALLINT

    OPEN WINDOW pantallap1 AT 05,12 WITH FORM "PANTALLAP1" ATTRIBUTE(BORDER)
    DISPLAY "              TIPOS  DE COMPROBANTES (ICEFAS)            "
            AT 2,1 ATTRIBUTE(REVERSE)

    INPUT BY NAME x_buscar
        BEFORE FIELD x_buscar
            LET x_buscar = "*"

        AFTER FIELD x_buscar
     IF x_buscar IS NULL THEN
         ERROR "DESCRIPCION A BUSCAR NO PUEDE SER NULA"
         NEXT FIELD x_buscar
     ELSE
         EXIT INPUT
     END IF
 END INPUT

 WHILE TRUE
     LET x_x = " SELECT * FROM tab_tipo_com ",
               " WHERE  tipcom_desc MATCHES ",'"',x_buscar CLIPPED,'"',
               " ORDER BY 1 " CLIPPED

     PREPARE pre_4 FROM x_x
     DECLARE cur_4 CURSOR FOR pre_4

     LET pos = 1
     FOREACH cur_4 INTO l_reg[pos].*
         LET pos = pos + 1
  IF pos >= 1000 THEN
      ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
      EXIT FOREACH
  END IF
     END FOREACH

     IF (pos-1) < 1 THEN
         ERROR "TABLA TIPO COMPROBANTES ICEFAS... VACIO"
     END IF

     CALL SET_COUNT(pos-1)
     DISPLAY ARRAY l_reg TO scr_1.*
         ON KEY ( INTERRUPT )
      LET pos = 0
      EXIT DISPLAY
  ON KEY ( CONTROL-M )
      LET pos = ARR_CURR()
      EXIT DISPLAY
     END DISPLAY

     IF pos <> 0 THEN
         EXIT WHILE
     END IF
 END WHILE
 CLOSE WINDOW pantallap1
 RETURN l_reg[pos].codigo,l_reg[pos].descripcion
END FUNCTION

FUNCTION seleccion(reg_s,v_oper)
#s-------------------------------
    DEFINE v_oper CHAR(001)
define act smallint
    DEFINE reg_5 RECORD
        nss                   LIKE tra_mae_icefa.nss         ,
        rfc                   LIKE tra_mae_icefa.rfc         ,
        icefa_cod             LIKE tra_mae_icefa.icefa_cod   ,
        nro_int_cta           LIKE tra_mae_icefa.nro_int_cta
    END RECORD

      DEFINE reg_s   RECORD
        usuario          CHAR(08)                         ,
        n_seguro         like tra_mae_icefa.n_seguro      ,
        nss              like tra_mae_icefa.nss           ,
        n_rfc            like afi_mae_afiliado.n_rfc      ,
        rfc              like tra_mae_icefa.rfc           ,
        icefa_cod        like tra_mae_icefa.icefa_cod     ,
        fecha_captura    like tra_mae_icefa.fecha_captura ,
        nro_int_cta      like tra_mae_icefa.nro_int_cta   ,
        status           like tra_mae_icefa.status        ,
        des_status       CHAR(013)                        ,
        correlativo      like tra_mae_icefa.correlativo,
	c1 smallint,
	c2 smallint
      END RECORD

    DEFINE #loc #date
        d_fecha_genera        DATE

    DEFINE #loc #char
        c_des_estado          CHAR(35) ,
        c11_n_seguro          CHAR(11)

    DEFINE
        s_lote_genera         ,
        i                     ,
        vcont                 SMALLINT
LET act = 0

    OPEN WINDOW tram0015 AT 2,2 WITH FORM "TRAM0106" ATTRIBUTE( BORDER)
    DISPLAY " TRAM010             DATOS DEL REGISTRO DE AFILIACION                          " AT 3,1 ATTRIBUTE(REVERSE) 
    DISPLAY HOY USING "dd-mm-yyyy" AT 3,65 ATTRIBUTE(REVERSE)
    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY " [ Ctrl-c ] Salir                                          " AT 1,1 ATTRIBUTE(BOLD)
    DISPLAY "                        DATOS DEL REGISTRO DE ICEFAS                             " AT 8,1 ATTRIBUTE(REVERSE)
    IF v_oper = "C" THEN 
    DISPLAY " [ Esc ] Confirma      [ Ctrl-c ] Salir                                          " AT 1,1 ATTRIBUTE(BOLD)
    ELSE 
    DISPLAY " [ Esc ] Confirma [Ctrl-p] Eliminar [ Ctrl-c ] Salir                                " AT 1,1 ATTRIBUTE(BOLD)
    END IF
    DISPLAY " MODIFICA " AT 1,65 ATTRIBUTE(REVERSE,BOLD)

    LET sw_1 = 0
    
    DISPLAY "                       DATOS DEL REGISTRO DE ICEFAS                          " AT 8,1 ATTRIBUTE(REVERSE)

LET reg_1.n_seguro = reg_s.n_seguro

            SELECT n_folio        ,
                   tipo_solicitud ,
                   n_seguro       ,
                   n_rfc          ,
                   n_unico        ,
                   fentcons       ,
                   paterno        ,
                   materno        ,
                   nombres
            INTO   reg_1.*
            FROM   afi_mae_afiliado
            WHERE  n_seguro = reg_1.n_seguro

            IF STATUS = NOTFOUND THEN
                ERROR " NSS INEXISTENTE "
            END IF
            DISPLAY BY NAME reg_1.n_folio THRU reg_1.nombres

    SELECT A.n_seguro         ,
           A.n_folio_tra      ,
           A.nss              ,
           A.rfc              ,
           A.fecha_solic_tra  ,
           A.fecha_captura    ,
           A.fecha_comp_icefa ,
           A.paterno          ,
           A.materno          ,
           A.nombres          ,
           A.icefa_cod        ,
           B.icefa_desc       ,
           A.nro_int_cta      ,
           A.saldo_sar_92     ,
           A.saldo_viv_92     ,
           A.fuente           ,
           A.usuario          ,
           A.tipo_comp_icefa  ,
           " "                ,#tipcom_desc
           A.fecha_genera     ,
           A.fecha_proceso    ,
           " "                 ,#fecha_liquidacion
           A.status           ,#estado
           " "                 ,#des_estado
           A.correlativo      ,
           " "                 ,#diag_proceso
           " "                 ,#des_diag_proceso
           A.fecha_genera     ,
           A.lote_genera
    INTO c11_n_seguro   ,
         arr_m[1].*     ,
         d_fecha_genera ,
         s_lote_genera
    FROM   tra_mae_icefa A, OUTER tab_icefa B
    WHERE  A.correlativo    = reg_s.correlativo
    AND    A.icefa_cod      = B.icefa_cod
    ORDER BY A.correlativo

    LET    cuantos = 1      

    DISPLAY " Nro.Icefas ",cuantos AT 8,1 ATTRIBUTE(REVERSE) 
    LET i = 1

        SELECT tipcom_desc
        INTO   arr_m[i].tipcom_desc
        FROM   tab_tipo_com
        WHERE  tipcom_cod = arr_m[i].tipo_comp_icefa

        SELECT des_estado
        INTO   arr_m[i].des_estado
        FROM   tra_status
        WHERE  estado = arr_m[i].estado 

        IF arr_m[i].estado = "3" THEN
            CALL codigo_rechazo(arr_m[i].nss         ,
                                arr_m[i].rfc         ,
                                arr_m[i].icefa_cod   ,
                                arr_m[i].nro_int_cta ,
                                d_fecha_genera       ,
                                s_lote_genera
                               ) #cr
            RETURNING arr_m[i].diag_proceso


            SELECT A.des_error 
            INTO   arr_m[i].des_diagnostico
            FROM   tab_rch_icefa  A
            WHERE  A.cod_error = arr_m[i].diag_proceso
        END IF


        IF arr_m[i].estado = "5" THEN
        LET arr_m[i].des_diagnostico = null
            CALL codigo_devol(arr_m[i].nss         ,
                              arr_m[i].rfc         ,
                              arr_m[i].icefa_cod   ,
                              arr_m[i].nro_int_cta ,
                              d_fecha_genera       ,
                              s_lote_genera
                             ) #cd
            RETURNING arr_m[i].diag_proceso

            SELECT A.des_devol
            INTO   arr_m[i].des_diagnostico
            FROM   tab_devolucion A
            WHERE  A.cod_devol = arr_m[i].diag_proceso
        END IF

        LET i = i + 1

    LET sw_3 = 0
    DISPLAY " Nro.Icefas ",cuantos AT 8,1 ATTRIBUTE(REVERSE)
    INPUT BY NAME reg_2.n_folio_tra      ,
                  reg_2.nss              ,
                  reg_2.rfc              ,
                  reg_2.fecha_solic_tra  ,
                  reg_2.fecha_captura    ,
                  reg_2.paterno2         ,
                  reg_2.materno2         ,
                  reg_2.nombres2         ,
                  reg_2.icefa_cod        ,
                  reg_2.des_icefa        ,
                  reg_2.nro_int_cta      ,
                  reg_2.tipo_comp_icefa   WITHOUT DEFAULTS

        BEFORE FIELD n_folio_tra
            LET arr_c = 1 #ARR_CURR()
           # LET scr_l = SCR_LINE()

            IF sw_3 = 0 THEN
                LET sw_3                   = 1
                LET reg_2.n_folio_tra      = arr_m[arr_c].n_folio_tra
                LET reg_2.nss              = arr_m[arr_c].nss
                LET reg_2.rfc              = arr_m[arr_c].rfc
                LET reg_2.fecha_solic_tra  = arr_m[arr_c].fecha_solic_tra
                LET reg_2.fecha_captura    = arr_m[arr_c].fecha_captura
                LET reg_2.fecha_comp_icefa = arr_m[arr_c].fecha_comp_icefa
                LET reg_2.paterno2         = arr_m[arr_c].paterno2
                LET reg_2.materno2         = arr_m[arr_c].materno2
                LET reg_2.nombres2         = arr_m[arr_c].nombres2
                LET reg_2.icefa_cod        = arr_m[arr_c].icefa_cod
                LET reg_2.des_icefa        = arr_m[arr_c].des_icefa
                LET reg_2.nro_int_cta      = arr_m[arr_c].nro_int_cta
                LET reg_2.saldo_sar_92     = arr_m[arr_c].saldo_sar_92
                LET reg_2.saldo_viv_92     = arr_m[arr_c].saldo_viv_92
                LET reg_2.fuente           = arr_m[arr_c].fuente
                LET reg_2.usuario          = arr_m[arr_c].usuario
                LET reg_2.tipo_comp_icefa  = arr_m[arr_c].tipo_comp_icefa
                LET reg_2.tipcom_desc  = arr_m[arr_c].tipcom_desc
                LET reg_2.fecha_genera     = arr_m[arr_c].fecha_genera
                LET reg_2.fecha_proceso    = arr_m[arr_c].fecha_proceso
                LET reg_2.fecha_liquidacion= arr_m[arr_c].fecha_liquidacion
                LET reg_2.status           = arr_m[arr_c].estado
                LET reg_2.des_estado       = arr_m[arr_c].des_estado
                LET reg_2.correlativo      = arr_m[arr_c].correlativo
                LET reg_2.diag_proceso     = arr_m[arr_c].diag_proceso
                LET reg_2.des_diagnostico  = arr_m[arr_c].des_diagnostico
                LET reg_5.nss              = arr_m[arr_c].nss
                LET reg_5.rfc              = arr_m[arr_c].rfc
                LET reg_5.icefa_cod        = arr_m[arr_c].icefa_cod
                LET reg_5.nro_int_cta      = arr_m[arr_c].nro_int_cta
            END IF

            DISPLAY BY NAME reg_1.* 
            DISPLAY reg_2.n_folio_tra      TO n_folio_tra
            DISPLAY reg_2.nss              TO nss
            DISPLAY reg_2.rfc              TO rfc
            DISPLAY reg_2.fecha_solic_tra  TO fecha_solic_tra
            DISPLAY reg_2.fecha_captura    TO fecha_captura
            DISPLAY reg_2.fecha_comp_icefa TO fecha_comp_icefa
            DISPLAY reg_2.paterno2         TO paterno2
            DISPLAY reg_2.materno2         TO materno2
            DISPLAY reg_2.nombres2         TO nombres2
            DISPLAY reg_2.icefa_cod        TO icefa_cod
            DISPLAY reg_2.des_icefa        TO des_icefa
            DISPLAY reg_2.nro_int_cta      TO nro_int_cta
            DISPLAY reg_2.saldo_sar_92     TO saldo_sar_92
            DISPLAY reg_2.saldo_viv_92     TO saldo_viv_92
            DISPLAY reg_2.fuente           TO fuente 
            DISPLAY reg_2.usuario          TO usuario
            DISPLAY reg_2.tipo_comp_icefa  TO tipo_comp_icefa
            DISPLAY reg_2.tipcom_desc  TO tipcom_desc
            DISPLAY reg_2.fecha_genera     TO fecha_genera
            DISPLAY reg_2.fecha_proceso    TO fecha_proceso 
            DISPLAY reg_2.fecha_liquidacion TO fecha_liquidacion
            DISPLAY reg_2.status           TO status
            DISPLAY reg_2.des_estado       TO des_estado
            DISPLAY reg_2.correlativo      TO correlativo   
            DISPLAY reg_2.diag_proceso     TO diag_proceso
            DISPLAY reg_2.des_diagnostico  TO des_diagnostico

        AFTER FIELD paterno2
            IF reg_2.paterno2 IS NULL THEN
                ERROR " CAMPO NO PUEDE SER NULO "
                NEXT FIELD paterno2
            END IF

        AFTER FIELD nombres2
            IF reg_2.nombres2 IS NULL THEN
                ERROR " CAMPO NO PUEDE SER NULO "
                NEXT FIELD nombres2
            END IF

 AFTER FIELD icefa_cod
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD nombres2
            END IF

     IF reg_2.icefa_cod IS NULL THEN
         CALL despliega_icefas()
                RETURNING reg_2.icefa_cod ,
                          reg_2.des_icefa

         IF reg_2.icefa_cod = 0 THEN 
                    NEXT FIELD icefa_cod 
                END IF
     ELSE
         SELECT icefa_desc
                INTO   reg_2.des_icefa
  FROM   tab_icefa
  WHERE  icefa_cod = reg_2.icefa_cod

  IF STATUS = NOTFOUND THEN
           ERROR " ICEFA INEXISTENTE "
      NEXT FIELD icefa_cod
  END IF
     END IF

     DISPLAY reg_2.icefa_cod TO icefa_cod
     DISPLAY reg_2.des_icefa TO des_icefa


                SELECT a.* 
		INTO  reg_fusion_icefa.*
		FROM  safre_tmp:tra_fusion_icefa a
		WHERE a.cve_ced_cuenta_org = reg_2.icefa_cod

                IF reg_fusion_icefa.cve_ced_cuenta_act <>
		   reg_2.icefa_cod THEN
		   ERROR"ICEFA FUSIONADA...",reg_2.icefa_cod," ",
			   reg_2.des_icefa
		   SLEEP 2

                   SELECT icefa_desc
                   INTO   reg_2.des_icefa
                   FROM   tab_icefa
                   WHERE  icefa_cod = reg_fusion_icefa.cve_ced_cuenta_act

                   LET reg_2.icefa_cod = reg_fusion_icefa.cve_ced_cuenta_act
		 END IF
                DISPLAY reg_2.icefa_cod TO icefa_cod
                DISPLAY reg_2.des_icefa TO des_icefa



 AFTER FIELD nro_int_cta
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD icefa_cod
            END IF

 AFTER FIELD tipo_comp_icefa
            IF reg_2.tipo_comp_icefa IS NULL THEN
                CALL despliega_tipo_comp_icefas() #dtci
                RETURNING reg_2.tipo_comp_icefa,
                          reg_2.tipcom_desc
                DISPLAY reg_2.tipcom_desc TO tipcom_desc
            ELSE
                SELECT tipcom_desc
                INTO   reg_2.tipcom_desc
                FROM   tab_tipo_com
                WHERE  tipcom_cod = reg_2.tipo_comp_icefa

                IF STATUS = NOTFOUND THEN
                    ERROR "TIPO DE COMPROBANTE INEXISTENTE"
                    NEXT FIELD tipo_comp_icefa
                END IF
                DISPLAY reg_2.tipcom_desc TO tipcom_desc
            END IF

        ON KEY ( INTERRUPT )
          LET su_estatus = reg_2.status
          IF v_oper = "C" AND(
             su_estatus = 20 OR 
             su_estatus = 9 ) THEN
            CASE su_estatus
            WHEN 20
             LET su_estatus=20
             LET des_estatus="CAPTURADA"
            EXIT CASE
           WHEN 3      
             LET su_estatus=9
             LET des_estatus="REENVIADA"
            EXIT CASE
           OTHERWISE
            EXIT CASE
           END CASE
          ELSE
             LET su_estatus=1
             LET des_estatus="CONFIRMADA"
          END IF
          LET sw_4 = 1
          EXIT INPUT


        ON KEY (CONTROL-P)
           IF v_oper = "M" THEN
              WHILE TRUE
                PROMPT "Desea Eliminar registro ? " FOR enter
                IF enter MATCHES"[SsNn]" THEN
                   EXIT WHILE
                END IF
              END WHILE

              IF enter MATCHES "[Nn]" THEN
                 LET su_estatus=1
                 LET des_estatus="CONFIRMADA"
                 LET act = 1
                 EXIT INPUT
              ELSE
               UPDATE tra_mae_icefa
               SET    tra_mae_icefa.n_folio_tra      = reg_2.n_folio_tra     ,
                      tra_mae_icefa.nss              = reg_2.nss             ,
                      tra_mae_icefa.rfc              = reg_2.rfc             ,
                      tra_mae_icefa.fecha_solic_tra  = reg_2.fecha_solic_tra ,
                      tra_mae_icefa.fecha_captura    = reg_2.fecha_captura   ,
                      tra_mae_icefa.fecha_comp_icefa = TODAY,
                      tra_mae_icefa.paterno          = reg_2.paterno2        ,
                      tra_mae_icefa.materno          = reg_2.materno2        ,
                      tra_mae_icefa.nombres          = reg_2.nombres2        ,
                      tra_mae_icefa.icefa_cod        = reg_2.icefa_cod       ,
                      tra_mae_icefa.nro_int_cta      = reg_2.nro_int_cta     ,
                      tra_mae_icefa.tipo_comp_icefa  = reg_2.tipo_comp_icefa ,
                      tra_mae_icefa.status           = 51                     ,
                      tra_mae_icefa.usuario          = g_usuario             ,
                      tra_mae_icefa.fecha_proceso    = HOY
               WHERE  tra_mae_icefa.n_seguro         = reg_1.n_seguro
               AND    tra_mae_icefa.correlativo      = reg_2.correlativo

               LET su_estatus  = 51 
               LET des_estatus = "BAJA"
               CASE v_oper
                  WHEN "M"
                       ERROR "REGISTRO DADO DE BAJA" SLEEP 2 ERROR ""
		       EXIT CASE
                  OTHERWISE
		       EXIT CASE
                  END CASE
                  CLEAR FORM
                  EXIT INPUT
               END IF
            END IF
        ON KEY (ACCEPT)
            ERROR "VERIFICANDO INFORMACION"

            IF reg_2.nss IS NULL AND reg_2.rfc IS NULL THEN
                ERROR " NSS Y RFC AMBOS NO PUEDEN SER NULOS "
                NEXT FIELD nss
            END IF

            IF reg_2.paterno2 IS NULL THEN
                ERROR " CAMPO NO PUEDE SER NULO "
                NEXT FIELD paterno2
            END IF

            IF reg_2.nombres2 IS NULL THEN
                ERROR " CAMPO NO PUEDE SER NULO "
                NEXT FIELD nombres2
            END IF

            IF reg_2.icefa_cod IS NULL THEN
                ERROR "CAMPO NO PUEDE SER NULO "
                NEXT FIELD icefa_cod
            END IF

#ff
            IF  reg_5.nss         <> reg_2.nss
            OR  reg_5.rfc         <> reg_2.rfc
            OR  reg_5.icefa_cod   <> reg_2.icefa_cod
            OR  reg_5.nro_int_cta <> reg_2.nro_int_cta THEN

                SELECT "OK"
                FROM   tra_mae_icefa
                WHERE  n_seguro    = reg_1.n_seguro
                AND    nss         = reg_2.nss
                AND    rfc         = reg_2.rfc
                AND    icefa_cod   = reg_2.icefa_cod
                AND    nro_int_cta = reg_2.nro_int_cta 
                GROUP BY 1

                IF STATUS <> NOTFOUND THEN
                    ERROR "ICEFA DUPLICADA" ATTRIBUTE(REVERSE) SLEEP 3
                    LET enter = "N"
                    NEXT FIELD n_folio_tra
                END IF
            END IF


            LET sw_4 = 1
            LET sw_3 = 2

            WHILE TRUE
             PROMPT "Desea Confirmar la Informacion [S/N]? " FOR enter
             IF enter MATCHES"[SsNn]" THEN
                 EXIT WHILE
             END IF
           END WHILE
                IF enter MATCHES "[Nn]" THEN

                 LET su_estatus = reg_2.status
                 IF v_oper = "C" AND(
                    su_estatus = 20 OR 
                    su_estatus = 9 ) THEN
                   CASE su_estatus
                   WHEN 20
                    LET su_estatus=20
                    LET des_estatus="CAPTURADA"
                   EXIT CASE
                  WHEN 9      
                    LET su_estatus=9
                    LET des_estatus="REENVIADA"
                   EXIT CASE
                  OTHERWISE
                   EXIT CASE
                  END CASE
                 ELSE
                    LET su_estatus=1
                    LET des_estatus="CONFIRMADA"
                 END IF
                 LET act = 1
                 EXIT INPUT
              ELSE
               UPDATE tra_mae_icefa
               SET    tra_mae_icefa.n_folio_tra      = reg_2.n_folio_tra     ,
                      tra_mae_icefa.nss              = reg_2.nss             ,
                      tra_mae_icefa.rfc              = reg_2.rfc             ,
                      tra_mae_icefa.fecha_solic_tra  = reg_2.fecha_solic_tra ,
                      tra_mae_icefa.fecha_captura    = reg_2.fecha_captura   ,
                      tra_mae_icefa.fecha_comp_icefa = TODAY,
                      tra_mae_icefa.paterno          = reg_2.paterno2        ,
                      tra_mae_icefa.materno          = reg_2.materno2        ,
                      tra_mae_icefa.nombres          = reg_2.nombres2        ,
                      tra_mae_icefa.icefa_cod        = reg_2.icefa_cod       ,
                      tra_mae_icefa.nro_int_cta      = reg_2.nro_int_cta     ,
                      tra_mae_icefa.tipo_comp_icefa  = reg_2.tipo_comp_icefa ,
                      tra_mae_icefa.status           = 1                     ,
                      tra_mae_icefa.usuario          = g_usuario             ,
                      tra_mae_icefa.fecha_proceso    = HOY
               WHERE  tra_mae_icefa.n_seguro         = reg_1.n_seguro
               AND    tra_mae_icefa.correlativo      = reg_2.correlativo

               LET su_estatus = 1 
               LET des_estatus = "CONFIRMADA"
            END IF

       CASE v_oper
            WHEN "C"
                 ERROR "REGISTRO CONFIRMADO" SLEEP 2 ERROR ""
                 --CALL Inicializa()
                 LET su_estatus=1
                 LET des_estatus="CONFIRMADA"
            WHEN "M"
       IF FIELD_TOUCHED(reg_2.*) THEN
                    ERROR "REGISTRO NO MODIFICADO" SLEEP 2 ERROR ""
                 ELSE
                    ERROR "REGISTRO MODIFICADO" SLEEP 2 ERROR ""
                 END IF
       END CASE

            CLEAR FORM
            EXIT INPUT
    END INPUT
CLOSE WINDOW tram0015
RETURN reg_2.nss     ,
       reg_2.rfc     ,
       reg_2.icefa_cod,
       reg_2.fecha_captura,
       reg_2.nro_int_cta,
       su_estatus,
       des_estatus,
       act 
    CLOSE WINDOW tram0015
END FUNCTION

FUNCTION codigo_rechazo(reg_3)
#cr---------------------------
    DEFINE reg_3 RECORD #loc #reg_3
        nss                   LIKE tra_mae_icefa.nss          ,
        rfc                   LIKE tra_mae_icefa.rfc          ,
        icefa_cod             LIKE tra_mae_icefa.icefa_cod    ,
        nro_int_cta           LIKE tra_mae_icefa.nro_int_cta  ,
        fecha_genera          LIKE tra_mae_icefa.fecha_genera ,
        lote_genera           LIKE tra_mae_icefa.lote_genera
    END RECORD

    DEFINE #loc #char
        c3_diag_proceso_1     CHAR(3)

    DEFINE reg_4 RECORD #loc #reg_4
        c10_fecha_genera      CHAR(10) ,
        c08_fecha_genera      CHAR(08) ,
        lote_genera           CHAR(03)
    END RECORD
    
    LET reg_4.c10_fecha_genera = reg_3.fecha_genera
    LET reg_4.c08_fecha_genera = reg_4.c10_fecha_genera[07,10],
                                 reg_4.c10_fecha_genera[01,02],
                                 reg_4.c10_fecha_genera[04,05]
    LET reg_4.lote_genera      = reg_3.lote_genera USING"&&&"

 
    WHENEVER ERROR CONTINUE
    IF reg_3.nss <> " " THEN
        IF reg_3.rfc <> " " THEN
            IF reg_3.nro_int_cta <> " " THEN
                SELECT A.diag_proceso_1
                INTO   c3_diag_proceso_1
                FROM   tra_det_rechazo A
                WHERE  A.n_seguro_ent            = reg_3.nss
                AND    A.rfc_ent                 = reg_3.rfc
                AND    A.cve_ced_cuenta          = reg_3.icefa_cod 
                AND    A.nro_ctrl_icefa          = reg_3.nro_int_cta
                AND    A.ident_lote_solic[06,13] = reg_4.c08_fecha_genera
                AND    A.ident_lote_solic[14,16] = reg_4.lote_genera
                GROUP BY A.diag_proceso_1
                RETURN c3_diag_proceso_1
            ELSE
                SELECT A.diag_proceso_1
                INTO   c3_diag_proceso_1
                FROM   tra_det_rechazo A
                WHERE  A.n_seguro_ent            = reg_3.nss
                AND    A.rfc_ent                 = reg_3.rfc
                AND    A.cve_ced_cuenta          = reg_3.icefa_cod 
                AND    A.ident_lote_solic[06,13] = reg_4.c08_fecha_genera
                AND    A.ident_lote_solic[14,16] = reg_4.lote_genera
                GROUP BY A.diag_proceso_1
                RETURN c3_diag_proceso_1
            END IF
        ELSE
            IF reg_3.nro_int_cta <> " " THEN
                SELECT A.diag_proceso_1
                INTO   c3_diag_proceso_1
                FROM   tra_det_rechazo A
                WHERE  A.n_seguro_ent            = reg_3.nss
                AND    A.cve_ced_cuenta          = reg_3.icefa_cod 
                AND    A.nro_ctrl_icefa          = reg_3.nro_int_cta
                AND    A.ident_lote_solic[06,13] = reg_4.c08_fecha_genera
                AND    A.ident_lote_solic[14,16] = reg_4.lote_genera
                GROUP BY A.diag_proceso_1
                RETURN c3_diag_proceso_1
            ELSE
                SELECT A.diag_proceso_1
                INTO   c3_diag_proceso_1
                FROM   tra_det_rechazo A
                WHERE  A.n_seguro_ent            = reg_3.nss
                AND    A.cve_ced_cuenta          = reg_3.icefa_cod 
                AND    A.ident_lote_solic[06,13] = reg_4.c08_fecha_genera
                AND    A.ident_lote_solic[14,16] = reg_4.lote_genera
                GROUP BY A.diag_proceso_1
                RETURN c3_diag_proceso_1
            END IF
        END IF
    ELSE
        IF reg_3.rfc <> " " THEN
            IF reg_3.nro_int_cta <> " " THEN
                SELECT A.diag_proceso_1
                INTO   c3_diag_proceso_1
                FROM   tra_det_rechazo A
                WHERE  A.rfc                     = reg_3.rfc
                AND    A.cve_ced_cuenta          = reg_3.icefa_cod 
                AND    A.nro_ctrl_icefa          = reg_3.nro_int_cta
                AND    A.ident_lote_solic[06,13] = reg_4.c08_fecha_genera
                AND    A.ident_lote_solic[14,16] = reg_4.lote_genera
                GROUP BY A.diag_proceso_1
                RETURN c3_diag_proceso_1
            ELSE
                SELECT A.diag_proceso_1
                INTO   c3_diag_proceso_1
                FROM   tra_det_rechazo A
                WHERE  A.rfc                     = reg_3.rfc
                AND    A.cve_ced_cuenta          = reg_3.icefa_cod 
                AND    A.ident_lote_solic[06,13] = reg_4.c08_fecha_genera
                AND    A.ident_lote_solic[14,16] = reg_4.lote_genera
                GROUP BY A.diag_proceso_1
                RETURN c3_diag_proceso_1
            END IF
        END IF
    END IF
    WHENEVER ERROR STOP
END FUNCTION

FUNCTION codigo_devol(reg_3)
#cd-------------------------
    DEFINE reg_3 RECORD #loc #reg_3
        nss                   LIKE tra_mae_icefa.nss          ,
        rfc                   LIKE tra_mae_icefa.rfc          ,
        icefa_cod             LIKE tra_mae_icefa.icefa_cod    ,
        nro_int_cta           LIKE tra_mae_icefa.nro_int_cta  ,
        fecha_genera          LIKE tra_mae_icefa.fecha_genera ,
        lote_genera           LIKE tra_mae_icefa.lote_genera
    END RECORD

    DEFINE #loc #char
        c2_status_registro    CHAR(02)

    DEFINE reg_4 RECORD #loc #reg_4
        c10_fecha_genera      CHAR(10) ,
        c08_fecha_genera      CHAR(08) ,
        lote_genera           CHAR(03)
    END RECORD
    
    LET reg_4.c10_fecha_genera = reg_3.fecha_genera
    LET reg_4.c08_fecha_genera = reg_4.c10_fecha_genera[07,10],
                                 reg_4.c10_fecha_genera[01,02],
                                 reg_4.c10_fecha_genera[04,05]
    LET reg_4.lote_genera      = reg_3.lote_genera USING"&&&"


    IF reg_3.nss IS NOT NULL THEN
        IF reg_3.rfc IS NOT NULL THEN
            IF reg_3.nro_int_cta IS NOT NULL THEN
                SELECT A.status_registro
                INTO   c2_status_registro
                FROM   tra_det_devol A
                WHERE  A.n_seguro_ent            = reg_3.nss
                AND    A.rfc_ent                 = reg_3.rfc
                AND    A.cve_ced_cuenta          = reg_3.icefa_cod 
                AND    A.nro_ctrl_icefa          = reg_3.nro_int_cta
               AND    A.ident_lote_solic[06,13] = reg_4.c08_fecha_genera
               AND    A.ident_lote_solic[14,16] = reg_4.lote_genera
                GROUP BY A.status_registro
                RETURN c2_status_registro
            ELSE
                SELECT A.status_registro
                INTO   c2_status_registro 
                FROM   tra_det_devol A
                WHERE  A.n_seguro_ent            = reg_3.nss
                AND    A.rfc_ent                 = reg_3.rfc
                AND    A.cve_ced_cuenta          = reg_3.icefa_cod 
                AND    A.ident_lote_solic[06,13] = reg_4.c08_fecha_genera
                AND    A.ident_lote_solic[14,16] = reg_4.lote_genera
                GROUP BY A.status_registro
                RETURN c2_status_registro
            END IF
        ELSE
            IF reg_3.nro_int_cta <> " " THEN
                SELECT A.status_registro
                INTO   c2_status_registro 
                FROM   tra_det_devol A
                WHERE  A.n_seguro_ent            = reg_3.nss
                AND    A.cve_ced_cuenta          = reg_3.icefa_cod 
                AND    A.nro_ctrl_icefa          = reg_3.nro_int_cta
                AND    A.ident_lote_solic[06,13] = reg_4.c08_fecha_genera
                AND    A.ident_lote_solic[14,16] = reg_4.lote_genera
                GROUP BY A.status_registro
                RETURN c2_status_registro 
            ELSE
                SELECT A.status_registro
                INTO   c2_status_registro 
                FROM   tra_det_devol A
                WHERE  A.n_seguro_ent            = reg_3.nss
                AND    A.cve_ced_cuenta          = reg_3.icefa_cod 
                AND    A.ident_lote_solic[06,13] = reg_4.c08_fecha_genera
                AND    A.ident_lote_solic[14,16] = reg_4.lote_genera
                GROUP BY A.status_registro
                RETURN c2_status_registro 
            END IF
        END IF
    ELSE
        IF reg_3.rfc <> " " THEN
            IF reg_3.nro_int_cta <> " " THEN
                SELECT A.status_registro
                INTO   c2_status_registro 
                FROM   tra_det_devol A
                WHERE  A.rfc                     = reg_3.rfc
                AND    A.cve_ced_cuenta          = reg_3.icefa_cod 
                AND    A.nro_ctrl_icefa          = reg_3.nro_int_cta
                AND    A.ident_lote_solic[06,13] = reg_4.c08_fecha_genera
                AND    A.ident_lote_solic[14,16] = reg_4.lote_genera
                GROUP BY A.status_registro
                RETURN c2_status_registro 
            ELSE
                SELECT A.status_registro
                INTO   c2_status_registro 
                FROM   tra_det_devol A
                WHERE  A.rfc                     = reg_3.rfc
                AND    A.cve_ced_cuenta          = reg_3.icefa_cod 
                AND    A.ident_lote_solic[06,13] = reg_4.c08_fecha_genera
                AND    A.ident_lote_solic[14,16] = reg_4.lote_genera
                GROUP BY A.status_registro
                RETURN c2_status_registro 
            END IF
        END IF
    END IF

END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------

    OPEN WINDOW ventana_1 AT 2,2 WITH FORM "TRAM0101" ATTRIBUTE(BORDER)
    DISPLAY " TRAM010          CONFIRMACION SOLICITUDES ICEFA-AFORE IMSS                    " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY "                                                                               " AT 7,1 ATTRIBUTE(REVERSE)

    DISPLAY hoy USING "dd-mm-yyyy" AT 3,68 ATTRIBUTE(REVERSE)

    MENU "Confirma afiliacion"
        COMMAND "Confirma" "Confirmacion Solicitud "
                CALL elige_solicitud("C")
                CALL Inicializa()
        COMMAND "Modifica" "Modificacion Solicitud "
            CALL Ingresa_autoriza() 
            IF aux_pausa = "N" THEN
                CALL elige_solicitud("M")
                CALL Inicializa()
            END IF
        COMMAND "Salir" "Salir de Programa"
            EXIT MENU
    END MENU

END FUNCTION

FUNCTION elige_solicitud(tipo_oper)
#es-------------------------------
 define sigue smallint
         DEFINE vvnss like tra_mae_icefa.nss      ,
			       vvrfc like tra_mae_icefa.rfc      ,
			       vvicefa_cod like tra_mae_icefa.icefa_cod,
			       vvfecha_captura like tra_mae_icefa.fecha_captura,
			       vvnro_int_cta  like tra_mae_icefa.nro_int_cta,
			       vvstatus  like tra_mae_icefa.status,
			       vvdes_status  char(13)

    DEFINE tipo_oper              CHAR(1),
           pos_e                  INTEGER,
           pos_x                  INTEGER,
           vstatus_interno        SMALLINT,
           cont                   SMALLINT,
           cla_where              CHAR(300),
           sel_where              CHAR(1000),
           L                      INTEGER

    OPEN WINDOW ventana_2 AT 7,5 WITH FORM "TRAM0102" ATTRIBUTE( BORDER)

    DISPLAY "                         OPCIONES DE CONSULTA                                  " AT 2,1 ATTRIBUTE(REVERSE) 
    DISPLAY "[ Ctrl-C ] Salir" AT 1,1 --ATTRIBUTES(REVERSE)

    LET cont = 2

    IF (cont-1) >= 1 THEN     
        CALL SET_COUNT(cont-1) 

        LET int_flag = FALSE

        CONSTRUCT BY NAME cla_where ON a.usuario        ,
                                       a.n_seguro       ,
                                       a.icefa_cod      ,
                                       a.fecha_captura  ,
                                       a.fuente         ,
                                       a.status  

        IF int_flag = TRUE THEN          
            LET int_flag = FALSE          
            ERROR "BUSQUEDA CANCELADA..." 
            SLEEP 2                       
            ERROR ""                      
            CLOSE WINDOW ventana_2        
	    RETURN 
        END IF

        IF  ordena() = TRUE THEN #o
	
        CLEAR FORM
        LET z_reg2[1] = z_reg.orden_1
        LET z_reg2[2] = z_reg.orden_2
        LET z_reg2[3] = z_reg.orden_3
        LET z_reg2[4] = z_reg.orden_4
        LET z_reg2[5] = z_reg.orden_5
        LET z_reg2[6] = z_reg.orden_6


        LET sel_where = " SELECT a.usuario, a.n_seguro, a.nss, ", 
                        " b.n_rfc,a.rfc,a.icefa_cod,a.fecha_captura,",
                        " a.nro_int_cta,a.status,",
              " CASE WHEN a.status=20 THEN ","'","CAPTURADA","'",
                   " WHEN a.status=1  THEN ","'","CONFIRMADA","'",
                   " WHEN a.status=9  THEN ","'","REENVIADA","'",
              " END CASE,a.correlativo", 
              " FROM tra_mae_icefa  a, afi_mae_afiliado b ",
              "WHERE a.n_seguro = b.n_seguro "

        IF tipo_oper = "C" THEN
           LET sel_where = sel_where CLIPPED,
		           " AND a.status in (40,20,9)" 
        ELSE
           LET sel_where = sel_where CLIPPED, 
                 " AND a.status = 1 " 
        END IF
        LET sel_where = sel_where CLIPPED,
			" AND ", cla_where CLIPPED ,
                        " ORDER BY ", z_reg.orden_1,",",z_reg.orden_2,",",
                                      z_reg.orden_3,",",z_reg.orden_4,",",
                                      z_reg.orden_5,",",z_reg.orden_6 
  
        LET sel_where = sel_where CLIPPED

        PREPARE qry_consul FROM sel_where 

        DECLARE cursor_c CURSOR FOR qry_consul

        LET pos_e = 1

        FOREACH cursor_c INTO reg[pos_e].usuario,
                              reg[pos_e].n_seguro,
                              reg[pos_e].nss,
                              reg[pos_e].n_rfc,
	                     reg[pos_e].rfc,
	                     reg[pos_e].icefa_cod,
	                     reg[pos_e].fecha_captura,
	                     reg[pos_e].nro_int_cta ,
	                     reg[pos_e].status ,
	                     reg[pos_e].des_status ,
                              reg[pos_e].correlativo
              LET reg[pos_e].c1 = pos_e 
                           LET pos_e = pos_e + 1                 
        END FOREACH                          
        FOR u = 1 TO (pos_e - 1)
	    LET reg[u].c2 = (pos_e - 1)
        END FOR

        CLOSE WINDOW ventana_2              
        INITIALIZE reg[pos_e].* TO NULL   
        IF (pos_e-1) >= 1 THEN                 
            CALL  SET_COUNT(pos_e-1)            
            LET int_flag = FALSE
            DISPLAY ARRAY reg TO  scr_1.*
               ON KEY ( CONTROL-M )
                  LET pos_e = ARR_CURR()
                  LET pos_x = ARR_CURR()
                  LET L   = SCR_LINE()

                  CALL seleccion( reg[pos_e].*,tipo_oper) 
                  RETURNING 
                   vvnss,
	          vvrfc,
	          vvicefa_cod,
	          vvfecha_captura,
	          vvnro_int_cta ,
	          vvstatus ,
	          vvdes_status ,
                   sigue
              IF sigue = 0 THEN
                   LET  reg[pos_e].nss           = vvnss            
	          LET  reg[pos_e].rfc           = vvrfc            
	          LET  reg[pos_e].icefa_cod     = vvicefa_cod      
	          LET  reg[pos_e].fecha_captura = vvfecha_captura  
	          LET  reg[pos_e].nro_int_cta   = vvnro_int_cta    
	          LET  reg[pos_e].status        = vvstatus         
	          LET  reg[pos_e].des_status    = vvdes_status

                  DISPLAY vvnss TO scr_1[L].nss
                  DISPLAY vvrfc TO scr_1[L].rfc
                  DISPLAY vvicefa_cod TO scr_1[L].icefa_cod
                  DISPLAY vvfecha_captura TO scr_1[L].fecha_captura
                  DISPLAY vvnro_int_cta TO scr_1[L].nro_int_cta
                  DISPLAY vvstatus TO scr_1[L].status
                  DISPLAY vvdes_status  TO scr_1[L].des_status
               END IF
               ON KEY ( INTERRUPT)
                  LET int_flag = FALSE
                  EXIT DISPLAY
            END DISPLAY
            
        ELSE                                   
            IF tipo_oper ="C" THEN
               ERROR "ARCHIVO DE SOLICITUDES SIN CONFIRMAR... VACIO"
            ELSE
               ERROR "ARCHIVO DE SOLICITUDES CONFIRMADAS/VALIDADAS ... VACIO"
            END IF
            SLEEP 2                        
            ERROR ""                            
	    RETURN 
        END IF                                 
      ELSE
            CLOSE WINDOW ventana_2        
	    RETURN 
      END IF
   END IF

END FUNCTION

FUNCTION ordena()
#o--------------
    DEFINE   resultado   SMALLINT

    LET resultado = TRUE

    OPEN WINDOW ventana_3 AT 7,5 WITH FORM "TRAM0103" ATTRIBUTE(BORDER)
    DISPLAY " [ Esc ] Grabar      [ Ctrl-C ] Salir"                                                 AT 1,1 ATTRIBUTE(BOLD) 
    DISPLAY "                      OPCIONES DE ORDENAMIENTO                                        " AT 2,1 ATTRIBUTE(REVERSE,BOLD)

    LET z_reg.orden_1 = 1
    LET z_reg.orden_2 = 2
    LET z_reg.orden_3 = 3
    LET z_reg.orden_4 = 4
    LET z_reg.orden_5 = 5
    LET z_reg.orden_6 = 6

    DISPLAY BY NAME z_reg.*
       
    INPUT BY NAME z_reg.* WITHOUT DEFAULTS
      AFTER FIELD orden_1
         IF z_reg.orden_1 IS NULL THEN
            ERROR "Campo NO puede ser NULO"
            NEXT FIELD orden_1
         ELSE
            IF z_reg.orden_1 < 1 OR z_reg.orden_1 > 6 THEN
               ERROR "La opcion de orden digitada no existe...Reingrese"
               NEXT FIELD orden_1
            END IF
         END IF

      AFTER FIELD orden_2
         IF z_reg.orden_2 IS NULL THEN
            ERROR "Campo NO puede ser NULO"
            NEXT FIELD orden_2
         ELSE
            IF z_reg.orden_2 < 1 OR z_reg.orden_2 > 6  THEN
               ERROR "La opcion de orden digitada no existe...Reingrese"
               NEXT FIELD orden_2
            END IF
            IF z_reg.orden_2 = z_reg.orden_1 THEN
               ERROR "Opcion ya digitada ...Reintente "
               NEXT FIELD orden_2
            END IF
         END IF

      AFTER FIELD orden_3
         IF z_reg.orden_3 IS NULL THEN
            ERROR "Campo NO puede ser NULO"
            NEXT FIELD orden_3
         ELSE
            IF z_reg.orden_3 < 1 OR z_reg.orden_3 > 6 THEN
               ERROR "La opcion de orden digitada no existe...Reingrese"
               NEXT FIELD orden_3
            END IF
            IF (z_reg.orden_3 = z_reg.orden_1)
               OR (z_reg.orden_3 = z_reg.orden_2) THEN
               ERROR "Opcion ya digitada ...Reintente "
               NEXT FIELD orden_3
            END IF
         END IF

      AFTER FIELD orden_4
         IF z_reg.orden_4 IS NULL THEN
            ERROR "Campo NO puede ser NULO"
            NEXT FIELD orden_4
         ELSE
            IF z_reg.orden_4 < 1 OR z_reg.orden_4 > 6  THEN
               ERROR "La opcion de orden digitada no existe...Reingrese"
               NEXT FIELD orden_4
            END IF
            IF (z_reg.orden_4 = z_reg.orden_1)
               OR (z_reg.orden_4 = z_reg.orden_2)
               OR (z_reg.orden_4 = z_reg.orden_3) THEN
               ERROR "Opcion ya digitada ...Reintente "
               NEXT FIELD orden_4
            END IF
         END IF

      AFTER FIELD orden_5
         IF z_reg.orden_5 IS NULL THEN
            ERROR "Campo NO puede ser NULO"
            NEXT FIELD orden_5
         ELSE
            IF z_reg.orden_5 < 1 OR z_reg.orden_5 > 5  THEN
               ERROR "La opcion de orden digitada no existe...Reingrese"
               NEXT FIELD orden_5
            END IF
            IF (z_reg.orden_5 = z_reg.orden_1)
               OR (z_reg.orden_5 = z_reg.orden_2)
               OR (z_reg.orden_5 = z_reg.orden_3) 
               OR (z_reg.orden_5 = z_reg.orden_4) THEN
               ERROR "Opcion ya digitada ...Reintente "
               NEXT FIELD orden_5
            END IF
         END IF

      AFTER FIELD orden_6
         IF z_reg.orden_6 IS NULL THEN
            ERROR "Campo NO puede ser NULO"
            NEXT FIELD orden_6
         ELSE
            IF z_reg.orden_6 < 1 OR z_reg.orden_6 > 6  THEN
               ERROR "La opcion de orden digitada no existe...Reingrese"
               NEXT FIELD orden_6
            END IF
            IF (z_reg.orden_6 = z_reg.orden_1)
               OR (z_reg.orden_6 = z_reg.orden_2)
               OR (z_reg.orden_6 = z_reg.orden_3) 
               OR (z_reg.orden_6 = z_reg.orden_4) 
               OR (z_reg.orden_6 = z_reg.orden_5) THEN
               ERROR "Opcion ya digitada ...Reintente "
               NEXT FIELD orden_6
            END IF
         END IF

      ON KEY ( ESC )
         IF z_reg.orden_1 IS NULL THEN
            ERROR "Campo NO puede ser NULO"
            NEXT FIELD orden_1
         ELSE
            IF z_reg.orden_1 < 1 OR z_reg.orden_1 > 6  THEN
               ERROR "La opcion de orden digitada no existe...Reingrese"
               NEXT FIELD orden_1
            END IF
         END IF

         IF z_reg.orden_2 IS NULL THEN
            ERROR "Campo NO puede ser NULO"
            NEXT FIELD orden_2
         ELSE
            IF z_reg.orden_2 < 1 OR z_reg.orden_2 > 6  THEN
               ERROR "La opcion de orden digitada no existe...Reingrese"
               NEXT FIELD orden_2
            END IF
            IF z_reg.orden_2 = z_reg.orden_1 THEN
               ERROR "Opcion ya digitada ...Reintente "
               NEXT FIELD orden_2
            END IF
         END IF

         IF z_reg.orden_3 IS NULL THEN
            ERROR "Campo NO puede ser NULO"
            NEXT FIELD orden_3
         ELSE
            IF z_reg.orden_3 < 1 OR z_reg.orden_3 > 6  THEN
               ERROR "La opcion de orden digitada no existe...Reingrese"
               NEXT FIELD orden_3
            END IF
            IF (z_reg.orden_3 = z_reg.orden_1)
               OR (z_reg.orden_3 = z_reg.orden_2) THEN
               ERROR "Opcion ya digitada ...Reintente "
               NEXT FIELD orden_3
            END IF
         END IF

         IF z_reg.orden_4 IS NULL THEN
            ERROR "Campo NO puede ser NULO"
            NEXT FIELD orden_4
         ELSE
            IF z_reg.orden_4 < 1 OR z_reg.orden_4 > 6  THEN
               ERROR "La opcion de orden digitada no existe...Reingrese"
               NEXT FIELD orden_4
            END IF
            IF (z_reg.orden_4 = z_reg.orden_1)
               OR (z_reg.orden_4 = z_reg.orden_2)
               OR (z_reg.orden_4 = z_reg.orden_3) THEN
               ERROR "Opcion ya digitada ...Reintente "
               NEXT FIELD orden_4
            END IF
         END IF

         IF z_reg.orden_5 IS NULL THEN
            ERROR "Campo NO puede ser NULO"
            NEXT FIELD orden_5
         ELSE
            IF z_reg.orden_5 < 1 OR z_reg.orden_5 > 6  THEN
               ERROR "La opcion de orden digitada no existe...Reingrese"
               NEXT FIELD orden_5
            END IF
            IF (z_reg.orden_5 = z_reg.orden_1)
               OR (z_reg.orden_5 = z_reg.orden_2)
               OR (z_reg.orden_5 = z_reg.orden_3) 
               OR (z_reg.orden_5 = z_reg.orden_4) THEN
               ERROR "Opcion ya digitada ...Reintente "
               NEXT FIELD orden_5
            END IF
         END IF

         IF z_reg.orden_6 IS NULL THEN
            ERROR "Campo NO puede ser NULO"
            NEXT FIELD orden_6
         ELSE
            IF z_reg.orden_6 < 1 OR z_reg.orden_6 > 6  THEN
               ERROR "La opcion de orden digitada no existe...Reingrese"
               NEXT FIELD orden_6
            END IF
            IF (z_reg.orden_6 = z_reg.orden_1)
               OR (z_reg.orden_6 = z_reg.orden_2)
               OR (z_reg.orden_6 = z_reg.orden_3) 
               OR (z_reg.orden_6 = z_reg.orden_4) 
               OR (z_reg.orden_6 = z_reg.orden_5) THEN
               ERROR "Opcion ya digitada ...Reintente "
               NEXT FIELD orden_6
            END IF
         END IF

        ON KEY ( INTERRUPT )
            LET int_flag = FALSE          
            LET resultado = FALSE
            ERROR "BUSQUEDA CANCELADA..." 
            SLEEP 2                       
            ERROR ""                      
            EXIT INPUT
    END INPUT
    CLOSE WINDOW ventana_3
    RETURN resultado
END FUNCTION

FUNCTION Ingresa_autoriza()
#ia------------------------

    DEFINE cod      DECIMAL(10,0)
    DEFINE pos      SMALLINT
    DEFINE desc     CHAR(60)
    DEFINE x_buscar CHAR(60)
    DEFINE x_texto  CHAR(200)

    LET aux_pausa = "S"

    OPEN WINDOW ventanilla_super AT 8,4 WITH FORM "TRAM0104" ATTRIBUTE(BORDER)
    DISPLAY "                      OPCION RESERVADA PARA SUPERVISORES                       " AT 3,1 ATTRIBUTE ( REVERSE)

    INPUT BY NAME r_cod.super_cod,r_cod.super_desc,r_cod.nip 

    AFTER FIELD super_cod
        IF r_cod.super_cod IS NULL THEN
            CALL Despliega_supervisores() 
                 RETURNING r_cod.super_cod, r_cod.super_desc

            SELECT area_cod, super_desc 
            INTO   r_cod.area_cod, r_cod.super_desc
            FROM   tab_supervisor 
            WHERE  super_cod = r_cod.super_cod

            IF STATUS = NOTFOUND THEN
                ERROR "No existe codigo ... "
                NEXT FIELD super_cod
            END IF
        ELSE
            SELECT area_cod, super_desc 
            INTO   r_cod.area_cod, r_cod.super_desc
            FROM   tab_supervisor 
            WHERE  super_cod = r_cod.super_cod

            IF STATUS = NOTFOUND THEN
                ERROR "Clave de supervisor invalida "
                SLEEP 3
                NEXT FIELD super_cod
            END IF
        END IF

        DISPLAY BY NAME r_cod.super_cod, r_cod.super_desc

    AFTER FIELD nip
        IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
            NEXT FIELD super_cod 
        END IF

        IF r_cod.nip IS NULL THEN
            LET aux_pausa = "S"
            ERROR "El campo NIP no puede ser nulo"
            NEXT FIELD nip
        END IF

        SELECT "X" 
        FROM   tab_supervisor
        WHERE  nip = r_cod.nip
        AND    super_cod = r_cod.super_cod

        IF STATUS = NOTFOUND THEN
            ERROR "Permiso denegado, nip incorrecto"
            LET r_cod.nip = NULL
            LET aux_pausa = "S"
            NEXT FIELD nip
        ELSE 
            LET aux_pausa = "N"
            EXIT INPUT
        END IF

    ON KEY ( INTERRUPT )
        LET aux_pausa = "S"
        EXIT INPUT

    END INPUT

    CLOSE WINDOW ventanilla_super

END FUNCTION

FUNCTION Despliega_supervisores()
	DEFINE aux_val		SMALLINT
	DEFINE aux_pausa		SMALLINT
	DEFINE cod      		INTEGER
	DEFINE pat,mat,nom		CHAR(60)
	DEFINE l_reg ARRAY[1000] OF RECORD
	       codigo		INTEGER,
	       descripcion	CHAR(50)
	END RECORD
	DEFINE pos		SMALLINT
	DEFINE x_x		char(100),
	       x_buscar		char(30)

	OPEN WINDOW vent_1 AT 05,12 WITH FORM "TRAM0105" ATTRIBUTE(BORDER)
           DISPLAY "                   S U P E R V I S O R E S                " AT 2,1 ATTRIBUTE(REVERSE)
	INPUT BY NAME x_buscar
	      AFTER FIELD x_buscar
		    IF x_buscar IS NULL THEN
		       ERROR "Descripcion a Buscar NO puede ser nulo"
		       NEXT FIELD x_buscar
		    ELSE
		       EXIT INPUT
		    END IF
	END INPUT
	WHILE TRUE
	      LET x_x = " SELECT super_cod,super_desc FROM tab_supervisor ",
	                " WHERE super_desc MATCHES ",'"',x_buscar CLIPPED,'"',
	                " ORDER BY 2 " CLIPPED
	      PREPARE curg13 FROM x_x
	      DECLARE cur_g13 CURSOR FOR curg13
	      LET pos = 1
	      FOREACH cur_g13 INTO l_reg[pos].*
		      LET pos = pos + 1
		      IF pos >= 1000 THEN
			 ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
			 EXIT FOREACH
		      END IF
	      END FOREACH
	      IF (pos-1) < 1 THEN
	         ERROR "ARCHIVO SUPERVISOR..... VACIO"
	      END IF
	      CALL SET_COUNT(pos-1)
	      DISPLAY ARRAY l_reg TO scr_1.*
	              ON KEY ( INTERRUPT )
		         LET pos = 0
		         EXIT DISPLAY
		      ON KEY ( CONTROL-M )
		         LET pos = ARR_CURR()
		         EXIT DISPLAY
	      END DISPLAY
	      IF pos <> 0 THEN
		 EXIT WHILE
	      END IF
	END WHILE
	CLOSE WINDOW vent_1
	RETURN l_reg[pos].codigo,l_reg[pos].descripcion
END FUNCTION
