###############################################################################
#Proyecto          => SISTEMA DE AFORES( MEXICO )                              #
#Owner             => E.F.P.                                                   #
#Programa RETM006  => MANTENEDOR DE RECHAZOS INTERNOS                          #
#Fecha creacion    => 26 DE JULIO DE 2002                                      #
#By                => LUIS ENRIQUE AVILA GUZMAN                                #
#Fecha actualiz.   =>                                                          #
#Actualizacion     =>                                                          #
#Sistema           => RET                                                      #
################################################################################
DATABASE safre_af
GLOBALS
    DEFINE arr_1 ARRAY[1000] OF RECORD #glo #arr_1
        nss                   LIKE ret_rechazo_interno.nss             ,
        tipo_seguro           LIKE ret_rechazo_interno.tipo_seguro     ,
        desc_tipo_seguro      CHAR(16)                                 ,
        tipo_pension          LIKE ret_rechazo_interno.tipo_pension    ,
        desc_tipo_pension     CHAR(16)                                 ,
        tipo_prestacion       LIKE ret_rechazo_interno.tipo_prestacion ,
        desc_tipo_prestacion  CHAR(16)                                 ,
        importe               LIKE ret_rechazo_interno.importe         ,
        paterno               LIKE ret_rechazo_interno.paterno         ,
        materno               LIKE ret_rechazo_interno.materno         ,
        nombres               LIKE ret_rechazo_interno.nombres         ,
        fecha_solicitud       LIKE ret_rechazo_interno.fecha_solicitud ,
        fecha_emision         LIKE ret_rechazo_interno.fecha_emision   ,
        codigo1               LIKE ret_rechazo_interno.codigo1         ,
        desc_codigo1          CHAR(30)                                 ,
        codigo2               LIKE ret_rechazo_interno.codigo2         ,
        desc_codigo2          CHAR(30)                                 ,
        codigo3               LIKE ret_rechazo_interno.codigo3         ,
        desc_codigo3          CHAR(30)                                 ,
        codigo4               LIKE ret_rechazo_interno.codigo4         ,
        desc_codigo4          CHAR(30)                                 ,
        devolucion_doctos     LIKE ret_rechazo_interno.devolucion_doctos,
        estado                CHAR(10)                                 ,
        docto_cod1            LIKE ret_rechazo_interno.docto_cod1      ,
        docto_desc1           LIKE tab_documento.docto_desc            ,
        docto_cod2            LIKE ret_rechazo_interno.docto_cod2      ,
        docto_desc2           LIKE tab_documento.docto_desc            ,
        docto_cod3            LIKE ret_rechazo_interno.docto_cod3      ,
        docto_desc3           LIKE tab_documento.docto_desc            ,
        docto_cod4            LIKE ret_rechazo_interno.docto_cod4      ,
        docto_desc4           LIKE tab_documento.docto_desc            ,
        docto_cod5            LIKE ret_rechazo_interno.docto_cod5      ,
        docto_desc5           LIKE tab_documento.docto_desc            ,
        docto_cod6            LIKE ret_rechazo_interno.docto_cod6      ,
        docto_desc6           LIKE tab_documento.docto_desc            ,
        docto_cod7            LIKE ret_rechazo_interno.docto_cod7      ,
        docto_desc7           LIKE tab_documento.docto_desc            ,
        docto_cod8            LIKE ret_rechazo_interno.docto_cod8      ,
        docto_desc8           LIKE tab_documento.docto_desc
    END RECORD

    DEFINE arr_2 ARRAY[1000] OF RECORD #glo #arr_1
        nss                   LIKE ret_rechazo_interno.nss             ,
        consec_rch            LIKE ret_rechazo_interno.consec_rch      ,
        tipo_seguro           LIKE ret_rechazo_interno.tipo_seguro     ,
        tipo_pension          LIKE ret_rechazo_interno.tipo_pension    ,
        tipo_prestacion       LIKE ret_rechazo_interno.tipo_prestacion ,
        importe               LIKE ret_rechazo_interno.importe         ,
        paterno               LIKE ret_rechazo_interno.paterno         ,
        materno               LIKE ret_rechazo_interno.materno         ,
        nombres               LIKE ret_rechazo_interno.nombres         ,
        fecha_solicitud       LIKE ret_rechazo_interno.fecha_solicitud ,
        fecha_emision         LIKE ret_rechazo_interno.fecha_emision   ,
        codigo1               LIKE ret_rechazo_interno.codigo1         ,
        codigo2               LIKE ret_rechazo_interno.codigo2         ,
        codigo3               LIKE ret_rechazo_interno.codigo3         ,
        codigo4               LIKE ret_rechazo_interno.codigo4         ,
        devolucion_doctos     LIKE ret_rechazo_interno.devolucion_doctos,
        estado                LIKE ret_rechazo_interno.estado          ,
        docto_cod1            LIKE ret_rechazo_interno.docto_cod1      ,
        docto_cod2            LIKE ret_rechazo_interno.docto_cod2      ,
        docto_cod3            LIKE ret_rechazo_interno.docto_cod3      ,
        docto_cod4            LIKE ret_rechazo_interno.docto_cod4      ,
        docto_cod5            LIKE ret_rechazo_interno.docto_cod5      ,
        docto_cod6            LIKE ret_rechazo_interno.docto_cod6      ,
        docto_cod7            LIKE ret_rechazo_interno.docto_cod7      ,
        docto_cod8            LIKE ret_rechazo_interno.docto_cod8  
    END RECORD

    DEFINE arrb_2 ARRAY[1000] OF RECORD #glo #arr_1
        nss                   LIKE ret_rch_int_benef.nss           ,
        consec_rch            LIKE ret_rch_int_benef.consec_rch    ,
        paterno_ben           LIKE ret_rch_int_benef.paterno_ben   ,
        materno_ben           LIKE ret_rch_int_benef.materno_ben   ,
        nombres_ben           LIKE ret_rch_int_benef.nombres_ben   ,
        calle                 LIKE ret_rch_int_benef.calle         ,
        numero                LIKE ret_rch_int_benef.numero        ,
        depto                 LIKE ret_rch_int_benef.depto         ,
        colonia               LIKE ret_rch_int_benef.colonia       ,
        ciudad                LIKE ret_rch_int_benef.ciudad        , 
        estado                LIKE ret_rch_int_benef.estado        ,
        delegacion            LIKE ret_rch_int_benef.delegacion    ,
        codpos                LIKE ret_rch_int_benef.codpos        ,
        telefono              LIKE ret_rch_int_benef.telefono  
    END RECORD

    DEFINE arrb_ins ARRAY[5] OF RECORD #glo #arr_1
        nss                   LIKE ret_rch_int_benef.nss           ,
        consec_rch            LIKE ret_rch_int_benef.consec_rch    ,
        paterno_ben           LIKE ret_rch_int_benef.paterno_ben   ,
        materno_ben           LIKE ret_rch_int_benef.materno_ben   ,
        nombres_ben           LIKE ret_rch_int_benef.nombres_ben   ,
        calle                 LIKE ret_rch_int_benef.calle         ,
        numero                LIKE ret_rch_int_benef.numero        ,
        depto                 LIKE ret_rch_int_benef.depto         ,
        colonia               LIKE ret_rch_int_benef.colonia       ,
        ciudad                LIKE ret_rch_int_benef.ciudad        , 
        estado                LIKE ret_rch_int_benef.estado        ,
        delegacion            LIKE ret_rch_int_benef.delegacion    ,
        codpos                LIKE ret_rch_int_benef.codpos        ,
        telefono              LIKE ret_rch_int_benef.telefono  
    END RECORD

    DEFINE arrb_1 ARRAY[1000] OF RECORD #glo #arr_1
        paterno_ben           LIKE ret_rch_int_benef.paterno_ben   ,
        materno_ben           LIKE ret_rch_int_benef.materno_ben   ,
        nombres_ben           LIKE ret_rch_int_benef.nombres_ben   ,
        calle                 LIKE ret_rch_int_benef.calle         ,
        numero                LIKE ret_rch_int_benef.numero        ,
        depto                 LIKE ret_rch_int_benef.depto         ,
        colonia               LIKE ret_rch_int_benef.colonia       ,
        codpos                LIKE ret_rch_int_benef.codpos        , 
        estado                LIKE ret_rch_int_benef.estado        ,
        desc_estado           CHAR(30)                             ,
        ciudad                LIKE ret_rch_int_benef.ciudad        ,
        desc_ciudad           CHAR(30)                             ,
        delegacion            LIKE ret_rch_int_benef.delegacion    ,
        desc_delegacion       CHAR(30)                             ,
        telefono              LIKE ret_rch_int_benef.telefono  
    END RECORD

    DEFINE arr_4 ARRAY[5] OF RECORD #loc #arr_4
        paterno               CHAR(30) ,
        materno               CHAR(30) ,
        nombres               CHAR(30) ,
        porcentaje            SMALLINT ,
        cod_plaza             SMALLINT ,
        nro_cuenta            CHAR(09) 
    END RECORD

    DEFINE arr_5 ARRAY[5] OF RECORD #loc #arr_4
        paterno               CHAR(30)      ,
        materno               CHAR(30)      ,
        nombres               CHAR(30)      ,
        porcentaje            DECIMAL(5,2)  ,
        monto_en_pesos        DECIMAL(12,2) ,
        cod_banco             SMALLINT      ,
        cod_ciudad            INTEGER       ,
        cod_sucursal          INTEGER  
    END RECORD

    DEFINE reg RECORD #glo #reg
        nss                   CHAR(11)      ,
        consec_rch            INTEGER       ,
        tipo_seguro           CHAR(02)      ,
        desc_tipo_seguro      CHAR(16)      ,
        tipo_pension          CHAR(02)      ,
        desc_tipo_pension     CHAR(16)      ,
        tipo_prestacion       SMALLINT      ,
        desc_tipo_prestacion  CHAR(16)      ,
        importe               DECIMAL(10,2) ,
        paterno               CHAR(25)      ,
        materno               CHAR(25)      ,
        nombres               CHAR(25)      ,
        fecha_solicitud       DATE          ,
        fecha_emision         DATE          ,
        codigo1               CHAR(02)      ,
        desc_codigo1          CHAR(30)      ,
        codigo2               CHAR(02)      ,
        desc_codigo2          CHAR(30)      ,
        codigo3               CHAR(02)      ,
        desc_codigo3          CHAR(30)      ,
        codigo4               CHAR(02)      ,
        desc_codigo4          CHAR(30)      ,
        devolucion_doctos     CHAR(01)      ,
        estado                CHAR(10)      ,
        docto_cod1            CHAR(02)      ,
        docto_desc1           CHAR(60)      ,
        docto_cod2            CHAR(02)      ,
        docto_desc2           CHAR(60)      ,
        docto_cod3            CHAR(02)      ,
        docto_desc3           CHAR(60)      ,
        docto_cod4            CHAR(02)      ,
        docto_desc4           CHAR(60)      ,
        docto_cod5            CHAR(02)      ,
        docto_desc5           CHAR(60)      ,
        docto_cod6            CHAR(02)      ,
        docto_desc6           CHAR(60)      ,
        docto_cod7            CHAR(02)      ,
        docto_desc7           CHAR(60)      ,
        docto_cod8            CHAR(02)      ,
        docto_desc8           CHAR(60)
    END RECORD
  
    DEFINE regb RECORD #glo #des
        paterno_ben           CHAR(40)      ,
        materno_ben           CHAR(40)      ,
        nombres_ben           CHAR(40)      ,
        calle                 CHAR(40)      ,
        numero                CHAR(10)      ,
        depto                 CHAR(10)      ,
        colonia               CHAR(40)      ,
        codpos                CHAR(05)      ,
        estado                SMALLINT      ,
        desc_estado           CHAR(30)      ,
        ciudad                SMALLINT      ,
        desc_ciudad           CHAR(30)      ,
        delegacion            SMALLINT      ,
        desc_delegacion       CHAR(30)      ,
        telefono              CHAR(22)
    END RECORD
  
    DEFINE des RECORD #glo #des
        desc_tipo_seguro      CHAR(40) ,
        desc_tipo_pension     CHAR(40) ,
        desc_tipo_prestacion  CHAR(40) ,
        desc_referencia       CHAR(40) ,
        desc_resolucion       CHAR(40) ,
        des_tipo_pago         CHAR(40) ,
        desc_estado           CHAR(10)
    END RECORD

    DEFINE #glo #date
        HOY                   DATE       ,
        flag                  SMALLINT   ,
        ya_capturada          DATE       ,
        ya_capturada1         CHAR(10)
        
    DEFINE #glo #char
        v_marca               CHAR(0100) ,
        k                     CHAR(1000) ,
        txt_1                 CHAR(1800) ,
        x_busca               CHAR(1200) ,
        c40_descripcion       CHAR(0040) ,
        enter                 CHAR(0001) ,
        usuario               CHAR(0008) ,
        wk_docto              CHAR(0005) ,
        wk_ciudad             CHAR(0005) ,
        wk_delega             CHAR(0005) ,
        wk_codigo             CHAR(02)   ,
        wk_desc_codigo        CHAR(30)   ,
        wk_docto_cod          CHAR(02)   ,
        wk_docto_desc         CHAR(60)   ,
        c8_usuario            CHAR(0008) ,
        opc                   CHAR(01)

    DEFINE #glo #smallint
        s_capturado           ,
        s_codigo_afore        ,
        beneficiarios         ,
        idx1                  ,
        idx2                  ,
        sw_1                  ,
        sw_2                  ,
        cuantos_ben           ,
        vcombina              ,
        pos		      SMALLINT

    DEFINE 
        vstatus                SMALLINT,       
        vdesc_status           CHAR(40),
        vconsecutivo           INTEGER

    DEFINE reg_10              RECORD
        nss                    CHAR(11) ,
        activo_marca           SMALLINT ,
        fecha_act_marca        DATE     ,
        marca_cod              SMALLINT
    END RECORD

    DEFINE reg_ret_reverso_2   RECORD LIKE ret_reverso_2.*

    DEFINE wk_consec_rch       INTEGER 
    DEFINE wk_nss              CHAR(11)

END GLOBALS

MAIN   
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP           ,
        PROMPT LINE LAST     ,
        ACCEPT KEY CONTROL-I 

    CALL init()
    INITIALIZE ya_capturada TO NULL

    OPEN WINDOW retm0061 AT 2,3 WITH FORM "RETM0061" ATTRIBUTE(BORDER)
    DISPLAY " RETM006                         RECHAZOS   INTERNOS                           " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,66 ATTRIBUTE(REVERSE)

    MENU"MENU"
        COMMAND KEY(A) "(A)grega" "Agrega solicitud"
            CALL inicializa()
            CALL agrega() #a
       
        COMMAND KEY(C) "(C)onsulta" "Consulta solicitud"
            CALL inicializa()
            CALL consulta() #c
   
        COMMAND KEY(M) "(M)odifica" "Modifica solicitud"
            CALL inicializa()
            CALL modifica() #m

        COMMAND KEY(E) "(E)limina" "Elimina solicitud"
            CALL inicializa()
            CALL elimina() #e

        COMMAND KEY(S) "(S)alida" "Vuelve al menu"
            EXIT PROGRAM
    END MENU
CLOSE WINDOW retm0061
END MAIN

FUNCTION init()
#--------------
    LET HOY = TODAY

    SELECT codigo_afore   ,
           USER
    INTO   s_codigo_afore ,
           usuario
    FROM   tab_afore_local

END FUNCTION

FUNCTION inicializa()
#i-------------------
    INITIALIZE reg.* TO NULL
    CLEAR FORM
END FUNCTION

FUNCTION agrega()
#a---------------
    DEFINE #loc char
        vtipo_presta_ch            CHAR(2)

    DEFINE #loc #integer
        ult_consecutivo            INTEGER

    DEFINE #loc #date
        f_fecha_conversion         DATE  ,
        x_fecha                    DATE  ,
        s_fecha_pago               DATE

    DEFINE vrfc                    CHAR(13),
           fecha_nac               DATE,
           c_fecha_nac             CHAR(10),
           edad                    SMALLINT,
           anyo_1                  SMALLINT,
           anyo_2                  SMALLINT,
           mes_1                   SMALLINT,
           mes_2                   SMALLINT,
           vcontinua               SMALLINT,
           fecha_emision_ultima    DATE

    DEFINE wk_tipo_seguro      CHAR(02)
    DEFINE wk_tipo_pension     CHAR(02)

    DISPLAY " RETM006                    RECHAZOS   INTERNOS                                " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,66 ATTRIBUTE(REVERSE)
    DISPLAY "AGREGA" AT 1,70 ATTRIBUTE(REVERSE)
    DISPLAY " ESC : Agrega    Ctrl-C : Salir " AT 2,1 

    LET sw_1                = 0
    LET sw_2                = 0
    LET idx1                = 0
    LET idx2                = 0
    LET reg.fecha_solicitud = HOY
    LET reg.estado          = "RECHAZADO"
    INITIALIZE fecha_emision_ultima TO NULL

    INPUT BY NAME 
                  reg.nss                   ,
                  reg.tipo_seguro           ,
                  reg.tipo_pension          ,
                  reg.tipo_prestacion       ,
                  reg.importe               ,
                  reg.paterno               ,
                  reg.materno               ,
                  reg.nombres               ,
                  reg.fecha_solicitud       ,
                  reg.fecha_emision         ,
                  reg.codigo1               ,
                  reg.codigo2               ,
                  reg.codigo3               ,
                  reg.codigo4               ,
                  reg.devolucion_doctos     ,
                  reg.docto_cod1            ,
                  reg.docto_cod2            ,
                  reg.docto_cod3            ,
                  reg.docto_cod4            ,
                  reg.docto_cod5            ,
                  reg.docto_cod6            ,
                  reg.docto_cod7            ,
                  reg.docto_cod8
                  WITHOUT DEFAULTS  
        BEFORE FIELD nss
            LET reg.fecha_solicitud = HOY
            LET reg.estado          = "RECHAZADO"
            DISPLAY reg.fecha_solicitud to fecha_solicitud
            DISPLAY reg.estado          to estado

        AFTER FIELD nss
	{
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD num_resolucion
            END IF
        }
            IF reg.nss IS NULL THEN
                ERROR "   CAMPO NO PUEDE SER NULO" ATTRIBUTE(NORMAL)
                NEXT FIELD nss
            ELSE
                SELECT paterno ,
                       materno ,
                       nombres
                INTO   reg.paterno ,
                       reg.materno ,
                       reg.nombres
                FROM   afi_mae_afiliado
                WHERE  n_seguro = reg.nss
                GROUP  BY 1,2,3

                IF STATUS = NOTFOUND THEN
                    SELECT "OK"
                    FROM   afi_solicitud
                    WHERE  n_seguro = reg.nss
                    GROUP  BY 1

                    IF STATUS = NOTFOUND THEN
                        ERROR "TRABAJADOR NO AFILIADO A LA AFORE"
                        NEXT FIELD nss
                    ELSE
                        ERROR "TRABAJADOR SE ENCUENTRA COMO PREAFILIADO"
                        NEXT FIELD nss
                    END IF
                ELSE
                    DISPLAY BY NAME reg.paterno
                    DISPLAY BY NAME reg.materno
                    DISPLAY BY NAME reg.nombres
                END IF
            END IF

        AFTER FIELD tipo_seguro
            SELECT descripcion
            INTO   reg.desc_tipo_seguro
            FROM   tab_seguro
            WHERE  clave = reg.tipo_seguro

            IF STATUS = NOTFOUND THEN
               ERROR "NO EXISTE ESTE TIPO DE SEGURO           "
	       CALL despliega_tipo_seguro()
               SELECT descripcion
               INTO   reg.desc_tipo_seguro
               FROM   tab_seguro
               WHERE  clave = reg.tipo_seguro
	       DISPLAY reg.tipo_seguro      TO tipo_seguro
	       DISPLAY reg.desc_tipo_seguro TO desc_tipo_seguro
            ELSE
	       DISPLAY reg.desc_tipo_seguro TO desc_tipo_seguro
            END IF

	AFTER FIELD tipo_pension
            SELECT descripcion
            INTO   reg.desc_tipo_pension
            FROM   tab_pension
            WHERE  codigo = reg.tipo_pension

	    IF STATUS = NOTFOUND THEN
	       ERROR "NO EXISTE ESTE TIPO DE PENSION          "
	       CALL despliega_tipo_pension()
               SELECT descripcion
               INTO   reg.desc_tipo_pension
               FROM   tab_pension
               WHERE  codigo = reg.tipo_pension
	       DISPLAY reg.tipo_pension      TO tipo_pension
	       DISPLAY reg.desc_tipo_pension TO desc_tipo_pension
            ELSE
	       DISPLAY reg.desc_tipo_pension TO desc_tipo_pension
            END IF

	AFTER FIELD tipo_prestacion
            SELECT descripcion
            INTO   reg.desc_tipo_prestacion
            FROM   tab_prestacion
            WHERE  codigo = reg.tipo_prestacion

	    IF STATUS = NOTFOUND THEN
	       ERROR "NO EXISTE ESTE TIPO DE PRESTACION       "
	       CALL despliega_tipo_prestacion()
               SELECT descripcion
               INTO   reg.desc_tipo_prestacion
               FROM   tab_prestacion
               WHERE  codigo = reg.tipo_prestacion
	       DISPLAY reg.tipo_prestacion      TO tipo_prestacion
	       DISPLAY reg.desc_tipo_prestacion TO desc_tipo_prestacion
	       IF reg.tipo_prestacion  = 6   OR     
	          reg.tipo_prestacion  = 7  THEN    
		  NEXT FIELD importe
               ELSE
		  LET reg.importe = 0
		  DISPLAY reg.importe TO importe
		  NEXT FIELD fecha_solicitud
               END IF
            ELSE
	       DISPLAY reg.desc_tipo_prestacion TO desc_tipo_prestacion
	       IF reg.tipo_prestacion  = 6   OR     
	          reg.tipo_prestacion  = 7  THEN    
		  NEXT FIELD importe
               ELSE
		  LET reg.importe = 0
		  DISPLAY reg.importe TO importe
		  NEXT FIELD fecha_solicitud
               END IF
            END IF
 
	    LET vcombina = 0
	    CALL valida_combina(reg.tipo_seguro, 
				reg.tipo_pension,
				reg.tipo_prestacion)
                 RETURNING vcombina
            IF  vcombina = 0 THEN
		ERROR 
	        " NO ES VALIDA LA COMBINACION SEGURO, PENSION, PRESTACION"
		NEXT FIELD tipo_seguro
	    END IF
 
 
	AFTER FIELD fecha_emision
	    LET beneficiarios   =  0
	    IF reg.tipo_pension = "VI"   OR
	       reg.tipo_pension = "AS"   OR
	       reg.tipo_pension = "VO"   OR
	       reg.tipo_pension = "OR"   OR 
	       reg.tipo_seguro  = "TJ"  THEN
	       LET beneficiarios = 1
            ELSE
               LET beneficiarios = 0
            END IF
 
	    IF reg.fecha_emision >= reg.fecha_solicitud THEN
	       ERROR "FECHA EMISION DEBE SER MENOR A FECHA SOLICITUD"
	       NEXT FIELD fecha_emision
            END IF
  
	    IF reg.fecha_emision IS NULL THEN
	       ERROR "FECHA EMISION NO PUEDE SER NULO               "
	       NEXT FIELD fecha_emision
            END IF

	AFTER FIELD codigo1
            SELECT desc_corta
            INTO   reg.desc_codigo1
            FROM   ret_txt_rch_interno
            WHERE  cod_rechazo = reg.codigo1

	    IF STATUS = NOTFOUND THEN
	       IF   reg.codigo1 <> 0 THEN
	            ERROR "NO EXISTE ESTE CODIGO DE RECHAZO        "
               END  IF
	       CALL despliega_rechazos()
	       LET  reg.desc_codigo1 = wk_desc_codigo
	       LET  reg.codigo1      = wk_codigo
            END IF
	    DISPLAY reg.desc_codigo1 TO desc_codigo1
	    DISPLAY reg.codigo1      TO codigo1

	AFTER FIELD codigo2
	    IF reg.codigo2 >= 0  THEN
               SELECT desc_corta
               INTO   reg.desc_codigo2
               FROM   ret_txt_rch_interno
               WHERE  cod_rechazo = reg.codigo2

	       IF STATUS = NOTFOUND THEN
	          IF   reg.codigo2 <> 0 THEN
	               ERROR "NO EXISTE ESTE CODIGO DE RECHAZO        "
                  END  IF
	          CALL despliega_rechazos()
	          LET  reg.desc_codigo2 = wk_desc_codigo
		  LET  reg.codigo2      = wk_codigo
               END IF
	       DISPLAY reg.desc_codigo2 TO desc_codigo2
	       DISPLAY reg.codigo2      TO codigo2
	       IF reg.codigo2 = reg.codigo1 THEN
		  ERROR "NO SE RACEPTAN CODIGOS REPETIDOS        "
		  NEXT FIELD codigo2
	       END IF
            ELSE
	       LET reg.codigo2 = reg.codigo3
	       LET reg.codigo3 = reg.codigo4
	       LET reg.codigo4 = ""
	       LET reg.desc_codigo2 = reg.desc_codigo3
	       LET reg.desc_codigo3 = reg.desc_codigo4
	       LET reg.desc_codigo4 = ""
	       DISPLAY reg.codigo2 TO codigo2
	       DISPLAY reg.codigo3 TO codigo3
	       DISPLAY reg.codigo4 TO codigo4
	       DISPLAY reg.desc_codigo2 TO desc_codigo2
	       DISPLAY reg.desc_codigo3 TO desc_codigo3
	       DISPLAY reg.desc_codigo4 TO desc_codigo4
	       NEXT FIELD devolucion_doctos
            END IF

	AFTER FIELD codigo3
	    IF reg.codigo3 >= 0 THEN
               SELECT desc_corta
               INTO   reg.desc_codigo3
               FROM   ret_txt_rch_interno
               WHERE  cod_rechazo = reg.codigo3

	       IF STATUS = NOTFOUND THEN
	          IF   reg.codigo3 <> 0 THEN
	               ERROR "NO EXISTE ESTE CODIGO DE RECHAZO        "
                  END  IF
	          CALL despliega_rechazos()
	          LET  reg.desc_codigo3 = wk_desc_codigo
		  LET  reg.codigo3      = wk_codigo
               END IF
	       DISPLAY reg.desc_codigo3 TO desc_codigo3
	       DISPLAY reg.codigo3      TO codigo3
	       IF reg.codigo3 = reg.codigo1  OR 
	          reg.codigo3 = reg.codigo2 THEN
		  ERROR "NO SE RACEPTAN CODIGOS REPETIDOS        "
		  NEXT FIELD codigo3
	       END IF
            ELSE
	       LET reg.codigo3 = ""
	       LET reg.codigo4 = ""
	       LET reg.desc_codigo3 = ""
	       LET reg.desc_codigo4 = ""
	       DISPLAY reg.codigo3 TO codigo3
	       DISPLAY reg.codigo4 TO codigo4
	       DISPLAY reg.desc_codigo3 TO desc_codigo3
	       DISPLAY reg.desc_codigo4 TO desc_codigo4
	       NEXT FIELD devolucion_doctos
            END IF

	AFTER FIELD codigo4
	    IF reg.codigo4 >= 0 THEN
               SELECT desc_corta
               INTO   reg.desc_codigo4
               FROM   ret_txt_rch_interno
               WHERE  cod_rechazo = reg.codigo4

	       IF STATUS = NOTFOUND THEN
	          IF   reg.codigo4 <> 0 THEN
	               ERROR "NO EXISTE ESTE CODIGO DE RECHAZO        "
                  END  IF
	          CALL despliega_rechazos()
	          LET  reg.desc_codigo4 = wk_desc_codigo
		  LET  reg.codigo4      = wk_codigo
               END IF
	       DISPLAY reg.desc_codigo4 TO desc_codigo4
	       DISPLAY reg.codigo4      TO codigo4
	       IF reg.codigo4 = reg.codigo1  OR 
	          reg.codigo4 = reg.codigo2  OR
	          reg.codigo4 = reg.codigo3 THEN
		  ERROR "NO SE RACEPTAN CODIGOS REPETIDOS        "
		  NEXT FIELD codigo4
	       END IF
            ELSE
	       LET reg.codigo4 = ""
	       LET reg.desc_codigo4 = ""
	       DISPLAY reg.codigo4 TO codigo4
	       DISPLAY reg.desc_codigo4 TO desc_codigo4
	       NEXT FIELD devolucion_doctos
            END IF
 
	AFTER FIELD devolucion_doctos
	    IF  reg.devolucion_doctos = "N" THEN
	        LET reg.docto_cod8  = ""
	        LET reg.docto_cod1  = ""
	        LET reg.docto_cod2  = ""
	        LET reg.docto_cod3  = ""
	        LET reg.docto_cod4  = ""
	        LET reg.docto_cod5  = ""
	        LET reg.docto_cod6  = ""
	        LET reg.docto_cod7  = ""
	        LET reg.docto_desc8 = ""
	        LET reg.docto_desc1 = ""
	        LET reg.docto_desc2 = ""
	        LET reg.docto_desc3 = ""
	        LET reg.docto_desc4 = ""
	        LET reg.docto_desc5 = ""
	        LET reg.docto_desc6 = ""
	        LET reg.docto_desc7 = ""
	        LET reg.docto_desc8 = ""
	        DISPLAY reg.docto_cod1  TO docto_cod1
	        DISPLAY reg.docto_cod2  TO docto_cod2
	        DISPLAY reg.docto_cod3  TO docto_cod3
	        DISPLAY reg.docto_cod4  TO docto_cod4
	        DISPLAY reg.docto_cod5  TO docto_cod5
	        DISPLAY reg.docto_cod6  TO docto_cod6
	        DISPLAY reg.docto_cod7  TO docto_cod7
	        DISPLAY reg.docto_cod8  TO docto_cod8
	        DISPLAY reg.docto_desc1 TO docto_desc1
	        DISPLAY reg.docto_desc2 TO docto_desc2
	        DISPLAY reg.docto_desc3 TO docto_desc3
	        DISPLAY reg.docto_desc4 TO docto_desc4
	        DISPLAY reg.docto_desc5 TO docto_desc5
	        DISPLAY reg.docto_desc6 TO docto_desc6
	        DISPLAY reg.docto_desc7 TO docto_desc7
	        DISPLAY reg.docto_desc8 TO docto_desc8
		NEXT FIELD nss
            ELSE
                IF  reg.devolucion_doctos <> "S"   OR
                    reg.devolucion_doctos =  " "   OR
                    reg.devolucion_doctos IS NULL THEN
	            ERROR "DEVOLUCION DE DOCUMENTOS DEBE SER (S o N)"
		    NEXT FIELD devolucion_doctos
		END IF
	    END IF
	    
	AFTER FIELD docto_cod1
	    LET wk_docto = ""
	    LET wk_docto = "400", reg.docto_cod1
            SELECT docto_desc
            INTO   reg.docto_desc1
            FROM   tab_documento
            WHERE  docto_cod = wk_docto

	    IF  STATUS = NOTFOUND THEN
		IF   reg.docto_cod1 <> 0 THEN
	             ERROR "NO EXISTE ESTE TIPO DE DOCUMENTO        "
                END IF
	        CALL despliega_documentos()
		LET  reg.docto_desc1 = wk_docto_desc
		LET  reg.docto_cod1  = wk_docto_cod
            END IF
	    DISPLAY reg.docto_desc1 TO docto_desc1
	    DISPLAY reg.docto_cod1  TO docto_cod1  

	AFTER FIELD docto_cod2
	    IF reg.docto_cod2 >= 0 THEN
	       LET wk_docto = ""
	       LET wk_docto = "400", reg.docto_cod2
               SELECT docto_desc
               INTO   reg.docto_desc2
               FROM   tab_documento
               WHERE  docto_cod = wk_docto

	       IF STATUS = NOTFOUND THEN
		  IF   reg.docto_cod2 <> 0 THEN
	               ERROR "NO EXISTE ESTE TIPO DE DOCUMENTO        "
                  END IF
	          CALL despliega_documentos()
		  LET  reg.docto_desc2 = wk_docto_desc
		  LET  reg.docto_cod2  = wk_docto_cod
               END IF
	       DISPLAY reg.docto_desc2 TO docto_desc2
	       DISPLAY reg.docto_cod2  TO docto_cod2 
	       IF reg.docto_cod2 = reg.docto_cod1 THEN
		  ERROR "NO SE RACEPTAN CODIGOS REPETIDOS        "
		  NEXT FIELD docto_cod2
	       END IF
            ELSE
	        LET reg.docto_cod2  = reg.docto_cod3
	        LET reg.docto_cod3  = reg.docto_cod4
	        LET reg.docto_cod4  = reg.docto_cod5
	        LET reg.docto_cod5  = reg.docto_cod6
	        LET reg.docto_cod6  = reg.docto_cod7
	        LET reg.docto_cod7  = reg.docto_cod8
	        LET reg.docto_cod8  = ""
	        LET reg.docto_desc2 = reg.docto_desc3
	        LET reg.docto_desc3 = reg.docto_desc4
	        LET reg.docto_desc4 = reg.docto_desc5
	        LET reg.docto_desc5 = reg.docto_desc6
	        LET reg.docto_desc6 = reg.docto_desc7
	        LET reg.docto_desc7 = reg.docto_desc8
	        LET reg.docto_desc8 = ""
	        DISPLAY reg.docto_cod2  TO docto_cod2
	        DISPLAY reg.docto_cod3  TO docto_cod3
	        DISPLAY reg.docto_cod4  TO docto_cod4
	        DISPLAY reg.docto_cod5  TO docto_cod5
	        DISPLAY reg.docto_cod6  TO docto_cod6
	        DISPLAY reg.docto_cod7  TO docto_cod7
	        DISPLAY reg.docto_cod8  TO docto_cod8
	        DISPLAY reg.docto_desc2 TO docto_desc2
	        DISPLAY reg.docto_desc3 TO docto_desc3
	        DISPLAY reg.docto_desc4 TO docto_desc4
	        DISPLAY reg.docto_desc5 TO docto_desc5
	        DISPLAY reg.docto_desc6 TO docto_desc6
	        DISPLAY reg.docto_desc7 TO docto_desc7
	        DISPLAY reg.docto_desc8 TO docto_desc8
	       NEXT FIELD nss
	    END IF

	AFTER FIELD docto_cod3
	    IF reg.docto_cod3 >= 0 THEN
	       LET wk_docto = ""
	       LET wk_docto = "400", reg.docto_cod3
               SELECT docto_desc
               INTO   reg.docto_desc3
               FROM   tab_documento
               WHERE  docto_cod = wk_docto

	       IF STATUS = NOTFOUND THEN
		  IF   reg.docto_cod3 <> 0 THEN
	               ERROR "NO EXISTE ESTE TIPO DE DOCUMENTO        "
                  END IF
	          CALL despliega_documentos()
		  LET  reg.docto_desc3 = wk_docto_desc
		  LET  reg.docto_cod3  = wk_docto_cod
               END IF
	       DISPLAY reg.docto_desc3 TO docto_desc3
	       DISPLAY reg.docto_cod3  TO docto_cod3 
	       IF reg.docto_cod3 = reg.docto_cod1  OR 
	          reg.docto_cod3 = reg.docto_cod2 THEN
		  ERROR "NO SE RACEPTAN CODIGOS REPETIDOS        "
		  NEXT FIELD docto_cod3
	       END IF
            ELSE
	        LET reg.docto_cod3  = reg.docto_cod4
	        LET reg.docto_cod4  = reg.docto_cod5
	        LET reg.docto_cod5  = reg.docto_cod6
	        LET reg.docto_cod6  = reg.docto_cod7
	        LET reg.docto_cod7  = reg.docto_cod8
	        LET reg.docto_cod8  = ""
	        LET reg.docto_desc3 = reg.docto_desc4
	        LET reg.docto_desc4 = reg.docto_desc5
	        LET reg.docto_desc5 = reg.docto_desc6
	        LET reg.docto_desc6 = reg.docto_desc7
	        LET reg.docto_desc7 = reg.docto_desc8
	        LET reg.docto_desc8 = ""
	        DISPLAY reg.docto_cod3  TO docto_cod3
	        DISPLAY reg.docto_cod4  TO docto_cod4
	        DISPLAY reg.docto_cod5  TO docto_cod5
	        DISPLAY reg.docto_cod6  TO docto_cod6
	        DISPLAY reg.docto_cod7  TO docto_cod7
	        DISPLAY reg.docto_cod8  TO docto_cod8
	        DISPLAY reg.docto_desc3 TO docto_desc3
	        DISPLAY reg.docto_desc4 TO docto_desc4
	        DISPLAY reg.docto_desc5 TO docto_desc5
	        DISPLAY reg.docto_desc6 TO docto_desc6
	        DISPLAY reg.docto_desc7 TO docto_desc7
	        DISPLAY reg.docto_desc8 TO docto_desc8
	       NEXT FIELD nss
            END IF

	AFTER FIELD docto_cod4
	    IF reg.docto_cod4 >= 0 THEN
	       LET wk_docto = ""
	       LET wk_docto = "400", reg.docto_cod4
               SELECT docto_desc
               INTO   reg.docto_desc4
               FROM   tab_documento
               WHERE  docto_cod = wk_docto

	       IF STATUS = NOTFOUND THEN
		  IF   reg.docto_cod4 <> 0 THEN
	               ERROR "NO EXISTE ESTE TIPO DE DOCUMENTO        "
                  END IF
	          CALL despliega_documentos()
		  LET  reg.docto_desc4 = wk_docto_desc
		  LET  reg.docto_cod4  = wk_docto_cod
               END IF
	       DISPLAY reg.docto_desc4 TO docto_desc4
	       DISPLAY reg.docto_cod4  TO docto_cod4 
	       IF reg.docto_cod4 = reg.docto_cod1  OR 
	          reg.docto_cod4 = reg.docto_cod2  OR 
	          reg.docto_cod4 = reg.docto_cod3 THEN
		  ERROR "NO SE RACEPTAN CODIGOS REPETIDOS        "
		  NEXT FIELD docto_cod4
	       END IF
            ELSE
	        LET reg.docto_cod4  = reg.docto_cod5
	        LET reg.docto_cod5  = reg.docto_cod6
	        LET reg.docto_cod6  = reg.docto_cod7
	        LET reg.docto_cod7  = reg.docto_cod8
	        LET reg.docto_cod8  = ""
	        LET reg.docto_desc4 = reg.docto_desc5
	        LET reg.docto_desc5 = reg.docto_desc6
	        LET reg.docto_desc6 = reg.docto_desc7
	        LET reg.docto_desc7 = reg.docto_desc8
	        LET reg.docto_desc8 = ""
	        DISPLAY reg.docto_cod4  TO docto_cod4
	        DISPLAY reg.docto_cod5  TO docto_cod5
	        DISPLAY reg.docto_cod6  TO docto_cod6
	        DISPLAY reg.docto_cod7  TO docto_cod7
	        DISPLAY reg.docto_cod8  TO docto_cod8
	        DISPLAY reg.docto_desc4 TO docto_desc4
	        DISPLAY reg.docto_desc5 TO docto_desc5
	        DISPLAY reg.docto_desc6 TO docto_desc6
	        DISPLAY reg.docto_desc7 TO docto_desc7
	        DISPLAY reg.docto_desc8 TO docto_desc8
	       NEXT FIELD nss
            END IF

	AFTER FIELD docto_cod5
	    IF reg.docto_cod5 >= 0 THEN
	       LET wk_docto = ""
	       LET wk_docto = "400", reg.docto_cod5
               SELECT docto_desc
               INTO   reg.docto_desc5
               FROM   tab_documento
               WHERE  docto_cod = wk_docto

	       IF STATUS = NOTFOUND THEN
		  IF   reg.docto_cod5 <> 0 THEN
	               ERROR "NO EXISTE ESTE TIPO DE DOCUMENTO        "
                  END IF
	          CALL despliega_documentos()
		  LET  reg.docto_desc5 = wk_docto_desc
		  LET  reg.docto_cod5  = wk_docto_cod
               END IF
	       DISPLAY reg.docto_desc5 TO docto_desc5
	       DISPLAY reg.docto_cod5  TO docto_cod5 
	       IF reg.docto_cod5 = reg.docto_cod1  OR 
	          reg.docto_cod5 = reg.docto_cod2  OR 
	          reg.docto_cod5 = reg.docto_cod2  OR 
	          reg.docto_cod5 = reg.docto_cod4 THEN
		  ERROR "NO SE RACEPTAN CODIGOS REPETIDOS        "
		  NEXT FIELD docto_cod5
	       END IF
            ELSE
	        LET reg.docto_cod5  = reg.docto_cod6
	        LET reg.docto_cod6  = reg.docto_cod7
	        LET reg.docto_cod7  = reg.docto_cod8
	        LET reg.docto_cod8  = ""
	        LET reg.docto_desc5 = reg.docto_desc6
	        LET reg.docto_desc6 = reg.docto_desc7
	        LET reg.docto_desc7 = reg.docto_desc8
	        LET reg.docto_desc8 = ""
	        DISPLAY reg.docto_cod5  TO docto_cod5
	        DISPLAY reg.docto_cod6  TO docto_cod6
	        DISPLAY reg.docto_cod7  TO docto_cod7
	        DISPLAY reg.docto_cod8  TO docto_cod8
	        DISPLAY reg.docto_desc5 TO docto_desc5
	        DISPLAY reg.docto_desc6 TO docto_desc6
	        DISPLAY reg.docto_desc7 TO docto_desc7
	        DISPLAY reg.docto_desc8 TO docto_desc8
	       NEXT FIELD nss
            END IF

	AFTER FIELD docto_cod6
	    IF reg.docto_cod6 >= 0 THEN
	       LET wk_docto = ""
	       LET wk_docto = "400", reg.docto_cod6
               SELECT docto_desc
               INTO   reg.docto_desc6
               FROM   tab_documento
               WHERE  docto_cod = wk_docto

	       IF STATUS = NOTFOUND THEN
		  IF   reg.docto_cod6 <> 0 THEN
	               ERROR "NO EXISTE ESTE TIPO DE DOCUMENTO        "
                  END IF
	          CALL despliega_documentos()
		  LET  reg.docto_desc6 = wk_docto_desc
		  LET  reg.docto_cod6  = wk_docto_cod
               END IF
	       DISPLAY reg.docto_desc6 TO docto_desc6
	       DISPLAY reg.docto_cod6  TO docto_cod6 
	       IF reg.docto_cod6 = reg.docto_cod1  OR 
	          reg.docto_cod6 = reg.docto_cod2  OR 
	          reg.docto_cod6 = reg.docto_cod3  OR 
	          reg.docto_cod6 = reg.docto_cod4  OR 
	          reg.docto_cod6 = reg.docto_cod5 THEN
		  ERROR "NO SE RACEPTAN CODIGOS REPETIDOS        "
		  NEXT FIELD docto_cod6
	       END IF
            ELSE
	        LET reg.docto_cod6  = reg.docto_cod7
	        LET reg.docto_cod7  = reg.docto_cod8
	        LET reg.docto_cod8  = ""
	        LET reg.docto_desc6 = reg.docto_desc7
	        LET reg.docto_desc7 = reg.docto_desc8
	        LET reg.docto_desc8 = ""
	        DISPLAY reg.docto_cod6  TO docto_cod6
	        DISPLAY reg.docto_cod7  TO docto_cod7
	        DISPLAY reg.docto_cod8  TO docto_cod8
	        DISPLAY reg.docto_desc6 TO docto_desc6
	        DISPLAY reg.docto_desc7 TO docto_desc7
	        DISPLAY reg.docto_desc8 TO docto_desc8
	       NEXT FIELD nss
            END IF

	AFTER FIELD docto_cod7
	    IF reg.docto_cod7 >= 0 THEN
	       LET wk_docto = ""
	       LET wk_docto = "400", reg.docto_cod7
               SELECT docto_desc
               INTO   reg.docto_desc7
               FROM   tab_documento
               WHERE  docto_cod = wk_docto

	       IF STATUS = NOTFOUND THEN
		  IF   reg.docto_cod7 <> 0 THEN
	               ERROR "NO EXISTE ESTE TIPO DE DOCUMENTO        "
                  END IF
	          CALL despliega_documentos()
		  LET  reg.docto_desc7 = wk_docto_desc
		  LET  reg.docto_cod7  = wk_docto_cod
               END IF
	       DISPLAY reg.docto_desc7 TO docto_desc7
	       DISPLAY reg.docto_cod7  TO docto_cod7 
	       IF reg.docto_cod7 = reg.docto_cod1  OR 
	          reg.docto_cod7 = reg.docto_cod2  OR 
	          reg.docto_cod7 = reg.docto_cod3  OR 
	          reg.docto_cod7 = reg.docto_cod4  OR 
	          reg.docto_cod7 = reg.docto_cod5  OR 
	          reg.docto_cod7 = reg.docto_cod6 THEN
		  ERROR "NO SE RACEPTAN CODIGOS REPETIDOS        "
		  NEXT FIELD docto_cod7
	       END IF
            ELSE
	        LET reg.docto_cod7  = reg.docto_cod8
	        LET reg.docto_cod8  = ""
	        LET reg.docto_desc7 = reg.docto_desc8
	        LET reg.docto_desc8 = ""
	        DISPLAY reg.docto_cod7  TO docto_cod7
	        DISPLAY reg.docto_cod8  TO docto_cod8
	        DISPLAY reg.docto_desc7 TO docto_desc7
	        DISPLAY reg.docto_desc8 TO docto_desc8
	       NEXT FIELD nss
            END IF

	AFTER FIELD docto_cod8
	    IF reg.docto_cod8 >= 0 THEN
	       LET wk_docto = ""
	       LET wk_docto = "400", reg.docto_cod8
               SELECT docto_desc
               INTO   reg.docto_desc8
               FROM   tab_documento
               WHERE  docto_cod = wk_docto

	       IF STATUS = NOTFOUND THEN
		  IF   reg.docto_cod8 <> 0 THEN
	               ERROR "NO EXISTE ESTE TIPO DE DOCUMENTO        "
                  END IF
	          CALL despliega_documentos()
		  LET  reg.docto_desc8 = wk_docto_desc
		  LET  reg.docto_cod8  = wk_docto_cod
               END IF
	       DISPLAY reg.docto_desc8 TO docto_desc8
	       DISPLAY reg.docto_cod8  TO docto_cod8 
	       IF reg.docto_cod8 = reg.docto_cod1  OR 
	          reg.docto_cod8 = reg.docto_cod2  OR 
	          reg.docto_cod8 = reg.docto_cod3  OR 
	          reg.docto_cod8 = reg.docto_cod4  OR 
	          reg.docto_cod8 = reg.docto_cod5  OR 
	          reg.docto_cod8 = reg.docto_cod6  OR 
	          reg.docto_cod8 = reg.docto_cod7 THEN
		  ERROR "NO SE RACEPTAN CODIGOS REPETIDOS        "
		  NEXT FIELD docto_cod8
	       END IF
            ELSE
	        LET reg.docto_cod8  = ""
	        LET reg.docto_desc8 = ""
	        DISPLAY reg.docto_cod8  TO docto_cod8
	        DISPLAY reg.docto_desc8 TO docto_desc8
	       NEXT FIELD nss
            END IF
 
        ON KEY (ESC)
	    LET sw_1 = 1
            EXIT INPUT     
 
        ON KEY(CONTROL-C)
	    LET sw_1 = 0
            EXIT INPUT

        ON KEY(INTERRUPT)
	    LET sw_1 = 0
            EXIT INPUT
    END INPUT
    IF  sw_1 = 1 THEN
      LET sw_2 = 1
      IF  beneficiarios = 1 THEN
          CALL agrega_bene()
      END IF
      IF sw_2 = 1 THEN
	LET opc = ""
	CALL pregunta_alta() RETURNING opc
	IF  opc MATCHES "[Ss]" THEN
	    SELECT MAX(consec_rch)
	    INTO   wk_consec_rch
	    FROM   ret_rechazo_interno
	    LET    wk_consec_rch = wk_consec_rch + 1
	    LET    wk_nss        = reg.nss           
		  
            INSERT 
		INTO ret_rechazo_interno
	        VALUES  (
                reg.nss                   ,
	        wk_consec_rch             ,
                reg.paterno               ,
                reg.materno               ,
                reg.nombres               ,
                0                         , #estado
                reg.tipo_seguro           ,
                reg.tipo_pension          ,
                reg.tipo_prestacion       ,
                reg.fecha_solicitud       ,
                reg.fecha_emision         ,
                reg.importe               ,
                reg.devolucion_doctos     ,
	        " "                       , # con_saldo
                reg.codigo1               ,
                reg.codigo2               ,
                reg.codigo3               ,
                reg.codigo4               ,
                reg.docto_cod1            ,
                reg.docto_cod2            ,
                reg.docto_cod3            ,
                reg.docto_cod4            ,
                reg.docto_cod5            ,
                reg.docto_cod6            ,
                reg.docto_cod7            ,
                reg.docto_cod8
		  )
            IF  beneficiarios = 1 THEN
                FOR idx2 = 1 TO idx1
		    INSERT 
			INTO ret_rch_int_benef
			VALUES  (
	                    arrb_ins[idx2].nss         ,
	                    wk_consec_rch              ,
	                    arrb_ins[idx2].paterno_ben ,
	                    arrb_ins[idx2].materno_ben ,
	                    arrb_ins[idx2].nombres_ben ,
	                    arrb_ins[idx2].calle       ,
	                    arrb_ins[idx2].numero      ,
	                    arrb_ins[idx2].depto       ,
	                    arrb_ins[idx2].colonia     ,
	                    arrb_ins[idx2].ciudad      ,
	                    arrb_ins[idx2].estado      ,
	                    arrb_ins[idx2].delegacion  ,
	                    arrb_ins[idx2].codpos      ,
	                    arrb_ins[idx2].telefono    
				)
		END FOR
            END IF
            DISPLAY 
                "REGISTRO INGRESADO ","" AT 20,1 ATTRIBUTE(REVERSE) 
            SLEEP 2
            DISPLAY 
                "                   ","" AT 20,1
        END IF
      END IF
    END IF
    CLEAR FORM   

END FUNCTION

FUNCTION pregunta_alta()
#-------------------------
  DEFINE opc CHAR(01)

     WHILE TRUE
         PROMPT " DESEAS INGRESAR SOLICITUD   S/N ? " FOR opc
         IF opc NOT MATCHES "[SsNn]" THEN
            CONTINUE WHILE
         ELSE
             EXIT WHILE
         END IF
     END WHILE

     RETURN opc

END FUNCTION

FUNCTION pregunta_cambio()
#-------------------------
  DEFINE opc CHAR(01)

     WHILE TRUE
         PROMPT " DESEAS MODIFICAR SOLICITUD   S/N ? " FOR opc
         IF opc NOT MATCHES "[SsNn]" THEN
            CONTINUE WHILE
         ELSE
             EXIT WHILE
         END IF
     END WHILE

     RETURN opc

END FUNCTION

FUNCTION pregunta_baja()
#-------------------------
  DEFINE opc CHAR(01)

     WHILE TRUE
         PROMPT " DESEAS ELIMINAR SOLICITUD   S/N ? " FOR opc
         IF opc NOT MATCHES "[SsNn]" THEN
            CONTINUE WHILE
         ELSE
             EXIT WHILE
         END IF
     END WHILE

     RETURN opc

END FUNCTION

FUNCTION despliega_tipo_seguro()
#dts----------------------------
    DEFINE l_reg ARRAY[1000] OF RECORD
        codigo                CHAR(02) ,
	descripcion	      CHAR(50)
     END RECORD

     DEFINE x_x		      CHAR(200),
            x_buscar	      CHAR(030)

     OPEN WINDOW retm0012 AT 05,20 WITH FORM "RETM0012" ATTRIBUTE(BORDER)
     DISPLAY "                      TIPOS DE SEGURO                    " AT 2,1 ATTRIBUTE(REVERSE)
     INPUT   BY NAME x_buscar
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
	 LET x_x = " SELECT * FROM tab_seguro ",
	           " WHERE descripcion MATCHES ",'"',x_buscar CLIPPED,'"',
	           " ORDER BY 2 " CLIPPED

	 PREPARE pre_1 FROM x_x
	 DECLARE cur_1 CURSOR FOR pre_1
	 LET pos = 1
	 FOREACH cur_1 INTO l_reg[pos].*
	     LET pos = pos + 1
	     IF pos >= 1000 THEN
		 ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
		 EXIT FOREACH
	     END IF
	 END FOREACH

	 IF (pos-1) < 1 THEN
	     ERROR "ARCHIVO TIPO DE SEGURO VACIO"
	 END IF

	 CALL SET_COUNT(pos-1)
	 DISPLAY ARRAY l_reg TO scr_1.*
	     ON KEY ( CONTROL-C )
	         LET pos = 0
		 EXIT DISPLAY

	     ON KEY ( INTERRUPT )
	         LET pos = 0
		 EXIT DISPLAY

	     ON KEY ( CONTROL-M )
	         LET pos = ARR_CURR()
		 IF l_reg[pos].codigo = "AP" THEN
		     ERROR " AP no es valido en este programa "
		     SLEEP 2
		 ELSE
                     LET reg.tipo_seguro = l_reg[pos].codigo     
                     LET des.desc_tipo_seguro = l_reg[pos].descripcion     
	             EXIT DISPLAY
		 END IF
	 END DISPLAY

	 IF pos <> 0 THEN
	     EXIT WHILE
	 END IF
     END WHILE
     CLOSE WINDOW retm0012
END FUNCTION

FUNCTION despliega_tipo_pension()
#dtp----------------------------
    DEFINE l_reg ARRAY[1000] OF RECORD
        codigo                CHAR(02) ,
	descripcion	      CHAR(50)
     END RECORD

     DEFINE x_x		      CHAR(200),
            x_buscar	      CHAR(030)

     OPEN WINDOW retm0012 AT 05,12 WITH FORM "RETM0012" ATTRIBUTE(BORDER)
     DISPLAY "             TIPO DE PENSIONES                           " AT 2,1 ATTRIBUTE(REVERSE)
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
	 LET x_x = " SELECT * FROM tab_pension ",
	           " WHERE descripcion MATCHES ",'"',x_buscar CLIPPED,'"',
	           " ORDER BY 2 " CLIPPED

	 PREPARE pre_2 FROM x_x
	 DECLARE cur_2 CURSOR FOR pre_2

	 LET pos = 1

	 FOREACH cur_2 INTO l_reg[pos].*
	     LET pos = pos + 1
	     IF pos >= 1000 THEN
		 ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
		 EXIT FOREACH
	     END IF
	 END FOREACH

	 IF (pos-1) < 1 THEN
	     ERROR "ARCHIVO TIPO DE SEGURO VACIO"
	 END IF

	 CALL SET_COUNT(pos-1)
	 DISPLAY ARRAY l_reg TO scr_1.*
	     ON KEY ( CONTROL-C )
	         LET pos = 0
		 EXIT DISPLAY

	     ON KEY ( INTERRUPT )
	         LET pos = 0
		 EXIT DISPLAY

	     ON KEY ( CONTROL-M )
	         LET pos = ARR_CURR()
		 IF l_reg[pos].codigo = "AP" THEN
		     ERROR " AP no es valido en este programa "
		     SLEEP 2
		 ELSE
                     LET reg.tipo_pension = l_reg[pos].codigo
                     LET des.desc_tipo_pension = l_reg[pos].descripcion
	             EXIT DISPLAY
                 END IF
	 END DISPLAY

	 IF pos <> 0 THEN
	     EXIT WHILE
	 END IF
     END WHILE
     CLOSE WINDOW retm0012
END FUNCTION

FUNCTION despliega_tipo_prestacion()
#dtpa----------------------------------
    DEFINE l_reg ARRAY[1000] OF RECORD
        codigo                CHAR(02) ,
	descripcion	      CHAR(50)
     END RECORD

     DEFINE x_x		      CHAR(200),
            x_buscar	      CHAR(030)

     OPEN WINDOW retm0012 AT 05,12 WITH FORM "RETM0012" ATTRIBUTE(BORDER)
     DISPLAY "             TIPO DE PRESTACION                          " AT 2,1 ATTRIBUTE(REVERSE)
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
	 LET x_x = " SELECT * FROM tab_prestacion ",
	           " WHERE descripcion MATCHES ",'"',x_buscar CLIPPED,'"',
	           " ORDER BY 1 " CLIPPED

	 PREPARE pre_3 FROM x_x
	 DECLARE cur_3 CURSOR FOR pre_3
	 LET pos = 1
	 FOREACH cur_3 INTO l_reg[pos].*
	     LET pos = pos + 1
	     IF pos >= 1000 THEN
		 ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
		 EXIT FOREACH
	     END IF
	 END FOREACH

	 IF (pos-1) < 1 THEN
	     ERROR "ARCHIVO TIPO DE SEGURO VACIO"
	 END IF

	 CALL SET_COUNT(pos-1)
	 DISPLAY ARRAY l_reg TO scr_1.*
	     ON KEY ( CONTROL-C )
	         LET pos = 0
		 EXIT DISPLAY

	     ON KEY ( INTERRUPT )
	         LET pos = 0
		 EXIT DISPLAY

	     ON KEY ( CONTROL-M )
	         LET pos = ARR_CURR()
                 LET reg.tipo_prestacion = l_reg[pos].codigo
                 LET des.desc_tipo_prestacion = l_reg[pos].descripcion
	         EXIT DISPLAY
	 END DISPLAY

	 IF pos <> 0 THEN
	     EXIT WHILE
	 END IF
     END WHILE
     CLOSE WINDOW retm0012
END FUNCTION

FUNCTION despliega_rechazos()
#dts----------------------------
    DEFINE l_reg ARRAY[1000] OF RECORD
        codigo                CHAR(02) ,
	descripcion	      CHAR(50)
     END RECORD

     DEFINE x_x		      CHAR(200),
            x_buscar	      CHAR(030)

     OPEN WINDOW retm0064 AT 05,20 WITH FORM "RETM0064" ATTRIBUTE(BORDER)
     DISPLAY "                      TIPOS DE RECHAZOS                  " AT 2,1 ATTRIBUTE(REVERSE)
     INPUT   BY NAME x_buscar
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
	 LET x_x = " SELECT cod_rechazo, desc_corta FROM ret_txt_rch_interno ",
	           " WHERE desc_corta MATCHES ",'"',x_buscar CLIPPED,'"',
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
	     ERROR "ARCHIVO DE RECHAZOS VACIO   "
	 END IF

	 CALL SET_COUNT(pos-1)
	 DISPLAY ARRAY l_reg TO scr_1.*
	     ON KEY ( CONTROL-C )
	         LET pos = 0
		 EXIT DISPLAY

	     ON KEY ( INTERRUPT )
	         LET pos = 0
		 EXIT DISPLAY

	     ON KEY ( CONTROL-M )
	         LET pos = ARR_CURR()
                 LET wk_codigo      = l_reg[pos].codigo     
                 LET wk_desc_codigo = l_reg[pos].descripcion     
	         EXIT DISPLAY
	 END DISPLAY

	 IF pos <> 0 THEN
	     EXIT WHILE
	 END IF
     END WHILE
     CLOSE WINDOW retm0064
END FUNCTION

FUNCTION despliega_documentos()
#dts----------------------------
    DEFINE l_reg ARRAY[1000] OF RECORD
        codigo                CHAR(05) ,
	descripcion	      CHAR(50)
     END RECORD

     DEFINE x_x		      CHAR(200),
            x_buscar	      CHAR(030)

     OPEN WINDOW retm0063 AT 05,20 WITH FORM "RETM0063" ATTRIBUTE(BORDER)
     DISPLAY "                      TIPOS DE DOCUMENTOS                " AT 2,1 ATTRIBUTE(REVERSE)
     INPUT   BY NAME x_buscar
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
	 LET x_x = " SELECT docto_cod, docto_desc FROM tab_documento ",
	           " WHERE docto_desc MATCHES ",'"',x_buscar CLIPPED,'"',
	           " AND   ( docto_cod  > 39999 and docto_cod < 49999 ) ",
	           " ORDER BY 1 " CLIPPED

	 PREPARE pre_5 FROM x_x
	 DECLARE cur_5 CURSOR FOR pre_5
	 LET pos = 1
	 FOREACH cur_5 INTO l_reg[pos].*
	     LET pos = pos + 1
	     IF pos >= 1000 THEN
		 ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
		 EXIT FOREACH
	     END IF
	 END FOREACH

	 IF (pos-1) < 1 THEN
	     ERROR "ARCHIVO DE DOCUMENTOS VACIO "
	 END IF

	 CALL SET_COUNT(pos-1)
	 DISPLAY ARRAY l_reg TO scr_1.*
	     ON KEY ( CONTROL-C )
	         LET pos = 0
		 EXIT DISPLAY

	     ON KEY ( INTERRUPT )
	         LET pos = 0
		 EXIT DISPLAY

	     ON KEY ( CONTROL-M )
	         LET pos = ARR_CURR()
                 LET wk_docto_cod  = l_reg[pos].codigo[4,5]
                 LET wk_docto_desc = l_reg[pos].descripcion     
	         EXIT DISPLAY
	 END DISPLAY

	 IF pos <> 0 THEN
	     EXIT WHILE
	 END IF
     END WHILE
     CLOSE WINDOW retm0063
END FUNCTION

FUNCTION consulta()
#c-----------------

    DEFINE #loc #integer
        arr_c                 ,
        i                     INTEGER

    DISPLAY " RETM006                      RECHAZOS   INTERNOS                              " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,66 ATTRIBUTE(REVERSE)
    
    DISPLAY "CONSULTA" AT 1,66 ATTRIBUTE(REVERSE)
    DISPLAY " Ctrl-C : Salir                                            ESC : Consulta" AT 2,1 

    INITIALIZE reg.* TO NULL
    INITIALIZE arr_1 TO NULL
    INITIALIZE arr_2 TO NULL
    CLEAR FORM

    LET reg.fecha_emision = HOY
    LET flag              = 0
    CONSTRUCT BY NAME x_busca ON A.nss            ,
                                 A.paterno        ,
                                 A.materno        ,
                                 A.nombres        ,
                                 A.tipo_seguro    ,
                                 A.tipo_pension   ,
                                 A.tipo_prestacion
    
        ON KEY (CONTROL-C) 
            IF flag = 0 THEN    
                LET flag=1     
                EXIT CONSTRUCT 
            END IF 

        ON KEY (INTERRUPT) 
            IF flag = 0 THEN    
                LET flag=1     
                EXIT CONSTRUCT 
            END IF 

        ON KEY (Esc)
            LET INT_FLAG = FALSE
            EXIT CONSTRUCT

    END CONSTRUCT

    IF  flag = 1 THEN 
        RETURN
    END IF

    LET txt_1 =" SELECT a.nss                ,",
                      " a.consec_rch         ,",
                      " a.tipo_seguro        ,",
                      " a.tipo_pension       ,",
                      " a.tipo_prestacion    ,",
                      " a.importe            ,",
                      " a.paterno            ,",
                      " a.materno            ,",
                      " a.nombres            ,",
                      " a.fecha_solicitud    ,",
                      " a.fecha_emision      ,",
                      " a.codigo1            ,",
                      " a.codigo2            ,",
                      " a.codigo3            ,",
                      " a.codigo4            ,",
                      " a.devolucion_doctos  ,",
                      " a.estado             ,",
                      " a.docto_cod1         ,",
                      " a.docto_cod2         ,",
                      " a.docto_cod3         ,",
                      " a.docto_cod4         ,",
                      " a.docto_cod5         ,",
                      " a.docto_cod6         ,",
                      " a.docto_cod7         ,",
                      " a.docto_cod8          ",
               " FROM   ret_rechazo_interno a ",
               " WHERE  ",x_busca CLIPPED

    PREPARE pre_6 FROM txt_1
    DECLARE cur_6 CURSOR FOR pre_6

    LET i = 1
    FOREACH cur_6 INTO arr_2[i].*

        LET arr_1[i].nss               = arr_2[i].nss
        LET arr_1[i].tipo_seguro       = arr_2[i].tipo_seguro
        LET arr_1[i].tipo_pension      = arr_2[i].tipo_pension
        LET arr_1[i].tipo_prestacion   = arr_2[i].tipo_prestacion
        LET arr_1[i].importe           = arr_2[i].importe
        LET arr_1[i].paterno           = arr_2[i].paterno
        LET arr_1[i].materno           = arr_2[i].materno
        LET arr_1[i].nombres           = arr_2[i].nombres
        LET arr_1[i].fecha_solicitud   = arr_2[i].fecha_solicitud
        LET arr_1[i].fecha_emision     = arr_2[i].fecha_emision
        LET arr_1[i].codigo1           = arr_2[i].codigo1
        LET arr_1[i].codigo2           = arr_2[i].codigo2
        LET arr_1[i].codigo3           = arr_2[i].codigo3
        LET arr_1[i].codigo4           = arr_2[i].codigo4
        LET arr_1[i].devolucion_doctos = arr_2[i].devolucion_doctos
	IF  arr_2[i].estado = 0 THEN
            LET arr_1[i].estado  =  "RECHAZADO"
        ELSE
            LET arr_1[i].estado  =  "ENVIADO  "
	END IF
        LET arr_1[i].docto_cod1        = arr_2[i].docto_cod1
        LET arr_1[i].docto_cod2        = arr_2[i].docto_cod2
        LET arr_1[i].docto_cod3        = arr_2[i].docto_cod3
        LET arr_1[i].docto_cod4        = arr_2[i].docto_cod4
        LET arr_1[i].docto_cod5        = arr_2[i].docto_cod5
        LET arr_1[i].docto_cod6        = arr_2[i].docto_cod6
        LET arr_1[i].docto_cod7        = arr_2[i].docto_cod7
        LET arr_1[i].docto_cod8        = arr_2[i].docto_cod8
    
	SELECT descripcion
        INTO   arr_1[i].desc_tipo_seguro
        FROM   tab_seguro
        WHERE  clave = arr_1[i].tipo_seguro

        SELECT descripcion
        INTO   arr_1[i].desc_tipo_pension
        FROM   tab_pension
        WHERE  codigo = arr_1[i].tipo_pension

        SELECT descripcion
        INTO   arr_1[i].desc_tipo_prestacion
        FROM   tab_prestacion
        WHERE  codigo = arr_1[i].tipo_prestacion

	SELECT desc_corta
        INTO   arr_1[i].desc_codigo1
        FROM   ret_txt_rch_interno
        WHERE  cod_rechazo = arr_1[i].codigo1

        IF arr_1[i].codigo2 > 0 THEN	
	   SELECT desc_corta
           INTO   arr_1[i].desc_codigo2 
           FROM   ret_txt_rch_interno
           WHERE  cod_rechazo = arr_1[i].codigo2
	END IF

        IF arr_1[i].codigo3 > 0 THEN	
	   SELECT desc_corta
           INTO   arr_1[i].desc_codigo3 
           FROM   ret_txt_rch_interno
           WHERE  cod_rechazo = arr_1[i].codigo3
	END IF

        IF arr_1[i].codigo4 > 0 THEN	
	   SELECT desc_corta
           INTO   arr_1[i].desc_codigo4 
           FROM   ret_txt_rch_interno
           WHERE  cod_rechazo = arr_1[i].codigo4
	END IF

        IF arr_1[i].docto_cod1 > 0  THEN	
	   LET wk_docto = ""
	   LET wk_docto = "400", arr_1[i].docto_cod1
	   SELECT docto_desc
           INTO   arr_1[i].docto_desc1
           FROM   tab_documento
           WHERE  docto_cod = wk_docto
	END IF

        IF arr_1[i].docto_cod2 > 0 THEN	
	   LET wk_docto = ""
	   LET wk_docto = "400", arr_1[i].docto_cod2
	   SELECT docto_desc
           INTO   arr_1[i].docto_desc2
           FROM   tab_documento
           WHERE  docto_cod = wk_docto
	END IF

        IF arr_1[i].docto_cod3 > 0 THEN	
	   LET wk_docto = ""
	   LET wk_docto = "400", arr_1[i].docto_cod3
	   SELECT docto_desc
           INTO   arr_1[i].docto_desc3
           FROM   tab_documento
           WHERE  docto_cod = wk_docto
	END IF

        IF arr_1[i].docto_cod4 > 0 THEN	
	   LET wk_docto = ""
	   LET wk_docto = "400", arr_1[i].docto_cod4
	   SELECT docto_desc
           INTO   arr_1[i].docto_desc4
           FROM   tab_documento
           WHERE  docto_cod = wk_docto
	END IF

        IF arr_1[i].docto_cod5 > 0 THEN	
	   LET wk_docto = ""
	   LET wk_docto = "400", arr_1[i].docto_cod5
	   SELECT docto_desc
           INTO   arr_1[i].docto_desc5
           FROM   tab_documento
           WHERE  docto_cod = wk_docto
	END IF

        IF arr_1[i].docto_cod6 > 0 THEN	
	   LET wk_docto = ""
	   LET wk_docto = "400", arr_1[i].docto_cod6
	   SELECT docto_desc
           INTO   arr_1[i].docto_desc6
           FROM   tab_documento
           WHERE  docto_cod = wk_docto
	END IF

        IF arr_1[i].docto_cod7 > 0 THEN	
	   LET wk_docto = ""
	   LET wk_docto = "400", arr_1[i].docto_cod7
	   SELECT docto_desc
           INTO   arr_1[i].docto_desc7
           FROM   tab_documento
           WHERE  docto_cod = wk_docto
	END IF

        IF arr_1[i].docto_cod8 > 0 THEN	
	   LET wk_docto = ""
	   LET wk_docto = "400", arr_1[i].docto_cod8
	   SELECT docto_desc
           INTO   arr_1[i].docto_desc8
           FROM   tab_documento
           WHERE  docto_cod = wk_docto
	END IF

        LET i = i + 1
    END FOREACH

    IF i = 1 THEN
        INITIALIZE reg.* TO NULL
        CLEAR FORM
        ERROR " NO EXISTE REGISTRO " 
	SLEEP 3
        ERROR "                    " 
        RETURN
    END IF

    CALL SET_COUNT(i-1)
     
    DISPLAY 
    " Ctrl-C : Salir     Ctrl-B : Beneficiarios                 ESC : Consulta"
      AT 2,1 
    DISPLAY ARRAY arr_1 TO scr_1.*
	  
        ON KEY ( CONTROL-B )
	    LET beneficiarios = 1
	    EXIT DISPLAY

        ON KEY ( CONTROL-C )
            CALL inicializa()
	    EXIT DISPLAY

        ON KEY ( INTERRUPT )
            CALL inicializa()
	    EXIT DISPLAY
    END DISPLAY

    IF beneficiarios = 1 THEN
       LET pos = ARR_CURR()
       LET wk_nss        = arr_2[pos].nss
       LET wk_consec_rch = arr_2[pos].consec_rch
       CALL consulta_bene()
    END IF

END FUNCTION

FUNCTION modifica()
#m-----------------
    DEFINE #loc #smallint
        i                     ,
        vmodif             INTEGER

    DISPLAY " RETM006                      RECHAZOS   INTERNOS                              " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,66 ATTRIBUTE(REVERSE)
    
    DISPLAY "MODIFICA" AT 1,66 ATTRIBUTE(REVERSE)
    DISPLAY " ESC : Modifica    Ctrl-C : Salir " AT 2,1 

    INITIALIZE reg.* TO NULL
    CLEAR FORM
    LET reg.fecha_emision = HOY
    LET flag              = 0

    CONSTRUCT BY NAME x_busca ON A.nss            ,
                                 A.paterno        ,
                                 A.materno        ,
                                 A.nombres        ,
                                 A.tipo_seguro    ,
                                 A.tipo_pension   ,
                                 A.tipo_prestacion
    
        ON KEY (CONTROL-C) 
            IF flag = 0 THEN    
                LET flag=1     
                EXIT CONSTRUCT 
            END IF 

        ON KEY (INTERRUPT) 
            IF flag = 0 THEN    
                LET flag=1     
                EXIT CONSTRUCT 
            END IF 

        ON KEY (Esc)
            LET INT_FLAG = FALSE
            EXIT CONSTRUCT

    END CONSTRUCT

    IF  flag = 1 THEN 
        RETURN
    END IF

    LET txt_1 =" SELECT a.nss                ,",
                      " a.consec_rch         ,",
                      " a.tipo_seguro        ,",
                      " a.tipo_pension       ,",
                      " a.tipo_prestacion    ,",
                      " a.importe            ,",
                      " a.paterno            ,",
                      " a.materno            ,",
                      " a.nombres            ,",
                      " a.fecha_solicitud    ,",
                      " a.fecha_emision      ,",
                      " a.codigo1            ,",
                      " a.codigo2            ,",
                      " a.codigo3            ,",
                      " a.codigo4            ,",
                      " a.devolucion_doctos  ,",
                      " a.estado             ,",
                      " a.docto_cod1         ,",
                      " a.docto_cod2         ,",
                      " a.docto_cod3         ,",
                      " a.docto_cod4         ,",
                      " a.docto_cod5         ,",
                      " a.docto_cod6         ,",
                      " a.docto_cod7         ,",
                      " a.docto_cod8          ",
               " FROM   ret_rechazo_interno a ",
               " WHERE  ",x_busca CLIPPED

    PREPARE pre_7 FROM txt_1
    DECLARE cur_7 CURSOR FOR pre_7

    LET i = 1
    FOREACH cur_7 INTO arr_2[i].*

        LET arr_1[i].nss               = arr_2[i].nss
        LET arr_1[i].tipo_seguro       = arr_2[i].tipo_seguro
        LET arr_1[i].tipo_pension      = arr_2[i].tipo_pension
        LET arr_1[i].tipo_prestacion   = arr_2[i].tipo_prestacion
        LET arr_1[i].importe           = arr_2[i].importe
        LET arr_1[i].paterno           = arr_2[i].paterno
        LET arr_1[i].materno           = arr_2[i].materno
        LET arr_1[i].nombres           = arr_2[i].nombres
        LET arr_1[i].fecha_solicitud   = arr_2[i].fecha_solicitud
        LET arr_1[i].fecha_emision     = arr_2[i].fecha_emision
        LET arr_1[i].codigo1           = arr_2[i].codigo1
        LET arr_1[i].codigo2           = arr_2[i].codigo2
        LET arr_1[i].codigo3           = arr_2[i].codigo3
        LET arr_1[i].codigo4           = arr_2[i].codigo4
        LET arr_1[i].devolucion_doctos = arr_2[i].devolucion_doctos
	IF  arr_2[i].estado = 0 THEN
            LET arr_1[i].estado  =  "RECHAZADO"
        ELSE
            LET arr_1[i].estado  =  "ENVIADO  "
	END IF
        LET arr_1[i].docto_cod1        = arr_2[i].docto_cod1
        LET arr_1[i].docto_cod2        = arr_2[i].docto_cod2
        LET arr_1[i].docto_cod3        = arr_2[i].docto_cod3
        LET arr_1[i].docto_cod4        = arr_2[i].docto_cod4
        LET arr_1[i].docto_cod5        = arr_2[i].docto_cod5
        LET arr_1[i].docto_cod6        = arr_2[i].docto_cod6
        LET arr_1[i].docto_cod7        = arr_2[i].docto_cod7
        LET arr_1[i].docto_cod8        = arr_2[i].docto_cod8
    
	SELECT descripcion
        INTO   arr_1[i].desc_tipo_seguro
        FROM   tab_seguro
        WHERE  clave = arr_1[i].tipo_seguro

        SELECT descripcion
        INTO   arr_1[i].desc_tipo_pension
        FROM   tab_pension
        WHERE  codigo = arr_1[i].tipo_pension

        SELECT descripcion
        INTO   arr_1[i].desc_tipo_prestacion
        FROM   tab_prestacion
        WHERE  codigo = arr_1[i].tipo_prestacion

	SELECT desc_corta
        INTO   arr_1[i].desc_codigo1
        FROM   ret_txt_rch_interno
        WHERE  cod_rechazo = arr_1[i].codigo1

        IF arr_1[i].codigo2 > 0 THEN	
	   SELECT desc_corta
           INTO   arr_1[i].desc_codigo2 
           FROM   ret_txt_rch_interno
           WHERE  cod_rechazo = arr_1[i].codigo2
	END IF

        IF arr_1[i].codigo3 > 0 THEN	
	   SELECT desc_corta
           INTO   arr_1[i].desc_codigo3 
           FROM   ret_txt_rch_interno
           WHERE  cod_rechazo = arr_1[i].codigo3
	END IF

        IF arr_1[i].codigo4 > 0 THEN	
	   SELECT desc_corta
           INTO   arr_1[i].desc_codigo4 
           FROM   ret_txt_rch_interno
           WHERE  cod_rechazo = arr_1[i].codigo4
	END IF

        IF arr_1[i].docto_cod1 > 0  THEN	
	   LET wk_docto = ""
	   LET wk_docto = "400", arr_1[i].docto_cod1
	   SELECT docto_desc
           INTO   arr_1[i].docto_desc1
           FROM   tab_documento
           WHERE  docto_cod = wk_docto
	END IF

        IF arr_1[i].docto_cod2 > 0 THEN	
	   LET wk_docto = ""
	   LET wk_docto = "400", arr_1[i].docto_cod2
	   SELECT docto_desc
           INTO   arr_1[i].docto_desc2
           FROM   tab_documento
           WHERE  docto_cod = wk_docto
	END IF

        IF arr_1[i].docto_cod3 > 0 THEN	
	   LET wk_docto = ""
	   LET wk_docto = "400", arr_1[i].docto_cod3
	   SELECT docto_desc
           INTO   arr_1[i].docto_desc3
           FROM   tab_documento
           WHERE  docto_cod = wk_docto
	END IF

        IF arr_1[i].docto_cod4 > 0 THEN	
	   LET wk_docto = ""
	   LET wk_docto = "400", arr_1[i].docto_cod4
	   SELECT docto_desc
           INTO   arr_1[i].docto_desc4
           FROM   tab_documento
           WHERE  docto_cod = wk_docto
	END IF

        IF arr_1[i].docto_cod5 > 0 THEN	
	   LET wk_docto = ""
	   LET wk_docto = "400", arr_1[i].docto_cod5
	   SELECT docto_desc
           INTO   arr_1[i].docto_desc5
           FROM   tab_documento
           WHERE  docto_cod = wk_docto
	END IF

        IF arr_1[i].docto_cod6 > 0 THEN	
	   LET wk_docto = ""
	   LET wk_docto = "400", arr_1[i].docto_cod6
	   SELECT docto_desc
           INTO   arr_1[i].docto_desc6
           FROM   tab_documento
           WHERE  docto_cod = wk_docto
	END IF

        IF arr_1[i].docto_cod7 > 0 THEN	
	   LET wk_docto = ""
	   LET wk_docto = "400", arr_1[i].docto_cod7
	   SELECT docto_desc
           INTO   arr_1[i].docto_desc7
           FROM   tab_documento
           WHERE  docto_cod = wk_docto
	END IF

        IF arr_1[i].docto_cod8 > 0 THEN	
	   LET wk_docto = ""
	   LET wk_docto = "400", arr_1[i].docto_cod8
	   SELECT docto_desc
           INTO   arr_1[i].docto_desc8
           FROM   tab_documento
           WHERE  docto_cod = wk_docto
	END IF

        LET i = i + 1
    END FOREACH

    IF i = 1 THEN
        INITIALIZE reg.* TO NULL
        CLEAR FORM
        ERROR " NO EXISTE REGISTRO " 
	SLEEP 3
        ERROR "                    " 
        RETURN
    END IF

    CALL SET_COUNT(i-1)
     
    DISPLAY 
    " Ctrl-C : Salir     Ctrl-B : Beneficiarios                 ESC : Modifica"
      AT 2,1 
    DISPLAY ARRAY arr_1 TO scr_1.*
	  
        ON KEY ( CONTROL-B )
           LET pos = ARR_CURR()
	   IF  arr_2[pos].estado = 1 THEN
	       ERROR " NO SE PUEDEN MODIFICAR REGISTROS YA ENVIADOS "
	       SLEEP 3
	       ERROR "                                              "
	       LET flag = 1
	       LET beneficiarios = 0
	   ELSE  
	       LET beneficiarios = 1
	   END IF
	   EXIT DISPLAY

        ON KEY ( CONTROL-C )   
           LET flag=1
           EXIT DISPLAY       
	  
        ON KEY ( INTERRUPT )   
           LET flag=1
           EXIT DISPLAY       
	  
        ON KEY ( ESC )
           LET pos = ARR_CURR()
	   IF  arr_2[pos].estado = 1 THEN
	       ERROR " NO SE PUEDEN MODIFICAR REGISTROS YA ENVIADOS "
	       SLEEP 3
	       ERROR "                                              "
	       LET flag = 1
	   END IF
           EXIT DISPLAY

    END DISPLAY
    LET pos = ARR_CURR()
    LET wk_nss        = arr_2[pos].nss
    LET wk_consec_rch = arr_2[pos].consec_rch

    IF beneficiarios = 1 THEN
       CALL modifica_bene()
    END IF

    IF flag = 1 THEN 
        RETURN
        LET flag=0
    ELSE
	CALL construccion()
    END IF
    CLEAR FORM   

END FUNCTION

FUNCTION construccion()
#c------------------------------------
    DEFINE #loc #smallint
        i                             ,
        s_tipo_movimiento_ant         ,
        primera_vez           SMALLINT,
        consecutivo_modifica  INTEGER ,
        opc                   CHAR(01),  
        vmodif2               INTEGER 
  
    LET reg.nss                  = arr_1[pos].nss     
    LET reg.paterno              = arr_1[pos].paterno
    LET reg.materno              = arr_1[pos].materno
    LET reg.nombres              = arr_1[pos].nombres
    LET reg.tipo_seguro          = arr_1[pos].tipo_seguro
    LET reg.desc_tipo_seguro     = arr_1[pos].desc_tipo_seguro
    LET reg.tipo_pension         = arr_1[pos].tipo_pension
    LET reg.desc_tipo_pension    = arr_1[pos].desc_tipo_pension
    LET reg.tipo_prestacion      = arr_1[pos].tipo_prestacion
    LET reg.desc_tipo_prestacion = arr_1[pos].desc_tipo_prestacion
    LET reg.fecha_solicitud      = arr_1[pos].fecha_solicitud
    LET reg.fecha_emision        = arr_1[pos].fecha_emision 
    LET reg.importe              = arr_1[pos].importe    
    LET reg.codigo1              = arr_1[pos].codigo1        
    LET reg.desc_codigo1         = arr_1[pos].desc_codigo1    
    LET reg.codigo2              = arr_1[pos].codigo2 
    LET reg.desc_codigo2         = arr_1[pos].desc_codigo2    
    LET reg.codigo3              = arr_1[pos].codigo3        
    LET reg.desc_codigo3         = arr_1[pos].desc_codigo3    
    LET reg.codigo4              = arr_1[pos].codigo4 
    LET reg.desc_codigo4         = arr_1[pos].desc_codigo4    
    LET reg.devolucion_doctos    = arr_1[pos].devolucion_doctos
    LET reg.estado               = arr_1[pos].estado        
    LET reg.docto_cod1           = arr_1[pos].docto_cod1       
    LET reg.docto_desc1          = arr_1[pos].docto_desc1         
    LET reg.docto_cod2           = arr_1[pos].docto_cod2        
    LET reg.docto_desc2          = arr_1[pos].docto_desc2         
    LET reg.docto_cod3           = arr_1[pos].docto_cod3         
    LET reg.docto_desc3          = arr_1[pos].docto_desc3         
    LET reg.docto_cod4           = arr_1[pos].docto_cod4         
    LET reg.docto_desc4          = arr_1[pos].docto_desc4         
    LET reg.docto_cod5           = arr_1[pos].docto_cod5         
    LET reg.docto_desc5          = arr_1[pos].docto_desc5         
    LET reg.docto_cod6           = arr_1[pos].docto_cod6   
    LET reg.docto_desc6          = arr_1[pos].docto_desc6         
    LET reg.docto_cod7           = arr_1[pos].docto_cod7         
    LET reg.docto_desc7          = arr_1[pos].docto_desc7         
    LET reg.docto_cod8           = arr_1[pos].docto_cod8   
    LET reg.docto_desc8          = arr_1[pos].docto_desc8         
  
    INPUT BY NAME 
          reg.nss                   ,
          reg.tipo_seguro           ,
          reg.tipo_pension          ,
          reg.tipo_prestacion       ,
          reg.importe               ,
          reg.paterno               ,
          reg.materno               ,
          reg.nombres               ,
          reg.fecha_solicitud       ,
          reg.fecha_emision         ,
          reg.codigo1               ,
          reg.codigo2               ,
          reg.codigo3               ,
          reg.codigo4               ,
          reg.devolucion_doctos     ,
          reg.docto_cod1            ,
          reg.docto_cod2            ,
          reg.docto_cod3            ,
          reg.docto_cod4            ,
          reg.docto_cod5            ,
          reg.docto_cod6            ,
          reg.docto_cod7            ,
          reg.docto_cod8
          WITHOUT DEFAULTS  

        AFTER FIELD tipo_seguro
            SELECT descripcion
            INTO   reg.desc_tipo_seguro
            FROM   tab_seguro
            WHERE  clave = reg.tipo_seguro

            IF STATUS = NOTFOUND THEN
               ERROR "NO EXISTE ESTE TIPO DE SEGURO           "
	       CALL despliega_tipo_seguro()
               SELECT descripcion
               INTO   reg.desc_tipo_seguro
               FROM   tab_seguro
               WHERE  clave = reg.tipo_seguro
	       DISPLAY reg.tipo_seguro      TO tipo_seguro
	       DISPLAY reg.desc_tipo_seguro TO desc_tipo_seguro
            ELSE
	       DISPLAY reg.desc_tipo_seguro TO desc_tipo_seguro
            END IF

	AFTER FIELD tipo_pension
            SELECT descripcion
            INTO   reg.desc_tipo_pension
            FROM   tab_pension
            WHERE  codigo = reg.tipo_pension

	    IF STATUS = NOTFOUND THEN
	       ERROR "NO EXISTE ESTE TIPO DE PENSION          "
	       CALL despliega_tipo_pension()
               SELECT descripcion
               INTO   reg.desc_tipo_pension
               FROM   tab_pension
               WHERE  codigo = reg.tipo_pension
	       DISPLAY reg.tipo_pension      TO tipo_pension
	       DISPLAY reg.desc_tipo_pension TO desc_tipo_pension
            ELSE
	       DISPLAY reg.desc_tipo_pension TO desc_tipo_pension
            END IF

	AFTER FIELD tipo_prestacion
            SELECT descripcion
            INTO   reg.desc_tipo_prestacion
            FROM   tab_prestacion
            WHERE  codigo = reg.tipo_prestacion

	    IF STATUS = NOTFOUND THEN
	       ERROR "NO EXISTE ESTE TIPO DE PRESTACION       "
	       CALL despliega_tipo_prestacion()
               SELECT descripcion
               INTO   reg.desc_tipo_prestacion
               FROM   tab_prestacion
               WHERE  codigo = reg.tipo_prestacion
	       DISPLAY reg.tipo_prestacion      TO tipo_prestacion
	       DISPLAY reg.desc_tipo_prestacion TO desc_tipo_prestacion
	       IF reg.tipo_prestacion  = 6   OR     
	          reg.tipo_prestacion  = 7  THEN    
		  NEXT FIELD importe
               ELSE
		  LET reg.importe = 0
		  DISPLAY reg.importe TO importe
		  NEXT FIELD fecha_solicitud
               END IF
            ELSE
	       DISPLAY reg.desc_tipo_prestacion TO desc_tipo_prestacion
	       IF reg.tipo_prestacion  = 6   OR     
	          reg.tipo_prestacion  = 7  THEN    
		  NEXT FIELD importe
               ELSE
		  LET reg.importe = 0
		  DISPLAY reg.importe TO importe
		  NEXT FIELD fecha_solicitud
               END IF
            END IF
 
	    LET vcombina = 0
	    CALL valida_combina(reg.tipo_seguro, 
				reg.tipo_pension,
				reg.tipo_prestacion)
                 RETURNING vcombina
            IF  vcombina = 0 THEN
		ERROR 
	        " NO ES VALIDA LA COMBINACION SEGURO, PENSION, PRESTACION"
		NEXT FIELD tipo_seguro
	    END IF
 
	AFTER FIELD fecha_emision
	    LET beneficiarios   =  0
	    IF reg.tipo_pension = "VI"   OR
	       reg.tipo_pension = "AS"   OR
	       reg.tipo_pension = "VO"   OR
	       reg.tipo_pension = "OR"   OR 
	       reg.tipo_seguro  = "TJ"  THEN
	       LET beneficiarios = 1
            ELSE
               LET beneficiarios = 0
            END IF
 
	    IF reg.fecha_emision >= reg.fecha_solicitud THEN
	       ERROR "FECHA EMISION DEBE SER MENOR A FECHA SOLICITUD"
	       NEXT FIELD fecha_emision
            END IF
  
	AFTER FIELD codigo1
            SELECT desc_corta
            INTO   reg.desc_codigo1
            FROM   ret_txt_rch_interno
            WHERE  cod_rechazo = reg.codigo1

	    IF STATUS = NOTFOUND THEN
	       IF   reg.codigo1 <> 0 THEN
	            ERROR "NO EXISTE ESTE CODIGO DE RECHAZO        "
               END  IF
	       ERROR "NO EXISTE ESTE CODIGO DE RECHAZO        "
	       CALL despliega_rechazos()
	       LET  reg.desc_codigo1 = wk_desc_codigo
	       LET  reg.codigo1      = wk_codigo
            END IF
	    DISPLAY reg.desc_codigo1 TO desc_codigo1
	    DISPLAY reg.codigo1      TO codigo1

	AFTER FIELD codigo2
	    IF reg.codigo2 >= 0 THEN
               SELECT desc_corta
               INTO   reg.desc_codigo2
               FROM   ret_txt_rch_interno
               WHERE  cod_rechazo = reg.codigo2

	       IF STATUS = NOTFOUND THEN
	          IF   reg.codigo2 <> 0 THEN
	               ERROR "NO EXISTE ESTE CODIGO DE RECHAZO        "
                  END  IF
	          CALL despliega_rechazos()
	          LET  reg.desc_codigo2 = wk_desc_codigo
	          LET  reg.codigo2      = wk_codigo
               END IF
	       DISPLAY reg.desc_codigo2 TO desc_codigo2
	       DISPLAY reg.codigo2      TO codigo2
	       IF reg.codigo2 = reg.codigo1 THEN
		  ERROR "NO SE RACEPTAN CODIGOS REPETIDOS        "
		  NEXT FIELD codigo2
	       END IF
            ELSE
	       LET reg.codigo2 = reg.codigo3
	       LET reg.codigo3 = reg.codigo4
	       LET reg.codigo4 = ""
	       LET reg.desc_codigo2 = reg.desc_codigo3
	       LET reg.desc_codigo3 = reg.desc_codigo4
	       LET reg.desc_codigo4 = ""
	       DISPLAY reg.codigo2 TO codigo2
	       DISPLAY reg.codigo3 TO codigo3
	       DISPLAY reg.codigo4 TO codigo4
	       DISPLAY reg.desc_codigo2 TO desc_codigo2
	       DISPLAY reg.desc_codigo3 TO desc_codigo3
	       DISPLAY reg.desc_codigo4 TO desc_codigo4
	       NEXT FIELD devolucion_doctos
            END IF

	AFTER FIELD codigo3
	    IF reg.codigo3 >= 0 THEN
               SELECT desc_corta
               INTO   reg.desc_codigo3
               FROM   ret_txt_rch_interno
               WHERE  cod_rechazo = reg.codigo3

	       IF STATUS = NOTFOUND THEN
	          IF   reg.codigo3 <> 0 THEN
	               ERROR "NO EXISTE ESTE CODIGO DE RECHAZO        "
                  END  IF
	          CALL despliega_rechazos()
	          LET  reg.desc_codigo3 = wk_desc_codigo
	          LET  reg.codigo3      = wk_codigo
               END IF
	       DISPLAY reg.desc_codigo3 TO desc_codigo3
	       DISPLAY reg.codigo3      TO codigo3
	       IF reg.codigo3 = reg.codigo1  OR 
	          reg.codigo3 = reg.codigo2 THEN
		  ERROR "NO SE RACEPTAN CODIGOS REPETIDOS        "
		  NEXT FIELD codigo3
	       END IF
            ELSE
	       LET reg.codigo3 = ""
	       LET reg.codigo4 = ""
	       LET reg.desc_codigo3 = ""
	       LET reg.desc_codigo4 = ""
	       DISPLAY reg.codigo3 TO codigo3
	       DISPLAY reg.codigo4 TO codigo4
	       DISPLAY reg.desc_codigo3 TO desc_codigo3
	       DISPLAY reg.desc_codigo4 TO desc_codigo4
	       NEXT FIELD devolucion_doctos
            END IF

	AFTER FIELD codigo4
	    IF reg.codigo4 >= 0 THEN
               SELECT desc_corta
               INTO   reg.desc_codigo4
               FROM   ret_txt_rch_interno
               WHERE  cod_rechazo = reg.codigo4

	       IF STATUS = NOTFOUND THEN
	          IF   reg.codigo4 <> 0 THEN
	               ERROR "NO EXISTE ESTE CODIGO DE RECHAZO        "
                  END  IF
	          CALL despliega_rechazos()
	          LET  reg.desc_codigo4 = wk_desc_codigo
	          LET  reg.codigo4      = wk_codigo
               END IF
	       DISPLAY reg.desc_codigo4 TO desc_codigo4
	       DISPLAY reg.codigo4      TO codigo4
	       IF reg.codigo4 = reg.codigo1  OR 
	          reg.codigo4 = reg.codigo2  OR
	          reg.codigo4 = reg.codigo3 THEN
		  ERROR "NO SE RACEPTAN CODIGOS REPETIDOS        "
		  NEXT FIELD codigo4
	       END IF
            ELSE
	       LET reg.codigo4 = ""
	       LET reg.desc_codigo4 = ""
	       DISPLAY reg.codigo4 TO codigo4
	       DISPLAY reg.desc_codigo4 TO desc_codigo4
	       NEXT FIELD devolucion_doctos
            END IF
 
	AFTER FIELD devolucion_doctos
	    IF  reg.devolucion_doctos = "N" THEN
	        LET reg.docto_cod8  = ""
	        LET reg.docto_cod1  = ""
	        LET reg.docto_cod2  = ""
	        LET reg.docto_cod3  = ""
	        LET reg.docto_cod4  = ""
	        LET reg.docto_cod5  = ""
	        LET reg.docto_cod6  = ""
	        LET reg.docto_cod7  = ""
	        LET reg.docto_desc8 = ""
	        LET reg.docto_desc1 = ""
	        LET reg.docto_desc2 = ""
	        LET reg.docto_desc3 = ""
	        LET reg.docto_desc4 = ""
	        LET reg.docto_desc5 = ""
	        LET reg.docto_desc6 = ""
	        LET reg.docto_desc7 = ""
	        LET reg.docto_desc8 = ""
	        DISPLAY reg.docto_cod1  TO docto_cod1
	        DISPLAY reg.docto_cod2  TO docto_cod2
	        DISPLAY reg.docto_cod3  TO docto_cod3
	        DISPLAY reg.docto_cod4  TO docto_cod4
	        DISPLAY reg.docto_cod5  TO docto_cod5
	        DISPLAY reg.docto_cod6  TO docto_cod6
	        DISPLAY reg.docto_cod7  TO docto_cod7
	        DISPLAY reg.docto_cod8  TO docto_cod8
	        DISPLAY reg.docto_desc1 TO docto_desc1
	        DISPLAY reg.docto_desc2 TO docto_desc2
	        DISPLAY reg.docto_desc3 TO docto_desc3
	        DISPLAY reg.docto_desc4 TO docto_desc4
	        DISPLAY reg.docto_desc5 TO docto_desc5
	        DISPLAY reg.docto_desc6 TO docto_desc6
	        DISPLAY reg.docto_desc7 TO docto_desc7
	        DISPLAY reg.docto_desc8 TO docto_desc8
		NEXT FIELD nss
            ELSE
                IF  reg.devolucion_doctos <> "S"   OR
                    reg.devolucion_doctos =  " "   OR
                    reg.devolucion_doctos IS NULL THEN
	            ERROR "DEVOLUCION DE DOCUMENTOS DEBE SER (S o N)"
		    NEXT FIELD devolucion_doctos
		END IF
	    END IF
	    
	AFTER FIELD docto_cod1
	    LET wk_docto = ""
	    LET wk_docto = "400", reg.docto_cod1
            SELECT docto_desc
            INTO   reg.docto_desc1
            FROM   tab_documento
            WHERE  docto_cod = wk_docto

	    IF  STATUS = NOTFOUND THEN
		IF   reg.docto_cod1 <> 0 THEN
	             ERROR "NO EXISTE ESTE TIPO DE DOCUMENTO        "
                END IF
	        CALL despliega_documentos()
		LET  reg.docto_desc1 = wk_docto_desc
		LET  reg.docto_cod1  = wk_docto_cod
            END IF
	    DISPLAY reg.docto_desc1 TO docto_desc1
	    DISPLAY reg.docto_cod1  TO docto_cod1  

	AFTER FIELD docto_cod2
	    IF reg.docto_cod2 >= 0 THEN
	       LET wk_docto = ""
	       LET wk_docto = "400", reg.docto_cod2
               SELECT docto_desc
               INTO   reg.docto_desc2
               FROM   tab_documento
               WHERE  docto_cod = wk_docto

	       IF STATUS = NOTFOUND THEN
		  IF   reg.docto_cod2 <> 0 THEN
	               ERROR "NO EXISTE ESTE TIPO DE DOCUMENTO        "
                  END IF
	          CALL despliega_documentos()
	  	  LET  reg.docto_desc2 = wk_docto_desc
		  LET  reg.docto_cod2  = wk_docto_cod
               END IF
	       DISPLAY reg.docto_desc2 TO docto_desc2
	       DISPLAY reg.docto_cod2  TO docto_cod2  
	       IF reg.docto_cod2 = reg.docto_cod1 THEN
		  ERROR "NO SE RACEPTAN CODIGOS REPETIDOS        "
		  NEXT FIELD docto_cod2
	       END IF
            ELSE
	        LET reg.docto_cod2  = reg.docto_cod3
	        LET reg.docto_cod3  = reg.docto_cod4
	        LET reg.docto_cod4  = reg.docto_cod5
	        LET reg.docto_cod5  = reg.docto_cod6
	        LET reg.docto_cod6  = reg.docto_cod7
	        LET reg.docto_cod7  = reg.docto_cod8
	        LET reg.docto_cod8  = ""
	        LET reg.docto_desc2 = reg.docto_desc3
	        LET reg.docto_desc3 = reg.docto_desc4
	        LET reg.docto_desc4 = reg.docto_desc5
	        LET reg.docto_desc5 = reg.docto_desc6
	        LET reg.docto_desc6 = reg.docto_desc7
	        LET reg.docto_desc7 = reg.docto_desc8
	        LET reg.docto_desc8 = ""
	        DISPLAY reg.docto_cod2  TO docto_cod2
	        DISPLAY reg.docto_cod3  TO docto_cod3
	        DISPLAY reg.docto_cod4  TO docto_cod4
	        DISPLAY reg.docto_cod5  TO docto_cod5
	        DISPLAY reg.docto_cod6  TO docto_cod6
	        DISPLAY reg.docto_cod7  TO docto_cod7
	        DISPLAY reg.docto_cod8  TO docto_cod8
	        DISPLAY reg.docto_desc2 TO docto_desc2
	        DISPLAY reg.docto_desc3 TO docto_desc3
	        DISPLAY reg.docto_desc4 TO docto_desc4
	        DISPLAY reg.docto_desc5 TO docto_desc5
	        DISPLAY reg.docto_desc6 TO docto_desc6
	        DISPLAY reg.docto_desc7 TO docto_desc7
	        DISPLAY reg.docto_desc8 TO docto_desc8
	       NEXT FIELD nss
	    END IF

	AFTER FIELD docto_cod3
	    IF reg.docto_cod3 >= 0 THEN
	       LET wk_docto = ""
	       LET wk_docto = "400", reg.docto_cod3
               SELECT docto_desc
               INTO   reg.docto_desc3
               FROM   tab_documento
               WHERE  docto_cod = wk_docto

	       IF STATUS = NOTFOUND THEN
		  IF   reg.docto_cod3 <> 0 THEN
	               ERROR "NO EXISTE ESTE TIPO DE DOCUMENTO        "
                  END IF
	          CALL despliega_documentos()
	  	  LET  reg.docto_desc3 = wk_docto_desc
		  LET  reg.docto_cod3  = wk_docto_cod
               END IF
	       DISPLAY reg.docto_desc3 TO docto_desc3
	       DISPLAY reg.docto_cod3  TO docto_cod3  
	       IF reg.docto_cod3 = reg.docto_cod1  OR 
	          reg.docto_cod3 = reg.docto_cod2 THEN
		  ERROR "NO SE RACEPTAN CODIGOS REPETIDOS        "
		  NEXT FIELD docto_cod3
	       END IF
            ELSE
	        LET reg.docto_cod3  = reg.docto_cod4
	        LET reg.docto_cod4  = reg.docto_cod5
	        LET reg.docto_cod5  = reg.docto_cod6
	        LET reg.docto_cod6  = reg.docto_cod7
	        LET reg.docto_cod7  = reg.docto_cod8
	        LET reg.docto_cod8  = ""
	        LET reg.docto_desc3 = reg.docto_desc4
	        LET reg.docto_desc4 = reg.docto_desc5
	        LET reg.docto_desc5 = reg.docto_desc6
	        LET reg.docto_desc6 = reg.docto_desc7
	        LET reg.docto_desc7 = reg.docto_desc8
	        LET reg.docto_desc8 = ""
	        DISPLAY reg.docto_cod3  TO docto_cod3
	        DISPLAY reg.docto_cod4  TO docto_cod4
	        DISPLAY reg.docto_cod5  TO docto_cod5
	        DISPLAY reg.docto_cod6  TO docto_cod6
	        DISPLAY reg.docto_cod7  TO docto_cod7
	        DISPLAY reg.docto_cod8  TO docto_cod8
	        DISPLAY reg.docto_desc3 TO docto_desc3
	        DISPLAY reg.docto_desc4 TO docto_desc4
	        DISPLAY reg.docto_desc5 TO docto_desc5
	        DISPLAY reg.docto_desc6 TO docto_desc6
	        DISPLAY reg.docto_desc7 TO docto_desc7
	        DISPLAY reg.docto_desc8 TO docto_desc8
	       NEXT FIELD nss
            END IF

	AFTER FIELD docto_cod4
	    IF reg.docto_cod4 >= 0 THEN
	       LET wk_docto = ""
	       LET wk_docto = "400", reg.docto_cod4
               SELECT docto_desc
               INTO   reg.docto_desc4
               FROM   tab_documento
               WHERE  docto_cod = wk_docto

	       IF STATUS = NOTFOUND THEN
		  IF   reg.docto_cod4 <> 0 THEN
	               ERROR "NO EXISTE ESTE TIPO DE DOCUMENTO        "
                  END IF
	          CALL despliega_documentos()
	  	  LET  reg.docto_desc4 = wk_docto_desc
		  LET  reg.docto_cod4  = wk_docto_cod
               END IF
	       DISPLAY reg.docto_desc4 TO docto_desc4
	       DISPLAY reg.docto_cod4  TO docto_cod4  
	       IF reg.docto_cod4 = reg.docto_cod1  OR 
	          reg.docto_cod4 = reg.docto_cod2  OR 
	          reg.docto_cod4 = reg.docto_cod3 THEN
		  ERROR "NO SE RACEPTAN CODIGOS REPETIDOS        "
		  NEXT FIELD docto_cod4
	       END IF
            ELSE
	        LET reg.docto_cod4  = reg.docto_cod5
	        LET reg.docto_cod5  = reg.docto_cod6
	        LET reg.docto_cod6  = reg.docto_cod7
	        LET reg.docto_cod7  = reg.docto_cod8
	        LET reg.docto_cod8  = ""
	        LET reg.docto_desc4 = reg.docto_desc5
	        LET reg.docto_desc5 = reg.docto_desc6
	        LET reg.docto_desc6 = reg.docto_desc7
	        LET reg.docto_desc7 = reg.docto_desc8
	        LET reg.docto_desc8 = ""
	        DISPLAY reg.docto_cod4  TO docto_cod4
	        DISPLAY reg.docto_cod5  TO docto_cod5
	        DISPLAY reg.docto_cod6  TO docto_cod6
	        DISPLAY reg.docto_cod7  TO docto_cod7
	        DISPLAY reg.docto_cod8  TO docto_cod8
	        DISPLAY reg.docto_desc4 TO docto_desc4
	        DISPLAY reg.docto_desc5 TO docto_desc5
	        DISPLAY reg.docto_desc6 TO docto_desc6
	        DISPLAY reg.docto_desc7 TO docto_desc7
	        DISPLAY reg.docto_desc8 TO docto_desc8
	       NEXT FIELD nss
            END IF

	AFTER FIELD docto_cod5
	    IF reg.docto_cod5 >= 0 THEN
	       LET wk_docto = ""
	       LET wk_docto = "400", reg.docto_cod5
               SELECT docto_desc
               INTO   reg.docto_desc5
               FROM   tab_documento
               WHERE  docto_cod = wk_docto

	       IF STATUS = NOTFOUND THEN
		  IF   reg.docto_cod5 <> 0 THEN
	               ERROR "NO EXISTE ESTE TIPO DE DOCUMENTO        "
                  END IF
	          CALL despliega_documentos()
	  	  LET  reg.docto_desc5 = wk_docto_desc
		  LET  reg.docto_cod5  = wk_docto_cod
               END IF
	       DISPLAY reg.docto_desc5 TO docto_desc5
	       DISPLAY reg.docto_cod5  TO docto_cod5  
	       IF reg.docto_cod5 = reg.docto_cod1  OR 
	          reg.docto_cod5 = reg.docto_cod2  OR 
	          reg.docto_cod5 = reg.docto_cod2  OR 
	          reg.docto_cod5 = reg.docto_cod4 THEN
		  ERROR "NO SE RACEPTAN CODIGOS REPETIDOS        "
		  NEXT FIELD docto_cod5
	       END IF
            ELSE
	        LET reg.docto_cod5  = reg.docto_cod6
	        LET reg.docto_cod6  = reg.docto_cod7
	        LET reg.docto_cod7  = reg.docto_cod8
	        LET reg.docto_cod8  = ""
	        LET reg.docto_desc5 = reg.docto_desc6
	        LET reg.docto_desc6 = reg.docto_desc7
	        LET reg.docto_desc7 = reg.docto_desc8
	        LET reg.docto_desc8 = ""
	        DISPLAY reg.docto_cod5  TO docto_cod5
	        DISPLAY reg.docto_cod6  TO docto_cod6
	        DISPLAY reg.docto_cod7  TO docto_cod7
	        DISPLAY reg.docto_cod8  TO docto_cod8
	        DISPLAY reg.docto_desc5 TO docto_desc5
	        DISPLAY reg.docto_desc6 TO docto_desc6
	        DISPLAY reg.docto_desc7 TO docto_desc7
	        DISPLAY reg.docto_desc8 TO docto_desc8
	       NEXT FIELD nss
            END IF

	AFTER FIELD docto_cod6
	    IF reg.docto_cod6 >= 0 THEN
	       LET wk_docto = ""
	       LET wk_docto = "400", reg.docto_cod6
               SELECT docto_desc
               INTO   reg.docto_desc6
               FROM   tab_documento
               WHERE  docto_cod = wk_docto

	       IF STATUS = NOTFOUND THEN
		  IF   reg.docto_cod6 <> 0 THEN
	               ERROR "NO EXISTE ESTE TIPO DE DOCUMENTO        "
                  END IF
	          CALL despliega_documentos()
	  	  LET  reg.docto_desc6 = wk_docto_desc
		  LET  reg.docto_cod6  = wk_docto_cod
               END IF
	       DISPLAY reg.docto_desc6 TO docto_desc6
	       DISPLAY reg.docto_cod6  TO docto_cod6 
	       IF reg.docto_cod6 = reg.docto_cod1  OR 
	          reg.docto_cod6 = reg.docto_cod2  OR 
	          reg.docto_cod6 = reg.docto_cod3  OR 
	          reg.docto_cod6 = reg.docto_cod4  OR 
	          reg.docto_cod6 = reg.docto_cod5 THEN
		  ERROR "NO SE RACEPTAN CODIGOS REPETIDOS        "
		  NEXT FIELD docto_cod6
	       END IF
            ELSE
	        LET reg.docto_cod6  = reg.docto_cod7
	        LET reg.docto_cod7  = reg.docto_cod8
	        LET reg.docto_cod8  = ""
	        LET reg.docto_desc6 = reg.docto_desc7
	        LET reg.docto_desc7 = reg.docto_desc8
	        LET reg.docto_desc8 = ""
	        DISPLAY reg.docto_cod6  TO docto_cod6
	        DISPLAY reg.docto_cod7  TO docto_cod7
	        DISPLAY reg.docto_cod8  TO docto_cod8
	        DISPLAY reg.docto_desc6 TO docto_desc6
	        DISPLAY reg.docto_desc7 TO docto_desc7
	        DISPLAY reg.docto_desc8 TO docto_desc8
	       NEXT FIELD nss
            END IF

	AFTER FIELD docto_cod7
	    IF reg.docto_cod7 >= 0 THEN
	       LET wk_docto = ""
	       LET wk_docto = "400", reg.docto_cod7
               SELECT docto_desc
               INTO   reg.docto_desc7
               FROM   tab_documento
               WHERE  docto_cod = wk_docto

	       IF STATUS = NOTFOUND THEN
		  IF   reg.docto_cod7 <> 0 THEN
	               ERROR "NO EXISTE ESTE TIPO DE DOCUMENTO        "
                  END IF
	          CALL despliega_documentos()
	  	  LET  reg.docto_desc7 = wk_docto_desc
		  LET  reg.docto_cod7  = wk_docto_cod
               END IF
	       DISPLAY reg.docto_desc7 TO docto_desc7
	       DISPLAY reg.docto_cod7  TO docto_cod7 
	       IF reg.docto_cod7 = reg.docto_cod1  OR 
	          reg.docto_cod7 = reg.docto_cod2  OR 
	          reg.docto_cod7 = reg.docto_cod3  OR 
	          reg.docto_cod7 = reg.docto_cod4  OR 
	          reg.docto_cod7 = reg.docto_cod5  OR 
	          reg.docto_cod7 = reg.docto_cod6 THEN
		  ERROR "NO SE RACEPTAN CODIGOS REPETIDOS        "
		  NEXT FIELD docto_cod7
	       END IF
            ELSE
	        LET reg.docto_cod7  = reg.docto_cod8
	        LET reg.docto_cod8  = ""
	        LET reg.docto_desc7 = reg.docto_desc8
	        LET reg.docto_desc8 = ""
	        DISPLAY reg.docto_cod7  TO docto_cod7
	        DISPLAY reg.docto_cod8  TO docto_cod8
	        DISPLAY reg.docto_desc7 TO docto_desc7
	        DISPLAY reg.docto_desc8 TO docto_desc8
	       NEXT FIELD nss
            END IF

	AFTER FIELD docto_cod8
	    IF reg.docto_cod8 >= 0 THEN
	       LET wk_docto = ""
	       LET wk_docto = "400", reg.docto_cod8
               SELECT docto_desc
               INTO   reg.docto_desc8
               FROM   tab_documento
               WHERE  docto_cod = wk_docto

	       IF STATUS = NOTFOUND THEN
		  IF   reg.docto_cod8 <> 0 THEN
	               ERROR "NO EXISTE ESTE TIPO DE DOCUMENTO        "
                  END IF
	          CALL despliega_documentos()
	  	  LET  reg.docto_desc8 = wk_docto_desc
		  LET  reg.docto_cod8  = wk_docto_cod
               END IF
	       DISPLAY reg.docto_desc8 TO docto_desc8
	       DISPLAY reg.docto_cod8  TO docto_cod8 
	       IF reg.docto_cod8 = reg.docto_cod1  OR 
	          reg.docto_cod8 = reg.docto_cod2  OR 
	          reg.docto_cod8 = reg.docto_cod3  OR 
	          reg.docto_cod8 = reg.docto_cod4  OR 
	          reg.docto_cod8 = reg.docto_cod5  OR 
	          reg.docto_cod8 = reg.docto_cod6  OR 
	          reg.docto_cod8 = reg.docto_cod7 THEN
		  ERROR "NO SE RACEPTAN CODIGOS REPETIDOS        "
		  NEXT FIELD docto_cod8
	       END IF
            ELSE
	        LET reg.docto_cod8  = ""
	        LET reg.docto_desc8 = ""
	        DISPLAY reg.docto_cod8  TO docto_cod8
	        DISPLAY reg.docto_desc8 TO docto_desc8
	       NEXT FIELD nss
            END IF

        ON KEY (ESC)
	    LET opc = ""
	    CALL pregunta_cambio() RETURNING opc
	    IF  opc MATCHES "[Ss]" THEN
                UPDATE  ret_rechazo_interno
	            SET    
                      tipo_seguro       = reg.tipo_seguro       ,
                      tipo_pension      = reg.tipo_pension      ,
                      tipo_prestacion   = reg.tipo_prestacion   ,
                      fecha_solicitud   = reg.fecha_solicitud   ,
                      fecha_emision     = reg.fecha_emision     ,
                      importe           = reg.importe           ,
                      devolucion_doctos = reg.devolucion_doctos ,
                      codigo1           = reg.codigo1           ,
                      codigo2           = reg.codigo2           ,
                      codigo3           = reg.codigo3           ,
                      codigo4           = reg.codigo4           ,
                      docto_cod1        = reg.docto_cod1        ,
                      docto_cod2        = reg.docto_cod2        ,
                      docto_cod3        = reg.docto_cod3        ,
                      docto_cod4        = reg.docto_cod4        ,
                      docto_cod5        = reg.docto_cod5        ,
                      docto_cod6        = reg.docto_cod6        ,
                      docto_cod7        = reg.docto_cod7        ,
                      docto_cod8        = reg.docto_cod8
                WHERE nss        = wk_nss
                AND   consec_rch = wk_consec_rch

                DISPLAY 
	 	    "REGISTRO ACTUALIZADO ","" AT 20,1 ATTRIBUTE(REVERSE) 
		SLEEP 3
            END IF

            CALL inicializa() #i
            DISPLAY "                   ","" AT 20,1
    END INPUT

END FUNCTION

FUNCTION elimina()
--------------------------------
    DEFINE #loc #integer
        arr_c                 ,
        i                     INTEGER

    DISPLAY " RETM006                      RECHAZOS   INTERNOS                              " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,66 ATTRIBUTE(REVERSE)
    
    DISPLAY "ELIMINA " AT 1,66 ATTRIBUTE(REVERSE)
    DISPLAY " Ctrl-C : Salir                                            ESC : Elimina" AT 2,1 

    INITIALIZE reg.* TO NULL
    CLEAR FORM

    LET reg.fecha_emision = HOY
    LET flag              = 0
    CONSTRUCT BY NAME x_busca ON A.nss            ,
                                 A.paterno        ,
                                 A.materno        ,
                                 A.nombres        ,
                                 A.tipo_seguro    ,
                                 A.tipo_pension   ,
                                 A.tipo_prestacion
    
        ON KEY (CONTROL-C) 
            IF flag = 0 THEN    
                LET flag=1     
                EXIT CONSTRUCT 
            END IF 

        ON KEY (INTERRUPT) 
            IF flag = 0 THEN    
                LET flag=1     
                EXIT CONSTRUCT 
            END IF 

        ON KEY (Esc)
            LET INT_FLAG = FALSE
            EXIT CONSTRUCT

    END CONSTRUCT

    IF  flag = 1 THEN 
        RETURN
    END IF

    LET txt_1 =" SELECT a.nss                ,",
                      " a.consec_rch         ,",
                      " a.tipo_seguro        ,",
                      " a.tipo_pension       ,",
                      " a.tipo_prestacion    ,",
                      " a.importe            ,",
                      " a.paterno            ,",
                      " a.materno            ,",
                      " a.nombres            ,",
                      " a.fecha_solicitud    ,",
                      " a.fecha_emision      ,",
                      " a.codigo1            ,",
                      " a.codigo2            ,",
                      " a.codigo3            ,",
                      " a.codigo4            ,",
                      " a.devolucion_doctos  ,",
                      " a.estado             ,",
                      " a.docto_cod1         ,",
                      " a.docto_cod2         ,",
                      " a.docto_cod3         ,",
                      " a.docto_cod4         ,",
                      " a.docto_cod5         ,",
                      " a.docto_cod6         ,",
                      " a.docto_cod7         ,",
                      " a.docto_cod8          ",
               " FROM   ret_rechazo_interno a ",
               " WHERE  ",x_busca CLIPPED

    PREPARE pre_8 FROM txt_1
    DECLARE cur_8 CURSOR FOR pre_8

    LET i = 1
    FOREACH cur_8 INTO arr_2[i].*

        LET arr_1[i].nss               = arr_2[i].nss
        LET arr_1[i].tipo_seguro       = arr_2[i].tipo_seguro
        LET arr_1[i].tipo_pension      = arr_2[i].tipo_pension
        LET arr_1[i].tipo_prestacion   = arr_2[i].tipo_prestacion
        LET arr_1[i].importe           = arr_2[i].importe
        LET arr_1[i].paterno           = arr_2[i].paterno
        LET arr_1[i].materno           = arr_2[i].materno
        LET arr_1[i].nombres           = arr_2[i].nombres
        LET arr_1[i].fecha_solicitud   = arr_2[i].fecha_solicitud
        LET arr_1[i].fecha_emision     = arr_2[i].fecha_emision
        LET arr_1[i].codigo1           = arr_2[i].codigo1
        LET arr_1[i].codigo2           = arr_2[i].codigo2
        LET arr_1[i].codigo3           = arr_2[i].codigo3
        LET arr_1[i].codigo4           = arr_2[i].codigo4
        LET arr_1[i].devolucion_doctos = arr_2[i].devolucion_doctos
	IF  arr_2[i].estado = 0 THEN
            LET arr_1[i].estado  =  "RECHAZADO"
        ELSE
            LET arr_1[i].estado  =  "ENVIADO  "
	END IF
        LET arr_1[i].docto_cod1        = arr_2[i].docto_cod1
        LET arr_1[i].docto_cod2        = arr_2[i].docto_cod2
        LET arr_1[i].docto_cod3        = arr_2[i].docto_cod3
        LET arr_1[i].docto_cod4        = arr_2[i].docto_cod4
        LET arr_1[i].docto_cod5        = arr_2[i].docto_cod5
        LET arr_1[i].docto_cod6        = arr_2[i].docto_cod6
        LET arr_1[i].docto_cod7        = arr_2[i].docto_cod7
        LET arr_1[i].docto_cod8        = arr_2[i].docto_cod8
    
	SELECT descripcion
        INTO   arr_1[i].desc_tipo_seguro
        FROM   tab_seguro
        WHERE  clave = arr_1[i].tipo_seguro

        SELECT descripcion
        INTO   arr_1[i].desc_tipo_pension
        FROM   tab_pension
        WHERE  codigo = arr_1[i].tipo_pension

        SELECT descripcion
        INTO   arr_1[i].desc_tipo_prestacion
        FROM   tab_prestacion
        WHERE  codigo = arr_1[i].tipo_prestacion

	SELECT desc_corta
        INTO   arr_1[i].desc_codigo1
        FROM   ret_txt_rch_interno
        WHERE  cod_rechazo = arr_1[i].codigo1

        IF arr_1[i].codigo2 > 0 THEN	
	   SELECT desc_corta
           INTO   arr_1[i].desc_codigo2 
           FROM   ret_txt_rch_interno
           WHERE  cod_rechazo = arr_1[i].codigo2
	END IF

        IF arr_1[i].codigo3 > 0 THEN	
	   SELECT desc_corta
           INTO   arr_1[i].desc_codigo3 
           FROM   ret_txt_rch_interno
           WHERE  cod_rechazo = arr_1[i].codigo3
	END IF

        IF arr_1[i].codigo4 > 0 THEN	
	   SELECT desc_corta
           INTO   arr_1[i].desc_codigo4 
           FROM   ret_txt_rch_interno
           WHERE  cod_rechazo = arr_1[i].codigo4
	END IF

        IF arr_1[i].docto_cod1 > 0  THEN	
	   LET wk_docto = ""
	   LET wk_docto = "400", arr_1[i].docto_cod1
	   SELECT docto_desc
           INTO   arr_1[i].docto_desc1
           FROM   tab_documento
           WHERE  docto_cod = wk_docto
	END IF

        IF arr_1[i].docto_cod2 > 0 THEN	
	   LET wk_docto = ""
	   LET wk_docto = "400", arr_1[i].docto_cod2
	   SELECT docto_desc
           INTO   arr_1[i].docto_desc2
           FROM   tab_documento
           WHERE  docto_cod = wk_docto
	END IF

        IF arr_1[i].docto_cod3 > 0 THEN	
	   LET wk_docto = ""
	   LET wk_docto = "400", arr_1[i].docto_cod3
	   SELECT docto_desc
           INTO   arr_1[i].docto_desc3
           FROM   tab_documento
           WHERE  docto_cod = wk_docto
	END IF

        IF arr_1[i].docto_cod4 > 0 THEN	
	   LET wk_docto = ""
	   LET wk_docto = "400", arr_1[i].docto_cod4
	   SELECT docto_desc
           INTO   arr_1[i].docto_desc4
           FROM   tab_documento
           WHERE  docto_cod = wk_docto
	END IF

        IF arr_1[i].docto_cod5 > 0 THEN	
	   LET wk_docto = ""
	   LET wk_docto = "400", arr_1[i].docto_cod5
	   SELECT docto_desc
           INTO   arr_1[i].docto_desc5
           FROM   tab_documento
           WHERE  docto_cod = wk_docto
	END IF

        IF arr_1[i].docto_cod6 > 0 THEN	
	   LET wk_docto = ""
	   LET wk_docto = "400", arr_1[i].docto_cod6
	   SELECT docto_desc
           INTO   arr_1[i].docto_desc6
           FROM   tab_documento
           WHERE  docto_cod = wk_docto
	END IF

        IF arr_1[i].docto_cod7 > 0 THEN	
	   LET wk_docto = ""
	   LET wk_docto = "400", arr_1[i].docto_cod7
	   SELECT docto_desc
           INTO   arr_1[i].docto_desc7
           FROM   tab_documento
           WHERE  docto_cod = wk_docto
	END IF

        IF arr_1[i].docto_cod8 > 0 THEN	
	   LET wk_docto = ""
	   LET wk_docto = "400", arr_1[i].docto_cod8
	   SELECT docto_desc
           INTO   arr_1[i].docto_desc8
           FROM   tab_documento
           WHERE  docto_cod = wk_docto
	END IF

        LET i = i + 1
    END FOREACH

    IF i = 1 THEN
        INITIALIZE reg.* TO NULL
        CLEAR FORM
        ERROR " NO EXISTE REGISTRO " 
	SLEEP 3
        ERROR "                    " 
        RETURN
    END IF

    CALL SET_COUNT(i-1)
    DISPLAY 
    " Ctrl-C : Salir     Ctrl-B : Beneficiarios                 ESC : Elimina "
      AT 2,1 
    DISPLAY ARRAY arr_1 TO scr_1.*
	  
        ON KEY ( CONTROL-B )
           LET pos = ARR_CURR()
	   IF  arr_2[pos].estado = 1 THEN
	       ERROR " NO SE PUEDEN MODIFICAR REGISTROS YA ENVIADOS "
	       SLEEP 3
	       ERROR "                                              "
	       LET flag = 1
	       LET beneficiarios = 0
	   ELSE  
	       LET beneficiarios = 1
	   END IF
	   EXIT DISPLAY

        ON KEY ( ESC )
           LET pos = ARR_CURR()
	   IF  arr_2[pos].estado = 1 THEN
	       ERROR " NO SE PUEDEN BORRAR  REGISTROS YA ENVIADOS "
	       SLEEP 3
	       ERROR "                                            "
	       LET flag = 1
	   END IF
	    LET opc = ""
	    CALL pregunta_baja() RETURNING opc
	    IF  opc MATCHES "[Ss]" THEN
                LET pos = ARR_CURR()
                LET wk_nss        = arr_2[pos].nss
                LET wk_consec_rch = arr_2[pos].consec_rch

                DELETE 
		FROM ret_rch_int_benef
		WHERE  nss        = wk_nss    
		AND    consec_rch = wk_consec_rch

                DELETE 
		FROM ret_rechazo_interno
		WHERE  nss        = wk_nss    
		AND    consec_rch = wk_consec_rch

		ERROR " REGISTRO ELIMINADO CON TODO Y SUS BENEFICIARIOS"
		SLEEP 3
		CLEAR FORM
		LET beneficiarios = 0
            END IF
	    EXIT DISPLAY
	  
        ON KEY ( CONTROL-C )
            CALL inicializa()
	    EXIT DISPLAY

        ON KEY ( INTERRUPT )
            CALL inicializa()
	    EXIT DISPLAY
    END DISPLAY
    IF beneficiarios = 1 THEN
       LET pos = ARR_CURR()
       LET wk_nss        = arr_2[pos].nss
       LET wk_consec_rch = arr_2[pos].consec_rch
       CALL elimina_bene()
    END IF
     
END FUNCTION

FUNCTION agrega_bene()
    DEFINE wk_tipo_seguro      CHAR(02)
    DEFINE wk_tipo_pension     CHAR(02)

    LET idx1 = 0

    OPEN WINDOW retm0062 AT 6,15 WITH FORM "RETM0062" ATTRIBUTE(BORDER)
    DISPLAY "                      BENEFICIARIOS                      " 
	    AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY " ESC : Agrega                            Ctrl-C : Salir  "
	    AT 2,1 

    SELECT calle              , 
	   numero             ,
	   depto              ,
	   colonia            ,
	   delega             ,
	   ciudad             ,
	   estado             ,
	   codpos 
    INTO   regb.calle         ,
	   regb.numero        ,
	   regb.depto         ,
	   regb.colonia       ,
	   regb.delegacion    ,
	   regb.ciudad        ,
	   regb.estado        ,
	   regb.codpos       
    FROM   afi_domicilio 
    WHERE  afi_domicilio.nss  =  reg.nss

    INPUT BY NAME 
          regb.paterno_ben           ,
          regb.materno_ben           ,
          regb.nombres_ben           ,
          regb.calle                 ,
          regb.numero                ,
          regb.depto                 ,
          regb.colonia               ,
          regb.codpos                ,
          regb.estado                ,
          regb.ciudad                ,
          regb.delegacion            ,
          regb.telefono            
          WITHOUT DEFAULTS  

	AFTER FIELD paterno_ben
           IF regb.paterno_ben IS NULL   OR
	      regb.paterno_ben =  " "   THEN
	      ERROR "FALTA EL APELLIDO PATERNO       "
	      NEXT FIELD paterno_ben
           END IF

	AFTER FIELD nombres_ben
           IF regb.nombres_ben IS NULL   OR
	      regb.nombres_ben =  " "   THEN
	      ERROR "FALTAN LOS NOMBRES              "
	      NEXT FIELD nombres_ben
           END IF

	AFTER FIELD calle
           IF regb.calle IS NULL   OR
	      regb.calle =  " "   THEN
	      ERROR "FALTA LA CALLE                  "
	      NEXT FIELD calle
           END IF

	AFTER FIELD codpos
            SELECT estad_cod  ,
		   ciudad_cod ,
		   deleg_cod
            INTO   regb.estado ,
		   regb.ciudad ,
		   regb.delegacion
            FROM   tab_codpos
            WHERE  cpos_cod = regb.codpos

	    IF STATUS = NOTFOUND THEN
	       ERROR "NO EXISTE ESTE CODIGO POSTAL            "
	       NEXT FIELD codpos
            ELSE
	       DISPLAY regb.estado TO estado
               SELECT estad_desc
               INTO   regb.desc_estado
               FROM   tab_estado
               WHERE  estad_cod = regb.estado

	       IF STATUS = NOTFOUND THEN
	          ERROR "NO EXISTE ESTE ESATDO                   "
	          NEXT FIELD estado
               ELSE
	          DISPLAY regb.desc_estado TO desc_estado
               END IF

	       DISPLAY regb.ciudad TO ciudad
               SELECT ciudad_desc
               INTO   regb.desc_ciudad
               FROM   tab_ciudad
               WHERE  ciudad_cod = regb.ciudad

	       IF STATUS = NOTFOUND THEN
	          ERROR "NO EXISTE ESTA CIUDAD EN ESTE ESTADO    "
	          NEXT FIELD ciudad
               ELSE
	          DISPLAY regb.desc_ciudad TO desc_ciudad
               END IF

	       DISPLAY regb.delegacion TO delegacion
               SELECT deleg_desc
               INTO   regb.desc_delegacion
               FROM   tab_delegacion
               WHERE  deleg_cod = regb.delegacion

	       IF STATUS = NOTFOUND THEN
	          ERROR "NO EXISTE ESTA DELEGACION EN ESTA CIUDAD"
	          NEXT FIELD delegacion
               ELSE
	          DISPLAY regb.desc_delegacion TO desc_delegacion
               END IF
            END IF

        ON KEY (ESC)
            DISPLAY " REGISTRO PREPARADO PARA SER DADO DE ALTA " AT 16,1
		    ATTRIBUTE(REVERSE)
	    SLEEP 3
            DISPLAY "                                          " AT 16,1
            LET idx1 = idx1 + 1
	    LET arrb_ins[idx1].nss         = reg.nss
	    LET arrb_ins[idx1].consec_rch  = 0     
	    LET arrb_ins[idx1].paterno_ben = regb.paterno_ben
	    LET arrb_ins[idx1].materno_ben = regb.materno_ben
	    LET arrb_ins[idx1].nombres_ben = regb.nombres_ben
	    LET arrb_ins[idx1].calle       = regb.calle
	    LET arrb_ins[idx1].numero      = regb.numero
	    LET arrb_ins[idx1].depto       = regb.depto
	    LET arrb_ins[idx1].colonia     = regb.colonia
	    LET arrb_ins[idx1].ciudad      = regb.ciudad
	    LET arrb_ins[idx1].estado      = regb.estado
	    LET arrb_ins[idx1].delegacion  = regb.delegacion
	    LET arrb_ins[idx1].codpos      = regb.codpos
	    LET arrb_ins[idx1].telefono    = regb.telefono
            LET   regb.paterno_ben = ""
            LET   regb.materno_ben = ""
            LET   regb.nombres_ben = ""
	    DISPLAY regb.paterno_ben TO paterno_ben
	    DISPLAY regb.materno_ben TO materno_ben
	    DISPLAY regb.nombres_ben TO nombres_ben
            NEXT FIELD paterno_ben

        ON KEY(CONTROL-C)
	    IF idx1 = 0 THEN
	       DISPLAY "SE REQUIEREN BENEFICIARIOS, NO SE GRABA" AT 16,1
	       SLEEP 3
	       DISPLAY "                                       " AT 16,1
	       LET sw_2 = 0
	    END IF
            EXIT INPUT

        ON KEY(INTERRUPT)
	    IF idx1 = 0 THEN
	       DISPLAY "SE REQUIEREN BENEFICIARIOS, NO SE GRABA" AT 16,1
	       SLEEP 3
	       DISPLAY "                                       " AT 16,1
	       LET sw_2 = 0
	    END IF
            EXIT INPUT
    END INPUT
    CLEAR FORM   
    CLOSE WINDOW retm0062

END FUNCTION
 
FUNCTION consulta_bene()
#c-----------------

    DEFINE #loc #integer
        arr_c                 ,
        i                     INTEGER

    OPEN WINDOW retm0062 AT 6,15 WITH FORM "RETM0062" ATTRIBUTE(BORDER)
    DISPLAY "                      BENEFICIARIOS                      " 
	    AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY " Ctrl-C : Salir                                          "
	    AT 2,1 

    INITIALIZE reg.* TO NULL
    CLEAR FORM

    LET reg.fecha_emision = HOY
    
    DECLARE cur_9 CURSOR FOR 
    SELECT *
    FROM   ret_rch_int_benef
    WHERE  ret_rch_int_benef.nss        = wk_nss
    AND    ret_rch_int_benef.consec_rch = wk_consec_rch

    LET i = 1
    FOREACH cur_9 INTO arrb_2[i].*

        LET arrb_1[i].paterno_ben  = arrb_2[i].paterno_ben
	LET arrb_1[i].materno_ben  = arrb_2[i].materno_ben
	LET arrb_1[i].nombres_ben  = arrb_2[i].nombres_ben
        LET arrb_1[i].calle        = arrb_2[i].calle
        LET arrb_1[i].numero       = arrb_2[i].numero
        LET arrb_1[i].depto        = arrb_2[i].depto
        LET arrb_1[i].colonia      = arrb_2[i].colonia
        LET arrb_1[i].ciudad       = arrb_2[i].ciudad
        LET arrb_1[i].estado       = arrb_2[i].estado
        LET arrb_1[i].delegacion   = arrb_2[i].delegacion
        LET arrb_1[i].codpos       = arrb_2[i].codpos
        LET arrb_1[i].telefono     = arrb_2[i].telefono

        SELECT estad_desc 
        INTO   arrb_1[i].desc_estado
        FROM   tab_estado
        WHERE  estad_cod = arrb_1[i].estado 

        SELECT ciudad_desc
        INTO   arrb_1[i].desc_ciudad
        FROM   tab_ciudad
        WHERE  ciudad_cod = arrb_1[i].ciudad

        SELECT deleg_desc
        INTO   arrb_1[i].desc_delegacion
        FROM   tab_delegacion
        WHERE  deleg_cod = arrb_1[i].delegacion

	LET i = i + 1
    END FOREACH

    IF  i = 1 THEN
        INITIALIZE reg.* TO NULL
        CLEAR FORM
        ERROR " NO EXISTE REGISTRO " 
	SLEEP 3
        ERROR "                    " 
	RETURN
    END IF
    
    CALL SET_COUNT(i-1)

    DISPLAY ARRAY arrb_1 TO scr_1.*
	  
        ON KEY ( CONTROL-C )
           CALL inicializa()
           EXIT DISPLAY

        ON KEY ( INTERRUPT )
           CALL inicializa()
           EXIT DISPLAY
    END DISPLAY

    CLOSE WINDOW retm0062

END FUNCTION
 
FUNCTION modifica_bene()
#m-----------------
    DEFINE #loc #smallint
        arr_c                 ,
        i                     INTEGER

    OPEN WINDOW retm0062 AT 6,15 WITH FORM "RETM0062" ATTRIBUTE(BORDER)
    DISPLAY "                      BENEFICIARIOS                      " 
	    AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY " Ctrl-C : Salir                           ESC : Modifica "
	    AT 2,1 

    INITIALIZE reg.* TO NULL
    CLEAR FORM

    LET reg.fecha_emision = HOY
    
    DECLARE cur_10 CURSOR FOR 
    SELECT *
    FROM   ret_rch_int_benef
    WHERE  ret_rch_int_benef.nss        = wk_nss
    AND    ret_rch_int_benef.consec_rch = wk_consec_rch

    LET i = 1
    FOREACH cur_10 INTO arrb_2[i].*

        LET arrb_1[i].paterno_ben  = arrb_2[i].paterno_ben
	LET arrb_1[i].materno_ben  = arrb_2[i].materno_ben
	LET arrb_1[i].nombres_ben  = arrb_2[i].nombres_ben
        LET arrb_1[i].calle        = arrb_2[i].calle
        LET arrb_1[i].numero       = arrb_2[i].numero
        LET arrb_1[i].depto        = arrb_2[i].depto
        LET arrb_1[i].colonia      = arrb_2[i].colonia
        LET arrb_1[i].ciudad       = arrb_2[i].ciudad
        LET arrb_1[i].estado       = arrb_2[i].estado
        LET arrb_1[i].delegacion   = arrb_2[i].delegacion
        LET arrb_1[i].codpos       = arrb_2[i].codpos
        LET arrb_1[i].telefono     = arrb_2[i].telefono

        SELECT estad_desc 
        INTO   arrb_1[i].desc_estado
        FROM   tab_estado
        WHERE  estad_cod = arrb_1[i].estado 

        SELECT ciudad_desc
        INTO   arrb_1[i].desc_ciudad
        FROM   tab_ciudad
        WHERE  ciudad_cod = arrb_1[i].ciudad

        SELECT deleg_desc
        INTO   arrb_1[i].desc_delegacion
        FROM   tab_delegacion
        WHERE  deleg_cod = arrb_1[i].delegacion

	LET i = i + 1
    END FOREACH

    IF  i = 1 THEN
        INITIALIZE reg.* TO NULL
        CLEAR FORM
        ERROR " NO EXISTE REGISTRO " 
	SLEEP 3
        ERROR "                    " 
	RETURN
    END IF
    
    CALL SET_COUNT(i-1)

    DISPLAY ARRAY arrb_1 TO scr_1.*
        ON KEY ( CONTROL-C )   
           LET flag=1
           EXIT DISPLAY       
	  
        ON KEY ( INTERRUPT )   
           LET flag=1
           EXIT DISPLAY       
	  
        ON KEY ( ESC )
           LET pos = ARR_CURR()
           EXIT DISPLAY
    END DISPLAY

    IF flag = 1 THEN 
        RETURN
        LET flag=0
    ELSE  
        CALL construccion_bene()
    END IF
    CLOSE WINDOW retm0062

END FUNCTION

FUNCTION construccion_bene()
#c------------------------------------
    DEFINE #loc #smallint
        i                             ,
        s_tipo_movimiento_ant         ,
        primera_vez           SMALLINT,
        consecutivo_modifica  INTEGER ,
        opc                   CHAR(01),  
        vmodif2               INTEGER 
 
    LET regb.paterno_ben       = arrb_1[pos].paterno_ben      
    LET regb.materno_ben       = arrb_1[pos].materno_ben      
    LET regb.nombres_ben       = arrb_1[pos].nombres_ben
    LET regb.calle             = arrb_1[pos].calle               
    LET regb.numero            = arrb_1[pos].numero             
    LET regb.depto             = arrb_1[pos].depto               
    LET regb.colonia           = arrb_1[pos].colonia            
    LET regb.codpos            = arrb_1[pos].codpos              
    LET regb.estado            = arrb_1[pos].estado       
    LET regb.desc_estado       = arrb_1[pos].desc_estado         
    LET regb.ciudad            = arrb_1[pos].ciudad           
    LET regb.desc_ciudad       = arrb_1[pos].desc_ciudad      
    LET regb.delegacion        = arrb_1[pos].delegacion 
    LET regb.desc_delegacion   = arrb_1[pos].desc_delegacion  
    LET regb.telefono          = arrb_1[pos].telefono   
 
    INPUT BY NAME 
          regb.paterno_ben           ,
          regb.materno_ben           ,
          regb.nombres_ben           ,
          regb.calle                 ,
          regb.numero                ,
          regb.depto                 ,
          regb.colonia               ,
          regb.codpos                ,
          regb.estado                ,
          regb.ciudad                ,
          regb.delegacion            ,
          regb.telefono            
          WITHOUT DEFAULTS  

	AFTER FIELD codpos
            SELECT estad_cod  ,
		   ciudad_cod ,
		   deleg_cod
            INTO   regb.estado ,
		   regb.ciudad ,
		   regb.delegacion
            FROM   tab_codpos
            WHERE  cpos_cod = regb.codpos

	    IF STATUS = NOTFOUND THEN
	       ERROR "NO EXISTE ESTE CODIGO POSTAL            "
	       NEXT FIELD codpos
            ELSE
	       DISPLAY regb.estado TO estado
               SELECT estad_desc
               INTO   regb.desc_estado
               FROM   tab_estado
               WHERE  estad_cod = regb.estado

	       IF STATUS = NOTFOUND THEN
	          ERROR "NO EXISTE ESTE ESATDO                   "
	          NEXT FIELD estado
               ELSE
	          DISPLAY regb.desc_estado TO desc_estado
               END IF

	       DISPLAY regb.ciudad TO ciudad
               SELECT ciudad_desc
               INTO   regb.desc_ciudad
               FROM   tab_ciudad
               WHERE  ciudad_cod = regb.ciudad

	       IF STATUS = NOTFOUND THEN
	          ERROR "NO EXISTE ESTA CIUDAD EN ESTE ESTADO    "
	          NEXT FIELD ciudad
               ELSE
	          DISPLAY regb.desc_ciudad TO desc_ciudad
               END IF

	       DISPLAY regb.delegacion TO delegacion
               SELECT deleg_desc
               INTO   regb.desc_delegacion
               FROM   tab_delegacion
               WHERE  deleg_cod = regb.delegacion

	       IF STATUS = NOTFOUND THEN
	          ERROR "NO EXISTE ESTA DELEGACION EN ESTA CIUDAD"
	          NEXT FIELD delegacion
               ELSE
	          DISPLAY regb.desc_delegacion TO desc_delegacion
               END IF
            END IF

        ON KEY (ESC)
	    LET opc = ""
	    CALL pregunta_cambio() RETURNING opc
	    IF  opc MATCHES "[Ss]" THEN
                UPDATE ret_rch_int_benef
                   SET     
                      paterno_ben = regb.paterno_ben ,
                      materno_ben = regb.materno_ben ,
                      nombres_ben = regb.nombres_ben ,
                      calle       = regb.calle       ,
                      numero      = regb.numero      ,
                      depto       = regb.depto       ,
                      colonia     = regb.colonia     ,
                      ciudad      = regb.ciudad      ,
                      estado      = regb.estado      ,
                      delegacion  = regb.delegacion  ,
                      codpos      = regb.codpos      ,
                      telefono    = regb.telefono            
                       
                WHERE nss         = wk_nss
		AND   consec_rch  = wk_consec_rch
                AND   paterno_ben = arrb_1[pos].paterno_ben
                AND   materno_ben = arrb_1[pos].materno_ben
                AND   nombres_ben = arrb_1[pos].nombres_ben
                AND   calle       = arrb_1[pos].calle     
                AND   numero      = arrb_1[pos].numero   
                AND   depto       = arrb_1[pos].depto   
                AND   colonia     = arrb_1[pos].colonia
                AND   ciudad      = arrb_1[pos].ciudad
                AND   estado      = arrb_1[pos].estado    
                AND   delegacion  = arrb_1[pos].delegacion
                AND   codpos      = arrb_1[pos].codpos   
                AND   telefono    = arrb_1[pos].telefono            

                DISPLAY "REGISTRO ACTUALIZADO " AT 16,1
		SLEEP 3
            END IF

            CALL inicializa() #i
            DISPLAY "                   " AT 16,1
	    EXIT INPUT
    END INPUT

END FUNCTION
 
FUNCTION elimina_bene()
--------------------------------
    DEFINE #loc #integer
        arr_c                 ,
        i                     INTEGER

    OPEN WINDOW retm0062 AT 6,15 WITH FORM "RETM0062" ATTRIBUTE(BORDER)
    DISPLAY "                      BENEFICIARIOS                      " 
	    AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY " Ctrl-C : Salir                          ESC : Elimimina "
	    AT 2,1 

    INITIALIZE reg.* TO NULL
    CLEAR FORM

    LET reg.fecha_emision = HOY
    
    DECLARE cur_11 CURSOR FOR 
    SELECT *
    FROM   ret_rch_int_benef
    WHERE  ret_rch_int_benef.nss        = wk_nss
    AND    ret_rch_int_benef.consec_rch = wk_consec_rch

    LET i = 1
    FOREACH cur_11 INTO arrb_2[i].*

        LET arrb_1[i].paterno_ben  = arrb_2[i].paterno_ben
	LET arrb_1[i].materno_ben  = arrb_2[i].materno_ben
	LET arrb_1[i].nombres_ben  = arrb_2[i].nombres_ben
        LET arrb_1[i].calle        = arrb_2[i].calle
        LET arrb_1[i].numero       = arrb_2[i].numero
        LET arrb_1[i].depto        = arrb_2[i].depto
        LET arrb_1[i].colonia      = arrb_2[i].colonia
        LET arrb_1[i].ciudad       = arrb_2[i].ciudad
        LET arrb_1[i].estado       = arrb_2[i].estado
        LET arrb_1[i].delegacion   = arrb_2[i].delegacion
        LET arrb_1[i].codpos       = arrb_2[i].codpos
        LET arrb_1[i].telefono     = arrb_2[i].telefono

        SELECT estad_desc 
        INTO   arrb_1[i].desc_estado
        FROM   tab_estado
        WHERE  estad_cod = arrb_1[i].estado 

        SELECT ciudad_desc
        INTO   arrb_1[i].desc_ciudad
        FROM   tab_ciudad
        WHERE  ciudad_cod = arrb_1[i].ciudad

        SELECT deleg_desc
        INTO   arrb_1[i].desc_delegacion
        FROM   tab_delegacion
        WHERE  deleg_cod = arrb_1[i].delegacion

	LET i = i + 1
    END FOREACH

    IF  i = 1 THEN
        INITIALIZE reg.* TO NULL
        CLEAR FORM
        ERROR " NO EXISTE REGISTRO " 
	SLEEP 3
        ERROR "                    " 
	RETURN
    ELSE
	LET cuantos_ben = i - 1
    END IF
    
    CALL SET_COUNT(i-1)
     
    DISPLAY ARRAY arrb_1 TO scr_1.*

        ON KEY ( ESC )
	    LET opc = ""
	    CALL pregunta_baja() RETURNING opc
	    IF  opc MATCHES "[Ss]" THEN
		IF  cuantos_ben > 1 THEN
                    LET pos = ARR_CURR()
                    DELETE 
		    FROM  ret_rch_int_benef
		    WHERE nss         = wk_nss
		    AND   consec_rch  = wk_consec_rch
                    AND   paterno_ben = arrb_1[pos].paterno_ben
	            AND   materno_ben = arrb_1[pos].materno_ben
	            AND   nombres_ben = arrb_1[pos].nombres_ben
                    AND   calle       = arrb_1[pos].calle
                    AND   numero      = arrb_1[pos].numero
                    AND   depto       = arrb_1[pos].depto
                    AND   colonia     = arrb_1[pos].colonia
                    AND   ciudad      = arrb_1[pos].ciudad
                    AND   estado      = arrb_1[pos].estado
                    AND   delegacion  = arrb_1[pos].delegacion
                    AND   codpos      = arrb_1[pos].codpos
                    AND   telefono    = arrb_1[pos].telefono
                    DISPLAY   " BENEFICIARIO ELIMINADO   " AT 16,1
		    SLEEP 3
                    DISPLAY   "                          " At 16,1
                ELSE
		    DISPLAY "NO SE PUEDE DEJAR EL REGISTRO SIN BENEFICIARIOS"
			    AT 16,1
		    SLEEP 2
		    DISPLAY "                                               "
			    AT 16,1
		END IF
            END IF
	    EXIT DISPLAY
	  
        ON KEY ( CONTROL-C )
            CALL inicializa()
	    EXIT DISPLAY

        ON KEY ( INTERRUPT )
            CALL inicializa()
	    EXIT DISPLAY
    END DISPLAY
    CLOSE WINDOW retm0062
END FUNCTION
 
