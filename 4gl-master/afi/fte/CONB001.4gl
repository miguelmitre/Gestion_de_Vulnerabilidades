#############################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                          #
#Propietario       => E.F.P.                                                #
#Programa CONB001  => REVALORIZACION INICIAL BONO PENSION                   #
#Sistema           => CON                                                   #
#Autor             => EDUARDO JOAQUIN RESENDIZ MEDINA                       #
#Fecha             => 25 DE NOVIEMBRE DE 2008                               #
#############################################################################

DATABASE safre_af

GLOBALS

    DEFINE g_reg RECORD
        generar CHAR(1)
    END RECORD

    DEFINE w_aux RECORD
        nss              CHAR(11),
        curp             CHAR(18),
        folio            INTEGER,
        consecutivo_lote INTEGER,
        monto_pesos      DECIMAL(22,6),
        monto_udis       DECIMAL(22,6),
        fecha_reden      CHAR(08)
    END RECORD

    DEFINE g_opcion CHAR(2)

   DEFINE
       HOY                DATE     ,
       HOYDIA             DATE     ,
       fecha_redencion    DATE     ,
       dias_calculados    INTEGER  ,
       precio_udi         DECIMAL(22,6),
       monto_act_pesos_t1 DECIMAL(22,6),
       monto_act_udis_t1  DECIMAL(22,6),
       saldo_ini_udis     DECIMAL(22,6),
       saldo_ini_pesos    DECIMAL(22,6),
       saldo_ini_pesos_udis DECIMAL(22,6),
       potencia           DECIMAL(22,6),
       potencia3          DECIMAL(22,7),
       constante          DECIMAL(22,3),
       fecha_comp         CHAR(10) ,
       dia                CHAR(2)  ,
       mes                CHAR(2)  ,
       anio               CHAR(4)  ,
       num                SMALLINT ,
       HAY_REGISTROS      SMALLINT ,
       vsoli_env          SMALLINT ,
       enter              CHAR(1)  ,
       HORA               CHAR(8)  ,
       consec             CHAR(10) ,
       nom_afi            CHAR(100),
       comm               CHAR(500),
       list_salida        CHAR(500),
       vfecha_actualiza   DATE     ,
       principal          CHAR(1000)

    DEFINE x_lotes RECORD LIKE tab_lote.*
    DEFINE g_afore RECORD LIKE tab_afore_local.*
    DEFINE g_seg_modulo RECORD LIKE seg_modulo.*

    DEFINE g_usuario CHAR(8)
    DEFINE G_LISTA   CHAR(100)
    DEFINE h_corr    SMALLINT
    DEFINE vchar     CHAR(1)

    DEFINE cont_reg    SMALLINT

END GLOBALS

MAIN

    DISPLAY " "
    DISPLAY ".1"

    CALL STARTLOG('CONB001.log')
    CALL inicio() #i

    DEFER INTERRUPT
    OPTIONS INPUT WRAP,
    PROMPT LINE LAST,
    ACCEPT KEY CONTROL-I

    CALL proceso_principal() #pp
    --CALL rescata_valores()   #rv

END MAIN

FUNCTION inicio()
#i--------------

    LET HOY    = TODAY
    LET HORA   = TIME
    LET num    = 1

    LET g_reg.generar = "S"

    SELECT * 
    INTO   g_seg_modulo.* 
    FROM   seg_modulo
    WHERE  modulo_cod = 'con'

    SELECT *, USER
    INTO   g_afore.*, g_usuario
    FROM   tab_afore_local

    LET cont_reg = 0

    SELECT precio_del_dia
    INTO   precio_udi
    FROM   glo_valor_accion
    WHERE  codigo_siefore  = 13
    AND    fecha_valuacion = HOY

END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------

    OPEN WINDOW ventana_1 AT 4,4 WITH FORM "CONB0011" ATTRIBUTE(BORDER)
    DISPLAY " CONB001  REVALORIZACION INICIAL BONO PENSION (POR REGISTRO)                   " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY "                           < Ctrl-C > Salir                                    " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"dd-mm-yyyy" AT 3,65 ATTRIBUTE(REVERSE)

    --DISPLAY g_seg_modulo.ruta_envio AT 10,9 

    INPUT BY NAME g_reg.generar
        AFTER FIELD generar

        IF g_reg.generar IS NULL THEN
            ERROR "Campo NO puede ser NULO"
            NEXT FIELD generar
        END IF

        IF g_reg.generar  MATCHES "[Ss]" THEN
            IF (precio_udi IS NULL OR
                precio_udi = 0) THEN
                 ERROR "NO EXISTE PRECIO DE UDIS PARA ESTE DIA"
                 SLEEP 3
                 ERROR "PROCESO CANCELADO"
                 SLEEP 3
                 EXIT PROGRAM
            END IF

            CALL rescata_valores()

            IF NOT HAY_REGISTROS THEN
                 ERROR "NO HAY REGISTROS PARA REVALORIZAR"
                 SLEEP 3
                 EXIT PROGRAM
            END IF

            EXIT INPUT
        ELSE
            ERROR "PROCESO CANCELADO" SLEEP 2
            EXIT PROGRAM
        END IF

        ON KEY ( INTERRUPT )
            ERROR "PROCESO CANCELADO" SLEEP 2
            EXIT PROGRAM
    END INPUT

    DISPLAY "NOMBRE LISTADO:", G_LISTA CLIPPED AT 16,1
    PROMPT " /[Enter] para salir" FOR enter

END FUNCTION

FUNCTION rescata_valores()
#rv----------------------
DEFINE recha SMALLINT

    LET HAY_REGISTROS = FALSE

    LET saldo_ini_udis   = 0
    LET saldo_ini_pesos  = 0
    LET saldo_ini_pesos_udis = 0
    LET potencia         = 0
    LET potencia3        = 0

    SELECT COUNT(*) 
    INTO   HAY_REGISTROS 
    FROM   dis_cuenta
    WHERE  subcuenta       = 36
    AND    tipo_movimiento = 17

    IF HAY_REGISTROS THEN

        LET vsoli_env = HAY_REGISTROS

        DISPLAY "BONOS A PROCESAR ", vsoli_env AT 11,9

        DECLARE curs_1 CURSOR FOR
        SELECT A.nss              ,
               A.curp             ,
               A.folio            ,
               A.consecutivo_lote ,
               A.monto_en_pesos   ,
               A.monto_en_acciones,
               B.fecha_reden
        FROM   dis_cuenta A, dis_det_bono B
        WHERE  A.subcuenta        = 36
        AND    A.tipo_movimiento  = 17
        AND    A.nss              = B.n_seguro
        AND    A.curp             = B.n_unico
        AND    A.folio            = B.folio
        AND    A.consecutivo_lote = B.consec_reg_lote

        DISPLAY "Procesando Informacion"

        LET G_LISTA = g_seg_modulo.ruta_listados CLIPPED,
                      "/",g_usuario CLIPPED, "LISTADO_INI_BONO."
                      CLIPPED,HOY USING "dd-mm-yy"

        START REPORT listado_2 TO G_LISTA

        FOREACH curs_1 INTO w_aux.*

            SELECT 'X'
            FROM   sdo_ini_bono
            WHERE  folio = w_aux.folio
            AND    nss   = w_aux.nss
            AND    curp  = w_aux.curp
            AND    estado= 10

            IF SQLCA.SQLCODE = 0 THEN
                ERROR "FOLIO DUPLICADO... NO PUEDE VOLVER A GENERAR LOS SALDOS INICIALES..."
                SLEEP 3
                EXIT PROGRAM
            END IF

            LET HAY_REGISTROS = TRUE
            LET cont_reg      = cont_reg + 1

            LET dia        = w_aux.fecha_reden[7,8]
            LET mes        = w_aux.fecha_reden[5,6]
            LET anio       = w_aux.fecha_reden[1,4]
            LET fecha_comp = mes USING "&&","/",dia USING "&&","/",anio USING "&&&&"
            LET HOYDIA     = fecha_comp
            LET fecha_redencion = HOYDIA

            LET dias_calculados = (HOYDIA - HOY)
            LET potencia3 = (dias_calculados / 365)
            LET constante = 1.035

            SELECT pow(constante,potencia3)
            INTO   potencia
            FROM   systables
            WHERE  tabid=1

            LET monto_act_pesos_t1 = (w_aux.monto_udis / (potencia) * precio_udi)
            LET monto_act_udis_t1  =  monto_act_pesos_t1 / precio_udi

            LET saldo_ini_pesos_udis = monto_act_pesos_t1 - monto_act_udis_t1

            IF saldo_ini_pesos = 0 THEN
               LET saldo_ini_pesos = monto_act_pesos_t1
            END IF

            IF saldo_ini_udis = 0 THEN
               LET saldo_ini_udis = monto_act_udis_t1
            END IF


            OUTPUT TO REPORT listado_2(w_aux.nss      ,
                                       w_aux.curp,
                                       w_aux.folio,
                                       w_aux.consecutivo_lote,
                                       w_aux.monto_udis     ,
                                       saldo_ini_pesos      ,
                                       saldo_ini_udis       ,
                                       saldo_ini_pesos_udis ,
                                       HOY)


           INSERT INTO sdo_ini_bono
           VALUES (w_aux.nss            ,
                   w_aux.curp           ,
                   w_aux.folio          ,
                   w_aux.consecutivo_lote,
                   w_aux.monto_udis     ,
                   saldo_ini_pesos      ,
                   saldo_ini_udis       ,
                   saldo_ini_pesos_udis ,
                   HOY                  ,  #fecha_actualiza
                   g_usuario            ,
                   10                   )

             LET saldo_ini_pesos      = 0   
             LET saldo_ini_udis       = 0   
             LET saldo_ini_pesos_udis = 0   



        END FOREACH

        FINISH REPORT listado_2
        
        --IF NOT bnd_proceso THEN
            DISPLAY "BONOS PROCESADOS ", cont_reg AT 16,20
            PROMPT  "Presione [enter] para continuar." FOR enter
        --ELSE
        --    DISPLAY "BONOS PROCESADOS ", cont_reg
        --END IF
 
        --CALL limpia_nulos()

    ELSE
        --IF bnd_proceso THEN
            DISPLAY "Program stopped, NO HAY BONOS A PROCESAR"
        --END IF
    END IF

END FUNCTION

REPORT listado_2(k_aux)
#----------------------

    DEFINE k_aux  RECORD
        nss               CHAR(11),
        curp              CHAR(18),
        folio             INTEGER,
        consecutivo_lote INTEGER,
        monto_udis  DECIMAL(22,6),
        saldo_ini_pesos   DECIMAL(22,6),
        saldo_ini_udis    DECIMAL(22,6),
        saldo_ini_pesos_udis DECIMAL(22,6),
        HOY               DATE
    END RECORD

    DEFINE
        d     CHAR(02),
        m     CHAR(02),
        a     CHAR(04)

    DEFINE
        dia       CHAR(02) ,
        mes       CHAR(02) ,
        ano       CHAR(04) ,
        dia1      CHAR(02) ,
        mes1      CHAR(02) ,
        ano1      CHAR(04) ,
        hoy       CHAR(8)  ,
        num10     CHAR(10) ,
        tot_char  CHAR(9)

    DEFINE x_afore RECORD LIKE tab_afore_local.*

    DEFINE tot SMALLINT

    OUTPUT
        LEFT MARGIN 0
        RIGHT MARGIN 0
        TOP MARGIN 0
        BOTTOM MARGIN 0
        PAGE LENGTH 90

    FORMAT
    PAGE HEADER
        LET hoy = YEAR(TODAY) USING "&&&&",
                  MONTH(TODAY)USING "&&"  ,
                  DAY(TODAY)  USING "&&"

        SELECT * 
        INTO   x_afore.*
        FROM   tab_afore_local

       -- PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
        PRINT COLUMN 01,'\033e\033(10U\033&l1O\033&k2S\033&l12d'
        PRINT
            COLUMN 01,"=================================================",
            COLUMN 50,"=================================================",
            COLUMN 99,"================================================="

        --PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
        PRINT COLUMN 01,'\033e\033(10U\033&l1O\033&k2S\033&l12d'
        PRINT
            COLUMN 01,x_afore.razon_social  CLIPPED                      ,
            COLUMN 35,"LISTADO REGISTROS DE BONO DE PENSION INICIAL " ,
            COLUMN 60,"FECHA     :",TODAY     USING "DD-MM-YYYY"

        --PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
        PRINT COLUMN 01,'\033e\033(10U\033&l1O\033&k2S\033&l12d'
        PRINT
            COLUMN 01,"CONB002"                                          ,
            COLUMN 35,"CALCULADOS"                               ,
            COLUMN 60,"Nro.PAGINA:",PAGENO    USING "##########"

        --PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
        PRINT COLUMN 01,'\033e\033(10U\033&l1O\033&k2S\033&l12d'
        PRINT
            COLUMN 01,"=================================================",
            COLUMN 50,"=================================================",
            COLUMN 99,"================================================="

        PRINT
            COLUMN 01 ,"NSS "          ,
            COLUMN 15 ,"CURP"          ,
            COLUMN 35 ,"FOLIO"         ,
            COLUMN 50 ,"MTO BONO         "  ,
            COLUMN 65 ,"SALDO INI PESOS               " ,
            COLUMN 90,"SALDO INI UDIS      ",
            COLUMN 110,"SALDO INI PESOS-UDIS     "

        --PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
        PRINT COLUMN 01,'\033e\033(10U\033&l1O\033&k2S\033&l12d'
        PRINT
            COLUMN 01,"-------------------------------------------------",
            COLUMN 50,"-------------------------------------------------",
            COLUMN 99,"-------------------------------------------------"

        ON EVERY ROW


        --PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
        PRINT COLUMN 01,'\033e\033(10U\033&l1O\033&k2S\033&l12d'
        PRINT
            COLUMN 01 ,k_aux.nss       ,
            COLUMN 15 ,k_aux.curp        ,
            COLUMN 35 ,k_aux.folio              USING "##########",
            COLUMN 40 ,k_aux.monto_udis         USING "##########&.######",
            COLUMN 60 ,k_aux.saldo_ini_pesos    USING "##########&.######" ,
            COLUMN 100,k_aux.saldo_ini_udis     USING "##########&.######" ,
            COLUMN 120,k_aux.saldo_ini_pesos_udis USING "##########&.######" 


     ON LAST ROW 
        PRINT
            COLUMN 01,"-------------------------------------------------",
            COLUMN 50,"-------------------------------------------------",
            COLUMN 99,"-------------------------------------------------"
        --PRINT '\033e\033(10U\033&k2S\033&l12d'
        PRINT COLUMN 01,'\033e\033(10U\033&l1O\033&k2S\033&l12d'
        PRINT COLUMN 001,"TOTALES:",
              COLUMN 45,SUM(k_aux.monto_udis) USING "##########&.######",
              COLUMN 63,SUM(k_aux.saldo_ini_pesos) USING "##########&.######",
              COLUMN 100,SUM(k_aux.saldo_ini_udis) USING "##########&.######",
              COLUMN 120,SUM(k_aux.saldo_ini_pesos_udis) USING "##########&.######"
        --PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
        PRINT COLUMN 01,'\033e\033(10U\033&l1O\033&k2S\033&l12d'
        PRINT
            COLUMN 01,"-------------------------------------------------",
            COLUMN 50,"-------------------------------------------------",
            COLUMN 99,"-------------------------------------------------"

END REPORT

