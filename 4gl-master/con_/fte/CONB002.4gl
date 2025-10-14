#############################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                          #
#Propietario       => E.F.P.                                                #
#Programa CONB002  => REVALORIZACION DIARIA BONO PENSION                    #
#Sistema           => CON                                                   #
#Autor             => EDUARDO JOAQUIN RESENDIZ MEDINA                       #
#Fecha             => 25 DE NOVIEMBRE DE 2008                               #
#############################################################################

DATABASE safre_af

GLOBALS

    DEFINE g_reg RECORD
--        folio        INTEGER,
        fecha_genera DATE,
        generar      CHAR(1)
    END RECORD
{
    DEFINE w_aux RECORD
        nss              CHAR(11),
        curp             CHAR(18),
        folio            INTEGER,
        consecutivo_lote INTEGER,
        monto_pesos      DECIMAL(22,6),
        monto_udis       DECIMAL(22,6),
        fecha_reden      CHAR(08)
    END RECORD
}
    DEFINE w_aux RECORD
        nss              CHAR(11),
        curp             CHAR(18),
        fecha_reden      DATE,
        fecha_reg        DATE,
        monto_udis       DECIMAL(22,6),
        monto_pesos      DECIMAL(22,6)
    END RECORD

    DEFINE g_opcion CHAR(2)

   DEFINE
       HOY                DATE     ,
       HOYDIA             DATE     ,
       fecha_redencion    DATE     ,
       dias_calculados    INTEGER  ,
       dias_calculados_ini INTEGER  ,
       precio_udi         DECIMAL(22,6),
       precio_udi_ini     DECIMAL(22,6),
       monto_act_pesos_t1 DECIMAL(22,6),
       monto_act_pesos_t1_ini DECIMAL(22,6),
       monto_act_udis_t1  DECIMAL(22,6),
       monto_act_udis_t1_ini  DECIMAL(22,6),
       mov_ant_710401     DECIMAL(22,6),
       mov_ant_710402     DECIMAL(22,6),
       mov_ant_710403     DECIMAL(22,6),
       mov_ant_710404     DECIMAL(22,6),
       sum_mov_ant_710401 DECIMAL(22,6),
       sum_mov_ant_710402 DECIMAL(22,6),
       sum_mov_ant_710403 DECIMAL(22,6),
       sum_mov_ant_710404 DECIMAL(22,6),
       mov_dia_710401     DECIMAL(22,6),
       mov_dia_710402     DECIMAL(22,6),
       mov_dia_710403     DECIMAL(22,6),
       mov_dia_710404     DECIMAL(22,6),
       sdo_dia_710401     DECIMAL(22,6),
       sdo_dia_710402     DECIMAL(22,6),
       sdo_dia_710403     DECIMAL(22,6),
       sdo_dia_710404     DECIMAL(22,6),
       saldo_ini_udis     DECIMAL(22,6),
       saldo_ini_pesos    DECIMAL(22,6),
       saldo_ini_pesos_udis DECIMAL(22,6),
       saldo_ant_710401   DECIMAL(22,6),
       saldo_ant_710402   DECIMAL(22,6),
       saldo_ant_710403   DECIMAL(22,6),
       saldo_ant_710404   DECIMAL(22,6),
       potencia           DECIMAL(22,6),
       potencia_ini       DECIMAL(22,6),
       potencia3          DECIMAL(22,7),
       potencia3_ini      DECIMAL(22,7),
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
       principal          CHAR(1500),
       encuentra          CHAR(1000)

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

    CALL STARTLOG('CONB002.log')
    CALL inicio() #i

    DEFER INTERRUPT
    OPTIONS INPUT WRAP,
    PROMPT LINE LAST,
    ACCEPT KEY CONTROL-I

    CALL proceso_principal() #pp
--    CALL rescata_valores()   #rv

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

END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------

    OPEN WINDOW ventana_1 AT 4,4 WITH FORM "CONB0021" ATTRIBUTE(BORDER)
    DISPLAY " CONB002  REVALORIZACION DIARIA BONO PENSION (POR REGISTRO)                    " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY "                           < Ctrl-C > Salir                                    " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"dd-mm-yyyy" AT 3,65 ATTRIBUTE(REVERSE)


    INPUT BY NAME g_reg.*
{        BEFORE FIELD folio

        AFTER FIELD folio

        IF g_reg.folio IS NULL THEN
           ERROR "Folio no puede ser NULO "
           NEXT FIELD folio
        END IF
}
        AFTER FIELD fecha_genera

        IF g_reg.fecha_genera IS NULL THEN
           ERROR "Fecha genera no puede ser NULO "
           NEXT FIELD fecha_genera
        ELSE
           IF g_reg.fecha_genera > HOY THEN
              ERROR "Fecha genera no puede ser mayor a fecha de hoy "
              NEXT FIELD fecha_genera
           ELSE
              SELECT UNIQUE(fecha_actualiza)
              FROM  act_dia_bono
              WHERE fecha_actualiza = g_reg.fecha_genera
              GROUP BY 1
--              AND   folio           = g_reg.folio

              IF SQLCA.SQLCODE <> NOTFOUND THEN
                  ERROR "Fecha ya procesada "
                  NEXT FIELD fecha_genera
              END IF
           END IF
        END IF

        AFTER FIELD generar

        IF g_reg.generar IS NULL THEN
            ERROR "Campo NO puede ser NULO"
            NEXT FIELD generar
        END IF

        IF g_reg.generar  MATCHES "[Ss]" THEN

            SELECT precio_del_dia
            INTO   precio_udi
            FROM   glo_valor_accion
            WHERE  codigo_siefore  = 13
            --AND    fecha_valuacion = HOY
            AND    fecha_valuacion = g_reg.fecha_genera

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

    LET mov_ant_710401   = 0
    LET mov_ant_710402   = 0
    LET mov_ant_710403   = 0
    LET mov_ant_710404   = 0
    LET saldo_ini_udis   = 0
    LET saldo_ini_pesos  = 0
    LET saldo_ini_pesos_udis = 0
    LET saldo_ant_710401 = 0
    LET saldo_ant_710402 = 0
    LET saldo_ant_710403 = 0
    LET saldo_ant_710404 = 0
    LET mov_dia_710401   = 0
    LET mov_dia_710402   = 0
    LET mov_dia_710403   = 0
    LET mov_dia_710404   = 0
    LET sdo_dia_710401   = 0
    LET sdo_dia_710402   = 0
    LET sdo_dia_710403   = 0
    LET sdo_dia_710404   = 0
    LET potencia         = 0
    LET potencia3        = 0
    LET precio_udi_ini   = 0
    LET dias_calculados_ini = 0
    LET potencia3_ini    = 0
    LET potencia_ini     = 0
    LET monto_act_pesos_t1_ini = 0
    LET monto_act_udis_t1_ini = 0
{
    SELECT COUNT(*) 
    INTO   HAY_REGISTROS 
    FROM   dis_cuenta
    WHERE  subcuenta       = 36
    AND    tipo_movimiento = 17
}

    sql
    INSERT INTO  cta_his_bono
    SELECT a.nss,curp,fecha_redencion,fecha_registro,udis,pesos,
           proceso,TODAY,'TAC','',usuario
    FROM   cta_act_bono a
       WHERE a.nss IN (SELECT nss
                         FROM dis_cuenta   d, taa_cd_ctr_folio  c
                        WHERE fecha_conversion     =  TODAY
                          AND  subcuenta           =   36
                          AND tipo_movimiento  IN(SELECT  tipo_movimiento
                                                    FROM  taa_cd_tipo_traspaso)
                          AND d.folio              =   c.folio
                          AND fecha_liquidacion     =  TODAY)
    END sql

    sql
    DELETE FROM cta_act_bono 
    WHERE nss IN (SELECT nss
                      FROM dis_cuenta   d, taa_cd_ctr_folio  c
                     WHERE fecha_conversion     =  TODAY
                       AND  subcuenta           =   36
                       AND tipo_movimiento  IN(SELECT  tipo_movimiento
                                                 FROM  taa_cd_tipo_traspaso)
                       AND d.folio              =   c.folio
                       AND fecha_liquidacion     =  TODAY)
    END sql


    SELECT COUNT(*) 
    INTO   HAY_REGISTROS 
    FROM   cta_act_bono

    IF HAY_REGISTROS THEN

        LET vsoli_env = HAY_REGISTROS

        DISPLAY "BONOS A PROCESAR ", vsoli_env AT 11,9
{
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
        --AND    A.nss              = B.n_seguro
        AND    A.curp             = B.n_unico
        AND    A.folio            = B.folio
        AND    A.consecutivo_lote = B.consec_reg_lote
}
        INITIALIZE w_aux.* TO NULL

        DECLARE curs_1 CURSOR FOR
        SELECT A.nss              ,
               A.curp             ,
               A.fecha_redencion,
               A.fecha_registro,
               A.udis   ,
               A.pesos
        FROM   cta_act_bono A

        DISPLAY "Procesando Informacion"

        LET G_LISTA = g_seg_modulo.ruta_listados CLIPPED,
                      "/",g_usuario CLIPPED, "LISTADO_ACT_BONO."
                      CLIPPED,g_reg.fecha_genera USING "dd-mm-yy"

        START REPORT listado_2 TO G_LISTA

        FOREACH curs_1 INTO w_aux.*
            LET HAY_REGISTROS = TRUE
            LET cont_reg      = cont_reg + 1

--->calcula saldo diario
            LET fecha_redencion = w_aux.fecha_reden
            LET HOYDIA          = fecha_redencion

            LET dias_calculados = (HOYDIA - g_reg.fecha_genera)

            LET potencia3 = (dias_calculados / 365)
            LET constante = 1.035

            SELECT pow(constante,potencia3)
            INTO   potencia
            FROM   systables
            WHERE  tabid=1

            LET monto_act_pesos_t1 = (w_aux.monto_udis / (potencia) * precio_udi)
            LET monto_act_udis_t1  =  monto_act_pesos_t1 / precio_udi

            SELECT MAX(fecha_actualiza)
            INTO   vfecha_actualiza
            FROM   act_dia_bono
            WHERE  nss          = w_aux.nss
            AND    curp         = w_aux.curp

--DISPLAY w_aux.nss," ",w_aux.curp ," ",vfecha_actualiza

            IF vfecha_actualiza IS NOT NULL THEN
               LET principal = 
               "SELECT mov_ant_710401,      ",
                      "mov_ant_710402,      ",
                      "mov_ant_710403,      ",
                      "mov_ant_710404,      ",
                      "saldo_ant_710401,    ",
                      "saldo_ant_710402,    ",
                      "saldo_ant_710403,    ",
                      "saldo_ant_710404     ",
               " FROM   act_dia_bono        ",
               " WHERE  nss             = ","'",w_aux.nss,"'",
               " AND    curp            = ","'",w_aux.curp,"'",
               " AND    fecha_actualiza = ","'",vfecha_actualiza,"'"
               --" AND    folio           = ",w_aux.folio

               PREPARE ejecuta FROM principal
               EXECUTE ejecuta INTO   mov_ant_710401,
                                      mov_ant_710402,      
                                      mov_ant_710403,      
                                      mov_ant_710404,      
                                      saldo_ant_710401,    
                                      saldo_ant_710402,    
                                      saldo_ant_710403,    
                                      saldo_ant_710404     

            ELSE
                   LET mov_ant_710401   = 0
                   LET mov_ant_710402   = 0
                   LET mov_ant_710403   = 0
                   LET mov_ant_710404   = 0
                   LET saldo_ant_710401 = 0
                   LET saldo_ant_710402 = 0
                   LET saldo_ant_710403 = 0
                   LET saldo_ant_710404 = 0
            END IF

            LET encuentra = 
            "SELECT NVL(SUM(mov_ant_710401),0),",
                  " NVL(SUM(mov_ant_710402),0),",
                  " NVL(SUM(mov_ant_710403),0),",
                  " NVL(SUM(mov_ant_710404),0) ",
            " FROM   act_dia_bono ",      
            " WHERE  nss             = ","'",w_aux.nss,"'",
            " AND    curp            = ","'",w_aux.curp,"'"
            --" AND    folio           = ",w_aux.folio

            PREPARE ejecuta2 FROM encuentra
            EXECUTE ejecuta2 INTO  sum_mov_ant_710401,
                                   sum_mov_ant_710402,      
                                   sum_mov_ant_710403,      
                                   sum_mov_ant_710404    
#{comentado
            SELECT @saldo_ini_udis, 
                   @saldo_ini_pesos, 
                   @saldo_ini_pesos_udis
            INTO   saldo_ini_udis, saldo_ini_pesos, saldo_ini_pesos_udis
            FROM   sdo_ini_bono
            WHERE  @nss             = w_aux.nss
            AND    @curp            = w_aux.curp
            --AND    @folio           = w_aux.folio

            IF STATUS = NOTFOUND THEN
--->calcula saldo inicial
{               LET fecha_redencion = w_aux.fecha_reden
               LET HOYDIA          = fecha_redencion
   
               LET dias_calculados = (HOYDIA - g_reg.fecha_genera)
               LET potencia3 = (dias_calculados / 365)
               LET constante = 1.035
   
               SELECT pow(constante,potencia3)
               INTO   potencia
               FROM   systables
               WHERE  tabid=1
   }
               LET monto_act_pesos_t1_ini = (w_aux.monto_udis / (potencia) * precio_udi)
               LET monto_act_udis_t1_ini  =  monto_act_pesos_t1_ini / precio_udi

               LET saldo_ini_pesos_udis = monto_act_pesos_t1_ini - monto_act_udis_t1_ini

               IF saldo_ini_pesos = 0 THEN
                  LET saldo_ini_pesos = monto_act_pesos_t1_ini
               END IF

               IF saldo_ini_udis = 0 THEN
                  LET saldo_ini_udis = monto_act_udis_t1_ini
               END IF
#comentado
               INSERT INTO sdo_ini_bono
               VALUES (w_aux.nss            ,
                       w_aux.curp           ,
                       --w_aux.folio          ,
                       0                    ,
                       --w_aux.consecutivo_lote,
                       0                    ,
                       w_aux.monto_udis     ,
                       saldo_ini_pesos      ,
                       saldo_ini_udis       ,
                       saldo_ini_pesos_udis ,
                       HOY                  ,  #fecha_actualiza
                       g_usuario            ,
                       20                   ) #20 = ingresos por traspasos
{
                 LET saldo_ini_pesos      = 0   
                 LET saldo_ini_udis       = 0   
                 LET saldo_ini_pesos_udis = 0   
}
---<fin calcula saldo inicial
            END IF  #comentado

            IF mov_ant_710402 = 0 THEN
               LET mov_dia_710402  = monto_act_udis_t1 - saldo_ini_udis
            ELSE
               LET mov_dia_710402  = monto_act_udis_t1 - saldo_ini_udis - sum_mov_ant_710402
            END IF

            IF mov_ant_710403 = 0   AND   saldo_ant_710403  =  0  THEN
               --LET mov_dia_710403 = ((saldo_ini_udis * precio_udi) - saldo_ini_pesos)
               LET mov_dia_710403 = ((saldo_ini_udis * precio_udi) - saldo_ini_pesos)
            ELSE
               --LET mov_dia_710403 = ((saldo_ini_udis * precio_udi) - saldo_ini_pesos) - sum_mov_ant_710403
               LET mov_dia_710403 = ((saldo_ini_udis * precio_udi) - saldo_ini_pesos) - sum_mov_ant_710403
            END IF

            IF mov_ant_710404 = 0 THEN
               LET mov_dia_710404 = mov_dia_710402 * precio_udi - mov_dia_710402
            ELSE
               LET mov_dia_710404 = mov_dia_710402 * precio_udi - mov_dia_710402
            END IF

            LET sdo_dia_710401  =saldo_ini_udis 
            LET sdo_dia_710402  = saldo_ant_710402 + mov_dia_710402

            IF  saldo_ant_710403 = 0 THEN
               LET sdo_dia_710403  = saldo_ini_pesos_udis + mov_dia_710403
            ELSE
               LET sdo_dia_710403  = saldo_ant_710403 + mov_dia_710403
            END IF

            LET sdo_dia_710404  = saldo_ant_710404 + mov_dia_710404
            LET saldo_ini_pesos_udis = monto_act_pesos_t1 - monto_act_udis_t1

            IF saldo_ini_pesos = 0 THEN
               LET saldo_ini_pesos = monto_act_pesos_t1
            END IF


            OUTPUT TO REPORT listado_2(w_aux.nss      ,
                                       w_aux.curp,
                                       --w_aux.folio,
                                       0,
                                       monto_act_udis_t1,
                                       monto_act_pesos_t1,
                                       mov_dia_710401,
                                       mov_dia_710402,
                                       mov_dia_710403,
                                       mov_dia_710404,
                                       sdo_dia_710401,
                                       sdo_dia_710402,
                                       sdo_dia_710403,
                                       sdo_dia_710404,
                                       precio_udi,
                                       fecha_redencion,
                                       dias_calculados,
                                       HOY)


           INSERT INTO act_dia_bono
           VALUES (w_aux.nss            ,
                   w_aux.curp           ,
                   --w_aux.folio          ,
                   0                    ,
                   --w_aux.consecutivo_lote,
                   0                    ,
                   --saldo_ant_710401     ,
                   --saldo_ant_710402     ,
                   --saldo_ant_710403     ,
                   --saldo_ant_710404     ,
                   sdo_dia_710401       ,
                   sdo_dia_710402       ,
                   sdo_dia_710403       ,
                   sdo_dia_710404       ,
                   --mov_ant_710401       ,
                   --mov_ant_710402       ,
                   --mov_ant_710403       ,
                   --mov_ant_710404       ,
                   mov_dia_710401       , 
                   mov_dia_710402       , 
                   mov_dia_710403       , 
                   mov_dia_710404       , 
                   monto_act_udis_t1    ,
                   monto_act_pesos_t1   ,
                   mov_dia_710401       ,
                   mov_dia_710402       ,
                   mov_dia_710403       ,
                   mov_dia_710404       ,
                   sdo_dia_710401       ,
                   sdo_dia_710402       ,
                   sdo_dia_710403       ,
                   sdo_dia_710404       ,
                   precio_udi           ,
                   fecha_redencion      ,
                   dias_calculados      ,
                   --HOY                  ,  #fecha_actualiza
                   g_reg.fecha_genera   ,  #fecha_actualiza
                   g_usuario            )

           LET mov_ant_710401   = 0
           LET mov_ant_710402   = 0
           LET mov_ant_710403   = 0
           LET mov_ant_710404   = 0
           LET saldo_ini_udis   = 0
           LET saldo_ini_pesos  = 0
           LET saldo_ant_710401 = 0
           LET saldo_ant_710402 = 0
           LET saldo_ant_710403 = 0
           LET saldo_ant_710404 = 0
           LET mov_dia_710401   = 0
           LET mov_dia_710402   = 0
           LET mov_dia_710403   = 0
           LET mov_dia_710404   = 0
           LET sdo_dia_710401   = 0
           LET sdo_dia_710402   = 0
           LET sdo_dia_710403   = 0
           LET sdo_dia_710404   = 0
           INITIALIZE w_aux.* TO NULL
           LET potencia         = 0
           LET potencia3        = 0
           LET precio_udi_ini   = 0
           LET dias_calculados_ini = 0
           LET potencia3_ini    = 0
           LET potencia_ini     = 0
           LET monto_act_pesos_t1_ini = 0
           LET monto_act_udis_t1_ini  = 0

        END FOREACH

        FINISH REPORT listado_2
        
        --IF NOT bnd_proceso THEN
            DISPLAY "BONOS PROCESADOS ", cont_reg AT 16,20
            PROMPT  "Presione [enter] para continuar." FOR enter
        --ELSE
        --    DISPLAY "BONOS PROCESADOS ", cont_reg
        --END IF

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
        monto_act_udis_t1 DECIMAL(22,6),
        monto_act_pesos_t1 DECIMAL(22,6),
        mov_dia_710401    DECIMAL(22,6),
        mov_dia_710402    DECIMAL(22,6),
        mov_dia_710403    DECIMAL(22,6),
        mov_dia_710404    DECIMAL(22,6),
        sdo_dia_710401    DECIMAL(22,6),
        sdo_dia_710402    DECIMAL(22,6),
        sdo_dia_710403    DECIMAL(22,6),
        sdo_dia_710404    DECIMAL(22,6),
        precio_udi        DECIMAL(22,6),
        fecha_redencion   DATE,
        dias_calculados   INTEGER,
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
            COLUMN 99,"=================================================",
            COLUMN 149,"================================================="

        --PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
        PRINT COLUMN 01,'\033e\033(10U\033&l1O\033&k2S\033&l12d'
        PRINT
            COLUMN 01,x_afore.razon_social  CLIPPED                      ,
            COLUMN 35,"LISTADO REGISTROS DE BONO DE PENSION CALCULADOS " ,
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
            COLUMN 99,"=================================================",
            COLUMN 149,"================================================="

        PRINT
            COLUMN 01 ,"NSS "          ,
            COLUMN 15 ,"CURP"          ,
            COLUMN 40 ,"FOLIO"         ,
            COLUMN 55 ,"MTO ACT UDIS          "  ,
            COLUMN 70 ,"MTO ACT PESOS               " ,
            COLUMN 90 ,"MOV DIA 710401    ",
            COLUMN 110,"MOV DIA 710402    ",
            COLUMN 130,"MOV DIA 710403      ",
            COLUMN 150,"MOV DIA 710404      ",
--            COLUMN 90,"SDO DIA 710401      ",
--            COLUMN 110,"SDO DIA 710402     ",
--            COLUMN 130,"SDO DIA 710403          ",
--            COLUMN 150,"SDO DIA 710404     ",
            COLUMN 170,"PRECIO UDI   "    ,
            COLUMN 190,"DIAS CALCULADOS"

        --PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
        PRINT COLUMN 01,'\033e\033(10U\033&l1O\033&k2S\033&l12d'
        PRINT
            COLUMN 01,"-------------------------------------------------",
            COLUMN 50,"-------------------------------------------------",
            COLUMN 99,"-------------------------------------------------",
            COLUMN 149,"-------------------------------------------------"

        ON EVERY ROW


        --PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
        PRINT COLUMN 01,'\033e\033(10U\033&l1O\033&k2S\033&l12d'
        PRINT
            COLUMN 01 ,k_aux.nss       ,
            COLUMN 15 ,k_aux.curp        ,
            COLUMN 35 ,k_aux.folio              USING "##########",
            COLUMN 40 ,k_aux.monto_act_udis_t1 ,
            COLUMN 60 ,k_aux.monto_act_pesos_t1,
            COLUMN 100,k_aux.mov_dia_710401     USING "##########&.######" ,
            COLUMN 120,k_aux.mov_dia_710402     USING "##########&.######" ,
            COLUMN 140,k_aux.mov_dia_710403     USING "##########&.######" ,
            COLUMN 160,k_aux.mov_dia_710404     USING "##########&.######" ,
--            COLUMN 100,k_aux.sdo_dia_710401     USING "##########&.######" ,
--            COLUMN 120,k_aux.sdo_dia_710402     USING "##########&.######" ,
--            COLUMN 140,k_aux.sdo_dia_710403     USING "##########&.######" ,
--            COLUMN 160,k_aux.sdo_dia_710404     USING "##########&.######" ,
            COLUMN 180,k_aux.precio_udi         USING "##########&.######" ,
            COLUMN 200,k_aux.dias_calculados    USING "##########&" 

     ON LAST ROW 
        PRINT
            COLUMN 01,"-------------------------------------------------",
            COLUMN 50,"-------------------------------------------------",
            COLUMN 99,"-------------------------------------------------",
            COLUMN 149,"-------------------------------------------------"
        --PRINT '\033e\033(10U\033&k2S\033&l12d'
        PRINT COLUMN 01,'\033e\033(10U\033&l1O\033&k2S\033&l12d'
        PRINT COLUMN 001,"TOTALES:",
              COLUMN 100,SUM(k_aux.mov_dia_710401) USING "##########&.######",
              COLUMN 120,SUM(k_aux.mov_dia_710402) USING "##########&.######",
              COLUMN 140,SUM(k_aux.mov_dia_710403) USING "##########&.######",
              COLUMN 160,SUM(k_aux.mov_dia_710404) USING "##########&.######"
--              COLUMN 100,SUM(k_aux.sdo_dia_710401) USING "##########&.######",
--              COLUMN 120,SUM(k_aux.sdo_dia_710402) USING "##########&.######",
--              COLUMN 140,SUM(k_aux.sdo_dia_710403) USING "##########&.######",
--              COLUMN 160,SUM(k_aux.sdo_dia_710404) USING "##########&.######"
        --PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
        PRINT COLUMN 01,'\033e\033(10U\033&l1O\033&k2S\033&l12d'
        PRINT
            COLUMN 01,"-------------------------------------------------",
            COLUMN 50,"-------------------------------------------------",
            COLUMN 149,"-------------------------------------------------"

END REPORT

