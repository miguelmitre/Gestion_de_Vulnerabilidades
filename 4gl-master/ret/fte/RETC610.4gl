################################################################################
#Owner            => E.F.P.                                                    #
#Programa RETC610 => CORRIGE RETIROS QUE NO SE DESMARCARON CORRECTAMENTE       #
#Fecha creacion   => 21 DE JUNIO DE 2004                                       #
#By               => JUAN CARLOS MENDOZA MORENO                                #
#Sistema          => RET                                                       #
################################################################################
DATABASE safre_af
GLOBALS
    DEFINE gr_rango RECORD #gr_rango_fechas
        fecha_ini             ,
        fecha_fin             DATE
    END RECORD

    DEFINE #glo #date
        HOY                   DATE

    DEFINE  # glo #char
        enter                 CHAR(01)

    DEFINE #glo #integer
        cont_1                ,
        tot_reg               INTEGER

END GLOBALS
################################################################################
MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP           ,
        PROMPT LINE LAST     ,
        ACCEPT KEY CONTROL-I

    CALL init() #i
{    
    OPEN WINDOW retc6101 AT 4,4 WITH FORM "RETC6101" ATTRIBUTE (BORDER)
    DISPLAY "                            CANCELAR < CTRL-C >                                  " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC610                     DESMARCA CUENTAS                                    " AT 3,1 ATTRIBUTE (REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE (REVERSE)

    INPUT BY NAME gr_rango.* WITHOUT DEFAULTS
        AFTER FIELD fecha_ini
            IF gr_rango.fecha_ini  IS NULL
            OR gr_rango.fecha_ini = " " THEN
                ERROR "    LA FECHA NO PUEDE SER NULA" ATTRIBUTE(NORMAL)
                NEXT FIELD fecha_ini
            END IF

        AFTER FIELD fecha_fin
            IF gr_rango.fecha_fin  IS NULL
            OR gr_rango.fecha_fin = " " THEN
                ERROR "    LA FECHA NO PUEDE SER NULA" ATTRIBUTE(NORMAL)
                NEXT FIELD fecha_fin
            END IF

            IF gr_rango.fecha_fin > HOY THEN
                ERROR "  LA FECHA FINAL NO PUEDE SER MAYOR A LA DE HOY  "
                NEXT FIELD fecha_fin
            END IF

            IF gr_rango.fecha_fin < gr_rango.fecha_ini THEN
                ERROR "  LA FECHA FINAL NO PUEDE SER MENOR QUE LA FECHA INICIAL  "
                NEXT FIELD fecha_fin
            END IF

            EXIT INPUT

        ON KEY (CONTROL-C)
            PROMPT " PROCESO CANCELADO...<ENTER> PARA SALIR " FOR CHAR enter
            EXIT PROGRAM

        ON KEY (INTERRUPT)
            PROMPT " PROCESO CANCELADO...<ENTER> PARA SALIR " FOR CHAR enter
            EXIT PROGRAM
    END INPUT

    WHILE TRUE
        PROMPT " ESTA SEGURO S/N ? " FOR CHAR enter
        IF enter MATCHES "[SsNn]" THEN
            IF enter MATCHES "[Ss]" THEN
                EXIT WHILE
            ELSE
                PROMPT " PROCESO CANCELADO...<ENTER> PARA SALIR "
                FOR CHAR enter
                EXIT PROGRAM
           END IF
        END IF
    END WHILE

    DISPLAY "PROCESANDO INFORMACION " AT 19,2 ATTRIBUTE(REVERSE)
}
    CALL crea_tmp_dis_cuenta()
    CALL primer_paso()   #pp  --Inserta registros en tabla nss_desmarca
                              --Desmarca registros con consecutivo y que han
                              --finalizado su proceso, sin importar su tipo de
                              --retiro(liquidacion)
                              --ESTADO 100
    CALL segundo_paso()  #sp  --Desmarcar retiros de parciales
                              --ESTADO 204
    CALL tercer_paso()   #tp  --Desmarcar retiros de transferencias(falta verif. con Franco)
                              --ESTADO 301
    CALL cuarto_paso()   #cp  --Desamrca retiros de disposicion
                              --ESTADO 401

    DISPLAY " REGISTROS PROCESADOS :  ",cont_1  AT 11,15

    PROMPT " PROCESO FINALIZADO...<ENTER> PARA SALIR " FOR char enter
--    CLOSE WINDOW retc6101
END MAIN
################################################################################
FUNCTION init()

    LET HOY = TODAY
    LET gr_rango.fecha_ini = "01/01/2007"
    LET gr_rango.fecha_fin = TODAY

    INITIALIZE gr_rango.* TO NULL

END FUNCTION
################################################################################
FUNCTION crea_tmp_dis_cuenta()
   DEFINE v_nombre_tabla  CHAR(20),
          sel_his         CHAR(5000)


    DATABASE safre_tmp

    WHENEVER ERROR CONTINUE
        DROP TABLE nss_desmarca
    WHENEVER ERROR STOP
    
    CREATE TABLE nss_desmarca (
        nss                 CHAR(11)                ,
        marca_cod           SMALLINT                ,
        fecha_ini           DATE                    ,
        hora_ini            DATETIME HOUR TO SECOND ,
        estado_marca        SMALLINT                ,
        marca_causa         SMALLINT                ,
        fecha_causa         DATE                    ,
        correlativo         INTEGER                 ,
        usuario             CHAR(8)                 ,
        estado_desmarca     SMALLINT                ,
        fecha_fin           DATE                    ,
        fecha_desmarca      DATE                    ,
        fecha_proceso       DATE                    
    )

    GRANT ALL ON nss_desmarca TO PUBLIC   
   
    DATABASE safre_af 

   INSERT INTO safre_tmp:nss_desmarca
   SELECT nss         ,
          marca_cod   ,
          fecha_ini   ,
          hora_ini    ,
          estado_marca,
          marca_causa ,
          fecha_causa ,
          correlativo ,
          usuario     ,
          0           , --estado_desmarca
          ""          , --fecha_fin
          ""          , --fecha_desmarca
          TODAY
   FROM   cta_act_marca
--   WHERE  fecha_ini BETWEEN gr_rango.fecha_ini AND gr_rango.fecha_fin
   WHERE  fecha_ini BETWEEN "01/01/2007" AND TODAY
   AND    marca_cod BETWEEN 800 AND 899
   AND    correlativo > 0

    DELETE 
    FROM   safre_tmp:nss_desmarca
    WHERE  marca_cod = 841

   WHENEVER ERROR CONTINUE
      DROP TABLE tmp_dis_cuenta_desmarca;
   WHENEVER ERROR STOP

   DECLARE cur_his CURSOR FOR
   SELECT tabname
   FROM   systables
   WHERE  tabname matches "dis_cuenta??"

   FOREACH cur_his INTO v_nombre_tabla
      LET sel_his = sel_his CLIPPED,
                    " SELECT * ",
                    " FROM   ",v_nombre_tabla CLIPPED,
                    " WHERE  nss IN (SELECT nss FROM safre_tmp:nss_desmarca)",
                    " AND    tipo_movimiento NOT IN (888,999)",
                    " UNION ALL "
   END FOREACH
   CLOSE cur_his
   DISPLAY sel_his CLIPPED

   LET sel_his =  sel_his CLIPPED,
                 " SELECT * ",
                 " FROM   dis_cuenta ",
                 " WHERE  nss IN (SELECT nss FROM safre_tmp:nss_desmarca)",
                 " AND    tipo_movimiento NOT IN (888,999)",
                 " INTO TEMP tmp_dis_cuenta_desmarca "
                 
   PREPARE eje_sel_his FROM sel_his
   EXECUTE eje_sel_his

   CREATE INDEX tmp_dis_cuenta_desmarca1 ON tmp_dis_cuenta_desmarca(tipo_movimiento,
                                                                    subcuenta,
                                                                    folio,
                                                                    consecutivo_lote)
   UPDATE STATISTICS FOR TABLE tmp_dis_cuenta_desmarca

END FUNCTION
################################################################################
FUNCTION primer_paso()
#pp------------------
    DEFINE reg_1 RECORD #loc #reg_1
         nss              CHAR(11),
         marca_cod        SMALLINT,
         fecha_ini        DATE,
         hora_ini         DATETIME HOUR TO SECOND,
         estado_marca     SMALLINT,
         marca_causa      SMALLINT,
         fecha_causa      DATE,
         correlativo      INTEGER,
         usuario          CHAR(8),
         estado_desmarca  SMALLINT,
         fecha_fin        DATE,
         fecha_desmarca   DATE
    END RECORD

    DEFINE #loc #date
        vfecha_conver         DATE


    DEFINE #loc #decimal
        vconsec               DECIMAL(11,0)

    INITIALIZE reg_1.*       TO NULL
    INITIALIZE vfecha_conver TO NULL

    LET tot_reg = 0

    SELECT COUNT(*)
    INTO   tot_reg
    FROM   safre_tmp:nss_desmarca

    IF tot_reg = 0 THEN
        RETURN
    ELSE
        DECLARE cur_1 CURSOR FOR
        SELECT *
        FROM   safre_tmp:nss_desmarca
        ORDER BY nss

        LET cont_1 = 0

        FOREACH cur_1 INTO reg_1.*
            DISPLAY " REGISTROS A PROCESAR :  ",tot_reg AT 10,15

            DISPLAY reg_1.nss
            DISPLAY reg_1.marca_cod
            DISPLAY reg_1.correlativo
            
            ----------------------------------------------------
            --  NSS QUE TIENEN CONSECUTIVO DIFERENTE DE CERO  --
            ----------------------------------------------------
            INITIALIZE vfecha_conver TO NULL

            SELECT UNIQUE fecha_conversion
            INTO   vfecha_conver
            FROM   tmp_dis_cuenta_desmarca
            WHERE  nss              = reg_1.nss
            AND    consecutivo_lote = reg_1.correlativo
            AND    tipo_movimiento  BETWEEN 800 AND 899

            IF vfecha_conver IS NOT NULL THEN
                -------------------------------------------------------
                --  DESMARCA NSS CON CONSECUTIVO Y FECHA_CONVERSION  --
                -------------------------------------------------------

                UPDATE cta_his_marca
                SET    fecha_fin    = vfecha_conver ,
                       usr_desmarca = "retc610"
                WHERE  nss          = reg_1.nss
                AND    correlativo  = reg_1.correlativo
                AND    marca_cod    = reg_1.marca_cod

                DELETE
                FROM   cta_act_marca
                WHERE  nss          = reg_1.nss
                AND    correlativo  = reg_1.correlativo
                AND    marca_cod    = reg_1.marca_cod

                UPDATE safre_tmp:nss_desmarca
                SET    estado_desmarca = 100           ,--todos los liquidados
                       fecha_fin       = vfecha_conver ,
                       fecha_desmarca  = HOY
                WHERE  nss             = reg_1.nss
                AND    correlativo     = reg_1.correlativo
                AND    fecha_proceso   = HOY

                LET cont_1 = cont_1 + 1
            END IF
        END FOREACH
    END IF

END FUNCTION
################################################################################
FUNCTION segundo_paso()
#sp--------------------
    DEFINE lr_parcial RECORD #loc #lr_parcial
    	   nss              CHAR(11),
         marca_cod        SMALLINT,
         fecha_ini        DATE,
         hora_ini         DATETIME HOUR TO SECOND,
         estado_marca     SMALLINT,
         marca_causa      SMALLINT,
         fecha_causa      DATE,
         correlativo      INTEGER,
         usuario          CHAR(8),
         estado_desmarca  SMALLINT,
         fecha_fin        DATE,
         fecha_desmarca   DATE,
         fecha_proceso    DATE
    END RECORD

    DEFINE #loc #smallint
        v_tipo_prestacion     SMALLINT

    DEFINE #loc #date
        vfecha_conver_parcial DATE

    DEFINE #loc #decimal
        vconsec_parcial       DECIMAL(11,0)

    DECLARE cur_parcial CURSOR FOR
    SELECT a.*
    FROM   safre_tmp:nss_desmarca a,
           ret_parcial b
    WHERE  a.nss             = b.nss
    AND    a.correlativo     = b.consecutivo
    AND    b.diag_cuenta_ind <> 400
    AND    a.marca_cod       IN (870,875)
    AND    a.estado_desmarca = 0

    FOREACH cur_parcial INTO lr_parcial.*
    	   INITIALIZE vfecha_conver_parcial TO NULL

         SELECT UNIQUE A.fecha_conversion
         INTO   vfecha_conver_parcial
         FROM   tmp_dis_cuenta_desmarca A
         WHERE  A.folio = (SELECT B.folio
                           FROM   ret_parcial_tx B
                           WHERE  B.nss             =  lr_parcial.nss
                           AND    B.consecutivo     =  lr_parcial.correlativo
                          )
         AND    A.tipo_movimiento IN (870,875)

         IF vfecha_conver_parcial IS NOT NULL THEN
             -----------------------------------------------------------
             --  DESMARCA NSS CON CONSECUTIVO Y SIN FECHA_CONVERSION  --
             --  FECHA BUSCADA POR FOLIO EN DIS_CUENTA;               --
             -----------------------------------------------------------
             UPDATE cta_his_marca
             SET    fecha_fin    = vfecha_conver_parcial ,
                    usr_desmarca = "retc610"   --Solicita HSBC
             WHERE  nss          = lr_parcial.nss
             AND    hora_ini     = lr_parcial.hora_ini
             AND    marca_cod    = lr_parcial.marca_cod
             AND    correlativo  = lr_parcial_correlativo

             DELETE
             FROM   cta_act_marca
             WHERE  nss         = lr_parcial.nss
             AND    hora_ini    = lr_parcial.hora_ini
             AND    marca_cod   = lr_parcial.marca_cod
             AND    correlativo = lr_parcial_correlativo

             UPDATE safre_tmp:nss_desmarca
             SET    estado_desmarca = 204                  ,
                    fecha_fin       = vfecha_conver_parcial,
                    fecha_desmarca  = HOY
             WHERE  nss             = lr_parcial.nss
             AND    correlativo     = lr_parcial_correlativo
             AND    fecha_proceso   = HOY

             LET cont_1 = cont_1 + 1
         END IF
    END FOREACH
END FUNCTION
################################################################################
FUNCTION tercer_paso()
#tp-------------------
    DEFINE lr_transfer RECORD #loc #lr_transfer
    	   nss              CHAR(11),
         marca_cod        SMALLINT,
         fecha_ini        DATE,
         hora_ini         DATETIME HOUR TO SECOND,
         estado_marca     SMALLINT,
         marca_causa      SMALLINT,
         fecha_causa      DATE,
         correlativo      INTEGER,
         usuario          CHAR(8),
         estado_desmarca  SMALLINT,
         fecha_fin        DATE,
         fecha_desmarca   DATE,
         fecha_proceso    DATE
    END RECORD

    DEFINE #loc #integer
        cont_transf           ,
        vfolio_transf         ,
        folio_uni_transf      INTEGER

    DEFINE #loc #date
        vfecha_conver_transf  ,
        v_fecha_carga         DATE

    DEFINE #loc #decimal
        vconsec_transf        ,
        consec_uni_transf     DECIMAL(11,0)

    DECLARE cur_transfer CURSOR FOR
    SELECT *
    FROM   safre_tmp:nss_desmarca
    WHERE  marca_cod      IN (800,810,815) --Retiros por Transferencia de Recursos
    AND    estado_desmarca = 0

    FOREACH cur_transfer INTO lr_transfer.*
    	   INITIALIZE vfecha_conver_transf TO NULL

         SELECT UNIQUE A.fecha_conversion
         INTO   vfecha_conver_transf
         FROM   tmp_dis_cuenta_desmarca A
         WHERE  A.folio = (SELECT B.folio
                           FROM   ret_transf_tx B
                           WHERE  B.nss         = lr_transfer.nss
                           AND    B.consecutivo = lr_transfer.correlativo
                           )
         AND    A.tipo_movimiento IN(800,810,815)
         AND    A.subcuenta       IN(1,2,4,5,6,9)

         IF vfecha_conver_transf IS NOT NULL THEN
             -----------------------------------------------------------
             --  DESMARCA NSS CON CONSECUTIVO Y SIN FECHA_CONVERSION  --
             --  FECHA BUSCADA POR FOLIO EN DIS_CUENTA;               --
             -----------------------------------------------------------
             UPDATE cta_his_marca
             SET    fecha_fin    = vfecha_conver_transf ,
                    usr_desmarca = "retc610"
             WHERE  nss         = lr_transfer.nss
             AND    hora_ini    = lr_transfer.hora_ini
             AND    marca_cod   = lr_transfer.marca_cod
             AND    correlativo = lr_transfer.correlativo

             DELETE
             FROM   cta_act_marca
             WHERE  nss         = lr_transfer.nss
             AND    hora_ini    = lr_transfer.hora_ini
             AND    marca_cod   = lr_transfer.marca_cod
             AND    correlativo = lr_transfer.correlativo

             UPDATE safre_tmp:nss_desmarca
             SET    estado_desmarca = 301                   ,
                    fecha_fin       = vfecha_conver_transf  ,
                    fecha_desmarca  = HOY
             WHERE  nss             = lr_transfer.nss
             AND    correlativo     = lr_transfer.correlativo
             AND    fecha_proceso   = HOY

             LET cont_1 = cont_1 + 1
         END IF
    END FOREACH
END FUNCTION
################################################################################
FUNCTION cuarto_paso()
#cp-------------------

    DEFINE lr_disposicion RECORD #loc #lr_disposicion
    	  nss              CHAR(11),
         marca_cod        SMALLINT,
         fecha_ini        DATE,
         hora_ini         DATETIME HOUR TO SECOND,
         estado_marca     SMALLINT,
         marca_causa      SMALLINT,
         fecha_causa      DATE,
         correlativo      INTEGER,
         usuario          CHAR(8),
         estado_desmarca  SMALLINT,
         fecha_fin        DATE,
         fecha_desmarca   DATE,
         fecha_proceso    DATE
    END RECORD

    DEFINE #loc #integer
        cont_disp             ,
        vfolio_disp           INTEGER

    DEFINE #loc #date
        vfecha_conver_disp    DATE

    DEFINE #loc #decimal
        vconsec_disp          DECIMAL(11,0)

    DECLARE cur_disp CURSOR FOR
    SELECT *
    FROM   safre_tmp:nss_desmarca
    WHERE  marca_cod       IN (820,830,840,850,860,880,890,895)
    AND    estado_desmarca = 0

    FOREACH cur_disp INTO lr_disposicion.*
    	   INITIALIZE vfecha_conver_disp TO NULL

         SELECT UNIQUE A.fecha_conversion
         INTO   vfecha_conver_disp
         FROM   tmp_dis_cuenta_desmarca A
         WHERE  A.folio = (SELECT B.folio
                           FROM   ret_solicitud_tx B
                           WHERE  B.nss         = lr_disposicion.nss
                           AND    B.consecutivo = lr_disposicion.correlativo
                           )
         AND    subcuenta       IN (1,2,4,5,6,7,8,9)
         AND    tipo_movimiento IN (820,830,840,850,860,880,890,895)

         IF vfecha_conver_disp IS NOT NULL THEN
             -----------------------------------------------------------
             --  DESMARCA NSS CON CONSECUTIVO Y SIN FECHA_CONVERSION  --
             --  FECHA BUSCADA POR FOLIO EN DIS_CUENTA;               --
             -----------------------------------------------------------
             UPDATE cta_his_marca
             SET    fecha_fin    = vfecha_conver_disp ,
                    usr_desmarca = "retc610"
             WHERE  nss         = lr_disposicion.nss
             AND    hora_ini    = lr_disposicion.hora_ini
             AND    marca_cod   = lr_disposicion.marca_cod
             AND    correlativo = lr_disposicion.correlativo

             DELETE
             FROM   cta_act_marca
             WHERE  nss         = lr_disposicion.nss
             AND    hora_ini    = lr_disposicion.hora_ini
             AND    marca_cod   = lr_disposicion.marca_cod
             AND    correlativo = lr_disposicion.correlativo

             UPDATE safre_tmp:nss_desmarca
             SET    estado_desmarca = 401                ,
                    fecha_fin       = vfecha_conver_disp ,
                    fecha_desmarca  = HOY
             WHERE  nss             = lr_disposicion.nss
             AND    correlativo     = lr_disposicion.correlativo
             AND    fecha_proceso   = HOY

             LET cont_1 = cont_1 + 1
         END IF
    END FOREACH
END FUNCTION
