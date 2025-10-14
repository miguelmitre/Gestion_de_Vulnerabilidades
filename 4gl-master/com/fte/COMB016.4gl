#*******************************************************************#
#Proyecto          => Sistema de Afores.( MEXICO )                  #
#Propietario       => E.F.P.                                        #
#Programa          => COMB016                                       #
#Descripcion       => EXTRACCION DE SALDOS, COMISIONES Y DATOS DE   #
#                  => LOS PROMOTORES                                #
#Sistema           => COM.                                          #
#Fecha             => 26 de septiembre del 2003                     #
#Por               => Laura Eugenia Cortes Guzman                   #
#Fecha Ult.Modif.  =>                                               #
#Por               =>                                               #
#*******************************************************************#
DATABASE safre_af
GLOBALS
  DEFINE   param      RECORD LIKE seg_modulo.*, 
           usuario    CHAR(08), 
           v_arch     CHAR(100), 
           v_ruta     CHAR(70),
           enter      CHAR(01),
           hoy_hoy    DATE
END GLOBALS

MAIN
    DEFINE opc     CHAR(1),
           ejecuta CHAR(300)

    OPTIONS PROMPT LINE LAST,
            COMMENT LINE LAST

    PREPARE x1 FROM "SET PDQPRIORITY 100"
    EXECUTE x1

    LET hoy_hoy = TODAY

    SELECT *, USER INTO param.*,usuario FROM seg_modulo
    WHERE  modulo_cod = "com"

    OPEN WINDOW COMB016 AT 2,2 WITH FORM "COMB0161" ATTRIBUTE(BORDER)
         DISPLAY " < ESC > Procesar                                       < Ctrl-C > Salir    " AT 1,1 ATTRIBUTE(REVERSE)
                                                                                
         DISPLAY " COMB016                ARCHIVO ESPECIAL COMISION                             "
                 AT 3,1 ATTRIBUTE(REVERSE)
         DISPLAY hoy_hoy  USING "DD-MM-YYYY " AT 3,63 ATTRIBUTE(REVERSE)

         WHILE TRUE
             PROMPT "DESEA GENERAR EL ARCHIVO S/N ? " FOR enter
             IF enter MATCHES "[sSnN]" THEN
                EXIT WHILE
             END IF
         END WHILE 

         IF enter MATCHES "[sS]"  THEN

             DISPLAY "GENERANDO INFORMACION.... AGUARDE UN MOMENTO..." at 19,1
                     ATTRIBUTE(REVERSE)

             CALL genera()
             LET ejecuta = "chmod 777 ",v_arch
             RUN ejecuta

             DISPLAY "" AT 19,1
             DISPLAY "" AT 18,1
             DISPLAY "ARCHIVO GENERADO EN : ",v_arch AT 18,1
             PROMPT "PROCESO FINALIZADO...< ENTER > PARA CONTINUAR " FOR enter
         ELSE
            PROMPT "PROCESO CANCELADO...< ENTER > PARA CONTINUAR " FOR enter
         END IF
    CLOSE WINDOW COMB016 
END MAIN

FUNCTION genera()
   DEFINE  mandar    RECORD
              nss               CHAR(11),
              rfc               CHAR(13),
              paterno           CHAR(40),
              materno           CHAR(40),
              nombres           CHAR(40),
              fena              DATE,
              folio             CHAR(11),
              tipo_solicitud    SMALLINT,
              cod_promotor      CHAR(11),
              finicta           DATE,
              frecafor          DATE,
              interno_desc      CHAR(40),
              salario           DECIMAL(12,2),
              sexo_desc         CHAR(20),
              calle_afi         CHAR(40),
              numero_afi        CHAR(10),
              depto_afi         CHAR(10),
              colonia_afi       CHAR(60),
              codigo_postal_afi CHAR(5),
              c_reparto_afi     CHAR(5),
              delega_afi        CHAR(40),
              estado_afi        CHAR(40),
              ciudad_afi        CHAR(40),
              cve_lada_afi      CHAR(03),
              telefono_afi      CHAR(20),
              mail_afi          CHAR(20),
              reg_patronal      CHAR(11),
              rfc_patron        CHAR(13),
              agenc_cod         CHAR(10),
              paterno_pro       CHAR(40),
              materno_pro       CHAR(40),
              nombres_pro       CHAR(40),
              razon_social      CHAR(50),
              calle_patron      CHAR(40),
              numero_patron     CHAR(20),
              colonia_patron    CHAR(60),
              delega_patron     CHAR(40),
              ciudad_patron     CHAR(40),
              estado_patron     CHAR(40),
              codpos_patron     CHAR(05),
              telefono_patron   CHAR(15),
              f_ult_aporte      DATE,
              monto_ult_aporte  DECIMAL(22,6),
              monto_rcv         DECIMAL(22,6),
              monto_vol         DECIMAL(22,6),
              total_saldo       DECIMAL(22,6),
              ingreso_flu       DECIMAL(22,6),
              ingreso_saldo     DECIMAL(22,6),
              total_ingreso     DECIMAL(22,6),
              monto_comis       DECIMAL(12,2),
              pagada            CHAR(01),
              fecha_pago        DATE,
              ret_matri         DECIMAL(22,6),
              ret_desempleo     DECIMAL(22,6),
              ret_cesvej        DECIMAL(22,6),
              ret_vol           DECIMAL(22,6)
            END RECORD ,

           p_mae_afi    RECORD
              nss               CHAR(11),
              rfc               CHAR(13),
              paterno           CHAR(40),
              materno           CHAR(40),
              nombres           CHAR(40),
              fena              DATE,
              folio             CHAR(11),
              tipo_solicitud    SMALLINT,
              cod_promotor      CHAR(11),
              finicta           DATE,
              frecafor          DATE,
              status_interno    SMALLINT,
              salario           DECIMAL(12,2),
              sexo              SMALLINT
            END RECORD ,

            p_dom      RECORD
               calle             CHAR(40),
               numero            CHAR(10),
               depto             CHAR(10),
               colonia           CHAR(60),
               codigo_postal     CHAR(5),
               municip_deleg     INTEGER,
               estado            SMALLINT,
               poblacion         SMALLINT
            END RECORD,

            d_patron      RECORD    --LIKE tab_patron.*,
               razon_social         CHAR(50),
               calle                CHAR(40),
               numero               CHAR(20),
               colonia              CHAR(60),
               delega               CHAR(40),
               ciudad               CHAR(40),
               estado               CHAR(40),
               codpos               CHAR(05),
               telefono             CHAR(15)
            END RECORD,

            hora_ini, hora_fin      CHAR(08),
            registro                INTEGER,
            domi                    INTEGER,
            long, i                 INTEGER

   INITIALIZE p_mae_afi.*, p_dom.*,v_arch, hora_ini, hora_fin TO NULL
   LET domi = 0    LET long = 0   LET i = 0   LET registro = 0

   LET hora_ini = TIME
   DISPLAY " inicia : ",hora_ini AT 10,13

   LET v_arch = param.ruta_envio CLIPPED,"/",
##   LET v_arch = "/safre/com/fte/",
                usuario CLIPPED,
                "_PRO_ESP_",TODAY USING "ddmmyyyy",
                ".txt"

   START REPORT salida TO v_arch

   DECLARE apt CURSOR FOR
     SELECT a.n_seguro, a.n_rfc,   a.paterno,        a.materno,      a.nombres, 
            a.fena,     a.n_folio, a.tipo_solicitud, a.cod_promotor,
            a.finicta,  a.frecafor,a.status_interno, a.salario_actual, a.sexo
       FROM afi_mae_afiliado a
   FOREACH apt INTO p_mae_afi.*

         INITIALIZE mandar.* TO NULL

         LET domi = 0    LET long = 0   LET i = 0
{
##prueba
         IF registro = 100 THEN
            EXIT FOREACH
         ELSE
            LET registro = registro + 1
         END IF
}
         LET mandar.nss               = p_mae_afi.nss
         LET mandar.rfc               = p_mae_afi.rfc
         LET mandar.paterno           = p_mae_afi.paterno
         LET mandar.materno           = p_mae_afi.materno
         LET mandar.nombres           = p_mae_afi.nombres
         LET mandar.fena              = p_mae_afi.fena
         LET mandar.folio             = p_mae_afi.folio
         LET mandar.tipo_solicitud    = p_mae_afi.tipo_solicitud
         LET mandar.cod_promotor      = p_mae_afi.cod_promotor
         LET mandar.finicta           = p_mae_afi.finicta
         LET mandar.frecafor          = p_mae_afi.frecafor
         LET mandar.salario           = p_mae_afi.salario
##telefono
         SELECT MIN(f.ROWID)
         INTO   domi
         FROM   afi_telefono f
         WHERE  f.nss            = p_mae_afi.nss
         AND    f.tel_cod        = 1

         IF domi > 0 OR domi IS NOT NULL THEN
            SELECT f.telefono INTO mandar.telefono_afi FROM afi_telefono f
            WHERE  f.rowid    = domi
         ELSE
                LET mandar.telefono_afi = "                    "
         END IF

         LET domi = 0    LET long = 0   LET i = 0

         SELECT MIN(f.ROWID)
         INTO   domi
         FROM   afi_telefono f
         WHERE  f.nss            = p_mae_afi.nss
         AND    f.tel_cod        = 7

         IF domi > 0 OR domi IS NOT NULL THEN
            SELECT f.cve_lada, f.telefono 
              INTO mandar.cve_lada_afi ,
                   mandar.mail_afi 
              FROM afi_telefono f
            WHERE  f.rowid    = domi
         ELSE
            LET mandar.mail_afi = "                    "
         END IF
##termina telefono
##domicilio

         LET domi = 0    LET long = 0   LET i = 0

         SELECT MIN(s.ROWID)
           INTO  domi
           FROM  afi_domicilio s
          WHERE  s.nss            = p_mae_afi.nss
            AND  s.n_folio        = p_mae_afi.folio
            AND  s.tipo_solicitud = p_mae_afi.tipo_solicitud
            AND  s.marca_envio    = "X"

         IF SQLCA.SQLCODE = 0 THEN
            SELECT  o.calle,  o.numero, o.depto, o.colonia, o.codpos,
                    o.delega, o.estado, o.ciudad
              INTO  p_dom.*
              FROM  afi_domicilio o
             WHERE  o.rowid = domi

            IF STATUS != NOTFOUND THEN

               LET mandar.calle_afi         = p_dom.calle
               LET mandar.numero_afi        = p_dom.numero
               LET mandar.depto_afi         = p_dom.depto
               LET mandar.colonia_afi       = p_dom.colonia
               LET mandar.codigo_postal_afi = p_dom.codigo_postal

               IF p_dom.codigo_postal IS NULL OR
                  p_dom.codigo_postal     = " "  THEN
                  LET mandar.codigo_postal_afi  = "     "
                  LET mandar.c_reparto_afi = "     "
               ELSE
                 SELECT am.centro_reparto INTO mandar.c_reparto_afi
                        FROM  tab_reparto am
                        WHERE am.codigo_postal = p_dom.codigo_postal

                 IF mandar.c_reparto_afi IS NULL OR
                    mandar.c_reparto_afi     = " "   THEN
                    LET mandar.c_reparto_afi = "     "
                 END IF
               END IF
            END IF

            IF p_dom.estado IS NULL OR
               p_dom.estado =  " "  THEN
               LET mandar.estado_afi ="  "
            ELSE
                SELECT p.estad_desc INTO mandar.estado_afi FROM tab_estado p
                       WHERE p.estad_cod = p_dom.estado

                IF SQLCA.SQLCODE != 0 THEN
                   LET mandar.estado_afi = "  "
                END IF
            END IF

            IF p_dom.municip_deleg IS NULL OR
               p_dom.municip_deleg =  " "  THEN
               LET mandar.delega_afi = " "
            ELSE
                SELECT q.deleg_desc INTO mandar.delega_afi
                       FROM tab_delegacion q
                       WHERE q.deleg_cod = p_dom.municip_deleg

                 IF SQLCA.SQLCODE != 0 THEN
                    LET mandar.delega_afi = " "
                 END IF
            END IF

            IF p_dom.poblacion IS NULL OR
               p_dom.poblacion =  0    THEN
               LET mandar.ciudad_afi = " "
            ELSE
                SELECT q.ciudad_desc INTO mandar.ciudad_afi
                       FROM  tab_ciudad q
                       WHERE q.ciudad_cod = p_dom.poblacion
                IF (STATUS = NOTFOUND) THEN
                    LET mandar.ciudad_afi = " "
                END IF
            END IF
         END IF
## termina domicilio
## sexo
         IF p_mae_afi.sexo IS NULL OR
            p_mae_afi.sexo = " " THEN
            LET mandar.sexo_desc = "                    "
         ELSE
             SELECT m.sexo_desc INTO mandar.sexo_desc FROM tab_sexo m
             WHERE  m.sexo_cod = p_mae_afi.sexo
         END IF

## termina sexo
## status_interno

         SELECT m.estado_desc INTO mandar.interno_desc FROM tab_status_afi m
         WHERE  m.estado_cod = p_mae_afi.status_interno

## termina status_interno
## promotor

         SELECT p.agenc_cod, p.paterno, p.materno, p.nombres
           INTO mandar.agenc_cod,   mandar.paterno_pro, 
                mandar.materno_pro, mandar.nombres_pro 
           FROM pro_mae_promotor p
          WHERE p.cod_promotor = p_mae_afi.cod_promotor

## termina promotor
## PATRON

         SELECT g.reg_patronal, g.reg_fed_contrib
         INTO   mandar.reg_patronal, mandar.rfc_patron
         FROM   afi_patron g
         WHERE  g.n_seguro       = p_mae_afi.nss
         AND    g.n_folio        = p_mae_afi.folio 
         AND    g.tipo_solicitud = p_mae_afi.tipo_solicitud

         IF STATUS = NOTFOUND THEN
            LET mandar.reg_patronal   = "            "
            LET mandar.rfc_patron     = "              "
            LET mandar.razon_social   = " "
            LET mandar.calle_patron   = " "
            LET mandar.numero_patron  = " "
            LET mandar.colonia_patron = " "
            LET mandar.delega_patron  = " "
            LET mandar.ciudad_patron  = " "
            LET mandar.estado_patron  = " "
            LET mandar.codpos_patron  = " "
            LET mandar.telefono_patron= " "
         ELSE
            SELECT vv.razon_social, vv.calle, vv.numero, vv.colonia,
                   vv.delega, vv.ciudad, vv.estado, vv.codpos, vv.telefono
            INTO  d_patron.*
            FROM  tab_patron vv
            WHERE vv.reg_patronal = patron.reg_patronal

            SELECT vb.deleg_desc 
              INTO mandar.delega_patron 
              FROM tab_delegacion vb
             WHERE vb.deleg_cod = d_patron.delega

            IF mandar.delega_patron IS NULL THEN
               LET mandar.delega_patron = " "
            END IF

            SELECT vg.ciudad_desc INTO mandar.ciudad_patron FROM tab_ciudad vg
             WHERE vg.ciudad = vg.ciudad_cod
            IF mandar.ciudad_patron IS NULL THEN
               LET mandar.ciudad_patron = " "
            END IF

            SELECT vh.estad_desc INTO mandar.estado_patron FROM tab_estado vh
             WHERE vh.ciudad = vh.ciudad_cod
            IF mandar.estado_patron IS NULL THEN
               LET mandar.estado_patron = " "
            END IF

            LET mandar.razon_social    = d_patron.razon_social
            LET mandar.calle_patron    = d_patron.calle
            LET mandar.numero_patron   = d_patron.numero
            LET mandar.colonia_patron  = d_patron.colonia
            LET mandar.codpos_patron   = d_patron.codpos
            LET mandar.telefono_patron = d_patron.telefono
         END IF
## termina patron
## aporte
         INITIALIZE mandar.f_ult_aporte TO NULL
         LET mandar.monto_ult_aporte = 0

         SELECT hh.fecha_ult_general INTO mandar.f_ult_aporte 
                FROM   cta_ctr_cuenta hh
                WHERE  hh.nss   = p_mae_afi.nss

         SELECT SUM(gj.monto_en_pesos) INTO mandar.monto_ult_aporte 
           FROM dis_cuenta gj
          WHERE gj.fecha_conversion = mandar.f_ult_aporte
            AND gj.nss              = p_mae_afi.nss
            AND gj.subcuenta        NOT IN(4,8)
            AND gj.tipo_movimiento  = 1

         IF mandar.monto_ult_aporte IS NULL OR mandar.monto_ult_aporte = 0 THEN
            LET mandar.monto_ult_aporte = 0
         END IF
        
## termina aporte
## rcv y vol
         LET mandar.monto_rcv      = 0
         LET mandar.monto_vol      = 0
         LET mandar.total_saldo    = 0

         SELECT SUM(gj.monto_en_pesos) INTO mandar.monto_rcv 
           FROM dis_cuenta gj
          WHERE gj.nss              = p_mae_afi.nss
            AND gj.subcuenta        IN(1,2,6,9)
            AND gj.tipo_movimiento  IN(1,2,3,4)
         IF mandar.monto_rcv IS NULL OR mandar.monto_rcv = 0 THEN
            LET mandar.monto_rcv = 0
         END IF

         SELECT SUM(gj.monto_en_pesos) INTO mandar.monto_vol
           FROM dis_cuenta gj
          WHERE gj.nss              = p_mae_afi.nss
            AND gj.subcuenta        IN(3,10)
            AND gj.tipo_movimiento  IN(1)
         IF mandar.monto_vol IS NULL OR mandar.monto_vol = 0 THEN
            LET mandar.monto_vol = 0
         END IF

         LET mandar.total_saldo = mandar.monto_rcv + mandar.monto_vol
## termina rcv y vol

## ingresos
         LET mandar.ingreso_flu    = 0
         LET mandar.ingreso_saldo  = 0
         LET mandar.total_ingreso  = 0

         SELECT SUM(gj.monto_en_pesos) INTO mandar.ingreso_flu 
           FROM dis_cuenta gj
          WHERE gj.nss              = p_mae_afi.nss
            AND gj.subcuenta        IN(1,2,6,9)
            AND gj.tipo_movimiento  IN(100,104,106)
         IF mandar.ingreso_flu IS NULL OR mandar.ingreso_flu = 0 THEN
            LET mandar.ingreso_flu = 0
         END IF

         SELECT SUM(gj.monto_en_pesos) INTO mandar.ingreso_saldo
           FROM dis_cuenta gj
          WHERE gj.nss              = p_mae_afi.nss
            AND gj.subcuenta        IN(1,3,7)
            AND gj.tipo_movimiento  IN(110)
         IF mandar.ingreso_saldo IS NULL OR mandar.ingreso_saldo = 0 THEN
            LET mandar.ingreso_saldo = 0
         END IF

         LET mandar.total_ingreso = mandar.ingreso_flu + mandar.ingreso_saldo
## termina ingresos
## comision
         LET mandar.monto_comis = 0
         INITIALIZE mandar.pagada, mandar.fecha_pago TO NULL

         SELECT f.monto_comision, f.comis_pagada, f.fecha_pago 
           INTO mandar.monto_comis, mandar.pagada, mandar.fecha_pago
           FROM com_comis_detalle f
          WHERE  f.nss            = p_mae_afi.nss
            AND  f.n_folio        = p_mae_afi.folio
            AND  f.tipo_solicitud = p_mae_afi.tipo_solicitud
         IF mandar.monto_comis IS NULL OR mandar.monto_comis = 0 THEN
            LET mandar.monto_comis = 0
         END IF
## termina comision
## retiros
   ##Ret.Parcial
         LET mandar.ret_matri = 0
         LET mandar.ret_desempleo = 0
         LET mandar.ret_cesvej = 0

         SELECT SUM(m.monto_en_pesos) INTO mandar.ret_matri FROM dis_cuenta m
          WHERE m.nss             = p_mae_afi.nss
            AND m.subcuenta       = 5
            AND m.tipo_movimiento IN(487)
         IF mandar.ret_matri IS NULL OR mandar.ret_matri = 0 THEN
            LET mandar.ret_matri = 0
         END IF

         SELECT SUM(m.monto_en_pesos) INTO mandar.ret_desempleo FROM dis_cuenta m
          WHERE m.nss             = p_mae_afi.nss
            AND m.subcuenta       IN(1,2,5,6,9)
            AND m.tipo_movimiento IN(486)
         IF mandar.ret_desempleo IS NULL OR mandar.ret_desempleo = 0 THEN
            LET mandar.ret_desempleo = 0
         END IF

   ##Ret.Totales

         SELECT SUM(m.monto_en_pesos) INTO mandar.ret_cesvej FROM dis_cuenta m
          WHERE m.nss             = p_mae_afi.nss
            AND m.subcuenta       IN(1,2,4,5,6,9)
            AND m.tipo_movimiento BETWEEN 457 AND 485
         IF mandar.ret_cesvej IS NULL OR mandar.ret_cesvej = 0 THEN
            LET mandar.ret_cesvej = 0
         END IF

   ##Ret.Voluntario
         SELECT SUM(m.monto_en_pesos) INTO mandar.ret_vol FROM dis_cuenta m
          WHERE m.nss             = p_mae_afi.nss
            AND m.subcuenta       IN(3)   --10 para retencion hasta que lo pida
            AND m.tipo_movimiento = 490
         IF mandar.ret_vol IS NULL OR mandar.ret_vol = 0 THEN
            LET mandar.ret_vol = 0
         END IF
## termina retiros

         OUTPUT TO REPORT salida(mandar.*)
   END FOREACH
   LET hora_fin = TIME
   DISPLAY "Finaliza : ",hora_fin AT 10,35
END FUNCTION

REPORT salida(recibe)

   DEFINE  recibe   RECORD
              nss               CHAR(11),
              rfc               CHAR(13),
              paterno           CHAR(40),
              materno           CHAR(40),
              nombres           CHAR(40),
              fena              DATE,
              folio             CHAR(11),
              tipo_solicitud    SMALLINT,
              cod_promotor      CHAR(11),
              finicta           DATE,
              frecafor          DATE,
              interno_desc      CHAR(40),
              salario           DECIMAL(12,2),
              sexo_desc         CHAR(20),
              calle_afi         CHAR(40),
              numero_afi        CHAR(10),
              depto_afi         CHAR(10),
              colonia_afi       CHAR(60),
              codigo_postal_afi CHAR(5),
              c_reparto_afi     CHAR(5),
              delega_afi        CHAR(40),
              estado_afi        CHAR(40),
              ciudad_afi        CHAR(40),
              cve_lada_afi      CHAR(03),
              telefono_afi      CHAR(20),
              mail_afi          CHAR(20),
              reg_patronal      CHAR(11),
              rfc_patron        CHAR(13),
              agenc_cod         CHAR(10),
              paterno_pro       CHAR(40),
              materno_pro       CHAR(40),
              nombres_pro       CHAR(40),
              razon_social      CHAR(50),
              calle_patron      CHAR(40),
              numero_patron     CHAR(20),
              colonia_patron    CHAR(60),
              delega_patron     CHAR(40),
              ciudad_patron     CHAR(40),
              estado_patron     CHAR(40),
              codpos_patron     CHAR(05),
              telefono_patron   CHAR(15),
              f_ult_aporte      DATE,
              monto_ult_aporte  DECIMAL(22,6),
              monto_rcv         DECIMAL(22,6),
              monto_vol         DECIMAL(22,6),
              total_saldo       DECIMAL(22,6),
              ingreso_flu       DECIMAL(22,6),
              ingreso_saldo     DECIMAL(22,6),
              total_ingreso     DECIMAL(22,6),
              monto_comis       DECIMAL(12,2),
              pagada            CHAR(01),
              fecha_pago        DATE,
              ret_matri         DECIMAL(22,6),
              ret_desempleo     DECIMAL(22,6),
              ret_cesvej        DECIMAL(22,6),
              ret_vol           DECIMAL(22,6)
           END RECORD

     OUTPUT
          TOP MARGIN 0
          BOTTOM MARGIN 0
          LEFT MARGIN   0
          RIGHT MARGIN  0
##          PAGE LENGTH  60
     FORMAT
{
       PAGE HEADER
           PRINT COLUMN 01, "PUNTO VENTA"            ,"|",
                            "NO.PROMOTOR"            ,"|",
                            "PATERNO PRO"            ,"|",
                            "MATERNO PRO"            ,"|",
                            "NOMBRES PRO"            ,"|",
                            "TIPO SOLICITUD"         ,"|",
                            "FOLIO"                  ,"|",
                            "NSS"                    ,"|",
                            "PATERNO AFI"            ,"|",
                            "MATERNO AFI"            ,"|",
                            "NOMBRES AFI"            ,"|",
                            "F.NACIM"                ,"|",
                            "RFC AFI"                ,"|",
                            "SEXO"                   ,"|",
                            "CALLE AFI "             ,"|",
                            "NUM.EXT.AFI"            ,"|",
                            "NUM.INT.AFI"            ,"|",
                            "COLONIA AFI"            ,"|",
                            "COD.POS.AFI"            ,"|",
                            "C.REPARTO AFI"          ,"|",
                            "DELEGA.AFI"             ,"|",
                            "ESTADO.AFI"             ,"|",
                            "CIUDAD.AFI"             ,"|",
                            "CVE.LADA AFI"           ,"|",
                            "TELEFONO AFI"           ,"|",
                            "MAIL AFI"               ,"|",
                            "REG.PATRONAL"           ,"|",
                            "RFC.PATRON"             ,"|",
                            "RAZON SOCIAL"           ,"|",
                            "CALLE PATRON"           ,"|",
                            "NUMERO PATRON"          ,"|",
                            "COLONIA PATRON"         ,"|",
                            "DELEGA PATRON"          ,"|",
                            "CIUDAD PATRON"          ,"|",
                            "ESTADO PATRON"          ,"|",
                            "COD.POS.PATRON"         ,"|",
                            "TELEFONO PATRON"        ,"|",
                            "F.CAPTURA"              ,"|",
                            "F.APERTURA"             ,"|",
                            "STATUS SOLICITUD"       ,"|",
                            "F.ULT.APORTE"           ,"|",
                            "MONTO.ULT.APOR"         ,"|",
                            "SDI"                    ,"|",
                            "SALDO RCV"              ,"|",
                            "SALDO VOL"              ,"|",
                            "TOTAL SALDO"            ,"|",
                            "ING.FLUJO"              ,"|",
                            "ING.SALDO"              ,"|",
                            "TOTAL ING."             ,"|",
                            "COMIS.POR PAGAR"        ,"|",
                            "ESTADO COMIS"           ,"|",
                            "FECHA PAGO"             ,"|",
                            "RET.MATRIMONIO"         ,"|",
                            "RET.DESEMPLEO"          ,"|",
                            "RET.CES.VEJ."           ,"|",
                            "RET.VOL."               ,"|"
}
       ON EVERY ROW

           PRINT COLUMN 01, recibe.agenc_cod         ,"|",
                            recibe.cod_promotor      ,"|",
                            recibe.paterno_pro       ,"|",
                            recibe.materno_pro       ,"|",
                            recibe.nombres_pro       ,"|",
                            recibe.tipo_solicitud    ,"|",
                            recibe.folio             ,"|",
                            recibe.nss               ,"|",
                            recibe.paterno           ,"|",
                            recibe.materno           ,"|",
                            recibe.nombres           ,"|",
                            recibe.fena              ,"|",
                            recibe.rfc               ,"|",
                            recibe.sexo_desc         ,"|",
                            recibe.calle_afi         ,"|",
                            recibe.numero_afi        ,"|",
                            recibe.depto_afi         ,"|",
                            recibe.colonia_afi       ,"|",
                            recibe.codigo_postal_afi ,"|",
                            recibe.c_reparto_afi     ,"|",
                            recibe.delega_afi        ,"|",
                            recibe.estado_afi        ,"|",
                            recibe.ciudad_afi        ,"|",
                            recibe.cve_lada_afi      ,"|",
                            recibe.telefono_afi      ,"|",
                            recibe.mail_afi          ,"|",
                            recibe.reg_patronal      ,"|",
                            recibe.rfc_patron        ,"|",
                            recibe.razon_social      ,"|",
                            recibe.calle_patron      ,"|",
                            recibe.numero_patron     ,"|",
                            recibe.colonia_patron    ,"|",
                            recibe.delega_patron     ,"|",
                            recibe.ciudad_patron     ,"|",
                            recibe.estado_patron     ,"|",
                            recibe.codpos_patron     ,"|",
                            recibe.telefono_patron   ,"|",
                            recibe.frecafor          ,"|",
                            recibe.finicta           ,"|",
                            recibe.interno_desc      ,"|",
                            recibe.f_ult_aporte      ,"|",
                            recibe.monto_ult_aporte  ,"|",
                            recibe.salario           ,"|",
                            recibe.monto_rcv         ,"|",
                            recibe.monto_vol         ,"|",
                            recibe.total_saldo       ,"|",
                            recibe.ingreso_flu       ,"|",
                            recibe.ingreso_saldo     ,"|",
                            recibe.total_ingreso     ,"|",
                            recibe.monto_comis       ,"|",
                            recibe.pagada            ,"|",
                            recibe.fecha_pago        ,"|",
                            recibe.ret_matri         ,"|",
                            recibe.ret_desempleo     ,"|",
                            recibe.ret_cesvej        ,"|",
                            recibe.ret_vol           ,"|"
END REPORT
