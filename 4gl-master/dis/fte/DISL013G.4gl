###############################################################################
#Proyecto          => SAFRE ( MEXICO )                                        #
#Owner             => E.F.P.                                                  #
#Programa DISL013G => Reporte de Relacion de Ctas. CAMBIO REG.                #
#Fecha             => 26/08/2011                                              #
#By                => DMR                                                     #
#Actualizacion     => 26 Ago 2011                                             #
#By                => DMR                                                     #
#Descripcion       =>                                                         #
#Sistema           => DIS                                                     #
###############################################################################
DATABASE safre_af

GLOBALS
   DEFINE
      g_reg                RECORD LIKE dis_det_devorec.*,

      paramgrales          RECORD LIKE int_parametro.*,
      g_param              RECORD LIKE dis_parametro.*,
      ho_y                 DATE,
      vfolio               INTEGER,
      sw                   SMALLINT,
      opc                  CHAR(01),
      enter                CHAR(01),
      nombre               CHAR(50),
      respuesta            CHAR(01),
      ejecuta              CHAR(100),
      G_LISTA              CHAR(100),
      t_monto_ahoret       DECIMAL(18,6), 
      t_monto_retredi      DECIMAL(18,6), 
      t_monto_retapor      DECIMAL(18,6),
      t_monto_cvtra        DECIMAL(18,6),
      t_monto_cvdep        DECIMAL(18,6),
      t_monto_cuosoc       DECIMAL(18,6),
      t_monto_ahosoldep    DECIMAL(18,6),
      t_monto_ahosoltra    DECIMAL(18,6),
      t_monto_bonopen      DECIMAL(18,6),
      t_monto_rem40        DECIMAL(18,6),
      t_monto_rem41        DECIMAL(18,6),
      num_reg              INTEGER,
      usuario              CHAR(08),
      hoy                  DATE,
      hora                 CHAR(08)

   DEFINE
      reg_nom  RECORD
                  nss      CHAR(11)
               END RECORD
END GLOBALS


MAIN
   OPTIONS INPUT  WRAP,
           PROMPT LINE LAST ,
           ERROR  LINE LAST -2,
           ACCEPT KEY CONTROL-I,
           FORM   LINE 3         

   CALL init()

   OPEN WINDOW disl013_ven AT 2,2 WITH FORM "DISL013G1" ATTRIBUTE(BORDER)

   DISPLAY " < ESC > Aceptar                                       < Ctrl-C > Cancelar   " AT 1,1 ATTRIBUTE(REVERSE)

   DISPLAY " DISL013G  LISTADO DE CUENTAS DEL PROCESO DE CAMBIO DE REGIMEN               " AT 3,1 ATTRIBUTE(REVERSE)

   DISPLAY ho_y USING "DD/MM/YYYY" AT 3,65 ATTRIBUTE(REVERSE)

   INPUT vfolio FROM FORMONLY.vfolio
      AFTER FIELD vfolio
         IF vfolio IS NULL OR vfolio = " " THEN
            ERROR "EL FOLIO DEL PROCESO NO puede ser NULO o BLANCO"
            NEXT FIELD vfolio
         ELSE
            SELECT UNIQUE "a.X"  FROM dis_cza_devorec a
            WHERE a.folio = vfolio

            IF STATUS = NOTFOUND THEN
               ERROR " NO EXISTE INFORMACION PARA ESTE FOLIO "
               NEXT FIELD vfolio
            END IF
         END IF

      ON KEY ( ESC)
         IF vfolio IS NULL OR vfolio = " " THEN
            ERROR "EL FOLIO DEL PROCESO NO puede ser NULO o BLANCO"
            NEXT FIELD vfolio
         END IF

         SELECT UNIQUE "a.X"  FROM dis_cza_devorec a
         WHERE a.folio = vfolio

         IF STATUS = NOTFOUND THEN
            ERROR " NO EXISTE INFORMACION PARA ESTE FOLIO "
            NEXT FIELD vfolio
         END IF

         LET sw = 0
         EXIT INPUT

      ON KEY ( CONTROL-C )
         LET sw = 1
         EXIT INPUT

   END INPUT

   IF sw = 1 THEN
      ERROR ""
      PROMPT " PROCESO CANCELADO...< ENTER > PARA CONTINUAR..." FOR enter
   ELSE
      LET hora = TIME
      LET G_LISTA = g_param.ruta_spool CLIPPED,"/",usuario CLIPPED,"-DISL013G.",
                    hoy USING "DDMMYYYY"

      START REPORT listado_ctas TO G_LISTA
         CALL genera()
      FINISH REPORT listado_ctas

      LET ejecuta = "chmod 777 ", G_LISTA CLIPPED
      RUN ejecuta

      DISPLAY " REPORTE GENERADO EN : ",G_LISTA  AT 17,1
      ERROR ""
      PROMPT " PROCESO FINALIZADO...< ENTER > PARA CONTINUAR..." FOR enter
   END IF

   CLOSE WINDOW disl013_ven
END MAIN


############################################################################
FUNCTION init()
   INITIALIZE g_reg.*, HOY, ho_y, reg_nom.*, respuesta, nombre, opc TO NULL
   INITIALIZE usuario TO NULL

   LET t_monto_ahoret     = 0
   LET t_monto_retredi    = 0
   LET t_monto_retapor    = 0
   LET t_monto_cvtra      = 0
   LET t_monto_cvdep      = 0
   LET t_monto_cuosoc     = 0
   LET t_monto_ahosoldep  = 0
   LET t_monto_ahosoltra  = 0
   LET t_monto_bonopen    = 0
   LET t_monto_rem40      = 0
   LET t_monto_rem41      = 0
   LET num_reg            = 0

   LET ho_y = TODAY

   SELECT user
   INTO   usuario
   FROM   glo_parametro

   SELECT *
   INTO   g_param.*
   FROM   dis_parametro

   LET hoy = TODAY
END FUNCTION


############################################################################
FUNCTION genera()
DEFINE
   reg2  RECORD
            aho_ret       DECIMAL(18,6),
            ret_redi      DECIMAL(18,6),
            ret_apor      DECIMAL(18,6),
            cv_tra        DECIMAL(18,6),
            cv_dep        DECIMAL(18,6),
            cuo_soc       DECIMAL(18,6),
            ahosol_dep    DECIMAL(18,6),
            ahosol_tra    DECIMAL(18,6),
            bono_pen      DECIMAL(18,6),
            rem_40        DECIMAL(18,6),
            rem_41        DECIMAL(18,6)
         END RECORD

   ERROR "GENERANDO INFORMACION... AGUARDE UN MOMENTO..."
 
   DECLARE apt_ctas_1 CURSOR FOR
   SELECT a.*
   FROM  dis_det_devorec a
   WHERE a.folio = vfolio
   AND   a.result_operacion <> "02"

   FOREACH apt_ctas_1 INTO g_reg.*
      SELECT b.n_seguro INTO reg_nom.*
      FROM   afi_mae_afiliado b
      WHERE  b.n_unico = g_reg.n_unico
      AND    b.tipo_solicitud = 8

      SELECT NVL(sum(monto_en_acciones),0)
      INTO   reg2.aho_ret
      FROM   dis_provision
      WHERE  folio = vfolio
      AND    nss   = reg_nom.nss
      AND    subcuenta = 13

      SELECT NVL(sum(monto_en_acciones),0)
      INTO   reg2.ret_redi
      FROM   dis_provision
      WHERE  folio = vfolio
      AND    nss   = reg_nom.nss
      AND    subcuenta = 30
      AND    tipo_movimiento = 89

      SELECT NVL(sum(monto_en_acciones),0)
      INTO   reg2.ret_apor
      FROM   dis_provision
      WHERE  folio = vfolio
      AND    nss   = reg_nom.nss
      AND    subcuenta = 30
      AND    tipo_movimiento = 87

      SELECT NVL(sum(monto_en_acciones),0)
      INTO   reg2.cv_tra
      FROM   dis_provision
      WHERE  folio = vfolio
      AND    nss   = reg_nom.nss
      AND    subcuenta = 31
      AND    tipo_movimiento = 89
      AND    id_aportante    = "APOR-TRAB"

      SELECT NVL(sum(monto_en_acciones),0)
      INTO   reg2.cv_dep
      FROM   dis_provision
      WHERE  folio = vfolio
      AND    nss   = reg_nom.nss
      AND    subcuenta = 31
      AND    tipo_movimiento = 89
      AND    id_aportante   <> "APOR-TRAB"

      SELECT NVL(sum(monto_en_acciones),0)
      INTO   reg2.cuo_soc
      FROM   dis_provision
      WHERE  folio = vfolio
      AND    nss   = reg_nom.nss
      AND    subcuenta = 32
      AND    tipo_movimiento = 89

      SELECT NVL(sum(monto_en_acciones),0)
      INTO   reg2.ahosol_dep
      FROM   dis_provision
      WHERE  folio = vfolio
      AND    nss   = reg_nom.nss
      AND    subcuenta = 33
      AND    tipo_movimiento = 89

      SELECT NVL(sum(monto_en_acciones),0)
      INTO   reg2.ahosol_tra
      FROM   dis_provision
      WHERE  folio = vfolio
      AND    nss   = reg_nom.nss
      AND    subcuenta = 34
      AND    tipo_movimiento = 87

      SELECT NVL(sum(monto_en_acciones),0)
      INTO   reg2.bono_pen
      FROM   dis_provision
      WHERE  folio = vfolio
      AND    nss   = reg_nom.nss
      AND    subcuenta = 36
      AND    tipo_movimiento = 89

      SELECT NVL(sum(monto_en_acciones),0)
      INTO   reg2.rem_40
      FROM   dis_provision
      WHERE  folio = vfolio
      AND    nss   = reg_nom.nss
      AND    subcuenta = 40
      AND    tipo_movimiento = 89

      SELECT NVL(sum(monto_en_acciones),0)
      INTO   reg2.rem_41
      FROM   dis_provision
      WHERE  folio = vfolio
      AND    nss   = reg_nom.nss
      AND    subcuenta = 41
      AND    tipo_movimiento = 89

      LET t_monto_ahoret     = t_monto_ahoret     + reg2.aho_ret
      LET t_monto_retredi    = t_monto_retredi    + reg2.ret_redi
      LET t_monto_retapor    = t_monto_retapor    + reg2.ret_apor
      LET t_monto_cvtra      = t_monto_cvtra      + reg2.cv_tra
      LET t_monto_cvdep      = t_monto_cvdep      + reg2.cv_dep
      LET t_monto_cuosoc     = t_monto_cuosoc     + reg2.cuo_soc
      LET t_monto_ahosoldep  = t_monto_ahosoldep  + reg2.ahosol_dep
      LET t_monto_ahosoltra  = t_monto_ahosoltra  + reg2.ahosol_tra
      LET t_monto_bonopen    = t_monto_bonopen    + reg2.bono_pen
      LET t_monto_rem40      = t_monto_rem40      + reg2.rem_40
      LET t_monto_rem41      = t_monto_rem41      + reg2.rem_41

      LET num_reg = num_reg + 1
      LET g_reg.n_seguro = reg_nom.nss

      OUTPUT TO REPORT listado_ctas(g_reg.*,
                                    reg2.*,
                                    num_reg,
                                    t_monto_ahoret,
                                    t_monto_retredi,
                                    t_monto_retapor,
                                    t_monto_cvtra,
                                    t_monto_cvdep,
                                    t_monto_cuosoc,
                                    t_monto_ahosoldep,
                                    t_monto_ahosoltra,
                                    t_monto_bonopen,
                                    t_monto_rem40,
                                    t_monto_rem41)

      INITIALIZE reg_nom.* TO NULL
   END FOREACH
END FUNCTION


############################################################################
REPORT listado_ctas(g_regp, reg2_p, num_reg, t_monto_ahoret,
                    t_monto_retredi,    t_monto_retapor,
                    t_monto_cvtra,      t_monto_cvdep,
                    t_monto_cuosoc,     t_monto_ahosoldep,
                    t_monto_ahosoltra,  t_monto_bonopen,
                    t_monto_rem40,      t_monto_rem41)
#cl-----------------

DEFINE
   g_regp   RECORD LIKE      dis_det_devorec.*,
   reg2_p   RECORD
               aho_ret       DECIMAL(18,6),
               ret_redi      DECIMAL(18,6),
               ret_apor      DECIMAL(18,6),
               cv_tra        DECIMAL(18,6),
               cv_dep        DECIMAL(18,6),
               cuo_soc       DECIMAL(18,6),
               ahosol_dep    DECIMAL(18,6),
               ahosol_tra    DECIMAL(18,6),
               bono_pen      DECIMAL(18,6),
               rem_40        DECIMAL(18,6),
               rem_41        DECIMAL(18,6)
            END RECORD

   DEFINE
      num_reg                INTEGER,
      t_monto_ahoret         DECIMAL(18,6),
      t_monto_retredi        DECIMAL(18,6),
      t_monto_retapor        DECIMAL(18,6),
      t_monto_cvtra          DECIMAL(18,6),
      t_monto_cvdep          DECIMAL(18,6),
      t_monto_cuosoc         DECIMAL(18,6),
      t_monto_ahosoldep      DECIMAL(18,6),
      t_monto_ahosoltra      DECIMAL(18,6),
      t_monto_bonopen        DECIMAL(18,6),
      t_monto_rem40          DECIMAL(18,6),
      t_monto_rem41          DECIMAL(18,6),
      L1                     CHAR(01),
      L2                     CHAR(02),
      L3                     CHAR(03),
      L4                     CHAR(04),
      L5                     CHAR(05),
      L6                     CHAR(06),
      L7                     CHAR(07),
      L8                     CHAR(08),
      L9                     CHAR(09),
      L10                    CHAR(10),
      L11                    CHAR(11)

   OUTPUT
      LEFT MARGIN    0
      RIGHT MARGIN   0
      TOP MARGIN     0
      BOTTOM MARGIN  0
      PAGE LENGTH   60

   FORMAT
      PAGE HEADER
         LET L1  = "\304"
         LET L2  = "\304\304"
         LET L3  = "\304\304\304"
         LET L4  = "\304\304\304\304"
         LET L5  = "\304\304\304\304\304"
         LET L6  = "\304\304\304\304\304\304"
         LET L7  = "\304\304\304\304\304\304\304"
         LET L8  = "\304\304\304\304\304\304\304\304"
         LET L9  = "\304\304\304\304\304\304\304\304\304"
         LET L10 = "\304\304\304\304\304\304\304\304\304\304"
         LET L11 = "\304\304\304\304\304\304\304\304\304\304\304"

         PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d\033(s12H'

         PRINT COLUMN 23, "REPORTE DE CUENTAS ENVIADAS COMO ",
                          "DEVOLUCION POR CAMBIO DE REGIMEN DE PENSION ",
               COLUMN 112,"pagina : ",PAGENO USING "###"

         PRINT
         PRINT COLUMN 02,"FOLIO PROCESO : ",vfolio USING "########",
               COLUMN 116,"DISL013G"
         PRINT 
         PRINT '\033e\033(s218T\033(s17H\033(s7B'

         PRINT COLUMN 01,"\332",L10,L10,L10,
                         "\302",L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,L10
                               ,L10,L10,L10,L10,L10,L10,L10,L10,L4,L2,
                         "\277"

         PRINT COLUMN  01,"|",
               COLUMN  07,"T R A B A J A D O R",
               COLUMN  32,"|",
               COLUMN 104,"S A L D O S     E N     T I T U L O S",
               COLUMN 239,"|"

         PRINT COLUMN 01,"\300",L10,L10,L10,
                         "\301",L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,L10
                               ,L10,L10,L10,L10,L10,L10,L10,L10,L4,L2,
                         "\331"

         PRINT COLUMN  01, "|",
               COLUMN  20, "|",
               COLUMN  32, "|",
               COLUMN  35, "AHORRO PARA EL",
               COLUMN  51, "|",
               COLUMN  59, "R E T I R O    2 0 0 8",
               COLUMN  89, "|",
               COLUMN 108, "|",
               COLUMN 127, "|",
               COLUMN 146, "|",
               COLUMN 148, "AHORRO SOLIDARIO",
               COLUMN 165, "|",
               COLUMN 167, "AHORRO SOLIDARIO",
               COLUMN 184, "|",
               COLUMN 203, "|",
               COLUMN 221, "|",
               COLUMN 239, "|"

         PRINT COLUMN  01, "|",
               COLUMN  07, "C U R P",
               COLUMN  20, "|",
               COLUMN  24, "N T I",
               COLUMN  32, "|",
               COLUMN  39, "RETIRO",
               COLUMN  51, "|",
               COLUMN  54, "BONO DE PENSION",
               COLUMN  75, "APORTACIONES",
               COLUMN  89, "|",
               COLUMN  92, "CV  TRABAJADOR",
               COLUMN 108, "|",
               COLUMN 111, "CV DEPENDENCIA",
               COLUMN 127, "|",
               COLUMN 131, "CUOTA SOCIAL",
               COLUMN 146, "|",
               COLUMN 152, "PATRONAL",
               COLUMN 165, "|",
               COLUMN 170, "TRABAJADOR",
               COLUMN 184, "|",
               COLUMN 186, "BONO DE PENSION",
               COLUMN 203, "|",
               COLUMN 204, "REMANENTE OPE 70%",
               COLUMN 221, "|",
               COLUMN 222, "REMANENTE OPE 20%",
               COLUMN 239, "|"

         PRINT COLUMN 01,"\300",L10,L5,L1,L1,L1,
                         "\301",L10,L1,
                         "\301",L10,L5,L1,L1,L1,
                         "\301",L10,L5,L1,L1,L1,
                         "\301",L10,L5,L1,L1,L1,
                         "\301",L10,L5,L1,L1,L1,
                         "\301",L10,L5,L1,L1,L1,
                         "\301",L10,L5,L1,L1,L1,
                         "\301",L10,L5,L1,L1,L1,
                         "\301",L10,L5,L1,L1,L1,
                         "\301",L10,L5,L1,L1,L1,
                         "\301",L10,L5,L1,L1,
                         "\301",L10,L5,L1,L1,
                         "\331"

   ON EVERY ROW
      PRINT COLUMN  02,g_regp.n_unico,
            COLUMN  21,g_regp.n_seguro,
            COLUMN  33,reg2_p.aho_ret     USING "##########&.&&&&&&",
            COLUMN  52,reg2_p.ret_redi    USING "##########&.&&&&&&",
            COLUMN  71,reg2_p.ret_apor    USING "##########&.&&&&&&",
            COLUMN  90,reg2_p.cv_tra      USING "##########&.&&&&&&",
            COLUMN 109,reg2_p.cv_dep      USING "##########&.&&&&&&",
            COLUMN 128,reg2_p.cuo_soc     USING "##########&.&&&&&&",
            COLUMN 147,reg2_p.ahosol_dep  USING "##########&.&&&&&&",
            COLUMN 166,reg2_p.ahosol_tra  USING "##########&.&&&&&&",
            COLUMN 185,reg2_p.bono_pen    USING "##########&.&&&&&&",
            COLUMN 204,reg2_p.rem_40      USING "#########&.&&&&&&",
            COLUMN 222,reg2_p.rem_41      USING "#########&.&&&&&&"

   ON LAST ROW
      SKIP 2 LINES
      PRINT COLUMN 01,"\332",L10,L5,L1,L1,L1,
                      "\302",L10,L1,
                      "\302",L10,L5,L1,L1,L1,
                      "\302",L10,L5,L1,L1,L1,
                      "\302",L10,L5,L1,L1,L1,
                      "\302",L10,L5,L1,L1,L1,
                      "\302",L10,L5,L1,L1,L1,
                      "\302",L10,L5,L1,L1,L1,
                      "\302",L10,L5,L1,L1,L1,
                      "\302",L10,L5,L1,L1,L1,
                      "\302",L10,L5,L1,L1,L1,
                      "\302",L10,L5,L1,L1,
                      "\302",L10,L5,L1,L1,
                      "\277"

      PRINT COLUMN 01,"|",
            COLUMN 08,"T O T A L E S : ",num_reg USING "######&",
            COLUMN 32,"|",
            COLUMN 33, t_monto_ahoret      USING "##########&.&&&&&&",
            COLUMN 51,"|",
            COLUMN 52, t_monto_retredi     USING "##########&.&&&&&&",
            COLUMN 70,"|",
            COLUMN 71, t_monto_retapor     USING "##########&.&&&&&&",
            COLUMN 89,"|",
            COLUMN 90, t_monto_cvtra       USING "##########&.&&&&&&",
            COLUMN 108,"|",
            COLUMN 109,t_monto_cvdep       USING "##########&.&&&&&&",
            COLUMN 127,"|",
            COLUMN 128,t_monto_cuosoc      USING "##########&.&&&&&&",
            COLUMN 146,"|",
            COLUMN 147,t_monto_ahosoldep   USING "##########&.&&&&&&",
            COLUMN 165,"|",
            COLUMN 166,t_monto_ahosoltra   USING "##########&.&&&&&&",
            COLUMN 184,"|",
            COLUMN 185,t_monto_bonopen     USING "##########&.&&&&&&",
            COLUMN 202,"|",
            COLUMN 203,t_monto_rem40       USING "#########&.&&&&&&",
            COLUMN 221,"|",
            COLUMN 222,t_monto_rem41       USING "#########&.&&&&&&",
            COLUMN 239,"|"

      PRINT COLUMN 01,"\300",L10,L5,L1,L1,L1,
                      "\301",L10,L1,
                      "\301",L10,L5,L1,L1,L1,
                      "\301",L10,L5,L1,L1,L1,
                      "\301",L10,L5,L1,L1,L1,
                      "\301",L10,L5,L1,L1,L1,
                      "\301",L10,L5,L1,L1,L1,
                      "\301",L10,L5,L1,L1,L1,
                      "\301",L10,L5,L1,L1,L1,
                      "\301",L10,L5,L1,L1,L1,
                      "\301",L10,L5,L1,L1,L1,
                      "\301",L10,L5,L1,L1,
                      "\301",L10,L5,L1,L1,
                      "\331"
END REPORT

