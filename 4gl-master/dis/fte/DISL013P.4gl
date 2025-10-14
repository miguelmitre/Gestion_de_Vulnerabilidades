###############################################################################
#Proyecto          => SAFRE ( MEXICO )                                        #
#Owner             => E.F.P.                                                  #
#Programa DISL013P => Reporte de Relacion de Ctas. Proximas a Redimirse Anual #
#Fecha             => 14/03/2011                                              #
#By                => DMR                                                     #
#Actualizacion     =>                                                         #
#By                =>                                                         #
#Sistema           => DIS                                                     #
###############################################################################
DATABASE safre_af

GLOBALS
   DEFINE
      g_reg                RECORD LIKE dis_det_bono.*,
      g_param              RECORD LIKE dis_parametro.*,
      cad_fecha            CHAR(08),
      sw                   SMALLINT,
      opc                  CHAR(01),
      enter                CHAR(01),
      nombre               CHAR(50),
      respuesta            CHAR(01),
      ejecuta              CHAR(100),
      G_LISTA              CHAR(100),
      t_monto_bono         DECIMAL(18,4), 
      t_monto_dbmx         DECIMAL(16,2), 
      num_reg              INTEGER,
      usuario              CHAR(08)

   DEFINE
      reg_nom  RECORD
                  nombre   CHAR(40),
                  paterno  CHAR(40),
                  materno  CHAR(40)
           END RECORD

   DEFINE
      hoy                  DATE,
      hora                 CHAR(08),
      nssa                 CHAR(11),
      anno                 SMALLINT
END GLOBALS


MAIN
   OPTIONS INPUT  WRAP,
           PROMPT LINE LAST ,
           ERROR  LINE LAST -2,
           ACCEPT KEY CONTROL-I,
           FORM   LINE 3         

   CALL init()

   OPEN WINDOW disl013_ven AT 2,2 WITH FORM "DISL013P1" ATTRIBUTE(BORDER)

   DISPLAY " < ESC > Aceptar                                         < Ctrl-C > Cancelar   " AT 1,1 ATTRIBUTE(REVERSE)

   DISPLAY " DISL013P    LISTADO DE CUENTAS PROXIMAS A REDIMIRSE ANUALMENTE                " AT 3,1 ATTRIBUTE(REVERSE)

   DISPLAY hoy  AT 3,67 ATTRIBUTE(REVERSE)

   INPUT anno FROM FORMONLY.anno

      AFTER FIELD anno
         IF anno IS NULL OR anno = " " THEN
            ERROR " El AÑO no puede ser Nulo o Blancos "
            SLEEP 2
            ERROR ""
            NEXT FIELD anno
         ELSE
            IF anno < 1900  OR  anno > 2100 THEN
               ERROR " Dato Incorrecto teclee un AÑO valido !!!"
               SLEEP 2
               ERROR ""
               NEXT FIELD anno
            END IF
         END IF

      ON KEY ( ESC)
         IF anno IS NULL OR anno = " " THEN
            ERROR " El AÑO no puede ser Nulo o Blanco "
            SLEEP 2
            ERROR ""
            NEXT FIELD anno
         ELSE
            IF anno < 1900  OR  anno > 2100 THEN
               ERROR " Dato Incorrecto teclee un AÑO valido !!!"
               SLEEP 2
               ERROR ""
               NEXT FIELD anno
            END IF
         END IF

         LET cad_fecha = anno USING "&&&&","0101"

         SELECT count(*) FROM dis_det_bono a
         WHERE a.fecha_reden = cad_fecha

         IF STATUS = NOTFOUND THEN
            ERROR " NO EXISTE INFORMACION PARA ESTE AÑO "
            SLEEP 2
            ERROR ""
            NEXT FIELD anno
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
      CALL pregu_arch_imp()

      LET hora = TIME

      LET G_LISTA = g_param.ruta_spool CLIPPED,"/","A-REDIMIR-",
                    anno USING "&&&&" CLIPPED,".",hora

      START REPORT listado_ctas TO G_LISTA

      CALL genera()

      LET ejecuta = "chmod 777 ", G_LISTA CLIPPED
      RUN ejecuta

      IF opc MATCHES "[Ii]" THEN
         LET ejecuta = "lp ", G_LISTA CLIPPED
         RUN ejecuta
      END IF

      ERROR ""
      PROMPT " PROCESO FINALIZADO...< ENTER > PARA CONTINUAR..." FOR enter
   END IF

   CLOSE WINDOW disl013_ven
END MAIN


############################################################################
FUNCTION init()
   INITIALIZE g_reg.*, HOY, reg_nom.*, respuesta, nombre, opc TO NULL
   INITIALIZE usuario TO NULL

   LET t_monto_bono = 0
   LET t_monto_dbmx = 0
   LET num_reg      = 0

   LET hoy = TODAY

   SELECT user
   INTO   usuario
   FROM   glo_parametro

   SELECT *
   INTO   g_param.*
   FROM   dis_parametro
END FUNCTION


############################################################################
FUNCTION genera()

   ERROR " GENERANDO INFORMACION... AGUARDE UN MOMENTO..."
         
   DECLARE apt_ctas_1 CURSOR FOR
   SELECT a.*   FROM dis_det_bono a
   WHERE  a.fecha_reden = cad_fecha
   ORDER BY n_unico

   FOREACH apt_ctas_1 INTO g_reg.*

       SELECT b.nombres, b.paterno, b.materno, n_seguro INTO reg_nom.*, nssa
       FROM   afi_mae_afiliado b
       WHERE  b.n_unico = g_reg.n_unico

       LET g_reg.importe_bono = g_reg.importe_bono / 10000
       LET t_monto_bono = t_monto_bono + g_reg.importe_bono

       LET g_reg.importe_dbmx = g_reg.importe_dbmx / 100
       LET t_monto_dbmx = t_monto_dbmx + g_reg.importe_dbmx

       LET nombre = reg_nom.nombre  CLIPPED," ",
                    reg_nom.materno CLIPPED," ",
                    reg_nom.paterno CLIPPED

       LET num_reg = num_reg + 1

       OUTPUT TO REPORT listado_ctas(g_reg.*, 
                                     nombre, 
                                     num_reg,
                                     t_monto_bono,
                                     t_monto_dbmx,
                                     nssa)

       INITIALIZE nombre, reg_nom.* TO NULL
   END FOREACH
   FINISH REPORT listado_ctas
END FUNCTION


############################################################################
REPORT listado_ctas(g_reg, nombre, num_reg, t_monto_bono, t_monto_dbmx, nssr)
#cl----------------

   DEFINE
      g_reg              RECORD LIKE dis_det_bono.*,
      nombre             CHAR(40),
      num_reg            INTEGER,
      t_monto_bono       DECIMAL(18,4),
      t_monto_dbmx       DECIMAL(16,2),
      nssr               CHAR(11),
      L1                 CHAR(01),
      L2                 CHAR(02),
      L3                 CHAR(03),
      L4                 CHAR(04),
      L5                 CHAR(05),
      L6                 CHAR(06),
      L7                 CHAR(07),
      L8                 CHAR(08),
      L9                 CHAR(09),
      L10                CHAR(10),
      L11                CHAR(11)

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

         PRINT COLUMN 46, " REPORTE DE CUENTAS PROXIMAS A REDIMIR ",
               COLUMN 113,"pagina : ",PAGENO USING "###"

         PRINT
         PRINT COLUMN 02,"FECHA PROCESO : ", hoy USING "DD/MM/YYYY",
               COLUMN 117,"DISL013P"
         PRINT
         PRINT '\033e\033(s218T\033(s17H\033(s7B'
         PRINT
               COLUMN 01,"\332",L10,L1,
                         "\302",L10,L4,L4,
                         "\302",L10,L10,L10,L10,
                         "\302",L10,L4,L1,L1,L1,
                         "\302",L10,L4,L1,L1,L1,
                         "\302",L10,L4,L1,L1,L1,
                         "\277"

         PRINT COLUMN 01, "|",
               COLUMN 02, "   N S S   ",
               COLUMN 13, "|",
               COLUMN 15, "     C U R P      ",
               COLUMN 33, "|",
               COLUMN 35, "N O M B R E",
               COLUMN 75, "|",
               COLUMN 76, "  MONTO DE BONO  ",
               COLUMN 93, "|",
               COLUMN 94, "  MONTO DE DBMX  ",
               COLUMN 111,"|",
               COLUMN 112,"FECHA REDENCION",
               COLUMN 127,"|"

         PRINT
             COLUMN 01,"\300",L10,L1,
                       "\301",L10,L4,L4,
                       "\301",L10,L10,L10,L10,
                       "\301",L10,L4,L1,L1,L1,
                       "\301",L10,L4,L1,L1,L1,
                       "\301",L10,L4,L1,L1,L1,
                       "\331"

    ON EVERY ROW
      PRINT COLUMN 02,nssa,
            COLUMN 15,g_reg.n_unico,
            COLUMN 35,nombre,
            COLUMN 76,g_reg.importe_bono USING "##########&.&&&&",
            COLUMN 94,g_reg.importe_dbmx USING "############&.&&",
            COLUMN 115,g_reg.fecha_reden

    ON LAST ROW
       SKIP 3 LINES
       PRINT
             COLUMN 01,"\332",L10,L1,
                       "\302",L10,L4,L4,
                       "\302",L10,L10,L10,L10,
                       "\302",L10,L4,L1,L1,L1,
                       "\302",L10,L4,L1,L1,L1,
                       "\302",L10,L4,L1,L1,L1,
                       "\277"

      PRINT COLUMN 01,"|",
            COLUMN 02,num_reg  USING "#######&",
            COLUMN 13,"|",
            COLUMN 15,"T O T A L E S : ",
            COLUMN 75,"|",
            COLUMN 76, t_monto_bono        USING "##########&.&&&&",
            COLUMN 93,"|",
            COLUMN 94, t_monto_dbmx        USING "############&.&&",
            COLUMN 111,"|",
            COLUMN 127,"|"

       PRINT
             COLUMN 01,"\300",L10,L1,
                       "\301",L10,L4,L4,
                       "\301",L10,L10,L10,L10,
                       "\301",L10,L4,L1,L1,L1,
                       "\301",L10,L4,L1,L1,L1,
                       "\301",L10,L4,L1,L1,L1,
                       "\331"
END REPORT


############################################################################
FUNCTION pregu_arch_imp()
   WHILE TRUE
      PROMPT "Desea Generar (A)rchivo o (I)mpresion ? " FOR opc
      IF opc MATCHES "[AaIi]" THEN
         EXIT WHILE
      ELSE
         CONTINUE WHILE
      END IF
   END WHILE
END FUNCTION

