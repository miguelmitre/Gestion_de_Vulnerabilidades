###############################################################################
#Proyecto          => SAFRE ( MEXICO )                                        #
#Owner             => E.F.P.                                                  #
#Programa DISL013  => Reporte de Relacion de Ctas. Marcadas                   # 
#Fecha             => 12/10/2001                                              #
#By                => Laura Eugenia Cortes Guzman                             #
#Actualizacion     =>                                                         #
#By                =>                                                         #
#Sistema           => DIS                                                     #
###############################################################################
DATABASE safre_af
GLOBALS

    DEFINE g_reg                RECORD LIKE cta_rehabilitada.*,
           paramgrales          RECORD LIKE int_parametro.*,
           g_param              RECORD LIKE dis_parametro.*,
           ho_y                 DATE,
           fecha                DATE,
           sw                   SMALLINT,
           opc                  CHAR(01),
           enter                CHAR(01),
           nombre               CHAR(50),
           respuesta            CHAR(01),
           ejecuta              CHAR(100),
           G_LISTA              CHAR(100),
           t_monto_retiro       DECIMAL(11,2), 
           t_monto_cesantia     DECIMAL(11,2), 
           t_monto_voluntaria   DECIMAL(11,2),
           t_monto_vivienda97   DECIMAL(11,2),
           t_monto_cuota_soc    DECIMAL(11,2),
           t_monto_sar          DECIMAL(11,2),
           t_monto_vivienda     DECIMAL(11,2),
           num_reg              INTEGER,

           usuario              CHAR(08),

           reg_nom      RECORD
                nombre          CHAR(40),
                paterno         CHAR(40),
                materno         CHAR(40)
           END RECORD

   DEFINE hoy  date,
          hora CHAR(08)
END GLOBALS

MAIN
    OPTIONS INPUT  WRAP,
            PROMPT LINE LAST ,
            ERROR  LINE LAST -4,
            ACCEPT KEY CONTROL-I,
            FORM   LINE 3         

    CALL init()

    OPEN WINDOW disl013_ven AT 2,2 WITH FORM "DISL0131" ATTRIBUTE(BORDER)

    DISPLAY " < ESC > Aceptar                                       < Ctrl-C > Cancelar    " AT 1,1 ATTRIBUTE(REVERSE)

    DISPLAY " DISL013     LISTADO DE CUENTAS MARCADAS COMO INHABILITADAS                    " AT 3,1 ATTRIBUTE(REVERSE)

    DISPLAY ho_y  AT 3,65 ATTRIBUTE(REVERSE)

    INPUT fecha FROM FORMONLY.fecha
        AFTER FIELD fecha
            IF fecha IS NULL OR fecha = " " THEN
                ERROR "No puede ser nulo o blancos la fecha"
                NEXT FIELD fecha
            END IF

         ON KEY ( ESC)
            IF fecha IS NULL OR fecha = " " THEN
                ERROR "No puede ser nulo o blancos la fecha"
                NEXT FIELD fecha
            END IF
            LET fecha = fecha USING "mm/dd/yyyy"

            SELECT UNIQUE "a.X"  FROM cta_rehabilitada a
                   WHERE a.fecha_actualiza = fecha
            IF STATUS = NOTFOUND THEN
               ERROR " NO EXISTE INFORMACION PARA ESTE FOLIO "
               NEXT FIELD fecha
            END IF
            LET sw = 0
            EXIT INPUT

         ON KEY ( CONTROL-C )
            LET sw = 1
            EXIT INPUT
    END INPUT

    IF sw = 1 THEN
       ERROR ""
       PROMPT " PROCESO CANCELADO...< ENTER > PARA CONTINUAR..."
              FOR enter
    ELSE
             LET fecha = fecha USING "MM/DD/YYYY"
             CALL pregu_arch_imp()


       LET hora = TIME

       LET G_LISTA = g_param.ruta_spool CLIPPED,"/",usuario CLIPPED,".DISL013.",
                     hoy USING "DD-MM-YY",".",hora

             START REPORT listado_ctas TO G_LISTA

             CALL genera()

             LET ejecuta = "chmod 777 ", G_LISTA CLIPPED
             RUN ejecuta

             IF opc MATCHES "[Ii]" THEN
                LET ejecuta = "lp ", G_LISTA CLIPPED
                RUN ejecuta
             END IF

             ERROR ""
             PROMPT " PROCESO FINALIZADO...< ENTER > PARA CONTINUAR..."
                    FOR enter
             
    END IF

    CLOSE WINDOW disl013_ven
END MAIN


############################################################################
FUNCTION init()

    INITIALIZE g_reg.*, HOY, ho_y, reg_nom.*, respuesta, nombre, opc TO NULL
    INITIALIZE usuario TO NULL
    LET t_monto_retiro       = 0
    LET t_monto_cesantia     = 0
    LET t_monto_voluntaria   = 0
    LET t_monto_vivienda97   = 0
    LET t_monto_cuota_soc    = 0
    LET t_monto_sar          = 0
    LET t_monto_vivienda     = 0
    LET num_reg              = 0

    LET ho_y = TODAY USING "dd-mm-yyyy"

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

   ERROR "GENERANDO INFORMACION... AGUARDE UN MOMENTO..."
         
   DECLARE apt_ctas_1 CURSOR FOR
       SELECT a.*   FROM cta_rehabilitada a
              WHERE a.fecha_actualiza  = fecha
   FOREACH apt_ctas_1 INTO g_reg.*

       SELECT b.nombres, b.paterno, b.materno INTO reg_nom.*
       FROM   afi_mae_afiliado b
       WHERE  b.n_seguro = g_reg.nss

       LET t_monto_retiro     = t_monto_retiro     + 
                                g_reg.monto_retiro

       LET t_monto_cesantia   = t_monto_cesantia   + 
                                g_reg.monto_cesantia

       LET t_monto_voluntaria = t_monto_voluntaria + 
                                g_reg.monto_voluntaria

       LET t_monto_vivienda97 = t_monto_vivienda97 + 
                                g_reg.monto_vivienda97

       LET t_monto_cuota_soc  = t_monto_cuota_soc  + 
                                g_reg.monto_cuota_soc

       LET t_monto_sar        = t_monto_sar        + 
                                g_reg.monto_sar

       LET t_monto_vivienda   = t_monto_vivienda   + 
                                g_reg.monto_vivienda92

       LET nombre = reg_nom.nombre  CLIPPED," ",
                    reg_nom.materno CLIPPED," ",
                    reg_nom.paterno CLIPPED

       LET num_reg = num_reg + 1

       OUTPUT TO REPORT listado_ctas(g_reg.*, 
                                     nombre, 
                                     num_reg,
                                     t_monto_retiro,
                                     t_monto_cesantia,
                                     t_monto_voluntaria,
                                     t_monto_vivienda97,
                                     t_monto_cuota_soc,
                                     t_monto_sar,
                                     t_monto_vivienda)

       INITIALIZE nombre, reg_nom.* TO NULL
   END FOREACH
   FINISH REPORT listado_ctas
END FUNCTION

############################################################################
REPORT listado_ctas(g_reg, nombre, num_reg, t_monto_retiro,
                    t_monto_cesantia,   t_monto_voluntaria,
                    t_monto_vivienda97, t_monto_cuota_soc, 
                    t_monto_sar,        t_monto_vivienda)
#cl------------------------

    DEFINE g_reg           RECORD LIKE cta_rehabilitada.*,
           nombre                 CHAR(40),  
           num_reg                INTEGER,
           t_monto_retiro         DECIMAL(11,2),
           t_monto_cesantia       DECIMAL(11,2),
           t_monto_voluntaria     DECIMAL(11,2),
           t_monto_vivienda97     DECIMAL(11,2),
           t_monto_cuota_soc      DECIMAL(11,2),
           t_monto_sar            DECIMAL(11,2),
           t_monto_vivienda       DECIMAL(11,2),
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
       LEFT MARGIN 0
       RIGHT MARGIN 0
       TOP MARGIN 0
       BOTTOM MARGIN 0
       PAGE LENGTH 60

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

         PRINT COLUMN 23, "REPORTE DE CUENTAS MARCADAS COMO ",
                          "INHABILITADAS Y QUE RECIBIERON RECURSOS ",
               COLUMN 111,"p\240gina : ",PAGENO USING "###"

         PRINT
         PRINT COLUMN 02,"FECHA PROCESO : ", fecha USING "DD/MM/YYYY",
               COLUMN 38,"CON POSTERIORIDAD AFORE BITAL, S.A. DE C.V.",
               COLUMN 116,"DISL013"
         PRINT 
         PRINT '\033e\033(s218T\033(s17H\033(s7B'
         PRINT 
             COLUMN 01,"\332",L10,L1,
                       "\302",L10,L10,L10,L10,L1,
                       "\302",L10,L1,
                       "\302",L10,L1,
                       "\302",L10,L1,
                       "\302",L10,L1,
                       "\302",L10,L2,
                       "\302",L10,L1,
                       "\302",L10,L1,
                       "\302",L10,L4,
                       "\302",L10,L4,
                       "\277" 

         PRINT COLUMN 01, "|",
               COLUMN 02, "   N S S   ",
               COLUMN 13, "|",
               COLUMN 15, "N O M B R E",
               COLUMN 55, "|",
               COLUMN 56, "R E T I R O",
               COLUMN 67, "|",
               COLUMN 68, "  C . V .  ",
               COLUMN 79, "|",
               COLUMN 80, "VOLUNTARIAS",
               COLUMN 91, "|",
               COLUMN 92, "V I V . '97",
               COLUMN 103, "|",
               COLUMN 104, "CUOTA SOCIAL",
               COLUMN 115, "|",
               COLUMN 116, "S A R . '92",
               COLUMN 127, "|",
               COLUMN 128, "V I V . '92",
               COLUMN 139, "|",
               COLUMN 140, "   FECHA DE   ",
               COLUMN 154, "|",
               COLUMN 155, "   FECHA DE   ",
               COLUMN 169, "|"

         PRINT COLUMN 01, "|",
               COLUMN 02, "           ",
               COLUMN 13, "|",
               COLUMN 15, "           ",
               COLUMN 55, "|",
               COLUMN 56, "           ",
               COLUMN 67, "|",
               COLUMN 68, "           ",
               COLUMN 79, "|",
               COLUMN 80, "           ",
               COLUMN 91, "|",
               COLUMN 92, "           ",
               COLUMN 103, "|",
               COLUMN 104, "            ",
               COLUMN 115, "|",
               COLUMN 116, "           ",
               COLUMN 127, "|",
               COLUMN 128, "           ",
               COLUMN 139, "|",
               COLUMN 140, "REHABILITACION",
               COLUMN 154, "|",
               COLUMN 155, "INHABILITACION",
               COLUMN 169, "|"

         PRINT
             COLUMN 01,"\300",L10,L1,
                       "\301",L10,L10,L10,L10,L1,
                       "\301",L10,L1,
                       "\301",L10,L1,
                       "\301",L10,L1,
                       "\301",L10,L1,
                       "\301",L10,L2,
                       "\301",L10,L1,
                       "\301",L10,L1,
                       "\301",L10,L4,
                       "\301",L10,L4,
                       "\331" 

    ON EVERY ROW
      PRINT COLUMN 02,g_reg.nss,
            COLUMN 15,nombre,
            COLUMN 56,g_reg.monto_retiro      USING "#######&.&&",
            COLUMN 68,g_reg.monto_cesantia    USING "#######&.&&",
            COLUMN 80,g_reg.monto_voluntaria  USING "#######&.&&",
            COLUMN 92,g_reg.monto_vivienda97  USING "#######&.&&",
            COLUMN 105,g_reg.monto_cuota_soc   USING "#######&.&&",
            COLUMN 117,g_reg.monto_sar        USING "#######&.&&",
            COLUMN 129,g_reg.monto_vivienda92 USING "#######&.&&",
            COLUMN 143,g_reg.fecha_rehabilita USING "dd/mm/yyyy",
            COLUMN 158,g_reg.fecha_inhabilita USING "dd/mm/yyyy"

    ON LAST ROW
       SKIP 3 LINES
       PRINT
             COLUMN 01,"\332",L10,L1,
                       "\302",L10,L10,L10,L10,L1,
                       "\302",L10,L1,
                       "\302",L10,L1,
                       "\302",L10,L1,
                       "\302",L10,L1,
                       "\302",L10,L2,
                       "\302",L10,L1,
                       "\302",L10,L10,L10,L10,
                       "\277" 

      PRINT COLUMN 01,"|",
            COLUMN 02,num_reg  USING "#######&",
            COLUMN 13,"|",
            COLUMN 15,"T O T A L E S : ",
            COLUMN 55,"|",
            COLUMN 56, t_monto_retiro      USING "#######&.&&",
            COLUMN 67,"|",
            COLUMN 68, t_monto_cesantia    USING "#######&.&&",
            COLUMN 79,"|",
            COLUMN 80, t_monto_voluntaria  USING "#######&.&&",
            COLUMN 91,"|",
            COLUMN 92, t_monto_vivienda97  USING "#######&.&&",
            COLUMN 103,"|",
            COLUMN 105,t_monto_cuota_soc   USING "#######&.&&",
            COLUMN 115,"|",
            COLUMN 117,t_monto_sar         USING "#######&.&&",
            COLUMN 128,"|",
            COLUMN 129,t_monto_vivienda    USING "#######&.&&",
            COLUMN 169,"|"

       PRINT
             COLUMN 01,"\300",L10,L1,
                       "\301",L10,L10,L10,L10,L1,
                       "\301",L10,L1,
                       "\301",L10,L1,
                       "\301",L10,L1,
                       "\301",L10,L1,
                       "\301",L10,L2,
                       "\301",L10,L1,
                       "\301",L10,L10,L10,L10,
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
