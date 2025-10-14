#########################################################################
#MODULO      : SEP                              
#PROGRAMA    : SEPM020      
#DESCRIPCION : Muestra los registros patronales que pertenecen al 
#              invadido y habilita al usuario para elegir los registros
#              patronales a ser considerados en la separacion de cuentas
#AUTOR       : JESUS YAÑEZ MORENO
#FECHA       : 3 abr 2010
#########################################################################

DATABASE  safre_af
GLOBALS
   DEFINE  esc_int                  SMALLINT
   DEFINE  g_idSolicitudSeparacion  INTEGER 
   DEFINE  g_invadido      LIKE  safre_af:dis_det_aporte.n_seguro
   DEFINE  g_asociado     LIKE  safre_af:dis_det_aporte.n_seguro
   DEFINE  g_today             DATE

   DEFINE  i                   ,
           i_arr               ,
           i_scr               SMALLINT 
   DEFINE  g_enter             CHAR(01) 

   DEFINE  reg_scr_reg_patro         ARRAY[20]   OF   RECORD
           reg_patro_separado  LIKE  safre_af:dis_det_aporte.reg_patronal_imss,
           reg_patro_separador LIKE  safre_af:dis_det_aporte.reg_patronal_imss
                                END   RECORD
END GLOBALS
MAIN
   
   OPTIONS
        PROMPT  LINE LAST,
        INPUT   WRAP,
        ACCEPT  KEY CONTROL-I
        DEFER   INTERRUPT

    call STARTLOG("SEPM020.log")

    LET g_idSolicitudSeparacion = ARG_VAL(1)
    LET g_invadido              = ARG_VAL(2)
    LET g_asociado              = ARG_VAL(3)

    LET     esc_int     = 0       

    CALL    trata_reg_patronales()

END MAIN

FUNCTION  trata_reg_patronales()
    LET    g_today             =  TODAY
    OPEN   WINDOW  SEPM020  AT  2,2  WITH FORM "SEPM020" ATTRIBUTE(BORDER)
    DISPLAY  "<Enter> Asigna nss Separado a Separador         <Ctrl-B> Elimina nss Separador    "   AT  1,1  ATTRIBUTE  (REVERSE)
    DISPLAY  "SEPM020              SEPARACION DE REGISTROS PATRONALES                           "    AT  3,1 ATTRIBUTE  (REVERSE)

    DISPLAY  "Fecha:",g_today USING "DD-MM-YYYY" AT 3,60 ATTRIBUTE(REVERSE)
    DISPLAY  "ID SOLICITUD  :",g_idSolicitudSeparacion AT  4,1
    DISPLAY  "NSS SEPARADO  :",g_invadido          AT  5,1
    DISPLAY  "NSS SEPARADOR :",g_asociado         AT  6,1

    DISPLAY  "                           REGISTROS  PATRONALES                                   "   AT  7,1  ATTRIBUTE  (REVERSE)
    DISPLAY  "            NSS SEPARADO                               NSS SEPARADOR               "   AT  9,1   ATTRIBUTE  (REVERSE)
    FOR    i               =  1      TO   10
           INITIALIZE   reg_scr_reg_patro[i].*    TO  NULL
    END FOR    
    DECLARE cur_disp_reg_patro    CURSOR  FOR
    SELECT  UNIQUE(reg_patronal_imss)
      FROM  safre_af:dis_det_aporte
     WHERE  n_seguro       =  g_invadido

    LET     i       =  1

    FOREACH  cur_disp_reg_patro     INTO 
             reg_scr_reg_patro[i].reg_patro_separado
       LET   i                  =   i  +  1
    END FOREACH

    CALL SET_COUNT(i-1)

    DISPLAY  ARRAY  reg_scr_reg_patro     TO  scr_reg_patro.*
        ON KEY(ESC)
             LET esc_int = 1
             EXIT   DISPLAY
        ON KEY(RETURN)
             LET    i_arr       =  ARR_CURR()
             LET    i_scr       =  SCR_LINE()
             LET    reg_scr_reg_patro[i_arr].reg_patro_separador      =
                    reg_scr_reg_patro[i_arr].reg_patro_separado 
             DISPLAY  reg_scr_reg_patro[i_arr].reg_patro_separador   TO
                          scr_reg_patro[i_scr].reg_patro_separador
        ON KEY (CONTROL-B)
             LET    i_arr       =  ARR_CURR()
             LET    i_scr       =  SCR_LINE()
             LET    reg_scr_reg_patro[i_arr].reg_patro_separador      =  NULL
             DISPLAY  reg_scr_reg_patro[i_arr].reg_patro_separador   TO
                          scr_reg_patro[i_scr].reg_patro_separador
        ON KEY(INTERRUPT)
             LET esc_int = 0

              SELECT "OK" 
              FROM sep_reg_patro_separador a
              WHERE a.idSolicitudSeparacion = g_idSolicitudSeparacion
              GROUP BY 1 
           
              IF STATUS = NOTFOUND THEN 
           
              INSERT INTO safre_af:sep_reg_patro_separador VALUES(
              g_idSolicitudSeparacion,
              "" , g_invadido,"INT","") 
              END IF

             EXIT PROGRAM
    END DISPLAY

    WHILE  TRUE
           PROMPT "   DESEA REALIZAR LA SEPARACION DE RECURSOS [S/N] ? " FOR g_enter
                IF    g_enter  MATCHES "[sSnN]" THEN
                      IF   g_enter  MATCHES "[sS]" THEN
                           EXIT WHILE
                      ELSE
                         SELECT "OK" 
                         FROM sep_reg_patro_separador a
                         WHERE a.idSolicitudSeparacion = g_idSolicitudSeparacion
                         GROUP BY 1 
                      
                         IF STATUS = NOTFOUND THEN 
                      
                         INSERT INTO safre_af:sep_reg_patro_separador VALUES(
                         g_idSolicitudSeparacion,
                         "", g_invadido,"INT","") 
                         END IF
                           EXIT PROGRAM
                      END IF
                END IF
    END WHILE

    FOR   i         =  1        TO  20
          IF     reg_scr_reg_patro[i].reg_patro_separador  IS  NOT  NULL  THEN
                 INSERT  INTO  safre_af:sep_reg_patro_separador
                         VALUES(
                               g_idSolicitudSeparacion, 
                               "",
                               g_invadido,
                               g_asociado,
                               reg_scr_reg_patro[i].reg_patro_separador
                               )
          END IF
   END FOR

  IF esc_int = 1 THEN

   SELECT "OK" 
   FROM sep_reg_patro_separador a
   WHERE a.idSolicitudSeparacion = g_idSolicitudSeparacion
   GROUP BY 1 

   IF STATUS = NOTFOUND THEN 

     INSERT INTO safre_af:sep_reg_patro_separador VALUES(
     g_idSolicitudSeparacion,
     "",g_invadido,"ESC","") 
   END IF
  END IF
END FUNCTION

