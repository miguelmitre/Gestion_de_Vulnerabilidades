################################################################################
#Proyecto            => SISTEMA DE safre_af( MEXICO )
#Por                 => MARCO ANTONIO GONZALEZ ROJAS
#Programa TRAC080    => CARGA ARCH PARA TRASPASAR AL MAESTRO DE TRA-ICE-AFO IMSS
#Fecha creacion      => 24 DE MAYO DEL 2005.
#Sistema             => TRA-ICE-IMSS
#Objetivo            => EL SIGUIENTE PROGRAMA TIENE POR OBJETIVO CARGAR INFOR-
#                       MACION A LA TABLA DE MAESTRO DE ICEFAS ES DECIR ALA
#                       TABLA safre_af:tra_mae_icefa. PARA EL MODULO DE TRA-MAE
#                       ICEFA IMSS.
###############################################################################
DATABASE safre_af
GLOBALS
DEFINE   HOY             DATE 
DEFINE   g_usuario       CHAR(10)
DEFINE   genter          CHAR(01)
DEFINE   gnom_arch       CHAR(15)
DEFINE   gruta           CHAR(22)
DEFINE   gruta1          CHAR(35) #DARLO DE ALTA CUANDO LO PASE A LA AFORE ACT
DEFINE   gbandera    ,         
         gnum_d_reg  ,
         gctos       ,
         gctos1      ,
         gctos2      ,
         gctosuser       SMALLINT
DEFINE   gruta_y_arch    CHAR(100)
DEFINE   gdd          ,
         gmm          ,
         gyy             CHAR(02)
DEFINE   gaa             CHAR(04)
DEFINE   garchsali92     CHAR(12)
DEFINE   gtxt            CHAR(200)
DEFINE   g_c_ice         CHAR(3)
DEFINE   g_c_nss         CHAR(11)
DEFINE   g_rfc_trab      CHAR(13)
DEFINE   g_c_num_ctrl    CHAR(30)
DEFINE   g_c_nombre_tra  CHAR(120)
DEFINE   g_fec_nac       CHAR(10)
DEFINE   g_retorno_carr  CHAR(01)

DEFINE   gr_carga  RECORD
                   plano CHAR(200)
                   END RECORD
DEFINE   g_car_nss RECORD LIKE  tra_mae_icefa.*

END GLOBALS
MAIN
  
   OPTIONS INPUT WRAP,
      PROMPT LINE LAST,
      ERROR LINE LAST,
      MESSAGE LINE  LAST,
      ACCEPT KEY CONTROL-I
      DEFER INTERRUPT

   CALL STARTLOG ("TRAC080.log")

      CALL init()
      CALL proceso_principal()

      PROMPT "PROCESO TERMINADO TECLEE < ENTER > PARA SALIR..." FOR genter 
      CLOSE WINDOW w_popup
      EXIT PROGRAM

END MAIN
#i---------
FUNCTION init()

   LET 	 HOY                        =        TODAY
   LET   gruta                      =        "/safre_prc/tra/rescate"
   #====FORMATEA NOMBRE DEL ARCHIVO DE SALIDA DE LOS REGS ENCONTRADOS EN LA
   #====B.D ES DECIR EN LOS ARCHIVOS DONDE SE LES HACE EL grep 
   LET 	 gdd                        =        DAY(HOY)   USING "&&"
   LET 	 gmm                        =        MONTH(HOY) USING "&&"
   LET 	 gaa                        =        YEAR(HOY)  USING "&&&&"
   LET   garchsali92                =        "ice92-" CLIPPED,gdd CLIPPED,                                                    gmm CLIPPED,gaa[3,4] CLIPPED
   #=======================                  =============================

   LET   gruta1                     =        "/safre_prc/tra/rescate/" CLIPPED,garchsali92 CLIPPED
 
   SELECT USER
   INTO g_usuario
   FROM safre_af:tab_afore_local 

   #=======TABLA TEMPORAL PARA CARGA DE ARCHIVO DE USUARIO ( "NSS" )
   CREATE TEMP TABLE tmp_ice ( nss CHAR(011) )
   #==============                                      ============

   #=======TABLA TEMPORAL PARA CARGA DE ARCHIVO DE USUARIO ( "NSS" )

   DATABASE safre_tmp
   SELECT "X"
   FROM systables
   WHERE tabname = "tmp_cash"

   IF ( STATUS = NOTFOUND ) THEN
      CREATE TABLE tmp_cash
      (plano CHAR(200));
   ELSE
      DROP TABLE tmp_cash
      CREATE TABLE tmp_cash
      (plano CHAR(200))
   END IF
   
   DATABASE safre_af

   #=======
END FUNCTION

#pp---------
FUNCTION proceso_principal()

   OPEN WINDOW w_popup AT 2,2  
   WITH 19 ROWS, 75 COLUMNS
   ATTRIBUTE (BORDER, FORM LINE 4)
   OPEN FORM f_popup from "TRAC0801" 
   DISPLAY FORM f_popup

   DISPLAY "TRAC080 CARGA ARCH PARA TRASPASAR AL MAESTRO DE TRA-ICE-AFO IMSS            " AT 3,1 ATTRIBUTE (REVERSE)
   DISPLAY "                         < CTRL-C > Salir                                      " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY  HOY USING "DD-MM-YYYY"  AT 3,66 ATTRIBUTE (REVERSE)

      LET           gnom_arch             =      "carnssice",gdd CLIPPED,
                                                             gmm CLIPPED,
                                                             gaa[3,4]

      INPUT BY NAME gnom_arch WITHOUT DEFAULTS

         BEFORE FIELD gnom_arch

            DISPLAY BY NAME gnom_arch

         AFTER FIELD gnom_arch
         IF ( gnom_arch IS NULL OR gnom_arch  = " ") THEN
            ERROR "ARCHIVO NO VALIDO...TECLEAR NOMBRE DE ARCHIVO VALIDO..."
            SLEEP 3
            ERROR ""
            NEXT FIELD gnom_arch
         END IF

         ON KEY (INTERRUPT)
            ERROR "PROGRAMA CANCELADO..."
            SLEEP 3
            ERROR ""
            EXIT PROGRAM

         ON KEY (ESC)
            IF ( gnom_arch IS NULL OR gnom_arch  = " ") THEN
               ERROR "ARCHIVO NO VALIDO...TECLEAR NOMBRE DE ARCHIVO VALIDO..."
               SLEEP 3
               ERROR ""
                NEXT FIELD gnom_arch
            ELSE
               EXIT INPUT
            END IF

      END INPUT

         DISPLAY "PROCESANDO INFORMACION" AT 19,1 ATTRIBUTE(REVERSE)
         LET gnom_arch                =          gnom_arch CLIPPED
         
         CALL carga_arch_de_user()

         IF ( gbandera = 0 ) THEN # NO HAY ERROR EN EL ARCHIVO

            ERROR "Archivo Plano Correcto..."
            SLEEP 2
            ERROR " "

            CALL checa_nss_maeafili()

            ERROR "Buscando NSS en la B.D. SAR92 << Pantalla Principal >> Saldra por unos momentos"
            SLEEP 5
            ERROR " "

            CALL barrido_arch92()
            CALL carplatabl() # CARGARA LOS REGS EN LA TABLA TEMPORAL 
                              # safre_tmp:tra_inf_ice92 QUE CONTENDRA
                              # LOS DATOS DE TRA-ICE-IMSS DE LOS ARCHIVOS
                              # BUSCADOS EN "/safre_prc/procesar"
            CALL carmaeice()      # FUNCION QUE CARGARA ALA TABLA DE
                                  # safre_af:tra_mae_icefa
         END IF 
         
         CALL borra_archgen()
         
         DISPLAY "                      " AT 19,1 ATTRIBUTE(REVERSE)
        
END FUNCTION

#pp---------
FUNCTION  carga_arch_de_user()
DEFINE l_reg                 RECORD
       nss         CHAR(011)
                             END RECORD
DEFINE l_long      INTEGER


   WHENEVER ERROR CONTINUE

      LET gruta_y_arch = gruta CLIPPED,"/",gnom_arch CLIPPED
      LOAD FROM gruta_y_arch  #DELIMITER ","
      INSERT INTO tmp_ice
   
   WHENEVER ERROR STOP 

   LET    gctosuser       =            0
   LET    gnum_d_reg      =            0
   LET    gbandera        =            0

   SELECT COUNT(*)
      INTO gctosuser
      FROM tmp_ice
   IF ( gctosuser   = 0  OR gctosuser  IS NULL ) THEN
      ERROR "NO EXISTE ARCHIVO DE CARGA..."
      SLEEP 2
      ERROR " "
      EXIT PROGRAM
   ELSE
      DISPLAY "NUMERO DE REGS EN EL ARCHIVO PARA BUSCAR EN LA B.D SAR92 : ",gctosuser AT 09,02
      ERROR "Verificando Archivo Plano..."
      SLEEP 2
      ERROR " "
   
      DECLARE ice CURSOR FOR

         SELECT *
            FROM tmp_ice

      FOREACH ice INTO l_reg.*

         LET gnum_d_reg      =             gnum_d_reg  +  1
         LET l_long          =             11
         IF ( valida_reg(11,l_reg.nss) ) THEN # SE ENCONTRO ERROR EN EL ARCH
            LET gbandera      =             1
            OPEN WINDOW  w_error AT 12,14 WITH 6 ROWS,50 COLUMNS ATTRIBUTE (BORDER)
               DISPLAY "    ARCHIVO DE BUSQUEDA SE ENCONTRO --ERRONEO--    " AT 01,01 ATTRIBUTE (REVERSE)
               DISPLAY " NUM DE LINEA:  ",gnum_d_reg  AT 03,01
               DISPLAY " DESCRIP DE LA LINEA:  ","nss:   ",l_reg.nss CLIPPED AT 04,01
               DISPLAY "__________________________________________________" AT 05,01
               PROMPT "TECLEE ENTER PARA SALIR..." FOR genter
            CLOSE WINDOW w_error
            EXIT PROGRAM
         ELSE  # NO SE ENCONTRO ERROR EN EL ARCH
         END IF

      END FOREACH

      END IF

END FUNCTION
#vr---------
FUNCTION valida_reg(long,cadena)
DEFINE   i       SMALLINT
DEFINE   long    SMALLINT
DEFINE   cadena  CHAR(100)
DEFINE   v_num   CHAR(01)

FOR i  =   1   TO long

   LET v_num  =  cadena[i,i]

   IF ( v_num NOT MATCHES "[0-9]" )  THEN
      RETURN TRUE
   END IF

END FOR

      RETURN FALSE

END FUNCTION

#cnm--------
FUNCTION checa_nss_maeafili()
DEFINE l_r             RECORD
       nss       CHAR(011)
                       END RECORD
DEFINE gnum_d_line     SMALLINT

LET  gnum_d_line          =    0

   DECLARE f  CURSOR FOR

      SELECT E.nss
         FROM tmp_ice E

   FOREACH  f INTO l_r.*

      LET gnum_d_line     =             gnum_d_line +  1

      SELECT "OK"
         FROM safre_af:afi_mae_afiliado
      WHERE n_seguro   =  l_r.nss
      GROUP BY 1
    
      IF ( STATUS = NOTFOUND ) THEN

         OPEN WINDOW w_errmae AT 12,14 WITH 6 ROWS,50 COLUMNS ATTRIBUTE(BORDER)
            DISPLAY  "    ARCHIVO DE BUSQUEDA SE ENCONTRO ---ERRONEO---     " AT 01,01 ATTRIBUTE (REVERSE)
            DISPLAY  "    NSS NO ENCONTRADO EN LA B.D DE AFILIACION         " AT 01,01 ATTRIBUTE (REVERSE)
            DISPLAY  " NUM DE LINE:   ",gnum_d_line,"  ","NSS:    ",l_r.nss CLIPPED AT 04,01
            DISPLAY "__________________________________________________" AT 05,01
            PROMPT "TECLEE ENTER PARA SALIR..." FOR genter
         CLOSE  WINDOW w_errmae
         EXIT PROGRAM
      END IF
     

   END FOREACH


END FUNCTION

#ba92-------
FUNCTION barrido_arch92()
DEFINE l_reg                 RECORD
       nss         CHAR(011)
                             END RECORD
DEFINE lnum_busc             SMALLINT


LET lnum_busc            =               0

DECLARE ice2 CURSOR FOR

   SELECT *
      FROM tmp_ice

FOREACH ice2 INTO l_reg.*
   LET lnum_busc            =   lnum_busc  +  1

   DISPLAY " "
   DISPLAY "    +---------------------------------------------------------------------+"
   DISPLAY "    |                         < CTRL-C > Salir                            |"
   DISPLAY "    |_____________________________________________________________________|"
   DISPLAY "    |                                                                     |"
   DISPLAY "    |   BUSCANDO INFORMACION EN LA B.D. SAR92                             |"
   DISPLAY "    |   DEL NSS: ",l_reg.nss,"                                              |"
   DISPLAY "    |   NUMERO DE REGS BUSCADOS: ",lnum_busc USING "###","                                      |"
   DISPLAY "    |_____________________________________________________________________|"
   DISPLAY "    |PROCESANDO INFORMACION POR FAVOR ESPERE.......                       |"
   DISPLAY "    +---------------------------------------------------------------------+"
   DISPLAY " "
   DISPLAY " "
   DISPLAY " "
   DISPLAY " "
   DISPLAY " "
   DISPLAY " "

   LET   gtxt  = "cd ",gruta CLIPPED,";","grep ",l_reg.nss CLIPPED," ",
                 "carimsscop.txt  >> " CLIPPED," ",garchsali92
   RUN   gtxt

   LET   gtxt  = "cd ",gruta CLIPPED,";","chmod 777 ",garchsali92
   RUN   gtxt

      
END FOREACH 


   
END FUNCTION

#cpt -------
FUNCTION carplatabl()

   ERROR "CARGANDO ARCHIVO PLANO DE --ICEFAS--..."
   SLEEP 2
   ERROR " "
   WHENEVER ERROR STOP

   LOAD FROM gruta1
   INSERT INTO safre_tmp:tmp_cash
   WHENEVER ERROR STOP

   LET gctos  = 0

   SELECT COUNT(*)
     INTO gctos
     FROM safre_tmp:tmp_cash
   IF ( gctos = 0 OR gctos IS NULL ) THEN
      PROMPT "NO SE ENCONTRARON LOS NSS EN LA B.D SAR92,TECLEE <ENTER> PARA SALIR..." FOR genter 
      CALL borra_archgen()
      EXIT PROGRAM
   ELSE
      DELETE FROM safre_tmp:tra_inf_ice92 
      
      LET gctos1 = 0
     
      DECLARE tra92 CURSOR FOR

      SELECT *
         FROM safre_tmp:tmp_cash
      FOREACH tra92 INTO gr_carga.*

         LET gctos1 = gctos1 + 1
         DISPLAY "NUMERO DE REGS ENCONTRADOS EN LA B.D SAR92   : ", gctos1 AT 11,14
         LET g_c_ice                    =      gr_carga.plano[1,3]
         LET g_c_nss                    =      gr_carga.plano[4,14]
         LET g_rfc_trab                 =      gr_carga.plano[15,27]
         LET g_c_num_ctrl               =      gr_carga.plano[28,57]
         LET g_c_nombre_tra             =      gr_carga.plano[58,177]
         LET g_fec_nac                  =      gr_carga.plano[178,187]
         LET g_retorno_carr             =      gr_carga.plano[188,188]

         INSERT INTO safre_tmp:tra_inf_ice92  VALUES ( g_c_ice,
                                                       g_c_nss,
                                                       g_rfc_trab,
                                                       g_c_num_ctrl,
                                                       g_c_nombre_tra,
                                                       g_fec_nac,
                                                       g_retorno_carr )


   END FOREACH

   END IF

END FUNCTION
#cmi -------
FUNCTION carmaeice()

   LET  gctos2 =  0

   SELECT COUNT(*)
   INTO gctos2 
   FROM safre_tmp:tra_inf_ice92

   IF ( gctos2 <> 0 OR gctos2 IS NOT NULL ) THEN
      DISPLAY "NUMERO DE REGS A CARGAR A LA B.D             : ", gctos2 AT 13,14
   ELSE
      ERROR "NO EXISTE INFORMACION A CARGAR ALA B.D..."
      SLEEP 3
      CALL borra_archgen()
      EXIT PROGRAM
   END IF

   PROMPT "ESTA SEGURO DE CARGAR LA INFORMACION AL MAESTRO S/N ?   " FOR genter   

      IF ( genter  = "S" OR genter  = "s" )                THEN
         CALL carga_inf()
      ELSE
         CALL borra_archgen()
         
         ERROR "PROCESO CANCELADO ..."
         SLEEP 3
         EXIT PROGRAM
      END IF

END FUNCTION

#ci---------
FUNCTION carga_inf()
DEFINE l_maxnfol          CHAR(008)
DEFINE l_numreg           INTEGER
DEFINE l_numcarmae        INTEGER
DEFINE l_numNOcarmae      INTEGER

   
   LET l_numreg       =   0
   LET l_maxnfol      =   0
   LET l_numcarmae    =   0
   LET l_numNOcarmae  =   0

   DECLARE e  CURSOR FOR
     
      SELECT A.c_nss,A.c_rfc_trabajador,A.c_nombre_trab[1,40],
             A.c_nombre_trab[41,80],A.c_nombre_trab[81,120],
             A.c_icefa,A.c_num_ctrl_int,B.n_folio,B.tipo_solicitud,
             B.n_seguro
      FROM   safre_tmp:tra_inf_ice92 A,
             safre_af:afi_mae_afiliado B
      WHERE  A.c_nss      =   B.n_seguro
      FOREACH e INTO g_car_nss.nss,g_car_nss.rfc,g_car_nss.paterno,
             g_car_nss.materno,g_car_nss.nombres,g_car_nss.icefa_cod,
             g_car_nss.nro_int_cta,g_car_nss.n_folio,
             g_car_nss.tipo_solicitud,g_car_nss.n_seguro 
       
      SELECT MAX(a.correlativo) 
      INTO l_numreg
      FROM safre_af:tra_mae_icefa a

      IF ( l_numreg IS NULL OR l_numreg = " " ) THEN
         LET l_numreg  = 0
      END IF

      LET  l_maxnfol                          =                 l_numreg 
      LET  l_maxnfol                          =                 l_maxnfol + 1

      LET  g_car_nss.n_folio_tra              =                 l_maxnfol
      LET  g_car_nss.fecha_solic_tra          =                 TODAY
      LET  g_car_nss.tipo_comp_icefa          =                 " "
      LET  g_car_nss.fecha_comp_icefa         =                 " "
      LET  g_car_nss.saldo_sar_92             =                 0
      LET  g_car_nss.saldo_viv_92             =                 0
      LET  g_car_nss.fecha_captura            =                 TODAY
      LET  g_car_nss.fecha_proceso            =                 TODAY
      LET  g_car_nss.lote_genera              =                 NULL
      LET  g_car_nss.fecha_genera             =                 NULL
      LET  g_car_nss.status                   =                 20
      LET  g_car_nss.fuente                   =                 "1"
      LET  g_car_nss.correlativo              =                 0 # Campo Serial
      LET  g_car_nss.usuario                  =                 g_usuario
      LET  g_car_nss.n_envios                 =                 "0"
      LET  g_car_nss.diagnostico              =                 NULL

         #=====CHECA SI EL REG. A INSERTAR YA EXISTE EN LA TBLA tra_mae_icefa

         SELECT "OK" 
            FROM safre_af:tra_mae_icefa
         WHERE  nss            =  g_car_nss.nss         #ICE
           AND  nro_int_cta    =  g_car_nss.nro_int_cta #ICE
           AND  rfc            =  g_car_nss.rfc         #ICE
           AND  icefa_cod      =  g_car_nss.icefa_cod   #ICE
           AND  n_seguro       =  g_car_nss.n_seguro    #AFIL
         IF ( STATUS = NOTFOUND ) THEN          
            LET l_numcarmae                   =                l_numcarmae + 1
 

            DISPLAY "TOTAL DE REGISTROS CARGADOS A LA B.D         :" AT 15,14 ATTRIBUTE (REVERSE)
            DISPLAY  l_numcarmae AT 15,61 ATTRIBUTE (REVERSE)


            INSERT INTO safre_af:tra_mae_icefa VALUES ( g_car_nss.n_folio,
                                                        g_car_nss.tipo_solicitud,
                                                        g_car_nss.n_seguro,
                                                        g_car_nss.nss,
                                                        g_car_nss.rfc,
                                                        g_car_nss.paterno,
                                                        g_car_nss.materno,
                                                        g_car_nss.nombres,
                                                        g_car_nss.n_folio_tra,
                                                        g_car_nss.icefa_cod,
                                                        g_car_nss.nro_int_cta,
                                                        g_car_nss.fecha_solic_tra,
                                                        g_car_nss.tipo_comp_icefa,
                                                        g_car_nss.fecha_comp_icefa,
                                                        g_car_nss.saldo_sar_92,
                                                        g_car_nss.saldo_viv_92,
                                                        g_car_nss.fecha_captura,
                                                        g_car_nss.fecha_proceso,
                                                        g_car_nss.lote_genera,
                                                        g_car_nss.fecha_genera,
                                                        g_car_nss.status,
                                                        g_car_nss.fuente,
                                                        g_car_nss.correlativo,
                                                        g_car_nss.usuario,
                                                        g_car_nss.n_envios,
                                                        g_car_nss.diagnostico )
         ELSE
            LET l_numNOcarmae          =              l_numNOcarmae + 1  
            DISPLAY "TOTAL DE REGISTROS <<NO>> CARGADOS A LA B.D (YA EXISTEN) : ",l_numNOcarmae  AT 17,02 ATTRIBUTE (REVERSE)
         END IF
 
   END FOREACH

END FUNCTION

#pp---------
FUNCTION borra_archgen()

    LET   gtxt  = "cd ",gruta CLIPPED,";","rm ",garchsali92
    RUN   gtxt

END FUNCTION
