################################################################################
#Proyecto            => SISTEMA DE safre_af( MEXICO )
#Por                 => MARCO ANTONIO GONZALEZ ROJAS
#Programa CARICE     => CARGA ARCH PARA TRASPASAR AL MAESTRO DE TRA-ICE-AFO IMSS
#Fecha creacion      => 24 DE MAYO DEL 2005.
#Sistema             => TRA-ICE-IMSS
# VERSION CUANDO YO ARMO EL ARCHIVO EN /safre_prc/rescate NOMARCH ice92-290705
#                                                                       ddmmyy
#                                                Corresponde ala Fech de carga
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
DEFINE   gnom_arch       CHAR(06)
DEFINE   gruta           CHAR(19)
DEFINE   gruta1          CHAR(33) #DARLO DE ALTA CUANDO LO PASE A LA AFORE ACT
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

   CALL STARTLOG ("CARICE.log")
      ERROR "PROCESANDO INFORMACION..."
      SLEEP 2
      ERROR " "

      CALL init()
      CALL proceso_principal()

      PROMPT "PROCESO TERMINADO TECLEE < ENTER > PARA SALIR..." FOR genter 
      EXIT PROGRAM

END MAIN
#i---------
FUNCTION init()

   LET 	 HOY                        =        TODAY
   LET   gruta                      =        "/safre_prc/procesar"
   #====FORMATEA NOMBRE DEL ARCHIVO DE SALIDA DE LOS REGS ENCONTRADOS EN LA
   #====B.D ES DECIR EN LOS ARCHIVOS DONDE SE LES HACE EL grep 
   LET 	 gdd                        =        DAY(HOY)   USING "&&"
   LET 	 gmm                        =        MONTH(HOY) USING "&&"
   LET 	 gaa                        =        YEAR(HOY)  USING "&&&&"
   LET   garchsali92                =        "ice92-" CLIPPED,gdd CLIPPED,                                                    gmm CLIPPED,gaa[3,4] CLIPPED
   #=======================                  =============================

   LET   gruta1                     =        "/safre_prc/procesar/" CLIPPED,garchsali92 CLIPPED
 
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


		 DISPLAY "PROCESANDO INFORMACION" AT 19,1 ATTRIBUTE(REVERSE)

		    CALL carplatabl() # CARGARA LOS REGS EN LA TABLA TEMPORAL 
				      # safre_tmp:tra_inf_ice92 QUE CONTENDRA
				      # LOS DATOS DE TRA-ICE-IMSS DE LOS ARCHIVOS
				      # BUSCADOS EN "/safre_prc/procesar"
		    CALL carmaeice()      # FUNCION QUE CARGARA ALA TABLA DE
                                  # safre_af:tra_mae_icefa
         
            CALL borra_archgen()
         
         DISPLAY "                      " AT 19,1 ATTRIBUTE(REVERSE)
        
END FUNCTION

#cpt -------
FUNCTION carplatabl()

   ERROR "CARGANDO ARCHIVO PLANO DE --ICEFAS--..."
   SLEEP 2
   ERROR " "

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
DEFINE l_cont             INTEGER
DEFINE l_maxnfol          CHAR(008)
DEFINE l_numreg           INTEGER
DEFINE l_numcarmae        INTEGER
DEFINE l_numNOcarmae      INTEGER

   
   LET l_numreg       =   0
   LET l_maxnfol      =   0
   LET l_numcarmae    =   0
   LET l_numNOcarmae  =   0
   LET l_cont         =   0
display "antes de cargar al MAESTRO"
sleep 2

   DECLARE e  CURSOR FOR
     
      SELECT A.c_nss,A.c_rfc_trabajador,A.c_nombre_trab[1,40],
             A.c_nombre_trab[41,80],A.c_nombre_trab[81,120],
             A.c_icefa,A.c_num_ctrl_int,"12480","1","01624255590"
      FROM   safre_tmp:tra_inf_ice92 A
             

      FOREACH e INTO g_car_nss.nss,g_car_nss.rfc,g_car_nss.paterno,
             g_car_nss.materno,g_car_nss.nombres,g_car_nss.icefa_cod,
             g_car_nss.nro_int_cta,g_car_nss.n_folio,
             g_car_nss.tipo_solicitud,g_car_nss.n_seguro 

      LET   l_cont                = l_cont + 1
      display "l_cont:      ",l_cont
      sleep 1
       
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

display "g_car_nss.*:   ",g_car_nss.*
sleep 2

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
