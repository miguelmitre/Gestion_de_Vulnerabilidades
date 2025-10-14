##########################################################################
# Funcion  SEPF005.4gl Es llamada por el programa SEPL005.4gl            #
#  Para la consulta de cuentas en los diferentes estados de separación   #
#Autor:José Francisco Lugo Cornejo  20/02/2010.                          #
#########################################################################

GLOBALS 

DEFINE ACCION CHAR(001)
DEFINE enter  CHAR(001)
DEFINE hoy    DATE

END GLOBALS 


FUNCTION despliega_nss(l_opc,l_tipo_solicitud)
#dnss------------------------------------------

    DEFINE bnd_mod CHAR(01)
    DEFINE l_tipo_solicitud SMALLINT
    DEFINE x_n_folio        DECIMAL(11,0)
    DEFINE l_opc  CHAR(001)
    DEFINE cont_1 INTEGER

    DEFINE arr_1 ARRAY[5000] OF RECORD
	   arc_1                 smallint, 
	   folio                 INTEGER,
	   diag_confronta        char(02),
	   clasifica_separacion  char(01),
	   clave_admon           CHAR(03),
	   nss                   CHAR(11),
	   nss_asociado          CHAR(11),
	   status_interno        SMALLINT,
	   estado_desc           CHAR(20)
    END RECORD

    DEFINE arr_2 ARRAY[5000] OF RECORD 
	   cve_cedente     CHAR(003)
    END RECORD

    DEFINE cuantos      SMALLINT
    DEFINE txt_nss      CHAR(1500)
    DEFINE l_criterio    CHAR(1500)
    DEFINE cad_construct CHAR(50)
    DEFINE x_nss        CHAR(11)
    DEFINE arc_1        INTEGER
    DEFINE total_pa        INTEGER
    DEFINE band         SMALLINT
    DEFINE band_c       SMALLINT

    OPEN WINDOW ventana_nss AT 2,2 WITH FORM "SEPF005" ATTRIBUTE(BORDER)

    DISPLAY "  [Ctrl-I] Capturar                                     [ Ctrl-C ] Salir       " AT 1,1 ATTRIBUTE(REVERSE)

    DISPLAY " SEPF005     CONSULTA DE CUENTAS EN PROCESO DE SEPARACIÓN                                    " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,68 ATTRIBUTE(REVERSE)


    CONSTRUCT BY NAME  cad_construct  ON   b.folio,b.diag_confronta,
    		       b.clasifica_separacion ,  b.n_seguro,a.estado

     ON KEY(ESC)
          LET int_flag = FALSE
          EXIT CONSTRUCT
    ON KEY(INTERRUPT)
       LET band_c = 0
       EXIT CONSTRUCT
   END CONSTRUCT

   IF NOT int_flag THEN 


CASE l_opc 
WHEN "T"
    LET    l_criterio      =
	" WHERE   ",cad_construct CLIPPED ,
	" AND    a.estado     NOT IN(2,3,4,5,6,8,9,10,21) ",
	" AND    a.estado                 =  e.estado    "
    LET txt_nss =
	" SELECT UNIQUE '',a.folio,'','','',           ",
	" a.n_seguro             , ",
	" a.nss                  , ",
	" a.estado               , ",
	" e.des_estado            ",
	" FROM         sep_det_reg_sol_reclamante a , ",
	" sep_estado_separacion      e   ",
	l_criterio      CLIPPED

WHEN "O"
    LET    l_criterio      =
	" WHERE   ",cad_construct CLIPPED ,
        " AND    b.folio                  =  a.folio ",
        " AND    a.n_seguro               =  b.n_seguro ",
	" AND    a.estado                 =  e.estado    "
    LET txt_nss =
	" SELECT UNIQUE '',b.folio,b.diag_confronta  , ",
	" b.clasifica_separacion , ",
	" b.clave_entidad_admon  , ",
	" b.n_seguro             , ",
	" a.nss                  , ",
	" a.estado               , ",
	" e.des_estado            ",
	" FROM         sep_det_reg_sol_reclamante a , ",
	" sep_det_solicitud          b , ",
	" sep_estado_separacion      e   ",
	l_criterio      CLIPPED

	EXIT CASE

WHEN "A"
    LET    l_criterio      =
          " WHERE   ",cad_construct CLIPPED ,
            " AND    a.estado           =  5" ,
            " AND    a.folio            =  b.folio ",
            " AND    a.n_seguro         =  b.n_seguro ",
            " AND    b.diag_confronta   = '01' ",
            " AND    (b.resultado_operacion in ('',' ','01') OR ",
            "         b.resultado_operacion is null ) ", 
            " AND    a.folio            =  c.folio ",
            " AND    a.nss              =  c.nss_asociado ",
            " AND    c.resultado_operacion <> '02' ",
            " AND    c.cont_servicio    =  b.cont_servicio + 1 ",
            " AND    c.nss_asociado NOT IN (SELECT  d.n_seguro FROM ",
            "                                      afi_solicitud d        ",
            "                               WHERE  tipo_solicitud = ",
                                                   l_tipo_solicitud,")"
          LET txt_nss = 
          " SELECT UNIQUE '',b.folio,b.diag_confronta  , ",    
                        " b.clasifica_separacion , ",
                        " b.clave_entidad_admon  , ", 
                        " b.n_seguro             , ", 
                        " c.nss_asociado         , ",
                        " ' '                    , ",
                        " ' '                    , ",
			" c.clave_entidad_involucrado ",
          " FROM   sep_det_reg_sol_reclamante a , ",
                 " sep_det_solicitud          b , ",
                 " sep_det03_solicitud        c   ",
               l_criterio      CLIPPED
          
    EXIT CASE
WHEN "C"
       LET      l_criterio     =
                 " WHERE   ",cad_construct CLIPPED ,
                   " AND    a.estado           =  5" ,
                   " AND    a.folio            =  b.folio ",
                   " AND    a.n_seguro         =  b.n_seguro ",
                   " AND    b.diag_confronta   = '01' ",
                   " AND    (b.resultado_operacion IN ('',' ','01') OR ",
                   "         b.resultado_operacion IS NULL ) ", 
                   " AND    a.folio            =  c.folio ",
                   " AND    a.nss              =  c.nss_asociado ",
                   " AND    c.resultado_operacion  <> '02' ",
                   " AND    c.cont_servicio    =  b.cont_servicio + 1 ",
                   " AND    c.nss_asociado     =  d.n_seguro  ",
                   " AND    d.tipo_solicitud   = ",l_tipo_solicitud,
                   " AND    d.status_interno   =  e.estado_cod"

       LET     txt_nss = 
                " SELECT  UNIQUE '',b.folio,b.diag_confronta  , ",    
                        " b.clasifica_separacion , ",
                        " b.clave_entidad_admon  , ", 
                        " b.n_seguro             , ", 
                        " c.nss_asociado         , ",
                        " d.status_interno       , ",
                        " e.estado_desc          , ",
			" c.clave_entidad_involucrado ",
                " FROM   sep_det_reg_sol_reclamante a , ",
                       " sep_det_solicitud          b , ",
                       " sep_det03_solicitud        c , ",
                       " afi_solicitud              d , ",
                       " tab_status_afi             e   ",
                 l_criterio      CLIPPED
        EXIT CASE

    END CASE

     CASE l_tipo_solicitud 

	  WHEN 6
           LET txt_nss = txt_nss CLIPPED,
		" AND c.tipo_entidad_nss_involucrado IN ('00','59') "
		EXIT CASE

	  WHEN 7
           LET txt_nss = txt_nss CLIPPED,
		" AND c.tipo_entidad_nss_involucrado = '01' " ,
                " AND c.clave_entidad_involucrado <> 562 "
		EXIT CASE
          OTHERWISE 
	       EXIT CASE
          END CASE
          IF      l_opc      =  "T"     THEN
                  LET     txt_nss    =  txt_nss   CLIPPED,
                  	  " ORDER  BY  a.estado,a.n_seguro  "
	  ELSE
                  LET     txt_nss    =  txt_nss   CLIPPED,
                          " ORDER  BY  a.estado,b.folio,b.diag_confronta,b.clasifica_separacion,b.n_seguro  "
	  END IF
           PREPARE qry_nss FROM txt_nss 
	   DECLARE cur_nss CURSOR FOR qry_nss


WHILE TRUE
   
           LET cont_1 = 1
   
           FOREACH   cur_nss INTO arr_1[cont_1].*,arr_2[cont_1].cve_cedente
	      IF      l_tipo_solicitud      =  6   OR
		      l_tipo_solicitud      =  7    THEN
		      SELECT   UNIQUE a.status_interno,b.estado_desc
		        INTO   arr_1[cont_1].status_interno,
    	                       arr_1[cont_1].estado_desc
	        	FROM   afi_solicitud      a,
	                       tab_status_afi   b
		       WHERE   a.n_seguro       =  arr_1[cont_1].nss_asociado
	          	AND   a.status_interno  =  b.estado_cod
		        AND   a.tipo_solicitud  =  l_tipo_solicitud
	      END IF


	      LET arr_1[cont_1].arc_1 = cont_1

            IF (arr_1[cont_1].estado_desc IS NULL OR 
                  arr_1[cont_1].estado_desc = " ")     THEN
		            LET arr_1[cont_1].estado_desc = "FALTA CAPTURA"
   	      END IF

              LET cont_1 = cont_1 + 1
           END FOREACH
  
          IF cont_1 = 1 THEN
              PROMPT " NO EXISTEN REGISTROS ...<ENTER> PARA SALIR "
              ATTRIBUTE(REVERSE)
              FOR CHAR enter
	          CLOSE WINDOW ventana_nss
             EXIT WHILE
	  ELSE  

          LET total_pa = cont_1 - 1 

	  DISPLAY BY NAME total_pa

           CALL SET_COUNT(cont_1-1)
            
             LET arc_1 = 0

             DISPLAY ARRAY arr_1 TO  scr_1.*

             ON KEY (CONTROL-I)

		LET arc_1 = ARR_CURR()
		LET x_nss = arr_1[arc_1].nss_asociado

      CASE l_opc 
		WHEN "A" 
          LET ACCION = "A"
          CALL Agrega(x_nss,l_tipo_solicitud,arr_2[arc_1].cve_cedente)
          LET band = 1
          EXIT DISPLAY
		    EXIT CASE

      WHEN "C" 
          LET ACCION = "C"
          IF (arr_1[arc_1].status_interno IS NULL OR 
              arr_1[arc_1].status_interno = " ")     THEN
              ERROR"REGISTRO SIN INFORMACION ..."     
          ELSE
		        CALL Consulta(x_nss)
          END IF
          LET band = 1
          EXIT DISPLAY
		    EXIT CASE
      END CASE

  ON KEY (INTERRUPT)
      LET band = 0
      EXIT DISPLAY
      END DISPLAY 

     IF band = 0 THEN
              PROMPT " PROCESO FINALIZADO...<ENTER> PARA SALIR " 
              ATTRIBUTE(REVERSE)
              FOR CHAR enter
	      CLOSE WINDOW ventana_nss
              EXIT WHILE
     END IF



 
     END IF
END WHILE
      ELSE 
	      CLOSE WINDOW ventana_nss
      END IF

   END FUNCTION
