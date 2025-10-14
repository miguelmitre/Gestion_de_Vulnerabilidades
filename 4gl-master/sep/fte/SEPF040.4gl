DATABASE safre_af 

GLOBALS 
DEFINE g_folio INTEGER
DEFINE ACCION CHAR(001)
DEFINE enter  CHAR(001)
DEFINE HOY    DATE
DEFINE g_ejecuta CHAR(100)
DEFINE g_ejecuta_rep CHAR(100)
DEFINE g_fecha_proceso DATE
DEFINE g_fecha_1nat    DATE
END GLOBALS 

MAIN
    OPTIONS INPUT WRAP,
    PROMPT LINE LAST  ,
    ACCEPT KEY CONTROL-I
    DEFER INTERRUPT

CALL STARTLOG("SEPF040.log")

INITIALIZE g_folio TO NULL
LET HOY = TODAY
LET g_fecha_1nat = MDY(MONTH(HOY),'01',YEAR(HOY))
 CALL cal_fecha_avant(g_fecha_1nat,1) RETURNING g_fecha_proceso
      OPEN WINDOW ventana_menu AT 2,2 WITH 20 ROWS, 78 COLUMNS ATTRIBUTE(BORDER)
      DISPLAY " SEPF040    REVERSO DE REGISTROS LIQUIDADOS EN SEPARACION DE CUENTAS           " AT  3,1 ATTRIBUTE(REVERSE)
      DISPLAY HOY USING "dd-mm-yyyy" AT 3,68 ATTRIBUTE(REVERSE)

        MENU "REVERSO DE LIQUIDACION"
        COMMAND KEY("R") "Reverso" "Busqueda Registros Liquidados para Reversar"
               CALL despliega_nss("C",6)
         COMMAND "Salir" "Salir del Programa"

               EXIT PROGRAM
         END MENU
END MAIN

FUNCTION despliega_nss(l_opc,l_tipo_solicitud)
#dnss------------------------------------------

    DEFINE bnd_mod CHAR(01)
    DEFINE l_tipo_solicitud SMALLINT
    DEFINE x_n_folio        DECIMAL(11,0)
    DEFINE l_opc  CHAR(001)
    DEFINE cont_1 INTEGER

    DEFINE arr_1 ARRAY[400] OF RECORD
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

    DEFINE arr_2 ARRAY[400] OF RECORD 
	   cve_cedente     CHAR(003)
    END RECORD

    DEFINE cuantos      SMALLINT
    DEFINE txt_nss      CHAR(1000)
    DEFINE txt_count     CHAR(1000)
    DEFINE cad_construct CHAR(50)
    DEFINE x_nss        CHAR(11)
    DEFINE arc_1        INTEGER
    DEFINE total_pa        INTEGER
    DEFINE tot_folio_ts        INTEGER
    DEFINE band         SMALLINT
    DEFINE band_c       SMALLINT

    OPEN WINDOW ventana_nss AT 2,2 WITH FORM "SEPC0101" ATTRIBUTE(BORDER)

    DISPLAY "  [Ctrl-I] Capturar                                     [ Ctrl-C ] Salir       " AT 1,1 ATTRIBUTE(REVERSE)

    DISPLAY " SEFB040             NSS EN POSIBILIDAD DE REVERSAR                            " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,68 ATTRIBUTE(REVERSE)

    CONSTRUCT BY NAME cad_construct ON b.folio
     ON KEY(ESC)
          LET int_flag = FALSE
          EXIT CONSTRUCT
    ON KEY(INTERRUPT)
       LET band_c = 0
       EXIT CONSTRUCT
   END CONSTRUCT

   IF NOT int_flag THEN 


 CASE l_opc 
WHEN "A"
          LET txt_nss = 
          " SELECT unique '',b.folio,b.diag_confronta  , ",    
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
          " WHERE   ",cad_construct CLIPPED ,
            " AND    c.cont_servicio  = b.cont_servicio + 1 ",
            " AND    b.diag_confronta  <> '02' ",
            " AND    b.resultado_operacion is null ",
            " AND    c.resultado_operacion <> '02' ",
            " AND    a.n_seguro = b.n_seguro "
    EXIT CASE
WHEN "C"

          LET txt_nss = 
          " SELECT unique '',b.folio,b.diag_confronta  , ",    
                        " b.clasifica_separacion , ",
                        " b.clave_entidad_admon  , ", 
                        " b.n_seguro             , ", 
                        " c.nss_asociado         , ",
                        " a.estado               , ",
                        " e.des_estado          , ",
                        " c.clave_entidad_involucrado ",
          " FROM   sep_det_reg_sol_reclamante a , ",
                 " sep_det_solicitud          b , ",
                 " sep_det03_solicitud        c , ",
                 " sep_estado_separacion      e , ",
                 " afi_mae_afiliado           d   ",
          " WHERE   ",cad_construct CLIPPED ,
            " AND    b.folio = a.folio ",
            " AND    b.folio = c.folio ",
            " AND    c.cont_servicio  = b.cont_servicio + 1 ",
            " AND    b.diag_confronta  = '01' ",
            " AND    b.clasifica_separacion in ('A','B','C') ",
            " AND   (b.resultado_operacion in (' ','','01') OR ",
            "        b.resultado_operacion is null) ",
            " AND    c.resultado_operacion <> '02' ",
            " AND    a.n_seguro = b.n_seguro ",
            " AND    a.nss      = d.n_seguro "


        EXIT CASE

    END CASE

     CASE l_tipo_solicitud 

	  WHEN 6
           LET txt_nss = txt_nss CLIPPED,
		" AND c.tipo_entidad_nss_involucrado IN ('00','59','01') "
		EXIT CASE

	  WHEN 7
           LET txt_nss = txt_nss CLIPPED,
		" AND c.tipo_entidad_nss_involucrado = '01' "
		EXIT CASE
          OTHERWISE 
	       EXIT CASE
          END CASE


      CASE l_opc
      WHEN "A" 
           LET txt_nss = txt_nss CLIPPED ,
           "AND   c.nss_asociado NOT IN (SELECT d.n_seguro FROM ",
           "                             afi_solicitud d)"
        EXIT CASE
      WHEN "C" 
           LET txt_nss = txt_nss CLIPPED ,
           " AND   c.nss_asociado = a.nss ",
           " AND   a.estado  = e.estado   ",
           " AND   a.estado = 8 " ,
           " AND   a.fecha_proceso = ","'",g_fecha_proceso,"'"
        EXIT CASE
      END CASE

      --PREPARE qry_nss FROM txt_nss 
      --DECLARE cur_nss CURSOR FOR qry_nss

WHILE TRUE
      PREPARE qry_nss FROM txt_nss 
      DECLARE cur_nss CURSOR FOR qry_nss
   
           LET cont_1 = 1
   
           FOREACH cur_nss INTO arr_1[cont_1].*,arr_2[cont_1].cve_cedente
{
              SELECT a.status_interno,b.estado_desc
	           INTO   arr_1[cont_1].status_interno,
		               arr_1[cont_1].estado_desc
	           FROM   afi_solicitud      a,
		               tab_status_afi   b
              WHERE  a.n_seguro       = arr_1[cont_1].nss_asociado
	           AND    a.status_interno = b.estado_cod
}
	           LET arr_1[cont_1].arc_1 = cont_1
{
              IF (arr_1[cont_1].estado_desc IS NULL OR 
                  arr_1[cont_1].estado_desc = " ")     THEN
           		 LET arr_1[cont_1].estado_desc = "FALTA CAPTURA"
   	        END IF
}
              LET cont_1 = cont_1 + 1
           END FOREACH
  
           IF cont_1 = 1 THEN
              ERROR ""
              CLEAR FORM
              PROMPT " NO EXISTEN REGISTROS PARA SEPARAR...<ENTER> PARA SALIR "
              ATTRIBUTE(REVERSE)
              FOR CHAR enter
	          CLOSE WINDOW ventana_nss
             EXIT WHILE
	   ELSE  

      CASE l_tipo_solicitud 
      WHEN 6 

          LET txt_count = 
          " SELECT COUNT(*) ",
          " FROM   sep_det_reg_sol_reclamante a , ",
                 " sep_det_solicitud          b , ",
                 " sep_det03_solicitud        c   ",
          " WHERE   ",cad_construct CLIPPED ,
            " AND    b.folio = a.folio ",
            " AND    b.folio = c.folio ",
            " AND    c.cont_servicio  = b.cont_servicio + 1 ",
            " AND    b.diag_confronta  = '01' ",
            " AND    b.clasifica_separacion in ('A','B','C') ",
            " AND    (b.resultado_operacion in (' ','','01') OR ",
            "         b.resultado_operacion is null) ",
            " AND    c.resultado_operacion <> '02' ",
            " AND    a.n_seguro = b.n_seguro ",
            " AND    c.tipo_entidad_nss_involucrado IN ('00','59','01') ",
            " AND    a.estado = 8 " ,
            " AND    a.fecha_proceso = ","'",g_fecha_proceso,"'"

        EXIT CASE
      WHEN 7 
          LET txt_count = 
          " SELECT COUNT(*) ",
          " FROM   sep_det_reg_sol_reclamante a , ",
                 " sep_det_solicitud          b , ",
                 " sep_det03_solicitud        c   ",
          " WHERE   ",cad_construct CLIPPED ,
            " AND    b.folio = a.folio ",
            " AND    b.folio = c.folio ",
            " AND    c.cont_servicio  = b.cont_servicio + 1 ",
            " AND    b.diag_confronta  <> '02' ",
            " AND    (b.resultado_operacion in (' ','','01') OR ",
            "         b.resultado_operacion is null) ",
            " AND    c.resultado_operacion <> '02' ",
            " AND    a.n_seguro = b.n_seguro ",
		      " AND    c.tipo_entidad_nss_involucrado = '01' "

        EXIT CASE
      END CASE

      PREPARE qry_txt_count FROM txt_count
      EXECUTE qry_txt_count INTO tot_folio_ts

          LET total_pa = cont_1 - 1 
	  DISPLAY BY NAME total_pa
	  DISPLAY BY NAME tot_folio_ts

           CALL SET_COUNT(cont_1-1)
            
             LET arc_1 = 0

             DISPLAY ARRAY arr_1 TO  scr_1.*

             ON KEY (CONTROL-I)

		LET arc_1 = ARR_CURR()
		LET x_nss = arr_1[arc_1].nss_asociado

                CASE l_opc 
		WHEN "A" 
                   LET ACCION = "A"
                      LET band = 1
                    EXIT DISPLAY

		   EXIT CASE

                WHEN "C" 
                   LET ACCION = "C"

                     IF (arr_1[arc_1].status_interno IS NULL OR 
                         arr_1[arc_1].status_interno = " ")     THEN

                         ERROR"REGISTRO SIN INFORMACION ..."     

                     ELSE
		                 LET arc_1 = ARR_CURR()

                       LET g_folio = arr_1[arc_1].folio

                       LET g_ejecuta = "fglgo SEPB040 ",arr_1[arc_1].folio," ",
                                        arr_1[arc_1].nss            ," ",
                                        arr_1[arc_1].nss_asociado   ," ",
                                        g_fecha_proceso

                        RUN g_ejecuta
                    LET band = 1
                    EXIT DISPLAY

                     END IF
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

FUNCTION cal_fecha_avant(x_fecha,ciclo)
#cf-------------------------------

    DEFINE cc         SMALLINT
    DEFINE x_fecha    DATE
    DEFINE ciclo      SMALLINT

    DEFINE sig_fecha    DATE
    DEFINE dia_semana SMALLINT
    DEFINE ant_habil  SMALLINT

    LET sig_fecha  = x_fecha
    LET cc = 1

    WHILE cc <= ciclo
        LET dia_semana = WEEKDAY(x_fecha)
        IF dia_semana = 0 OR dia_semana = 6 THEN
        LET x_fecha  = x_fecha + 1
           CONTINUE WHILE
        ELSE
           SELECT "ok"
           FROM   tab_feriado
           WHERE  feria_fecha = x_fecha
           IF STATUS <> NOTFOUND THEN
              LET x_fecha  = x_fecha + 1
              CONTINUE WHILE
           ELSE
         LET cc = cc + 1
           END IF
        END IF
    END WHILE
    RETURN x_fecha
END FUNCTION
