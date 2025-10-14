DATABASE safre_af
GLOBALS
DEFINE g_enter char(001)
   DEFINE r27       RECORD
          n_seguro        LIKE safre_af:sep_det_solicitud.n_seguro,
          paterno         LIKE safre_af:sep_det_solicitud.paterno,
          materno         LIKE safre_af:sep_det_solicitud.materno,
          nombres         LIKE safre_af:sep_det_solicitud.nombres,
          diag_confro     LIKE safre_af:sep_det_solicitud.diag_confronta,
          clas_sep        LIKE safre_af:sep_det_solicitud.clasifica_separacion,
          estado          LIKE safre_af:sep_det_reg_sol_reclamante.estado,
          des_estado      LIKE safre_af:sep_estado_separacion.des_estado ,
	  cont_servicio   LIKE safre_af:sep_det_solicitud.cont_servicio,
	  nss_asociado    LIKE safre_af:sep_det03_solicitud.nss_asociado,
	  tipo_ent        LIKE safre_af:sep_det03_solicitud.tipo_entidad_nss_involucrado,
	  cve_ent         LIKE safre_af:sep_det03_solicitud.clave_entidad_involucrado,
	  resultado       LIKE safre_af:sep_det03_solicitud.resultado_operacion,
	  d1              LIKE safre_af:sep_det03_solicitud.diag_proc1,
	  d2              LIKE safre_af:sep_det03_solicitud.diag_proc2,
	  d3              LIKE safre_af:sep_det03_solicitud.diag_proc3
                    END RECORD

END GLOBALS

GLOBALS "SEPRPTS.4gl"    

################################################################################
FUNCTION genera_reporte()
#gr---------------------
DEFINE l_hora  CHAR(005)
DEFINE l_enter CHAR(005)

DEFINE l_impre_rep    ,
       l_lista        CHAR(300)

    LET l_hora = TIME

    LET l_impre_rep = g_seg_modulo.ruta_listados CLIPPED,"/",g_usuario CLIPPED,
          ".LIS_",g_nombre_prog CLIPPED,".",hoy USING "DDMMYY", 
	  "_",l_hora[1,2],"_",l_hora[4,5] 
    LET l_impre_rep  = l_impre_rep  CLIPPED

    START REPORT rpt_oper27 TO l_impre_rep

    CALL cuentas_impri()

    LET l_lista = "chmod 777 ",l_impre_rep CLIPPED
    RUN l_lista

  --LET l_lista = "lp ",l_impre_rep
  --RUN l_lista

END FUNCTION
################################################################################
FUNCTION cuentas_impri()
#ci--------------------

   LET r27.estado                         =                    " "

   CALL define_querys()
   PREPARE sql_02 FROM g_sql_02
   PREPARE sql_01 FROM g_sql_01

   DECLARE cur_sql_01  CURSOR  FOR sql_01

   FOREACH cur_sql_01  USING g_folio
   INTO r27.n_seguro,r27.paterno,r27.materno,
        r27.nombres,r27.diag_confro,r27.clas_sep,
	r27.cont_servicio

      SELECT a.estado
      INTO r27.estado
      FROM sep_det_reg_sol_reclamante a
      WHERE a.n_seguro = r27.n_seguro
      IF ( STATUS = NOTFOUND ) THEN 
         LET r27.estado                         =                    " "
      END IF 

        
      SELECT b.des_estado
      INTO r27.des_estado
      FROM sep_estado_separacion b
      WHERE b.estado = r27.estado
      IF ( STATUS = NOTFOUND ) THEN 
         LET r27.des_estado                     =                   "SIN ESTADO"   
      END IF 
 
      LET r27.cont_servicio                    =         r27.cont_servicio  + 1
      EXECUTE sql_02 USING g_folio ,
			   r27.cont_servicio 
                     INTO  r27.nss_asociado  ,
			   r27.tipo_ent      ,
			   r27.cve_ent       ,
			   r27.resultado     ,
			   r27.d1            ,
			   r27.d2            ,
			   r27.d3            

         OUTPUT TO REPORT rpt_oper27(r27.*)

   END FOREACH

   FINISH REPORT rpt_oper27

   DISPLAY "REPORTE GENERADO EN: " at 16,2
   DISPLAY g_seg_modulo.ruta_listados AT 17,2 ATTRIBUTE(REVERSE)

END FUNCTION 
################################################################################
REPORT rpt_oper27(l_r27)
DEFINE l_r27       RECORD
          n_seguro        LIKE safre_af:sep_det_solicitud.n_seguro,
          paterno         LIKE safre_af:sep_det_solicitud.paterno,
          materno         LIKE safre_af:sep_det_solicitud.materno,
          nombres         LIKE safre_af:sep_det_solicitud.nombres,
          diag_confro     LIKE safre_af:sep_det_solicitud.diag_confronta,
          clas_sep        LIKE safre_af:sep_det_solicitud.clasifica_separacion,
          estado          LIKE safre_af:sep_det_reg_sol_reclamante.estado,
          des_estado      LIKE safre_af:sep_estado_separacion.des_estado,
	  cont_servicio   LIKE safre_af:sep_det_solicitud.cont_servicio,
	  nss_asociado    LIKE safre_af:sep_det03_solicitud.nss_asociado,
	  tipo_ent        LIKE safre_af:sep_det03_solicitud.tipo_entidad_nss_involucrado,
	  cve_ent         LIKE safre_af:sep_det03_solicitud.clave_entidad_involucrado,
	  resultado       LIKE safre_af:sep_det03_solicitud.resultado_operacion,
	  d1              LIKE safre_af:sep_det03_solicitud.diag_proc1,
	  d2              LIKE safre_af:sep_det03_solicitud.diag_proc2,
	  d3              LIKE safre_af:sep_det03_solicitud.diag_proc3
                    END RECORD

DEFINE campo        RECORD
           afore_cod  CHAR(03),
           raz_social CHAR(50)
                    END  RECORD

DEFINE lnombre        CHAR(120)


  OUTPUT
        TOP MARGIN    0
        BOTTOM MARGIN 0
        LEFT MARGIN   0
        RIGHT MARGIN  0
        PAGE LENGTH   90

   ORDER BY l_r27.estado 

    FORMAT

        PAGE HEADER
           PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
           PRINT COLUMN 02,"__________________________________________________",
                           "__________________________________________________",
                           "__________________________________________________",
                           "______________________________"

           SELECT codigo_afore,razon_social
           INTO campo.afore_cod,campo.raz_social
           FROM tab_afore_local

        SKIP 1 LINE
        PRINT COLUMN 02,"PROGRAMA         :      ",g_nombre_prog,
              COLUMN 165,"Pagina: ",PAGENO USING "<<<<"
        PRINT COLUMN 02,"ADMINISTRADORA   : ","     ",campo.afore_cod USING "###","  ",campo.raz_social
        PRINT COLUMN 02,"FOLIO            :",g_folio,"   ",g_tipo_desc1
        PRINT COLUMN 02,"FECHA GENERACION :","      ",TODAY  USING"DD-MM-YYYY"
        PRINT COLUMN 02,"__________________________________________________",
                        "__________________________________________________",
                        "__________________________________________________",
                        "______________________________"
        SKIP 1 LINE

        PRINT COLUMN 002,"NSS",
              COLUMN 015,"NOMBRE",
              COLUMN 060,"DIAG",
              COLUMN 065,"CLASIFICA",
              COLUMN 076,"NSS ASOC",
              COLUMN 088,"ENT",
              COLUMN 092,"CVE",
              COLUMN 097,"RES",
              COLUMN 101,"D1",
              COLUMN 107,"D2",
              COLUMN 112,"D3"
        PRINT COLUMN 02,"--------------------------------------------------",
                        "--------------------------------------------------",
                        "--------------------------------------------------",
                        "------------------------------"
        
        ON EVERY ROW
             LET  lnombre                    =      l_r27.paterno CLIPPED,"  ",
                                                    l_r27.materno CLIPPED,"  ",
                                                    l_r27.nombres CLIPPED
           PRINT COLUMN 002,l_r27.n_seguro,
                 COLUMN 015,lnombre CLIPPED,
                 COLUMN 060,l_r27.diag_confro  ,
                 COLUMN 065,l_r27.clas_sep     ,
                 COLUMN 076,l_r27.nss_asociado , 
                 COLUMN 088,l_r27.tipo_ent, 
                 COLUMN 092,l_r27.cve_ent,
                 COLUMN 097,l_r27.resultado,
                 COLUMN 101,l_r27.d1,
                 COLUMN 107,l_r27.d2,
                 COLUMN 112,l_r27.d3

        BEFORE GROUP  OF  l_r27.estado 
          

           PRINT COLUMN 02,'\033(s7B',"ESTADO:  ",'\033(s0B',l_r27.estado USING "###","   ",'\033(s7B',"DESCRIPCION DE ESTADO:   ",'\033(s0B',l_r27.des_estado
        SKIP 1 LINE

        AFTER GROUP OF l_r27.estado
           SKIP 1 LINES
           PRINT COLUMN 02,"--------------------------------------------------",
                           "--------------------------------------------------",
                           "--------------------------------------------------",
                           "------------------------------"

           PRINT COLUMN 02,"TOTAL POR ESTADO:  ",GROUP COUNT(*)  USING "######","  ","REGISTRO(S)"


           PRINT COLUMN 02,"--------------------------------------------------",
                           "--------------------------------------------------",
                           "--------------------------------------------------",
                           "------------------------------"
        
           SKIP 2 LINE

        ON LAST ROW

           SKIP 1 LINE

           PRINT COLUMN  2,"TOTAL NSS","==>",g_total_cuentas USING "######&"," ","REGISTRO(S)"

END REPORT 
