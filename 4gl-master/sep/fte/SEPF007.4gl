##############################################################################
#Owner             => E.F.P.
#Programa SEPREP   => GENERA  OP.  29   EPARACION DE CUENTAS   
#Fecha creacion    => 23 de JUNIO DEL 2011
#AUTOR:            => JESUS DAVID YANEZ MORENO
#Sistema           => SEPARACION DE CUENTAS
##############################################################################
DATABASE safre_af 

GLOBALS 
   DEFINE  g_sql_1                     CHAR(1000),
           enter                       CHAR(001),
           g_nss_invadido              CHAR(11),
           g_nss_asociado              CHAR(11),
           g_usuario                   CHAR(10),
           g_today                     DATE,
           g_archivo                   CHAR(100),
           g_ejecuta                   CHAR(100),
           g_cabeza                    CHAR(300),
           g_detalle                   CHAR(300),
           g_sumario                   CHAR(300),
           arc_1                       ,
           g_reg_det                   ,
           g_reg_det_02                ,
           g_reg_det_05                ,
           g_reg_det_03                ,
           g_reg_det_06                ,
           g_t_reg                     ,
           g_reg_tot                   INTEGER,
           i                           ,
           g_ind                       SMALLINT,
      g_seg_modulo                     RECORD LIKE safre_af:seg_modulo.*
   DEFINE  arr_tmp_archivo   ARRAY[1000] OF RECORD
           registro  CHAR(300) 
   END RECORD


   DEFINE  arr_1     ARRAY[1000]  OF  RECORD
	   arc_1                       smallint, 
	   folio                       INTEGER,
	   diag_confronta              char(02),
	   clasifica_separacion        char(01),
	   clave_admon                 CHAR(03),
	   nss                         CHAR(11),
	   nss_asociado                CHAR(11),
	   status_interno              SMALLINT,
	   estado_desc                 CHAR(20)
    END RECORD

END GLOBALS 

MAIN
    OPTIONS  INPUT  WRAP,
    PROMPT   LINE   LAST  ,
    ACCEPT   KEY    CONTROL-I
    --DEFER    INTERRUPT
    CALL     f_010_inicio()
END MAIN

FUNCTION    f_010_inicio()
   CALL     STARTLOG  ('SEPF007.log')

   LET      g_today                      =  TODAY
   SELECT   *,user
     INTO   g_seg_modulo.*,g_usuario
     FROM   seg_modulo
    WHERE   modulo_cod                   =  'sep';
   INITIALIZE      g_detalle            TO    NULL
   CALL     f_020_despliega_nss("C",6)
END FUNCTION

FUNCTION    f_020_despliega_nss(l_opc,l_tipo_solicitud)
   DEFINE   l_tipo_solicitud             SMALLINT
   DEFINE   l_opc                        CHAR(001)
   DEFINE   cont_1                       INTEGER
   DEFINE   txt_nss                      CHAR(1500)
   DEFINE   cad_construct                CHAR(100)
   DEFINE   x_nss                        CHAR(11)
   DEFINE   tot_folio_ts                 INTEGER
   DEFINE   band                         SMALLINT
   DEFINE   band_c                       SMALLINT

   OPEN WINDOW ventana_nss AT 2,2 WITH FORM "SEPF007" ATTRIBUTE(BORDER)

   DISPLAY "  <Esc> Muestra Regs.  [Ctrl-I] Genera Archivo Op. 29    [Ctrl-C ] Salir       " AT 1,1 ATTRIBUTE(REVERSE)

   DISPLAY "<SEPF007>  GENERA OP. 29 (REGISTROS PATRONALES )                           " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY g_today USING "DD-MM-YYYY  " AT 3,68 ATTRIBUTE(REVERSE)


   CONSTRUCT BY NAME   cad_construct  ON   a.folio
         ON KEY(ESC)
                ERROR   "  SELECCIONANDO REGISTROS PARA  OP. 29...                        "
                LET     int_flag = FALSE
                EXIT    CONSTRUCT
         ON KEY(INTERRUPT)
                EXIT PROGRAM
   END CONSTRUCT
   LET    txt_nss = 
          " SELECT unique '',a.folio,b.diag_confronta  , ",    
                        " b.clasifica_separacion , ",
                        " b.clave_entidad_admon  , ", 
                        " a.n_seguro             , ", 
                        " a.nss                  , ",
                        " a.estado               , ",
                        " e.des_estado            ",
          " FROM   sep_det_reg_sol_reclamante  a , ",
          "        sep_det_solicitud           b , ",
          "        sep_estado_separacion       e   ",
          " WHERE   ",cad_construct CLIPPED ,
            " AND    a.estado                  =  11   ",
            " AND    a.correlativo             =  b.idSolicitudSeparacion ",
            " AND    a.estado                  =  e.estado   ",
            " AND    a.correlativo in (select c.idsolicitudseparacion ",
                                      " from sep_op29 c ",
                                      " where c.idactiva = 0) ",
            " UNION " ,
          " SELECT unique '',a.folio,b.diag_confronta  , ",    
                        " b.clasifica_separacion , ",
                        " b.clave_entidad_admon  , ", 
                        " a.n_seguro             , ", 
                        " a.nss                  , ",
                        " a.estado               , ",
                        " e.des_estado            ",
          " FROM   sep_det_reg_sol_reclamante  a , ",
          "        sep_det_solicitud           b , ",
          "        sep_estado_separacion       e   ",
          " WHERE   ",cad_construct CLIPPED ,
            " AND    a.estado                  =  13   ",
            " AND    a.correlativo             =  b.idSolicitudSeparacion ",
            " AND    a.estado                  =  e.estado   ",
            " ORDER  BY  a.n_seguro "  CLIPPED

   PREPARE  qry_nss FROM txt_nss 
   DECLARE  cur_nss CURSOR FOR qry_nss
   LET      cont_1                 =  1
   FOREACH  cur_nss INTO arr_1[cont_1].*
	    LET    arr_1[cont_1].arc_1    =   cont_1
            LET    cont_1                 =  cont_1    +  1
   END FOREACH
   IF       cont_1                  =  1     THEN
            ERROR ""
            CLEAR FORM
            PROMPT " NO HAY REGISTROS PARA NOTIFICAR EN OP. 29 <ENTER> PARA SALIR... "
            ATTRIBUTE(REVERSE)   FOR   enter
            EXIT  PROGRAM
   END IF
   LET      tot_folio_ts        =  cont_1  -  1 
   LET      g_ind               =  tot_folio_ts
   DISPLAY BY NAME tot_folio_ts
   CALL     SET_COUNT(cont_1-1)
   LET      arc_1               =  0
   ERROR ""
   DISPLAY ARRAY arr_1 TO  scr_1.*
          ON KEY    (CONTROL-I)
                  CALL      f_100_genera_archivo()
          ON KEY    (INTERRUPT)
                  EXIT PROGRAM
   END  DISPLAY
END FUNCTION

FUNCTION    f_100_genera_archivo()

   --CREATE   TEMP   TABLE   tmp_archivo (ind   INTEGER,registro  CHAR(730));

   LET      g_reg_det            =  0
   LET      g_reg_det_02         =  0
   LET      g_reg_det_05         =  0
   LET      g_reg_det_03         =  0
   LET      g_reg_det_06         =  0
   LET      g_reg_tot            =  0
   LET      g_archivo                    =
            g_seg_modulo.ruta_envio   CLIPPED,"/",g_usuario   CLIPPED,
            ".SEP_OP_29.",g_today      USING   "YYMMDD"
   START    REPORT       R_590_imprime_archivo     TO  g_archivo
   DISPLAY  "      PROCESANDO  INFORMACION ..............         "  AT  20,1
   CALL     f_120_arma_cza()
   FOR      i        =  1       TO  g_ind
            LET      g_nss_invadido        =  arr_1[i].nss
            LET      g_nss_asociado        =  arr_1[i].nss_asociado
            CALL     f_140_arma_det_02()
            CALL     f_150_arma_det_05()
            CALL     f_160_arma_det_03()
            CALL     f_170_arma_det_06()
            CALL     f_190_arma_det_09()
   END FOR
   CALL     f_200_genera_reporte()
   FINISH   REPORT   R_590_imprime_archivo
   LET      g_ejecuta                     =  "chmod   777 ",g_archivo CLIPPED
   RUN      g_ejecuta
   ERROR    "ARCHIVO:",g_archivo," "    
   PROMPT   "  FIN DE PROCESO TECLEE <Enter> PARA SALIR... "  FOR  enter


   UPDATE sep_det_reg_sol_reclamante 
   SET    estado = 12  --enviado op29
   WHERE  estado = 11 
   AND    correlativo in (select a.idSolicitudSeparacion 
                          from sep_op29 a 
                          where a.idactiva = 0)

   UPDATE sep_det_reg_sol_reclamante
   SET    estado = 12 --enviado op29
   WHERE  estado = 13

   UPDATE sep_op29 
   SET    idactiva = 1 
   WHERE  idactiva = 0

   EXIT     PROGRAM
END FUNCTION
   
FUNCTION    f_120_arma_cza()
   DEFINE   l_codigo_afore           LIKE    tab_afore_local.codigo_afore
   LET      g_t_reg             =  0
   SELECT   codigo_afore,user
     INTO   l_codigo_afore,g_usuario
     FROM   safre_af:tab_afore_local;
   LET      g_cabeza            =  "01022901"        CLIPPED,
                                   l_codigo_afore    USING  "&&&","03001",
                                   g_today           USING  "YYYYMMDD",
                                   '001' CLIPPED
END FUNCTION

FUNCTION    f_140_arma_det_02()
   LET      g_t_reg         =  g_t_reg    +  1
   LET      g_reg_det_02    =  g_reg_det_02     +  1
   LET      g_reg_det    =  g_reg_det     +  1
   LET      g_detalle       =  "02"   CLIPPED,
                               g_reg_det        USING  "&&&&&&&&&&",
                               g_nss_invadido    CLIPPED
   LET arr_tmp_archivo[g_t_reg].registro = g_detalle
   --INSERT   INTO  tmp_archivo    VALUES  (g_t_reg,g_detalle);
END FUNCTION

FUNCTION    f_150_arma_det_05()
--------------------------------

   DEFINE   hay_nrps    SMALLINT

   DEFINE   l_nrp                CHAR(11),
            l_separado           SMALLINT

   LET hay_nrps = 0

   DECLARE  c_nrp_invadido       CURSOR   FOR
   SELECT   UNIQUE(a.reg_patronal_imss)
     FROM   dis_det_aporte  a
    WHERE   a.n_seguro               =  g_nss_invadido
    ORDER   BY  a.reg_patronal_imss
   FOREACH  c_nrp_invadido    INTO   l_nrp
            IF       arr_1[i].clasifica_separacion       =  "B"      THEN
                     CONTINUE FOREACH
            END IF

            IF       (arr_1[i].clasifica_separacion       =  "C"  OR 
                      arr_1[i].clasifica_separacion       =  "D")     THEN

                     LET      l_separado                 =  0
                 
                     SELECT   COUNT(UNIQUE  p.reg_patronal_imss)
                       INTO   l_separado
                       FROM   sep_reg_patro_separador    p
                      WHERE   p.nss_separado             =  g_nss_invadido
                        AND   p.reg_patronal_imss        =  l_nrp
                     IF       l_separado                 >  0     THEN
                              CONTINUE  FOREACH
                     END IF
            END IF
            LET      hay_nrps        =  1
            LET      g_detalle       =  "05"    CLIPPED
            LET      g_reg_det_05    =  g_reg_det_05     +  1
            LET      g_reg_det    =  g_reg_det     +  1
            LET      g_detalle       =  g_detalle     CLIPPED,
                                        g_reg_det  USING  "&&&&&&&&&&",
                                        l_nrp   CLIPPED
            LET      g_t_reg         =  g_t_reg    +  1
            LET      arr_tmp_archivo[g_t_reg].registro = g_detalle
   END FOREACH

   IF hay_nrps = 0 THEN
            LET      g_detalle       =  "05"    CLIPPED
            LET      g_reg_det_05    =  g_reg_det_05     +  1
            LET      g_reg_det       =  g_reg_det     +  1
            LET      g_detalle       =  g_detalle     CLIPPED,
                                        g_reg_det  USING  "&&&&&&&&&&",
                                        "00000000000"     CLIPPED
            LET      g_t_reg         =  g_t_reg    +  1
            LET      arr_tmp_archivo[g_t_reg].registro = g_detalle
   END IF

END FUNCTION

FUNCTION    f_160_arma_det_03()
   LET      g_detalle       =  " "
   LET      g_reg_det_03    =  g_reg_det_03     +  1
   LET      g_reg_det    =  g_reg_det     +  1
   LET      g_detalle       =  "03"       CLIPPED,
                               g_reg_det     USING  "&&&&&&&&&&",
                               g_nss_asociado    CLIPPED
   LET      g_t_reg         =  g_t_reg    +  1
   LET arr_tmp_archivo[g_t_reg].registro = g_detalle
   --INSERT   INTO  tmp_archivo    VALUES  (g_t_reg,g_detalle);
END FUNCTION

FUNCTION    f_170_arma_det_06()
----------------------------------
   DEFINE   hay_nrps    SMALLINT
   DEFINE   l_nrp                CHAR(11),
            l_separado           SMALLINT


   LET hay_nrps = 0

   DECLARE  c_nrp_asociado      CURSOR   FOR
   SELECT   UNIQUE(a.reg_patronal_imss)
     FROM   dis_det_aporte  a
    WHERE   a.n_seguro               =  g_nss_invadido
    ORDER   BY  a.reg_patronal_imss
   FOREACH  c_nrp_asociado    INTO   l_nrp
            IF    (   arr_1[i].clasifica_separacion       =  "C"    OR 
                     arr_1[i].clasifica_separacion       = "D" )  THEN
                     LET      l_separado                 =  0
                     SELECT   COUNT(UNIQUE  p.reg_patronal_imss)
                       INTO   l_separado
                       FROM   sep_reg_patro_separador    p
                      WHERE   p.nss_separado             =  g_nss_invadido
                        AND   p.reg_patronal_imss        =  l_nrp
                     IF       l_separado                 =  0     THEN
                              CONTINUE  FOREACH
                     END IF
            END IF
            LET hay_nrps = 1
            LET      g_detalle       =  " "
            LET      g_reg_det_06    =  g_reg_det_06     +  1
            LET      g_reg_det    =  g_reg_det     +  1
            LET      g_detalle       =  "06"  CLIPPED,
                                        g_reg_det  USING  "&&&&&&&&&&",
                                        l_nrp
            LET      g_t_reg         =  g_t_reg    +  1
   LET arr_tmp_archivo[g_t_reg].registro = g_detalle
   END  FOREACH

   IF hay_nrps = 0 THEN
            LET      g_detalle       =  " "
            LET      g_reg_det_06    =  g_reg_det_06     +  1
            LET      g_reg_det    =  g_reg_det     +  1
            LET      g_detalle       =  "06"  CLIPPED,
                                        g_reg_det  USING  "&&&&&&&&&&",
                                        "00000000000"
            LET      g_t_reg         =  g_t_reg    +  1
   LET arr_tmp_archivo[g_t_reg].registro = g_detalle
    END IF
END FUNCTION

FUNCTION    f_190_arma_det_09()
   LET      g_sumario       =  "09",
                               g_reg_det_02   USING  "&&&&&&&&&&",
                               g_reg_det_05   USING  "&&&&&&&&&&",
                               g_reg_det_03   USING  "&&&&&&&&&&",
                               g_reg_det_06   USING  "&&&&&&&&&&",
                               g_t_reg        USING  "&&&&&&&&&&"
END FUNCTION

FUNCTION    f_200_genera_reporte()
DEFINE f_i INTEGER
{
   DECLARE  c_reporte     CURSOR   FOR
   SELECT   ind,registro
     FROM   tmp_archivo
    ORDER   BY  ind
   FOREACH  c_reporte      INTO  g_ind,g_detalle
            OUTPUT           TO   REPORT    R_590_imprime_archivo()
   END FOREACH
}
FOR f_i = 1 TO g_t_reg 
   LET g_ind = f_i 
   LET g_detalle = arr_tmp_archivo[g_ind].registro
   IF arr_tmp_archivo[g_ind].registro is not null then 
   OUTPUT           TO   REPORT    R_590_imprime_archivo()
   end if
END FOR
END FUNCTION

REPORT      R_590_imprime_archivo()
   OUTPUT
       TOP      MARGIN   0
       BOTTOM   MARGIN   0
       LEFT     MARGIN   0
       RIGHT    MARGIN   0
       PAGE     LENGTH   1
   FORMAT
        ON EVERY ROW
           IF g_ind = 1 THEN 
                PRINT    COLUMN   01,g_cabeza 
           END IF
                PRINT    COLUMN   01,g_detalle
           IF g_ind = g_t_reg THEN 
                PRINT    COLUMN   01,g_sumario
           END IF
END REPORT
