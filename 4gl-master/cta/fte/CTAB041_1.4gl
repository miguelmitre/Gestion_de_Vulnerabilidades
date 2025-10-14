############################################################################
#Propietario         => EFP                                                #
#Programa CTAB041_1  => GENERA REPORTE DE REGISTROS ACEPTADOS Y RECHAZADOS #
#                       DEL PROCESO DE IDENTIFICACION POR EDAD             #
#Fecha creacion      => 22 SEPTIEMBRE 2011                                 #
#Por                 => STEFANIE DANIELA VERA PIÑA                         #
############################################################################

DATABASE safre_af

GLOBALS

   DEFINE reg_repor RECORD
      fecha_calculo       DATE,
      nss                 CHAR(11),
      tipo_solicitud      CHAR(28),
      edad                SMALLINT,
      siefore_ant         SMALLINT,
      siefore             SMALLINT,
      subcuenta           SMALLINT,
      monto_en_acciones   DECIMAL(16,6),
      desc_estado         CHAR(150)
   END RECORD

   DEFINE 
      vruta_rescate     CHAR(40),
      G_LISTA           CHAR(100),
      permisos          CHAR(100),
      HOY               DATE,
      vfecha_corte      DATE,
      vtipo             SMALLINT

END GLOBALS   

MAIN 

   LET vfecha_corte = ARG_VAL(1)
   LET vTipo = ARG_VAL(2)
   LET HOY = TODAY
   
   SELECT ruta_rescate
   INTO   vruta_rescate
   FROM   safre_af:seg_modulo
   WHERE  modulo_cod = "cta"
   
   #-- REPORTE DE REGISTROS ACEPTADOS --#

IF vTipo = 1 THEN
 DATABASE safre_tmp
ELSE
 DATABASE safre_af
END IF
   
   LET G_LISTA = vruta_rescate CLIPPED,"/","REPORTE_TES_ACEP_",HOY  USING "YYYYMMDD",".txt"

   START REPORT genera_archivo TO G_LISTA
   DECLARE cur_1 CURSOR FOR
   SELECT a.fecha_calculo,
          a.nss,
          DECODE(a.tipo_solicitud, 1, 'REGISTRO INICIAL ',
                           2, 'TRASPASO ',
                           3, 'UNIFICACION ',
                           4, 'VIRTUAL ',
                           5, 'ASIGNACION ',
                           6, 'REGISTRO SEPARACION  ',
                           7, 'TRASPASO SEPARACION ',
                           8, 'NO AFILIADO ',
                           9, 'TRASPASO INTERNET ',
                           10, 'ACTIVACION ',
                           11, 'REGISTRO INTERNET ',
                           12, 'NO AFIL INTERNET ',
                           13, 'TRASPASO INDEBIDO ',
                           15, 'TRASPASO POR CURP ',
                           17, 'TRASPASO INDEBIDO POR CURP'),
          b.edad,
          a.siefore_ant,
          a.siefore,
          a.subcuenta,
          a.monto_en_acciones,
          "SIN MARCA"
   FROM   safre_tmp:tmp_reporte_tes a,
          cta_transf_edad b
   WHERE  a.estado =  0 
   AND    a.cod_rechazo IN  (0,2)
   AND    a.nss = b.nss
   AND    b.fecha_corte = vfecha_corte
   ORDER BY nss,subcuenta

   FOREACH cur_1 INTO reg_repor.*
      OUTPUT TO REPORT genera_archivo(reg_repor.*)
   END FOREACH

   FINISH REPORT genera_archivo

   LET permisos = " chmod 777 ",G_LISTA CLIPPED
   RUN permisos

   #-- REPORTE DE REGISTROS ACEPTADOS PERO SIN SALDO EN RCV --#

   SELECT nss
   FROM   safre_tmp:tmp_reporte_tes
   WHERE  estado = 0
   AND    cod_rechazo IN  (0,2)
   GROUP BY 1
   HAVING SUM(monto_en_acciones) = 0
   INTO TEMP tmp_ctab041

   LET G_LISTA = vruta_rescate CLIPPED,"/","REPORTE_TES_ACEPRCV_",HOY  USING "YYYYMMDD",".txt"

   START REPORT genera_archivo TO G_LISTA
   DECLARE cur_8 CURSOR FOR
   SELECT a.fecha_calculo,
          a.nss,
          DECODE(a.tipo_solicitud, 1, 'REGISTRO INICIAL ',
                           2, 'TRASPASO ',
                           3, 'UNIFICACION ',
                           4, 'VIRTUAL ',
                           5, 'ASIGNACION ',
                           6, 'REGISTRO SEPARACION  ',
                           7, 'TRASPASO SEPARACION ',
                           8, 'NO AFILIADO ',
                           9, 'TRASPASO INTERNET ',
                           10, 'ACTIVACION ',
                           11, 'REGISTRO INTERNET ',
                           12, 'NO AFIL INTERNET ',
                           13, 'TRASPASO INDEBIDO ',
                           15, 'TRASPASO POR CURP ',
                           17, 'TRASPASO INDEBIDO POR CURP'),
          b.edad,
          a.siefore_ant,
          a.siefore,
          a.subcuenta,
          a.monto_en_acciones,
          "SIN MARCA"
   FROM   safre_tmp:tmp_reporte_tes a,
          cta_transf_edad b
   WHERE  a.estado =  0 
   AND    a.cod_rechazo IN  (0,2)
   AND    a.nss IN (SELECT nss FROM tmp_ctab041)
   AND    a.nss = b.nss
   AND    b.fecha_corte = vfecha_corte
   ORDER BY nss,subcuenta

   FOREACH cur_8 INTO reg_repor.*
      OUTPUT TO REPORT genera_archivo(reg_repor.*)
   END FOREACH

   FINISH REPORT genera_archivo

   LET permisos = " chmod 777 ",G_LISTA CLIPPED
   RUN permisos


   #-- REPORTE DE REGISTROS RECHAZADOS --#

   LET G_LISTA = vruta_rescate CLIPPED,"/","REPORTE_TES_RECH_",HOY  USING "YYYYMMDD",".txt"

   START REPORT genera_archivo TO G_LISTA
   DECLARE cur_2 CURSOR FOR
   SELECT a.fecha_calculo,
          a.nss,
          DECODE(a.tipo_solicitud, 1, 'REGISTRO INICIAL ',
                           2, 'TRASPASO ',
                           3, 'UNIFICACION ',
                           4, 'VIRTUAL ',
                           5, 'ASIGNACION ',
                           6, 'REGISTRO SEPARACION  ',
                           7, 'TRASPASO SEPARACION ',
                           8, 'NO AFILIADO ',
                           9, 'TRASPASO INTERNET ',
                           10, 'ACTIVACION ',
                           11, 'REGISTRO INTERNET ',
                           12, 'NO AFIL INTERNET ',
                           13, 'TRASPASO INDEBIDO ',
                           15, 'TRASPASO POR CURP ',
                           17, 'TRASPASO INDEBIDO POR CURP'),
          b.edad,
          a.siefore_ant,
          a.siefore,
          a.subcuenta,
          a.monto_en_acciones,
          a.desc_estado
   FROM   safre_tmp:tmp_reporte_tes a,
          cta_transf_edad b
   WHERE  (a.estado <> 0 
   OR     a.cod_rechazo = 8)
   AND    a.nss = b.nss
   AND    b.fecha_corte = vfecha_corte
   ORDER BY nss,subcuenta
   
   FOREACH cur_2 INTO reg_repor.*
      OUTPUT TO REPORT genera_archivo(reg_repor.*)
   END FOREACH
   
   FINISH REPORT genera_archivo
   
   LET permisos = " chmod 777 ",G_LISTA CLIPPED
   RUN permisos

   DISPLAY "CTAB041_1: PROCESO FINALIZADO"
   
END MAIN


REPORT genera_archivo(reg_repor)
#--------------------------------

   DEFINE reg_repor RECORD
      fecha_calculo       DATE,
      nss                 CHAR(11),
      tipo_solicitud      CHAR(28),
      edad                SMALLINT,
      siefore_ant         SMALLINT,
      siefore             SMALLINT,
      subcuenta           SMALLINT,
      monto_en_acciones   DECIMAL(16,6),
      desc_estado         CHAR(150)
   END RECORD

    OUTPUT
        PAGE LENGTH   45
        LEFT MARGIN    0
        RIGHT MARGIN   0
        TOP MARGIN     0
        BOTTOM MARGIN  0

    FORMAT
        FIRST PAGE HEADER

           PRINT
              COLUMN 003,"FECHA",
              COLUMN 017,"NSS",
              COLUMN 034,"TIPO SOLICITUD",
              COLUMN 055,"EDAD",
              COLUMN 061,"SIEFORE",
              COLUMN 070,"SIEFORE",
              COLUMN 079,"SUBCUENTA",
              COLUMN 095,"TITULOS",
              COLUMN 113,"PROCESO"

           PRINT
              COLUMN 002,"CALCULO",
              COLUMN 061,"ACTUAL",
              COLUMN 070,"CAMBIO"

           PRINT
              COLUMN 001,"--------------------------------------------",
                         "--------------------------------------------",
                         "--------------------------------------------",
                         "-----------------------------"

        ON EVERY ROW

              PRINT
                 COLUMN 001,reg_repor.fecha_calculo USING "DD/MM/YYYY",
                 COLUMN 013,reg_repor.nss ,
                 COLUMN 028,reg_repor.tipo_solicitud,
                 COLUMN 056,reg_repor.edad USING "##",
                 COLUMN 063,reg_repor.siefore_ant USING "#",
                 COLUMN 073,reg_repor.siefore  USING "#",
                 COLUMN 084,reg_repor.subcuenta USING "##",
                 COLUMN 091,reg_repor.monto_en_acciones USING "#########.######",
                 COLUMN 113,reg_repor.desc_estado 

END REPORT
