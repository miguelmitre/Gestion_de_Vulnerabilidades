#################################################################################
#Owner             => E.F.P.                                                    #
#Programa RETM013B => FUNCIONES PARA LA CARGA Y RECEPCION DE ARCHIVOS DE LOS    #
#                  => REINTEGROS NO COBRADOS                                    #
#Fecha creacion    => 20 DE DICIEMBRE DE 2021                                   #
#By                => CRISTINA ABASOLO TAPIA                                    #
#Sistema           => RET (CPL-3437)                                            #
#################################################################################

DATABASE safre_af

GLOBALS "RETM015A.4gl"

#--------------------------------------------------------------------------------------#
# f_carga_registros : Realiza la carga de los registros para el reintegro de recursos  #
#                no cobrados de retiros parciales por desempleo.                       #
#--------------------------------------------------------------------------------------#
FUNCTION f_carga_registros()

    DEFINE
        lc_ruta_arch            CHAR(200)
        
    DEFINE
        li_tot_regs             ,
        li_tot_acep             ,
        li_tot_rech             INTEGER

    DEFINE
        ls_estado               SMALLINT,
        ld_monto_calculado      DECIMAL (22,2)
        
    DEFINE ls_marca,
          ls_cod_rechazo,
          ls_error             SMALLINT
        
    DEFINE lr_reintegro         RECORD LIKE ret_par_reintegro_des.*
    DEFINE lr_temp_reint        RECORD
    	     nss                  CHAR(11),
           consecutivo          DECIMAL(11,0),
           folio_liq            DECIMAL(11,0),
           fecha_liq            DATE,
           siefore              SMALLINT,
           monto_en_pesos       DECIMAL(22,2)
    END RECORD
    
    DEFINE v_diferencia         DECIMAL (22,2)
    
    ---DESMARCA CUENTA
    LET gc_char = " "
    LET gc_char = "EXECUTE PROCEDURE desmarca_cuenta ( ?,?,?,?,?,? )"
    PREPARE cur_desmarca FROM gc_char
    
    ---MARCA CUENTA
    LET gc_char = " "
    LET gc_char = "EXECUTE PROCEDURE safre_af:marca_cuenta(?,?,?,?,?,?,?,?)"
    PREPARE cur_marca FROM gc_char
    
     -- Se realiza la carga del archivo
     LET lc_ruta_arch = gr_seg_modulo.ruta_rescate CLIPPED,"/",
                        gr_dat.nom_archivo CLIPPED
                        
     INITIALIZE lr_reintegro.* TO NULL

     SELECT COUNT(*)
     INTO   li_tot_regs
     FROM   tmp_nss_reint_reg

     IF li_tot_regs = 0 THEN
         CALL f_lib_error_msg("ARCHIVO VACIO SIN REGISTROS A CARGAR")
     ELSE
     	   ERROR "PROCESANDO INFORMACION"
     	   
     	   LET li_tot_acep         = 0     
     	   LET li_tot_rech         = 0
     	   
     	   DISPLAY "TOTAL DE REGISTROS PROCESADOS            : ", li_tot_regs AT 10,9
     	   
     	   LET lc_ruta_arch = gr_seg_modulo.ruta_listados CLIPPED, "/",gc_usuario CLIPPED,".RPT_REINTEGRO_CARGA.",
               HOY USING "DDMMYYYY"
     	   
     	   START REPORT r_reporte_carga TO lc_ruta_arch

         DECLARE cur_reint_reg CURSOR FOR
         SELECT *
         FROM   tmp_nss_reint_reg

         FOREACH cur_reint_reg INTO lr_temp_reint.*
         	
         	   LET v_diferencia = 0
         	   LET ls_estado = gr_edo.capturado
         	   LET gc_error = "REGISTRO CARGADO CON EXITO" CLIPPED
         	   
         	   LET lr_reintegro.id_ret_par_reintegro_des = gs_zero
         	   LET lr_reintegro.nss = lr_temp_reint.nss
         	   LET lr_reintegro.consecutivo = lr_temp_reint.consecutivo
         	   LET lr_reintegro.folio_his_liq = lr_temp_reint.folio_liq
         	   LET lr_reintegro.fecha_his_liq = lr_temp_reint.fecha_liq
         	   LET lr_reintegro.siefore = lr_temp_reint.siefore
         	   LET lr_reintegro.monto_en_pesos = lr_temp_reint.monto_en_pesos
         	   LET lr_reintegro.fecha_carga = CURRENT
         	   LET lr_reintegro.usuario = gc_usuario
         	   
         	   SELECT "OK"
         	   FROM ret_par_reintegro_des
             WHERE nss = lr_temp_reint.nss
             AND  consecutivo = lr_temp_reint.consecutivo
             AND  folio_his_liq = lr_temp_reint.folio_liq
             AND  estado NOT IN (gr_edo.liquidado, gr_edo.rechazado, gr_edo.suspendido)
             GROUP BY 1
             
             IF SQLCA.SQLCODE = 0 THEN
                  LET  ls_estado = gr_edo.rechazado
             	    LET  gc_error = "YA EXISTE EL REGISTRO EN PROCESO PARA REINTEGRO " CLIPPED
             END IF
             
         	   SELECT "OK"
             FROM safre_af:afi_mae_afiliado
             WHERE n_seguro = lr_temp_reint.nss
             GROUP BY 1

             IF SQLCA.SQLCODE = 100 AND ls_estado = gr_edo.capturado THEN
             	    LET  ls_estado = gr_edo.rechazado
             	    LET  gc_error = "CUENTA NO ADMINISTRADA POR AFORE" CLIPPED
             END IF
             
             SELECT "OK"
             FROM safre_af:ret_parcial
             WHERE nss = lr_temp_reint.nss
             AND consecutivo = lr_temp_reint.consecutivo
             AND tipo_prestacion = 6
             GROUP BY 1
             
             IF SQLCA.SQLCODE = 100 AND ls_estado = gr_edo.capturado THEN
             	    LET  ls_estado = gr_edo.rechazado
             	    LET  gc_error = "EL REGISTRO NO CORRESPONDE A UNA RETIRO PARCIAL POR DESEMPLEO" CLIPPED
             END IF
             
             CALL f_genera_tmp_cuenta(lr_temp_reint.nss)
             
             SELECT tipo_pago, curp, num_resolucion
             INTO lr_reintegro.tipo_pago, lr_reintegro.curp, lr_reintegro.num_resolucion
             FROM ret_parcial
             WHERE nss = lr_temp_reint.nss
             AND consecutivo = lr_temp_reint.consecutivo
             AND tipo_prestacion = 6
                          
             IF lr_reintegro.tipo_pago >= 4 AND ls_estado = gr_edo.capturado THEN   --Es un reintegro de parcialidades no cobradas
             	     SELECT "OK"
             	     FROM   ret_parcialidad_des
             	     WHERE  nss = lr_temp_reint.nss
                   AND consecutivo = lr_temp_reint.consecutivo
                   AND consec_pago = 1
                   AND estado = gr_edo.liquidado
                   
                   IF SQLCA.SQLCODE = 100 THEN
                   	    LET  ls_estado = gr_edo.rechazado
                   	    LET  gc_error = "EL PAGO DE LA PRIMER PARCIALIDAD NO SE ENCUENTRA LIQUIDADO" CLIPPED
                   END IF
                   
                   IF lr_temp_reint.folio_liq IS NULL OR lr_temp_reint.folio_liq = 0 THEN   --Se trata de reintegro por suspension
                   	
                        SELECT NVL(sum(monto_en_pesos),0)
                        INTO ld_monto_calculado
                        FROM ret_parcialidad_des
                        WHERE  nss = lr_temp_reint.nss
                        AND consecutivo = lr_temp_reint.consecutivo
                        AND estado IN (gr_edo.suspendido, gr_edo.capturado)
                   ELSE        --Se trata de reintegro por parcialidades no cobradas
                        SELECT NVL(sum(monto_en_pesos)*-1,0)
                        INTO ld_monto_calculado
                        FROM tmp_dis_cuenta_3437
                        WHERE  nss = lr_temp_reint.nss
                        AND consecutivo_lote = lr_temp_reint.consecutivo
                        AND folio = lr_temp_reint.folio_liq
                        AND tipo_movimiento IN (875,876,877,878)

                   END IF
                   
                   IF ld_monto_calculado <> lr_temp_reint.monto_en_pesos AND ls_estado = gr_edo.capturado THEN
                       LET v_diferencia = lr_temp_reint.monto_en_pesos - ld_monto_calculado
                       IF (v_diferencia > 0.01) OR (v_diferencia < -0.01) THEN
                           LET  ls_estado = gr_edo.rechazado
                           LET  gc_error = "EL MONTO SOLICITADO PARA EL REINTEGRO NO COINCIDE CON EL DEL CONTROL DE PARCIALIDADES" CLIPPED
                       END IF
                   END IF
             	
             ELSE  --Se trata de un reintegro de pago unico
             	     IF ls_estado = gr_edo.capturado THEN
                       SELECT NVL(sum(monto_en_pesos)*-1,0)
                       INTO ld_monto_calculado
                       FROM tmp_dis_cuenta_3437
                       WHERE  nss = lr_temp_reint.nss
                       AND consecutivo_lote = lr_temp_reint.consecutivo
                       AND folio = lr_temp_reint.folio_liq
                       AND tipo_movimiento IN (875,876,877,878)

                       IF ld_monto_calculado <> lr_temp_reint.monto_en_pesos AND ls_estado = gr_edo.capturado THEN
                           LET v_diferencia = lr_temp_reint.monto_en_pesos - ld_monto_calculado
                           IF (v_diferencia > 0.01) OR (v_diferencia < -0.01) THEN
                               LET  ls_estado = gr_edo.rechazado
                               LET  gc_error = "EL MONTO SOLICITADO PARA EL REINTEGRO NO COINCIDE CON EL MONTO LIQUIDADO" CLIPPED
                           END IF
                       END IF
                   END IF
             END IF
             
             IF ls_estado = gr_edo.capturado THEN
             	   
             	   SELECT "OK"
                 FROM   cta_act_marca
                 WHERE  nss = lr_temp_reint.nss
                 AND    marca_cod = gs_marca_des
                 GROUP BY 1
                 
                 IF SQLCA.SQLCODE = 0 THEN
                      EXECUTE cur_desmarca USING lr_temp_reint.nss,           -- nss
                                                 gs_marca_des ,               -- marca entrante
                                                 lr_temp_reint.consecutivo,   -- consecutivo
                                                 gs_zero          ,           -- estado_marca
                                                 gs_marca_des ,               -- marca_causa
                                                 gc_usuario                   -- usuario
                 END IF
                 
                 DECLARE cur_marca_reg CURSOR FOR cur_marca
                 OPEN cur_marca_reg USING
                          lr_temp_reint.nss,         #pnss
                          gs_marca_proceso,           #pmarca_entra
                          lr_temp_reint.consecutivo,  #pcorrelativo
                          gs_zero,                    #pestado_marca
                          gs_zero,                    #pcodigo_rechazo
                          gs_marca_proceso,           #pmarca_causa
                          HOY,                        #pfecha_causa
                          gc_usuario                  #pusuario
                 
                 FETCH cur_marca_reg INTO ls_marca, ls_cod_rechazo
                 
                 IF ls_cod_rechazo != 0 THEN    #No convive
                 	   IF ls_cod_rechazo != 489 THEN    #No convive
                         LET  ls_estado = gr_edo.rechazado
                         LET  gc_error = "CUENTA EN PROCESO OPERATIVO, NO SE PUEDE APLICAR LA MARCA DEL REINTEGRO" CLIPPED

                         LET li_tot_rech = li_tot_rech + 1
                     ELSE
                         LET li_tot_acep = li_tot_acep + 1
                     END IF
                 ELSE
                 	   LET li_tot_acep = li_tot_acep + 1
                 END IF

             ELSE
                 LET li_tot_rech = li_tot_rech + 1
             END IF
             
             LET lr_reintegro.estado = ls_estado
             INSERT INTO ret_par_reintegro_des VALUES (lr_reintegro.*)

             CALL r_reporte_carga(lr_temp_reint.nss, lr_temp_reint.consecutivo, gc_error)
             INITIALIZE lr_reintegro.* TO NULL
         END FOREACH
         
         FINISH REPORT r_reporte_carga
         
         ERROR ""
                  
         DISPLAY "TOTAL DE REGISTROS ACEPTADOS            : ", li_tot_acep AT 14,9
         DISPLAY "TOTAL DE REGISTROS RECHAZADOS           : ", li_tot_rech AT 15,9

         DISPLAY "REPORTE GENERADO EN: " AT 17,1 ATTRIBUTE(REVERSE)
         DISPLAY lc_ruta_arch AT 18,1
         
         CALL f_lib_error_msg("PROCESO TERMINADO SATISFACTORIAMENTE ...")
     END IF

     DISPLAY "                                                                        " AT 19,1
     DISPLAY "                                                                        " AT 18,1
     DISPLAY "                                                                        " AT 17,1
     DISPLAY "                                                                        " AT 10,1
     DISPLAY "                                                                        " AT 13,1
     DISPLAY "                                                                        " AT 14,1
     DISPLAY "                                                                        " AT 15,1
     DISPLAY "                                                                        " AT 16,1

END FUNCTION

#------------------------------------------------------------------------------------#
# f_genera_op69 : En base a las solicitudes suspendidas genera el archivo de la OP69 #
#------------------------------------------------------------------------------------#
FUNCTION f_genera_op69()

    DEFINE lr_op69_arch RECORD
           nss                CHAR(11),
           consecutivo        DECIMAL(11,0),
           tipo_prestacion    CHAR(02),
           num_resolucion     CHAR(06),
           monto_en_pesos     DECIMAL(15,2)
    END RECORD

    DEFINE lr_parcial_op69 RECORD LIKE ret_parcial_op69.*

    DEFINE ld_monto_en_pesos        ,
           l_acciones_10            DECIMAL (15,2)

    DEFINE
         l_cuenta             SMALLINT,
         ls_sie_act           SMALLINT

    DEFINE
         lc_ruta_cons_detalle ,
         lc_ruta_subencabezado   CHAR(400)   ,
         lc_ruta_tmp             CHAR(400)   ,
         lc_comando_cat          CHAR(1000)  ,
         lc_comando_rm           CHAR(1000)  ,
         lc_comando_chmod        CHAR(1000)

    DEFINE
         lc_ruta                 ,
         lc_archivo_desinv_90    VARCHAR(255)

    DEFINE
         li_folio             ,
         li_folio_his         ,
         li_cuantos           INTEGER,
         lc_comando           CHAR(100)

    -----------------------------------------------------------

    LET l_cuenta = 0
    LET gd_folio = f_lib_obtiene_ult_folio()

    INITIALIZE lr_op69_arch.*, lr_parcial_op69.* TO NULL
    LET lc_ruta_cons_detalle  = gr_seg_modulo.ruta_envio CLIPPED, "/", "op69_part_1"
    LET lc_ruta_cons_detalle  = lc_ruta_cons_detalle CLIPPED
    LET lc_ruta_subencabezado = gr_seg_modulo.ruta_envio CLIPPED, "/", "op69_part_2"
    LET lc_ruta_subencabezado = lc_ruta_subencabezado CLIPPED

    SELECT  "OK"
    FROM    ret_par_reintegro_des
    WHERE   folio IS NULL
    AND     estado = gr_edo.capturado
    GROUP BY 1

    IF SQLCA.SQLCODE = 100 THEN
         CALL f_lib_error_msg("NO HAY REGISTROS PARA GENERAR LA OP69 ")
         DISPLAY "                                                                        " AT 18,1
    ELSE
         LET li_folio = gd_folio

         ERROR "PROCESANDO INFORMACION"

         #Se genera encabezado y detalle del reporte
         START REPORT r_genera_op69 TO lc_ruta_cons_detalle

               DECLARE cur_arc_69 CURSOR FOR
               SELECT  a.nss, a.consecutivo, "60", a.num_resolucion, SUM(a.monto_en_pesos)
               FROM    ret_par_reintegro_des a
               WHERE   a.folio IS NULL
               AND     a.estado = gr_edo.capturado
               GROUP BY 1,2,3,4

               FOREACH cur_arc_69 INTO lr_op69_arch.*

                  --CALL f_genera_tmp_cuenta(lr_op69_arch.nss)

                   LET l_cuenta = l_cuenta + 1

                   SELECT codigo_siefore
                   INTO ls_sie_act
                   FROM cta_nss_regimen
                   WHERE nss = lr_op69_arch.nss
                   AND grupo_regimen = 1
                   
                   LET ld_monto_en_pesos = lr_op69_arch.monto_en_pesos
                   LET lr_op69_arch.monto_en_pesos = ld_monto_en_pesos * 100

                   #Envia a reporte
                   OUTPUT TO REPORT  r_genera_op69(lr_op69_arch.*)
                   
                   SELECT folio_his_liq
                   INTO   li_folio_his
                   FROM   ret_par_reintegro_des
                   WHERE  nss = lr_op69_arch.nss
                   AND    consecutivo = lr_op69_arch.consecutivo
                   AND    folio = gd_folio
                   AND    estado = gr_edo.capturado

                   # Unicamente para parcialidades pendientes que no se liquidaron se actualizara el estado a suspendido
           		     IF li_folio_his IS NULL OR li_folio_his = 0 THEN
                         UPDATE safre_af:ret_parcial_sub
                         SET estado = gr_edo.enviado_op69
                         WHERE nss = lr_op69_arch.nss
                         AND consecutivo = lr_op69_arch.consecutivo
                         AND estado = gr_edo.suspendido
                   END IF
                   
                   UPDATE ret_par_reintegro_des
                   SET    estado = gr_edo.enviado,
                          fecha_envio = CURRENT,
                          folio = li_folio
                   WHERE nss = lr_op69_arch.nss
                   AND consecutivo = lr_op69_arch.consecutivo
                   AND folio IS NULL
                   AND estado = gr_edo.capturado

                   LET lr_parcial_op69.nss = lr_op69_arch.nss
                   LET lr_parcial_op69.consecutivo = lr_op69_arch.consecutivo
                   LET lr_parcial_op69.folio_op69 = li_folio
                   LET lr_parcial_op69.monto_en_pesos = ld_monto_en_pesos
                   LET lr_parcial_op69.num_resolucion = lr_op69_arch.num_resolucion
                   LET lr_parcial_op69.fecha_carga = CURRENT
                   LET lr_parcial_op69.usuario = gc_usuario

                   INSERT INTO ret_parcial_op69
                   VALUES (lr_parcial_op69.*)

                   INITIALIZE lr_op69_arch.*, lr_parcial_op69.* TO NULL

               END FOREACH

         FINISH REPORT r_genera_op69

         -- Cambia los permisos de archivo
         INITIALIZE lc_comando_chmod TO NULL
         LET lc_comando_chmod = "chmod 777 ", lc_ruta_cons_detalle CLIPPED
         LET lc_comando_chmod = lc_comando_chmod CLIPPED
         RUN lc_comando_chmod

         CALL f_lib_borra_lineas (gr_seg_modulo.ruta_envio, "op69_part_1")

         #Se genera el sumario del reporte
         START REPORT r_sumario_op69 TO lc_ruta_subencabezado

            OUTPUT TO REPORT r_sumario_op69(l_cuenta)

         FINISH REPORT r_sumario_op69

         -- Cambia los permisos de archivo
         INITIALIZE lc_comando_chmod TO NULL
         LET lc_comando_chmod = "chmod 777 ", lc_ruta_subencabezado CLIPPED
         LET lc_comando_chmod = lc_comando_chmod CLIPPED
         RUN lc_comando_chmod

         LET lc_ruta = gr_seg_modulo.ruta_envio CLIPPED, "/PRTFT.DP.A01.OP69.GDG.",HOY USING "yyyymmdd"
         LET lc_ruta = lc_ruta CLIPPED

         LET lc_comando_cat = "cat ", lc_ruta_cons_detalle CLIPPED, " ", lc_ruta_subencabezado CLIPPED," > ", lc_ruta
         LET lc_comando_cat = lc_comando_cat CLIPPED
         RUN lc_comando_cat

         -- Elimina los archivos temporales
             INITIALIZE lc_comando_rm TO NULL
             LET lc_comando_rm = "rm -f ", lc_ruta_cons_detalle CLIPPED
             LET lc_comando_rm = lc_comando_rm CLIPPED
             RUN lc_comando_rm
             INITIALIZE lc_comando_rm TO NULL
             LET lc_comando_rm = "rm -f ", lc_ruta_subencabezado CLIPPED
             LET lc_comando_rm = lc_comando_rm CLIPPED
             RUN lc_comando_rm

         --CALL f_reporte(gd_folio) RETURNING lc_archivo_desinv_90

         ERROR ""
         DISPLAY "FOLIO GENERADO: ", gd_folio USING "<<<<<<<<<"  AT 13,1
         DISPLAY "REGISTROS GENERADOS: ", l_cuenta USING "&<<<<<<<<<" AT 14,1
         DISPLAY "ARCHIVO GENERADO: " AT 15,1 ATTRIBUTE(REVERSE)
         DISPLAY lc_ruta AT 16,1
         --DISPLAY "REPORTE GENERADO REINTEGRO_OP69: " AT 17,1 ATTRIBUTE(REVERSE)
         --DISPLAY lc_archivo_desinv_90 AT 18,1

         #CALL f_genera_reportes (li_folio, 67)

         CALL f_lib_error_msg("PROCESO TERMINADO SATISFACTORIAMENTE ...")
         DISPLAY "                                                                        " AT 18,1
         DISPLAY "                                                                        " AT 17,1
         DISPLAY "                                                                        " AT 10,1
         DISPLAY "                                                                        " AT 13,1
         DISPLAY "                                                                        " AT 14,1
         DISPLAY "                                                                        " AT 15,1
         DISPLAY "                                                                        " AT 16,1

    END IF

END FUNCTION

---------------------------------------------------------------------------------#
# r_genera_op69 : Reporte Encabezado y detalle                                    #
#---------------------------------------------------------------------------------#
REPORT r_genera_op69(p_op69_arch)
    DEFINE p_op69_arch RECORD
           nss                CHAR(11),
           consecutivo        DECIMAL(11,0),
           tipo_prestacion    CHAR(02),
           num_resolucion     CHAR(06),
           monto_en_pesos     DECIMAL(15,2)
    END RECORD

   OUTPUT
            PAGE      LENGTH   2
            LEFT      MARGIN   0
            RIGHT     MARGIN   0
            TOP       MARGIN   0
            BOTTOM    MARGIN   0
   FORMAT

   FIRST PAGE HEADER
              PRINT  COLUMN    1,   "01",
                     COLUMN    3,   "04",
                     COLUMN    5,   "01",
                     COLUMN    7,   gs_codigo_afore USING "<<<<<<",
                     COLUMN   10,   "03",
                     COLUMN   12,   "001",
                     COLUMN   15,   TODAY USING "YYYYMMDD",
                     COLUMN   23,   " ",
                     COLUMN   25,   " ",
                     COLUMN   28,   " ",
                     COLUMN   31,   " ",
                     COLUMN   34,   " ",
                     COLUMN  250,   " "

   ON EVERY ROW
              PRINT  COLUMN    1,   "03",
                     COLUMN    3,   "04",
                     COLUMN    5,   "69",
                     COLUMN    7,   p_op69_arch.nss CLIPPED,
                     COLUMN   18,   gs_codigo_afore USING "<<<<<<",
                     COLUMN   21,   p_op69_arch.tipo_prestacion CLIPPED,
                     COLUMN   23,   p_op69_arch.monto_en_pesos USING "&&&&&&&&&&",
                     COLUMN   33,   p_op69_arch.num_resolucion USING "&&&&&&",
                     COLUMN   39,   " ",
                     COLUMN  250,   " "


END REPORT
#---------------------------------------------------------------------------------#
# r_genera_subencabezado : Reporte subencabezado                                  #
#---------------------------------------------------------------------------------#
REPORT r_sumario_op69 (p_cuenta)

    DEFINE
          p_cuenta   SMALLINT

    OUTPUT
        PAGE LENGTH   1
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0

    FORMAT
        ON EVERY ROW
            PRINT  COLUMN    1,   "09",
                   COLUMN    3,   "04",
                   COLUMN    5,   "01",
                   COLUMN    7,   gs_codigo_afore USING "&&&",
                   COLUMN   10,   "03",
                   COLUMN   12,   "001",
                   COLUMN   15,   TODAY USING "YYYYMMDD",
                   COLUMN   23,   p_cuenta USING "&&&&&&",
                   COLUMN   29,   " ",
                   COLUMN  250,   " "


END REPORT

#---------------------------------------------------------------------------#
# f_carga_op70 : Realiza la carga de la OP70 para el reintegro de recursos  #
#                no cobrados de retiros parciales por desempleo.            #
#---------------------------------------------------------------------------#
FUNCTION f_carga_op70()

    CALL f_valida_info()   #-- Realiza la preliquidacion de los montos
    CALL f_integra_info()   #-- Vacia la informacion hacia las tablas fisicas

END FUNCTION

#---------------------------------------------------------------------------#
# f_valida_info: Carga y vacia la informacion del archivo de la operacion 70#
#               a las tablas temporales donde se realizara su validacion    #
#---------------------------------------------------------------------------#
FUNCTION f_valida_info()

    DEFINE
        lc_reg_carga            CHAR(900)

    DEFINE
        li_cont                 INTEGER

    -- -----------------------------------------------------------------------------

    DISPLAY " CARGANDO ARCHIVO ... " AT 22,1 ATTRIBUTE(REVERSE)
    SLEEP 1

    LET li_cont = 0

    DECLARE cur_ar CURSOR FOR
    SELECT  *
    FROM    tmp_arch_parc
    WHERE   n_registros[1,2] = "03"

    CALL f_ins_recepcion(gr_dat.nom_archivo)

    FOREACH cur_ar INTO lc_reg_carga

        LET li_cont     = li_cont + 1
        CALL f_carga_det(lc_reg_carga)

    END FOREACH

    UPDATE tmp_recepcion
    SET    tot_registros = li_cont
    WHERE  folio         = 1

END FUNCTION

#----------------------------------------------------------------------------#
# f_integra_info : Guarda la informacion en las tablas fisicas una vez que ya#
#               fue validada                                                 #
#----------------------------------------------------------------------------#
FUNCTION f_integra_info()

    DEFINE lr_tmp_op70      RECORD LIKE ret_parcial_op70.*,
           lr_par_inversion RECORD LIKE ret_par_reinversion.*,
           lr_reintegro     RECORD LIKE ret_par_reintegro_des.*

    DEFINE
        li_folio                ,
        li_num_marca            ,
        li_tot_registros        ,
        li_tot_acep             ,
        li_tot_rech             ,
        li_tot_detalle          INTEGER

    DEFINE
        ls_estado               SMALLINT

    DEFINE
        ld_consecutivo      DECIMAL(11,0),
        lc_archivo          CHAR(100),
        ls_clave            SMALLINT
        
    ---DESMARCA CUENTA
    LET gc_char = " "
    LET gc_char = "EXECUTE PROCEDURE desmarca_cuenta ( ?,?,?,?,?,? )"
    PREPARE eje_desmarca_op69 FROM gc_char

    INITIALIZE lr_tmp_op70.*, lr_par_inversion.*, lr_reintegro.* TO NULL

    DISPLAY "                                                                           " AT 9,1
    DISPLAY "                                                                           " AT 10,1
    DISPLAY "                                                                           " AT 11,1
    DISPLAY "                                             " AT 18,1
    DISPLAY " ACTUALIZANDO TABLAS ... " AT 18,1 ATTRIBUTE(REVERSE)
    SLEEP 1

    LET li_tot_registros    = 0     LET li_tot_detalle      = 0
    LET li_tot_acep         = 0     LET li_tot_rech         = 0

    SELECT COUNT(*) INTO   li_tot_registros
    FROM   tmp_arch_parc

    DISPLAY "TOTAL DE REGISTROS PROCESADOS            : ", li_tot_registros AT 10,9
    DISPLAY "TOTAL DE REGISTROS DE DETALLE            : ", li_tot_detalle AT 11,9

    LET li_tot_detalle = 0
    
    SELECT MAX(folio)
    INTO   gd_folio
    FROM   ret_par_reintegro_des
    WHERE  nss in (SELECT unique nss FROM tmp_parcial_op70
    WHERE  folio_op70 = 1)
    AND    estado = gr_edo.enviado
    
    DISPLAY "FOLIO : ",gd_folio USING "<<<<<<<<<<<" AT 15,1

    DECLARE cur_ok_70 CURSOR FOR
    SELECT *
    FROM   tmp_parcial_op70
    WHERE  folio_op70 = 1

    FOREACH cur_ok_70 INTO lr_tmp_op70.*

       LET li_tot_detalle = li_tot_detalle + 1
       DISPLAY "TOTAL DE REGISTROS DE DETALLE            : ",li_tot_detalle AT 11,9

       DECLARE cur_par_rein_des CURSOR FOR   
       SELECT *
       FROM   ret_par_reintegro_des
       WHERE  nss    = lr_tmp_op70.nss
       AND    estado = gr_edo.enviado
       
       FOREACH cur_par_rein_des INTO lr_reintegro.*

           LET lr_tmp_op70.consecutivo = lr_reintegro.consecutivo
           
            IF lr_tmp_op70.consecutivo IS NULL THEN
               CALL f_lib_error_msg("NSS SIN ENVIO: "||lr_tmp_op70.nss)
               CONTINUE FOREACH
            END IF
           
           LET lr_tmp_op70.folio_op70 = lr_reintegro.folio
           
           IF lr_tmp_op70.resultado_op = 1 OR
              (lr_tmp_op70.resultado_op = 2 AND
               (lr_tmp_op70.diagnostico = "036" OR
               lr_tmp_op70.diagnostico = "049")) THEN
               	
               	IF lr_reintegro.tipo_pago >= 4 THEN
               		
               		# Unicamente para parcialidades pendientes que no se liquidaron se actualizara el estado a suspendido
               		IF lr_reintegro.folio_his_liq IS NULL OR lr_reintegro.folio_his_liq = 0 THEN
                       UPDATE ret_parcialidad_des
                       SET    estado = gr_edo.suspendido
                       WHERE  nss = lr_tmp_op70.nss
                       AND    consecutivo = lr_tmp_op70.consecutivo
                       AND    estado = gr_edo.capturado
                       
                       UPDATE ret_parcial_sub
                       SET estado = gr_edo.reinvertido
                       WHERE  nss = lr_tmp_op70.nss
                       AND    consecutivo = lr_tmp_op70.consecutivo
                       AND    estado = gr_edo.enviado_op69
                       
                       SELECT "OK" FROM cta_act_marca
                       WHERE nss = lr_tmp_op70.nss
                       AND correlativo = lr_tmp_op70.consecutivo
                       AND marca_cod   = gs_marca_proceso
                       GROUP BY 1
                       
                        IF SQLCA.SQLCODE = 0 THEN
                           EXECUTE eje_desmarca_op69 USING lr_tmp_op70.nss,           -- nss
                                                      gs_marca_proceso,          -- marca entrante
                                                      lr_tmp_op70.consecutivo,   -- consecutivo
                                                      gs_zero          ,         -- estado_marca
                                                      gs_marca_proceso ,         -- marca_causa
                                                      gc_usuario                 -- usuario                   
                        END IF
                        
                        SELECT "OK"
                        FROM ret_par_reinversion
                        WHERE nss = lr_tmp_op70.nss
                        AND consecutivo = lr_tmp_op70.consecutivo
                        GROUP BY 1
                        
                        IF SQLCA.SQLCODE = 100 THEN
                             LET lr_par_inversion.nss = lr_tmp_op70.nss
                             LET lr_par_inversion.consecutivo = lr_tmp_op70.consecutivo
                             LET lr_par_inversion.tipo = gs_tipo_reinv
                             LET lr_par_inversion.folio = li_folio
                             LET lr_par_inversion.estado = gr_edo.reinvertido
                             LET lr_par_inversion.usuario = gc_usuario
                             LET lr_par_inversion.fecha = CURRENT
                             
                             INSERT INTO ret_par_reinversion
                             VALUES (lr_par_inversion.*)
                        END IF
                  END IF
                END IF
                
                # Unicamente para parcialidades pendientes que no se liquidaron se actualizara el estado a suspendido
                IF lr_reintegro.folio_his_liq IS NULL OR lr_reintegro.folio_his_liq = 0 THEN
                	  UPDATE ret_par_reintegro_des
                    SET    estado = gr_edo.suspendido,
                           fecha_respuesta = CURRENT
                    WHERE  nss = lr_tmp_op70.nss
                    AND    consecutivo = lr_tmp_op70.consecutivo
                    AND    folio = lr_reintegro.folio
                    AND    estado = gr_edo.enviado
                ELSE
                	  UPDATE ret_par_reintegro_des
                    SET    estado = gr_edo.recibido,
                           fecha_respuesta = CURRENT
                    WHERE  nss = lr_tmp_op70.nss
                    AND    consecutivo = lr_tmp_op70.consecutivo
                    AND    folio = lr_reintegro.folio
                    AND    estado = gr_edo.enviado
                END IF
                
           ELSE
           	    # Unicamente para parcialidades pendientes que no se liquidaron se actualizara el estado a suspendido
                IF lr_reintegro.folio_his_liq IS NULL OR lr_reintegro.folio_his_liq = 0 THEN
                     UPDATE ret_parcial_sub
                     SET   estado       = gr_edo.suspendido,
                           ind_suspende = 2  --CPL-2382
                     WHERE nss          = lr_tmp_op70.nss
                     AND   consecutivo  = lr_tmp_op70.consecutivo
                     AND   estado       = gr_edo.enviado_op69
                END IF
                
                UPDATE ret_par_reintegro_des
                SET    estado = gr_edo.capturado,
                       folio = NULL,
                       fecha_envio = NULL
                WHERE  nss = lr_tmp_op70.nss
                AND    consecutivo = lr_tmp_op70.consecutivo
                AND    folio = lr_reintegro.folio
                AND    estado = gr_edo.enviado
           
           END IF
           
           INITIALIZE lr_reintegro.* TO NULL
       END FOREACH

       INSERT INTO ret_parcial_op70
       VALUES (lr_tmp_op70.*)

       INITIALIZE lr_tmp_op70.* TO NULL

    END FOREACH
    
    --Se actualiza el registro del archivo en la tabla temporal
    UPDATE tmp_recepcion
    SET    folio         = gd_folio
    WHERE  folio         = 1
    
    INSERT INTO ret_recepcion
    SELECT * FROM tmp_recepcion

    SELECT 'x'
    FROM   ret_parcial_op70
    WHERE  resultado_op ="02"
    AND    diagnostico NOT IN ("036","049")
    AND    folio_op70   = gd_folio
    GROUP BY 1

    IF SQLCA.SQLCODE = 0 THEN
       CALL f_reporte(gd_folio) RETURNING lc_archivo
       DISPLAY "ARCHIVO GENERADO REINTEGRO_OP70: " AT 17,1 ATTRIBUTE(REVERSE)
       DISPLAY lc_archivo AT 18,1
    END IF

    DISPLAY "                                                                        " AT 18,1
    CALL f_lib_error_msg("PROCESO FINALIZADO")

END FUNCTION

#---------------------------------------------------------------------------#
# f_ins_recepcion : Carga en la tabla temporal los valores de la recepcion  #
#                   del archivo de notificacion                             #
#---------------------------------------------------------------------------#
FUNCTION f_ins_recepcion(pr_encab)

    DEFINE pr_encab RECORD
        nom_archivo     LIKE ret_recepcion.nom_archivo
    END RECORD

    DEFINE lr_recepcion RECORD LIKE ret_recepcion.*

    -- -----------------------------------------------------------------------------

    INITIALIZE lr_recepcion.* TO NULL

    IF gs_accion = 1 THEN
       LET lr_recepcion.tipo_operacion     = 69
    ELSE
       LET lr_recepcion.tipo_operacion     = 70
    END IF

    LET lr_recepcion.folio              = 1
    LET lr_recepcion.fecha_recepcion    = HOY
    LET lr_recepcion.hora_recepcion     = CURRENT HOUR TO SECOND
    LET lr_recepcion.nom_archivo        = pr_encab.nom_archivo
    LET lr_recepcion.tot_registros      = 0
    LET lr_recepcion.usuario            = gc_usuario
    LET lr_recepcion.estado             = gr_edo.recibido
    LET lr_recepcion.mot_rechazo_01     = 0
    LET lr_recepcion.mot_rechazo_02     = 0
    LET lr_recepcion.mot_rechazo_03     = 0

    INSERT INTO tmp_recepcion
    VALUES(lr_recepcion.*)

END FUNCTION

#---------------------------------------------------------------------------#
# f_carga_det : Carga en la tabla temporal los valores del detalle del      #
#               archivo de la operacion 70                                  #
#---------------------------------------------------------------------------#
FUNCTION f_carga_det(pc_registro)

    DEFINE
        pc_registro         CHAR(900)

    DEFINE lr_parcial_op70_tmp  RECORD LIKE ret_parcial_op70.*

    DEFINE
        ls_edo_recep        ,
        ls_aceptado         SMALLINT

    DEFINE
        lc_fechas           CHAR(10) ,
        lc_mto_1            CHAR(12) ,
        lc_mto_2            CHAR(09) ,
        lc_mto_3            CHAR(11)

    -- -----------------------------------------------------------------------------

    INITIALIZE lr_parcial_op70_tmp.* TO NULL
    
    LET lr_parcial_op70_tmp.folio_op70          = 1 -- Folio temporal
    LET lr_parcial_op70_tmp.nss                 = pc_registro[007,017]

    -- El formato para la fecha de retiro es "AAAAMMDD"
    LET lc_fechas                               = pc_registro[043,044],"/",
                                                  pc_registro[045,046],"/",
                                                  pc_registro[039,042]
    LET lr_parcial_op70_tmp.fecha_retiro        = lc_fechas
    LET lr_parcial_op70_tmp.dias_descontados    = pc_registro[047,056]

    LET lc_mto_1 = pc_registro[057,064],".",pc_registro[065,066]
    LET lr_parcial_op70_tmp.monto_reintegro     = lc_mto_1

    LET lc_mto_2 = pc_registro[067,071],".",pc_registro[072,073]
    LET lr_parcial_op70_tmp.valor_dia_reintegro = lc_mto_2
    LET lr_parcial_op70_tmp.num_max_semanas     = pc_registro[074,103]

    -- El formato para la fecha de reintegro es "AAAAMMDD"
    LET lc_fechas                               = pc_registro[108,109],"/",
                                                  pc_registro[110,111],"/",
                                                  pc_registro[104,107]
    LET lr_parcial_op70_tmp.fecha_reintegro     = lc_fechas

    LET lc_mto_3 = pc_registro[112,119],".",pc_registro[120,121]
    LET lr_parcial_op70_tmp.monto_solicitado    = lc_mto_3
    LET lr_parcial_op70_tmp.num_sem_procesar    = pc_registro[122,151]
    LET lr_parcial_op70_tmp.resultado_op        = pc_registro[152,153]
    LET lr_parcial_op70_tmp.diagnostico         = pc_registro[154,156]
    LET lr_parcial_op70_tmp.fecha_carga         = CURRENT
    LET lr_parcial_op70_tmp.usuario             = gc_usuario

    INSERT INTO tmp_parcial_op70
    VALUES(lr_parcial_op70_tmp.*)

END FUNCTION

#---------------------------------------------------------------------------#
# f_reporte : Generación Reporte de OP69 y OP70                             #
#---------------------------------------------------------------------------#

FUNCTION f_reporte (li_folio)

   DEFINE
      li_folio             INTEGER,
      lc_archivo           CHAR(300),
      lc_archivo_det           CHAR(300),
      lc_comando           CHAR(3000)

   IF   gs_accion = 2 THEN
      LET lc_archivo = gr_seg_modulo.ruta_listados CLIPPED, "/", gc_usuario CLIPPED,
                                  ".RPT_REINTEGRO_OP69.", HOY USING "YYYYMMDD"
      LET lc_archivo_det = gr_seg_modulo.ruta_listados CLIPPED, "/", gc_usuario CLIPPED,
                                  ".RPT_REINTEGRO_OP69_det.", HOY USING "YYYYMMDD"

      LET lc_comando = " SELECT   b.nss            ,                  \n",
                       "          TRIM(d.nombre_bdnsar) ||' '||       \n",
                       "          TRIM(d.paterno_bdnsar) ||' '||      \n",
                       "          TRIM(d.materno_bdnsar) nombre,            \n",
                       "          a.tipo_pago      ,                  \n",
                       "          c.ind_suspende   ,                  \n",
                       "          (SELECT e.codigo_siefore            \n",
                       "          FROM   cta_nss_regimen   e          \n",
                       "          WHERE  e.nss= b.nss                 \n",
                       "          AND    e.grupo_regimen = 1) siefore,\n",
                       "          SUM(b.monto_en_pesos) pesos,        \n",
                       "          SUM(b.monto_en_acciones) acciones   \n",
                       " FROM     ret_parcial     a,                  \n",
                       "          dis_cuenta      b,                  \n",
                       "          ret_parcial_sub c,                  \n",
                       "          OUTER ret_parcial_resol d           \n",
                       " WHERE    b.folio = ", gd_folio , "            \n",
                       " AND      a.nss = d.nss                       \n",
                       " AND      a.num_resolucion = d.num_resolucion \n",
                       " AND      a.nss = b.nss                       \n",
                       " AND      a.consecutivo = b.consecutivo_lote  \n",
                       " AND      a.nss = c.nss                       \n",
                       " AND      a.consecutivo = c.consecutivo       \n",
                       " AND      b.tipo_movimiento = 929             \n",
                       " GROUP BY 1,2,3,4,5                           \n",
                       " INTO TEMP tmp_rpt_deis"

      LET lc_comando = lc_comando CLIPPED

      PREPARE exe_rpt FROM lc_comando
      EXECUTE exe_rpt

      START REPORT rpt_cza TO lc_archivo
      OUTPUT TO REPORT rpt_cza()
      FINISH REPORT rpt_cza

      UNLOAD TO lc_archivo_det
      SELECT *
      FROM   tmp_rpt_deis

   ELSE
      LET lc_archivo = gr_seg_modulo.ruta_listados CLIPPED, "/", gc_usuario CLIPPED,
                                  ".RPT_REINTEGRO_OP70.", HOY USING "YYYYMMDD"
      LET lc_archivo_det = gr_seg_modulo.ruta_listados CLIPPED, "/", gc_usuario CLIPPED,
                                  ".RPT_REINTEGRO_OP70_det.", HOY USING "YYYYMMDD"

      LET lc_comando =" SELECT   b.nss            ,                   \n",
                      "          TRIM(d.nombre_bdnsar) ||' '||        \n",
                      "          TRIM(d.paterno_bdnsar) ||' '||       \n",
                      "          TRIM(d.materno_bdnsar) nombre,       \n",
                      "          b.diagnostico          ,             \n",
                      "          b.resultado_op         ,             \n",
                      "          b.fecha_carga          ,             \n",
                      "          b.monto_solicitado                   \n",
                      " FROM     ret_parcial     a,                   \n",
                      "          ret_parcial_op70 b,                  \n",
                      "          OUTER ret_parcial_resol d            \n",
                      " WHERE    b.folio_op70 =",li_folio ,      "    \n",
                      " AND      a.nss = d.nss                        \n",
                      " AND      a.num_resolucion = d.num_resolucion  \n",
                      " AND      a.nss = b.nss                        \n",
                      " AND      a.consecutivo = b.consecutivo        \n",
                      " AND      b.resultado_op ='02'                 \n",
                      " AND      b.diagnostico NOT IN ('036','049')   \n",
                      " INTO TEMP tmp_rpt_deis"
      --DISPLAY lc_comando CLIPPED
      PREPARE exe_rpt2 FROM lc_comando
      EXECUTE exe_rpt2

      START REPORT rpt_cza TO lc_archivo
      OUTPUT TO REPORT rpt_cza()
      FINISH REPORT rpt_cza

      UNLOAD TO lc_archivo_det
      SELECT *
      FROM   tmp_rpt_deis
   END IF

   LET lc_comando = "cat ", lc_archivo_det     CLIPPED, " >> ",
                            lc_archivo     CLIPPED
   RUN lc_comando
   #se borra el archivo generado del detalle
   LET lc_comando = "rm ", lc_archivo_det CLIPPED
   RUN lc_comando

   RETURN lc_archivo
END FUNCTION

REPORT rpt_cza()
   OUTPUT
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0
      PAGE LENGTH   1

   FORMAT
   ON EVERY ROW
   IF gs_accion = 2 THEN
   PRINT
      'NSS','|'
     ,'Nombre','|'
     ,'Tipo Pago','|'
     ,'Tipo Suspensión','|'
     ,'Siefore','|'
     ,'Importe en pesos','|'
     ,'Monto en Acciones','|'

   ELSE
      PRINT
       'NSS','|'
      ,'Nombre','|'
      ,'Diagnóstico de Rechazo','|'
      ,'Resultado de la Operación','|'
      ,'Fecha Carga OP70 ','|'
      ,'Importe en pesos ','|'


   END IF

END REPORT


#---------------------------------------------------------------------------------#
# r_reporte_carga : Genera el reporte de carga inicial de los registros           #
#---------------------------------------------------------------------------------#
REPORT r_reporte_carga(pr_reporte)

    DEFINE pr_reporte    RECORD
         nss             CHAR(11),
         consecutivo     DECIMAL(11,0),
         error           CHAR(500)
    END RECORD

   OUTPUT
            PAGE      LENGTH   2
            LEFT      MARGIN   0
            RIGHT     MARGIN   0
            TOP       MARGIN   0
            BOTTOM    MARGIN   0
   FORMAT

   FIRST PAGE HEADER
       PRINT COLUMN 1,
              "NSS",                  "|",
              "CONSECUTIVO",          "|",
              "DIAGNOSTICO",          "|"

          ON EVERY ROW

            PRINT COLUMN 1,
                          pr_reporte.nss               CLIPPED           ,  "|",
                          pr_reporte.consecutivo       USING "<<<<<<<<"  ,  "|",
                          pr_reporte.error             CLIPPED           ,  "|"

END REPORT