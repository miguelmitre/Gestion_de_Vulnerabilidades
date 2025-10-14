#*******************************************************************#
#Proyecto          => Sistema de Afores.( MEXICO )                  #
#Propietario       => E.F.P.                                        #
#Programa          => INTB0307                                      #
#Descripcion       => INTERFASE DE RESPUESTA A SCOTIABANK DE RETIROS#
#                  => OBLIGADOS   (LIQUIDADO)                       #
#Sistema           => INT .                                         #
#Fecha Elaboracion => 09 de Marzo del 2006   .                      #
#Elaborado por     => Laura Eugenia Cortes Guzman                   #
#Fecha Ult. Modif. => 09 de Marzo del 2006   .                      #
#Modificado por    => Laura Eugenia Cortes Guzman                   #
#*******************************************************************#
DATABASE safre_af
GLOBALS
      DEFINE v_rep                 CHAR(500),
             selec_tab             CHAR(600),
             fecha_dia             CHAR(08),
             f_pago                CHAR(08),
             enter                 CHAR(01),
             nss                   CHAR(20),
             hoy                   CHAR(6),
             ban                   SMALLINT,
             fecha_conversion      DATE,
             fecha_ini, fecha_fin  DATE,
             numero_reg            INTEGER,
             hoy_hoy               DATE,
             hora                  CHAR(08),
             ho_ra                 CHAR(06),

## NOTA : para hacer pruebas en int_scot cambiarle a int_scot

             p_tabafore            RECORD LIKE tab_afore_local.*,

             importe               DECIMAL(17,2),
             importe_abono         DECIMAL(17,2),
             importe_orden         DECIMAL(17,2),

             v_ruta, v_arch,
             v_det, v_cza, v_sum   CHAR(70),
             v_det820,v_det830,v_det840,v_det850, v_det860,v_det870,v_det875,
             v_det880,v_det490    CHAR(70),
             ejecuta               CHAR(500)


END GLOBALS
#
# ---------------------------------------------------------------------
# main 
# ---------------------------------------------------------------------
#
MAIN

   OPTIONS INPUT WRAP,
           PROMPT LINE LAST,
           ACCEPT KEY CONTROL-I

   DEFER INTERRUPT       
##     WHENEVER ERROR CONTINUE
    CALL STARTLOG("INTB0307.log")

    INITIALIZE v_det, v_ruta, ejecuta, fecha_ini, fecha_fin TO NULL
    INITIALIZE p_tabafore.*, fecha_dia, v_cza, v_sum TO NULL
    INITIALIZE v_det820,v_det830,v_det840,v_det850, v_det860,v_det870,v_det875,
               v_det880,v_det490    TO NULL

    LET hoy_hoy = TODAY
    LET fecha_dia = TODAY USING "YYYYMMDD"

    SELECT f.* INTO p_tabafore.*
        FROM tab_afore_local f

    LET hora = TIME
    LET ho_ra = hora[1,2],hora[4,5],hora[7,8]
    DELETE FROM safre_tmp:int_scot_obli

    SELECT c.ruta_envio INTO v_ruta 
        FROM seg_modulo c
        WHERE c.modulo_cod = "int"

    LET hoy =TODAY USING "yymmdd"

    SELECT v.nom_arch, v.arch_det
        INTO  v_arch, v_det
        FROM  tab_layout v
        WHERE v.layout_cod = 37

{
    LET v_arch = "Ro",hoy CLIPPED,".txt"
    LET v_cza = "scot_o_cza.txt" CLIPPED

    LET v_det  = "scot_o_det.txt" CLIPPED

    LET v_sum = "scot_o_sum.txt" CLIPPED
    LET v_arch = v_ruta CLIPPED,"/Ro",hoy CLIPPED,ho_ra CLIPPED,".txt"
}
    LET v_arch = v_ruta CLIPPED,"/Ro",hoy CLIPPED,".txt"
    LET v_cza = v_ruta CLIPPED,"/scot_o_cza.txt" CLIPPED

    LET v_det  = v_ruta CLIPPED,"/scot_o_det.txt" CLIPPED

    LET v_sum = v_ruta CLIPPED,"/scot_o_sum.txt" CLIPPED

    LET v_det820 = v_ruta CLIPPED,"/det820.txt" CLIPPED
    LET v_det830 = v_ruta CLIPPED,"/det830.txt" CLIPPED
    LET v_det840 = v_ruta CLIPPED,"/det840.txt" CLIPPED
    LET v_det850 = v_ruta CLIPPED,"/det850.txt" CLIPPED
    LET v_det860 = v_ruta CLIPPED,"/det860.txt" CLIPPED
    LET v_det870 = v_ruta CLIPPED,"/det870.txt" CLIPPED
    LET v_det875 = v_ruta CLIPPED,"/det875.txt" CLIPPED
    LET v_det880 = v_ruta CLIPPED,"/det880.txt" CLIPPED
    LET v_det490 = v_ruta CLIPPED,"/det490.txt" CLIPPED

    OPEN WINDOW INTB0307 AT 2,2 WITH FORM "INTB03071" ATTRIBUTE(BORDER)
         DISPLAY " < ESC > Procesar                                          <",
		 " Ctrl-C > Salir    " AT 1,1 ATTRIBUTE(REVERSE)
                                                                                
         DISPLAY " INTB0307              RESPUESTA DE RETIROS A CAJAS     ",
		 "                          "
                 AT 3,1 ATTRIBUTE(REVERSE)
         DISPLAY hoy_hoy  USING "DD-MM-YYYY " AT 3,66 ATTRIBUTE(REVERSE)

         INPUT fecha_ini, fecha_fin FROM   fecha_ini, fecha_fin

                   AFTER FIELD fecha_ini
                      IF fecha_ini IS NULL OR
                          fecha_ini = " "   THEN
                          ERROR " Digite correctamente la Fecha Inicial "
                          NEXT FIELD fecha_ini
                      END IF

                   AFTER FIELD fecha_fin
                      IF fecha_fin IS NULL OR
                          fecha_fin = " "   THEN
                          ERROR " Digite correctamente la Fecha Final "
                          NEXT FIELD fecha_fin
                      END IF

                      IF fecha_ini > fecha_fin THEN
                         ERROR "La Fecha Inicial no puede ser mayor",
                               " a la Fecha Final"
                         NEXT FIELD fecha_ini
                      END IF

                      IF fecha_fin > TODAY THEN
                         ERROR "La Fecha Final no puede ser mayor",
                               " a la Fecha de HOY"
                         NEXT FIELD fecha_fin
                      END IF

                   ON KEY(ESC)
                      IF fecha_ini IS NULL OR
                          fecha_ini = " "   THEN
                          ERROR " Digite correctamente la Fecha Inicial "
                          NEXT FIELD fecha_ini
                      END IF

                      IF fecha_fin IS NULL OR
                          fecha_fin = " "   THEN
                          ERROR " Digite correctamente la Fecha Final "
                          NEXT FIELD fecha_fin
                      END IF

                      IF fecha_ini > fecha_fin THEN
                         ERROR "La Fecha Inicial no puede ser mayor",
                               " a la Fecha Final"
                         NEXT FIELD fecha_ini
                      END IF

                      IF fecha_fin > TODAY THEN
                         ERROR "La Fecha Final no puede ser mayor",
                               " a la Fecha de HOY"
                         NEXT FIELD fecha_fin
                      END IF

                      LET ban = 0
                      EXIT INPUT

                   ON KEY(CONTROL-C,INTERRUPT)
                         LET ban = 1
                         EXIT INPUT
         END INPUT 

         IF ban = 0 THEN
             DISPLAY "GENERANDO INFORMACION.... AGUARDE UN MOMENTO..." at 19,1
                     ATTRIBUTE(REVERSE)

             CALL genera()

             LET ejecuta = "cat ",v_cza," > ",v_arch
             RUN ejecuta

               LET ejecuta = "cat ",v_det820," >> ",v_arch
               RUN ejecuta
               LET ejecuta = "cat ",v_det830," >> ",v_arch
               RUN ejecuta
               LET ejecuta = "cat ",v_det840," >> ",v_arch
               RUN ejecuta
               LET ejecuta = "cat ",v_det850," >> ",v_arch
               RUN ejecuta
               LET ejecuta = "cat ",v_det860," >> ",v_arch
               RUN ejecuta
               LET ejecuta = "cat ",v_det870," >> ",v_arch
               RUN ejecuta
               LET ejecuta = "cat ",v_det875," >> ",v_arch
               RUN ejecuta
               LET ejecuta = "cat ",v_det880," >> ",v_arch
               RUN ejecuta
               LET ejecuta = "cat ",v_det490," >> ",v_arch
               RUN ejecuta

--             LET ejecuta = "cat ",v_det," >> ",v_arch
--             RUN ejecuta

             LET ejecuta = "cat ",v_sum," >> ",v_arch
             RUN ejecuta


             LET ejecuta = "echo Ro",hoy CLIPPED,".txt > ",v_ruta CLIPPED,
			   "/rescate.Ro"
             RUN ejecuta
                                                     
             LET ejecuta = "echo Ro",hoy CLIPPED,".R13.txt > ",v_ruta CLIPPED,
			   "/rescate.R13"
             RUN ejecuta


             LET ejecuta = "rm ",v_cza," ",v_det," ",v_sum
             RUN ejecuta
             LET ejecuta = "rm ", v_det820," ", v_det830," ",v_det840
             RUN ejecuta
             LET ejecuta = "rm ", v_det850," ", v_det860," ",v_det870
             RUN ejecuta
             LET ejecuta = "rm ", v_det875," ", v_det880," ",v_det490
             RUN ejecuta

             DISPLAY "" AT 19,1
             DISPLAY "ARCHIVO GENERADO EN : ",v_arch AT 19,1
             PROMPT "PROCESO FINALIZADO...< ENTER > PARA CONTINUAR " 
                    FOR enter
         ELSE
            PROMPT "PROCESO CANCELADO...< ENTER > PARA CONTINUAR " FOR enter
         END IF
    CLOSE WINDOW INTB0307 
END MAIN

FUNCTION genera()

   DEFINE ret_obli       RECORD
              tipo_reg             CHAR(02),
              nss                  CHAR(11),
              paterno              CHAR(40),
              materno              CHAR(40),
              nombres              CHAR(40),
              forma_pago           SMALLINT,
              cable                CHAR(18),
              fecha_captura        DATE,
              monto_neto           DECIMAL(15,2),
              mto_antes_impto      DECIMAL(15,2),
              impto_retenido       DECIMAL(11,2),
              folio_soli           INTEGER,
              tienda               INTEGER,
              tipo_retiro          CHAR(03),
              consecutivo          INTEGER,
              curp                 CHAR(18),
              bla15                CHAR(15)
          END RECORD,

          ret_nega    RECORD
              consecutivo          INTEGER,
              tipo_movimiento      SMALLINT,
              nss                  CHAR(11),
              paterno              CHAR(40),
              materno              CHAR(40),
              nombres              CHAR(40),
              tipo_pago            SMALLINT,
              fecha_captura        DATE,
              tienda               INTEGER,
              folio_soli           INTEGER
          END RECORD,

          tot_reg                  INTEGER,
          tmonto_neto              DECIMAL(17,2),
          tmto_antes_impto         DECIMAL(17,2),
          timpto_retenido          DECIMAL(17,2),
          tpago_efectivo           DECIMAL(17,2),
          tpago_deposito           DECIMAL(17,2),
          bla216                   CHAR(216),
          bla163                   CHAR(163),
          bla15                    CHAR(15),
          bla18                    CHAR(18),
          sub_1, sub_11            DECIMAL(15,2),
          sub_2, sub_22            DECIMAL(15,2),
          sub_3, sub_33            DECIMAL(15,2),
          sub_4, sub_44            DECIMAL(15,2),
          sub_5, sub_55            DECIMAL(15,2),
          sub_6, sub_66            DECIMAL(15,2),
          v_rep                    CHAR(2200)

         INITIALIZE ret_obli.*, ret_nega.* TO NULL

        LET bla18  = "                  "
        LET bla15  = "               "

        LET bla163 = "                                                  ",
                     "                                                  ",
                     "                                                  ",
                     "             "

        LET bla216 = "                                                  ",
                     "                                                  ",
                     "                                                  ",
                     "                                                  ",
                     "                "

        LET tot_reg           = 0
        LET tmonto_neto       = 0
        LET tmto_antes_impto  = 0
        LET timpto_retenido   = 0
        LET tpago_efectivo    = 0
        LET tpago_deposito    = 0

         START REPORT r_report TO v_det820
##Detalle (D-820)

         INITIALIZE ret_obli.*,ret_nega.* TO NULL
         LET sub_1 = 0    LET sub_2 = 0   LET sub_3 = 0   LET sub_4 = 0
         LET sub_5 = 0    LET sub_6 = 0
         LET sub_11= 0    LET sub_22= 0   LET sub_33= 0   LET sub_44= 0
         LET sub_55= 0    LET sub_66= 0

         DECLARE apt_D CURSOR FOR

              SELECT UNIQUE a.consecutivo_lote, a.tipo_movimiento,
                     b.nss, b.paterno, b.materno, b.nombres, b.tipo_pago,
                     b.fecha_captura, b.tienda_cod,c.folio_solicitud 
                     FROM  dis_cuenta a, ret_beneficiario b, ret_solicitud_tx c
                     WHERE a.nss = b.nss
                       AND a.subcuenta IN(1,2,4,5,6,7,8,9)
                       AND a.tipo_movimiento = 820
                       AND a.consecutivo_lote = b.consecutivo
                       AND a.nss  = c.nss
                       AND a.consecutivo_lote = c.consecutivo
                       AND a.fecha_conversion BETWEEN fecha_ini AND fecha_fin
               ORDER BY 1,2,3
         FOREACH apt_D INTO ret_nega.*

              SELECT ROUND(SUM(m.monto_en_pesos),2)
	      INTO   sub_1  FROM dis_cuenta m
	      WHERE  m.nss             = ret_nega.nss 
	        AND  m.subcuenta       IN(1)
	        AND  m.tipo_movimiento = ret_nega.tipo_movimiento 
	        AND  m.consecutivo_lote= ret_nega.consecutivo
                AND m.fecha_conversion BETWEEN fecha_ini AND fecha_fin
                AND  m.tipo_movimiento <> 10
              IF sub_1 IS NULL OR sub_1 = 0 THEN
                 LET sub_1 = 0
              END IF

              SELECT ROUND(SUM(m.monto_en_pesos),2)
	      INTO   sub_11 FROM dis_cuenta m
	      WHERE  m.nss             = ret_nega.nss 
	        AND  m.subcuenta       IN(1)
	        AND  m.tipo_movimiento = 10
	        AND  m.consecutivo_lote= ret_nega.consecutivo
                AND m.fecha_conversion BETWEEN fecha_ini AND fecha_fin
              IF sub_11 IS NULL OR sub_11 = 0 THEN
                 LET sub_11 = 0
              END IF

              SELECT ROUND(SUM(m.monto_en_pesos),2)
	      INTO   sub_2  FROM dis_cuenta m
	      WHERE  m.nss             = ret_nega.nss 
	        AND  m.subcuenta       IN(2,6,9)
	        AND  m.tipo_movimiento = ret_nega.tipo_movimiento 
	        AND  m.consecutivo_lote= ret_nega.consecutivo
                AND m.fecha_conversion BETWEEN fecha_ini AND fecha_fin
                AND  m.tipo_movimiento <> 10
              IF sub_2 IS NULL OR sub_2 = 0 THEN
                 LET sub_2 = 0
              END IF

              SELECT ROUND(SUM(m.monto_en_pesos),2)
	      INTO   sub_22 FROM dis_cuenta m
	      WHERE  m.nss             = ret_nega.nss 
	        AND  m.subcuenta       IN(2,6,9)
	        AND  m.tipo_movimiento = 10
	        AND  m.consecutivo_lote= ret_nega.consecutivo
                AND m.fecha_conversion BETWEEN fecha_ini AND fecha_fin
              IF sub_22 IS NULL OR sub_22 = 0 THEN
                 LET sub_22 = 0
              END IF

              SELECT ROUND(SUM(m.monto_en_pesos),2)
	      INTO   sub_3  FROM dis_cuenta m
	      WHERE  m.nss             = ret_nega.nss 
	        AND  m.subcuenta       IN(4)
	        AND  m.tipo_movimiento = ret_nega.tipo_movimiento 
	        AND  m.consecutivo_lote= ret_nega.consecutivo
                AND m.fecha_conversion BETWEEN fecha_ini AND fecha_fin
                AND  m.tipo_movimiento <> 10
              IF sub_3 IS NULL OR sub_3 = 0 THEN
                 LET sub_3 = 0
              END IF

              SELECT ROUND(SUM(m.monto_en_pesos),2)
	      INTO   sub_33  FROM dis_cuenta m
	      WHERE  m.nss             = ret_nega.nss 
	        AND  m.subcuenta       IN(4)
	        AND  m.tipo_movimiento = 10
	        AND  m.consecutivo_lote= ret_nega.consecutivo
                AND m.fecha_conversion BETWEEN fecha_ini AND fecha_fin
              IF sub_33 IS NULL OR sub_33 = 0 THEN
                 LET sub_33 = 0
              END IF

              SELECT ROUND(SUM(m.monto_en_pesos),2)
	      INTO   sub_4  FROM dis_cuenta m
	      WHERE  m.nss             = ret_nega.nss 
	        AND  m.subcuenta       IN(5)
	        AND  m.tipo_movimiento = ret_nega.tipo_movimiento 
	        AND  m.consecutivo_lote= ret_nega.consecutivo
                AND m.fecha_conversion BETWEEN fecha_ini AND fecha_fin
                AND  m.tipo_movimiento <> 10
              IF sub_4 IS NULL OR sub_4 = 0 THEN
                 LET sub_4 = 0
              END IF

              SELECT ROUND(SUM(m.monto_en_pesos),2)
	      INTO   sub_44 FROM dis_cuenta m
	      WHERE  m.nss             = ret_nega.nss 
	        AND  m.subcuenta       IN(5)
	        AND  m.tipo_movimiento = 10
	        AND  m.consecutivo_lote= ret_nega.consecutivo
                AND  m.fecha_conversion BETWEEN fecha_ini AND fecha_fin
              IF sub_44 IS NULL OR sub_44 = 0 THEN
                 LET sub_44 = 0
              END IF

              SELECT ROUND(SUM(m.monto_en_pesos),2)
	      INTO   sub_5  FROM dis_cuenta m
	      WHERE  m.nss             = ret_nega.nss 
	        AND  m.subcuenta       IN(7)
	        AND  m.tipo_movimiento = ret_nega.tipo_movimiento 
	        AND  m.consecutivo_lote= ret_nega.consecutivo
                AND  m.fecha_conversion BETWEEN fecha_ini AND fecha_fin
                AND  m.tipo_movimiento <> 10
              IF sub_5 IS NULL OR sub_5 = 0 THEN
                 LET sub_5 = 0
              END IF

              SELECT ROUND(SUM(m.monto_en_pesos),2)
	      INTO   sub_55 FROM dis_cuenta m
	      WHERE  m.nss             = ret_nega.nss 
	        AND  m.subcuenta       IN(7)
	        AND  m.tipo_movimiento = 10
	        AND  m.consecutivo_lote= ret_nega.consecutivo
                AND  m.fecha_conversion BETWEEN fecha_ini AND fecha_fin
              IF sub_55 IS NULL OR sub_55 = 0 THEN
                 LET sub_55 = 0
              END IF

              SELECT ROUND(SUM(m.monto_en_pesos),2)
	      INTO   sub_6  FROM dis_cuenta m
	      WHERE  m.nss             = ret_nega.nss 
	        AND  m.subcuenta       IN(8)
	        AND  m.tipo_movimiento = ret_nega.tipo_movimiento 
	        AND  m.consecutivo_lote= ret_nega.consecutivo
                AND  m.fecha_conversion BETWEEN fecha_ini AND fecha_fin
                AND  m.tipo_movimiento <> 10
              IF sub_6 IS NULL OR sub_6 = 0 THEN
                 LET sub_6 = 0
              END IF

              SELECT ROUND(SUM(m.monto_en_pesos),2)
	      INTO   sub_66 FROM dis_cuenta m
	      WHERE  m.nss             = ret_nega.nss 
	        AND  m.subcuenta       IN(8)
	        AND  m.tipo_movimiento = 10
	        AND  m.consecutivo_lote= ret_nega.consecutivo
                AND  m.fecha_conversion BETWEEN fecha_ini AND fecha_fin
              IF sub_66 IS NULL OR sub_66 = 0 THEN
                 LET sub_66 = 0
              END IF

              LET ret_obli.monto_neto    = sub_1 + sub_2 + sub_3 + sub_4 +
					   sub_5 + sub_6

              LET ret_obli.impto_retenido= sub_11+ sub_22+ sub_33+ sub_44+
					   sub_55+ sub_66

              LET ret_obli.mto_antes_impto=ret_obli.monto_neto +
                                           ret_obli.impto_retenido

              LET tot_reg   = tot_reg + 1
              LET tmonto_neto = tmonto_neto + ret_obli.monto_neto 
              LET tmto_antes_impto = tmto_antes_impto + ret_obli.impto_retenido
              LET timpto_retenido = timpto_retenido + ret_obli.mto_antes_impto
              LET tpago_efectivo = tpago_efectivo + ret_obli.monto_neto

              LET v_rep  =  "D"                     ,"|",
                             ret_nega.nss            ,"|",
                             ret_nega.nombres        ,"|",
                             ret_nega.paterno        ,"|",
                             ret_nega.materno        ,"|",
                             1                       ,"|",
                             bla18                   ,"|",
                             ret_nega.fecha_captura USING "MMDDYYYYY"  ,"|",
                             ret_obli.monto_neto     ,"|",
                             ret_obli.mto_antes_impto,"|",
                             ret_obli.impto_retenido ,"|",
                             ret_nega.folio_soli     ,"|",
                             ret_nega.tienda         ,"|",
                             "820"                   ,"|",
                             ret_nega.consecutivo    ,"|",
                             "                  "    ,"|",
                             "               "       ,"|"
              OUTPUT TO REPORT r_report(v_rep,37,2)

              INITIALIZE ret_obli.* TO NULL
              LET sub_1 = 0    LET sub_2 = 0   LET sub_3 = 0   LET sub_4 = 0
              LET sub_5 = 0    LET sub_6 = 0
              LET sub_11= 0    LET sub_22= 0   LET sub_33= 0   LET sub_44= 0
              LET sub_55= 0    LET sub_66= 0

         END FOREACH
         FINISH REPORT r_report

##Detalle (E-830)
         START REPORT r_report TO v_det830

         LET importe_abono = 0   LET importe_orden = 0
         INITIALIZE ret_obli.*, ret_nega.* TO NULL
         LET sub_1 = 0    LET sub_2 = 0   LET sub_3 = 0   LET sub_4 = 0
         LET sub_5 = 0    LET sub_6 = 0
         LET sub_11= 0    LET sub_22= 0   LET sub_33= 0   LET sub_44= 0
         LET sub_55= 0    LET sub_66= 0
         DECLARE apt_E CURSOR FOR
              SELECT UNIQUE a.consecutivo_lote, a.tipo_movimiento,
                     b.nss, b.paterno, b.materno, b.nombres, b.tipo_pago,
                     b.fecha_captura, b.tienda_cod,c.folio_solicitud 
                     FROM  dis_cuenta a, ret_beneficiario b, ret_solicitud_tx c
                     WHERE a.nss = b.nss      
                       AND a.subcuenta IN(1,2,4,5,6,7,8,9)
                       AND a.tipo_movimiento = 830
                       AND a.consecutivo_lote = b.consecutivo
                       AND a.fecha_conversion BETWEEN fecha_ini AND fecha_fin
                       AND a.nss  = c.nss
                       AND a.consecutivo_lote = c.consecutivo
               ORDER BY 1,2,3
         FOREACH apt_E INTO ret_nega.*
              SELECT ROUND(SUM(m.monto_en_pesos),2)
	      INTO   sub_1  FROM dis_cuenta m
	      WHERE  m.nss             = ret_nega.nss 
	        AND  m.subcuenta       IN(1)
	        AND  m.tipo_movimiento = ret_nega.tipo_movimiento 
	        AND  m.consecutivo_lote= ret_nega.consecutivo
                AND m.fecha_conversion  BETWEEN fecha_ini AND fecha_fin
              IF sub_1 IS NULL OR sub_1 = 0 THEN
                 LET sub_1 = 0
              END IF

              SELECT ROUND(SUM(m.monto_en_pesos),2)
	      INTO   sub_11 FROM dis_cuenta m
	      WHERE  m.nss             = ret_nega.nss 
	        AND  m.subcuenta       IN(1)
	        AND  m.tipo_movimiento = 10
	        AND  m.consecutivo_lote= ret_nega.consecutivo
                AND m.fecha_conversion BETWEEN fecha_ini AND fecha_fin
              IF sub_11 IS NULL OR sub_11 = 0 THEN
                 LET sub_11 = 0
              END IF

              SELECT ROUND(SUM(m.monto_en_pesos),2)
	      INTO   sub_2  FROM dis_cuenta m
	      WHERE  m.nss             = ret_nega.nss 
	        AND  m.subcuenta       IN(2,6,9)
	        AND  m.tipo_movimiento = ret_nega.tipo_movimiento 
	        AND  m.consecutivo_lote= ret_nega.consecutivo
                AND m.fecha_conversion BETWEEN fecha_ini AND fecha_fin
              IF sub_2 IS NULL OR sub_2 = 0 THEN
                 LET sub_2 = 0
              END IF

              SELECT ROUND(SUM(m.monto_en_pesos),2)
	      INTO   sub_22  FROM dis_cuenta m
	      WHERE  m.nss             = ret_nega.nss 
	        AND  m.subcuenta       IN(2,6,9)
	        AND  m.tipo_movimiento = 10
	        AND  m.consecutivo_lote= ret_nega.consecutivo
                AND m.fecha_conversion BETWEEN fecha_ini AND fecha_fin
              IF sub_22 IS NULL OR sub_22 = 0 THEN
                 LET sub_22 = 0
              END IF

              SELECT ROUND(SUM(m.monto_en_pesos),2)
	      INTO   sub_3  FROM dis_cuenta m
	      WHERE  m.nss             = ret_nega.nss 
	        AND  m.subcuenta       IN(4)
	        AND  m.tipo_movimiento = ret_nega.tipo_movimiento 
	        AND  m.consecutivo_lote= ret_nega.consecutivo
                AND m.fecha_conversion BETWEEN fecha_ini AND fecha_fin
              IF sub_3 IS NULL OR sub_3 = 0 THEN
                 LET sub_3 = 0
              END IF

              SELECT ROUND(SUM(m.monto_en_pesos),2)
	      INTO   sub_33  FROM dis_cuenta m
	      WHERE  m.nss             = ret_nega.nss 
	        AND  m.subcuenta       IN(4)
	        AND  m.tipo_movimiento = 10
	        AND  m.consecutivo_lote= ret_nega.consecutivo
                AND m.fecha_conversion BETWEEN fecha_ini AND fecha_fin
              IF sub_33 IS NULL OR sub_33 = 0 THEN
                 LET sub_33 = 0
              END IF

              SELECT ROUND(SUM(m.monto_en_pesos),2)
	      INTO   sub_4  FROM dis_cuenta m
	      WHERE  m.nss             = ret_nega.nss 
	        AND  m.subcuenta       IN(5)
	        AND  m.tipo_movimiento = ret_nega.tipo_movimiento 
	        AND  m.consecutivo_lote= ret_nega.consecutivo
                AND m.fecha_conversion BETWEEN fecha_ini AND fecha_fin
              IF sub_4 IS NULL OR sub_4 = 0 THEN
                 LET sub_4 = 0
              END IF

              SELECT ROUND(SUM(m.monto_en_pesos),2)
	      INTO   sub_44  FROM dis_cuenta m
	      WHERE  m.nss             = ret_nega.nss 
	        AND  m.subcuenta       IN(5)
	        AND  m.tipo_movimiento = 10
	        AND  m.consecutivo_lote= ret_nega.consecutivo
                AND m.fecha_conversion BETWEEN fecha_ini AND fecha_fin
              IF sub_44 IS NULL OR sub_44 = 0 THEN
                 LET sub_44 = 0
              END IF

              SELECT ROUND(SUM(m.monto_en_pesos),2)
	      INTO   sub_5  FROM dis_cuenta m
	      WHERE  m.nss             = ret_nega.nss 
	        AND  m.subcuenta       IN(7)
	        AND  m.tipo_movimiento = ret_nega.tipo_movimiento 
	        AND  m.consecutivo_lote= ret_nega.consecutivo
                AND m.fecha_conversion BETWEEN fecha_ini AND fecha_fin
              IF sub_5 IS NULL OR sub_5 = 0 THEN
                 LET sub_5 = 0
              END IF

              SELECT ROUND(SUM(m.monto_en_pesos),2)
	      INTO   sub_55  FROM dis_cuenta m
	      WHERE  m.nss             = ret_nega.nss 
	        AND  m.subcuenta       IN(7)
	        AND  m.tipo_movimiento = 10
	        AND  m.consecutivo_lote= ret_nega.consecutivo
                AND m.fecha_conversion BETWEEN fecha_ini AND fecha_fin
              IF sub_55 IS NULL OR sub_55 = 0 THEN
                 LET sub_55 = 0
              END IF

              SELECT ROUND(SUM(m.monto_en_pesos),2)
	      INTO   sub_6  FROM dis_cuenta m
	      WHERE  m.nss             = ret_nega.nss 
	        AND  m.subcuenta       IN(8)
	        AND  m.tipo_movimiento = ret_nega.tipo_movimiento 
	        AND  m.consecutivo_lote= ret_nega.consecutivo
                AND m.fecha_conversion BETWEEN fecha_ini AND fecha_fin
              IF sub_6 IS NULL OR sub_6 = 0 THEN
                 LET sub_6 = 0
              END IF

              SELECT ROUND(SUM(m.monto_en_pesos),2)
	      INTO   sub_66  FROM dis_cuenta m
	      WHERE  m.nss             = ret_nega.nss 
	        AND  m.subcuenta       IN(8)
	        AND  m.tipo_movimiento = 10
	        AND  m.consecutivo_lote= ret_nega.consecutivo
                AND m.fecha_conversion BETWEEN fecha_ini AND fecha_fin
              IF sub_66 IS NULL OR sub_66 = 0 THEN
                 LET sub_66 = 0
              END IF

              LET ret_obli.monto_neto    = sub_1 + sub_2 + sub_3 + sub_4 +
					   sub_5 + sub_6

              LET ret_obli.impto_retenido= sub_11+ sub_22+ sub_33+ sub_44+
					   sub_55+ sub_66

              LET ret_obli.mto_antes_impto=ret_obli.monto_neto +
                                           ret_obli.impto_retenido

              LET tot_reg   = tot_reg + 1
              LET tmonto_neto = tmonto_neto + ret_obli.monto_neto 
              LET tmto_antes_impto = tmto_antes_impto + ret_obli.impto_retenido
              LET timpto_retenido = timpto_retenido + ret_obli.mto_antes_impto
              LET tpago_efectivo = tpago_efectivo + ret_obli.monto_neto

              LET v_rep  =  "D"                     ,"|",
                             ret_nega.nss            ,"|",
                             ret_nega.nombres        ,"|",
                             ret_nega.paterno        ,"|",
                             ret_nega.materno        ,"|",
                             1                       ,"|",
                             bla18                   ,"|",
                             ret_nega.fecha_captura USING "MMDDYYYY"  ,"|",
                             ret_obli.monto_neto     ,"|",
                             ret_obli.mto_antes_impto,"|",
                             ret_obli.impto_retenido ,"|",
                             ret_nega.folio_soli     ,"|",
                             ret_nega.tienda         ,"|",
                             "830"                   ,"|",
                             ret_nega.consecutivo    ,"|",
                             "                  "    ,"|",
                             "               "       ,"|"
              OUTPUT TO REPORT r_report(v_rep,37,2)

              INITIALIZE ret_obli.* TO NULL
              LET sub_1 = 0    LET sub_2 = 0   LET sub_3 = 0   LET sub_4 = 0
              LET sub_5 = 0    LET sub_6 = 0
              LET sub_11= 0    LET sub_22= 0   LET sub_33= 0   LET sub_44= 0
              LET sub_55= 0    LET sub_66= 0
         END FOREACH
         FINISH REPORT r_report

##Detalle (F-840)
         START REPORT r_report TO v_det840

         LET importe_abono = 0   LET importe_orden = 0
         INITIALIZE ret_obli.*, ret_nega.* TO NULL
         LET sub_1 = 0    LET sub_2 = 0   LET sub_3 = 0   LET sub_4 = 0
         LET sub_5 = 0    LET sub_6 = 0
         LET sub_11= 0    LET sub_22= 0   LET sub_33= 0   LET sub_44= 0
         LET sub_55= 0    LET sub_66= 0
         DECLARE apt_F CURSOR FOR
              SELECT UNIQUE a.consecutivo_lote, a.tipo_movimiento,
                     b.nss, b.paterno, b.materno, b.nombres, b.tipo_pago,
                     b.fecha_captura, b.tienda_cod,c.folio_solicitud 
                     FROM  dis_cuenta a, ret_beneficiario b, ret_solicitud_tx c
                     WHERE a.nss = b.nss      
                       AND a.subcuenta IN(1,2,4,5,6,7,8,9)
                       AND a.tipo_movimiento = 840
                       AND a.consecutivo_lote = b.consecutivo
                       AND a.fecha_conversion BETWEEN fecha_ini AND fecha_fin
                       AND a.nss  = c.nss
                       AND a.consecutivo_lote = c.consecutivo
               ORDER BY 1,2,3
         FOREACH apt_F INTO ret_nega.*
              SELECT ROUND(SUM(m.monto_en_pesos),2)
	      INTO   sub_1  FROM dis_cuenta m
	      WHERE  m.nss             = ret_nega.nss 
	        AND  m.subcuenta       IN(1)
	        AND  m.tipo_movimiento = ret_nega.tipo_movimiento 
	        AND  m.consecutivo_lote= ret_nega.consecutivo
                AND m.fecha_conversion BETWEEN fecha_ini AND fecha_fin
              IF sub_1 IS NULL OR sub_1 = 0 THEN
                 LET sub_1 = 0
              END IF

              SELECT ROUND(SUM(m.monto_en_pesos),2)
	      INTO   sub_11  FROM dis_cuenta m
	      WHERE  m.nss             = ret_nega.nss 
	        AND  m.subcuenta       IN(1)
	        AND  m.tipo_movimiento = 10
	        AND  m.consecutivo_lote= ret_nega.consecutivo
                AND m.fecha_conversion BETWEEN fecha_ini AND fecha_fin
              IF sub_11 IS NULL OR sub_11 = 0 THEN
                 LET sub_11 = 0
              END IF

              SELECT ROUND(SUM(m.monto_en_pesos),2)
	      INTO   sub_2  FROM dis_cuenta m
	      WHERE  m.nss             = ret_nega.nss 
	        AND  m.subcuenta       IN(2,6,9)
	        AND  m.tipo_movimiento = ret_nega.tipo_movimiento 
	        AND  m.consecutivo_lote= ret_nega.consecutivo
                AND m.fecha_conversion BETWEEN fecha_ini AND fecha_fin
              IF sub_2 IS NULL OR sub_2 = 0 THEN
                 LET sub_2 = 0
              END IF

              SELECT ROUND(SUM(m.monto_en_pesos),2)
	      INTO   sub_22  FROM dis_cuenta m
	      WHERE  m.nss             = ret_nega.nss 
	        AND  m.subcuenta       IN(2,6,9)
	        AND  m.tipo_movimiento = 10
	        AND  m.consecutivo_lote= ret_nega.consecutivo
                AND m.fecha_conversion BETWEEN fecha_ini AND fecha_fin
              IF sub_22 IS NULL OR sub_22 = 0 THEN
                 LET sub_22 = 0
              END IF

              SELECT ROUND(SUM(m.monto_en_pesos),2)
	      INTO   sub_3  FROM dis_cuenta m
	      WHERE  m.nss             = ret_nega.nss 
	        AND  m.subcuenta       IN(4)
	        AND  m.tipo_movimiento = ret_nega.tipo_movimiento 
	        AND  m.consecutivo_lote= ret_nega.consecutivo
                AND m.fecha_conversion BETWEEN fecha_ini AND fecha_fin
              IF sub_3 IS NULL OR sub_3 = 0 THEN
                 LET sub_3 = 0
              END IF

              SELECT ROUND(SUM(m.monto_en_pesos),2)
	      INTO   sub_33  FROM dis_cuenta m
	      WHERE  m.nss             = ret_nega.nss 
	        AND  m.subcuenta       IN(4)
	        AND  m.tipo_movimiento = 10
	        AND  m.consecutivo_lote= ret_nega.consecutivo
                AND m.fecha_conversion BETWEEN fecha_ini AND fecha_fin
              IF sub_33 IS NULL OR sub_33 = 0 THEN
                 LET sub_33 = 0
              END IF

              SELECT ROUND(SUM(m.monto_en_pesos),2)
	      INTO   sub_4  FROM dis_cuenta m
	      WHERE  m.nss             = ret_nega.nss 
	        AND  m.subcuenta       IN(5)
	        AND  m.tipo_movimiento = ret_nega.tipo_movimiento 
	        AND  m.consecutivo_lote= ret_nega.consecutivo
                AND m.fecha_conversion BETWEEN fecha_ini AND fecha_fin
              IF sub_4 IS NULL OR sub_4 = 0 THEN
                 LET sub_4 = 0
              END IF

              SELECT ROUND(SUM(m.monto_en_pesos),2)
	      INTO   sub_44  FROM dis_cuenta m
	      WHERE  m.nss             = ret_nega.nss 
	        AND  m.subcuenta       IN(5)
	        AND  m.tipo_movimiento = 10
	        AND  m.consecutivo_lote= ret_nega.consecutivo
                AND m.fecha_conversion BETWEEN fecha_ini AND fecha_fin
              IF sub_44 IS NULL OR sub_44 = 0 THEN
                 LET sub_44 = 0
              END IF

              SELECT ROUND(SUM(m.monto_en_pesos),2)
	      INTO   sub_5  FROM dis_cuenta m
	      WHERE  m.nss             = ret_nega.nss 
	        AND  m.subcuenta       IN(7)
	        AND  m.tipo_movimiento = ret_nega.tipo_movimiento 
	        AND  m.consecutivo_lote= ret_nega.consecutivo
                AND m.fecha_conversion BETWEEN fecha_ini AND fecha_fin
              IF sub_5 IS NULL OR sub_5 = 0 THEN
                 LET sub_5 = 0
              END IF

              SELECT ROUND(SUM(m.monto_en_pesos),2)
	      INTO   sub_55  FROM dis_cuenta m
	      WHERE  m.nss             = ret_nega.nss 
	        AND  m.subcuenta       IN(7)
	        AND  m.tipo_movimiento = 10
	        AND  m.consecutivo_lote= ret_nega.consecutivo
                AND m.fecha_conversion BETWEEN fecha_ini AND fecha_fin
              IF sub_55 IS NULL OR sub_55 = 0 THEN
                 LET sub_55 = 0
              END IF

              SELECT ROUND(SUM(m.monto_en_pesos),2)
	      INTO   sub_6  FROM dis_cuenta m
	      WHERE  m.nss             = ret_nega.nss 
	        AND  m.subcuenta       IN(8)
	        AND  m.tipo_movimiento = ret_nega.tipo_movimiento 
	        AND  m.consecutivo_lote= ret_nega.consecutivo
                AND m.fecha_conversion BETWEEN fecha_ini AND fecha_fin
              IF sub_6 IS NULL OR sub_6 = 0 THEN
                 LET sub_6 = 0
              END IF

              SELECT ROUND(SUM(m.monto_en_pesos),2)
	      INTO   sub_66  FROM dis_cuenta m
	      WHERE  m.nss             = ret_nega.nss 
	        AND  m.subcuenta       IN(8)
	        AND  m.tipo_movimiento = 10
	        AND  m.consecutivo_lote= ret_nega.consecutivo
                AND m.fecha_conversion BETWEEN fecha_ini AND fecha_fin
              IF sub_66 IS NULL OR sub_66 = 0 THEN
                 LET sub_66 = 0
              END IF

              LET ret_obli.monto_neto    = sub_1 + sub_2 + sub_3 + sub_4 +
					   sub_5 + sub_6

              LET ret_obli.impto_retenido= sub_11+ sub_22+ sub_33+ sub_44+
					   sub_55+ sub_66

              LET ret_obli.mto_antes_impto=ret_obli.monto_neto +
                                           ret_obli.impto_retenido

              LET tot_reg   = tot_reg + 1
              LET tmonto_neto = tmonto_neto + ret_obli.monto_neto 
              LET tmto_antes_impto = tmto_antes_impto + ret_obli.impto_retenido
              LET timpto_retenido = timpto_retenido + ret_obli.mto_antes_impto
              LET tpago_efectivo = tpago_efectivo + ret_obli.monto_neto

              LET v_rep  =  "D"                     ,"|",
                             ret_nega.nss            ,"|",
                             ret_nega.nombres        ,"|",
                             ret_nega.paterno        ,"|",
                             ret_nega.materno        ,"|",
                             1                       ,"|",
                             bla18                   ,"|",
                             ret_nega.fecha_captura USING "MMDDYYYY"  ,"|",
                             ret_obli.monto_neto     ,"|",
                             ret_obli.mto_antes_impto,"|",
                             ret_obli.impto_retenido ,"|",
                             ret_nega.folio_soli     ,"|",
                             ret_nega.tienda         ,"|",
                             "840"                   ,"|",
                             ret_nega.consecutivo    ,"|",
                             "                  "    ,"|",
                             "               "       ,"|"
              OUTPUT TO REPORT r_report(v_rep,37,2)

              INITIALIZE ret_obli.* TO NULL
              LET sub_1 = 0    LET sub_2 = 0   LET sub_3 = 0   LET sub_4 = 0
              LET sub_5 = 0    LET sub_6 = 0
              LET sub_11= 0    LET sub_22= 0   LET sub_33= 0   LET sub_44= 0
              LET sub_55= 0    LET sub_66= 0
         END FOREACH
         FINISH REPORT r_report

##Detalle (G-850)
         START REPORT r_report TO v_det850

         LET importe_abono = 0   LET importe_orden = 0
         INITIALIZE ret_obli.*, ret_nega.* TO NULL
         LET sub_1 = 0    LET sub_2 = 0   LET sub_3 = 0   LET sub_4 = 0
         LET sub_5 = 0    LET sub_6 = 0
         LET sub_11= 0    LET sub_22= 0   LET sub_33= 0   LET sub_44= 0
         LET sub_55= 0    LET sub_66= 0
         DECLARE apt_G CURSOR FOR
              SELECT UNIQUE a.consecutivo_lote, a.tipo_movimiento,
                     b.nss, b.paterno, b.materno, b.nombres, b.tipo_pago,
                     b.fecha_captura, b.tienda_cod,c.folio_solicitud 
                     FROM  dis_cuenta a, ret_beneficiario b, ret_solicitud_tx c
                     WHERE a.nss = b.nss      
                       AND a.subcuenta IN(1,2,4,5,6,7,8,9)
                       AND a.tipo_movimiento = 850
                       AND a.consecutivo_lote = b.consecutivo
                       AND a.fecha_conversion BETWEEN fecha_ini AND fecha_fin
                       AND a.nss  = c.nss
                       AND a.consecutivo_lote = c.consecutivo
               ORDER BY 1,2,3
         FOREACH apt_G INTO ret_nega.*
              SELECT ROUND(SUM(m.monto_en_pesos),2)
	      INTO   sub_1  FROM dis_cuenta m
	      WHERE  m.nss             = ret_nega.nss 
	        AND  m.subcuenta       IN(1)
	        AND  m.tipo_movimiento = ret_nega.tipo_movimiento 
	        AND  m.consecutivo_lote= ret_nega.consecutivo
                AND m.fecha_conversion BETWEEN fecha_ini AND fecha_fin
              IF sub_1 IS NULL OR sub_1 = 0 THEN
                 LET sub_1 = 0
              END IF

              SELECT ROUND(SUM(m.monto_en_pesos),2)
	      INTO   sub_11  FROM dis_cuenta m
	      WHERE  m.nss             = ret_nega.nss 
	        AND  m.subcuenta       IN(1)
	        AND  m.tipo_movimiento = 10
	        AND  m.consecutivo_lote= ret_nega.consecutivo
                AND m.fecha_conversion BETWEEN fecha_ini AND fecha_fin
              IF sub_11 IS NULL OR sub_11 = 0 THEN
                 LET sub_11 = 0
              END IF

              SELECT ROUND(SUM(m.monto_en_pesos),2)
	      INTO   sub_2  FROM dis_cuenta m
	      WHERE  m.nss             = ret_nega.nss 
	        AND  m.subcuenta       IN(2,6,9)
	        AND  m.tipo_movimiento = ret_nega.tipo_movimiento 
	        AND  m.consecutivo_lote= ret_nega.consecutivo
                AND m.fecha_conversion BETWEEN fecha_ini AND fecha_fin
              IF sub_2 IS NULL OR sub_2 = 0 THEN
                 LET sub_2 = 0
              END IF

              SELECT ROUND(SUM(m.monto_en_pesos),2)
	      INTO   sub_22  FROM dis_cuenta m
	      WHERE  m.nss             = ret_nega.nss 
	        AND  m.subcuenta       IN(2,6,9)
	        AND  m.tipo_movimiento = 10
	        AND  m.consecutivo_lote= ret_nega.consecutivo
                AND m.fecha_conversion BETWEEN fecha_ini AND fecha_fin
              IF sub_22 IS NULL OR sub_22 = 0 THEN
                 LET sub_22 = 0
              END IF

              SELECT ROUND(SUM(m.monto_en_pesos),2)
	      INTO   sub_3  FROM dis_cuenta m
	      WHERE  m.nss             = ret_nega.nss 
	        AND  m.subcuenta       IN(4)
	        AND  m.tipo_movimiento = ret_nega.tipo_movimiento 
	        AND  m.consecutivo_lote= ret_nega.consecutivo
                AND m.fecha_conversion BETWEEN fecha_ini AND fecha_fin
              IF sub_3 IS NULL OR sub_3 = 0 THEN
                 LET sub_3 = 0
              END IF

              SELECT ROUND(SUM(m.monto_en_pesos),2)
	      INTO   sub_33  FROM dis_cuenta m
	      WHERE  m.nss             = ret_nega.nss 
	        AND  m.subcuenta       IN(4)
	        AND  m.tipo_movimiento = 10
	        AND  m.consecutivo_lote= ret_nega.consecutivo
                AND m.fecha_conversion BETWEEN fecha_ini AND fecha_fin
              IF sub_33 IS NULL OR sub_33 = 0 THEN
                 LET sub_33 = 0
              END IF

              SELECT ROUND(SUM(m.monto_en_pesos),2)
	      INTO   sub_4  FROM dis_cuenta m
	      WHERE  m.nss             = ret_nega.nss 
	        AND  m.subcuenta       IN(5)
	        AND  m.tipo_movimiento = ret_nega.tipo_movimiento 
	        AND  m.consecutivo_lote= ret_nega.consecutivo
                AND m.fecha_conversion BETWEEN fecha_ini AND fecha_fin
              IF sub_4 IS NULL OR sub_4 = 0 THEN
                 LET sub_4 = 0
              END IF

              SELECT ROUND(SUM(m.monto_en_pesos),2)
	      INTO   sub_44  FROM dis_cuenta m
	      WHERE  m.nss             = ret_nega.nss 
	        AND  m.subcuenta       IN(5)
	        AND  m.tipo_movimiento = 10
	        AND  m.consecutivo_lote= ret_nega.consecutivo
                AND m.fecha_conversion BETWEEN fecha_ini AND fecha_fin
              IF sub_44 IS NULL OR sub_44 = 0 THEN
                 LET sub_44 = 0
              END IF

              SELECT ROUND(SUM(m.monto_en_pesos),2)
	      INTO   sub_5  FROM dis_cuenta m
	      WHERE  m.nss             = ret_nega.nss 
	        AND  m.subcuenta       IN(7)
	        AND  m.tipo_movimiento = ret_nega.tipo_movimiento 
	        AND  m.consecutivo_lote= ret_nega.consecutivo
                AND m.fecha_conversion BETWEEN fecha_ini AND fecha_fin
              IF sub_5 IS NULL OR sub_5 = 0 THEN
                 LET sub_5 = 0
              END IF

              SELECT ROUND(SUM(m.monto_en_pesos),2)
	      INTO   sub_55  FROM dis_cuenta m
	      WHERE  m.nss             = ret_nega.nss 
	        AND  m.subcuenta       IN(7)
	        AND  m.tipo_movimiento = 10
	        AND  m.consecutivo_lote= ret_nega.consecutivo
                AND m.fecha_conversion BETWEEN fecha_ini AND fecha_fin
              IF sub_55 IS NULL OR sub_55 = 0 THEN
                 LET sub_55 = 0
              END IF

              SELECT ROUND(SUM(m.monto_en_pesos),2)
	      INTO   sub_6  FROM dis_cuenta m
	      WHERE  m.nss             = ret_nega.nss 
	        AND  m.subcuenta       IN(8)
	        AND  m.tipo_movimiento = ret_nega.tipo_movimiento 
	        AND  m.consecutivo_lote= ret_nega.consecutivo
                AND m.fecha_conversion BETWEEN fecha_ini AND fecha_fin
              IF sub_6 IS NULL OR sub_6 = 0 THEN
                 LET sub_6 = 0
              END IF

              SELECT ROUND(SUM(m.monto_en_pesos),2)
	      INTO   sub_66  FROM dis_cuenta m
	      WHERE  m.nss             = ret_nega.nss 
	        AND  m.subcuenta       IN(8)
	        AND  m.tipo_movimiento = 10
	        AND  m.consecutivo_lote= ret_nega.consecutivo
                AND m.fecha_conversion BETWEEN fecha_ini AND fecha_fin
              IF sub_66 IS NULL OR sub_66 = 0 THEN
                 LET sub_66 = 0
              END IF

              LET ret_obli.monto_neto    = sub_1 + sub_2 + sub_3 + sub_4 +
					   sub_5 + sub_6

              LET ret_obli.impto_retenido= sub_11+ sub_22+ sub_33+ sub_44+
					   sub_55+ sub_66

              LET ret_obli.mto_antes_impto=ret_obli.monto_neto +
                                           ret_obli.impto_retenido

              LET tot_reg   = tot_reg + 1
              LET tmonto_neto = tmonto_neto + ret_obli.monto_neto 
              LET tmto_antes_impto = tmto_antes_impto + ret_obli.impto_retenido
              LET timpto_retenido = timpto_retenido + ret_obli.mto_antes_impto
              LET tpago_efectivo = tpago_efectivo + ret_obli.monto_neto

              LET v_rep  =  "D"                     ,"|",
                             ret_nega.nss            ,"|",
                             ret_nega.nombres        ,"|",
                             ret_nega.paterno        ,"|",
                             ret_nega.materno        ,"|",
                             1                       ,"|",
                             bla18                   ,"|",
                             ret_nega.fecha_captura USING "MMDDYYYY"  ,"|",
                             ret_obli.monto_neto     ,"|",
                             ret_obli.mto_antes_impto,"|",
                             ret_obli.impto_retenido ,"|",
                             ret_nega.folio_soli     ,"|",
                             ret_nega.tienda         ,"|",
                             "850"                   ,"|",
                             ret_nega.consecutivo    ,"|",
                             "                  "    ,"|",
                             "               "       ,"|"
              OUTPUT TO REPORT r_report(v_rep,37,2)

              INITIALIZE ret_obli.* TO NULL
              LET sub_1 = 0    LET sub_2 = 0   LET sub_3 = 0   LET sub_4 = 0
              LET sub_5 = 0    LET sub_6 = 0
              LET sub_11= 0    LET sub_22= 0   LET sub_33= 0   LET sub_44= 0
              LET sub_55= 0    LET sub_66= 0
         END FOREACH
         FINISH REPORT r_report

##Detalle (H-860)
         START REPORT r_report TO v_det860

         LET importe_abono = 0   LET importe_orden = 0
         INITIALIZE ret_obli.*, ret_nega.* TO NULL
         LET sub_1 = 0    LET sub_2 = 0   LET sub_3 = 0   LET sub_4 = 0
         LET sub_11= 0    LET sub_22= 0   LET sub_33= 0   LET sub_44= 0
         LET sub_55= 0    LET sub_66= 0
         DECLARE apt_H CURSOR FOR
              SELECT UNIQUE a.consecutivo_lote, a.tipo_movimiento,
                     b.nss, b.paterno, b.materno, b.nombres, b.tipo_pago,
                     b.fecha_captura, b.tienda_cod,c.folio_solicitud 
                     FROM  dis_cuenta a, ret_beneficiario b, ret_solicitud_tx c
                     WHERE a.nss = b.nss      
                       AND a.subcuenta IN(7,8)
                       AND a.tipo_movimiento = 860
                       AND a.consecutivo_lote = b.consecutivo
                       AND a.fecha_conversion BETWEEN fecha_ini AND fecha_fin
                       AND a.nss  = c.nss
                       AND a.consecutivo_lote = c.consecutivo
               ORDER BY 1,2,3
         FOREACH apt_H INTO ret_nega.*
              SELECT ROUND(SUM(m.monto_en_pesos),2)
	      INTO   sub_1  FROM dis_cuenta m
	      WHERE  m.nss             = ret_nega.nss 
	        AND  m.subcuenta       = 7
	        AND  m.tipo_movimiento = ret_nega.tipo_movimiento 
	        AND  m.consecutivo_lote= ret_nega.consecutivo
                AND m.fecha_conversion BETWEEN fecha_ini AND fecha_fin
              IF sub_1 IS NULL OR sub_1 = 0 THEN
                 LET sub_1 = 0
              END IF

              SELECT ROUND(SUM(m.monto_en_pesos),2)
	      INTO   sub_11  FROM dis_cuenta m
	      WHERE  m.nss             = ret_nega.nss 
	        AND  m.subcuenta       = 7
	        AND  m.tipo_movimiento = 10
	        AND  m.consecutivo_lote= ret_nega.consecutivo
                AND m.fecha_conversion BETWEEN fecha_ini AND fecha_fin
              IF sub_11 IS NULL OR sub_11 = 0 THEN
                 LET sub_11 = 0
              END IF

              SELECT ROUND(SUM(m.monto_en_pesos),2)
	      INTO   sub_2  FROM dis_cuenta m
	      WHERE  m.nss             = ret_nega.nss 
	        AND  m.subcuenta       = 8
	        AND  m.tipo_movimiento = ret_nega.tipo_movimiento 
	        AND  m.consecutivo_lote= ret_nega.consecutivo
                AND m.fecha_conversion BETWEEN fecha_ini AND fecha_fin
              IF sub_2 IS NULL OR sub_2 = 0 THEN
                 LET sub_2 = 0
              END IF

              SELECT ROUND(SUM(m.monto_en_pesos),2)
	      INTO   sub_22  FROM dis_cuenta m
	      WHERE  m.nss             = ret_nega.nss 
	        AND  m.subcuenta       = 8
	        AND  m.tipo_movimiento = 10
	        AND  m.consecutivo_lote= ret_nega.consecutivo
                AND m.fecha_conversion BETWEEN fecha_ini AND fecha_fin
              IF sub_22 IS NULL OR sub_22 = 0 THEN
                 LET sub_22 = 0
              END IF

              LET ret_obli.monto_neto    = sub_1 + sub_2

              LET ret_obli.impto_retenido= sub_11+ sub_22

              LET ret_obli.mto_antes_impto=ret_obli.monto_neto +
                                           ret_obli.impto_retenido

              LET tot_reg   = tot_reg + 1
              LET tmonto_neto = tmonto_neto + ret_obli.monto_neto 
              LET tmto_antes_impto = tmto_antes_impto + ret_obli.impto_retenido
              LET timpto_retenido = timpto_retenido + ret_obli.mto_antes_impto
              LET tpago_efectivo = tpago_efectivo + ret_obli.monto_neto

              LET v_rep  =  "D"                     ,"|",
                             ret_nega.nss            ,"|",
                             ret_nega.nombres        ,"|",
                             ret_nega.paterno        ,"|",
                             ret_nega.materno        ,"|",
                             1                       ,"|",
                             bla18                   ,"|",
                             ret_nega.fecha_captura USING "MMDDYYYY"  ,"|",
                             ret_obli.monto_neto     ,"|",
                             ret_obli.mto_antes_impto,"|",
                             ret_obli.impto_retenido ,"|",
                             ret_nega.folio_soli     ,"|",
                             ret_nega.tienda         ,"|",
                             "860"                   ,"|",
                             ret_nega.consecutivo    ,"|",
                             "                  "    ,"|",
                             "               "       ,"|"
              OUTPUT TO REPORT r_report(v_rep,37,2)

              INITIALIZE ret_obli.* TO NULL
              LET sub_1 = 0    LET sub_2 = 0
              LET sub_11= 0    LET sub_22= 0   LET sub_33= 0   LET sub_44= 0
              LET sub_55= 0    LET sub_66= 0
         END FOREACH
         FINISH REPORT r_report

##Detalle (I-870)  PARCIAL Matrimonio
         START REPORT r_report TO v_det870

         LET importe_abono = 0
         LET importe_orden = 0
         LET sub_1 = 0    LET sub_2 = 0   LET sub_3 = 0   LET sub_4 = 0
         LET sub_5 = 0    LET sub_6 = 0
         LET sub_11= 0    LET sub_22= 0   LET sub_33= 0   LET sub_44= 0
         LET sub_55= 0    LET sub_66= 0
         INITIALIZE ret_obli.*, ret_nega.* TO NULL
         DECLARE apt_I CURSOR FOR
              SELECT UNIQUE a.consecutivo_lote, a.tipo_movimiento,
                     b.nss, b.paterno, b.materno, b.nombres, b.tipo_pago,
                     b.fecha_captura, b.tienda_cod,c.folio_solicitud 
                     FROM  dis_cuenta a, ret_beneficiario b, ret_parcial c
                     WHERE a.nss = b.nss      
                       AND a.subcuenta IN(1,2,5,6,9)
                       AND a.tipo_movimiento = 870
                       AND a.consecutivo_lote = b.consecutivo
                       AND a.fecha_conversion BETWEEN fecha_ini AND fecha_fin
                       AND a.nss  = c.nss
                       AND a.consecutivo_lote = c.consecutivo
               ORDER BY 1,2,3

         FOREACH apt_I INTO ret_nega.*

              SELECT ROUND(SUM(m.monto_en_pesos),2)
	      INTO   sub_1  FROM dis_cuenta m
	      WHERE  m.nss             = ret_nega.nss 
	        AND  m.subcuenta       IN(1)
	        AND  m.tipo_movimiento = ret_nega.tipo_movimiento 
	        AND  m.consecutivo_lote= ret_nega.consecutivo
                AND m.fecha_conversion BETWEEN fecha_ini AND fecha_fin
              IF sub_1 IS NULL OR sub_1 = 0 THEN
                 LET sub_1 = 0
              END IF

              SELECT ROUND(SUM(m.monto_en_pesos),2)
	      INTO   sub_11  FROM dis_cuenta m
	      WHERE  m.nss             = ret_nega.nss 
	        AND  m.subcuenta       IN(1)
	        AND  m.tipo_movimiento = 10
	        AND  m.consecutivo_lote= ret_nega.consecutivo
                AND m.fecha_conversion BETWEEN fecha_ini AND fecha_fin
              IF sub_11 IS NULL OR sub_11 = 0 THEN
                 LET sub_11 = 0
              END IF

              SELECT ROUND(SUM(m.monto_en_pesos),2)
	      INTO   sub_2  FROM dis_cuenta m
	      WHERE  m.nss             = ret_nega.nss 
	        AND  m.subcuenta       IN(5)
	        AND  m.tipo_movimiento = ret_nega.tipo_movimiento 
	        AND  m.consecutivo_lote= ret_nega.consecutivo
                AND m.fecha_conversion BETWEEN fecha_ini AND fecha_fin
              IF sub_2 IS NULL OR sub_2 = 0 THEN
                 LET sub_2 = 0
              END IF

              SELECT ROUND(SUM(m.monto_en_pesos),2)
	      INTO   sub_22  FROM dis_cuenta m
	      WHERE  m.nss             = ret_nega.nss 
	        AND  m.subcuenta       IN(5)
	        AND  m.tipo_movimiento = 10
	        AND  m.consecutivo_lote= ret_nega.consecutivo
                AND m.fecha_conversion BETWEEN fecha_ini AND fecha_fin
              IF sub_22 IS NULL OR sub_22 = 0 THEN
                 LET sub_22 = 0
              END IF

              SELECT ROUND(SUM(m.monto_en_pesos),2)
	      INTO   sub_3  FROM dis_cuenta m
	      WHERE  m.nss             = ret_nega.nss 
	        AND  m.subcuenta       IN(2,6,9)
	        AND  m.tipo_movimiento = ret_nega.tipo_movimiento 
	        AND  m.consecutivo_lote= ret_nega.consecutivo
                AND m.fecha_conversion BETWEEN fecha_ini AND fecha_fin
              IF sub_3 IS NULL OR sub_3 = 0 THEN
                 LET sub_3 = 0
              END IF

              SELECT ROUND(SUM(m.monto_en_pesos),2)
	      INTO   sub_33  FROM dis_cuenta m
	      WHERE  m.nss             = ret_nega.nss 
	        AND  m.subcuenta       IN(2,6,9)
	        AND  m.tipo_movimiento = 10
	        AND  m.consecutivo_lote= ret_nega.consecutivo
                AND m.fecha_conversion BETWEEN fecha_ini AND fecha_fin
              IF sub_33 IS NULL OR sub_33 = 0 THEN
                 LET sub_33 = 0
              END IF

              LET ret_obli.monto_neto    = sub_1 + sub_2 + sub_3 

              LET ret_obli.impto_retenido= sub_11+ sub_22+ sub_33

              LET ret_obli.mto_antes_impto=ret_obli.monto_neto +
                                           ret_obli.impto_retenido

              LET tot_reg   = tot_reg + 1
              LET tmonto_neto = tmonto_neto + ret_obli.monto_neto 
              LET tmto_antes_impto = tmto_antes_impto + ret_obli.impto_retenido
              LET timpto_retenido = timpto_retenido + ret_obli.mto_antes_impto
              LET tpago_efectivo = tpago_efectivo + ret_obli.monto_neto

              LET v_rep  =  "D"                     ,"|",
                             ret_nega.nss            ,"|",
                             ret_nega.nombres        ,"|",
                             ret_nega.paterno        ,"|",
                             ret_nega.materno        ,"|",
                             1                       ,"|",
                             bla18                   ,"|",
                             ret_nega.fecha_captura  USING "MMDDYYYY" ,"|",
                             ret_obli.monto_neto     ,"|",
                             ret_obli.mto_antes_impto,"|",
                             ret_obli.impto_retenido ,"|",
                             ret_nega.folio_soli     ,"|",
                             ret_nega.tienda         ,"|",
                             "870"                   ,"|",
                             ret_nega.consecutivo    ,"|",
                             "                  "    ,"|",
                             "               "       ,"|"
              OUTPUT TO REPORT r_report(v_rep,37,2)
              
              INITIALIZE ret_obli.* TO NULL
              LET sub_1 = 0    LET sub_2 = 0   LET sub_3 = 0   LET sub_4 = 0
              LET sub_5 = 0    LET sub_6 = 0
              LET sub_11= 0    LET sub_22= 0   LET sub_33= 0   LET sub_44= 0
              LET sub_55= 0    LET sub_66= 0
         END FOREACH
         FINISH REPORT r_report

##Detalle (I-875)  PARCIAL Desempleo
         START REPORT r_report TO v_det875

         LET importe_abono = 0
         LET importe_orden = 0
         LET sub_1 = 0    LET sub_2 = 0   LET sub_3 = 0   LET sub_4 = 0
         LET sub_5 = 0    LET sub_6 = 0
         LET sub_11= 0    LET sub_22= 0   LET sub_33= 0   LET sub_44= 0
         LET sub_55= 0    LET sub_66= 0
         INITIALIZE ret_obli.*, ret_nega.* TO NULL
         DECLARE apt_I1 CURSOR FOR
              SELECT UNIQUE a.consecutivo_lote, a.tipo_movimiento,
                     b.nss, b.paterno, b.materno, b.nombres, b.tipo_pago,
                     b.fecha_captura, b.tienda_cod,c.folio_solicitud 
                     FROM  dis_cuenta a, ret_beneficiario b, ret_parcial c
                     WHERE a.nss = b.nss      
                       AND a.subcuenta IN(1,2,5,6,9)
                       AND a.tipo_movimiento = 875
                       AND a.consecutivo_lote = b.consecutivo
                       AND a.fecha_conversion BETWEEN fecha_ini AND fecha_fin
                       AND a.nss  = c.nss
                       AND a.consecutivo_lote = c.consecutivo
               ORDER BY 1,2,3

         FOREACH apt_I1 INTO ret_nega.*

              SELECT ROUND(SUM(m.monto_en_pesos),2)
	      INTO   sub_1  FROM dis_cuenta m
	      WHERE  m.nss             = ret_nega.nss 
	        AND  m.subcuenta       IN(1)
	        AND  m.tipo_movimiento = ret_nega.tipo_movimiento 
	        AND  m.consecutivo_lote= ret_nega.consecutivo
                AND  m.fecha_conversion BETWEEN fecha_ini AND fecha_fin
              IF sub_1 IS NULL OR sub_1 = 0 THEN
                 LET sub_1 = 0
              END IF

              SELECT ROUND(SUM(m.monto_en_pesos),2)
	      INTO   sub_11  FROM dis_cuenta m
	      WHERE  m.nss             = ret_nega.nss 
	        AND  m.subcuenta       IN(1)
	        AND  m.tipo_movimiento = 10
	        AND  m.consecutivo_lote= ret_nega.consecutivo
                AND  m.fecha_conversion BETWEEN fecha_ini AND fecha_fin
              IF sub_11 IS NULL OR sub_11 = 0 THEN
                 LET sub_11 = 0
              END IF

              SELECT ROUND(SUM(m.monto_en_pesos),2)
	      INTO   sub_2  FROM dis_cuenta m
	      WHERE  m.nss             = ret_nega.nss 
	        AND  m.subcuenta       IN(5)
	        AND  m.tipo_movimiento = ret_nega.tipo_movimiento 
	        AND  m.consecutivo_lote= ret_nega.consecutivo
                AND  m.fecha_conversion BETWEEN fecha_ini AND fecha_fin
              IF sub_2 IS NULL OR sub_2 = 0 THEN
                 LET sub_2 = 0
              END IF

              SELECT ROUND(SUM(m.monto_en_pesos),2)
	      INTO   sub_22  FROM dis_cuenta m
	      WHERE  m.nss             = ret_nega.nss 
	        AND  m.subcuenta       IN(5)
	        AND  m.tipo_movimiento = 10
	        AND  m.consecutivo_lote= ret_nega.consecutivo
                AND  m.fecha_conversion BETWEEN fecha_ini AND fecha_fin
              IF sub_22 IS NULL OR sub_22 = 0 THEN
                 LET sub_22 = 0
              END IF

              SELECT ROUND(SUM(m.monto_en_pesos),2)
	      INTO   sub_3  FROM dis_cuenta m
	      WHERE  m.nss             = ret_nega.nss 
	        AND  m.subcuenta       IN(2,6,9)
	        AND  m.tipo_movimiento = ret_nega.tipo_movimiento 
	        AND  m.consecutivo_lote= ret_nega.consecutivo
                AND m.fecha_conversion BETWEEN fecha_ini AND fecha_fin
              IF sub_3 IS NULL OR sub_3 = 0 THEN
                 LET sub_3 = 0
              END IF

              SELECT ROUND(SUM(m.monto_en_pesos),2)
	      INTO   sub_33  FROM dis_cuenta m
	      WHERE  m.nss             = ret_nega.nss 
	        AND  m.subcuenta       IN(2,6,9)
	        AND  m.tipo_movimiento = 10
	        AND  m.consecutivo_lote= ret_nega.consecutivo
                AND m.fecha_conversion BETWEEN fecha_ini AND fecha_fin
              IF sub_33 IS NULL OR sub_33 = 0 THEN
                 LET sub_33 = 0
              END IF

              LET ret_obli.monto_neto    = sub_1 + sub_2 + sub_3 

              LET ret_obli.impto_retenido= sub_11+ sub_22+ sub_33

              LET ret_obli.mto_antes_impto=ret_obli.monto_neto +
                                           ret_obli.impto_retenido

              LET tot_reg   = tot_reg + 1
              LET tmonto_neto = tmonto_neto + ret_obli.monto_neto 
              LET tmto_antes_impto = tmto_antes_impto + ret_obli.impto_retenido
              LET timpto_retenido = timpto_retenido + ret_obli.mto_antes_impto
              LET tpago_efectivo = tpago_efectivo + ret_obli.monto_neto

              LET v_rep  =  "D"                     ,"|",
                             ret_nega.nss            ,"|",
                             ret_nega.nombres        ,"|",
                             ret_nega.paterno        ,"|",
                             ret_nega.materno        ,"|",
                             1                       ,"|",
                             bla18                   ,"|",
                             ret_nega.fecha_captura USING "MMDDYYYY"  ,"|",
                             ret_obli.monto_neto     ,"|",
                             ret_obli.mto_antes_impto,"|",
                             ret_obli.impto_retenido ,"|",
                             ret_nega.folio_soli     ,"|",
                             ret_nega.tienda         ,"|",
                             "875"                   ,"|",
                             ret_nega.consecutivo    ,"|",
                             "                  "    ,"|",
                             "               "       ,"|"
              OUTPUT TO REPORT r_report(v_rep,37,2)

              INITIALIZE ret_obli.* TO NULL
              LET sub_1 = 0    LET sub_2 = 0   LET sub_3 = 0 LET sub_4 = 0
              LET sub_5 = 0    LET sub_6 = 0
              LET sub_11= 0    LET sub_22= 0   LET sub_33= 0   LET sub_44= 0
              LET sub_55= 0    LET sub_66= 0
         END FOREACH
         FINISH REPORT r_report

##Detalle (J-880)
         START REPORT r_report TO v_det880

         LET importe_abono = 0   LET importe_orden = 0
         INITIALIZE ret_obli.*, ret_nega.* TO NULL
         LET sub_1=0  LET sub_2=0  LET sub_3=0   LET sub_4=0  LET sub_5=0
         LET sub_5 = 0    LET sub_6 = 0
         LET sub_11= 0    LET sub_22= 0   LET sub_33= 0   LET sub_44= 0
         LET sub_55= 0    LET sub_66= 0

         DECLARE apt_J CURSOR FOR
              SELECT UNIQUE a.consecutivo_lote, a.tipo_movimiento,
                     b.nss, b.paterno, b.materno, b.nombres, b.tipo_pago,
                     b.fecha_captura, b.tienda_cod,c.folio_solicitud 
                     FROM  dis_cuenta a, ret_beneficiario b, ret_solicitud_tx c
                     WHERE a.nss = b.nss      
                       AND a.subcuenta IN(1,2,4,5,6,7,8,9)             
                       AND a.tipo_movimiento = 880
                       AND a.consecutivo_lote = b.consecutivo
                       AND a.fecha_conversion BETWEEN fecha_ini AND fecha_fin
                       AND a.nss  = c.nss
                       AND a.consecutivo_lote = c.consecutivo
               ORDER BY 3
         FOREACH apt_J INTO ret_nega.*

              SELECT ROUND(SUM(m.monto_en_pesos),2)
	      INTO   sub_1  FROM dis_cuenta m
	      WHERE  m.nss             = ret_nega.nss 
	        AND  m.subcuenta       IN(1)
	        AND  m.tipo_movimiento = ret_nega.tipo_movimiento 
	        AND  m.consecutivo_lote= ret_nega.consecutivo
                AND m.fecha_conversion BETWEEN fecha_ini AND fecha_fin
              IF sub_1 IS NULL OR sub_1 = 0 THEN
                 LET sub_1 = 0
              END IF

              SELECT ROUND(SUM(m.monto_en_pesos),2)
	      INTO   sub_11  FROM dis_cuenta m
	      WHERE  m.nss             = ret_nega.nss 
	        AND  m.subcuenta       IN(1)
	        AND  m.tipo_movimiento = 10
	        AND  m.consecutivo_lote= ret_nega.consecutivo
                AND m.fecha_conversion BETWEEN fecha_ini AND fecha_fin
              IF sub_11 IS NULL OR sub_11 = 0 THEN
                 LET sub_11 = 0
              END IF

              SELECT ROUND(SUM(m.monto_en_pesos),2)
	      INTO   sub_2  FROM dis_cuenta m
	      WHERE  m.nss             = ret_nega.nss 
	        AND  m.subcuenta       IN(2,6,9)
	        AND  m.tipo_movimiento = ret_nega.tipo_movimiento 
	        AND  m.consecutivo_lote= ret_nega.consecutivo
                AND m.fecha_conversion BETWEEN fecha_ini AND fecha_fin
              IF sub_2 IS NULL OR sub_2 = 0 THEN
                 LET sub_2 = 0
              END IF

              SELECT ROUND(SUM(m.monto_en_pesos),2)
	      INTO   sub_22  FROM dis_cuenta m
	      WHERE  m.nss             = ret_nega.nss 
	        AND  m.subcuenta       IN(2,6,9)
	        AND  m.tipo_movimiento = 10
	        AND  m.consecutivo_lote= ret_nega.consecutivo
                AND m.fecha_conversion BETWEEN fecha_ini AND fecha_fin
              IF sub_22 IS NULL OR sub_22 = 0 THEN
                 LET sub_22 = 0
              END IF

              SELECT ROUND(SUM(m.monto_en_pesos),2)
	      INTO   sub_3  FROM dis_cuenta m
	      WHERE  m.nss             = ret_nega.nss 
	        AND  m.subcuenta       IN(4)
	        AND  m.tipo_movimiento = ret_nega.tipo_movimiento 
	        AND  m.consecutivo_lote= ret_nega.consecutivo
                AND m.fecha_conversion BETWEEN fecha_ini AND fecha_fin
              IF sub_3 IS NULL OR sub_3 = 0 THEN
                 LET sub_3 = 0
              END IF

              SELECT ROUND(SUM(m.monto_en_pesos),2)
	      INTO   sub_33  FROM dis_cuenta m
	      WHERE  m.nss             = ret_nega.nss 
	        AND  m.subcuenta       IN(4)
	        AND  m.tipo_movimiento = 10
	        AND  m.consecutivo_lote= ret_nega.consecutivo
                AND m.fecha_conversion BETWEEN fecha_ini AND fecha_fin
              IF sub_33 IS NULL OR sub_33 = 0 THEN
                 LET sub_33 = 0
              END IF

              SELECT ROUND(SUM(m.monto_en_pesos),2)
	      INTO   sub_4  FROM dis_cuenta m
	      WHERE  m.nss             = ret_nega.nss 
	        AND  m.subcuenta       IN(5)
	        AND  m.tipo_movimiento = ret_nega.tipo_movimiento 
	        AND  m.consecutivo_lote= ret_nega.consecutivo
                AND m.fecha_conversion BETWEEN fecha_ini AND fecha_fin
              IF sub_4 IS NULL OR sub_4 = 0 THEN
                 LET sub_4 = 0
              END IF

              SELECT ROUND(SUM(m.monto_en_pesos),2)
	      INTO   sub_44  FROM dis_cuenta m
	      WHERE  m.nss             = ret_nega.nss 
	        AND  m.subcuenta       IN(5)
	        AND  m.tipo_movimiento = 10
	        AND  m.consecutivo_lote= ret_nega.consecutivo
                AND m.fecha_conversion BETWEEN fecha_ini AND fecha_fin
              IF sub_44 IS NULL OR sub_44 = 0 THEN
                 LET sub_44 = 0
              END IF

              SELECT ROUND(SUM(m.monto_en_pesos),2)
	      INTO   sub_5  FROM dis_cuenta m
	      WHERE  m.nss             = ret_nega.nss 
	        AND  m.subcuenta       IN(7)
	        AND  m.tipo_movimiento = ret_nega.tipo_movimiento 
	        AND  m.consecutivo_lote= ret_nega.consecutivo
                AND m.fecha_conversion BETWEEN fecha_ini AND fecha_fin
              IF sub_5 IS NULL OR sub_5 = 0 THEN
                 LET sub_5 = 0
              END IF

              SELECT ROUND(SUM(m.monto_en_pesos),2)
	      INTO   sub_55  FROM dis_cuenta m
	      WHERE  m.nss             = ret_nega.nss 
	        AND  m.subcuenta       IN(7)
	        AND  m.tipo_movimiento = 10
	        AND  m.consecutivo_lote= ret_nega.consecutivo
                AND m.fecha_conversion BETWEEN fecha_ini AND fecha_fin
              IF sub_55 IS NULL OR sub_55 = 0 THEN
                 LET sub_55 = 0
              END IF

              SELECT ROUND(SUM(m.monto_en_pesos),2)
	      INTO   sub_6  FROM dis_cuenta m
	      WHERE  m.nss             = ret_nega.nss 
	        AND  m.subcuenta       IN(8)
	        AND  m.tipo_movimiento = ret_nega.tipo_movimiento 
	        AND  m.consecutivo_lote= ret_nega.consecutivo
                AND m.fecha_conversion BETWEEN fecha_ini AND fecha_fin
              IF sub_6 IS NULL OR sub_6 = 0 THEN
                 LET sub_6 = 0
              END IF

              SELECT ROUND(SUM(m.monto_en_pesos),2)
	      INTO   sub_66  FROM dis_cuenta m
	      WHERE  m.nss             = ret_nega.nss 
	        AND  m.subcuenta       IN(8)
	        AND  m.tipo_movimiento = 10
	        AND  m.consecutivo_lote= ret_nega.consecutivo
                AND m.fecha_conversion BETWEEN fecha_ini AND fecha_fin
              IF sub_66 IS NULL OR sub_66 = 0 THEN
                 LET sub_66 = 0
              END IF

              LET ret_obli.monto_neto    = sub_1 + sub_2 + sub_3 + sub_4 +
					   sub_5 + sub_6

              LET ret_obli.impto_retenido= sub_11+ sub_22+ sub_33+ sub_44+
					   sub_55+ sub_66

              LET ret_obli.mto_antes_impto=ret_obli.monto_neto +
                                           ret_obli.impto_retenido

              LET tot_reg   = tot_reg + 1
              LET tmonto_neto = tmonto_neto + ret_obli.monto_neto 
              LET tmto_antes_impto = tmto_antes_impto + ret_obli.impto_retenido
              LET timpto_retenido = timpto_retenido + ret_obli.mto_antes_impto
              LET tpago_efectivo = tpago_efectivo + ret_obli.monto_neto

              LET v_rep  =  "D"                     ,"|",
                             ret_nega.nss            ,"|",
                             ret_nega.nombres        ,"|",
                             ret_nega.paterno        ,"|",
                             ret_nega.materno        ,"|",
                             1                       ,"|",
                             bla18                   ,"|",
                             ret_nega.fecha_captura USING "MMDDYYYY"  ,"|",
                             ret_obli.monto_neto     ,"|",
                             ret_obli.mto_antes_impto,"|",
                             ret_obli.impto_retenido ,"|",
                             ret_nega.folio_soli     ,"|",
                             ret_nega.tienda         ,"|",
                             "880"                   ,"|",
                             ret_nega.consecutivo    ,"|",
                             "                  "    ,"|",
                             "               "       ,"|"
              OUTPUT TO REPORT r_report(v_rep,37,2)

              INITIALIZE ret_obli.* TO NULL
              LET sub_1 = 0    LET sub_2 = 0   LET sub_3 = 0   LET sub_4 = 0
              LET sub_5 = 0
              LET sub_11= 0    LET sub_22= 0   LET sub_33= 0   LET sub_44= 0
              LET sub_55= 0    LET sub_66= 0
         END FOREACH
         FINISH REPORT r_report


##Detalle ( -490) RETIRO Voluntario
         START REPORT r_report TO v_det490

         LET importe_abono = 0   LET importe_orden = 0
         INITIALIZE ret_obli.*, ret_nega.* TO NULL
         LET sub_1=0  LET sub_2=0  LET sub_3=0   LET sub_4=0  LET sub_5=0
         LET sub_5 = 0    LET sub_6 = 0
         LET sub_11= 0    LET sub_22= 0   LET sub_33= 0   LET sub_44= 0
         LET sub_55= 0    LET sub_66= 0

         DECLARE apt_VOL CURSOR FOR
              SELECT UNIQUE a.consecutivo_lote, a.tipo_movimiento,
                     b.nss, b.paterno, b.materno, b.nombres, b.tipo_pago,
                     a.fecha_conversion, b.tienda_cod,c.n_folio_sol 
                     FROM  dis_cuenta a, ret_beneficiario b, ret_cta_vol c
                     WHERE a.nss = b.nss      
                       AND a.subcuenta IN(3,10)             
                       AND a.tipo_movimiento = 490
                       AND a.consecutivo_lote = b.consecutivo
                       AND a.fecha_conversion BETWEEN fecha_ini AND fecha_fin
                       AND a.nss  = c.n_seguro
                       AND a.consecutivo_lote = c.consecutivo
               ORDER BY 3
         FOREACH apt_VOL INTO ret_nega.*

              SELECT ROUND(SUM(m.monto_en_pesos),2)
	      INTO   sub_1  FROM dis_cuenta m
	      WHERE  m.nss             = ret_nega.nss 
	        AND  m.subcuenta       IN(3,10)
	        AND  m.tipo_movimiento = 490
	        AND  m.consecutivo_lote= ret_nega.consecutivo
                AND m.fecha_conversion BETWEEN fecha_ini AND fecha_fin
              IF sub_1 IS NULL OR sub_1 = 0 THEN
                 LET sub_1 = 0
              END IF

              SELECT ROUND(SUM(m.monto_en_pesos),2)
	      INTO   sub_11 FROM dis_cuenta m
	      WHERE  m.nss             = ret_nega.nss 
	        AND  m.subcuenta       IN(3,10)
	        AND  m.tipo_movimiento = 10
	        AND  m.consecutivo_lote= ret_nega.consecutivo
                AND m.fecha_conversion BETWEEN fecha_ini AND fecha_fin
              IF sub_11 IS NULL OR sub_11= 0 THEN
                 LET sub_11= 0
              END IF

              LET ret_obli.monto_neto    = sub_1 

              LET ret_obli.impto_retenido= sub_11

              LET ret_obli.mto_antes_impto=ret_obli.monto_neto +
                                           ret_obli.impto_retenido

              LET tot_reg          = tot_reg + 1
              LET tmonto_neto      = tmonto_neto + ret_obli.monto_neto 
              LET tmto_antes_impto = tmto_antes_impto + ret_obli.impto_retenido
              LET timpto_retenido  = timpto_retenido + ret_obli.mto_antes_impto
              LET tpago_efectivo   = tpago_efectivo + ret_obli.monto_neto

              LET v_rep  =  "D"                     ,"|",
                             ret_nega.nss            ,"|",
                             ret_nega.nombres        ,"|",
                             ret_nega.paterno        ,"|",
                             ret_nega.materno        ,"|",
                             1                       ,"|",
                             bla18                   ,"|",
                             ret_nega.fecha_captura USING "MMDDYYYY"  ,"|",
                             ret_obli.monto_neto     ,"|",
                             ret_obli.mto_antes_impto,"|",
                             ret_obli.impto_retenido ,"|",
                             ret_nega.folio_soli     ,"|",
                             ret_nega.tienda         ,"|",
                             "490"                   ,"|",
                             ret_nega.consecutivo    ,"|",
                             "                  "    ,"|",
                             "               "       ,"|"
              OUTPUT TO REPORT r_report(v_rep,37,2)

              INITIALIZE ret_obli.* TO NULL
              LET sub_1 = 0    LET sub_2 = 0   LET sub_3 = 0   LET sub_4 = 0
              LET sub_5 = 0
              LET sub_11= 0    LET sub_22= 0   LET sub_33= 0   LET sub_44= 0
              LET sub_55= 0    LET sub_66= 0
         END FOREACH

         FINISH REPORT r_report

## Encabezado
         START REPORT r_report TO v_cza
               LET v_rep = "E"                         ,"|",
                           "12345678"                  ,"|",
                           TODAY     USING "MMDDYYYY"  ,"|",
                           fecha_ini USING "MMDDYYYY"  ,"|",
                           fecha_fin USING "MMDDYYYY"  ,"|",
                           tot_reg   USING "&&&&&&&&&" ,"|",
                           bla216                      ,"|"

               OUTPUT TO REPORT r_report(v_rep,37,1)
         FINISH REPORT r_report
##Sumario
         START REPORT r_report TO v_sum
               LET v_rep = "S"                                       ,"|",
                           tot_reg          USING "&&&&&&&&&"        ,"|",
                           tmonto_neto      USING "&&&&&&&&&&&&&&.&&","|",
                           timpto_retenido  USING "&&&&&&&&&&&&&&.&&","|",
                           tmto_antes_impto USING "&&&&&&&&&&&&&&.&&","|",
                           tpago_efectivo   USING "&&&&&&&&&&&&&&.&&","|",
                           tpago_deposito   USING "&&&&&&&&&&&&&&.&&","|",
                           bla163                                    ,"|"

               OUTPUT TO REPORT r_report(v_rep,37,3)
         FINISH REPORT r_report

END FUNCTION
