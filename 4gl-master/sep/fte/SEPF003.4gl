##############################################################################
#Owner             => E.F.P.
#Programa          => SEPF003                
#Fecha creacion    => 05 DE MAYO DEL 2005 
#By                => MARCO ANTONIO GONZALEZ ROJAS
#Sistema           => SEPARACION DE CUENTAS.            
#Objetivo          => Este programa Genera 2 Archivos.
#                     Archivo 1 :  Información histórica de los Aportes que ha r
#                     ealizado el NSS en la Administradora.
#                     Archivo 2 :   Información de los Traspasos  ICEFA-AFORE qu
#                     e ha recibido el NSS en la Administradora)
# Archivo 1: Nombre con el que se generan los Archivos:
#   	     Si se genero el Reporte por "NSS"  lo deja con el sig nombre: mgon
#            zale_54901500048_AP_220705_0356
#            usuario  nss       fecha   hora
#	     							
# 	     Si se genero el Reporte por "FOLIO"  lo deja con el sig nombre: mgo
#            nzale_10010_AP_220705_0356
#            usuario  folio fecha   hora			
#
#Archivo 2: Nombre con el que se generan los Archivos:
#  	    Si se genero el Reporte por "NSS"  lo deja con el sig nombre: mgon
#           zale_54901500048_IC_220705_0356
#           usuario  nss        fecha   hora			
#     							
#           Si se genero el Reporte por "FOLIO"  lo deja con el sig nombre: mgo
#           nzale_10010_IC_220705_0356
#           usuario  folio    fecha   hora
# 
#NOTA:  Las Variables que recibe este programa son las siguientes: g_nss_o_fol (
#puede ser FOLIO "o" NSS  )
#            g_tip_rep     (puede ser 1 =  NSS   "o"
#                           2 =  FOLIO)
##############################################################################
DATABASE safre_af
GLOBALS
   DEFINE  g_azul      RECORD LIKE safre_af:afi_mae_afiliado.* 
   DEFINE  g_nss_o_fol  CHAR(11),
           g_enter      CHAR(01),
           gtabname     CHAR(50),
           gtabname1    CHAR(50),
           gtabname1_1  CHAR(50),
           gtabname1_2  CHAR(50),
           gsepdetsol   CHAR(50),
           g_sql1       CHAR(1000),
           g_sql2       CHAR(1000),
           g_sql2_1     CHAR(1000),
           g_sql3       CHAR(1000),
           g_sql4       CHAR(1000),
           g_sql4_1     CHAR(1000),
           g_ruta_arch  CHAR(100),
           g_usuario    CHAR(08),
           g_nom_prog   CHAR(02),
           g_nom_prog1  CHAR(02),
           g_hora       CHAR(05),
           g_impre_apor CHAR(300),
           g_impre_ice  CHAR(300),
           g_folio      DECIMAL(8,0),
           g_hoy        DATE,
           g_regs_si_no INTEGER,
           g_regs_s_n   INTEGER,
           g_tip_rep    SMALLINT

   DEFINE  g_aporte RECORD
           n_seguro            LIKE safre_af:dis_det_aporte.n_seguro,
           n_rfc               LIKE safre_af:dis_det_aporte.n_rfc,
           nom_trabajador      LIKE safre_af:dis_det_aporte.nom_trabajador,
           fech_valor_rcv      LIKE safre_af:dis_det_aporte.fech_valor_rcv,
           fech_pago           LIKE safre_af:dis_det_aporte.fech_pago,
           folio_pago_sua      LIKE safre_af:dis_det_aporte.folio_pago_sua,
           reg_patronal_imss   LIKE safre_af:dis_det_aporte.reg_patronal_imss,
           rfc_patron          LIKE safre_af:dis_det_aporte.rfc_patron,
           periodo_pago        LIKE safre_af:dis_det_aporte.periodo_pago,
           impt_ret            LIKE safre_af:dis_det_aporte.impt_ret,
           impt_act_rec_ret    LIKE safre_af:dis_det_aporte.impt_act_rec_ret,
           impt_ces_vej        LIKE safre_af:dis_det_aporte.impt_ces_vej,
           impt_act_r_ces_vej  LIKE safre_af:dis_det_aporte.impt_act_r_ces_vej,
           impt_aport_vol      LIKE safre_af:dis_det_aporte.impt_aport_vol,
           impt_aport_compl    LIKE safre_af:dis_det_aporte.impt_aport_compl,
           impt_aport_pat      LIKE safre_af:dis_det_aporte.impt_aport_pat,
           impt_cuota_soc      LIKE safre_af:dis_det_aporte.impt_cuota_soc,
           impt_aport_est      LIKE safre_af:dis_det_aporte.impt_aport_est,
           impt_aport_esp      LIKE safre_af:dis_det_aporte.impt_aport_esp,
           impt_act_cuo_soc    LIKE safre_af:dis_det_aporte.impt_act_cuo_soc,
           impt_act_aport_est  LIKE safre_af:dis_det_aporte.impt_act_aport_est,
           impt_act_cuo_esp    LIKE safre_af:dis_det_aporte.impt_act_cuo_esp,
           folio               LIKE safre_af:dis_det_aporte.folio
                    END RECORD
                      
   DEFINE  g_ice    RECORD
           nss                 LIKE safre_af:tra_mae_icefa.n_seguro,
           nro_int_cta         LIKE safre_af:tra_mae_icefa.nro_int_cta,
           icefa_cod           LIKE safre_af:tra_mae_icefa.icefa_cod,
           rfc                 LIKE safre_af:tra_mae_icefa.rfc,
           paterno             LIKE safre_af:tra_mae_icefa.paterno,
           materno             LIKE safre_af:tra_mae_icefa.materno,
           nombres             LIKE safre_af:tra_mae_icefa.nombres,
           impt_origi_sar      LIKE safre_af:tra_mae_icefa.saldo_sar_92,
           impt_origi_viv      LIKE safre_af:tra_mae_icefa.saldo_sar_92,
           n_folio             LIKE safre_af:tra_mae_icefa.n_folio 
                    END RECORD
END GLOBALS

MAIN
#m ---------------------------
   OPTIONS INPUT WRAP,
   PROMPT LINE LAST  ,
   ACCEPT KEY CONTROL-I
   DEFER INTERRUPT


   LET  g_tip_rep                 =                    ARG_VAL(1)

   LET  g_nss_o_fol               =                    ARG_VAL(2)

   CALL chec_siesxfol()

   CALL init()
   CALL arma_qrys()  
   CALL prep_qrys()  
   CALL nss_o_folio() 

END MAIN
################################################################################
FUNCTION  arma_qrys()

#====QRY PARA APORTES
   LET  g_sql1   =

        ' SELECT a.n_seguro,a.n_rfc,a.nom_trabajador,a.fech_valor_rcv, ',
        '        a.fech_pago,a.folio_pago_sua,a.reg_patronal_imss, ',
        '        a.rfc_patron,a.periodo_pago,a.impt_ret,a.impt_act_rec_ret, ',
        '        a.impt_ces_vej,a.impt_act_r_ces_vej,a.impt_aport_vol, ',
        '        a.impt_aport_compl,a.impt_aport_pat,a.impt_cuota_soc, ',
        '        a.impt_aport_est,a.impt_aport_esp,a.impt_act_cuo_soc, ',
        '        a.impt_act_aport_est,a.impt_act_cuo_esp,a.folio ',
        '  FROM  ',gtabname

   LET g_sql1                        =                     g_sql1  CLIPPED

        CASE  g_tip_rep
           WHEN 1
              LET g_sql1 = g_sql1 CLIPPED , ' WHERE a.n_seguro =  ? ',' ORDER BY 1 '
        END CASE

#===QRY PARA ICEFAS
   CASE g_tip_rep
      WHEN  1
         LET g_sql3   =

             ' SELECT a.n_seguro,a.nro_ctrl_icefa,a.cve_ced_cuenta,a.rfc, ',
             '        a.paterno,a.materno,a.nombre, ',
             '        a.saldo_sar_92 + b.int_sar_92, ',
             '        a.saldo_viv_92 + b.int_viv_92, ',
             '        a.folio ',
             '  FROM  ',gtabname1_1,',',
             '  OUTER (',gtabname1_2 CLIPPED,') ',
             ' WHERE a.n_seguro =  ? ',
             '   AND a.n_seguro = b.n_seguro '

         LET g_sql3                        =                     g_sql3  CLIPPED

      OTHERWISE 

   END CASE

END FUNCTION
################################################################################
FUNCTION prep_qrys()  
      
   PREPARE sql1  FROM  g_sql1

   IF ( g_tip_rep = 1)  THEN 
      PREPARE sql3  FROM  g_sql3
   ELSE 
   END IF    

END FUNCTION
################################################################################
FUNCTION init()


   SELECT A.ruta_listados
   INTO   g_ruta_arch
   FROM   seg_modulo A
   WHERE  A.modulo_cod = "sep"

   SELECT  USER
   INTO    g_usuario
   FROM    tab_afore_local
   
   LET  g_hoy                     =                    TODAY 
   LET  g_nom_prog                =                    "AP"
   LET  g_nom_prog1               =                    "IC"
   LET  g_hora                    =                    TIME  
   LET  gsepdetsol                =                    "sep_det_solicitud d"
   LET  gtabname                  =                    "dis_det_aporte a"  
   LET  gtabname1_1               =                    "tra_det_trasp_sal a"
   LET  gtabname1_2               =                    "tra_det_trasp_int b"


END FUNCTION
###############################################################################
FUNCTION arch_aporte() 

DEFINE   l_nss_ap_fol     RECORD
         nss              CHAR(11)
                          END RECORD

#==== ASIGNA NOMBRE PARA ARCH DE APORTES PARA VER SI ES X "NSS" O "FOLIO
   CASE g_tip_rep 
      WHEN 1
         LET g_impre_apor        =     g_ruta_arch CLIPPED,"/",g_usuario CLIPPED,"_",g_nss_o_fol CLIPPED,"_",g_nom_prog CLIPPED,"_",g_hoy USING "DDMMYY","_",g_hora[1,2] CLIPPED,g_hora[4,5] CLIPPED
         LET g_impre_apor        =     g_impre_apor CLIPPED
      WHEN 2 
         LET g_impre_apor        =     g_ruta_arch CLIPPED,"/",g_usuario CLIPPED,"_",g_nss_o_fol CLIPPED,"_",g_nom_prog CLIPPED,"_",g_hoy USING "DDMMYY","_",g_hora[1,2] CLIPPED,g_hora[4,5] CLIPPED 

         LET g_impre_apor        =     g_impre_apor CLIPPED
      OTHERWISE 

   END CASE

#=====

   START REPORT rpt_aporte TO g_impre_apor
    
   CASE g_tip_rep
      WHEN   1
         DECLARE c CURSOR FOR sql1

         FOREACH c USING g_nss_o_fol
         INTO g_aporte.*

            LET g_regs_si_no                   =        g_regs_si_no  +   1

            OUTPUT TO REPORT rpt_aporte(g_aporte.*)
      
         END FOREACH 
      WHEN   2
         DECLARE c_aporte CURSOR FOR 
            SELECT A.nss
               FROM nss_x_fol A
        
         FOREACH c_aporte  INTO l_nss_ap_fol.*
             
            DECLARE c_aporte1 CURSOR FOR
               SELECT a.n_seguro,a.n_rfc,a.nom_trabajador,a.fech_valor_rcv,
                      a.fech_pago,a.folio_pago_sua,a.reg_patronal_imss,
                      a.rfc_patron,a.periodo_pago,a.impt_ret,a.impt_act_rec_ret,
                      a.impt_ces_vej,a.impt_act_r_ces_vej,a.impt_aport_vol,
                      a.impt_aport_compl,a.impt_aport_pat,a.impt_cuota_soc,
                      a.impt_aport_est,a.impt_aport_esp,a.impt_act_cuo_soc,
                      a.impt_act_aport_est,a.impt_act_cuo_esp,a.folio
               INTO g_aporte.*
               FROM  dis_det_aporte a
               WHERE a.n_seguro =  l_nss_ap_fol.nss
               ORDER BY 1

            FOREACH c_aporte1 INTO g_aporte.*    

               LET g_regs_si_no                   =        g_regs_si_no  +   1

               OUTPUT TO REPORT rpt_aporte(g_aporte.*)

            END FOREACH 

         END FOREACH        

      OTHERWISE

   END CASE

   FINISH REPORT rpt_aporte

END FUNCTION
###############################################################################
REPORT  rpt_aporte( r_aporte )

   DEFINE  r_aporte RECORD
           n_seguro            LIKE safre_af:dis_det_aporte.n_seguro,
           n_rfc               LIKE safre_af:dis_det_aporte.n_rfc,
           nom_trabajador      LIKE safre_af:dis_det_aporte.nom_trabajador,
           fech_valor_rcv      LIKE safre_af:dis_det_aporte.fech_valor_rcv,
           fech_pago           LIKE safre_af:dis_det_aporte.fech_pago,
           folio_pago_sua      LIKE safre_af:dis_det_aporte.folio_pago_sua,
           reg_patronal_imss   LIKE safre_af:dis_det_aporte.reg_patronal_imss,
           rfc_patron          LIKE safre_af:dis_det_aporte.rfc_patron,
           periodo_pago        LIKE safre_af:dis_det_aporte.periodo_pago,
           impt_ret            LIKE safre_af:dis_det_aporte.impt_ret,
           impt_act_rec_ret    LIKE safre_af:dis_det_aporte.impt_act_rec_ret,
           impt_ces_vej        LIKE safre_af:dis_det_aporte.impt_ces_vej,
           impt_act_r_ces_vej  LIKE safre_af:dis_det_aporte.impt_act_r_ces_vej,
           impt_aport_vol      LIKE safre_af:dis_det_aporte.impt_aport_vol,
           impt_aport_compl    LIKE safre_af:dis_det_aporte.impt_aport_compl,
           impt_aport_pat      LIKE safre_af:dis_det_aporte.impt_aport_pat,
           impt_cuota_soc      LIKE safre_af:dis_det_aporte.impt_cuota_soc,
           impt_aport_est      LIKE safre_af:dis_det_aporte.impt_aport_est,
           impt_aport_esp      LIKE safre_af:dis_det_aporte.impt_aport_esp,
           impt_act_cuo_soc    LIKE safre_af:dis_det_aporte.impt_act_cuo_soc,
           impt_act_aport_est  LIKE safre_af:dis_det_aporte.impt_act_aport_est,
           impt_act_cuo_esp    LIKE safre_af:dis_det_aporte.impt_act_cuo_esp,
           folio               LIKE safre_af:dis_det_aporte.folio
                    END RECORD
   
   OUTPUT
       PAGE LENGTH   1
       LEFT MARGIN   0
       RIGHT MARGIN  0
       TOP MARGIN    0
       BOTTOM MARGIN 0

   FORMAT
   ON EVERY ROW
       PRINT
           COLUMN 01,r_aporte.n_seguro,                            "|",
                     r_aporte.n_rfc,                               "|",
                     r_aporte.nom_trabajador               CLIPPED,"|",
                     r_aporte.fech_valor_rcv               CLIPPED,"|",
                     r_aporte.fech_pago                    CLIPPED,"|",
                     r_aporte.folio_pago_sua               CLIPPED,"|",
                     r_aporte.reg_patronal_imss            CLIPPED,"|",
                     r_aporte.rfc_patron                   CLIPPED,"|",
                     r_aporte.periodo_pago                 CLIPPED,"|",
                     r_aporte.impt_ret                     CLIPPED,"|",
                     r_aporte.impt_act_rec_ret             CLIPPED,"|",
                     r_aporte.impt_ces_vej                 CLIPPED,"|",
                     r_aporte.impt_act_r_ces_vej           CLIPPED,"|",
                     r_aporte.impt_aport_vol               CLIPPED,"|",
                     r_aporte.impt_aport_compl             CLIPPED,"|",
                     r_aporte.impt_aport_pat               CLIPPED,"|",
                     r_aporte.impt_cuota_soc               CLIPPED,"|",
                     r_aporte.impt_aport_est               CLIPPED,"|",
                     r_aporte.impt_aport_esp               CLIPPED,"|",
                     r_aporte.impt_act_cuo_soc             CLIPPED,"|",
                     r_aporte.impt_act_aport_est           CLIPPED,"|",
                     r_aporte.impt_act_cuo_esp             CLIPPED,"|",
                     r_aporte.folio                        CLIPPED
 
END REPORT
###############################################################################
FUNCTION arch_icefas() 
DEFINE   l_nss_ice_fol     RECORD
         nss               CHAR(11)
                           END RECORD

#==== ASIGNA NOMBRE PARA ARCH DE ICEFAS PARA VER SI ES X "NSS" O "FOLIO
   CASE g_tip_rep 
      WHEN 1
         LET g_impre_ice         =     g_ruta_arch CLIPPED,"/",g_usuario CLIPPED,"_",g_nss_o_fol CLIPPED,"_",g_nom_prog1 CLIPPED,"_",g_hoy USING "DDMMYY","_",g_hora[1,2] CLIPPED,g_hora[4,5] CLIPPED
         LET g_impre_ice         =     g_impre_ice CLIPPED
      WHEN 2 
         LET g_impre_ice         =     g_ruta_arch CLIPPED,"/",g_usuario CLIPPED,"_",g_nss_o_fol CLIPPED,"_",g_nom_prog1 CLIPPED,"_",g_hoy USING "DDMMYY","_",g_hora[1,2] CLIPPED,g_hora[4,5] CLIPPED 
         LET g_impre_ice         =     g_impre_ice CLIPPED
      OTHERWISE 

   END CASE
#=====

   START REPORT rpt_ice TO g_impre_ice 
    
   CASE g_tip_rep
      WHEN  1
         DECLARE c1 CURSOR FOR sql3

         FOREACH c1 USING g_nss_o_fol
         INTO g_ice.*

           LET g_regs_s_n                   =        g_regs_s_n  +   1
           OUTPUT TO REPORT rpt_ice(g_ice.*)
      
        END FOREACH 
      WHEN  2   
         DECLARE c_icefas CURSOR FOR
            SELECT A.nss
               FROM nss_x_fol A

         FOREACH c_icefas  INTO l_nss_ice_fol.*
 
            DECLARE c_icefas1 CURSOR FOR
               SELECT a.n_seguro,a.nro_ctrl_icefa,a.cve_ced_cuenta,a.rfc,
                      a.paterno,a.materno,a.nombre,
                      a.saldo_sar_92 + b.int_sar_92,
                      a.saldo_viv_92 + b.int_viv_92,
                      a.folio
                  FROM tra_det_trasp_sal a,
                  OUTER (tra_det_trasp_int b)
               WHERE  a.n_seguro = l_nss_ice_fol.nss
                 AND  a.n_seguro = b.n_seguro

            FOREACH c_icefas1 INTO g_ice.*

               LET g_regs_s_n                   =        g_regs_s_n  +   1

               OUTPUT TO REPORT rpt_ice(g_ice.*)

            END FOREACH 

         END FOREACH

      OTHERWISE

   END CASE
 
   FINISH REPORT rpt_ice    

END FUNCTION

###############################################################################
REPORT  rpt_ice( r_ice )

   DEFINE  r_ice    RECORD
           nss                 LIKE safre_af:tra_mae_icefa.n_seguro,
           nro_int_cta         LIKE safre_af:tra_mae_icefa.nro_int_cta,
           icefa_cod           LIKE safre_af:tra_mae_icefa.icefa_cod,
           rfc                 LIKE safre_af:tra_mae_icefa.rfc,
           paterno             LIKE safre_af:tra_mae_icefa.paterno,
           materno             LIKE safre_af:tra_mae_icefa.materno,
           nombres             LIKE safre_af:tra_mae_icefa.nombres,
           impt_origi_sar      LIKE safre_af:tra_mae_icefa.saldo_sar_92,
           impt_origi_viv      LIKE safre_af:tra_mae_icefa.saldo_sar_92,
           n_folio             LIKE safre_af:tra_mae_icefa.n_folio 
                    END RECORD
   
   OUTPUT
       PAGE LENGTH   1
       LEFT MARGIN   0
       RIGHT MARGIN  0
       TOP MARGIN    0
       BOTTOM MARGIN 0

   FORMAT
   ON EVERY ROW
       PRINT
           COLUMN 01,r_ice.nss,                                 "|",
                     r_ice.nro_int_cta,                         "|",
                     r_ice.icefa_cod                    CLIPPED,"|",
                     r_ice.rfc                          CLIPPED,"|",
                     r_ice.paterno                      CLIPPED,"|",
                     r_ice.materno                      CLIPPED,"|",
                     r_ice.nombres                      CLIPPED,"|",
                     r_ice.impt_origi_sar               CLIPPED,"|",
                     r_ice.impt_origi_viv               CLIPPED,"|",
                     r_ice.n_folio                      CLIPPED,"|"

END REPORT
################################################################################
FUNCTION nss_o_folio() 
   
#==ESTA FUNCION PERMITE VER SI LOS REPORTES SE GENERARAN POR "NSS" O "FOL==

   CASE g_tip_rep
      WHEN      1 #RECIBIO DE PARAMETRO "NSS"
         LET  g_sql2   =

            ' SELECT  COUNT(*) ',
            '  FROM  ',gsepdetsol,
            ' WHERE d.n_seguro =  ? '

         LET g_sql2                        =                     g_sql2  CLIPPED

         PREPARE sql2  FROM   g_sql2
         EXECUTE sql2  USING  g_nss_o_fol INTO g_regs_si_no

            IF ( g_regs_si_no   >   0 )  THEN #HAY REGS PARA GENERAR ARCH_APOR
               CALL arch_aporte()
               CALL s_o_n_regicenss()
            ELSE #CHECA SI HAY REGS PARA GEN ARCH DE ICEFAS
               CALL s_o_n_regicenss()
            END IF
      WHEN 2 # RECIBIO PARAMETRO POR FOLIO

         LET  g_sql2_1   =

            ' SELECT  COUNT(*) ',
            '  FROM  ',gsepdetsol,
            ' WHERE d.n_seguro IN ( SELECT B.nss ',
            '                       FROM nss_x_fol B) '

         LET g_sql2_1                     =                   g_sql2_1  CLIPPED

         PREPARE sql2_1  FROM   g_sql2_1
         EXECUTE sql2_1  INTO g_regs_si_no

            IF ( g_regs_si_no   >   0 )  THEN #HAY REGS PARA GENERAR ARCH_APOR
               CALL arch_aporte()
               CALL s_o_n_regicefol()
            ELSE #CHECA SI HAY REGS PARA GEN ARCH DE ICEFAS
               CALL s_o_n_regicefol()
            END IF 


      OTHERWISE # SE MANDO UN TIPO CRITERIO <> A "NSS" Y "FOLIO" SALDRA DEL PRO
         EXIT PROGRAM

   END CASE

END FUNCTION
################################################################################
FUNCTION chec_siesxfol()

   IF ( g_tip_rep      =   2 ) THEN #BUSQUEDA POR FOLIO

      CREATE TEMP TABLE nss_x_fol(nss CHAR(11))

         INSERT INTO nss_x_fol
         SELECT h.n_seguro
             FROM safre_af:sep_det_solicitud h
         WHERE h.folio  =  g_nss_o_fol 
         GROUP BY 1

   END IF 

END FUNCTION
################################################################################
FUNCTION s_o_n_regicefol()

   LET  g_sql4_1   =

        ' SELECT  COUNT(*) ',
        '  FROM  ',gtabname1_1,
        ' WHERE a.n_seguro IN ( SELECT B.nss ',
        '                   FROM nss_x_fol B) '
   	
   LET g_sql4_1               =                  g_sql4_1  CLIPPED
                 
      PREPARE sql4_1  FROM   g_sql4_1
      EXECUTE sql4_1  INTO g_regs_s_n

         IF ( g_regs_s_n   >   0 ) THEN #HAY REGS PARA GEN ARCH_ICE
            CALL arch_icefas()
         ELSE # NO HUBO REGS PARA ARCH DE APORT "E" ICEFAS
         END IF 


END FUNCTION
################################################################################
FUNCTION s_o_n_regicenss()

   LET  g_sql4   =

      ' SELECT  COUNT(*) ',
      '  FROM  ',gtabname1_1,
      ' WHERE a.n_seguro =  ? ' 
   	
   LET g_sql4                 =                     g_sql4  CLIPPED

   PREPARE sql4  FROM   g_sql4
   EXECUTE sql4  USING  g_nss_o_fol INTO g_regs_s_n

      IF ( g_regs_s_n   >   0 ) THEN #HAY REGS PARA GEN ARCH_ICE
         CALL arch_icefas()
      ELSE # NO HUBO REGS PARA ARCH DE APORT "E" ICEFAS
      END IF 

END FUNCTION
